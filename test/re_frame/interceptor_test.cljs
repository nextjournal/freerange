(ns re-frame.interceptor-test
  (:require [cljs.test :refer-macros [is deftest testing]]
            [re-frame.interop :refer [ratom]]
            [re-frame.frame :as frame]
            [re-frame.interceptor :refer [context get-coeffect assoc-effect assoc-coeffect get-effect update-coeffect update-effect]]
            [re-frame.std-interceptors :refer [debug trim-v path enrich after on-changes
                                               db-handler->interceptor fx-handler->interceptor]]
            [re-frame.interceptor :as interceptor]))

(enable-console-print!)

(deftest test-trim-v
  (let [ctx           (context (frame/make-frame) [:event-id :b :c] [])
        ctx-trimmed   ((:before trim-v) ctx)
        ctx-untrimmed ((:after trim-v) ctx-trimmed)]
    (is (= (get-coeffect ctx-trimmed :event)
           [:b :c]))
    (is (= (get-coeffect ctx-untrimmed :event)
           [:event-id :b :c]))
    (is (= ctx-untrimmed ctx))))


(deftest test-one-level-path
  (let [db   {:showing true :another 1}
        p1   (path [:showing])]   ;; a simple one level path

    (let [b4 (-> (context (frame/make-frame) [] [])
                 (assoc-coeffect :db db)
                 ((:before p1)))         ;; before
          a (-> b4
                (assoc-effect :db false)
                ((:after p1)))]         ;; after

      (is (= (get-coeffect b4 :db)      ;; test before
             true))
      (is (= (get-effect a :db)         ;; test after
             {:showing false :another 1})))))


(deftest test-two-level-path
  (let [db  {:1 {:2 :target}}
        p  (path [:1 :2])]    ;; a two level path

    (let [b4 (-> (context (frame/make-frame) [] [])
                 (assoc-coeffect :db db)
                 ((:before p))) ]          ;; before

      (is (= (get-coeffect b4 :db)      ;; test before
             :target))

      ;; test #1
      (is (= {:1 {:2 :4}}
             (-> b4
                 (assoc-effect :db :4)    ;; <-- db becomes :4
                 ((:after p))
                 (get-effect :db))))

      ;; test #2 - set db to nil
      (is (= {:1 {:2 nil}}
             (-> b4
                 (assoc-effect :db nil)   ;; <-- db becomes nil
                 ((:after p))
                 (get-effect :db)))))))

(deftest path-with-no-db-returned
  (let [path-interceptor (path :a)]
    (-> (context (frame/make-frame) [] [path-interceptor])
        (assoc-coeffect :db {:a 1})
        (interceptor/invoke-interceptors :before)
        interceptor/change-direction
        (interceptor/invoke-interceptors :after)
        (get-effect :db)
        (nil?)                                              ;; We don't expect an effect to be added.
        (is))))

(deftest test-db-handler-interceptor
  (let [event   [:a :b]

        handler (fn [db v]
                  ;; make sure it was given the right arguements
                  (is (= db :original-db-val))
                  (is (= v event))
                  ;; return a specific value for later checking
                  :new-db-val)

        i1      (db-handler->interceptor handler)
        db      (-> (context (frame/make-frame) event [])
                    (assoc-coeffect :db :original-db-val)
                    ((:before i1))            ;; calls handler - causing :db in :effects to change
                    (get-effect :db))]
    (is (= :new-db-val db))))



(deftest test-fx-handler-interceptor
  (let [event   [:a :b]
        coeffect {:db 4 :event event}
        effect   {:db 5 :dispatch [:a]}

        handler (fn [world v]
                  ;; make sure it was given the right arguements
                  (is (= world coeffect))
                  (is (= v event))

                  ;; return a specific value for later checking
                  effect)

        i1      (fx-handler->interceptor handler)
        e       (-> (context (frame/make-frame) event [])
                    (assoc-coeffect :db (:db coeffect))
                    ((:before i1))            ;; call the handler
                    (get-effect))]
    (is (= {:db 5 :dispatch [:a]} e))))

(deftest test-on-changes
  (let [change-handler-i  (->  (fn [db v] (assoc db :a 10))
                               db-handler->interceptor)

        no-change-handler-i  (->  (fn [db v] db)
                                  db-handler->interceptor)

        no-db-handler-i (-> (fn [ctx v] {})
                            fx-handler->interceptor)

        change-i   (on-changes + [:c] [:a] [:b])
        orig-db    {:a 0 :b 2}]

    (is (=  {:a 0 :b 2}
            (-> (context (frame/make-frame) [] [])
                (assoc-coeffect :db orig-db)
                ((:before no-change-handler-i))   ;; no change to :a and :b
                ((:after change-i))
                (get-effect :db))))
    (is (=  {:a 10 :b 2 :c 12}
            (-> (context (frame/make-frame) [] [])
                (assoc-coeffect :db orig-db)
                ((:before change-handler-i))       ;; cause change to :a
                ((:after change-i))
                (get-effect :db))))

    (is (=  ::not-found
            (-> (context (frame/make-frame) [] [])
                (assoc-coeffect :db orig-db)
                ((:before no-db-handler-i))       ;; no db effect in context
                ((:after change-i))
                (get-effect :db ::not-found))))))

(deftest test-after
  (testing "when no db effect is returned"
    (let [after-db-val (ratom nil)]
      (-> (context (frame/make-frame) [:a :b]
            [(after (fn [db] (reset! after-db-val db)))])
          (assoc-coeffect :db {:a 1})
          (interceptor/invoke-interceptors :before)
          interceptor/change-direction
          (interceptor/invoke-interceptors :after))
      (is (= @after-db-val {:a 1}))))
  (testing "when a false db effect is returned"
    (let [after-db-val (ratom :not-reset)]
      (-> (context (frame/make-frame) [:a :b]
            [(after (fn [db] (reset! after-db-val db)))])
          (assoc-effect :db nil)
          (interceptor/invoke-interceptors :before)
          interceptor/change-direction
          (interceptor/invoke-interceptors :after))
      (is (= @after-db-val nil))))
  (testing "when a nil db effect is returned"
    (let [after-db-val (ratom :not-reset)]
      (-> (context (frame/make-frame) [:a :b]
            [(after (fn [db] (reset! after-db-val db)))])
          (assoc-effect :db false)
          (interceptor/invoke-interceptors :before)
          interceptor/change-direction
          (interceptor/invoke-interceptors :after))
      (is (= @after-db-val false)))))

(deftest test-enrich
  (testing "when no db effect is returned"
    (let [ctx (-> (context (frame/make-frame) [] [])
                  (assoc-coeffect :db {:a 1}))]
      (is (= ::not-found (get-effect ctx :db ::not-found)))
      (-> ctx (:after (enrich (fn [db] (is (= db {:a 1})))))))))

(deftest test-update-effect
  (let [context {:effects {:db {:a 1}}
                 :coeffects {:db {:a 1}}}]
    (is (= {:effects {:db {:a 2}}
            :coeffects {:db {:a 1}}}
         (update-effect context :db update :a inc)))))

(deftest test-update-coeffect
  (let [context {:effects {:db {:a 1}}
                 :coeffects {:db {:a 1}}}]
    (is (= {:effects {:db {:a 1}}
            :coeffects {:db {:a 2}}}
         (update-coeffect context :db update :a inc)))))
