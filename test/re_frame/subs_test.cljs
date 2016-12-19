(ns re-frame.subs-test
  (:require [cljs.test         :as test :refer-macros [is deftest testing]]
            [reagent.ratom     :as r :refer-macros [reaction]]
            [re-frame.registry :as reg]
            [re-frame.subs     :as subs]
            [re-frame.db       :as db]
            [re-frame.core     :as re-frame]))

(def registry (atom nil))

(defn reg-sub-raw
  "Associate a given `query id` with a given subscription handler function `handler-fn`
   which is expected to take two arguments: app-db and query vector, and return
   a `reaction`.

  This is a low level, advanced function.  You should probably be using reg-sub
  instead."
  [query-id handler-fn]
  (reg/register-handler @registry subs/kind query-id handler-fn))


(test/use-fixtures :each {:before (fn []
                                    (reset! registry (reg/make-registry))
                                    (subs/clear-all-handlers! @registry))})

;;=====test basic subscriptions  ======

(deftest test-reg-sub
  (reg-sub-raw
   :test-sub
   (fn [db [_]] (reaction (deref db))))

  (let [test-sub (subs/subscribe @registry [:test-sub])]
    (is (= @db/app-db @test-sub))
    (reset! db/app-db 1)
    (is (= 1 @test-sub))))

(deftest test-chained-subs
  (reg-sub-raw
   :a-sub
   (fn [db [_]] (reaction (:a @db))))

  (reg-sub-raw
   :b-sub
   (fn [db [_]] (reaction (:b @db))))

  (reg-sub-raw
   :a-b-sub
   (fn [db [_]]
     (let [a (subs/subscribe @registry [:a-sub])
           b (subs/subscribe @registry [:b-sub])]
       (reaction {:a @a :b @b}))))

  (let [test-sub (subs/subscribe @registry [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1 :b 2} @test-sub))
    (swap! db/app-db assoc :b 3)
    (is (= {:a 1 :b 3} @test-sub))))

(deftest test-sub-parameters
  (reg-sub-raw
   :test-sub
   (fn [db [_ b]] (reaction [(:a @db) b])))

  (let [test-sub (subs/subscribe @registry [:test-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= [1 :c] @test-sub))))


(deftest test-sub-chained-parameters
  (reg-sub-raw
   :a-sub
   (fn [db [_ a]] (reaction [(:a @db) a])))

  (reg-sub-raw
   :b-sub
   (fn [db [_ b]] (reaction [(:b @db) b])))

  (reg-sub-raw
   :a-b-sub
   (fn [db [_ c]]
     (let [a (subs/subscribe @registry [:a-sub c])
           b (subs/subscribe @registry [:b-sub c])]
       (reaction {:a @a :b @b}))))

  (let [test-sub (subs/subscribe @registry [:a-b-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a [1 :c], :b [2 :c]} @test-sub))))

(deftest test-nonexistent-sub
  (is (nil? (subs/subscribe @registry [:non-existence]))))

;;============== test cached-subs ================
(def side-effect-atom (atom 0))

(deftest test-cached-subscriptions
  (reset! side-effect-atom 0)

  (reg-sub-raw
   :side-effecting-handler
   (fn side-effect
     [db [_] [_]]
     (swap! side-effect-atom inc)
     (reaction @db)))

  (let [test-sub (subs/subscribe @registry [:side-effecting-handler])]
    (reset! db/app-db :test)
    (is (= :test @test-sub))
    (is (= @side-effect-atom 1))
    (subs/subscribe @registry [:side-effecting-handler])  ;; this should be handled by cache
    (is (= @side-effect-atom 1))
    (subs/subscribe @registry [:side-effecting-handler :a]) ;; should fire again because of the param
    (is (= @side-effect-atom 2))
    (subs/subscribe @registry [:side-effecting-handler :a]) ;; this should be handled by cache
    (is (= @side-effect-atom 2))))

;============== test clear-subscription-cache! ================

(deftest test-clear-subscription-cache!
  (re-frame/reg-sub
   @registry
   :clear-subscription-cache!
   (fn clear-subs-cache [db _] 1))

  (testing "cold cache"
    (is (nil? (subs/cache-lookup [:clear-subscription-cache!]))))
  (testing "cache miss"
    (is (= 1 @(subs/subscribe [:clear-subscription-cache!])))
    (is (some? (subs/cache-lookup [:clear-subscription-cache!]))))
  (testing "clearing"
    (subs/clear-subscription-cache!)
    (is (nil? (subs/cache-lookup [:clear-subscription-cache!])))))

;============== test register-pure macros ================

(deftest test-reg-sub-macro
  (subs/reg-sub
   @registry
   :test-sub
   (fn [db [_]] db))

  (let [test-sub (subs/subscribe @registry [:test-sub])]
    (is (= @db/app-db @test-sub))
    (reset! db/app-db 1)
    (is (= 1 @test-sub))))

(deftest test-reg-sub-macro-singleton
  (subs/reg-sub
   @registry
   :a-sub
   (fn [db [_]] (:a db)))

  (subs/reg-sub
   @registry
   :a-b-sub
   (fn [_ _ _]
     (subs/subscribe @registry [:a-sub]))
   (fn [a [_]]
     {:a a}))

  (let [test-sub (subs/subscribe @registry [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1} @test-sub))
    (swap! db/app-db assoc :b 3)
    (is (= {:a 1} @test-sub))))

(deftest test-reg-sub-macro-vector
  (subs/reg-sub
   @registry
   :a-sub
   (fn [db [_]] (:a db)))

  (subs/reg-sub
   @registry
   :b-sub
   (fn [db [_]] (:b db)))

  (subs/reg-sub
   @registry
   :a-b-sub
   (fn [_ _ _]
     [(subs/subscribe @registry [:a-sub])
      (subs/subscribe @registry [:b-sub])])
   (fn [[a b] [_]]
     {:a a :b b}))

  (let [test-sub (subs/subscribe @registry [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1 :b 2} @test-sub))
    (swap! db/app-db assoc :b 3)
    (is (= {:a 1 :b 3} @test-sub))))

(deftest test-reg-sub-macro-map
  (subs/reg-sub
   @registry
   :a-sub
   (fn [db [_]] (:a db)))

  (subs/reg-sub
   @registry
   :b-sub
   (fn [db [_]] (:b db)))

  (subs/reg-sub
   @registry
   :a-b-sub
   (fn [_ _ _]
     {:a (subs/subscribe @registry [:a-sub])
      :b (subs/subscribe @registry [:b-sub])})
   (fn [{:keys [a b]} [_]]
     {:a a :b b}))

  (let [test-sub (subs/subscribe @registry [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1 :b 2} @test-sub))
    (swap! db/app-db assoc :b 3)
    (is (= {:a 1 :b 3} @test-sub))))

(deftest test-sub-macro-parameters
  (subs/reg-sub
   @registry
   :test-sub
   (fn [db [_ b]] [(:a db) b]))

  (let [test-sub (subs/subscribe @registry [:test-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= [1 :c] @test-sub))))

(deftest test-sub-macros-chained-parameters
  (subs/reg-sub
   @registry
   :a-sub
   (fn [db [_ a]] [(:a db) a]))

  (subs/reg-sub
   @registry
   :b-sub
   (fn [db [_ b]] [(:b db) b]))

  (subs/reg-sub
   @registry
   :a-b-sub
   (fn [[_ c] _]
     [(subs/subscribe @registry [:a-sub c])
      (subs/subscribe @registry [:b-sub c])])
   (fn [[a b] [_ c]] {:a a :b b}))

  (let [test-sub (subs/subscribe @registry [:a-b-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a [1 :c] :b [2 :c]} @test-sub))))

(deftest test-sub-macros-<-
  "test the syntactial sugar"
  (subs/reg-sub
   @registry
   :a-sub
   (fn [db [_]] (:a db)))

  (subs/reg-sub
   @registry
   :a-b-sub
   :<- [:a-sub]
   (fn [a [_]] {:a a}))

  (let [test-sub (subs/subscribe @registry [:a-b-sub])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1} @test-sub))))

(deftest test-sub-macros-chained-parameters-<-
  "test the syntactial sugar"
  (subs/reg-sub
   @registry
   :a-sub
   (fn [db [_]] (:a db)))

  (subs/reg-sub
   @registry
   :b-sub
   (fn [db [_]] (:b db)))

  (subs/reg-sub
   @registry
   :a-b-sub
   :<- [:a-sub]
   :<- [:b-sub]
   (fn [[a b] [_ c]] {:a a :b b}))

  (let [test-sub (subs/subscribe @registry [:a-b-sub :c])]
    (reset! db/app-db {:a 1 :b 2})
    (is (= {:a 1 :b 2} @test-sub) )))

(deftest test-registering-subs-doesnt-create-subscription
  (let [sub-called? (atom false)]
    (with-redefs [subs/subscribe (fn [& args] (reset! sub-called? true))]
      (subs/reg-sub
       @registry
       :a-sub
       (fn [db [_]] (:a db)))

      (subs/reg-sub
       @registry
       :b-sub
       (fn [db [_]] (:b db)))

      (subs/reg-sub
       @registry
       :fn-sub
       (fn [[_ c] _]
         [(subs/subscribe @registry [:a-sub c])
          (subs/subscribe @registry [:b-sub c])])
       (fn [db [_]] (:b db)))

      (subs/reg-sub
       @registry
       :a-sugar-sub
       :<- [:a-sub]
       (fn [[a] [_ c]] {:a a}))

      (subs/reg-sub
       @registry
       :a-b-sub
       :<- [:a-sub]
       :<- [:b-sub]
       (fn [[a b] [_ c]] {:a a :b b})))

    (is (false? @sub-called?))))

;; Dynamic subscriptions

(deftest test-dynamic-subscriptions
  (subs/reg-sub
    @registry
    :dyn-sub
    (fn [db ev dynv]
      (first dynv)))

  (testing "happy case"
    (is (= 1 @(subs/subscribe @registry [:dyn-sub] [(r/atom 1)]))))
  (testing "subscription that doesn't exist"
    (is (nil? (subs/subscribe @registry [:non-existent] [(r/atom 1)]))))
  (testing "Passing a non-reactive value to a dynamic subscription"
    (is (thrown-with-msg? js/Error #"No protocol method IDeref" @(subs/subscribe @registry [:dyn-sub] [1])))))
