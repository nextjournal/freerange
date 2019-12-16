(ns re-frame.router-test
  (:require [clojure.test :refer :all]
            [re-frame.frame :as frame]))

(def frame (atom nil))

(defn init-frame []
  (reset! frame (doto (frame/make-frame)
                  (frame/reg-event-db ::test
                                      (fn [db [_ i]]
                                        (update db ::test (fnil conj []) i)))

                  (frame/reg-fx ::promise
                                (fn [{:keys [p val]}]
                                  (deliver p val)))

                  (frame/reg-event-fx ::sentinel
                                      (fn [cofx [_ p val]]
                                        {::promise {:p p :val val}})))))

(use-fixtures :each {:before init-frame})

(deftest dispatching-race-condition-469-test
  ;; Checks for day8/re-frame#469
  (let [p (promise)]
    (is (nil? (dotimes [i 1000]
                (frame/dispatch @frame [::test i]))))
    (is (nil? (frame/dispatch @frame [::sentinel p ::done])))
    (let [val (deref p 1000 ::timed-out)]
      (is (= ::done val)))
    (is (= (::test @(:app-db @frame))
           (range 1000)))))
