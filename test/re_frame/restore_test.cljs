(ns re-frame.restore-test
  (:require [cljs.test :refer-macros [is deftest async use-fixtures testing]]
            [re-frame.core :as core :refer [make-restore-fn reg-sub subscribe]]
            [re-frame.subs :as subs]))

;; TODO: future tests in this area could check DB state and registrations are being correctly restored.

(use-fixtures :each {:before (partial subs/clear-all-handlers! core/the-frame)})

(defn one? [x] (= 1 x))
(defn two? [x] (= 2 x))

(defn register-test-subs []
  (reg-sub
    :test-sub
    (fn [db ev]
      (:test-sub db)))

  (reg-sub
    :test-sub2
    (fn [db ev]
      (:test-sub2 db))))

(deftest make-restore-fn-test
  (testing "no existing subs, then making one subscription"
    (register-test-subs)
    (let [original-subs @(:subs-cache core/the-frame)
          restore-fn    (make-restore-fn)]
      (is (zero? (count original-subs)))
      @(subscribe [:test-sub])
      (is (one? (count @(:subs-cache core/the-frame))))
      (is (contains? @(:subs-cache core/the-frame) [[:test-sub] []]))
      (restore-fn)
      (is (zero? (count @(:subs-cache core/the-frame)))))))

(deftest make-restore-fn-test2
  (testing "existing subs, making more subscriptions"
    (register-test-subs)
    @(subscribe [:test-sub])
    (let [original-subs @(:subs-cache core/the-frame)
          restore-fn    (make-restore-fn)]
      (is (one? (count original-subs)))
      @(subscribe [:test-sub2])
      (is (contains? @(:subs-cache core/the-frame) [[:test-sub2] []]))
      (is (two? (count @(:subs-cache core/the-frame))))
      (restore-fn)
      (is (not (contains? @(:subs-cache core/the-frame) [[:test-sub2] []])))
      (is (one? (count @(:subs-cache core/the-frame)))))))

(deftest make-restore-fn-test3
  (testing "existing subs, making more subscriptions with different params on same subscriptions"
    (register-test-subs)
    @(subscribe [:test-sub])
    (let [original-subs @(:subs-cache core/the-frame)
          restore-fn    (make-restore-fn)]
      (is (one? (count original-subs)))
      @(subscribe [:test-sub :extra :params])
      (is (two? (count @(:subs-cache core/the-frame))))
      (restore-fn)
      (is (one? (count @(:subs-cache core/the-frame)))))))

(deftest nested-restores
  (testing "running nested restores"
    (register-test-subs)
    (let [restore-fn-1 (make-restore-fn)
          _            @(subscribe [:test-sub])
          _            (is (one? (count @(:subs-cache core/the-frame))))
          restore-fn-2 (make-restore-fn)]
      @(subscribe [:test-sub2])
      (is (two? (count @(:subs-cache core/the-frame))))
      (restore-fn-2)
      (is (one? (count @(:subs-cache core/the-frame))))
      (restore-fn-1)
      (is (zero? (count @(:subs-cache core/the-frame)))))))