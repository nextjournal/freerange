(ns re-frame.frame
  (:require [re-frame.cofx :as cofx]
            [re-frame.events :as events]
            [re-frame.fx :as fx]
            [re-frame.interop :as interop]
            [re-frame.registry :as reg]
            [re-frame.router :as router]
            [re-frame.std-interceptors :as stdi]
            [re-frame.subs :as subs]))

(defprotocol IFrame
  ;; dispatch ----
  (dispatch [this event-v])
  (dispatch-sync [this event-v])

  ;; subs ----
  (reg-sub-raw [this query-id handler-fn])
  (reg-sub [this query-id args])
  (subscribe
    [this query-v]
    [this query-v dyn-v])
  (clear-sub [this query-id])
  (clear-subscriptions-cache [this])

  ;; fx ----
  (reg-fx [this fx-id handler-fn])
  (clear-fx [this fx-id])

  ;; cofx ----
  (reg-cofx [this cofx-id handler-fn])
  (inject-cofx
    [this cofx-id]
    [this cofx-id value])
  (clear-cofx [this cofx-id])

  ;; events ----
  (clear-event [this event-id])
  (reg-event-db
    [this id db-handler]
    [this id interceptors db-handler])
  (reg-event-fx
    [this id fx-handler]
    [this id interceptors fx-handler])
  (reg-event-ctx
    [this id handler]
    [this id interceptors handler]))

;; connect all the pieces of state ----
(defrecord Frame [registry event-queue app-db subs-cache default-interceptors]
  IFrame
  ;; dispatch ----
  (dispatch [this event-v]
    (router/dispatch event-queue event-v))
  (dispatch-sync [this event-v]
    (router/dispatch-sync this event-v))

  ;; subs ----
  (reg-sub-raw [this query-id handler-fn]
    (reg/register-handler registry subs/kind query-id handler-fn))
  (reg-sub [this query-id args]
    (apply subs/reg-sub this query-id args))
  (subscribe [this query-v]
    (subs/subscribe this query-v))
  (subscribe [this query-v dyn-v]
    (subs/subscribe this query-v dyn-v))
  (clear-sub [this query-id]
    (reg/clear-handlers registry subs/kind query-id))
  (clear-subscriptions-cache [this]
    (subs/clear-subscription-cache! subs-cache))

  ;; fx ----
  (reg-fx [this fx-id handler-fn]
    (reg/register-handler registry fx/kind fx-id handler-fn))
  (clear-fx [this fx-id]
    (reg/clear-handlers registry fx/kind fx-id))

  ;; cofx ----
  (reg-cofx [this cofx-id handler-fn]
    (reg/register-handler registry cofx/kind cofx-id handler-fn))
  (inject-cofx [this cofx-id]
    (cofx/inject-cofx registry cofx-id))
  (inject-cofx [this cofx-id value]
    (cofx/inject-cofx registry cofx-id value))
  (clear-cofx [this cofx-id]
    (reg/clear-handlers registry cofx/kind cofx-id))

  ;; events ----
  (clear-event [this id]
    (reg/clear-handlers registry events/kind id))

  (reg-event-db [this id db-handler]
    (reg-event-db this id nil db-handler))
  (reg-event-db [this id interceptors db-handler]
    (events/register
     registry
     id
     [default-interceptors interceptors (stdi/db-handler->interceptor db-handler)]))
  (reg-event-fx [this id fx-handler]
    (reg-event-fx this id nil fx-handler))
  (reg-event-fx [this id interceptors fx-handler]
    (events/register
     registry
     id
     [default-interceptors interceptors (stdi/fx-handler->interceptor fx-handler)]))
  (reg-event-ctx [this id handler]
    (reg-event-ctx this id nil handler))
  (reg-event-ctx [this id interceptors handler]
    (events/register
     registry
     id
     [default-interceptors interceptors (stdi/ctx-handler->interceptor handler)])))

(defn make-frame [& [{:keys [registry app-db]}]]
  (let [registry    (or registry (reg/make-registry))
        app-db      (or app-db (interop/ratom {}))
        frame       (map->Frame
                     {:registry             registry
                      :app-db               app-db
                      :subs-cache           (subs/->SubscriptionCache (atom {}))
                      :default-interceptors [(cofx/inject-cofx registry :db) (fx/do-fx registry)]})
        event-queue (router/->EventQueue :idle interop/empty-queue {} frame)]
    (assoc frame :event-queue event-queue)))
