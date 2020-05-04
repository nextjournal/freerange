(ns re-frame.context
  (:require ["react" :as react]
            [goog.object :as gobj]
            [lambdaisland.glogi :as log]
            [re-frame.core :as r]
            [re-frame.frame :as frame]
            [re-frame.subs :as subs]
            [reagent.core])
  (:require-macros [re-frame.context :refer [defc import-with-frame]]))

(def frame-context (.createContext react r/default-frame))

(defn set-default-frame [frame]
  (gobj/set frame-context "_currentValue" frame)
  (gobj/set frame-context "_currentValue2" frame))

(defn current-context
  "Gets the react Context for the current component, to be used in lifecycle
  hooks (e.g. render). Assumes that Component.contextType has been set."
  []
  (when-let [cmp (reagent.core/current-component)]
    ;; When used without setting the right contextType we will get #js {} back
    (when (not (object? (.-context cmp)))
      (.-context cmp))))

(def ^:dynamic *current-frame*)

(defn current-frame
  "Get the current frame provided by the context, falling back to the default
  frame. Assumes that Component.contextType = frame-context."
  []
  (or *current-frame*
      (current-context)
      (gobj/get frame-context "_currentValue")))


(defn with-frame
  "Component that acts as a provider for the frame, so to run an isolated version
  of your app, use.

      [with-frame (frame/make-frame)
       [app]]"
  [frame & children]
  (reagent.core/create-element
   (.-Provider frame-context)
   #js {:value frame
        :children (reagent.core/as-element (into [:<>] children))}))

(defc with-app-db
  "Component that acts as a provider for the app-db, it takes the registry from
  the current frame, but uses the given atom for the app-db"
  [app-db & children]
  `[~with-frame ~(frame/make-frame {:registry (:registry (current-frame))
                                    :app-db   app-db})
    ~@children])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Complete copy of the top-level re-frame API. If you are using the context
;; approach then import re-frame.context instead of re-frame.core and things
;; should generally Just Work™

(import-with-frame subscribe re-frame.frame/subscribe)
(import-with-frame dispatch re-frame.frame/dispatch)
(import-with-frame dispatch-sync re-frame.frame/dispatch-sync)
(import-with-frame clear-sub re-frame.frame/clear-sub)
(import-with-frame reg-fx re-frame.frame/reg-fx)
(import-with-frame reg-cofx re-frame.frame/reg-cofx)
(import-with-frame inject-cofx re-frame.frame/inject-cofx)
(import-with-frame clear-cofx re-frame.frame/clear-cofx)
(import-with-frame reg-event-db re-frame.frame/reg-event-db)
(import-with-frame reg-event-fx re-frame.frame/reg-event-fx)
(import-with-frame reg-event-ctx re-frame.frame/reg-event-ctx)
(import-with-frame clear-event re-frame.frame/clear-event)

;; A few special cases which we can't import directly

(defn reg-sub-raw [query-id handler-fn]
  (frame/reg-sub-raw
   (current-frame)
   query-id
   (fn
     ([frame query-v]
      (handler-fn (:app-db frame) query-v))
     ([frame query-v dyn-v]
      (handler-fn (:app-db frame) query-v dyn-v)))))

;; some slight weirdness here because protocols don't support variadic functions
(defn reg-sub [query-id & args]
  (frame/reg-sub (current-frame) query-id args))

(defn clear-subscriptions-cache! [& args]
  (apply subs/-clear (:subs-cache (current-frame)) args))


(defn context-fns
  "Returns subscribe/dispatch/dispatch-sync functions that are bound to the current frame. Use like this

      (defc my-component []
        (reagent/with-let [{:keys [subscribe dispatch]} (re-frame/context-fns)]
          ,,,
          )) "
  []
  {:subscribe (partial re-frame.frame/subscribe (current-frame))
   :dispatch (partial re-frame.frame/dispatch (current-frame))
   :dispatch-sync (partial re-frame.frame/dispatch-sync (current-frame))})
