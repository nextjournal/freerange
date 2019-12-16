(ns re-frame.context
  (:require [reagent.core]
            ["react" :as react]
            [re-frame.core :as r]
            [re-frame.subs :as subs]
            [re-frame.frame :as frame])
  (:require-macros [re-frame.context :refer [defc]]))

(def frame-context (.createContext react r/default-frame))

(defn current-context
  "Gets the react Context for the current component, to be used in lifecycle
  hooks (e.g. render). Assumes that Component.contextType has been set."
  []
  (when-let [cmp (reagent.core/current-component)]
    (.-context cmp)))

(defn current-frame
  "Get the current frame provided by the context, falling back to the default
  frame. Assumes that Component.contextType = frame-context."
  []
  (or (current-context) r/default-frame))

(defn with-frame
  "Component that acts as a provider for the frame, so to run an isolated version
  of your app, use.

      [with-frame (frame/make-frame)
       [app]]
  "
  [frame & children]
  (reagent.core/create-element
   (.-Provider frame-context)
   #js {:value frame
        :children (reagent.core/as-element (into [:<>] children))}))

(def with-app-db
  "Component that acts as a provider for the app-db, it takes the registry from
  the current frame, but uses the given atom for the app-db"
  ^{:context-type frame-context}
  (fn [app-db & children]
    `[~with-frame ~(frame/make-frame {:registry (:registry (current-frame))
                                      :app-db   app-db})
      ~@children]))