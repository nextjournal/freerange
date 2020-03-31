(ns re-frame.context
  (:require [cljs.env]
            [cljs.analyzer]))

(defmacro defc
  "For definining Reagent components that honor the contextual frame. Like defn
  but sets a :context-type metadata on the function, which Reagent will pick up
  on, so that the correct React context is set for this component."
  [name & fntail]
  `(def ~name
     ^{:context-type frame-context}
     (fn ~@fntail)))

(defmacro import-with-frame [var-sym]
  `(defn
     ~(symbol (name var-sym))
     ;; Attempt at propagating the doc string / arglists, for some reason CIDER
     ;; is not picking this up though.
     ~(select-keys (:meta (cljs.analyzer/resolve-var cljs.env/*compiler* var-sym))
                   [:doc :arglists])
     [& args#]
     (apply ~var-sym (current-frame) args#)))
