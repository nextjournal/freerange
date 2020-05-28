(ns re-frame.context
  (:refer-clojure :exclude [bound-fn])
  (:require [cljs.env]
            [cljs.analyzer]))

(defmacro defc
  "For definining Reagent components that honor the contextual frame. Like defn
  but sets a :context-type metadata on the function, which Reagent will pick up
  on, so that the correct React context is set for this component."
  [name & fntail]
  (let [[doc fntail] (if (string? (first fntail))
                       [(first fntail) (rest fntail)]
                       [nil fntail])]
    `(def ~(with-meta name (merge {:doc doc} (:meta &form)))
       ^{:context-type frame-context}
       (fn ~@fntail))))

(defmacro with-frame-binding [frame & body]
  `(binding [~'re-frame.registry/*current-frame* ~frame]
     (assert (instance? ~'re-frame.frame/IFrame ~frame) "given frame is not of type `re-frame.frame/IFrame`")
     ~@body))

(defmacro import-with-frame
  ([var-sym]
   `(import-with-frame ~(symbol (name var-sym)) ~var-sym))
  ([name var-sym]
   `(defn ~name
      ;; Attempt at propagating the doc string / arglists, for some reason CIDER
      ;; is not picking this up though.
      ~(select-keys (:meta (cljs.analyzer/resolve-var cljs.env/*compiler* var-sym))
                    [:doc :arglists])
      [& args#]
      (apply ~var-sym (current-frame) args#))))

(defmacro bound-fn [& args]
  (let [[name argv & body] (if (symbol? (first args))
                             args
                             (into [nil] args))]
    `(let [frame# (~'re-frame.context/current-frame)]
       (fn ~@(when name name) ~argv
         (binding [~'re-frame.registry/*current-frame* frame#]
           ~@body)))))
