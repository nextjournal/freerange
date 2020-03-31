(ns re-frame.context
  (:require [cljs.env]
            [cljs.analyzer]))

(defmacro defc [name args1 args2 & body]
  `(def ~name
     ^{:context-type frame-context}
     (fn ~args2
       (let [frame# (^:cljs.analyzer/no-resolve re-frame.context/current-frame)
             {:keys ~args1}
             {:frame frame#
              :<sub  (fn [& args#] (apply re-frame.frame/subscribe frame# args#))
              :evt>  (fn [& args#] (apply re-frame.frame/dispatch frame# args#))}]
         ~@body))))

(defmacro import-with-frame [var-sym]
  `(defn
     ~(symbol (name var-sym))
     ;; Attempt at propagating the doc string / arglists, for some reason CIDER
     ;; is not picking this up though.
     ~(select-keys (:meta (cljs.analyzer/resolve-var cljs.env/*compiler* var-sym))
                   [:doc :arglists])
     [& args#]
     (apply ~var-sym (current-frame) args#)))
