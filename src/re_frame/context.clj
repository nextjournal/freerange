(ns re-frame.context)

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
