(ns re-frame.core
  (:require [lambdaisland.glogi :as glogi]
            [re-frame.events :as events]
            [re-frame.subs :as subs]
            [re-frame.frame :as frame]
            [re-frame.interop :as interop]
            [re-frame.fx :as fx]
            [re-frame.cofx :as cofx]
            [re-frame.router :as router]
            [re-frame.loggers :as loggers]
            [re-frame.registry :as reg]
            [re-frame.interceptor :as interceptor]
            [re-frame.std-interceptors :as std-interceptors]))

#?(:cljs
   (glogi/set-levels
    {:glogi/root :info}))


;; -- API ---------------------------------------------------------------------
;;
;; This namespace represents the re-frame API
;;
;; Below, you'll see we've used this technique:
;;   (def  api-name-for-fn    deeper.namespace/where-the-defn-is)
;;
;; So, we promote a `defn` in a deeper namespace "up" to the API
;; via a `def` in this namespace.
;;
;; Turns out, this approach makes it hard:
;;   - to auto-generate API docs
;;   - for IDEs to provide code completion on functions in the API
;;
;; Which is annoying. But there are pros and cons and we haven't
;; yet revisited the decision.  To compensate, we've added more nudity
;; to the docs.
;;

;; -- interceptor related
;; useful if you are writing your own interceptors
(def ->interceptor   interceptor/->interceptor)
(def enqueue         interceptor/enqueue)
(def get-coeffect    interceptor/get-coeffect)
(def get-effect      interceptor/get-effect)
(def assoc-effect    interceptor/assoc-effect)
(def assoc-coeffect  interceptor/assoc-coeffect)


;; --  standard interceptors
(def debug       std-interceptors/debug)
(def path        std-interceptors/path)
(def enrich      std-interceptors/enrich)
(def trim-v      std-interceptors/trim-v)
(def after       std-interceptors/after)
(def on-changes  std-interceptors/on-changes)

;; XXX move API functions up to this core level - to enable code completion and docs
;; XXX on figwheel reload, is there a way to not get the re-registration messages.

;; Export API wrapping `default-frame` singleton ---

(def default-frame (frame/make-frame))

(fx/register-built-in! default-frame)
(cofx/register-built-in! default-frame)

(def dispatch (partial frame/dispatch default-frame))
(def dispatch-sync (partial frame/dispatch-sync default-frame))

(defn reg-sub-raw [query-id handler-fn]
  (frame/reg-sub-raw
   default-frame
   query-id
   (fn [frame query-v]
     (handler-fn (:app-db frame) query-v))))

;; some slight weirdness here because protocols don't support variadic functions
(defn reg-sub [query-id & args]
  (frame/reg-sub default-frame query-id args))

(def subscribe (partial frame/subscribe default-frame))
(def clear-sub (partial frame/clear-sub default-frame))
(def clear-subscriptions-cache! (partial subs/-clear (:subs-cache default-frame)))

(def reg-fx (partial frame/reg-fx default-frame))
(def clear-fx (partial frame/clear-fx default-frame))

(def reg-cofx (partial frame/reg-cofx default-frame))
(def inject-cofx (partial frame/inject-cofx default-frame))
(def clear-cofx (partial frame/clear-cofx default-frame))

(def reg-event-db (partial frame/reg-event-db default-frame))
(def reg-event-fx (partial frame/reg-event-fx default-frame))
(def reg-event-ctx (partial frame/reg-event-ctx default-frame))
(def clear-event (partial frame/clear-event default-frame))

;; --  logging ----------------------------------------------------------------
;; Internally, re-frame uses the logging functions: warn, log, error, group and groupEnd
;; By default, these functions map directly to the js/console implementations,
;; but you can override with your own fns (set or subset).
;; Example Usage:
;;   (defn my-fn [& args]  (post-it-somewhere (apply str args)))  ;; here is my alternative
;;   (re-frame.core/set-loggers!  {:warn my-fn :log my-fn})       ;; override the defaults with mine
(def set-loggers! loggers/set-loggers!)

;; If you are writing an extension to re-frame, like perhaps
;; an effects handler, you may want to use re-frame logging.
;;
;; usage: (console :error "Oh, dear God, it happened: " a-var " and " another)
;;        (console :warn "Possible breach of containment wall at: " dt)
(def console loggers/console)


;; -- unit testing ------------------------------------------------------------

(defn make-restore-fn
  "Checkpoints the state of re-frame and returns a function which, when
  later called, will restore re-frame to that checkpointed state.

  Checkpoint includes app-db, all registered handlers and all subscriptions."
  []
  (frame/make-restore-fn default-frame))

(defn purge-event-queue
  "Remove all events queued for processing"
  []
  (router/purge (:event-queue default-frame)))

;; -- Event Processing Callbacks  ---------------------------------------------

(defn add-post-event-callback
  "Registers a function `f` to be called after each event is processed
   `f` will be called with two arguments:
    - `event`: a vector. The event just processed.
    - `queue`: a PersistentQueue, possibly empty, of events yet to be processed.

   This is useful in advanced cases like:
     - you are implementing a complex bootstrap pipeline
     - you want to create your own handling infrastructure, with perhaps multiple
       handlers for the one event, etc.  Hook in here.
     - libraries providing 'isomorphic javascript' rendering on  Nodejs or Nashorn.

  'id' is typically a keyword. Supplied at \"add time\" so it can subsequently
  be used at \"remove time\" to get rid of the right callback."
  ([f]
   (add-post-event-callback f f))   ;; use f as its own identifier
  ([id f]
   (router/add-post-event-callback (:event-queue default-frame) id f)))


(defn remove-post-event-callback
  [id]
  (router/remove-post-event-callback (:event-queue default-frame) id))
