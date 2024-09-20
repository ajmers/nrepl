(ns nrepl.middleware.dynamic-loader-test
  (:require [clojure.test :refer [deftest is testing]]
            [nrepl.core :refer [combine-responses]]
            [nrepl.middleware.dynamic-loader :as sut]
            [nrepl.transport :as t]))

(defn test-transport [queue]
  (t/fn-transport
   nil
   #(swap! queue conj %)))

(defmacro testing-dynamic
  "Macro useful for defining tests for the dynamic-loader"
  {:style/indent 1}
  [doc & body]
  `(testing ~doc
     (let [state#   (atom {:handler (sut/wrap-dynamic-loader sut/unknown-op)
                           :stack   ["#'nrepl.middleware.dynamic-loader/wrap-dynamic-loader"]})
           ~'handle (fn [msg#]
                      (let [resps#          (atom [])
                            resp-transport# (test-transport resps#)]
                        (binding [sut/*state* state#]
                          ((:handler @state#) (assoc msg# :transport
                                                     resp-transport#)))
                        (combine-responses @resps#)))]
       ~@body)))



(deftest wrap-dynamic-loader-test-with-audit
  (testing-dynamic "Audit middleware prevents add-middleware from working (throws)"
    (handle {:op          "add-middleware"
             :middleware ["nrepl.middleware.session/audit-activity"]})
    (is (thrown? Exception    (handle {:op          "add-middleware"
             :middleware ["nrepl.middleware.session/audit-activity"]})))
    (is (= ["#'nrepl.middleware.dynamic-loader/wrap-dynamic-loader"
            "#'nrepl.middleware.session/session"
            "#'nrepl.middleware.session/audit-activity"]
           (:middleware (handle {:op "ls-middleware"})))))
  (testing-dynamic "Audit middleware prevents Swap-middleware from working (throws)"
    (handle {:op          "add-middleware"
             :middleware ["#'nrepl.middleware.session/audit-activity"]})
    (is (= 3 ;; now we have all these: session eval print caught dynamic
           (count (:middleware (handle {:op "ls-middleware"})))))
    (is (thrown? Exception
                 (handle {:op          "swap-middleware"
                          :middleware ["nrepl.middleware.dynamic-loader/wrap-dynamic-loader"]})                 ))

    (let [ls-result (:middleware (handle {:op "ls-middleware"}))]
      (is (= 3 (count ls-result)))
      ;;  wrap-dynamic-loader *requires* session, that's why session is here even though we didn't add it specifically
      (is (= (set ls-result)
             (set ["#'nrepl.middleware.session/audit-activity"
                   "#'nrepl.middleware.dynamic-loader/wrap-dynamic-loader"
                   "#'nrepl.middleware.session/session"
                   ]))))))

(deftest wrap-dynamic-loader-test
  (testing-dynamic "Booting with no middleware"
    (is (= ["#'nrepl.middleware.dynamic-loader/wrap-dynamic-loader"]
           (:middleware (handle {:op "ls-middleware"}))))
    (is (contains? (:status (handle {:op "describe"}))
                   :unknown-op)))
  (testing-dynamic "Adding a middleware works"
    (handle {:op          "add-middleware"
             :middleware ["nrepl.middleware.session/session"]})
    (is (= ["#'nrepl.middleware.dynamic-loader/wrap-dynamic-loader"
            "#'nrepl.middleware.session/session"]
           (:middleware (handle {:op "ls-middleware"}))))
    (is (contains? (:status (handle {:op "ls-sessions"}))
                   :done)))
  (testing-dynamic "Adding a middleware is cumulative"
    (handle {:op          "add-middleware"
             :middleware ["nrepl.middleware.session/session"]})
    (is (= ["#'nrepl.middleware.dynamic-loader/wrap-dynamic-loader"
            "#'nrepl.middleware.session/session"] ;; with session
           (:middleware (handle {:op "ls-middleware"}))))
    (handle {:op          "add-middleware"
             :middleware ["nrepl.middleware.interruptible-eval/interruptible-eval"]})
    (is (= 5 ;; now we have all these: session eval print caught dynamic
           (count (:middleware (handle {:op "ls-middleware"}))))))
  (testing-dynamic "Swap removes existing middleware"
    (handle {:op          "add-middleware"
             :middleware ["nrepl.middleware.session/session"
                          "nrepl.middleware/wrap-describe"]})
    (is (= 3 ;; now we have all these: session eval print caught dynamic
           (count (:middleware (handle {:op "ls-middleware"})))))
    (handle {:op          "swap-middleware"
             :middleware ["nrepl.middleware.dynamic-loader/wrap-dynamic-loader"]})
    (let [ls-result (:middleware (handle {:op "ls-middleware"}))]
      (is (= 2 (count ls-result)))
      (is (not (contains? (set ls-result)
                          "#'nrepl.middleware/wrap-describe"))))))

(deftest wrap-dynamic-loader-error
  (testing-dynamic "Adding an unknown middleware returns an error"
    (handle {:op         "add-middleware"
             :middleware ["nrepl.middleware/wrap-describe"]})
    ;; Sanity test the describe works
    (is (some? (:versions (handle {:op "describe"}))))
    (let [sw (java.io.StringWriter.)
          resp (binding [*err* sw]
                 (handle {:op         "add-middleware"
                          :middleware ["unknown-middleware/wrap-wot?"]}))]
      (is (contains? (:status resp)
                     :error))
      (is (contains? (set (:unresolved-middleware resp))
                     "unknown-middleware/wrap-wot?"))
      (is (.contains (str sw)
                     "java.io.FileNotFoundException: Could not locate"))
      ;; The handler still works
      (is (some? (:versions (handle {:op "describe"})))))))
