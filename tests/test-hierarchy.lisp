;;;; test-hierarchy.lisp - Tests for hierarchical logger functionality
;;;; SPDX-License-Identifier: MIT

(in-package #:llog/tests)

(in-suite :llog)

;;; Basic Logger Creation and Retrieval

(def-test get-logger-creates-new-logger ()
  "Test that get-logger creates a new logger if it doesn't exist."
  (llog::clear-logger-registry)
  (let ((logger (llog:get-logger "test.logger")))
    (is (not (null logger)))  ; lint:suppress not-null
    (is (typep logger 'llog::logger))
    (is (string= "test.logger" (llog::logger-name logger)))))

(def-test get-logger-returns-existing-logger ()
  "Test that get-logger returns the same logger instance for the same name."
  (llog::clear-logger-registry)
  (let ((logger1 (llog:get-logger "test.logger"))
        (logger2 (llog:get-logger "test.logger")))
    (is (eq logger1 logger2))))

(def-test find-logger-returns-nil-for-missing ()
  "Test that find-logger returns NIL when logger doesn't exist."
  (llog::clear-logger-registry)
  (is (null (llog:find-logger "nonexistent"))))

(def-test find-logger-returns-existing ()
  "Test that find-logger returns existing logger."
  (llog::clear-logger-registry)
  (let ((logger (llog:get-logger "test")))
    (is (eq logger (llog:find-logger "test")))))

;;; Name Parsing

(def-test split-logger-name-basic ()
  "Test splitting logger names into components."
  (is (equal '("app") (llog::split-logger-name "app")))
  (is (equal '("app" "app.db") (llog::split-logger-name "app.db")))
  (is (equal '("app" "app.db" "app.db.query")
             (llog::split-logger-name "app.db.query"))))

(def-test split-logger-name-empty ()
  "Test splitting empty logger name."
  (is (null (llog::split-logger-name ""))))

(def-test parent-logger-name-basic ()
  "Test getting parent logger names."
  (is (string= "" (llog::parent-logger-name "app")))
  (is (string= "app" (llog::parent-logger-name "app.db")))
  (is (string= "app.db" (llog::parent-logger-name "app.db.query"))))

(def-test parent-logger-name-root ()
  "Test parent of root logger is NIL."
  (is (null (llog::parent-logger-name ""))))

;;; Hierarchical Inheritance

(def-test child-inherits-parent-level ()
  "Test that child logger inherits level from parent."
  (llog::clear-logger-registry)
  (let* ((parent (llog:get-logger "app" :level :debug))
         (child (llog:get-logger "app.db")))
    (is (= llog:+debug+ (logger-level parent)))
    (is (= llog:+debug+ (logger-level child)))))

(def-test child-inherits-from-nearest-parent ()
  "Test that child inherits from nearest existing parent."
  (llog::clear-logger-registry)
  (let* ((grandparent (llog:get-logger "app" :level :warn))
         (child (llog:get-logger "app.db.query")))
    ;; app.db doesn't exist, so app.db.query should inherit from app
    (is (= llog:+warn+ (logger-level child)))))

(def-test explicit-level-overrides-inheritance ()
  "Test that explicit level overrides parent inheritance."
  (llog::clear-logger-registry)
  (let* ((parent (llog:get-logger "app" :level :warn))
         (child (llog:get-logger "app.db" :level :debug)))
    (is (= llog:+warn+ (logger-level parent)))
    (is (= llog:+debug+ (logger-level child)))))

(def-test child-without-parent-uses-defaults ()
  "Test that logger without parent uses default configuration."
  (llog::clear-logger-registry)
  (let ((logger (llog:get-logger "standalone")))
    (is (= llog:+info+ (logger-level logger)))
    (is (= 1 (length (logger-outputs logger))))))

;;; Registry Management

(def-test list-loggers-returns-all-names ()
  "Test that list-loggers returns all registered logger names."
  (llog::clear-logger-registry)
  (llog:get-logger "app")
  (llog:get-logger "app.db")
  (llog:get-logger "api")
  (let ((names (llog:list-loggers)))
    (is (= 3 (length names)))
    (is (member "app" names :test #'string=))
    (is (member "app.db" names :test #'string=))
    (is (member "api" names :test #'string=))))

(def-test clear-logger-registry-removes-all ()
  "Test that clear-logger-registry removes all loggers."
  (llog::clear-logger-registry)
  (llog:get-logger "app")
  (llog:get-logger "app.db")
  (llog::clear-logger-registry)
  (is (null (llog:list-loggers))))

;;; Level Propagation

(def-test set-logger-level-recursive-propagates ()
  "Test that set-logger-level-recursive propagates to children."
  (llog::clear-logger-registry)
  (let* ((parent (llog:get-logger "app" :level :info))
         (child1 (llog:get-logger "app.db"))
         (child2 (llog:get-logger "app.api"))
         (unrelated (llog:get-logger "other")))
    (llog:set-logger-level-recursive "app" :debug)
    (is (= llog:+debug+ (logger-level parent)))
    (is (= llog:+debug+ (logger-level child1)))
    (is (= llog:+debug+ (logger-level child2)))
    (is (= llog:+info+ (logger-level unrelated)))))

(def-test set-logger-level-recursive-no-propagate ()
  "Test that set-logger-level-recursive with :propagate nil only affects target."
  (llog::clear-logger-registry)
  (let* ((parent (llog:get-logger "app" :level :info))
         (child (llog:get-logger "app.db")))
    (llog:set-logger-level-recursive "app" :debug :propagate nil)
    (is (= llog:+debug+ (logger-level parent)))
    (is (= llog:+info+ (logger-level child)))))

(def-test set-logger-level-recursive-deep-hierarchy ()
  "Test level propagation through deep hierarchy."
  (llog::clear-logger-registry)
  (llog:get-logger "a")
  (llog:get-logger "a.b")
  (llog:get-logger "a.b.c")
  (llog:get-logger "a.b.c.d")
  (llog:set-logger-level-recursive "a" :trace)
  (is (= llog:+trace+ (logger-level (llog:find-logger "a"))))
  (is (= llog:+trace+ (logger-level (llog:find-logger "a.b"))))
  (is (= llog:+trace+ (logger-level (llog:find-logger "a.b.c"))))
  (is (= llog:+trace+ (logger-level (llog:find-logger "a.b.c.d")))))

;;; Root Logger

(def-test root-logger-has-empty-name ()
  "Test that root logger has empty string as name."
  (llog::clear-logger-registry)
  (let ((root (llog:root-logger)))
    (is (string= "" (llog::logger-name root)))))

(def-test set-root-level-propagates-to-all ()
  "Test that set-root-level propagates to all loggers."
  (llog::clear-logger-registry)
  (let ((root (llog:root-logger))
        (app (llog:get-logger "app"))
        (db (llog:get-logger "app.db")))
    (llog:set-root-level :warn)
    (is (= llog:+warn+ (logger-level root)))
    (is (= llog:+warn+ (logger-level app)))
    (is (= llog:+warn+ (logger-level db)))))

(def-test all-loggers-inherit-from-root ()
  "Test that all loggers inherit from root when no other parent exists."
  (llog::clear-logger-registry)
  (let* ((root (llog:root-logger))
         (app (llog:get-logger "app")))
    (llog:set-root-level :error)
    ;; Existing logger won't update, but new ones will inherit
    (let ((new-logger (llog:get-logger "newapp")))
      (is (= llog:+error+ (logger-level new-logger))))))

;;; Integration Tests

(def-test hierarchical-logging-respects-levels ()
  "Test that hierarchical loggers respect their individual levels."
  (llog::clear-logger-registry)
  (let* ((stream (make-string-output-stream))
         (parent (llog:get-logger "app" :level :warn
                                   :outputs (list (llog:make-stream-output stream))))
         (child (llog:get-logger "app.db" :level :debug
                                  :outputs (list (llog:make-stream-output stream)))))

    ;; Parent should only log WARN and above
    (let ((llog:*logger* parent))
      (debug "Parent debug")  ; Should not log
      (warn "Parent warn"))   ; Should log

    ;; Child should log DEBUG and above
    (let ((llog:*logger* child))
      (debug "Child debug")   ; Should log
      (warn "Child warn"))    ; Should log

    (let ((output (get-output-stream-string stream)))
      (is (not (search "Parent debug" output)))
      (is (search "Parent warn" output))
      (is (search "Child debug" output))
      (is (search "Child warn" output)))))

(def-test ensure-logger-hierarchy-creates-parents ()
  "Test that ensure-logger-hierarchy creates all parent loggers."
  (llog::clear-logger-registry)
  (llog::ensure-logger-hierarchy "app.db.query")
  (is (not (null (llog:find-logger "app"))))  ; lint:suppress not-null
  (is (not (null (llog:find-logger "app.db"))))  ; lint:suppress not-null
  (is (not (null (llog:find-logger "app.db.query")))))  ; lint:suppress not-null

;;; Error Handling

(def-test set-logger-level-recursive-invalid-level ()
  "Test that setting invalid level raises error."
  (llog::clear-logger-registry)
  (llog:get-logger "app")
  (signals cl:error
    (llog:set-logger-level-recursive "app" :invalid-level)))
