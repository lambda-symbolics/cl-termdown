(asdf:defsystem #:cl-termdown
  :description "A terminal Markdown parser and semantic span renderer."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :serial t
  :depends-on (#:clinedi
               #:colordiff
               #:colorlisp
               #:serapeum)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "spans")
                             (:file "layout")
                             (:file "markdown"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:cl-termdown/tests))))

(asdf:defsystem #:cl-termdown/tests
  :description "Tests for cl-termdown."
  :depends-on (#:cl-termdown)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "tests"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:termdown/tests '#:run-tests)))
