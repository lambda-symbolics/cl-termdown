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
                             (:file "markdown")))))
