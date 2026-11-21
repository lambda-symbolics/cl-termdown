(in-package #:termdown/tests)

(-> test-markdown-inline-math () null)
(defun test-markdown-inline-math ()
  "Test symbol conversion and the boundaries of inline math."
  (flet ((render (text)
           (first (markdown-render-inline text 80))))
    (test-assert (equal (render "**\\(\\rho(x) \\ne A*B\\)**")
                        (list (make-span :strong "ρ(x) ≠ A*B")))
                 "inline math inherits emphasis and treats stars literally")
    (test-assert (equal (render "`\\(\\rho(x)\\)`")
                        (list (make-span :code "\\(\\rho(x)\\)")))
                 "code takes precedence over math")
    (dolist (source '("\\(\\rhox + *A*\\)" "\\(\\rho + *A*" "\\(x^{2}\\)"
                      "\\(a\\\\)b\\)"))
      (test-assert (string= (markdown-tests--row-text (render source)) source)
                   "unknown, structured, and incomplete math is literal"))
    (test-assert (string= (markdown-tests--row-text
                           (render "\\\\(x\\) and \\(a\\)\\(b\\)"))
                          "\\(x) and ab")
                 "escaped openers are literal; adjacent math spans render separately"))
  nil)
