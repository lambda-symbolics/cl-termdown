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
    (dolist (source '("\\(\\rhox + *A*\\)" "\\(\\rho + *A*" "\\(x^{2\\)"
                      "\\(a\\\\)b\\)"))
      (test-assert (string= (markdown-tests--row-text (render source)) source)
                   "unknown, structured, and incomplete math is literal"))
    (test-assert (string= (markdown-tests--row-text
                           (render "\\\\(x\\) and \\(a\\)\\(b\\)"))
                          "\\(x) and ab")
                 "escaped openers are literal; adjacent math spans render separately"))
  (dolist (pair (list (cons "A_{ij}^2" "Aᵢⱼ²") (cons "x^{k+1}" "x^(k+1)")
                      (cons "\\sqrt{\\frac{x_1}{y^2}}" "√(((x₁)/(y²)))")))
    (test-assert (equal (termdown::math-symbol-text (car pair)) (cdr pair)) "structured equation"))
  (dolist (bad (list "x_1_2" "x^" "a}" "\\frac{1}" "\\sqrt{}" "\\sqrt{\\unknown}"
                     (format nil "~{~A~}x~{~A~}" (make-list 40 :initial-element "\\sqrt{")
                             (make-list 40 :initial-element "}"))))
    (test-assert (null (termdown::math-symbol-text bad)) "reject incomplete or unsupported math"))
  nil)
