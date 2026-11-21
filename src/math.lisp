(in-package #:termdown)

(defparameter *math-symbols*
  '(("alpha" . #\α) ("beta" . #\β) ("gamma" . #\γ) ("delta" . #\δ)
    ("epsilon" . #\ε) ("zeta" . #\ζ) ("eta" . #\η) ("theta" . #\θ)
    ("iota" . #\ι) ("kappa" . #\κ) ("lambda" . #\λ) ("mu" . #\μ)
    ("nu" . #\ν) ("xi" . #\ξ) ("pi" . #\π) ("rho" . #\ρ) ("sigma" . #\σ)
    ("tau" . #\τ) ("upsilon" . #\υ) ("phi" . #\φ) ("chi" . #\χ)
    ("psi" . #\ψ) ("omega" . #\ω) ("Gamma" . #\Γ) ("Delta" . #\Δ)
    ("Theta" . #\Θ) ("Lambda" . #\Λ) ("Xi" . #\Ξ) ("Pi" . #\Π)
    ("Sigma" . #\Σ) ("Upsilon" . #\Υ) ("Phi" . #\Φ) ("Psi" . #\Ψ)
    ("Omega" . #\Ω) ("times" . #\×) ("cdot" . #\·) ("pm" . #\±)
    ("le" . #\≤) ("leq" . #\≤) ("ge" . #\≥) ("geq" . #\≥)
    ("ne" . #\≠) ("neq" . #\≠) ("approx" . #\≈) ("infty" . #\∞)
    ("to" . #\→) ("rightarrow" . #\→) ("in" . #\∈) ("circ" . #\∘))
  "Supported LaTeX control words and their Unicode characters.")

(-> math-symbol-text (string) (option string))
(defun math-symbol-text (text)
  "Translate a flat math expression, or return NIL for unsupported syntax."
  (with-output-to-string (out)
    (loop with index = 0
          while (< index (length text))
          for ch = (char text index)
          do (incf index)
             (cond
               ((char= ch #\\)
                (let* ((end (or (position-if-not #'alpha-char-p text :start index)
                                (length text)))
                       (entry (assoc (subseq text index end) *math-symbols*
                                     :test #'string=)))
                  (unless entry (return-from math-symbol-text nil))
                  (write-char (cdr entry) out)
                  (setf index end)))
               ((or (not (graphic-char-p ch)) (find ch "{}_^$%&#~"))
                (return-from math-symbol-text nil))
               (t (write-char ch out))))))
