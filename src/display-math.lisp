(in-package #:termdown)

;;;; -- Terminal display math --

(-> display-math-trim (string) string)
(defun display-math-trim (text)
  (string-trim '(#\Space #\Tab #\Return) text))

(-> display-math-closed-p (string) boolean)
(defun display-math-closed-p (line)
  "Recognize a final unescaped backslash and right bracket."
  (let ((end (length line)))
    (and (> end 1) (char= (char line (1- end)) #\])
         (oddp (loop for i downfrom (- end 2) to 0
                     while (char= (char line i) #\\) count t)))))

(-> display-math-split (string string) list)
(defun display-math-split (delimiter text)
  "Split TEXT on literal DELIMITER, retaining empty fields."
  (loop for start = 0 then (+ end (length delimiter))
        for end = (search delimiter text :start2 start)
        collect (subseq text start end) while end))

(-> display-math-matrix (string string integer) list)
(defun display-math-matrix (prefix body width)
  "Return rectangular matrix lines, or NIL if any cell or width is unsupported."
  (let ((rows nil) (widths nil))
    (dolist (raw-row (display-math-split "\\\\" body))
      (let ((row (loop for cell in (display-math-split "&" raw-row)
                       for source = (display-math-trim cell)
                       for text = (and (plusp (length source)) (math-symbol-text source))
                       unless text do (return-from display-math-matrix nil)
                       collect text)))
        (unless widths (setf widths (make-list (length row) :initial-element 0)))
        (unless (= (length row) (length widths)) (return-from display-math-matrix nil))
        (loop for cell in row for tail on widths
              do (setf (car tail) (max (car tail) (text-cell-width cell))))
        (push row rows)))
    (let* ((indent (text-cell-width prefix))
           (row-width (+ indent 2 (* 2 (1- (length widths))) (reduce #'+ widths))))
      (when (and (<= row-width width) (<= (* row-width (length rows)) 65536))
        (loop for (row . rest) on (nreverse rows) for first = t then nil
              for brackets = (cond ((and first (null rest)) "[]") (first "⎡⎤")
                                   (rest "⎢⎥") (t "⎣⎦"))
              collect (with-output-to-string (s)
                        (write-string (if first prefix (make-string indent :initial-element #\Space)) s)
                        (write-char (char brackets 0) s)
                        (loop for (cell . more) on row for w in widths
                              do (write-string (fit-text cell w) s)
                                 (when more (write-string "  " s)))
                        (write-char (char brackets 1) s)))))))

(-> display-math-lines (list integer) list)
(defun display-math-lines (source width)
  "Translate one closed block. Join physical lines before interpreting its body."
  (let* ((joined (display-math-trim (format nil "~{~A~^ ~}" source)))
         (body   (display-math-trim (subseq joined 2 (- (length joined) 2))))
         (begin  "\\begin{bmatrix}") (end "\\end{bmatrix}")
         (start  (search begin body)))
    (cond (start
           (let ((stop (search end body :start2 (+ start (length begin))))
                 (prefix (if (zerop start) "" (math-symbol-text (subseq body 0 start)))))
             (when (and prefix stop (= (+ stop (length end)) (length body)))
               (display-math-matrix prefix (subseq body (+ start (length begin)) stop) width))))
          ((plusp (length body))
           (let ((text (math-symbol-text body)))
             (when (and text (<= (text-cell-width text) width)) (list text)))))))
