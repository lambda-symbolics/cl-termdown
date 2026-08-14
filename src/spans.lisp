(in-package #:termdown)

;;;; -- Fundamental Types --

(deftype option (inner-type)
  "A value that is either NIL or an instance of INNER-TYPE."
  `(or null ,inner-type))


;;;; -- Semantic Spans --

(-> span-p (t) boolean)
(defun span-p (value)
  "Return true when VALUE is a semantic role and text cons."
  (and (consp value)
       (keywordp (first value))
       (stringp (rest value))))

(deftype span ()
  "A semantic role and text cons."
  '(satisfies span-p))

(-> make-span (keyword string) span)
(defun make-span (role text)
  "Return one semantic span pairing ROLE with TEXT."
  (cons role text))

(-> span-role (span) keyword)
(defun span-role (span)
  "Return SPAN's semantic role."
  (first span))

(-> span-text (span) string)
(defun span-text (span)
  "Return SPAN's text."
  (rest span))

(-> spans-width (list) (integer 0))
(defun spans-width (spans)
  "Return the total terminal cell width of SPANS."
  (loop for span in spans
        sum (text-cell-width (span-text span))))

(-> termdown--spans-subseq (list integer integer) list)
(defun termdown--spans-subseq (spans start end)
  "Return the character range from START to END within styled SPANS."
  (let ((position 0)
        (result nil))
    (dolist (span spans (nreverse result))
      (let* ((text (span-text span))
             (span-end (+ position (length text)))
             (part-start (max start position))
             (part-end (min end span-end)))
        (when (< part-start part-end)
          (push (make-span
                 (span-role span)
                 (subseq text
                         (- part-start position)
                         (- part-end position)))
                result))
        (setf position span-end)))))
