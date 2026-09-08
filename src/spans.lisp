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


(-> spans-text (list &key (:single-line-p boolean)) string)
(defun spans-text (spans &key single-line-p)
  "Return sanitized text from SPANS, optionally flattened onto one line."
  (with-output-to-string (stream)
    (dolist (span spans)
      (write-string (sanitize-text (span-text span)
                                   :single-line-p single-line-p)
                    stream))))

(-> fit-spans (list integer) list)
(defun fit-spans (spans maximum-width)
  "Return sanitized single-line SPANS clipped to MAXIMUM-WIDTH cells.

Retain semantic roles and complete graphemes, including graphemes whose
characters occur in adjacent spans."
  (let* ((safe (mapcar (lambda (span)
                         (make-span (span-role span)
                                    (sanitize-text (span-text span)
                                                   :single-line-p t)))
                       spans))
         (text (apply #'concatenate 'string (mapcar #'span-text safe)))
         (visible (clinedi:text-cell-prefix text (max 0 maximum-width))))
    (termdown--spans-subseq safe 0 (length visible))))

(-> render-spans (list &key (:style-function (option function))) string)
(defun render-spans (spans &key style-function)
  "Return presentation for SPANS using an optional trusted STYLE-FUNCTION.

STYLE-FUNCTION receives the semantic role and sanitized text of each span,
and returns its trusted presentation. Without it, return plain visible text."
  (with-output-to-string (stream)
    (dolist (span spans)
      (let ((text (sanitize-text (span-text span))))
        (write-string (if style-function
                          (funcall style-function (span-role span) text)
                          text)
                      stream)))))
