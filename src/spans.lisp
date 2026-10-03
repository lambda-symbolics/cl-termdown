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

;;;; -- Widgets --

(defstruct (widget
            (:constructor make-widget (role label action))
            (:copier nil))
  "A LABEL shown like a span of ROLE that carries an application ACTION.

Span functions present a widget as its label. WIDGET-REGIONS records where
each label lands in the presented text, so the application can map a
character offset, such as a mouse click, back to the ACTION."
  (role   :plain :type keyword :read-only t)
  (label  ""     :type string  :read-only t)
  (action nil    :read-only t))

(-> presentation-spans (list) list)
(defun presentation-spans (items)
  "Return ITEMS with every widget replaced by the span showing its label."
  (if (some #'widget-p items)
      (mapcar (lambda (item)
                (if (widget-p item)
                    (make-span (widget-role item) (widget-label item))
                    item))
              items)
      items))

(-> widget-regions (list &key (:start integer)) list)
(defun widget-regions (items &key (start 0))
  "Return the (START END ACTION) region of each widget among ITEMS.

Offsets count characters of the text SPANS-TEXT presents for ITEMS, plus
START, so regions of separately presented rows can be joined after their
text is. Widgets with empty labels have no region."
  (let ((position start)
        (regions nil))
    (dolist (item items (nreverse regions))
      (let ((length (length (sanitize-text (if (widget-p item)
                                               (widget-label item)
                                               (span-text item))))))
        (when (and (widget-p item) (plusp length))
          (push (list position (+ position length) (widget-action item))
                regions))
        (incf position length)))))

(-> shift-regions (list integer) list)
(defun shift-regions (regions offset)
  "Return REGIONS moved later by OFFSET characters."
  (if (zerop offset)
      regions
      (mapcar (lambda (region)
                (destructuring-bind (start end action) region
                  (list (+ start offset) (+ end offset) action)))
              regions)))

(-> region-action (list integer) t)
(defun region-action (regions offset)
  "Return the action of the region in REGIONS covering character OFFSET, or NIL."
  (loop for (start end action) in regions
        when (and (<= start offset) (< offset end))
          return action))


;;;; -- Span Presentation --

(-> spans-width (list) (integer 0))
(defun spans-width (spans)
  "Return the total terminal cell width of SPANS and widgets."
  (loop for span in (presentation-spans spans)
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
  "Return sanitized text from SPANS and widgets, optionally flattened onto one line."
  (with-output-to-string (stream)
    (dolist (span (presentation-spans spans))
      (write-string (sanitize-text (span-text span)
                                   :single-line-p single-line-p)
                    stream))))

(-> fit-spans (list integer) list)
(defun fit-spans (spans maximum-width)
  "Return sanitized single-line SPANS clipped to MAXIMUM-WIDTH cells.

Retain semantic roles and complete graphemes, including graphemes whose
characters occur in adjacent spans. Widgets become the spans of their labels."
  (let* ((safe (mapcar (lambda (span)
                         (make-span (span-role span)
                                    (sanitize-text (span-text span)
                                                   :single-line-p t)))
                       (presentation-spans spans)))
         (text (apply #'concatenate 'string (mapcar #'span-text safe)))
         (visible (clinedi:text-cell-prefix text (max 0 maximum-width))))
    (termdown--spans-subseq safe 0 (length visible))))

(-> render-spans (list &key (:style-function (option function))) string)
(defun render-spans (spans &key style-function)
  "Return presentation for SPANS and widgets using an optional trusted STYLE-FUNCTION.

STYLE-FUNCTION receives the semantic role and sanitized text of each span,
and returns its trusted presentation. Without it, return plain visible text."
  (with-output-to-string (stream)
    (dolist (span (presentation-spans spans))
      (let ((text (sanitize-text (span-text span))))
        (write-string (if style-function
                          (funcall style-function (span-role span) text)
                          text)
                      stream)))))
