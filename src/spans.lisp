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

(-> item-role ((or span widget)) keyword)
(defun item-role (item)
  "Return the semantic role of a span or widget ITEM."
  (if (widget-p item) (widget-role item) (span-role item)))

(-> item-text ((or span widget)) string)
(defun item-text (item)
  "Return the untrusted text or label of ITEM."
  (if (widget-p item) (widget-label item) (span-text item)))

(-> item-with-text ((or span widget) string &key (:role keyword)) (or span widget))
(defun item-with-text (item text &key (role (item-role item)))
  "Return ITEM with TEXT and ROLE, retaining a widget's original action."
  (if (widget-p item)
      (make-widget role text (widget-action item))
      (make-span role text)))

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

(-> termdown--cell-offsets (string) (values vector vector))
(defun termdown--cell-offsets (text)
  "Return start and end cell offsets for every character boundary in TEXT.
Boundaries inside a grapheme cover that complete grapheme. Newlines occupy no cells."
  (let ((starts (make-array (1+ (length text)) :initial-element 0))
        (ends   (make-array (1+ (length text)) :initial-element 0))
        (cells  0))
    (loop for start = 0 then end
          while (< start (length text))
          for end = (clinedi:grapheme-next-boundary text start)
          for width = (if (char= (char text start) #\Newline)
                          0
                          (clinedi:grapheme-cell-width text start end))
          do (loop for index from start below end
                   do (setf (aref starts index) cells
                            (aref ends (1+ index)) (+ cells width)))
             (incf cells width)
             (setf (aref starts end) cells))
    (values starts ends)))

(-> widget-regions (list &key (:start integer) (:unit (member :character :cell))) list)
(defun widget-regions (items &key (start 0) (unit ':character))
  "Return the (START END ACTION) region of each widget among ITEMS.

UNIT defaults to :CHARACTER for offsets into SPANS-TEXT. Use :CELL for
terminal hit testing. Cell regions cover whole graphemes even when their
characters cross item boundaries. START shifts either unit; empty labels
and labels without visible cells have no cell region."
  (let* ((texts    (mapcar (lambda (item) (sanitize-text (item-text item))) items))
         (text     (apply #'concatenate 'string texts))
         (position 0)
         (regions  nil))
    (multiple-value-bind (starts ends)
        (when (eq unit ':cell) (termdown--cell-offsets text))
      (loop for item in items
            for label in texts
            for end = (+ position (length label))
            do (when (and (widget-p item) (< position end))
                 (let ((a (if starts (aref starts position) position))
                       (b (if ends (aref ends end) end)))
                   (when (< a b)
                     (push (list (+ start a) (+ start b) (widget-action item)) regions))))
               (setf position end)))
    (nreverse regions)))

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
  "Return the single-row cell width of joined, sanitized SPANS and widgets."
  (text-cell-width (spans-text spans :single-line-p t)))

(-> termdown--spans-subseq (list integer integer) list)
(defun termdown--spans-subseq (spans start end)
  "Return the character range from START to END within styled SPANS."
  (let ((position 0)
        (result   nil))
    (dolist (span spans (nreverse result))
      (let* ((text       (item-text span))
             (span-end   (+ position (length text)))
             (part-start (max start position))
             (part-end   (min end span-end)))
        (when (< part-start part-end)
          (push (item-with-text
                 span
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

Retain semantic roles, widget actions and complete graphemes, including
graphemes whose characters occur in adjacent items. Use PRESENTATION-SPANS
when an explicit display-only projection is needed."
  (let* ((safe    (mapcar (lambda (span)
                            (item-with-text span
                                            (sanitize-text (item-text span)
                                                           :single-line-p t)))
                          spans))
         (text    (apply #'concatenate 'string (mapcar #'item-text safe)))
         (visible (clinedi:text-cell-prefix text (max 0 maximum-width))))
    (termdown--spans-subseq safe 0 (length visible))))

(-> pad-spans (list integer &key (:role keyword)) list)
(defun pad-spans (spans width &key (role ':plain))
  "Clip or pad SPANS to WIDTH cells, using ROLE for padding and retaining actions."
  (let* ((width     (max 0 width))
         (clipped   (fit-spans spans width))
         (remaining (- width (spans-width clipped))))
    (if (plusp remaining)
        (append clipped (list (make-span role (make-string remaining :initial-element #\Space))))
        clipped)))

(-> split-first-line (list) (values list list))
(defun split-first-line (spans)
  "Return SPANS before their first newline and the rest from that newline on.
A split widget retains its original action in both pieces."
  (loop for (item . more) on spans
        for text = (item-text item)
        for newline = (position #\Newline text)
        if newline
          return (values
                  (append head (when (plusp newline)
                                 (list (item-with-text item (subseq text 0 newline)))))
                  (cons (item-with-text item (subseq text newline)) more))
        else
          collect item into head
        finally (return (values head nil))))

(-> render-spans (list &key (:style-function (option function))) string)
(defun render-spans (spans &key style-function)
  "Return sanitized presentation for SPANS, adding OSC 8 links when styled.

Link widgets are wrapped with CLINEDI:ANSI-HYPERLINK after styling.  A style
function should recognize the :LINK role and avoid adding a second hyperlink
around its text."
  (with-output-to-string (stream)
    (dolist (item spans)
      (let* ((widget       (widget-p item))
             (span         (if widget (make-span (widget-role item) (widget-label item)) item))
             (text         (sanitize-text (span-text span)))
             (presentation (if style-function
                               (funcall style-function (span-role span) text)
                               text)))
        (if (and style-function widget
                 (consp (widget-action item))
                 (eq (first (widget-action item)) ':open-url))
            (write-string (ansi-hyperlink presentation
                                           (second (widget-action item)))
                          stream)
            (write-string presentation stream))))))
