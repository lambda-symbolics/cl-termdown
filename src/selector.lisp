(in-package #:termdown)

;;;; -- Grouped Selector Tables --

(-> selector-table--cell-spans (t) list)
(defun selector-table--cell-spans (cell)
  "Return CELL, a string or a list of spans, as a list of spans."
  (if (stringp cell)
      (list (make-span ':plain cell))
      cell))

(-> selector-table--row-spans (list keyword) list)
(defun selector-table--row-spans (spans role)
  "Return SPANS with every :PLAIN span given the row's ROLE."
  (loop for span in spans
        collect (if (eq (span-role span) ':plain)
                    (make-span role (span-text span))
                    span)))

(-> selector-table--padded (list integer keyword) list)
(defun selector-table--padded (spans width role)
  "Return SPANS fitted to exactly WIDTH cells, padded with a ROLE span."
  (let* ((fitted (fit-spans spans width))
         (padding (- width (spans-width fitted))))
    (if (plusp padding)
        (append fitted
                (list (make-span role (make-string padding
                                                   :initial-element #\Space))))
        fitted)))

(-> selector-table-rows
    (clinedi:selector integer
     &key (:label-function function) (:group-function function)
          (:column-functions list) (:gap-width (integer 0))
          (:selected-marker string) (:marker string)
          (:selected-marker-role keyword) (:marker-role keyword)
          (:label-role keyword) (:heading-role keyword)
          (:selected-role keyword) (:unselected-role keyword))
    list)
(defun selector-table-rows
    (selector width &key (label-function #'princ-to-string)
                         (group-function (constantly nil))
                         (column-functions nil)
                         (gap-width 2)
                         (selected-marker "▸ ")
                         (marker "  ")
                         (selected-marker-role ':brand)
                         (marker-role ':dim)
                         (label-role ':plain)
                         (heading-role ':strong)
                         (selected-role ':plain)
                         (unselected-role ':dim))
  "Return span rows presenting SELECTOR's visible window as a grouped table.

SELECTOR must use the :VERTICAL arrangement; it is arranged for WIDTH cells and
each row is clipped to WIDTH. A row starts with SELECTED-MARKER or MARKER and
the candidate's LABEL-FUNCTION string, followed by one cell per function in
COLUMN-FUNCTIONS, each returning a string or a list of spans. Column widths
follow the visible rows, GAP-WIDTH cells apart, and a column empty in every
visible row is left out. Every column but the last is padded to its width.
:PLAIN spans in the columns take SELECTED-ROLE in the selected row and
UNSELECTED-ROLE elsewhere; other roles are kept.

When GROUP-FUNCTION returns a group different from the previous row's, a
HEADING-ROLE row naming it precedes the candidate, after an empty row unless it
is the first group."
  (unless (eq (clinedi:selector-arrangement selector) ':vertical)
    (error 'type-error :datum (clinedi:selector-arrangement selector)
                       :expected-type '(eql :vertical)))
  (let* ((marker-width (max (text-cell-width selected-marker) (text-cell-width marker)))
         (indexes (mapcar #'first
                          (clinedi:selector-arrange
                           selector width
                           :width-function (lambda (item)
                                             (text-cell-width
                                              (funcall label-function item))))))
         (items (clinedi:selector-items selector))
         (entries (mapcar (lambda (index) (nth index items)) indexes))
         (column-cells
           (remove-if (lambda (cells)
                        (every (lambda (spans) (zerop (spans-width spans))) cells))
                      (loop for function in column-functions
                            collect (loop for entry in entries
                                          collect (selector-table--cell-spans
                                                   (funcall function entry))))))
         (widths
           (column-widths (loop for entry in entries
                                for row from 0
                                collect (cons (funcall label-function entry)
                                              (loop for cells in column-cells
                                                    collect (spans-text (nth row cells)
                                                                        :single-line-p t))))
                          (max 0 (- width marker-width))
                          :gap-width gap-width
                          :minimum-widths (cons 1 (make-list (length column-cells)
                                                             :initial-element 0))))
         (gap (make-string gap-width :initial-element #\Space))
         (indent (make-string marker-width :initial-element #\Space))
         (previous-group nil))
    (loop for index in indexes
          for entry in entries
          for row from 0
          for selected-p = (= index (clinedi:selector-selection selector))
          for role = (if selected-p selected-role unselected-role)
          for group = (funcall group-function entry)
          for heading-p = (and group (not (equal group previous-group)))
          append (append
                  (when (and heading-p previous-group)
                    (list nil))
                  (when heading-p
                    (list (fit-spans (list (make-span heading-role
                                                      (concatenate 'string indent group)))
                                     width)))
                  (list
                   (fit-spans
                    (append
                     (list (make-span (if selected-p selected-marker-role marker-role)
                                      (if selected-p selected-marker marker))
                           (make-span label-role
                                      (fit-text (funcall label-function entry)
                                                (first widths))))
                     (loop for cells in column-cells
                           for column-width in (rest widths)
                           for column from 1
                           for spans = (selector-table--row-spans (nth row cells) role)
                           when (plusp column-width)
                             append (cons (make-span ':plain gap)
                                          (if (= column (length column-cells))
                                              (fit-spans spans column-width)
                                              (selector-table--padded
                                               spans column-width role)))))
                    width)))
          do (setf previous-group group))))
