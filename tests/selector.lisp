(in-package #:termdown/tests)

;;;; -- Grouped Selector Table Tests --

(defun selector-tests--rows (entries width &key (selection 0) (visible-count 10))
  "Return table rows for ENTRIES, plists of :name :group :tally and :description."
  (let ((selector (clinedi:make-selector :items entries :visible-count visible-count)))
    (clinedi:selector-move selector selection)
    (termdown:selector-table-rows
     selector width
     :label-function (lambda (entry) (getf entry :name))
     :group-function (lambda (entry) (getf entry :group))
     :column-functions (list (lambda (entry) (or (getf entry :tally) ""))
                             (lambda (entry) (or (getf entry :description-spans)
                                                 (getf entry :description)
                                                 "")))
     :gap-width 2
     :label-role ':user)))

(defun selector-tests--text (row)
  "Return ROW's visible text, or NIL for an empty separator row."
  (and row (termdown:spans-text row)))

(defun test-selector-table-rows ()
  "Test grouped headings, aligned columns, empty columns, roles and clipping."
  (let* ((entries (list (list :name "alpha" :group "Models" :tally "3" :description "first")
                        (list :name "beta" :group "Models" :description "second")
                        (list :name "gamma-long" :group "Tools" :tally "12"
                              :description-spans (list (make-span ':code "code")
                                                       (make-span ':plain " text")))
                        (list :name "delta" :description "ungrouped")))
         (rows (selector-tests--rows entries 60 :selection 1)))
    (test-assert (equal (mapcar #'selector-tests--text rows)
                        (list "  Models"
                              "  alpha       3   first"
                              "▸ beta            second"
                              nil
                              "  Tools"
                              "  gamma-long  12  code text"
                              "  delta           ungrouped"))
                 "groups get headings and visible columns align on their widest cells")
    (let ((heading (first rows))
          (selected (third rows))
          (spanned (sixth rows)))
      (test-assert (equal (span-role (first heading)) ':strong)
                   "group headings use the heading role")
      (test-assert (and (equal (first selected) (make-span ':brand "▸ "))
                        (eq (span-role (second selected)) ':user)
                        (eq (span-role (first (last selected))) ':plain)
                        (eq (span-role (first (second rows))) ':dim)
                        (eq (span-role (first (last (second rows)))) ':dim))
                   "the selected row keeps plain cells plain and other rows dim them")
      (test-assert (find (make-span ':code "code") spanned :test #'equal)
                   "styled cell spans keep their own roles")))
  (let ((rows (selector-tests--rows (list (list :name "one" :description "a")
                                          (list :name "two" :description "b"))
                                    40)))
    (test-assert (equal (mapcar #'selector-tests--text rows)
                        (list "▸ one  a" "  two  b"))
                 "a column empty in every visible row is left out"))
  (let ((rows (selector-tests--rows (list (list :name "name" :description
                                                "a long description that does not fit"))
                                    20)))
    (test-assert (every (lambda (row) (<= (termdown:spans-width row) 20)) rows)
                 "every row fits the width")
    (test-assert (string= (selector-tests--text (first rows)) "▸ name  a long descr")
                 "the last column is clipped rather than wrapped"))
  (let ((rows (selector-tests--rows (loop for index below 6
                                          collect (list :name (format nil "item ~D" index)))
                                    30 :selection 5 :visible-count 3)))
    (test-assert (equal (mapcar #'selector-tests--text rows)
                        (list "  item 3" "  item 4" "▸ item 5"))
                 "only the selector's visible window is rendered"))
  (test-assert (handler-case
                   (progn (termdown:selector-table-rows
                           (clinedi:make-selector :items '("a") :arrangement ':grid) 20)
                          nil)
                 (type-error () t))
               "a grid selector is rejected"))
