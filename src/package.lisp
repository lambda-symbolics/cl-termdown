(defpackage #:termdown
  (:use #:cl)
  (:import-from #:clinedi
                #:sanitize-text
                #:text-cell-width
                #:wrap-text)
  (:import-from #:colordiff
                #:highlight-lines)
  (:import-from #:colorlisp
                #:language
                #:language-find)
  (:import-from #:serapeum
                #:->)
  (:export
   #:column-widths
   #:fit-text
   #:fit-spans
   #:render-spans
   #:selector-table-rows
   #:spans-text
   #:make-span
   #:make-widget
   #:markdown-render-inline
   #:markdown-render-line
   #:markdown-render-partial
   #:markdown-renderer
   #:markdown-renderer-closed-code-source
   #:markdown-renderer-create
   #:markdown-renderer-width
   #:span
   #:span-p
   #:span-role
   #:span-text
   #:spans-width
   #:presentation-spans
   #:region-action
   #:shift-regions
   #:widget
   #:widget-action
   #:widget-label
   #:widget-p
   #:widget-regions
   #:widget-role))

(in-package #:termdown)
