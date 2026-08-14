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
   #:make-span
   #:markdown-render-inline
   #:markdown-render-line
   #:markdown-render-partial
   #:markdown-renderer
   #:markdown-renderer-create
   #:markdown-renderer-width
   #:span
   #:span-p
   #:span-role
   #:span-text
   #:spans-width))

(in-package #:termdown)
