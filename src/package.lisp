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
   #:spans-text
   #:make-span
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
   #:spans-width))

(in-package #:termdown)
