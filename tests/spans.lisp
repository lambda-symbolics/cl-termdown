(in-package #:termdown/tests)

(defun test-span-presentation ()
  "Test safe rendering, role preservation, grapheme clipping and cell limits."
  (let* ((accent (string (code-char #x301)))
         (spans (list (make-span ':plain "e") (make-span ':strong accent)
                      (make-span ':code "界z"))))
    (loop for width from -1 to 5
          for expected in (list "" "" (concatenate 'string "e" accent)
                                (concatenate 'string "e" accent)
                                (concatenate 'string "e" accent "界")
                                (concatenate 'string "e" accent "界z")
                                (concatenate 'string "e" accent "界z"))
          for fitted = (termdown:fit-spans spans width)
          do (test-assert (string= (termdown:spans-text fitted) expected)
                          "fitting retains complete cross-span graphemes")
             (test-assert (<= (text-cell-width (termdown:spans-text fitted))
                              (max 0 width))
                          "fitting obeys the cell budget")))
  (let* ((unsafe (format nil "a~C[31mb~%c" #\Escape))
         (spans (list (make-span ':strong unsafe)))
         (seen nil)
         (rendered
           (termdown:render-spans
            spans :style-function
            (lambda (role text)
              (push (cons role text) seen)
              (concatenate 'string "[" text "]")))))
    (test-assert (equal seen (list (cons ':strong (clinedi:sanitize-text unsafe))))
                 "style callbacks receive semantic roles and sanitized text")
    (test-assert (string= rendered
                          (concatenate 'string "[" (clinedi:sanitize-text unsafe) "]"))
                 "trusted presentation surrounds only safe text")
    (test-assert (string= (termdown:render-spans spans)
                          (termdown:spans-text spans))
                 "unstyled rendering returns sanitized text")
    (test-assert (not (find #\Newline (termdown:spans-text spans :single-line-p t)))
                 "single-line presentation removes row breaks"))
  t)
