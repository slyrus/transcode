
(in-package #:transcode)

(cl:defpackage #:transcode-scratch
  (:use #:cl #:transcode))

(cl:in-package #:transcode-scratch)

(defparameter *test-file*
  #P"/mnt/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a")

(let ((file *test-file*))
  (map 'list
       (lambda (x)
         (list (first x)
               (map 'string #'code-char (second x))
               (length (third x))))
       (read-iso-media-file file)))

(defun iso-box-info (iso-boxes)
  (map 'list
       (lambda (x)
         (list (map 'string #'code-char (second x))
               (length (third x))))
       iso-boxes))

(let ((file *test-file*))
  (iso-box-info (read-iso-media-file file)))

(let ((file *test-file*))
  (remove-if-not (lambda (box)
                   (equalp (second box)
                           (map 'vector #'char-code "moov")))
                 (read-iso-media-file file)))

(let ((file *test-file*))
  (flex:with-input-from-sequence
      (stream 
       (third
        (car (remove-if-not (lambda (box)
                              (equalp (second box)
                                      (map 'vector #'char-code "moov")))
                            (read-iso-media-file file))))) 
    (iso-box-info (read-iso-media-stream stream))))

(let ((file *test-file*))
  (flex:with-input-from-sequence
      (stream 
       (third
        (car (remove-if-not (lambda (box)
                              (equalp (second box)
                                      (map 'vector #'char-code "moov")))
                            (read-iso-media-file file))))) 
    (do-iso-media-stream stream #'read-iso-media-box-data)))

(let ((file *test-file*))
  (do-iso-media-file file (lambda (type size stream)
                            (cond
                              ((equalp type (map 'vector #'char-code "moov"))
                               ())
                              (t (read-iso-media-box-data type (- size 8) stream)
                                 (list (map 'string #'code-char type) size))))))


