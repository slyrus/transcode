
(in-package #:transcode)

(cl:defpackage #:transcode-scratch
  (:use #:cl #:transcode))

(cl:in-package #:transcode-scratch)

(defparameter *test-file*
  #P"/mnt/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a")

(let ((file *test-file*))
  (read-iso-media-file file))

(let ((file *test-file*))
  (iso-media-box-data
   (first
    (remove-if-not (lambda (box)
                     (equalp (iso-media-box-type box)
                             (map 'vector #'char-code "moov")))
                   (read-iso-media-file file)))))

(let ((file *test-file*))
  (find-box-type "mvhd"
                 (iso-media-box-data
                  (find-box-type "moov" (read-iso-media-file file)))))

(let ((file *test-file*))
  (map 'string #'code-char
       (iso-media-box-data
        (find-box-type "udta"
                       (iso-media-box-data
                        (find-box-type "moov" (read-iso-media-file file)))))))

(let ((file *test-file*))
  (iso-media-box-data
   (find-box-type "moov" (read-iso-media-file file))))

