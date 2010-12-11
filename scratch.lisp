

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+nil (asdf:oos 'asdf:load-op 'transcode)
  (load "transcode.lisp"))


(cl:defpackage #:transcode-scratch
  (:use #:cl #:transcode #:iso-media))

(cl:in-package #:transcode-scratch)

(defparameter *test-file*
  #P"/Volumes/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a")

(let ((file *test-file*))
  (read-iso-media-file file))

