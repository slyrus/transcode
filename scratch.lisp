

(eval-when (:compile-toplevel :load-toplevel :execute)
  #+nil (asdf:oos 'asdf:load-op 'transcode)
  (load "transcode.lisp"))


(cl:in-package #:transcode)

(defparameter *test-file*
  #P"/Volumes/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a")

(let ((file *test-file*))
  (read-iso-media-file file))

;;;

(remove-duplicates
 (mapcar #'pathname-type
         (recursively-list-files #P"/Volumes/iTunes_music/Archive/"
                                 :test (lambda (x) (not (apple-cruft-file-p x)))))
 :test 'equal)

(remove-if-not
 (lambda (x) (equal (pathname-type x) "mov"))
 (recursively-list-files #P"/Volumes/iTunes_music/Archive/"
                         :test (lambda (x) (not (apple-cruft-file-p x)))))

(remove-if-not
 (lambda (f) (equal (pathname-type f) NIL))
 (recursively-list-files #P"/Volumes/iTunes_music/Archive/"
                         :test (lambda (x) (not (apple-cruft-file-p x)))))


(let ((files)
      (test (lambda (x) (not (apple-cruft-file-p x)))))
  (apply #'fad:walk-directory  #P"/Volumes/iTunes_music/Archive/Wilco/"
         (lambda (f) (when (fad:file-exists-p f)
                       (push f files)))
         :test test
         :directories nil
         (when test `(:test ,test)))
    (nreverse files))


(remove-if-not
 (lambda (x) (equal (pathname-type x) "m4r"))
 *music-files*)

(remove-duplicates (mapcar #'pathname-type *music-files*) :test 'equal)

(let (x)
  (maphash (lambda (k v) (push (list k (length v)) x))
           *file-type-hash*)
  (nreverse x))

(let (x)
  (maphash (lambda (k v) (push (list k (length v)) x))
           *file-type-hash*)
  (nreverse x))


;; (("mp3" 32673)
;;  ("MP3" 100)
;;  ("m4a" 35840)
;;  ("mp4" 26)
;;  ("pdf" 4)
;;  ("mpg" 4)
;;  ("mov" 2))

#p"/Compilations/Wicked (Original Broadway Cast Recording)/.AppleDouble/01 No One Mourns the Wicked.m4a"

(setf *music-files*
      (recursively-list-files #P"/Volumes/iTunes_music/Archive/Compilations/Wicked (Original Broadway Cast Recording)"
                              :test (lambda (x) (not (apple-cruft-file-p x)))))

(subseq (loop for key being the hash-keys of *codec-hash*
           using (hash-value value)
           collect (list key value))
        0 1000)
