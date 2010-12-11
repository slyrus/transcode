;;;
;;; TODO: get mp3, mp4 or AAC stream as appropriate!
;;;

(eval-when (:compile-toplevel :load-toplevel)
  (asdf:oos 'asdf:load-op 'iso-media)
  (asdf:oos 'asdf:load-op 'cl-fad)) 

(cl:defpackage #:transcode
  (:use #:cl #:iso-media)
  (:export #:run-audio-decoder
           #:run-audio-encoder
           
           #:convert-audio-directory))

(cl:in-package #:transcode)

(defparameter *audio-source-root-directory* #p"/Volume/iTunes_Music/Archive/")
(defparameter *audio-destination-root-directory* #p"/Volume/iTunes_Music/Active/")

(defun run-audio-decoder (src &key (input-type "alac"))
  (case (intern input-type)
    (#.(intern "alac") (sb-ext:run-program "/usr/bin/alac-decoder"
                                         (list (sb-ext:native-namestring src))
                                         :output :stream
                                         :wait nil))
    (t (format *error-output* "~&No decoder for ~s" src))))

(defun run-audio-encoder (input dest &key (output-type "mp4a"))
  (case (intern output-type)
    (#.(intern "mp4a") (sb-ext:run-program "/usr/bin/faac"
                      `("-o"
                        ,(sb-ext:native-namestring dest)
                        "-")
                      :input input))))

(defun is-file-p (f)
  (and (fad:file-exists-p f)
       (not (fad:directory-exists-p f))))

(defun recursively-list-files (dir &key test)
  (let ((files))
    (apply #'fad:walk-directory dir
           (lambda (f) (when (is-file-p f)
                         (push f files)))
           :directories :breadth-first
           (when test `(:test ,test)))
    (nreverse files)))

(defun apple-cruft-file-p (f)
  (let ((name (pathname-name f)))
    (or (equal name ".DS_Store")
        (equal name ".AppleDouble")
        (equal name ".Parent")
        (equal name "_VUC69~7")
        (and (null name)
             (equal (first (last (pathname-directory f))) ".AppleDouble"))
        (and (>= (length name) 2)
             (equal "._" (subseq name 0 2))))))

(defun convert-audio-directory (srcdir &key input-type output-type)
  (let ((files
         (recursively-list-files srcdir :test (lambda (x) (not (apple-cruft-file-p x))))))
    (mapcar (lambda (src)
              (let ((dest (merge-pathnames (enough-namestring src *audio-source-root-directory*)
                                           *audio-destination-root-directory*)))
                (ensure-directories-exist dest)
                (cond
                  ((find (string-downcase (pathname-type src)) '("mp4" "m4a") :test 'equal)
                   (let* ((input-type (or input-type (audio-sample-type (read-iso-media-file src)))) 
                          (decoder (apply #'run-audio-decoder
                                          src
                                          (when input-type (list :input-type input-type))))
                          (encoder (apply #'run-audio-encoder 
                                          (sb-ext:process-output decoder)
                                          dest
                                          (when output-type (list output-type)))))
                     (cond ((not (zerop (sb-ext:process-exit-code decoder)))
                            (with-output-to-string (str)
                              (fad:copy-stream (sb-ext:process-error decoder) str)
                              str)) 
                           ((not (zerop (sb-ext:process-exit-code encoder)))
                            (with-output-to-string (str)
                              (fad:copy-stream (sb-ext:process-error encoder) str)
                              str))
                           (t dest))))
                  ((find (string-downcase (pathname-type src)) '("mp3") :test 'equal)
                   (fad:copy-file src dest))
                  (t
                   (print "Ignoring: ~s" src)))))
            files)))

(defun read-audio-directory-info (srcdir &key debug-limit)
  (let ((files
         (recursively-list-files srcdir :test (lambda (x) (not (apple-cruft-file-p x))))))
    (mapcar (lambda (file)
              (let ((iso-media (read-iso-media-file file)))
                (audio-sample-type iso-media)))
            (if debug-limit
                (subseq files 0 debug-limit)
                files))))


(defvar *music-files* nil)
(defvar *file-type-hash* (make-hash-table :test 'equal))
(defvar *codec-hash* (make-hash-table :test 'equal))

(defun update-music-db (&optional (dir #P"/Volumes/iTunes_music/Archive/"))
  (setf *music-files*
        (recursively-list-files dir
                                :test (lambda (x) (not (apple-cruft-file-p x)))))
  (clrhash *file-type-hash*)
  (clrhash *codec-hash*)
  (map nil
       (lambda (x)
         (block nil
           (handler-bind
               ((error (lambda (condition)
                         (declare (ignore condition))
                         (format *error-output* "~&Ignoring ~s" x)
                         (return))))
             (let ((ft (pathname-type x)))
               (when (find (string-downcase ft) '("mp4" "m4a") :test 'equal)
                 (let ((iso-media (read-iso-media-file x)))
                   (setf (gethash x *codec-hash*)
                         (audio-sample-type iso-media))))
               (setf (gethash ft *file-type-hash*)
                     (cons x (gethash ft *file-type-hash*)))))))
       *music-files*))


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
