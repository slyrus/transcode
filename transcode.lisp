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

(defparameter *audio-source-root-directory* #P"/Volumes/iTunes_music/Archive/")
(defparameter *audio-destination-root-directory* #p"/Volumes/iTunes_music/Active/")

(defun run-audio-decoder (src &key (input-type "alac"))
  (case (intern input-type)
    (#.(intern "alac")
       (sb-ext:run-program "/usr/bin/alac-decoder"
                           (list (sb-ext:native-namestring src))
                           :output :stream
                           :wait nil))
    (#.(intern "mp4a")
       (sb-ext:run-program "/usr/bin/faad"
                           (list (sb-ext:native-namestring src))
                           :output :stream
                           :wait nil))
    (t (format *error-output* "~&No decoder for ~s" src))))

(defun run-audio-encoder (input dest &key (output-type "mp4a"))
  (case (intern output-type)
    (#.(intern "mp4a")
       (sb-ext:run-program "/usr/bin/faac"
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
              (print src)
              (let ((dest (merge-pathnames (enough-namestring src *audio-source-root-directory*)
                                           *audio-destination-root-directory*)))
                (print dest)
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


