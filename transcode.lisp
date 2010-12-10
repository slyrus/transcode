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

(defparameter *audio-source-root-directory* #p"/mnt/iTunes_Music/")
(defparameter *audio-destination-root-directory* #p"/mnt/iTunes_Music/AAC/")

(defun run-audio-decoder (src &key (input-type "alac"))
  (case (intern input-type)
    (#.(intern "alac") (sb-ext:run-program "/usr/bin/alac-decoder"
                                         (list (sb-ext:native-namestring src))
                                         :output :stream
                                         :wait nil))))

(defun run-audio-encoder (input dest &key (output-type "mp4a"))
  (case (intern output-type)
    (#.(intern "mp4a") (sb-ext:run-program "/usr/bin/faac"
                      `("-o"
                        ,(sb-ext:native-namestring dest)
                        "-")
                      :input input))))

 (defun recursively-list-files (dir &key test)
   (let ((files))
     (apply #'fad:walk-directory dir (lambda (f) (push f files))
            (append '(:directories nil)
                    (when test `(:test ,test))))
     (nreverse files)))

(defun convert-audio-directory (srcdir &key input-type output-type)
  (let ((files
         (recursively-list-files
          srcdir
          :test (lambda (x) (not (equal "._" (subseq (pathname-name x) 0 2)))))))
    (mapcar (lambda (src)
              (let ((dest (merge-pathnames (enough-namestring src *audio-source-root-directory*)
                                           *audio-destination-root-directory*)))
                (ensure-directories-exist dest)
                (cond
                  ((equal (pathname-type src) "mp3")
                   nil)
                  ((equal (pathname-type src) "flac")
                   nil)
                  ((find (pathanme-type src) '("mp4" "m4a") :test 'equal)))
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
                        (t dest)))))
            (subseq files 0 1))))

(defun read-audio-directory-info (srcdir &key debug-limit)
  (let ((files
         (recursively-list-files
          srcdir
          :test (lambda (x) (not (equal "._" (subseq (pathname-name x) 0 2)))))))
    (mapcar (lambda (file)
              (let ((iso-media (read-iso-media-file file)))
                (audio-sample-type iso-media)))
            (if debug-limit
                (subseq files 0 debug-limit)
                files))))

(defun temp-list-files (srcdir &key debug-limit)
  (let ((files
         (recursively-list-files
          srcdir
          :test (lambda (x) (not (equal "._" (subseq (pathname-name x) 0 2)))))))
    (mapcar (lambda (file)
              (pathname-type file))
            (if debug-limit
                (subseq files 0 debug-limit)
                files))))