
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

(defun run-audio-decoder (src &key (input-type :apple-lossless))
  (case input-type
    (:apple-lossless (sb-ext:run-program "/usr/bin/alac-decoder"
                                         (list (sb-ext:native-namestring src))
                                         :output :stream
                                         :wait nil))))

(defun run-audio-encoder (input dest &key (output-type :aac))
  (case output-type
    (:aac (sb-ext:run-program "/usr/bin/faac"
                      `("-o"
                        ,(sb-ext:native-namestring dest)
                        "-")
                      :input input))))

(defun convert-audio-directory (srcdir &key input-type output-type)
  (let ((files
         (recursively-list-files
          srcdir
          :test (lambda (x) (not (equal "._" (subseq (pathname-name x) 0 2)))))))
    (mapcar (lambda (src)
              (let ((dest (merge-pathnames (enough-namestring src *audio-source-root-directory*)
                                           *audio-destination-root-directory*)))
                (ensure-directories-exist dest)
                (let* ((decoder (apply #'run-audio-decoder
                                       src
                                       (when input-type (list input-type))))
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
            files)))

