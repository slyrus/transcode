
(eval-when (:compile-toplevel :load-toplevel)
  (asdf:oos 'asdf:load-op 'cl-fad)
  (asdf:oos 'asdf:load-op 'flexi-streams)) 

(cl:defpackage #:transcode
  (:use #:cl))

(cl:in-package #:transcode)

(defparameter *audio-source-root-directory* #p"/mnt/iTunes_Music/")
(defparameter *audio-destination-root-directory* #p"/mnt/iTunes_Music/AAC/")

(defun recursively-list-files (dir &key test)
  (let ((files))
    (apply #'fad:walk-directory dir (lambda (f) (push f files))
           (append '(:directories nil)
                   (when test `(:test ,test))))
    (nreverse files)))

(defun read-n-bytes (stream n)
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (let ((bytes-read (read-sequence buf stream)))
      (values buf bytes-read))))

(defun read-32-bit-int (stream)
  (multiple-value-bind (buf bytes-read) 
      (read-n-bytes stream 4)
    (when (= bytes-read 4)
      (reduce (lambda (x y) (+ (ash x 8) y)) buf))))

(defun read-iso-media-box (stream)
  (let* ((box-size (read-32-bit-int stream)))
    (when box-size
      (let ((box-type (read-n-bytes stream 4))
            (box-data (read-n-bytes stream (- box-size 8))))
        (list box-size box-type box-data)))))

(defun read-iso-media-box-info (stream)
  (let* ((box-size (read-32-bit-int stream))
         (box-type (read-n-bytes stream 4)))
    (print (list box-size box-type))
    (list box-size box-type)))

(defun get-file-type (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-box stream)))

(defun iso-media-box-data (type size stream)
  (read-n-bytes stream size))

(defun do-iso-media-stream (stream fn)
  (declare (optimize (debug 3)))
  (loop for (size type) = (read-iso-media-box-info stream)
     while (and size (plusp size)) 
     collect (funcall fn type size stream)))

(defun do-iso-media-file (file fn)
  (declare (optimize (debug 3)))
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (do-iso-media-stream stream fn)))

(defun read-iso-media-stream (stream)
  (loop for box = (read-iso-media-box stream)
     while box
     collect box))

(defun read-iso-media-file (file)
  (with-open-file (stream file :element-type '(unsigned-byte 8))
    (read-iso-media-stream stream)))

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

