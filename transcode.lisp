;;;
;;; TODO: get mp3, mp4 or AAC stream as appropriate!
;;;

(cl:in-package #:transcode)

(defun run-audio-decoder (src &key (input-type "alac"))
  (case (intern input-type)
    (#.(intern "alac")
       (sb-ext:run-program "/usr/bin/alac-decoder"
                           (list (sb-ext:native-namestring src))
                           :output :stream
                           :error :stream
                           :wait nil))
    (#.(intern "mp4a")
       (sb-ext:run-program "/usr/bin/faad"
                           (list "-q" "-w" (sb-ext:native-namestring src))
                           :output :stream
                           :error :stream
                           :wait nil))
    (#.(intern "flac")
       (sb-ext:run-program "/usr/bin/flac"
                           (list "-c" "-d" (sb-ext:native-namestring src))
                           :output :stream
                           :error :stream
                           :if-error-exists :supersede
                           :wait nil))
    (t (format *error-output* "~&No decoder for ~s" src))))

(defun run-audio-encoder (input dest &key (output-type "mp4a"))
  (case (intern output-type)
    (#.(intern "mp4a")
       (sb-ext:run-program "/usr/bin/faac"
                           `("-o"
                             ,(sb-ext:native-namestring dest)
                             "-")
                           :error nil
                           :input input
                           :wait t))))

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
                (when (equalp src dest)
                  (error "same src ~S and destination ~S" src dest))
                (ensure-directories-exist dest)
                (cond
                  ((find (string-downcase (pathname-type src)) '("mp4" "m4a") :test 'equal)
                   (let ((input-type (or input-type (audio-sample-type (read-iso-media-file src)))))
                     (cond
                       ((equal input-type "mp4a")
                        (fad:copy-file src dest :overwrite t)
                        dest)
                       ((equal input-type "alac")
                        (print (list src dest))
                        (let* ((decoder (apply #'run-audio-decoder
                                               src
                                               (when input-type (list :input-type input-type))))
                               (encoder (apply #'run-audio-encoder 
                                               (sb-ext:process-output decoder)
                                               dest
                                               (when output-type (list :output-type output-type)))))
                          (cond ((and (sb-ext:process-exit-code decoder)
                                      (not (zerop (sb-ext:process-exit-code decoder))))
                                 (with-output-to-string (str)
                                   (fad:copy-stream (sb-ext:process-error decoder) str)
                                   str)) 
                                ((and (sb-ext:process-exit-code encoder)
                                      (not (zerop (sb-ext:process-exit-code encoder))))
                                 (with-output-to-string (str)
                                   (fad:copy-stream (sb-ext:process-error encoder) str)
                                   str))
                                (t 
                                 (copy-iso-tags src dest)
                                 dest))
                          (sb-ext:process-close encoder)
                          (sb-ext:process-close decoder))))))
                  ((find (string-downcase (pathname-type src)) '("flac") :test 'equal)
                   (print (list src dest))
                   (let* ((decoder (run-audio-decoder src :input-type "flac"))
                          (encoder (apply #'run-audio-encoder 
                                          (sb-ext:process-output decoder)
                                          (merge-pathnames (make-pathname :type "m4a") dest)
                                          (when output-type (list :output-type output-type)))))
                     (cond ((and (sb-ext:process-exit-code decoder)
                                 (not (zerop (sb-ext:process-exit-code decoder))))
                            (with-output-to-string (str)
                              (fad:copy-stream (sb-ext:process-error decoder) str)
                              str)) 
                           ((and (sb-ext:process-exit-code encoder)
                                 (not (zerop (sb-ext:process-exit-code encoder))))
                            (with-output-to-string (str)
                              (fad:copy-stream (sb-ext:process-error encoder) str)
                              str))
                           (t 
                            (copy-iso-tags src dest)
                            dest))
                     (sb-ext:process-close encoder)
                     (sb-ext:process-close decoder)))
                  ((find (string-downcase (pathname-type src)) '("mp3") :test 'equal)
                   (fad:copy-file src dest :overwrite t))
                  (t
                   (format *error-output* "Ignoring: ~s" src)))))
            files)))

#+nil
(defun convert-audio-tags (srcdir &key input-type output-type)
  (let ((files
         (recursively-list-files srcdir :test (lambda (x) (not (apple-cruft-file-p x))))))
    (mapcar (lambda (src)
              (let ((dest (merge-pathnames (enough-namestring src *audio-source-root-directory*)
                                           *audio-destination-root-directory*)))
                (when (equalp src dest)
                  (error "same src ~S and destination ~S" src dest))
                (ensure-directories-exist dest)
                (cond
                  ((find (string-downcase (pathname-type src)) '("mp4" "m4a") :test 'equal)
                   (let ((input-type (or input-type (audio-sample-type (read-iso-media-file src)))) )
                     (cond
                       ((equal input-type "mp4a")
                        t)
                       ((equal input-type "alac")
                        (print (list src dest))
                        ;;; use ffmpeg to extract tags from src and apply to src
                        
                        (sb-ext:run-program "/usr/bin/ffmpeg -map_meta_data "
                                            (list (format nil "~A:~A"
                                                          (sb-ext:native-namestring dest)
                                                          (sb-ext:native-namestring src)))
                                            
                                            
                                            ;; broken
                                            )))))
                  ((find (string-downcase (pathname-type src)) '("mp3") :test 'equal)
                   (fad:copy-file src dest :overwrite t))
                  (t
                   (format *error-output* "Ignoring: ~s" src)))))
            files)))

(defun copy-iso-tags (src dest)
  (when (equalp src dest)
    (error "same src ~S and destination ~S" src dest))
  (ensure-directories-exist dest)
  (cond
    ((find (string-downcase (pathname-type src)) '("mp4" "m4a") :test 'equal)
     (let* ((src-container (read-iso-media-file src))
            (input-type (audio-sample-type src-container)))
       (cond
         ((equal input-type "alac")
          ;; here we should copy the ISO tags from src to dest!!!!
          (let ((dest-container (read-iso-media-file dest)))
            (print (list src dest))
            (let ((src-ilst-box
                   (reduce #'find-child '("moov" "udta" "meta" "ilst")
                           :initial-value src-container))
                  (dest-ilst-box
                   (reduce #'find-child '("moov" "udta" "meta" "ilst")
                           :initial-value dest-container)))
              (let ((dest-box-children-types
                     (map 'list #'box-type (children dest-ilst-box))))
                (loop for box in (children src-ilst-box)
                   do
                   (unless (member (box-type box) dest-box-children-types
                                   :test 'equal)
                     (pushnew box (children dest-ilst-box)
                              :key #'box-type :test 'equal)))
                (setf (children dest-ilst-box)
                      (nreverse (children dest-ilst-box))))
              (iso-media::update-size dest-ilst-box))                          
            (iso-media::update-stco-box dest-container)
            (write-iso-media-file dest dest-container)
            dest)))))))

(defun copy-iso-tags-for-files-in-directory (srcdir)
  (let ((files
         (recursively-list-files srcdir :test (lambda (x) (not (apple-cruft-file-p x))))))
    (mapcar (lambda (src)
              (let ((dest (merge-pathnames (enough-namestring src *audio-source-root-directory*)
                                           *audio-destination-root-directory*)))
                (copy-iso-tags src dest)))
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

(defun update-music-db (&optional (dir #P"/mnt/iTunes_music/Archive/"))
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


;;; "optimizing files"

(defun optimize-files-in-directory (srcdir)
  (let ((files
         (recursively-list-files srcdir :test (lambda (x) (not (apple-cruft-file-p x))))))
    (mapcar (lambda (src)
              (cond
                ((find (string-downcase (pathname-type src)) '("mp4" "m4a") :test 'equal)
                 (restart-case
                     (let ((iso-container (iso-media:read-iso-media-file src)))
                       (unless (iso-media:iso-container-optimized-p iso-container)
                         (format *standard-output* "~&Optimizing: ~s~%" src)
                         (iso-media:write-iso-media-file src iso-container :optimize t)))
                   (skip-file () nil)))
                (t
                 (format *error-output* "~&Ignoring: ~s~%" src))))
            files)))
