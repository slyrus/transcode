
(in-package #:transcode)

(let ((file #P"/mnt/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a"))
  (map 'list
       (lambda (x)
         (list (map 'string #'code-char (second x))
               (length (third x))))
       (read-iso-media-file file)))

(defun iso-box-info (iso-boxes)
  (map 'list
       (lambda (x)
         (list (map 'string #'code-char (second x))
               (length (third x))))
       iso-boxes))

(let ((file #P"/mnt/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a"))
  (iso-box-info (read-iso-media-file file)))

(let ((file #P"/mnt/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a"))
  (remove-if-not (lambda (box)
                   (equalp (second box)
                           (map 'vector #'char-code "moov")))
                 (read-iso-media-file file)))

(let ((file #P"/mnt/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a"))
  (flex:with-input-from-sequence
      (stream 
       (third
        (car (remove-if-not (lambda (box)
                              (equalp (second box)
                                      (map 'vector #'char-code "moov")))
                            (read-iso-media-file file))))) 
    (iso-box-info (read-iso-media-stream stream))))

(let ((file #P"/mnt/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a"))
  (flex:with-input-from-sequence
      (stream 
       (third
        (car (remove-if-not (lambda (box)
                              (equalp (second box)
                                      (map 'vector #'char-code "moov")))
                            (read-iso-media-file file))))) 
    (do-iso-media-stream stream #'iso-media-box-data)))


(let ((file #P"/mnt/iTunes_Music/ALAC/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a"))
  (do-iso-media-file file (lambda (type size stream)
                            (print (list type size))
                            (iso-media-box-data type (- size 8) stream)
                            (list (map 'string #'code-char type) size))))