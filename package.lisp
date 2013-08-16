
(cl:defpackage #:transcode
  (:use #:cl #:iso-media)
  (:export #:run-audio-decoder
           #:run-audio-encoder
           
           #:convert-audio-directory

           #:is-file-p
           #:recursively-list-files
           #:apple-cruft-file-p
           #:remove-apple-cruft-files))

