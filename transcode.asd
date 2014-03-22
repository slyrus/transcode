
(asdf:defsystem #:transcode
  :name "transcode"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.0.1"
  :licence "Private"
  :depends-on (iso-media cl-fad mixalot-flac)
  :serial t
  :components
  ((:cl-source-file "package")
   (:cl-source-file "transcode-config")
   (:cl-source-file "transcode")))

