

(pushnew *default-pathname-defaults* asdf:*central-registry* :test 'equalp)
(asdf:load-system 'transcode)

(cl:in-package #:transcode)

(defun listify (l)
  (if (listp l)
      l
      (list l)))

(defun convert-artist (l)
  (loop for artist in (listify l) do
       (loop for dir in
            (directory (merge-pathnames artist *audio-source-root-directory*))
          do (print dir)
            (convert-audio-directory dir))))

(convert-artist "Michael_Jackson")
(convert-artist "Maurice_Tani")
(convert-artist "Lou Reed")
(convert-artist "Tedeschi_Trucks_Band")
(convert-artist "Jon_Carroll_&_Love_Returns")
(convert-artist "Jimmy_Webb")
(convert-artist "American_Nomad")

(convert-artist "The Meters")
(convert-artist "Sex Pistols")
(convert-artist "Aerosmith")
(convert-artist "Van Halen")
(convert-artist "Ozzy Osbourne")

(mapcar #'convert-artist
        '("Elvis_Costello"
          "Bruce Springsteen"
          "Son_House"
          "Clarence_Gatemouth_Brown"
          "Blind_Lemon_Jefferson"))

(mapcar #'convert-artist
        '("Elmore_James"
          "Lightnin_Hopkins"
          "ZZ_Top"
          "Cream"
          "Ike_&_Tina_Turner"
          "Roy_Buchanan"))

(mapcar #'convert-artist
        '("Sonny_Boy_Williamson"
          "Pretenders"))

;; "Luther_Allison"
(mapcar #'convert-artist
        '("Magic_Sam_Blues_Band"))


(defparameter *test-file*
  #P"/mnt/music-archive/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a")
)

(let ((file *test-file*))
  (read-iso-media-file file))

;;;

(remove-duplicates
 (mapcar #'(lambda (x) (list (pathname-name x) x))
         (recursively-list-files *audio-source-root-directory*
                                 :test (lambda (x) (not (apple-cruft-file-p x)))))
 :test 'equal)

(remove-if-not
 (lambda (x) (equal (pathname-type x) "mov"))
 (recursively-list-files *audio-source-root-directory*
                         :test (lambda (x) (not (apple-cruft-file-p x)))))

(remove-if-not
 (lambda (f) (equal (pathname-type f) NIL))
 (recursively-list-files *audio-source-root-directory*
                         :test (lambda (x) (not (apple-cruft-file-p x)))))

(let ((files)
      (test (lambda (x) (not (apple-cruft-file-p x)))))
  (apply #'fad:walk-directory *audio-source-root-directory*
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




(let ((src "/tmp/foo/mp4a-file.m4a")
      (dest "/tmp/foo/lame.m4a"))
  (let* ((input-type (audio-sample-type (read-iso-media-file src)))
         (decoder (sb-ext:run-program "/usr/bin/faad"
                                      (list "-q" "-w" (sb-ext:native-namestring src))
                                      :output :stream
                                      :wait nil))
         (encoder (sb-ext:run-program "/usr/bin/faac"
                                      `("-o"
                                        ,(sb-ext:native-namestring dest)
                                        "-")
                                      :input (sb-ext:process-output decoder))))
    (describe decoder)
    (describe encoder)))


(let ((src "/tmp/foo/alac-file.m4a")
      (dest "/tmp/foo/alac-converted.m4a"))
  (let* ((input-type (audio-sample-type (read-iso-media-file src)))
         (decoder (sb-ext:run-program "/usr/bin/alac-decoder"
                           (list (sb-ext:native-namestring src))
                           :output :stream
                           :wait nil))
         (encoder (sb-ext:run-program "/usr/bin/faac"
                                      `("-o"
                                        ,(sb-ext:native-namestring dest)
                                        "-")
                                      :input (sb-ext:process-output decoder))))
    (describe decoder)
    (describe encoder)))


(defun copy-tags ()
  (let ((tagged-file #P"/mnt/stor1/music/Archive/Albert King/Born Under A Bad Sign/01 Born Under A Bad Sign.m4a")
        (untagged-file #P"/mnt/stor1/music/Main/Albert King/Born Under A Bad Sign/01 Born Under A Bad Sign.m4a"))
    (let ((tagged (read-iso-media-file tagged-file))
          (untagged (read-iso-media-file untagged-file)))
      (let ((src-tags (reduce #'(lambda (x y) (when x (find-child x y)))
                              (list tagged "moov" "udta" "meta" "ilst")))
            (dest-container (reduce #'(lambda (x y) (when x (find-child x y)))
                                    (list untagged "moov" "udta" "meta"))))
        (let ((pos (position "ilst" (children dest-container) :key #'box-type :test 'equal)))
          (when pos
            (setf (elt (children dest-container) pos)
                  src-tags)
            (write-iso-media-file untagged-file untagged)))))))

(defparameter *foo* (read-iso-media-file #P"/tmp/foo.mp4"))

(reduce #'(lambda (x y) (when x (find-child x y)))
        (list (read-iso-media-file #P"/tmp/foo.mp4") "moov" "udta" "meta"))

(defparameter *untagged* (read-iso-media-file #P"/mnt/iTunes_music/Active/The Microscopic Septet/Friday The Thirteenth_ The Micros Play Monk/01 Brilliant Corners.m4a"))

(let ((dir #p"/mnt/music/The Band/Music From Big Pink \\[Disc 1]/"))
  (optimize-files-in-directory dir))

(let ((dir #p"/mnt/music/"))
  (optimize-files-in-directory dir))

;; #P"/mnt/music/Robert Cray/1992, June 27, Paul Masson Winery, Saratoga, CA/Robert Cray 19920627 t16.m4a"

(let ((dir #p"/mnt/iTunes_music/Archive/David Berkeley/"))
  (convert-audio-directory dir))

;;; Everything
(let ((dir #p"/mnt/stor1/music/Archive/"))
  (convert-audio-directory dir))


(mapcar (lambda (dir)
          (convert-audio-directory dir)
          (copy-iso-tags dir))
        (list #p"/mnt/stor1/music/Archive/Jimmy Smith/"
              #p"/mnt/stor1/music/Archive/Django Reinhardt/"))


(let ((dir #p"/mnt/stor1/music/Archive/Bill Laswell/2005, June 18, Haus der Kulturen der Welt, In Transit Festival, Berlin Germany/"))
  (convert-audio-directory dir))

(let ((dir #p"/mnt/stor1/music/Archive/Audioslave/2000, July 5, Montreux Jazz Festival, Miles Davis Hall, Montreux Switzerland/"))
  (convert-audio-directory dir))

(defparameter *foo* 
  (loop for a in *dest-files*
     for relative-path = (enough-namestring (first *foo*)
                                           *audio-destination-root-directory*)
     nconc (when (not (gethash a *archive-file-hash*))
             (list a))))

(length *foo*)
(first *archive-files*)

(hash-table-count *archive-file-hash*)
(hash-table-count *dest-file-hash*)

(let* ((dir #p"/mnt/stor1/music/Archive/John Coltrane/1961, November 18, Theatre de Olympia, Paris France")
       (files (recursively-list-files dir
                                      :test (lambda (x) (not (apple-cruft-file-p x))))))
  (mapcar (lambda (x)
            (let ((iso (read-iso-media-file x)))
              (iso-media:album-artist iso)
              (unless (null (iso-media:album-artist iso))
                (setf (iso-media:album-artist iso) "This is bogus!")
                (write-iso-media-file "/tmp/foo.m4a" iso))))
          files))

(defparameter *foo* (read-iso-media-file "/tmp/foo.m4a"))

(album-artist *foo*)

;;
;; use these next two forms to read the most-recently modified
;; directories in *audio-source-root-directory* and to convert some
;; number of those directories.

(defparameter *directories-and-times*
  (sort
   (let ((dirs (remove-apple-cruft-files
                (cl-fad:list-directory *audio-source-root-directory*))))
     (mapcar (lambda (x) (cons x (file-write-date x)))
             dirs))
   #'> :key #'cdr))

(loop for dir in (subseq *directories-and-times* 0 1)
   do (convert-audio-directory (car dir)))

(mapcar (lambda (x) (multiple-value-list (decode-universal-time (cdr x))))
        (subseq *directories-and-times* 0 20))

(mapcar (lambda (x) (- (get-universal-time) (cdr x)))
        (subseq *directories-and-times* 0 20))

(loop for dir in (remove-if-not (lambda (x) (<  (- (get-universal-time) (cdr x)) 100000))
                                *directories-and-times*)
   do (print (car dir)))

(loop for dir in (remove-if-not (lambda (x) (<  (- (get-universal-time) (cdr x)) 100000))
                                *directories-and-times*)
   do (convert-audio-directory (car dir)))

(loop for dir in (directory (merge-pathnames "John Prine/L*" *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))


(defparameter *directories-and-times-2*
  (sort
   (let ((dirs (remove-apple-cruft-files
                (directory (merge-pathnames "*/*" *audio-source-root-directory*)))))
     (mapcar (lambda (x) (cons x (file-write-date x)))
             dirs))
   #'> :key #'cdr))


(mapcar (lambda (x) (- (get-universal-time) (cdr x)))
        (subseq *directories-and-times* 0 20))

(mapcar #'print
        (subseq *directories-and-times-2* 0 20))

(loop for dir in (subseq *directories-and-times-2* 0 1)
   do (convert-audio-directory (car dir)))

(loop for dir in (directory (merge-pathnames "Guns N' Roses/*" *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))

(loop for dir in (directory (merge-pathnames "Camper*/*" *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))

(loop for dir in (directory (merge-pathnames "Jason Collett/Here's to Being Here"
                                             *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))

(loop for dir in (directory (merge-pathnames "Travis"
                                             *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))


(loop for dir in (directory (merge-pathnames "Troy_Nelson"
                                             *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))


(defparameter *guitar-source-root-directory* #P"/tank/music/Guitar Instruction Archive/")
(defparameter *guitar-destination-root-directory* #P"/tank/music/Guitar Instruction/")

(loop for dir in (directory (merge-pathnames "Signature*"
                                             *guitar-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir
                              :source-root-directory *guitar-source-root-directory*
                              :destination-root-directory *guitar-destination-root-directory*))


(loop for dir in (directory (merge-pathnames "Freddie_King"
                                             *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))

(loop for dir in (directory (merge-pathnames "Travis"
                                             *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))


(loop for artist in
     '("Sarah Vaughan"
       "Pete Townshend"
       "Van Halen"
       "Toots & the Maytals"
       "The Who"
       "Townes Van Zandt"
       "Rod Stewart"
       "Freddie_King"
       "Bobby_Blue_Bland"
       "Albert_King"
       "Jim_Florentine"
       "Timbuk_3"
       "Ike_&_Tina_Turner"
       "Trip_Shakespeare"
       "Caetano_Veloso_e_Gilberto_Gil")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("B.B._King" "Albert_Collins" "Paul_Simon" "The_Yardbirds")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))


(loop for artist in
     '("Miles Davis"
       "Miles Davis and Bill Laswell"
       "Miles Davis and Gil Evans"
       "Miles Davis and John Coltrane"
       "Miles Davis and Prince"
       "Miles Davis and Quincy Jones"
       "Miles Davis and Thelonious Monk"
       "Miles Davis Quartet"
       "Miles Davis Quintet")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("John Coltrane"
       "Tom Waits"
       "Troy Nelson"
       "Ray Charles"
       "Herbie Hancock"
       "Albert King"
       "The Streets"
       "David Gray"
       "The Clash"
       "The Village Green"
       "The Kooks"
       "Pink Floyd"
       "Roxy Music"
       "Peter Gabriel"
       "Robert Johnson"
       "Paul Simon"
       "The Rowan Brothers"
       "Big Star"
       "The Kinks"
       "David Bowie"
       "Bob Dylan"
       "Led Zeppelin"
       "The Shins"
       "The Band"
       "Natalie Merchant"
       "Johnny Cash"
       "Willie Nelson")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("Ali Farka Touré & Toumani Diabaté" "Alpha Blondy" "Arcade Fire" "Arctic Monkeys"
       "Band Of Horses" "Barnes and Barnes"
       "Beastie Boys"
       "Benny Bell"
       "Betty Davis"
       "Bill_Monroe_&_Doc_Watson")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("Black Keys")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("Blind Willie McTell"
       "Circle Jerks"
       "Cody Simpson"
       "Cyrus Chestnut"
       "Delaney & Bonnie"
       "De La Soul"
       "Dead Kennedys"
       "Derek & The Dominos"
       "Ratdog"
       "Wolfgang Amadeus Mozart")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))


(loop for artist in
     '("The Black Eyed Peas"
       "Fugees"
       "Lake Street Dive"
       "The National"
       "Leon Russell"
       "The Dead Weather")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("Squeeze"
       "The Only Ones"
       "The Meters"
       "Sonny Clark"
       "Waylon Jennings")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("Tift Merritt"
       "The_Paul_Butterfield_Blues_Band"
       "The_Wrights"
       "The Mountain Goats"
       "The_Brothers_Johnson"
       "Tenacious_D"
       "Stanley_Clarke")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("T-Bone Walker"
       "Seu George"
       "The Raconteurs"
       "of Montreal"
       "Nine Inch Nails"
       "Murder By Death"
       "Mimi & Richard Fariña"
       "Rodrigo Y Gabriela & C.U.B.A_")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("Kevin_Drew"
       "Kimya_Dawson"
       "Love Canon"
       "Les McCann"
       "Le Tigre"
       "Malecon Social Club"
       "Mark Olson"
       "Michael_Bloomfield,_Al_Kooper,_Steve_Stills"
       "Kaki King"
       "Jimmy_Cliff"
       "Junior Murvin"
       "John_McCutcheon"
       "Grace Potter & The Nocturnals"
       "Great Northern"
       "The_Uncluded")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("Seu Jorge"
       "")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("Barrett_Tagliarino")
   do
     (loop for dir in (directory (merge-pathnames artist *audio-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir)))

(loop for artist in
     '("William_Leavitt")
   do
     (loop for dir in (directory (merge-pathnames artist *guitar-source-root-directory*))
        do (print dir)
          (convert-audio-directory dir
                                   :source-root-directory *guitar-source-root-directory*
                                   :destination-root-directory *guitar-destination-root-directory*)))

(loop for dir in (directory (merge-pathnames "Eric_Clapton/Me_And_Mr._Johnson" *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))

(loop for dir in (directory (merge-pathnames "Van_Morrison/Its_Too_Late_To_Stop_Now*/" *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))

(loop for dir in (directory (merge-pathnames "Van_Morrison/Saint_Dominics_Preview/" *audio-source-root-directory*))
   do (print dir)
     (convert-audio-directory dir))
