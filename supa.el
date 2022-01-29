(defimage tiles
  ((:type png :file "~/Downloads/GAMES/supaplex/tiles.png")))

(defconst levels-buf (get-buffer "LEVELS.DAT"))

(defconst zero-height-newline (propertize "\n" 'face '(:height 0)))

(defun list-levels ()
  (remove-overlays)
  (widen)
  (with-silent-modifications
    (set-text-properties 1 (point-max) nil)
    (dotimes (lvl 111)
      (let* ((s (1+ (* 1536 lvl)))
             (e (+ s 1536))
             (name (buffer-substring (+ s 1446) (+ s 1469))))
        (put-text-property s e 'display (format "%03d: %s\n" (1+ lvl) name))))))

(defun edit-level (lvl)
  (remove-overlays)
  (widen)
  (let* ((l (1+ (* 1536 (1- lvl))))
         (meta (+ l (* 24 60)))
         (level-contents (buffer-substring l meta))
         ;(info-req (aref (buffer-substring (+ meta 30) (+ meta 30)) 0))
         (info-cnt (seq-count (lambda (x) (= x 4)) level-contents)))
    (with-silent-modifications
      (set-text-properties 1 (point-max) nil)
      (narrow-to-region l (+ l 1536))
      (dotimes (i 24)
        (let* ((line (+ l (* 60 i)))
               (lend (+ line 60))
               (o    (make-overlay lend lend)))
          (overlay-put o 'before-string zero-height-newline)
          (dotimes (j 60)
            (let* ((s (+ line j))
                   (e (1+ s))
                   (c (aref level-contents (+ (* 60 i) j))))
              (put-text-property s e 'display (list tiles (list 'slice (* 16 c) 0 16 16)))))))
      (set-text-properties meta        (+ meta 96) nil)
      (put-text-property   meta        (+ meta  7) 'display (format "%03d " lvl))
      (put-text-property   (+ meta 29) (+ meta 30) 'display (format " %03d / %03d " 0 info-cnt))
      (put-text-property   (+ meta 30) (+ meta 31) 'display (list tiles '(slice 64 0 16 16)))
      (put-text-property   (+ meta 31) (+ meta 96) 'invisible t))
    ))

(define-derived-mode supa-mode
  fundamental-mode "Supa"
  "Major mode for Supaplex LEVELS.DAT file."
  (list-levels))

(define-minor-mode supa-level-mode
  "Minor mode for Supaplex level within the LEVELS.DAT file.")

(with-current-buffer levels-buf
  (list-levels))

(with-current-buffer levels-buf
  (edit-level 5))

(with-current-buffer (make-indirect-buffer levels-buf "LEVELS.DAT@5" t)
  (edit-level 5))

(with-current-buffer levels-buf
  (put-text-property 1 (point-max) 'invisible t))

;; fill line with an object
(with-current-buffer levels-buf
  (let ((n 58))
    (delete-char n)
    (dotimes (i n)
      (insert-char 24))))

;; make blank level
(with-current-buffer levels-buf
  (delete-region (+ 1536 61) (+ 1537 (* 23 60)))
  (dotimes (i 22)
    (insert-char 6)
    (dotimes (j 58) (insert-char 2))
    (insert-char 6))
  ;
  )

;; put all available objects in one line
(with-current-buffer levels-buf
  (delete-char 40)
  (dotimes (i 40) (insert-char i)))

(defconst over1
  (with-current-buffer levels-buf
    (make-overlay 61 61)))

(overlay-put over1 'before-string "\n")
(delete-overlay over1)
