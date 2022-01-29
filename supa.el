;;
;; Supaplex level editor for GNU Emacs.
;; Copyright (C) 2020-2022  Alex Shulgin
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
(defimage supa-tiles-image
  ((:type png :file "~/src/supa-el/tiles.png")))
;; (setq supa-tiles-image (find-image '((:type png :file "~/src/supa-el/tiles.png"))))

(defconst supa-zero-height-newline (propertize "\n" 'face '(:height 0)))

(defun supa-list-levels ()
  (interactive)
  (remove-overlays)
  (widen)
  (with-silent-modifications
    (set-text-properties 1 (point-max) nil)
    (dotimes (lvl 111)
      (let* ((s (1+ (* 1536 lvl)))
             (e (+ s 1536))
             (name (buffer-substring (+ s 1446) (+ s 1469))))
        (put-text-property s e 'display (format "%03d: %s\n" (1+ lvl) name))))))

(defun supa-edit-level-at-point ()
  (interactive)
  (supa-edit-level (1+ (/ (1- (point))
                          1536))))

(defun supa-put-text-prop-tile (pos tile-n)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-tiles-image
                           (list 'slice (* 16 tile-n) 0 16 16))))

(defun supa-edit-level (lvl)
  (interactive "nLevel: ")
  (remove-overlays)
  (widen)
  (let* ((l (1+ (* 1536 (1- lvl))))
         (meta (+ l (* 24 60)))
         (level-contents (buffer-substring l meta))
         ;; (info-req (aref (buffer-substring (+ meta 30) (+ meta 30)) 0))
         (info-cnt (seq-count (lambda (x) (= x 4)) level-contents)))
    (with-silent-modifications
      (set-text-properties 1 (point-max) nil)
      (narrow-to-region l (+ l 1536))
      (dotimes (i 24)
        (let* ((line (+ l (* 60 i)))
               (lend (+ line 60))
               (o    (make-overlay lend lend)))
          (overlay-put o 'before-string supa-zero-height-newline)
          (dotimes (j 60)
            (supa-put-text-prop-tile (+ line j)
                                     (aref level-contents (+ (* 60 i) j))))))
      (set-text-properties meta        (+ meta 96) nil)
      (put-text-property   meta        (+ meta  7) 'display (format "%03d " lvl))
      (put-text-property   (+ meta 29) (+ meta 30) 'display (format " %03d / %03d " 0 info-cnt))
      (put-text-property   (+ meta 30) (+ meta 31) 'display (list supa-tiles-image '(slice 64 0 16 16)))
      (put-text-property   (+ meta 31) (+ meta 96) 'invisible t))))

(defun supa-is-editable-tile (&optional pos)
  (let* ((p (% (1- (or pos (point)))
               1536))
         (y (/ p 60))
         (x (% p 60)))
    (and (< 0 y 23)
         (< 0 x 59))))

(defun supa-tile-at-point ()
  (interactive)
  (char-after))

(defun supa-set-tile-at-point (tile-n)
  (interactive)
  (when (supa-is-editable-tile)
    (let ((inhibit-read-only 't))
      (delete-char 1)
      (insert tile-n)
      (backward-char)
      (supa-put-text-prop-tile (point) tile-n))))

(defun supa-undo (&optional arg)
  (interactive)
  (let ((inhibit-read-only 't))
    (undo arg)))

(defun supa-port-tile-toggled-gravity (tile-n)
  (cond
   ((= tile-n  9) 13)
   ((= tile-n 10) 14)
   ((= tile-n 11) 15)
   ((= tile-n 12) 16)
   ((= tile-n 13)  9)
   ((= tile-n 14) 10)
   ((= tile-n 15) 11)
   ((= tile-n 16) 12)))

(defun supa-toggle-port-gravity-at-point ()
  (interactive)
  (when-let ((old-tile-n (supa-tile-at-point))
             (new-tile-n (supa-port-tile-toggled-gravity old-tile-n)))
      (supa-set-tile-at-point new-tile-n)))

(defun supa-set-tile-at-point-spc ()      (interactive) (supa-set-tile-at-point  0))
(defun supa-set-tile-at-point-zonk ()     (interactive) (supa-set-tile-at-point  1))
(defun supa-set-tile-at-point-base ()     (interactive) (supa-set-tile-at-point  2))
(defun supa-set-tile-at-point-murphy ()   (interactive) (supa-set-tile-at-point  3))
(defun supa-set-tile-at-point-info ()     (interactive) (supa-set-tile-at-point  4))
(defun supa-set-tile-at-point-chip ()     (interactive) (supa-set-tile-at-point  5))
(defun supa-set-tile-at-point-hard ()     (interactive) (supa-set-tile-at-point  6))
(defun supa-set-tile-at-point-exit ()     (interactive) (supa-set-tile-at-point  7))
(defun supa-set-tile-at-point-disk ()     (interactive) (supa-set-tile-at-point  8))
(defun supa-set-tile-at-point-port-lr ()  (interactive) (supa-set-tile-at-point  9))
(defun supa-set-tile-at-point-port-ud ()  (interactive) (supa-set-tile-at-point 10))
(defun supa-set-tile-at-point-port-rl ()  (interactive) (supa-set-tile-at-point 11))
(defun supa-set-tile-at-point-port-du ()  (interactive) (supa-set-tile-at-point 12))
(defun supa-set-tile-at-point-snik ()     (interactive) (supa-set-tile-at-point 17))
(defun supa-set-tile-at-point-y-disk ()   (interactive) (supa-set-tile-at-point 18))
(defun supa-set-tile-at-point-term ()     (interactive) (supa-set-tile-at-point 19))
(defun supa-set-tile-at-point-r-disk ()   (interactive) (supa-set-tile-at-point 20))
(defun supa-set-tile-at-point-port-v ()   (interactive) (supa-set-tile-at-point 21))
(defun supa-set-tile-at-point-port-h ()   (interactive) (supa-set-tile-at-point 22))
(defun supa-set-tile-at-point-port-p ()   (interactive) (supa-set-tile-at-point 23))
(defun supa-set-tile-at-point-electron () (interactive) (supa-set-tile-at-point 24))
(defun supa-set-tile-at-point-bug ()      (interactive) (supa-set-tile-at-point 25))
(defun supa-set-tile-at-point-chip-l ()   (interactive) (supa-set-tile-at-point 26))
(defun supa-set-tile-at-point-chip-r ()   (interactive) (supa-set-tile-at-point 27))
(defun supa-set-tile-at-point-hw0 ()      (interactive) (supa-set-tile-at-point 28))
(defun supa-set-tile-at-point-hw1 ()      (interactive) (supa-set-tile-at-point 29))
(defun supa-set-tile-at-point-hw2 ()      (interactive) (supa-set-tile-at-point 30))
(defun supa-set-tile-at-point-hw3 ()      (interactive) (supa-set-tile-at-point 31))
(defun supa-set-tile-at-point-hw4 ()      (interactive) (supa-set-tile-at-point 32))
(defun supa-set-tile-at-point-hw5 ()      (interactive) (supa-set-tile-at-point 33))
(defun supa-set-tile-at-point-hw6 ()      (interactive) (supa-set-tile-at-point 34))
(defun supa-set-tile-at-point-hw7 ()      (interactive) (supa-set-tile-at-point 35))
(defun supa-set-tile-at-point-hw8 ()      (interactive) (supa-set-tile-at-point 36))
(defun supa-set-tile-at-point-hw9 ()      (interactive) (supa-set-tile-at-point 37))
(defun supa-set-tile-at-point-chip-u ()   (interactive) (supa-set-tile-at-point 38))
(defun supa-set-tile-at-point-chip-d ()   (interactive) (supa-set-tile-at-point 39))

(defconst supa-map (make-sparse-keymap))

;; major: list levels
(define-key supa-map (kbd "RET") 'supa-edit-level-at-point)

;; minor: edit level
(define-key supa-map (kbd "q") 'supa-list-levels)

(define-key supa-map (kbd "SPC") 'supa-set-tile-at-point-spc)
(define-key supa-map (kbd "z")   'supa-set-tile-at-point-zonk)
(define-key supa-map (kbd "b")   'supa-set-tile-at-point-base)
(define-key supa-map (kbd "M")   'supa-set-tile-at-point-murphy)
(define-key supa-map (kbd "i")   'supa-set-tile-at-point-info)
(define-key supa-map (kbd "c")   'supa-set-tile-at-point-chip)
(define-key supa-map (kbd "h")   'supa-set-tile-at-point-hard)
(define-key supa-map (kbd "E")   'supa-set-tile-at-point-exit)
(define-key supa-map (kbd "d")   'supa-set-tile-at-point-disk)
(define-key supa-map (kbd ">")   'supa-set-tile-at-point-port-lr)
(define-key supa-map (kbd "V")   'supa-set-tile-at-point-port-ud)
(define-key supa-map (kbd "<")   'supa-set-tile-at-point-port-rl)
(define-key supa-map (kbd "^")   'supa-set-tile-at-point-port-du)
(define-key supa-map (kbd "s")   'supa-set-tile-at-point-snik)
(define-key supa-map (kbd "y")   'supa-set-tile-at-point-y-disk)
(define-key supa-map (kbd "T")   'supa-set-tile-at-point-term)
(define-key supa-map (kbd "r")   'supa-set-tile-at-point-r-disk)
(define-key supa-map (kbd "|")   'supa-set-tile-at-point-port-v)
(define-key supa-map (kbd "-")   'supa-set-tile-at-point-port-h)
(define-key supa-map (kbd "+")   'supa-set-tile-at-point-port-p)
(define-key supa-map (kbd "e")   'supa-set-tile-at-point-electron)
(define-key supa-map (kbd "B")   'supa-set-tile-at-point-bug)
(define-key supa-map (kbd "[")   'supa-set-tile-at-point-chip-l)
(define-key supa-map (kbd "]")   'supa-set-tile-at-point-chip-r)
(define-key supa-map (kbd "0")   'supa-set-tile-at-point-hw0)
(define-key supa-map (kbd "1")   'supa-set-tile-at-point-hw1)
(define-key supa-map (kbd "2")   'supa-set-tile-at-point-hw2)
(define-key supa-map (kbd "3")   'supa-set-tile-at-point-hw3)
(define-key supa-map (kbd "4")   'supa-set-tile-at-point-hw4)
(define-key supa-map (kbd "5")   'supa-set-tile-at-point-hw5)
(define-key supa-map (kbd "6")   'supa-set-tile-at-point-hw6)
(define-key supa-map (kbd "7")   'supa-set-tile-at-point-hw7)
(define-key supa-map (kbd "8")   'supa-set-tile-at-point-hw8)
(define-key supa-map (kbd "9")   'supa-set-tile-at-point-hw9)
(define-key supa-map (kbd "{")   'supa-set-tile-at-point-chip-u)
(define-key supa-map (kbd "}")   'supa-set-tile-at-point-chip-d)

(define-key supa-map (kbd "G")   'supa-toggle-port-gravity-at-point)
(define-key supa-map (kbd "u")   'supa-undo)
(define-key supa-map (kbd "U")   'supa-undo)

(define-key supa-map [remap undo] 'supa-undo)

(define-derived-mode supa-mode
  fundamental-mode "Supa"
  "Major mode for Supaplex LEVELS.DAT file."
  (read-only-mode)
  (supa-list-levels)
  (derived-mode-set-keymap 'supa))

(define-minor-mode supa-level-mode
  "Minor mode for Supaplex level within the LEVELS.DAT file.")

;; (with-current-buffer (make-indirect-buffer levels-buf "LEVELS.DAT@5" t)
;;   (edit-level 5))

;; (with-current-buffer levels-buf
;;   (put-text-property 1 (point-max) 'invisible t))

;; ;; fill line with an object
;; (with-current-buffer levels-buf
;;   (let ((n 58))
;;     (delete-char n)
;;     (dotimes (i n)
;;       (insert-char 24))))

;; ;; make blank level
;; (with-current-buffer levels-buf
;;   (delete-region (+ 1536 61) (+ 1537 (* 23 60)))
;;   (dotimes (i 22)
;;     (insert-char 6)
;;     (dotimes (j 58) (insert-char 2))
;;     (insert-char 6))
;;   ;
;;   )

(defun supa-clear-level ()
  (interactive)
  (delete-region 1 (1+ (* 24 60)))
  (dotimes (x 60)
    (insert-char 6))
  (dotimes (y 22)
    (insert-char 6)
    (dotimes (x 58)
      (insert-char 2))
    (insert-char 6))
  (dotimes (x 60)
    (insert-char 6)))

;; ;; put all available objects in one line
;; (with-current-buffer levels-buf
;;   (delete-char 40)
;;   (dotimes (i 40) (insert-char i)))

;; (defconst over1
;;   (with-current-buffer levels-buf
;;     (make-overlay 61 61)))

;; (overlay-put over1 'before-string "\n")
;; (delete-overlay over1)
