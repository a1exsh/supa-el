;; -*- lexical-binding: t -*-
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
(defconst supa-level-size-in-bytes 1536) ;; 3 disk sectors apparently

(defun supa-level-number-at-point ()
  (1+ (/ (1- (point))
         supa-level-size-in-bytes)))

(defvar supa-tiles-image nil)
(defvar supa-tile-size nil)

(defun supa-set-tiles-scale (n)
  (setq supa-tile-size (* 16 n))
  (setq supa-tiles-image
        (find-image (list (list :type 'png :file (format "~/src/supa-el/tiles_x%d.png" n))))))

(defun supa-list-levels ()
  (remove-overlays)
  (widen)
  (with-silent-modifications
    (set-text-properties 1 (point-max) nil)
    (dotimes (lvl 111)
      (let* ((s (1+ (* supa-level-size-in-bytes lvl)))
             (e (+ s supa-level-size-in-bytes))
             (name (buffer-substring (+ s 1446) (+ s 1469))))
        (put-text-property s e 'display (format "%03d: %s\n" (1+ lvl) name))))))

(defun supa-put-text-prop-tile (pos tile-n)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-tiles-image
                           (list 'slice
                                 (* supa-tile-size tile-n)
                                 0
                                 supa-tile-size
                                 supa-tile-size))))

(defun supa-count-level-infotrons (level-bytes)
  (seq-count (lambda (x) (= x 4)) level-bytes))

(defun supa-edit-level (lvl)
  (remove-overlays)
  (widen)
  (let* ((l (1+ (* supa-level-size-in-bytes (1- lvl))))
         (meta (+ l (* 24 60)))
         (level-contents (buffer-substring l meta))
         (info-req (aref (buffer-substring (+ meta 30) (+ meta 31)) 0))
         (info-cnt (supa-count-level-infotrons level-contents))
         (zero-height-newline (propertize "\n" 'face '(:height 0))))
    (with-silent-modifications
      (set-text-properties 1 (point-max) nil)
      (narrow-to-region l (+ l supa-level-size-in-bytes))
      (dotimes (i 24)
        (let* ((line (+ l (* 60 i)))
               (lend (+ line 60))
               (o    (make-overlay lend lend)))
          (overlay-put o 'before-string zero-height-newline)
          (dotimes (j 60)
            (supa-put-text-prop-tile (+ line j)
                                     (aref level-contents (+ (* 60 i) j))))))
      (set-text-properties meta        (+ meta 96) nil)
      (put-text-property   meta        (+ meta  7) 'display (format "%03d " lvl))
      (put-text-property   (+ meta 29) (+ meta 30) 'display (format " %03d / %03d " info-cnt info-req))
      (supa-put-text-prop-tile (+ meta 30) 4)
      (put-text-property   (+ meta 31) (+ meta 96) 'invisible t))))

(defun supa-edit-level-at-point ()
  (supa-edit-level (supa-level-number-at-point)))

(defun supa-level-start-pos (&optional pos)
  (let ((p (1- (or pos (point)))))
    (1+ (- p (% p supa-level-size-in-bytes)))))

(defun supa-update-info-count ()
  (let* ((start-pos    (supa-level-start-pos))
         (meta-pos     (+ start-pos (* 24 60)))
         (meta-end-pos (+ meta-pos 96))
         (level-bytes  (buffer-substring start-pos meta-pos))
         (meta-bytes   (buffer-substring meta-pos meta-end-pos))
         (info-cnt     (supa-count-level-infotrons level-bytes))
         (info-req     (aref meta-bytes 30)))
    (put-text-property (+ meta-pos 29) (+ meta-pos 30)
                       'display
                       (format " %03d / %03d " info-cnt info-req))))

(defun supa-is-editable-tile (&optional pos)
  (let* ((p (% (1- (or pos (point)))
               supa-level-size-in-bytes))
         (y (/ p 60))
         (x (% p 60)))
    (and (< 0 y 23)
         (< 0 x 59))))

(defun supa-tile-at-point ()
  (char-after))

(defun supa-set-tile-at-point (tile-n)
  (when (supa-is-editable-tile)
    (let ((inhibit-read-only 't))
      (delete-char 1)
      (insert tile-n)
      (backward-char)
      (supa-put-text-prop-tile (point) tile-n)
      (with-silent-modifications
        (supa-update-info-count)))))

(defun supa-refresh-text-prop-tile-at-point ()
  (supa-put-text-prop-tile (point) (supa-tile-at-point)))

(defun supa-undo (&optional arg)
  (interactive)
  (let ((inhibit-read-only 't))
    (undo arg)
    (with-silent-modifications
      ;; undo also restores the original tile size, so enforce the current one:
      (supa-refresh-text-prop-tile-at-point)
      (supa-update-info-count))))

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

(defconst supa-kbd-tile-alist
  '(("SPC" . ( 0 space))
    ("z"   . ( 1 zonk))
    ("b"   . ( 2 base))
    ("M"   . ( 3 murphy))
    ("i"   . ( 4 infotron))
    ("c"   . ( 5 chip))
    ("h"   . ( 6 hardware))
    ("E"   . ( 7 exit))
    ("d"   . ( 8 disk))
    (">"   . ( 9 port))
    ("V"   . (10 port))
    ("<"   . (11 port))
    ("^"   . (12 port))
    ;; we skip the 4 special ports, as there is a separate binding to toggle them
    ("s"   . (17 snick-snack))
    ("y"   . (18 yellow-disk))
    ("T"   . (19 terminal))
    ("r"   . (20 red-disk))
    ("|"   . (21 port))
    ("-"   . (22 port))
    ("+"   . (23 port))
    ("e"   . (24 electron))
    ("B"   . (25 bug))
    ("["   . (26 chip-left))
    ("]"   . (27 chip-right))
    ("0"   . (28 hw0))
    ("1"   . (29 hw1))
    ("2"   . (30 hw2))
    ("3"   . (31 hw3))
    ("4"   . (32 hw4))
    ("5"   . (33 hw5))
    ("6"   . (34 hw6))
    ("7"   . (35 hw7))
    ("8"   . (36 hw8))
    ("9"   . (37 hw9))
    ("{"   . (38 chip-up))
    ("}"   . (39 chip-down))))

(defconst supa-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'supa-level-mode)
    (define-key map (kbd "?")   'supa-show-help)
    map))

(defconst supa-help-buffer-name "*Supa Help*")

(defun supa-show-help ()
  (interactive)
  (with-help-window supa-help-buffer-name
    (with-current-buffer supa-help-buffer-name
      (princ "?\tShow this help window\n")
      (princ "\n")
      (princ "RET\tEnter a level for editing / go back to the levels list\n")
      (princ "u, U\tUndo\n")
      (princ "G\tToggle port gravity flag\n")
      (princ "\n")
      ;; key, tile, sym
      (seq-doseq (kts supa-kbd-tile-alist)
        (princ (car kts))
        (princ "\t.")
        (supa-put-text-prop-tile (length (buffer-string)) (cadr kts))
        (princ " ")
        (princ (caddr kts))
        (princ "\n")))))

(defun supa-text-scale-adjust-hook ()
  (supa-set-tiles-scale (1+ (max 0 (min text-scale-mode-amount 3))))
  (when supa-level-mode
    (supa-edit-level-at-point)))

(define-derived-mode supa-mode
  special-mode "Supa"
  "Major mode for Supaplex LEVELS.DAT file."
  (setq-local truncate-lines 't)
  (supa-list-levels)
  (derived-mode-set-keymap 'supa-mode)
  (supa-set-tiles-scale 1)
  (add-hook 'text-scale-mode-hook 'supa-text-scale-adjust-hook)
  (message "? - Help"))

(defconst supa-level-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "G") 'supa-toggle-port-gravity-at-point)

    (define-key map (kbd "u") 'supa-undo)
    (define-key map (kbd "U") 'supa-undo)
    (define-key map [remap undo] 'supa-undo)

    ;; key, tile, sym
    (seq-doseq (kts supa-kbd-tile-alist)
      (let ((key    (kbd (car kts)))
            (tile-n (cadr kts)))
        (define-key map key
          (lambda ()
            (interactive)
            (supa-set-tile-at-point tile-n)))))

    map))

(define-minor-mode supa-level-mode
  "Minor mode for Supaplex level within the LEVELS.DAT file."
  nil
  "Level"
  'supa-level-mode-map
  :after-hook (if supa-level-mode
                  (supa-edit-level-at-point)
                  (supa-list-levels)))

;; (defun supa-clear-level ()
;;   (interactive)
;;   (delete-region 1 (1+ (* 24 60)))
;;   (dotimes (x 60)
;;     (insert-char 6))
;;   (dotimes (y 22)
;;     (insert-char 6)
;;     (dotimes (x 58)
;;       (insert-char 2))
;;     (insert-char 6))
;;   (dotimes (x 60)
;;     (insert-char 6)))
