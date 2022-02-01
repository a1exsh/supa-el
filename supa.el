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
(defvar supa-tiles-image nil)
(defvar supa-tile-size nil)

(defun supa-set-tiles-scale (n)
  (interactive "nScale (1-4): ")
  (setq supa-tile-size (* 16 n))
  (setq supa-tiles-image
        (find-image (list (list :type 'png :file (format "~/src/supa-el/tiles_x%d.png" n))))))

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
                           (list 'slice
                                 (* supa-tile-size tile-n)
                                 0
                                 supa-tile-size
                                 supa-tile-size))))

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
      (supa-put-text-prop-tile (+ meta 30) 4)
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

(defun supa-refresh-text-prop-tile-at-point ()
  (supa-put-text-prop-tile (point) (supa-tile-at-point)))

(defun supa-undo (&optional arg)
  (interactive)
  (let ((inhibit-read-only 't))
    (undo arg)
    ;; undo also restores the original tile size, so enforce the current one:
    (with-silent-modifications
      (supa-refresh-text-prop-tile-at-point))))

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

(defconst supa-mode-map (make-sparse-keymap))

;; major: list levels
(define-key supa-mode-map (kbd "RET") 'supa-edit-level-at-point)

;; minor: edit level
(define-key supa-mode-map (kbd "?") 'supa-show-help)
(define-key supa-mode-map (kbd "q") 'supa-list-levels)
(define-key supa-mode-map (kbd "G") 'supa-toggle-port-gravity-at-point)

(define-key supa-mode-map (kbd "u") 'supa-undo)
(define-key supa-mode-map (kbd "U") 'supa-undo)
(define-key supa-mode-map [remap undo] 'supa-undo)

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

;; key, tile, sym
(seq-doseq (kts supa-kbd-tile-alist)
  (let ((tile-n (cadr kts)))
    (define-key
      supa-mode-map
      (kbd (car kts))
      (lambda ()
        (interactive)
        (supa-set-tile-at-point tile-n)))))

(defconst supa-help-buffer-name "*Supa Help*")

(defun supa-show-help ()
  (interactive)
  (with-help-window supa-help-buffer-name
    (with-current-buffer supa-help-buffer-name
      (princ "?\tShow this help window\n")
      (princ "\n")
      (princ "RET\tOpen level for editing from the list\n")
      (princ "q\tShow the levels list\n")
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
  (supa-set-tiles-scale (or (cond
                             ((<= text-scale-mode-amount 0) 1)
                             ((>= text-scale-mode-amount 3) 4))
                            (1+ text-scale-mode-amount)))
  (supa-edit-level-at-point))

(define-derived-mode supa-mode
  special-mode "Supa"
  "Major mode for Supaplex LEVELS.DAT file."
  (read-only-mode)
  (setq truncate-lines 't)
  (supa-list-levels)
  (derived-mode-set-keymap 'supa-mode)
  (supa-set-tiles-scale 1)
  (add-hook 'text-scale-mode-hook 'supa-text-scale-adjust-hook)
  (message "? - Help"))

(define-minor-mode supa-level-mode
  "Minor mode for Supaplex level within the LEVELS.DAT file.")

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
