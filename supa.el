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
(defconst supa-level-max 111)
(defconst supa-level-size-in-bytes 1536) ;; 3 disk sectors apparently

(defun supa-level-number-at-point ()
  (1+ (/ (1- (point))
         supa-level-size-in-bytes)))

(defconst supa-level-rows 24)
(defconst supa-level-cols 60)
(defconst supa-level-total-tiles (* supa-level-rows supa-level-cols))

;; TODO: allow to customize
(defvar supa-tiles-image nil)
(defvar supa-tile-size nil)

(defun supa-set-tiles-scale (n)
  (setq-local supa-tile-size (* 16 n))
  (setq-local supa-tiles-image
              (find-image (list (list :type 'png
                                      :file (format "~/src/supa-el/tiles_x%d.png" n))))))

;; TODO: could be reused
(defun supa-print-levels-list ()
  (save-restriction
    (widen)
    (dotimes (lvl supa-level-max)
      (let* ((start-pos (1+ (* supa-level-size-in-bytes lvl)))
             (end-pos   (+ start-pos supa-level-size-in-bytes))
             (name (buffer-substring (+ start-pos supa-level-total-tiles 6)
                                     (+ start-pos supa-level-total-tiles 29))))
        (princ (format "%03d %s\n" (1+ lvl) name))))))

(defun supa-overwrite-level-lst ()
  (let* ((levels-list-str (with-output-to-string (supa-print-levels-list)))

         (lst-file (concat (file-name-directory (buffer-file-name)) "LEVEL.LST"))
         (buffer (find-file-literally lst-file)))
    (with-current-buffer buffer
      (erase-buffer)
      (let ((standard-output buffer))
        (princ levels-list-str))
      (save-buffer)
      (kill-buffer))))

(defun supa-show-levels-list ()
  (remove-overlays)
  (widen)
  (with-silent-modifications
    (set-text-properties (point-min) (point-max) nil)
    (dotimes (lvl supa-level-max)
      (let* ((start-pos (1+ (* supa-level-size-in-bytes lvl)))
             (end-pos   (+ start-pos supa-level-size-in-bytes))
             (name (buffer-substring (+ start-pos supa-level-total-tiles 6)
                                     (+ start-pos supa-level-total-tiles 29))))
        (put-text-property start-pos end-pos
                           'display (format "%03d %s\n" (1+ lvl) name))))))

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

(defun supa-edit-level (level-n)
  (remove-overlays)
  (widen)
  (let* ((start-pos    (1+ (* supa-level-size-in-bytes (1- level-n))))
         (meta-pos     (+ start-pos supa-level-total-tiles))
         (meta-end-pos (+ meta-pos 96))
         (level-bytes  (buffer-substring start-pos meta-pos))
         (meta-bytes   (buffer-substring meta-pos meta-end-pos))
         (info-cnt     (supa-count-level-infotrons level-bytes))
         (info-req     (aref meta-bytes 30))
         (zero-height-newline (propertize "\n" 'face '(:height 0))))
    (with-silent-modifications
      (set-text-properties (point-min) (point-max) nil)
      (narrow-to-region start-pos (+ start-pos supa-level-size-in-bytes))
      (dotimes (y supa-level-rows)
        (let* ((row-pos     (+ start-pos (* supa-level-cols y)))
               (row-end-pos (+ row-pos supa-level-cols)))
          (overlay-put (make-overlay row-end-pos row-end-pos)
                       'before-string zero-height-newline)
          (dotimes (x supa-level-cols)
            (supa-put-text-prop-tile (+ row-pos x)
                                     (aref level-bytes (+ (* supa-level-cols y) x))))))
      (set-text-properties meta-pos meta-end-pos nil)
      (put-text-property   meta-pos (+ meta-pos 6)
                           'display (format "%03d " level-n))
      (put-text-property   (+ meta-pos 29) (+ meta-pos 30)
                           'display (format " %03d / %03d " info-cnt info-req))
      (supa-put-text-prop-tile (+ meta-pos 30) 4)
      (put-text-property   (+ meta-pos 31) meta-end-pos 'invisible t))))

(defun supa-edit-level-at-point ()
  (supa-edit-level (supa-level-number-at-point)))

(defun supa-level-start-pos (&optional pos)
  (let ((p (1- (or pos (point)))))
    (1+ (- p (% p supa-level-size-in-bytes)))))

(defun supa-update-info-count ()
  (let* ((start-pos    (supa-level-start-pos))
         (meta-pos     (+ start-pos supa-level-total-tiles))
         (meta-end-pos (+ meta-pos 96))
         (level-bytes  (buffer-substring start-pos meta-pos))
         (meta-bytes   (buffer-substring meta-pos meta-end-pos))
         (info-cnt     (supa-count-level-infotrons level-bytes))
         (info-req     (aref meta-bytes 30)))
    (put-text-property (+ meta-pos 29) (+ meta-pos 30)
                       'display
                       (format " %03d / %03d " info-cnt info-req))))

(defun supa-set-required-infotrons (n)
  (interactive "nRequired infotrons (0-255): ")
  (if (not (<= 0 n 255))
    (message "The count must be between 0 and 255!")
    (let* ((inhibit-read-only 't)
           (start-pos (supa-level-start-pos))
           (meta-pos  (+ start-pos supa-level-total-tiles)))
      (goto-char (+ meta-pos 30))
      (delete-char 1)
      (insert n)
      ;; ignore for undo
      (with-silent-modifications
        (supa-update-info-count)
        ;; TODO: shouldn't be needed, actually
        (supa-put-text-prop-tile (+ meta-pos 30) 4)))))

(defun supa-normal-level-name (name)
  (let* ((name (upcase (substring name 0 (min 23 (length name)))))
         (len (length name)))
    (or (cond
         ((= len 23) name)
         ((= len 22) (concat name "-"))
         ((= len 21) (concat "-" name "-")))

        (let* ((left-pad  (/ (max 0 (- 21 len)) 2))
               (right-pad    (max 0 (- 21 len left-pad))))
          (concat (make-string left-pad ?-)
                  " "
                  name
                  " "
                  (make-string right-pad ?-))))))

(defun supa-level-rename (name)
  (interactive "sLevel name: ")
  (let* (
         (inhibit-read-only 't)
         (start-pos (supa-level-start-pos))
         (meta-pos  (+ start-pos supa-level-total-tiles)))
    (goto-char (+ meta-pos 6))
    (delete-char 23)
    ;; ignore for undo
    (with-silent-modifications
      (insert (supa-normal-level-name name)))))

(defun supa-is-editable-tile (&optional pos)
  (let* ((p (% (1- (or pos (point)))
               supa-level-size-in-bytes))
         (y (/ p supa-level-cols))
         (x (% p supa-level-cols)))
    (and (< 0 y (1- supa-level-rows))
         (< 0 x (1- supa-level-cols)))))

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

(defun supa-save-dat-and-lst ()
  (interactive)
  (supa-overwrite-level-lst)
  (save-buffer))

(defconst supa-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'supa-level-mode)
    (define-key map (kbd "?")   'supa-show-help)
    (define-key map [remap save-buffer] 'supa-save-dat-and-lst)
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
      (princ "R\tRename the level\n")
      (princ "I\tSet level's infotrons requirement\n")
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
  (set-buffer-multibyte nil)
  (setq-local truncate-lines 't)
  (supa-show-levels-list)
  (derived-mode-set-keymap 'supa-mode)
  (supa-set-tiles-scale 1)
  (add-hook 'text-scale-mode-hook 'supa-text-scale-adjust-hook)
  (message "? - Help"))

(defconst supa-level-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "R") 'supa-level-rename)
    (define-key map (kbd "I") 'supa-set-required-infotrons)
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

;; (defun supa-level-clear ()
;;   (interactive)
;;   (delete-region 1 (1+ supa-level-total-tiles))
;;   (dotimes (x supa-level-cols)
;;     (insert-char 6))
;;   (dotimes (y ((- supa-level-rows 2))
;;     (insert-char 6)
;;     (dotimes (x (- supa-level-cols 2))
;;       (insert-char 2))
;;     (insert-char 6))
;;   (dotimes (x supa-level-cols)
;;     (insert-char 6)))
