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
  (min (1+ (/ (1- (point))
              supa-level-size-in-bytes))
       supa-level-max))

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
  (widen)
  ;; some overlays may be outside the current restriction, so widen first
  (remove-overlays)
  (with-silent-modifications
    (set-text-properties (point-min) (point-max) nil)
    (dotimes (lvl supa-level-max)
      (let* ((start-pos    (1+ (* supa-level-size-in-bytes lvl)))
             (meta-pos     (+ start-pos supa-level-total-tiles))
             (meta-end-pos (+ meta-pos 96)))
        (put-text-property start-pos (+ meta-pos 6) 'display (format "%03d " (1+ lvl)))
        ;; the level name goes unchanged here, which allows searching for it
        (put-text-property (+ meta-pos 29) meta-end-pos 'display (format "\n"))))))

;; Also used in the help buffer, so no "level" in the func name:
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

(defun supa-level-update-info-line ()
  (let* ((level-n        (supa-level-number-at-point))
         (start-pos      (1+ (* supa-level-size-in-bytes (1- level-n))))
         (meta-pos       (+ start-pos supa-level-total-tiles))
         (level-bytes    (buffer-substring start-pos meta-pos))
         (meta-end-pos   (+ meta-pos 96))
         (meta-bytes     (buffer-substring meta-pos meta-end-pos))
         (info-cnt       (seq-count (lambda (x) (= x 4)) level-bytes))
         (info-req       (aref meta-bytes 30))
         (init-gravity   (= 1 (aref meta-bytes 4)))
         (level-info-str (format "\n%03d %s %03d / %03d%s"
                                 level-n
                                 (substring meta-bytes 6 29)
                                 info-cnt
                                 info-req
                                 (if init-gravity " Gravity" ""))))
    (put-text-property meta-pos meta-end-pos 'display level-info-str)))

(defun supa-level-edit-at-point ()
  (widen)
  ;; some overlays may be outside the current restriction, so widen first
  (remove-overlays)
  (let* ((level-n     (supa-level-number-at-point))
         (start-pos   (1+ (* supa-level-size-in-bytes (1- level-n))))
         (meta-pos    (+ start-pos supa-level-total-tiles))
         (end-pos     (+ start-pos supa-level-size-in-bytes))
         ;; narrow first, to prevent accessing bytes from other levels:
         (_           (narrow-to-region start-pos end-pos))
         (level-bytes (buffer-substring start-pos meta-pos)))

    (with-silent-modifications
      (set-text-properties start-pos end-pos nil)

      (dotimes (y supa-level-rows)
        (let* ((row-offset (* supa-level-cols y)))

          (dotimes (x supa-level-cols)
            (let ((tile-offset (+ row-offset x)))
              (supa-put-text-prop-tile (+ start-pos tile-offset)
                                       (aref level-bytes tile-offset))))

          (let ((row-end-pos (+ start-pos row-offset supa-level-cols)))
            (overlay-put (make-overlay row-end-pos row-end-pos)
                         'before-string (propertize "\n" 'face '(:height 0))))))

      (supa-level-update-info-line))))

(defun supa-level-start-pos (&optional pos)
  (let ((p (1- (or pos (point)))))
    (1+ (- p (% p supa-level-size-in-bytes)))))

(defun supa-level-normal-name (name)
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
  (save-excursion
    (let* ((inhibit-read-only 't)
           (start-pos (supa-level-start-pos))
           (meta-pos  (+ start-pos supa-level-total-tiles)))
      (goto-char (+ meta-pos 6))
      (delete-char 23)
      (insert (supa-level-normal-name name))
      (supa-level-update-info-line))))

(defun supa-level-set-requirement (n)
  (interactive "nRequired infotrons (0-255): ")
  (if (not (<= 0 n 255))
    (message "The count must be between 0 and 255!")
    (save-excursion
      (let* ((inhibit-read-only 't)
             (start-pos (supa-level-start-pos))
             (meta-pos  (+ start-pos supa-level-total-tiles)))
        (goto-char (+ meta-pos 30))
        (delete-char 1)
        (insert n)
        (supa-level-update-info-line)))))

(defun supa-level-toggle-init-gravity ()
  (interactive)
  (save-excursion
    (let* ((inhibit-read-only 't)
           (start-pos    (supa-level-start-pos))
           (meta-pos     (+ start-pos supa-level-total-tiles))
           (meta-end-pos (+ meta-pos 96))
           (meta-bytes   (buffer-substring meta-pos meta-end-pos))
           (init-gravity (= 1 (aref meta-bytes 4))))
      (goto-char (+ meta-pos 4))
      (delete-char 1)
      (insert (if init-gravity 0 1))
      (supa-level-update-info-line))))


(defun supa-level-is-editable-tile (&optional pos)
  (let* ((p (% (1- (or pos (point)))
               supa-level-size-in-bytes))
         (y (/ p supa-level-cols))
         (x (% p supa-level-cols)))
    (and (< 0 y (1- supa-level-rows))
         (< 0 x (1- supa-level-cols)))))

(defun supa-level-tile-at-point ()
  (char-after))

(defun supa-level-set-tile-at-point (tile-n)
  (when (supa-level-is-editable-tile)
    (let ((inhibit-read-only 't))
      (delete-char 1)
      (insert tile-n)
      (backward-char)
      (supa-put-text-prop-tile (point) tile-n)
      (with-silent-modifications
        (supa-level-update-info-line)))))

(defun supa-level-refresh-text-prop-tile-at-point ()
  (supa-put-text-prop-tile (point) (supa-level-tile-at-point)))

(defun supa-undo (&optional arg)
  (interactive)
  (let ((inhibit-read-only 't))
    ;; save the point, or it may jump to the end of narrowing and gets stuck:
    (save-excursion
      (undo arg)
      (with-silent-modifications
        ;; undo also restores the original tile size, so enforce the current one:
        (supa-level-refresh-text-prop-tile-at-point)
        (supa-level-update-info-line)))))

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

(defun supa-level-toggle-port-gravity-at-point ()
  (interactive)
  (when-let ((old-tile-n (supa-level-tile-at-point))
             (new-tile-n (supa-port-tile-toggled-gravity old-tile-n)))
      (supa-level-set-tile-at-point new-tile-n)))

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
    (">"   . ( 9 port-left-to-right))
    ("V"   . (10 port-up-to-down))
    ("<"   . (11 port-right-to-left))
    ("^"   . (12 port-down-to-up))
    ;; we skip the 4 special ports, as there is a separate binding to toggle them
    ("s"   . (17 snick-snack))
    ("y"   . (18 yellow-disk))
    ("T"   . (19 terminal))
    ("r"   . (20 red-disk))
    ("|"   . (21 port-two-way-vert))
    ("-"   . (22 port-two-way-horz))
    ("+"   . (23 port-four-way))
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
  (when (buffer-modified-p)
    (supa-overwrite-level-lst))
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
  ;; the tile vars are buffer local, so set them explicitly in the help buffer:
  (let ((current-tiles-image supa-tiles-image)
        (current-tile-size   supa-tile-size))

    (with-help-window supa-help-buffer-name
      (with-current-buffer supa-help-buffer-name

        (setq-local supa-tiles-image current-tiles-image)
        (setq-local supa-tile-size   current-tile-size)
        (princ "Welcome to Supaplex level editing in Emacs!\n")
        (princ "\n")
        (princ "Hint: you can increase the tiles size (up to 4x) by using the text scaling commands\n")
        (princ "(which are usually bound to C-x C-=, C-x C--, etc.)\n")
        (princ "\n")
        (princ "?\tShow this help window\n")
        (princ "RET\tEnter a level for editing / go back to the levels list\n")
        (princ "\n")
        (princ "When editing a level:\n")
        (princ "m\tGo to Murphy\n")
        (princ "R\tRename the level\n")
        (princ "I\tSet level's infotrons requirement\n")
        (princ "G\tToggle level's initial gravity flag\n")
        (princ "g\tToggle gravity flag of a port (when pointing at one)\n")
        (princ "u, U\tUndo\n")
        (princ "\n")
        (princ "The following keys replace the tile at point:\n")
        ;; key, tile, sym
        (seq-doseq (kts supa-kbd-tile-alist)
          (let ((key    (car kts))
                (tile-n (cadr kts))
                (sym    (caddr kts)))
            (princ key)
            (princ "\t.")
            (supa-put-text-prop-tile (buffer-size) tile-n)
            (princ " ")
            (princ sym)
            (princ "\n")))))))

(defun supa-text-scale-adjust-hook ()
  (supa-set-tiles-scale (1+ (max 0 (min text-scale-mode-amount 3))))
  (when supa-level-mode
    ;; this just refreshes the level currently being edited
    (supa-level-edit-at-point)))

(define-derived-mode supa-mode
  special-mode "Supa"
  "Major mode for Supaplex LEVELS.DAT file."
  (with-silent-modifications
    (set-buffer-multibyte nil))
  (setq-local truncate-lines 't)
  (supa-show-levels-list)
  (derived-mode-set-keymap 'supa-mode)
  (supa-set-tiles-scale 1)
  (add-hook 'text-scale-mode-hook 'supa-text-scale-adjust-hook)
  (message "? - Help, RET - Enter a level for editing"))


(defun supa-level-adjust-point (fn)
  (let* ((pos (point))
         (p   (% (1- pos)
                 supa-level-size-in-bytes))
         (y   (/ p supa-level-cols))
         (x   (% p supa-level-cols))
         (xy  (funcall fn x y))
         (x   (car xy))
         (y   (cdr xy)))
    (goto-char (+ (- pos p)
                  (* (max 0 (min y (1- supa-level-rows)))
                     supa-level-cols)
                  (max 0 (min x (1- supa-level-cols)))))))

(defun supa-level-next-row ()
  (interactive)
  (supa-level-adjust-point (lambda (x y) (cons x (1+ y)))))

(defun supa-level-prev-row ()
  (interactive)
  (supa-level-adjust-point (lambda (x y) (cons x (1- y)))))

(defun supa-level-next-col ()
  (interactive)
  (supa-level-adjust-point (lambda (x y) (cons (1+ x) y))))

(defun supa-level-prev-col ()
  (interactive)
  (supa-level-adjust-point (lambda (x y) (cons (1- x) y))))


(defun supa-level-goto-murphy ()
  (interactive)
  (goto-char (point-min))
  (while (not (= 3 (supa-level-tile-at-point)))
    (forward-char)))


(defconst supa-level-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "m") 'supa-level-goto-murphy)
    (define-key map (kbd "R") 'supa-level-rename)
    (define-key map (kbd "I") 'supa-level-set-requirement)
    (define-key map (kbd "G") 'supa-level-toggle-init-gravity)
    (define-key map (kbd "g") 'supa-level-toggle-port-gravity-at-point)

    (define-key map (kbd "u") 'supa-undo)
    (define-key map (kbd "U") 'supa-undo)
    (define-key map [remap undo] 'supa-undo)

    (define-key map [remap next-line]     'supa-level-next-row)
    (define-key map [remap previous-line] 'supa-level-prev-row)

    (define-key map [remap right-char]    'supa-level-next-col)
    (define-key map [remap forward-char]  'supa-level-next-col)
    (define-key map [remap left-char]     'supa-level-prev-col)
    (define-key map [remap backward-char] 'supa-level-prev-col)

    ;; key, tile, sym
    (seq-doseq (kts supa-kbd-tile-alist)
      (let ((key    (kbd (car kts)))
            (tile-n (cadr kts)))
        (define-key map key
          (lambda ()
            (interactive)
            (supa-level-set-tile-at-point tile-n)))))

    map))

(define-minor-mode supa-level-mode
  "Minor mode for Supaplex level within the LEVELS.DAT file."
  nil
  " SupaLevel"
  'supa-level-mode-map
  :after-hook (if supa-level-mode
                  (progn
                    (supa-level-edit-at-point)
                    (supa-level-goto-murphy))
                  (supa-show-levels-list)))

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
