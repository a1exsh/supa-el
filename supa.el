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
(defvar supa-border-image nil)
(defvar supa-tile-size nil)

(defun supa-set-tiles-scale (n)
  (setq-local supa-tile-size (* 16 n))
  (setq-local supa-tiles-image
              (find-image (list (list :type 'png
                                      :file (format "~/src/supa-el/tiles_x%d.png" n)))))
  (setq-local supa-border-image
              (find-image (list (list :type 'png
                                      :file (format "~/src/supa-el/border_x%d.png" n))))))

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

(defun supa-put-text-prop-border-top-left (pos)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-border-image
                           (list 'slice
                                 0
                                 0
                                 (/ supa-tile-size 2)
                                 (/ supa-tile-size 2)))))

(defun supa-put-text-prop-border-top (pos)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-border-image
                           (list 'slice
                                 (/ supa-tile-size 2)
                                 0
                                 supa-tile-size
                                 (/ supa-tile-size 2)))))

(defun supa-put-text-prop-border-top-right (pos)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-border-image
                           (list 'slice
                                 (+ supa-tile-size (/ supa-tile-size 2))
                                 0
                                 (/ supa-tile-size 2)
                                 (/ supa-tile-size 2)))))

(defun supa-put-text-prop-border-left (pos)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-border-image
                           (list 'slice
                                 0
                                 (/ supa-tile-size 2)
                                 (/ supa-tile-size 2)
                                 supa-tile-size))))

(defun supa-put-text-prop-border-right (pos)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-border-image
                           (list 'slice
                                 (+ supa-tile-size (/ supa-tile-size 2))
                                 (/ supa-tile-size 2)
                                 (/ supa-tile-size 2)
                                 supa-tile-size))))

(defun supa-put-text-prop-border-bottom-left (pos)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-border-image
                           (list 'slice
                                 0
                                 (+ supa-tile-size (/ supa-tile-size 2))
                                 (/ supa-tile-size 2)
                                 (/ supa-tile-size 2)))))

(defun supa-put-text-prop-border-bottom (pos)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-border-image
                           (list 'slice
                                 (/ supa-tile-size 2)
                                 (+ supa-tile-size (/ supa-tile-size 2))
                                 supa-tile-size
                                 (/ supa-tile-size 2)))))

(defun supa-put-text-prop-border-bottom-right (pos)
  (put-text-property pos
                     (1+ pos)
                     'display
                     (list supa-border-image
                           (list 'slice
                                 (+ supa-tile-size (/ supa-tile-size 2))
                                 (+ supa-tile-size (/ supa-tile-size 2))
                                 (/ supa-tile-size 2)
                                 (/ supa-tile-size 2)))))

(defun supa-level-refresh-tile-at-point ()
  (supa-put-text-prop-tile (point) (supa-level-tile-at-point)))

(defun supa-level-refresh-tiles ()
  (remove-overlays)
  (save-excursion
    (goto-char (point-min))

    (dotimes (y supa-level-rows)
      (dotimes (x supa-level-cols)
        (cond
         ((= y 0)
          (cond
           ((= x 0)                    (supa-put-text-prop-border-top-left (point)))
           ((< x (1- supa-level-cols)) (supa-put-text-prop-border-top (point)))
           (t                          (supa-put-text-prop-border-top-right (point)))))
         ((< y (1- supa-level-rows))
          (cond
           ((= x 0)                    (supa-put-text-prop-border-left (point)))
           ((< x (1- supa-level-cols)) (supa-level-refresh-tile-at-point)) ;; tile
           (t                          (supa-put-text-prop-border-right (point)))))
         (t
          (cond
           ((= x 0)                    (supa-put-text-prop-border-bottom-left (point)))
           ((< x (1- supa-level-cols)) (supa-put-text-prop-border-bottom (point)))
           (t                          (supa-put-text-prop-border-bottom-right (point))))))
        (forward-char))

      (overlay-put (make-overlay (point) (point))
                   'before-string (propertize "\n" 'face '(:height 0))))))

(defun supa-level-edit (level-n)
  (widen)
  (let* ((start-pos (1+ (* supa-level-size-in-bytes (1- level-n))))
         (end-pos   (+ start-pos supa-level-size-in-bytes)))
    ;; narrow first and reset point to prevent accessing bytes from other levels:
    (narrow-to-region start-pos end-pos)
    (goto-char (point-min))

    (with-silent-modifications
      (set-text-properties start-pos end-pos nil)
      (supa-level-refresh-tiles)
      (supa-level-update-info-line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Gravity ports database
;;
;; Part of the level metadata.  1 byte + up to 10 * 6 bytes records
;;
;; offset	size	meaning
;; 31		1	number of records (0-10)
;;
;; each record:
;;  0		1	high byte of xy
;;  1		1	low  byte of xy
;;  2		1	gravity flag (0=off)
;;  3		3	not used in the original level set
;;
;; Port coordinates are encoded in two bytes as follows: 2 * ((60 * y) + x)
;;
;; For more details see:
;;
;; https://github.com/sergiou87/open-supaplex/blob/v7.1.2/resources/SPFIX63.DOC#L2072-L2092
;;
(defconst supa-level-ports-db-max-count 10)

(defun supa-level-ports-db-pos ()
  (+ (supa-level-start-pos) supa-level-total-tiles 31))

(defun supa-level-ports-db-enc-xy2b (x y)
  (* 2 (+ x (* y supa-level-cols))))

(defun supa-level-ports-db-dec-xy2b (hi lo)
  (supa-level-pos-xy (1+ (/ (+ (* 256 hi) lo) 2))))

(defun supa-level-ports-db-vector ()
  (let* ((ports-db-pos (supa-level-ports-db-pos))
         (port-count   (char-after ports-db-pos)))
    (->> (number-sequence 0 (1- port-count))
         (seq-map (lambda (port-n)
                    (let* ((port-pos (+ ports-db-pos 1 (* 6 port-n)))
                           (hi-xy    (char-after    port-pos))
                           (lo-xy    (char-after (+ port-pos 1)))
                           (gravity  (char-after (+ port-pos 2))))
                      (cons (supa-level-ports-db-dec-xy2b hi-xy lo-xy)
                            gravity))))
         (apply 'vector))))

(defun supa-level-ports-db-xy2b (port-n)
  (let* ((ports-db-pos (supa-level-ports-db-pos))
         (port-count   (char-after ports-db-pos)))
    (when (< port-n port-count)
      (let* ((port-pos (+ ports-db-pos 1 (* 6 port-n) 0))
             (hi (char-after port-pos))
             (lo (char-after (1+ port-pos))))
        (+ (* 256 hi) lo)))))

(defun supa-level-ports-db-number-at-xy (x y)
  (let* ((xy2b         (supa-level-ports-db-enc-xy2b x y))
         (ports-db-pos (supa-level-ports-db-pos))
         (port-count   (char-after ports-db-pos)))
    (seq-first
     (seq-filter (lambda (port-n) (= xy2b (supa-level-ports-db-xy2b port-n)))
                 (number-sequence 0 (1- port-count))))))

(defun supa-level-ports-db-toggle-gravity (port-n)
  (let* ((ports-db-pos (+ (supa-level-start-pos) supa-level-total-tiles 31))
         (port-count   (char-after ports-db-pos)))
    (when (< port-n port-count)
      (save-excursion
        (goto-char (+ ports-db-pos 1 (* 6 port-n) 2))
        (let ((gravity (= 1 (char-after))))
          (delete-char 1)
          (insert (if gravity 0 1)))))))

(defun supa-level-gravity-port-tile-p (tile-n)
  (<= 13 tile-n 16))

(defun supa-level-toggle-port-gravity-at-point ()
  (interactive)
  (if (and (supa-level-editable-tile-p)
           (supa-level-gravity-port-tile-p (supa-level-tile-at-point)))
      (let ((inhibit-read-only t)
            (xy (supa-level-pos-xy)))
        (supa-level-ports-db-toggle-gravity
         (supa-level-ports-db-number-at-xy (car xy) (cdr xy)))
        (with-silent-modifications
          (supa-level-update-info-line)))

    (message "Point is not at a gravity port!")))

(defun supa-level-ports-db-dump (ports-db-vec)
  (concat
   (string (length ports-db-vec))
   (->> ports-db-vec
        (seq-mapcat
         (lambda (port)
           (let* ((hi-lo (supa-level-ports-db-enc-xy2b (caar port) (cdar port)))
                  (hi    (/ hi-lo 256))
                  (lo    (% hi-lo 256)))
             (list hi lo (cdr port) 0 0 0))))
        (seq-concatenate 'string))))

(defun supa-level-ports-db-update (ports-db-vec)
  (save-excursion
    (let ((ports-db-pos (supa-level-ports-db-pos))
          (ports-db-str (supa-level-ports-db-dump ports-db-vec)))
      (goto-char ports-db-pos)
      (delete-char (length ports-db-str))
      (insert ports-db-str))))

(defun supa-level-ports-db-report (ports-db-vec)
  (when (not (seq-empty-p ports-db-vec))
    (princ "     x, y   gravity \n")
    (princ "--------------------\n")
    (->> ports-db-vec
         (seq-do-indexed
          (lambda (port port-n)
            (princ (format "%d: %s %s\n"
                           port-n
                           (supa-level-format-pos-xy (caar port) (cdar port))
                           (if (= 1 (cdr port)) "[.....ON]" "[off....]"))))))))

;; TODO: no longer "a single line"
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
         (level-info-str (format "\n%03d %s %03d / %03d%s\n\n%s"
                                 level-n
                                 (substring meta-bytes 6 29)
                                 info-cnt
                                 info-req
                                 (if init-gravity " Gravity" "")
                                 (with-output-to-string
                                   (supa-level-ports-db-report (supa-level-ports-db-vector))))))
    (put-text-property meta-pos meta-end-pos 'display level-info-str)))

(defun supa-level-edit-at-point ()
  (supa-level-edit (supa-level-number-at-point)))

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


(defun supa-level-editable-tile-p (&optional pos)
  "Returns t if tile coordinates at position (or point) are
within the editable area of the level (that is, excluding the
borders)."
  (let ((xy (supa-level-pos-xy pos)))
    (and (< 0 (cdr xy) (1- supa-level-rows))
         (< 0 (car xy) (1- supa-level-cols)))))

(defun supa-level-tile-at-point ()
  (char-after))

(defun supa-level-set-tile-at-point (tile-n)
  (when (supa-level-editable-tile-p)
    (let* ((inhibit-read-only 't)
           (tile-xy    (supa-level-pos-xy))
           (old-tile-n (char-after))
           (new-gravity-port (supa-level-gravity-port-tile-p tile-n))
           (old-gravity-port (supa-level-gravity-port-tile-p old-tile-n))
           (ports-db-vec (supa-level-ports-db-vector))
           (ports-db-vec
            (if new-gravity-port
                (when (not old-gravity-port)
                  ;; need to add an entry
                  (if (= supa-level-ports-db-max-count (length ports-db-vec))
                      (error "All %d port slots are taken"
                             supa-level-ports-db-max-count)
                    (append ports-db-vec (list (cons tile-xy 1)))))
              (when old-gravity-port
                ;; remove old port entry
                (->> ports-db-vec
                     (seq-remove (lambda (port) (equal tile-xy (car port))))
                     (apply 'vector))))))
      (delete-char 1)
      (insert tile-n)
      (backward-char)
      (supa-put-text-prop-tile (point) tile-n)
      (when ports-db-vec
        (supa-level-ports-db-update ports-db-vec))
      (with-silent-modifications
        (supa-level-update-info-line)))))

(defun supa-undo (&optional arg)
  (interactive)
  (let ((inhibit-read-only 't))
    ;; save the point, or it may jump to the end of narrowing and gets stuck:
    (save-excursion
      (undo arg)
      (with-silent-modifications
        ;; undo also restores the original tile size, so enforce the current one:
        (supa-level-refresh-tiles)
        (supa-level-update-info-line)))))


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
    ("M->" . (13 gravity-port-left-to-right))
    ("M-V" . (14 gravity-port-up-to-down))
    ("M-<" . (15 gravity-port-right-to-left))
    ("M-^" . (16 gravity-port-down-to-up))
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
        (princ "m\tMove point to Murphy\n")
        (princ "R\tRename the level\n")
        (princ "I\tSet level's infotrons requirement\n")
        (princ "G\tToggle level's initial gravity flag\n")
        (princ "g\tToggle gravity flag of a port (when pointing at one)\n")
        (princ "u, U\tUndo\n")
        (princ "M-n\tGo to next level\n")
        (princ "M-p\tGo to previous level\n")
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
    (with-silent-modifications
      (supa-level-refresh-tiles))))

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


(defun supa-level-pos-xy (&optional pos)
  (let* ((pos (% (1- (or pos (point)))
                 supa-level-size-in-bytes)))
    (cons (% pos supa-level-cols)
          (/ pos supa-level-cols))))

(defun supa-level-format-pos-xy (x y)
  (format "(%2d,%2d)" x y))

(defun supa-level-format-pos (&optional pos)
  (let ((xy (supa-level-pos-xy pos)))
    (supa-level-format-pos-xy (car xy) (cdr xy))))


(defun supa-level-adjust-point (fn)
  (let* ((pos (point))
         (xy  (supa-level-pos-xy pos))
         (xy  (funcall fn (car xy) (cdr xy)))
         (x   (max 0 (min (car xy) (1- supa-level-cols))))
         (y   (max 0 (min (cdr xy) (1- supa-level-rows)))))
    (goto-char (+ (supa-level-start-pos pos)
                  (* y supa-level-cols)
                  x)))
  ;; TODO: ideally, should not be needed
  (force-mode-line-update))

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

(defun supa-level-goto-pos (pos-str)
  (interactive "sTile position (x,y): ")
  (save-match-data
    (let* ((strs (split-string pos-str ","))
           (x    (string-to-number (or (car  strs) "0")))
           (y    (string-to-number (or (cadr strs) "0"))))
      (supa-level-adjust-point (lambda (_x _y) (cons x y))))))

(defun supa-level-goto-murphy ()
  (interactive)
  (goto-char (point-min))
  (while (not (= 3 (supa-level-tile-at-point)))
    (forward-char))
  (force-mode-line-update))


(defun supa-level-edit-next ()
  (interactive)
  (supa-level-edit (max 1 (min (1+ (supa-level-number-at-point))
                               supa-level-max)))
  (supa-level-goto-murphy))

(defun supa-level-edit-prev ()
  (interactive)
  (supa-level-edit (max 1 (min (1- (supa-level-number-at-point))
                               supa-level-max)))
  (supa-level-goto-murphy))


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

    (define-key map [remap goto-line] 'supa-level-goto-pos)
    (define-key map [remap goto-char] 'supa-level-goto-pos)

    (define-key map (kbd "M-n") 'supa-level-edit-next)
    (define-key map (kbd "M-p") 'supa-level-edit-prev)

    ;; key, tile, sym
    (seq-doseq (kts supa-kbd-tile-alist)
      (let ((key    (kbd (car kts)))
            (tile-n (cadr kts)))
        (define-key map key
          (lambda ()
            (interactive)
            (supa-level-set-tile-at-point tile-n)))))

    map))

(defun supa-level-enter ()
  (setq-local mode-line-position '(:eval (supa-level-format-pos)))
  (supa-level-edit-at-point)
  (supa-level-goto-murphy))

(defun supa-level-leave ()
  (setq-local mode-line-position 'mode-line-percent-position)
  (goto-char (point-min))               ; avoids point jumping around
  (supa-show-levels-list))

(define-minor-mode supa-level-mode
  "Minor mode for Supaplex level within the LEVELS.DAT file."
  nil
  " SupaLevel"
  'supa-level-mode-map
  :after-hook (if supa-level-mode
                (supa-level-enter)
                (supa-level-leave)))

(defun supa-level-clear ()
  (interactive)
  (let ((inhibit-read-only t)
        (start-pos (supa-level-start-pos)))
    (delete-region start-pos (+ start-pos supa-level-total-tiles))
    (dotimes (x supa-level-cols)
      (insert-char 6))
    (dotimes (y (- supa-level-rows 2))
      (insert-char 6)
      (dotimes (x (- supa-level-cols 2))
        (insert-char 2))
      (insert-char 6))
    (dotimes (x supa-level-cols)
      (insert-char 6)))
  (with-silent-modifications
    (supa-level-update-info-line)
    (supa-level-refresh-tiles)))
