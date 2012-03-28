;;; -*- Mode:Emacs-Lisp -*-
;;; etos.el --- Run ETOS in an [X]Emacs shell buffer

;; Copyright (C) 1998-1999 Universite de Montreal, All Rights Reserved.

;; Authors: Marc Feeley <feeley@iro.umontreal.ca>

;; To use this package, make sure this file is accessible from your
;; load-path and that the following line is in your ".emacs" file:
;;
;; (require 'etos)
;;
;; You can then start a shell with "M-x shell" and run the ETOS compiler
;; from that shell.  To pinpoint the source code of an error, place the
;; cursor on the error message an type "\M-;" (escape semicolon).

;------------------------------------------------------------------------------

;; User overridable parameters.

(defvar etos-pinpoint-command "\M-;"
  "Emacs keybinding for pinpointing the Erlang source code of an error.")

(defvar etos-highlight-source-color "gold"
  "Color of the overlay for highlighting Erlang source code.")

(defvar etos-highlight-error-color "gray"
  "Color of the overlay for highlighting error messages.")

(defvar etos-highlight-source-face
  (let ((face 'etos-highlight-source-face))
    (condition-case nil
        (progn
          (make-face face)
          (if (x-display-color-p)
              (set-face-background face etos-highlight-source-color)
              (set-face-underline-p face t)))
        (error (setq face nil)))
    face)
  "Face of overlay for highlighting Erlang source code.")

(defvar etos-highlight-error-face
  (let ((face 'etos-highlight-error-face))
    (condition-case nil
        (progn
          (make-face face)
          (if (x-display-color-p)
              (set-face-background face etos-highlight-error-color)
              (set-face-underline-p face t)))
        (error (setq face nil)))
    face)
  "Face of overlay for highlighting error messages.")

(defvar etos-new-window-height 8
  "Height of a window opened to highlight Erlang source code.")

(defvar etos-move-to-highlighted (not etos-highlight-source-face)
  "Flag to move to window opened to highlight Erlang source code.")

(setq etos-location-regexp-alist
  '(("^\\([^ :]+\\):\\([0-9]+\\):" -1 2)
    ("\\(\\\"\\(\\\\\\\\\\|\\\\\"\\|[^\\\"\n]\\)+\\\"\\)@\\([0-9]+\\)\\.\\([0-9]+\\)[^-0-9]" 1 3 4)
    ("\\(\\\"\\(\\\\\\\\\\|\\\\\"\\|[^\\\"\n]\\)+\\\"\\)@\\([0-9]+\\)\\.\\([0-9]+\\)-\\([0-9]+\\)\\.\\([0-9]+\\):" 1 3 4 5 6))
;  "Regular expressions to detect location information in error messages"
)

;------------------------------------------------------------------------------

;; Portable functions for FSF Emacs and Xemacs.

(defun window-top-edge (window)
  (if (fboundp 'window-edges)
      (car (cdr (window-edges window)))
      (car (cdr (window-pixel-edges window)))))

;; Xemacs calls its overlays "extents", so we have to use them to emulate 
;; overlays on Xemacs.  Some versions of Xemacs have the portability package
;; "overlays.el" for this, so we could simply do:
;;
;; (condition-case nil ; load "overlay.el" if we have it
;;     (require 'overlay)
;;   (error nil))
;;
;; Unfortunately some versions of Xemacs don't have this package so
;; we explicitly define an interface to extents.

(if (not (fboundp 'make-overlay))
    (defun make-overlay (start end)
      (make-extent start end)))

(if (not (fboundp 'overlay-put))
    (defun overlay-put (overlay prop val)
      (set-extent-property overlay prop val)))

(if (not (fboundp 'move-overlay))
    (defun move-overlay (overlay start end buffer)
      (set-extent-endpoints overlay start end buffer)))

;------------------------------------------------------------------------------

;; Procedures to intercept and process the location information output
;; by Etos.

(defun etos-pinpoint-error ()
  (interactive)
  (let ((locat
         (etos-extract-location (etos-get-current-line))))
    (if locat
        (etos-highlight-location locat))))

(defun etos-get-current-line ()
  (save-excursion
    (beginning-of-line)
    (let ((b (point)))
      (end-of-line)
      (buffer-substring b (point)))))

(defun etos-extract-location (str)
  (let ((location nil)
        (alist etos-location-regexp-alist))
    (while (and (not location) (not (null alist)))
      (let* ((regexp (car alist))
             (x (string-match (car regexp) str)))
        (if x
            (let* ((pos1 (nth 1 regexp))
                   (pos2 (nth 2 regexp))
                   (name (if (< pos1 0)
                             (substring str
                                        (match-beginning (- pos1))
                                        (match-end (- pos1)))
                             (read (substring str
                                              (match-beginning pos1)
                                              (match-end pos1)))))
                   (line1 (read (substring str
                                           (match-beginning pos2)
                                           (match-end pos2)))))
              (if (not (null (cdr (cdr (cdr regexp)))))
                  (let* ((pos3 (nth 3 regexp))
                         (column1 (read (substring str
                                                   (match-beginning pos3)
                                                   (match-end pos3)))))
                    (if (not (null (cdr (cdr (cdr (cdr regexp))))))
                        (let* ((pos4 (nth 4 regexp))
                               (pos5 (nth 5 regexp))
                               (line2 (read (substring str
                                                       (match-beginning pos4)
                                                       (match-end pos4))))
                               (column2 (read (substring str
                                                         (match-beginning pos5)
                                                         (match-end pos5)))))
                          (setq location
                                (list name line1 column1 line2 column2)))
                        (setq location
                              (list name line1 column1 '() '()))))
                  (setq location
                        (list name line1 '() '() '())))))
        (setq alist (cdr alist))))
    location))

(defun etos-highlight-location (locat)
  (let ((name (car locat))
        (line1 (car (cdr locat)))
        (column1 (car (cdr (cdr locat))))
        (line2 (car (cdr (cdr (cdr locat)))))
        (column2 (car (cdr (cdr (cdr (cdr locat)))))))
    (cond ((stringp name)
           (let ((buffer (find-file-noselect name)))
             (if buffer
                 (if column1
                     (let ((p1 (etos-file-pos-to-point buffer
                                                       line1
                                                       column1)))
                       (and p1
                            (let ((p2 (etos-file-pos-to-point buffer
                                                              line2
                                                              column2)))
                              (etos-highlight-source buffer p1 p2))))
                     (let ((p12 (etos-file-line-to-points buffer line1)))
                       (and p12
                            (etos-highlight-source buffer
                                                   (car p12)
                                                   (cdr p12)))))))))))

(defun etos-file-pos-to-point (buffer line column)
  (and line
       (save-excursion
         (set-buffer buffer)
         (goto-line line)
         (forward-char (- column 1))
         (point))))

(defun etos-file-line-to-points (buffer line)
  (and line
       (save-excursion
         (set-buffer buffer)
         (goto-line line)
         (beginning-of-line)
         (let ((b (point)))
           (end-of-line)
           (cons b (point))))))

(defun etos-highlight-source (location-buffer pos1 pos2)

"Highlight the source code at a specific location in a buffer.

The location buffer is the one that contains the region to highlight
and \"pos1\" points to the first character of the region and \"pos2\"
just past the region.  If the location buffer is not visible then we
must display it in a window.  We also have to make sure the
highlighted region is visible, which may require the window to be
scrolled.

Our approach is simple: if the location buffer is not visible then we
split the selected window in 2 and use the bottom window to display
the location buffer.  Before we do the split, we enlarge the window if
it is too small."

  (let* ((location-windows
          (etos-windows-displaying-buffer location-buffer))
         (initially-current-buffer
          (current-buffer))
         (initially-selected-window
          (selected-window)))

    ; "location-windows" is the list of windows containing
    ; the location buffer.

    (if (null location-windows)

        (let* ((window-to-split
                initially-selected-window)
               (height
                (window-height window-to-split)))
          (select-window window-to-split)
          (if (< height (* 2 etos-new-window-height))
              (enlarge-window
               (- (* 2 etos-new-window-height)
                  height)))
          (let ((bottom-window
                 (split-window
                  window-to-split
                  (- (window-height window-to-split)
                     etos-new-window-height))))
            (select-window bottom-window)
            (switch-to-buffer location-buffer)))

        (select-window (car (reverse location-windows))))

    ; Highlight the region in the location buffer.

    (save-excursion
      (set-buffer (window-buffer (selected-window)))
      (goto-char pos1)
      (if (not (pos-visible-in-window-p))
          (recenter (- (/ (window-height) 2) 1)))
      (etos-highlight-source-region
       location-buffer
       pos1
       (or pos2
           (progn
             (condition-case nil
                 (forward-sexp)
               (error ; if forward-sexp fails with this condition name
                (forward-char 1)))
             (point))))
      (etos-highlight-error-region
       initially-current-buffer)
      (goto-char pos1))

    (if (not (eq initially-selected-window (selected-window)))
        (progn
          (goto-char pos1)
          (if (not etos-move-to-highlighted)
              (select-window initially-selected-window))))))

(defun etos-windows-displaying-buffer (buffer)
  (let ((windows '()))
    (walk-windows (function
                   (lambda (w)
                     (if (eq buffer (window-buffer w))
                         (setq windows (cons w windows)))))
                  t
                  'visible)
    (sort windows
          (function
           (lambda (w1 w2)
             (< (window-top-edge w1)
                (window-top-edge w2)))))))

(defvar etos-highlight-source-overlay
  (let ((ovl (make-overlay (point-min) (point-min))))
    (overlay-put ovl 'face etos-highlight-source-face)
    ovl)
  "Overlay for highlighting Erlang source code.")

(defvar etos-highlight-error-overlay
  (let ((ovl (make-overlay (point-min) (point-min))))
    (overlay-put ovl 'face etos-highlight-error-face)
    ovl)
  "Overlay for highlighting error message.")

(defun etos-highlight-source-region (buffer start end)
  (if etos-highlight-source-overlay
      (move-overlay etos-highlight-source-overlay start end buffer)))

(defun etos-highlight-error-region (buffer)
  (if etos-highlight-error-overlay
      (save-excursion
        (set-buffer buffer)
        (beginning-of-line)
        (let ((b (point)))
          (end-of-line)
          (let ((e (point)))
            (move-overlay etos-highlight-error-overlay b e buffer))))))

;------------------------------------------------------------------------------

(defun etos-extend-mode-map (map)
  (define-key map etos-pinpoint-command 'etos-pinpoint-error))

; Redefine next-line and previous-line so that they call etos-pinpoint-error.

(defun next-line (arg)
  (interactive "p")
  (if (and next-line-add-newlines (= arg 1))
      (let ((opoint (point)))
        (end-of-line)
        (if (eobp)
            (newline 1)
          (goto-char opoint)
          (line-move arg)))
    (if (interactive-p)
        (condition-case nil
            (line-move arg)
          ((beginning-of-buffer end-of-buffer) (ding)))
      (line-move arg)))
  (etos-pinpoint-error)
  nil)

(defun previous-line (arg)
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
          (line-move (- arg))
        ((beginning-of-buffer end-of-buffer) (ding)))
    (line-move (- arg)))
  (etos-pinpoint-error)
  nil)

(eval-after-load "shell"
  '(etos-extend-mode-map shell-mode-map))

(provide 'etos)

;------------------------------------------------------------------------------
