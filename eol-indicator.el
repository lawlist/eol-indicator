(defvar my-eol-ruler nil
"A horizontal ruler stretching from eol (end of line) to the window edge.")
(make-variable-buffer-local 'my-eol-ruler)

(defvar my-eol-pilcrow nil
"A pilcrow symbol placed at the end of every line except the current line.")
(make-variable-buffer-local 'my-eol-pilcrow)

(defsubst col-at-eovl ()
  (save-excursion
    (let ((movement-indicator (vertical-motion 1)))
      (when (= movement-indicator 1)
        (backward-char 1)))
    (- (current-column) (progn (vertical-motion 0) (current-column)))))

(defun my-eol-ruler-function ()
  (when (not (minibufferp (current-buffer)))
    (when (or my-eol-ruler my-eol-pilcrow)
      (dolist (description `(,my-eol-ruler ,my-eol-pilcrow))
        (remove-overlays (point-min) (point-max) 'after-string description)))
    (let* ((opoint-peol (line-end-position))
           (window-width (window-width))
           (window-start (window-start))
           (window-end (window-end nil t))
           (col-eovl (col-at-eovl))
           (my-current-line-length (if (not (= opoint-peol (point-max)))
                                     (- (- window-width col-eovl) 2)
                                     (- (- window-width col-eovl) 1)))
           (pilcrow
             (propertize (char-to-string ?\u00B6)
                         'face '(:foreground "red")
                         'cursor t))
           (pilcrow-underlined
             (propertize (char-to-string ?\u00B6)
                         'face '(:foreground "RoyalBlue" :underline "blue")
                         'cursor t))
           (underline (propertize (char-to-string ?\u2009)
                                  'display `(space :width ,my-current-line-length)
                                  'face '(:underline "blue")
                                  'cursor t)))
      (setq my-eol-ruler (if (not (= opoint-peol (point-max)))
                           (concat pilcrow-underlined underline)
                           underline))
      (setq my-eol-pilcrow pilcrow)
      (overlay-put (make-overlay (line-end-position) (line-end-position)) 'after-string my-eol-ruler)
      (save-excursion
        (goto-char window-end)
        (while (re-search-backward "\n" window-start t)
          (let ((peol (line-end-position)))
            (when (not (= peol opoint-peol))
              (overlay-put (make-overlay peol peol) 'after-string my-eol-pilcrow))))))))

(add-hook 'post-command-hook 'my-eol-ruler-function)

(provide 'eol-indicator)