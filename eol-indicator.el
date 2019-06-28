
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
  (let* ((opoint (point))
         (window-width (window-width))
         (window-start (window-start))
         (window-end (window-end nil t))
         (col-eovl (col-at-eovl))
         (my-current-line-length (- (- window-width col-eovl) 3))
         (pilcrow
           (propertize (char-to-string ?\u00B6)
             'face '(:foreground "red")
             'cursor t))
         (pilcrow-underlined
           (propertize (char-to-string ?\u00B6)
             'face '(:foreground "white" :underline "blue")
             'cursor t))
         (underline (propertize (char-to-string ?\u2009)
               'display `(space :width ,my-current-line-length)
               'face '(:underline "blue")
               'cursor t)))
  (when (or my-eol-ruler my-eol-pilcrow)
    (dolist (description `(,my-eol-ruler
                           ,my-eol-pilcrow ))
      (remove-overlays (point-min) (point-max) 'after-string description)))
  (setq my-eol-ruler (concat pilcrow-underlined underline))
  (setq my-eol-pilcrow pilcrow)
  (save-excursion
    (end-of-line)
    (overlay-put (make-overlay (point) (point)) 'after-string my-eol-ruler))
  (save-excursion
    (goto-char window-end)
    (while (re-search-backward "\n" window-start t)
      (let ((pbol (point-at-bol))
            (peol (point))
            (col-eovl (col-at-eovl)))
        (when
          (or
            (< opoint pbol)
            (> opoint peol))
        (overlay-put (make-overlay peol peol) 'after-string my-eol-pilcrow)))))))

(add-hook 'post-command-hook 'my-eol-ruler-function)

(provide 'eol-indicator)