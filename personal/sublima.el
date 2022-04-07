;; https://github.com/Parveshdhull/sublima/
;; Original SHA : 624ade6

(require 'recentf)

(setq sublima-cache-directory (concat user-emacs-directory ".cache/sublima"))

(add-to-list 'recentf-exclude (expand-file-name sublima-cache-directory))
(make-directory (expand-file-name sublima-cache-directory) t)


;; Create Sublima Scratch Buffer
(defun sublima-scratch()
  (interactive)
  (let ((scratch-file (make-temp-file (expand-file-name (concat sublima-cache-directory "/scratch-")))))
    (find-file scratch-file)))


;; Save Scratch Buffer as File
(defun sublima-save-as-buffer (filename)
  (interactive "F")
  (save-buffer)
  (write-region (point-min) (point-max) filename)
  (kill-buffer)
  (find-file filename))


;; Delete empty buffers
(defun delete-empty-scratches ()
  (eshell-command (concat "find " sublima-cache-directory " -size 0 -delete")))

(add-hook 'emacs-startup-hook #'delete-empty-scratches)
(add-hook 'kill-emacs-hook #'delete-empty-scratches)


;; Open scratch buffers on startup
(find-file (concat sublima-cache-directory "/*") t)


;; Auto Save Scratch buffers on focus lost
(defun sublima-save-all-buffers ()
  (interactive)
  (dolist (curr-buff (buffer-list))
    (with-current-buffer curr-buff
      (when (and (string-prefix-p (expand-file-name sublima-cache-directory) (buffer-file-name))
        (buffer-modified-p))
      (save-buffer))
      )))

(add-function :after after-focus-change-function 'sublima-save-all-buffers)
