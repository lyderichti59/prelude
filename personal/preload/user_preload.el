;;Starting the server
;;(require 'server)
;;(unless (server-running-p)
;;  (server-start))
(require 'org-protocol)

(custom-set-faces
 '(highlight ((t (:background "#111111" :foreground nil))))
 '(lsp-face-highlight-read ((t (:inherit highlight)))))

(unless package-archive-contents
             (package-refresh-contents))

;; Setting personal theme
(unless (package-installed-p 'dracula-theme)
  (package-install 'dracula-theme))

(setq prelude-theme 'dracula)

(defun customize-dracula ()
  "Customize dracula theme"
  (when (member 'dracula custom-enabled-themes)
    (custom-theme-set-faces
     'dracula
     '(term ((t (:background nil)))))))
