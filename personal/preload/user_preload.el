;;Starting the server
;;(require 'server)
;;(unless (server-running-p)
;;  (server-start))
(require 'org-protocol)

(custom-set-faces
 '(highlight ((t (:background "#111111" :foreground nil))))
 '(lsp-face-highlight-read ((t (:inherit highlight)))))

;; Setting personal theme
(unless (package-installed-p 'dracula-theme)
  (package-install 'dracula-theme))

(setq prelude-theme 'dracula)
