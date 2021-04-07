;;Starting the server
;;(require 'server)
;;(unless (server-running-p)
;;  (server-start))
(require 'org-protocol)

;; Setting personal theme
(unless (package-installed-p 'dracula-theme)
  (package-install 'dracula-theme))
(setq prelude-theme 'dracula)
