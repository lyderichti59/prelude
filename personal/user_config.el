;; START WITH PACKAGE INSTALLATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Install all the packages loaded from the custom.el
(package-install-selected-packages)

;;;;;;;;;;;;;;;;;;
;; KEYBOARD
;;;;;;;;;;;;;;;;;;
(setq mac-right-option-modifier nil)

;;;;;;;;;;;;;;;;;;
;; HOST
;;;;;;;;;;;;;;;;;;
(setq desktop (if (file-exists-p "~/Bureau/") "~/Bureau/" "~/Desktop/"))

;;;;;;;;;;;;;;;;;;
;; UI
;;;;;;;;;;;;;;;;;;

(customize-dracula)

;; Beautiful modeline on the bottom (less verbose than the default one and
;; supports GUI components)
(defun setup-doom-modeline-icons (&optional frame)
  "This function updates the modeline and makes GUI Icons use icons when possible.
   Note : To make this function run on every frame creation, add it to :
   after-make-frame-functions variable"
  (with-selected-frame (or frame (selected-frame))
    (setq doom-modeline-icon (display-graphic-p))))

(use-package doom-modeline
  :ensure t
  :custom (doom-modeline-height 15)
  :init
  (doom-modeline-mode 1)
  (add-hook 'after-make-frame-functions 'setup-doom-modeline-icons))

;; Adding tabs management
(use-package centaur-tabs
  :demand
  :config
  (setq centaur-tabs-style "bar"
        centaur-tabs-set-icons t
        centaur-tabs-height 32
        centaur-tabs-set-bar 'over
        centaur-tabs-set-modified-marker t
        centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-mode t)
  (centaur-tabs-group-by-projectile-project)
  (defun centaur-tabs-buffer-groups ()
     "`centaur-tabs-buffer-groups' control buffers' group rules.
 Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
 All buffer name start with * will group to \"Emacs\".
 Other buffer group by `centaur-tabs-get-group-name' with project name."
   (list
    (cond
;; ((not (eq (file-remote-p (buffer-file-name)) nil))
;; "Remote")
    ((derived-mode-p 'prog-mode)
     "Editing")
    ((derived-mode-p 'dired-mode)
     "Dired")
    ((derived-mode-p 'vterm-mode)
     "Shells")
    ((memq major-mode '(helpful-mode
                help-mode))
     "Help")
    ((memq major-mode '(org-mode
                org-agenda-clockreport-mode
                org-src-mode
                org-agenda-mode
                org-beamer-mode
                org-indent-mode
                org-bullets-mode
                org-cdlatex-mode
                org-agenda-log-mode
                diary-mode))
     "OrgMode")
    ((or (string-equal "*" (substring (buffer-name) 0 1))
         (memq major-mode '(magit-process-mode
                            magit-status-mode
                            magit-diff-mode
                            magit-log-mode
                            magit-file-mode
                            magit-blob-mode
                            magit-blame-mode)))
     "Emacs")
    (t
     (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (dired-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))

;; Having short description beside commands when `M-x`
(use-package ivy-rich
  :init (ivy-rich-mode 1))

;; Enhances the knowledge base views
(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(add-hook 'magit-process-mode-hook 'goto-address-mode)

;;;;;;;;;;;;;;;;;;
;; NAVIGATION
;;;;;;;;;;;;;;;;;;

;; Bindings for windows : enable moving from windows to windows with S-<arrows>
(require 'windmove)
(require 'framemove)
(windmove-default-keybindings)
(setq framemove-hook-into-windmove t)

;; Org mode tuning
(load-library "find-lisp")
(defun update-org-files ()
  (interactive)
  (setq org-agenda-files
        (find-lisp-find-files (concat desktop "braindump") "\.org$")))
(update-org-files)

(setq org-directory (concat desktop "braindump"))
(setq org-replace-disputed-keys 1)
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
(add-hook 'before-save-hook 'time-stamp)
(add-hook 'org-mode-hook 'turn-on-auto-fill)

;; Sending code to IELM from Org-mode
;; Source : https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Lisp-02.org#follow-along-with-ielm
(require 'ielm)
(defun efs/ielm-send-line-or-region ()
  (interactive)
  (unless (use-region-p)
    (backward-sexp)
    (set-mark-command nil)
    (forward-sexp))
  (let ((text (buffer-substring-no-properties (region-beginning)
                                              (region-end))))
    (with-current-buffer "*ielm*"
      (insert text)
      (ielm-send-input))
    (deactivate-mark)))

(define-key org-mode-map (kbd "C-c C-e") 'efs/ielm-send-line-or-region)

;; Neotree (File tree)
(defun setup-neo-theme (&optional frame)
  "This function updates Neotree theming when Running in GUI Emacs.
   Note : To make this function run on every frame creation, add it to :
   after-make-frame-functions custom variable"
  (with-selected-frame (or frame (selected-frame))
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow))))
(setq neo-autorefresh t)
(require 'neotree)
(global-set-key [f8] 'neotree-show)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setup-neo-theme)
(add-hook 'after-make-frame-functions 'setup-neo-theme)

;;;;;;;;;;;;;;;;;;
;; TEXT EDITING
;;;;;;;;;;;;;;;;;;

(dolist (mode '(org-mode-hook
                neotree-mode-hook
                vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; YASnippet integration within hippie
(add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand)

;; Multiple cursor configuration
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c M-h") 'mc/mark-all-in-region)
(global-set-key (kbd "ESC <mouse-1>") 'mc/add-cursor-on-click)

;;;;;;;;;;;
;; SHELLS
;;;;;;;;;;;
(require 'vterm)
(require 'vterm-toggle)

(global-set-key [f1] 'vterm)
(global-set-key [C-f1] 'vterm-toggle)
(global-set-key [f2] 'vterm-toggle-cd)

(defun display-buffer-above-selected (buffer alist)
  (let ((window (cond
                 ((get-buffer-window buffer (selected-frame))
                  (get-buffer-window buffer (selected-frame)))
                 ((window-in-direction 'above)
                  (window-in-direction 'above))
                 ((window-in-direction 'left)
                  (window-in-direction 'left))
                 (t (selected-window)))))
    (window--display-buffer buffer window 'window alist
                            display-buffer-mark-dedicated)
    (select-window (get-buffer-window (buffer-name buffer)))))

(defun find-file-above (path)
  (if-let* ((buf (find-file-noselect path))
            (window (display-buffer-above-selected buf nil)))
      (select-window window)
    (message "Failed to open file: %s" path)))

;;;;;;;;;;;;;;;;;;
;; PROGRAMMING
;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js-mode))
(global-set-key (kbd "C-S-k") 'fixup-whitespace)

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))

(defun fix-ediff-size ()
  (with-selected-window (get-buffer-window "*Ediff Control Panel*")
    (setq window-size-fixed t)
    (window-resize (selected-window) (- 5 (window-total-height)) nil t)))

(add-hook 'ediff-after-setup-windows-hook 'fix-ediff-size)

(defun xml-pretty-print ()
  (interactive)
  (mark-whole-buffer)
  (sgml-pretty-print (region-beginning) (region-end)))

(add-hook 'nxml-mode-hook (lambda () (local-set-key (kbd "M-q") #'xml-pretty-print)))

(setq-default comment-column nil
              gc-cons-threshold (* 64 1024 1024)
              read-process-output-max (* 2 1024 1024)
              treemacs-space-between-root-nodes nil
              company-minimum-prefix-length 1
              lsp-idle-delay 0.500
              lsp-log-io nil
              lsp-lens-enable t
              lsp-ui-sideline-enable nil
              lsp-signature-auto-activate nil
              lsp-enable-file-watchers nil
              lsp-enable-on-type-formatting nil
              lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
                                        ;lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
              )


;; NixOS
;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-nix-mode-hook ()
  (lsp)
  (lsp-mode 1)
  (lsp-ui-mode 0)
  (fira-code-mode 1)
)

(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-mode-hook 'my-nix-mode-hook))

(global-set-key (kbd "C-c C-S-n") 'helm-nixos-options)

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-nixos-options)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common))

(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  :major-modes '(nix-mode)
                  :server-id 'nix))


;; CLOJURE PROGRAMMING
;;;;;;;;;;;;;;;;;;;;;;;;

(require 'sexp)

(defun my-clojure-mode-hook ()
  (lsp)
  (lsp-mode 1)
  (lsp-ui-mode 0)
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (setq-default cursor-type 'bar)


  ;; Overriding  prelude keybindings
  (define-key prelude-mode-map (kbd "C-S-<down>") 'transpose-pairs)
  (define-key prelude-mode-map (kbd "C-S-<up>") 'reverse-transpose-pairs)

  ;;Conveninent keybindings
  (define-key clojure-mode-map (kbd "C-c C-r C-r") 'lsp-clojure-add-missing-libspec)

  (if (and (stringp buffer-file-name)
           (or (string-match "\\.edn\\'" buffer-file-name)
               (string-match "\\api.clj\\'" buffer-file-name)))
      (progn (whitespace-turn-off)
             (toggle-truncate-lines nil))
    (aggressive-indent-mode)))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'clojurescript-mode-hook #'my-clojure-mode-hook)
(add-hook 'clojuresc-mode-hook #'my-clojure-mode-hook)
(add-hook 'cider-repl-mode-hook #'paredit-mode)
(add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)


;; WEB PROGRAMMING
;;;;;;;;;;;;;;;;;;;;;;

;; Tagedit mode for editing HTML like with Paredit
(eval-after-load 'sgml-mode
  '(progn
     (require 'tagedit)
     (tagedit-add-paredit-like-keybindings)
     (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)

;; Note taking with a shortcut
(defun open-notes ()
  (interactive)
  (find-file (concat desktop "braindump/private/random.org")))

(global-set-key [f6] 'open-notes)

;; CSV FILES
;;;;;;;;;;;;;;;;;;;;;;;;
(require 'csv-highlight)
