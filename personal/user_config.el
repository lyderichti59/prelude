;; START WITH PACKAGE INSTALLATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Installing all-the-icons fonts on the host's system
(let ((font-dest (cl-case window-system
                   (x  (concat (or (getenv "XDG_DATA_HOME")            ;; Default Linux install directories
                                   (concat (getenv "HOME") "/.local/share"))
                               "/fonts/"))
                   (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                   (ns (concat (getenv "HOME") "/Library/Fonts/" )))))
  (unless (file-exists-p (concat font-dest "all-the-icons.ttf"))
    (all-the-icons-install-fonts t)))

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

(defface org-dont-underline-indents
  '((t :underline nil))
  "Avoid underlining of indentation.")

(defun org-search-underlined-indents (limit)
  "Match function for `org-dont-underline-indents'."
  (let (ret face)
    (while (and (setq ret (re-search-forward "^[[:space:]]+" limit t))
                (or (null (setq face (plist-get (text-properties-at (match-beginning 0)) 'face)))
                    (eq face 'org-dont-underline-indents))))
    ret))

(defun org-dont-underline-indents ()
  "Remove underlining at indents."
  (add-to-list 'org-font-lock-extra-keywords '(org-search-underlined-indents 0 'org-dont-underline-indents t) 'append))

(add-hook 'org-font-lock-set-keywords-hook #'org-dont-underline-indents 'append)

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

;; Org-roam
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (concat desktop "braindump/public"))
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
(use-package org-roam-protocol
  :config)


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


;; Fira Code Symbole font (workaround)
(require 'fira-code-mode)
(custom-set-variables
 '(fira-code-mode-disabled-ligature '("[]" "#{" "#(" "#_" "#_(" "x")))
(add-hook 'prog-mode-hook 'fira-code-mode)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; This works when using emacs --daemon + emacsclient
(defun setup-firacode-font (&optional frame)
  "This function aims at being added to
after-make-frame-functions to use Fira Code with emacs --daemon and emacsclient"
  (with-selected-frame (or frame (selected-frame))
    (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
    (when (featurep 'doom-modeline)
      (set-fontset-font t #Xe161 nil))))
(setup-firacode-font)
(add-hook 'after-make-frame-functions 'setup-firacode-font)

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


;; NixOS
;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-c C-S-n") 'helm-nixos-options)

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-nixos-options)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common))


;; CLOJURE PROGRAMMING
;;;;;;;;;;;;;;;;;;;;;;;;

;; Toggling align forms
(defun toggle-align-forms ()
  (interactive)
  (setq clojure-align-forms-automatically (not clojure-align-forms-automatically)))

;; Toggling Edition mode
(setq aggressive-editing-enabled 1)
(defun toggle-aggressivity ()
  (interactive)
  (setq aggressive-editing-enabled (not aggressive-editing-enabled))
  (setq clojure-align-forms-automatically aggressive-editing-enabled)
  (aggressive-indent-mode aggressive-editing-enabled)
  (whitespace-mode aggressive-editing-enabled)
  (toggle-truncate-lines (not aggressive-editing-enabled)))

(require 'sexp)

;; Shadowing a paredit

(defun get-key-combo (key)
  "Just return the key combo entered by the user"
  (interactive "kKey combo: ") key)

(defun keymap-unset-key (key keymap)
  "Remove binding of KEY in a keymap
   KEY is a string or vector representing a sequence of keystrokes."
  (interactive
   (list (call-interactively #'get-key-combo)
         (completing-read "Which map: " minor-mode-map-alist nil t)))
  (let ((map (cdr (assoc (intern keymap) minor-mode-map-alist))))
    (when map
      (define-key map key nil)
      (message  "%s unbound for %s" key keymap))))

(defun unset-paredit-C-arrows ()
  (interactive)
  ;; <C-right> (translated from <C-S-right>) runs paredit-forward-slurp-sexp
  (keymap-unset-key (kbd "C-<right>") "paredit-mode") ;; It is still bound to C-)
  ;; <C-left> (translated from <C-S-left>) runs command paredit-forward-barf-sexp
  (keymap-unset-key (kbd "C-<left>") "paredit-mode") ;; It is still bound to C-}
  ;; <C-M-right> runs paredit-backward-barf-sexp
  (keymap-unset-key (kbd "C-M-<right>") "paredit-mode") ;; It is still bound to C-{, ESC <C-right>.
  ;; <C-M-left> runs the command paredit-backward-slurp-sexp
  (keymap-unset-key (kbd "C-M-<left>") "paredit-mode")) ;; It is still bound to C-(, ESC <C-left>.

(defun my-clojure-mode-hook ()
  (lsp)
  (lsp-mode 1)
  (lsp-ui-mode 0)
  (paredit-mode 1)
  (unset-paredit-C-arrows)
  (rainbow-delimiters-mode 1)
  (aggressive-indent-mode 1)
  (setq-default cursor-type 'bar)


  ;; Overriding  prelude keybindings
  (define-key prelude-mode-map (kbd "C-S-<down>") 'transpose-pairs)
  (define-key prelude-mode-map (kbd "C-S-<up>") 'reverse-transpose-pairs)

  ;;Conveninent keybindings
  (define-key clojure-mode-map (kbd "C-c C-r C-r") 'lsp-clojure-add-missing-libspec)

  (setq comment-column nil
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
  )

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

;; DOTFILES
;;;;;;;;;;;;;;;;;;;;;;;
(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-mode-hook 'fira-code-mode))

;; CSV FILES
;;;;;;;;;;;;;;;;;;;;;;;;
(require 'csv-highlight)
