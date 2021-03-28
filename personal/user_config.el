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
        (find-lisp-find-files "~/Bureau/braindump" "\.org$")))
(update-org-files)
(setq org-directory "~/Bureau/braindump/")
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
  (org-roam-directory "~/Bureau/braindump/public")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))
(use-package org-roam-protocol
  :config
  )


;; Neotree (File tree)
(defun setup-neo-theme ()
  "This function updates Neotree theming when Running in GUI Emacs.
   Note : To make this function run on every frame creation, add it to :
   server-after-make-frame-hook custom variable"
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
(setq neo-autorefresh t)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-smart-open t)
(setq projectile-switch-project-action 'neotree-projectile-action)
(setup-neo-theme)


;;;;;;;;;;;;;;;;;;
;; TEXT EDITING
;;;;;;;;;;;;;;;;;;

;; Line number fix when zooming
(eval-after-load "linum"
  '(set-face-attribute 'linum nil :height 100))

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
(defun setup-firacode-font ()
  "This function aims at being added to
server-after-make-frame-functions to use Fira Code with emacs --daemon and emacsclient"
  (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol"))
(setup-firacode-font)


;; Multiple cursor configuration
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c M-h") 'mc/mark-all-in-region)
(global-set-key (kbd "ESC <mouse-1>") 'mc/add-cursor-on-click)


;;;;;;;;;;;;;;;;;;
;; PROGRAMMING
;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js-mode))
(global-set-key (kbd "C-S-k") 'fixup-whitespace)

;; CLOJURE PROGRAMMING
;;;;;;;;;;;;;;;;;;;;;;;;

;; Define a way to move a form within a list
(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  ;; when transpose-sexps can no longer transpose, it throws an error and code
  ;; below this line won't be executed. So, we don't have to worry about side
  ;; effects of backward-sexp and forward-sexp.
  (backward-sexp (1+ arg))
  (forward-sexp 2))

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
  ;; <C-right> (translated from <C-S-right>) runs paredit-forward-slurp-sexp
  (keymap-unset-key [C-right] "paredit-mode") ;; It is still bound to C-)
  ;; <C-left> (translated from <C-S-left>) runs command paredit-forward-barf-sexp
  (keymap-unset-key [C-left] "paredit-mode") ;; It is still bound to C-}
  ;; <C-M-right> runs paredit-backward-barf-sexp
  (keymap-unset-key [C-M-right] "paredit-mode") ;; It is still bound to C-{, ESC <C-right>.
  ;; <C-M-left> runs the command paredit-backward-slurp-sexp
  (keymap-unset-key [C-M-left] "paredit-mode")) ;; It is still bound to C-(, ESC <C-left>.

;; Enable clj-kondo through flycheck to lint clojure code
(require 'flycheck-clj-kondo)
;; Enable clj-refactor when editing clojure code
(require 'clj-refactor)
(defun my-clojure-mode-hook ()
  (cider-mode 1)
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  (paredit-mode 1)
  (rainbow-delimiters-mode 1)
  (aggressive-indent-mode 1)
  (setq-default cursor-type 'bar)
  (unset-paredit-C-arrows)
  ;; This choice of keybindingn leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m")

  ;;Static source code analysis without a REPL (relying on CLJ-Kondo)
  (anakondo-minor-mode 0)
  )

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
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
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/Bureau/braindump/private/random.org")))

;; DOTFILES
;;;;;;;;;;;;;;;;;;;;;;;
(use-package nix-mode
  :mode "\\.nix\\'"
  :config
  (add-hook 'nix-mode-hook 'fira-code-mode))
