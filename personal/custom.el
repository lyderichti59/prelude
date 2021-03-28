(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(blink-cursor-mode 1)
 '(column-number-mode t)
 '(cursor-type 'bar)
 '(custom-safe-themes
   '("81c3de64d684e23455236abde277cda4b66509ef2c28f66e059aa925b8b12534" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "2dff5f0b44a9e6c8644b2159414af72261e38686072e063aa66ee98a2faecf0e" default))
 '(fira-code-mode-disabled-ligature '("[]" "#{" "#(" "#_" "#_(" "x"))
 '(fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
 '(ispell-dictionary "en")
 '(js-indent-level 2)
 '(js-jsx-syntax t)
 '(neo-window-fixed-size nil)
 '(org-babel-load-languages '((emacs-lisp . t) (shell . t)))
 '(org-capture-templates
   `(("p" "Protocol" entry
      (file+headline ,(concat org-directory "random.org")
                     "Inbox")
      "* [[%:link][%:description]]
Captured On: %U
#+BEGIN_QUOTE
%i
#+END_QUOTE

%?
")
     ("L" "Protocol Link" entry
      (file+headline ,(concat org-directory "private/" "random.org")
                     "Inbox")
      "* %? [[%:link][%:description]]
Captured On: %U")))
 '(org-directory "~/Bureau/braindump/")
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")))
 '(org-support-shift-select 'always)
 '(org-tempo-keywords-alist
   '(("L" . "latex")
     ("H" . "html")
     ("A" . "ascii")
     ("i" . "index")))
 '(package-selected-packages
   '(nix-mode rjsx-mode php-mode tldr helm-projectile helm async avy bind-key dash dash-functional gh git-commit ht inflections logito lsp-mode markdown-mode marshal multiple-cursors pcache prescient transient with-editor rust-mode org-roam visual-regexp-steroids visual-regexp all-the-icons-dired all-the-icons-ivy-rich all-the-icons neotree tagedit scss-mode anakondo docker-compose-mode aggressive-indent pretty-symbols fira-code-mode use-package ta clojure-snippets afternoon-theme dracula-theme flycheck-clj-kondo command-log-mode clj-refactor cider clojure-mode-extra-font-locking clojure-mode yaml-mode web-mode lsp-ui company-lsp json-mode rainbow-mode elisp-slime-nav rainbow-delimiters company counsel swiper ivy-prescient ivy exec-path-from-shell zop-to-char zenburn-theme which-key volatile-highlights undo-tree super-save smartrep smartparens operate-on-number nlinum move-text magit projectile imenu-anywhere hl-todo guru-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major crux browse-kill-ring anzu ag ace-window))
 '(prelude-flyspell nil)
 '(prelude-theme 'dracula-theme)
 '(safe-local-variable-values
   '((cider-shadow-watched-builds "app")
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))))
 '(server-after-make-frame-hook '(setup-neo-theme setup-firacode-font))
 '(size-indication-mode t)
 '(sp-base-key-bindings 'paredit)
 '(sp-override-key-bindings
   '(("C-<right>" . transpose-sexps)
     ("C-<left>" . reverse-transpose-sexps)))
 '(tool-bar-mode nil))
