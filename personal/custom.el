(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "#111111" :foreground nil))))
 '(lsp-face-highlight-read ((t (:inherit highlight)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-image-file-mode t)
 '(blink-cursor-mode 1)
 '(c-default-style
   '((java-mode . "java")
     (clojure-mode . "java")
     (awk-mode . "awk")
     (other . "gnu")))
 '(c-old-style-variable-behavior t)
 '(cider-test-show-report-on-success t)
 '(cljr-add-ns-to-blank-clj-files nil)
 '(cljr-auto-clean-ns t)
 '(cljr-auto-sort-ns nil)
 '(cljr-insert-newline-after-require t)
 '(cljr-project-clean-exceptions '("dev/user.clj" "project.clj" "boot.clj" "src/wiring.clj"))
 '(clojure-align-forms-automatically t)
 '(column-number-mode t)
 '(cursor-type 'bar)
 '(custom-safe-themes
   '("81c3de64d684e23455236abde277cda4b66509ef2c28f66e059aa925b8b12534" "57e3f215bef8784157991c4957965aa31bac935aca011b29d7d8e113a652b693" "2dff5f0b44a9e6c8644b2159414af72261e38686072e063aa66ee98a2faecf0e" default))
 '(ediff-split-window-function 'split-window-horizontally)
 '(exec-path-from-shell-variables
   '("PATH" "MANPATH" "LEIN_USERNAME" "LEIN_PASSWORD" "NIX_PATH"))
 '(fira-code-mode-disabled-ligature '("[]" "#{" "#(" "#_" "#_(" "x"))
 '(fira-code-mode-disabled-ligatures '("[]" "#{" "#(" "#_" "#_(" "x"))
 '(ispell-dictionary "en")
 '(ivy-initial-inputs-alist
   '((counsel-minor . "^+")
     (counsel-package . "^+")
     (counsel-org-capture . "^")
     (counsel-M-x . "")
     (counsel-describe-symbol . "^")
     (org-refile . "^")
     (org-agenda-refile . "^")
     (org-capture-refile . "^")
     (Man-completion-table . "^")
     (woman . "^")))
 '(ivy-magic-slash-non-match-action nil)
 '(js-indent-level 2)
 '(js-jsx-syntax t)
 '(lsp-enable-symbol-highlighting t)
 '(lsp-symbol-highlighting-skip-current nil)
 '(magit-diff-refine-hunk 'all)
 '(magit-diff-refine-ignore-whitespace nil)
 '(neo-window-fixed-size nil)
 '(org-agenda-custom-commands
   '(("w" "Agenda and URGENT, WIP, NEXT"
      ((agenda "" nil)
       (todo "URGENT" nil)
       (todo "WIP" nil)
       (todo "NEXT" nil))
      nil)
     ("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)))
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
 '(org-todo-keywords
   '((type "TOLOOK(l)" "ASYNC(a)" "URGENT(u)" "DONE(d)")
     (sequence "TODO(t)" "NEXT(n)" "WIP(w)" "DONE(d)")))
 '(org-use-fast-todo-selection 'expert)
 '(package-selected-packages
   '(ripgrep projectile-ripgrep csv helm-nixos-options company-nixos-options image+ plantuml-mode dockerfile-mode csv-mode js2-mode windresize vterm-toggle vterm centaur-tabs counsel-projectile helpful doom-modeline lsp-treemacs nix-mode rjsx-mode php-mode tldr helm-projectile helm async avy bind-key dash dash-functional gh git-commit ht inflections logito lsp-mode markdown-mode marshal multiple-cursors pcache prescient transient with-editor rust-mode org-roam visual-regexp-steroids visual-regexp all-the-icons-dired all-the-icons-ivy-rich all-the-icons neotree tagedit scss-mode anakondo docker-compose-mode aggressive-indent pretty-symbols fira-code-mode use-package ta clojure-snippets afternoon-theme dracula-theme flycheck-clj-kondo command-log-mode clj-refactor cider clojure-mode-extra-font-locking clojure-mode yaml-mode web-mode lsp-ui json-mode rainbow-mode elisp-slime-nav rainbow-delimiters company counsel swiper ivy-prescient ivy exec-path-from-shell zop-to-char zenburn-theme which-key volatile-highlights undo-tree super-save smartrep smartparens operate-on-number move-text magit projectile imenu-anywhere hl-todo guru-mode gitignore-mode gitconfig-mode git-timemachine gist flycheck expand-region epl editorconfig easy-kill diminish diff-hl discover-my-major crux browse-kill-ring anzu ag ace-window))
 '(plantuml-default-exec-mode 'jar)
 '(plantuml-jar-path "/Users/lyderic.dutillieux/bin/plantuml.jar")
 '(prelude-flyspell nil)
 '(prelude-theme 'dracula-theme)
 '(projectile-enable-caching t)
 '(projectile-globally-ignored-directories
   '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd" "node_modules" ".next"))
 '(projectile-indexing-method 'native)
 '(safe-local-variable-values
   '((cider-preferred-build-tool . shadow-cljs)
     (cider-shadow-default-options . "app")
     (cider-default-cljs-repl . shadow)
     (cider-shadow-watched-builds "app")
     (eval font-lock-add-keywords nil
           `((,(concat "("
                       (regexp-opt
                        '("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl")
                        t)
                       "\\_>")
              1 'font-lock-variable-name-face)))))
 '(size-indication-mode t)
 '(sp-base-key-bindings 'paredit)
 '(sp-override-key-bindings
   '(("C-<right>" . transpose-sexps)
     ("C-<left>" . reverse-transpose-sexps)))
 '(tool-bar-mode nil)
 '(vterm-eval-cmds
   '(("find-file" find-file)
     ("message" message)
     ("vterm-clear-scrollback" vterm-clear-scrollback)
     ("find-file-above" find-file-above)))
 '(word-wrap t))
