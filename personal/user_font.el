;; Installing all-the-icons fonts on the host's system
(let ((font-dest (cl-case window-system
                   (x  (concat (or (getenv "XDG_DATA_HOME")            ;; Default Linux install directories
                                   (concat (getenv "HOME") "/.local/share"))
                               "/fonts/"))
                   (mac (concat (getenv "HOME") "/Library/Fonts/" ))
                   (ns (concat (getenv "HOME") "/Library/Fonts/" )))))
  (unless (file-exists-p (concat font-dest "all-the-icons.ttf"))
    (all-the-icons-install-fonts t)))


;; Fixing old org styling bugs
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


;; Setup Fira Code


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


(provide 'user_font)
