(use-package notmuch
  :load-path (lambda () (shell-command-to-string "realpath -m \"$(which notmuch-emacs-mua)/../../share/emacs/site-lisp/\" | tr -d '\n'"))
  :demand
  :hook
  (notmuch-message-mode . turn-off-auto-fill))
