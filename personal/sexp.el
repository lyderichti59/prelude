(require 'paredit)
(require 'clojure-mode)

(defun clojure-transpose-logical-sexp (arg)
  (interactive "*p")
  (let ((backward-sexp 'clojure-backward-logical-sexp)
        (forward-sexp 'clojure-forward-logical-sexp))
    (transpose-sexps arg)))

(defun transpose-pair ()
  (interactive)
  (unless (< (paredit-count-sexps-forward) 1)
    (transpose-sexps 1) ;; transpose-sexps is built-in
    (clojure-backward-logical-sexp 2)
    (transpose-sexps 1)
    (clojure-forward-logical-sexp 1)))

(defun transpose-pairs ()
  (interactive)
  (unless (< (paredit-count-sexps-forward) 2)
    (transpose-pair)
    (transpose-pair)))

;; define a way to move a form within a list
(defun reverse-transpose-sexps (&optional arg)
  (interactive "*p")
  (unless arg (setq arg 1))
  (unless (< (paredit-count-sexps-backward) (1+ arg))
    (transpose-sexps (- arg))))

(defun reverse-transpose-pair ()
  (interactive)
  (unless (< (paredit-count-sexps-backward) 3)
    (clojure-backward-logical-sexp 1)
    (reverse-transpose-sexps)
    (clojure-forward-logical-sexp 2)
    (reverse-transpose-sexps)))

(defun reverse-transpose-pairs ()
  (interactive)
  (unless (< (paredit-count-sexps-backward) 4)
    (reverse-transpose-pair)
    (reverse-transpose-pair)))

(provide 'sexp)
