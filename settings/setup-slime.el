;;; ==============================================================
;;; + slime - common lisp development environment configurations +
;;; ==============================================================

(require 'slime)

;; Default common lisp implementation
(if (eq system-type 'windows-nt)
    (setq inferior-lisp-program "wx86cl64") ; 注：如果此处路径有空格，在M-x slime时会出现问题：apply: Spawning child process: invalid argument
  (setq inferior-lisp-program "ccl"))

(require 'slime-autoloads) ; 注意这里加载的是 slime-autoloads，而不是 slime，要不然C-c C-c等很多功能都没有

(slime-setup '(slime-fancy))

(provide 'setup-slime)