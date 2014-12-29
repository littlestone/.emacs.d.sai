;; All roads lead to $HOME
(setq default-directory "~/")

;; Write temporary files to own directory
(progn
  (defvar --temporary-directory (concat user-emacs-directory "temps"))
  (if (not (file-exists-p --temporary-directory))
      (make-directory --temporary-directory))

  (setq temporary-file-directory (concat user-emacs-directory "temps/")
        save-place-file (expand-file-name "places" temporary-file-directory)
        savehist-file (expand-file-name "history" temporary-file-directory)
        recentf-save-file (expand-file-name "recentf" temporary-file-directory)
        abbrev-file-name (expand-file-name "abbrev_defs" temporary-file-directory)
        tramp-persistency-file-name (expand-file-name "tramp" temporary-file-directory)
        ido-save-directory-list-file (expand-file-name "ido.last" temporary-file-directory)
        auto-save-list-file-prefix "~/.emacs.d/temps/auto-save-list/.saves-"
        auto-save-file-name-transforms `((".*" ,temporary-file-directory t))))

;; Automatically save and restore sessions
(progn
  (setq desktop-dirname             temporary-file-directory
        desktop-base-file-name      "emacs.desktop"
        desktop-base-lock-name      "emacs.desktop.lock"
        desktop-path                (list desktop-dirname)
        desktop-save                t
        desktop-files-not-to-save   "^$" ;reload tramp paths
        desktop-load-locked-desktop nil)
  ;; desktop-save-mode error fix
  (setq desktop-restore-frames nil
        desktop-restore-in-current-display t
        desktop-restore-forces-onscreen nil)
  (desktop-save-mode 1))

;; Set default coding systems to UTF-8 
(progn
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  ;; Treat clipboard input as UTF-8 string first; compound text next, etc.
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
(setq backup-by-copying t) ; stop emacs's backup changing the file's creation date of the original file

;; Save place in files between sessions
(setq-default save-place t)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode +1)
(setq recentf-max-saved-items 50) ; just 20 is too recent

;; CUA rectangle support
(setq cua-enable-cua-keys nil)
(setq cua-toggle-set-mark nil) ;; original set-mark behavior, i.e. no transient-mark-mode
(cua-mode)

;; Sane defaults
(setq delete-selection-mode 1                 ; replace a region with yank buffer contents
      delete-by-moving-to-trash t             ; move deleted file to Recycle Bin
      save-interprogram-paste-before-kill t   ; preserve clipboard content in Emacs on Windows.
      apropos-do-all t                        ; apropos commands perfom more extensive searches than default.
      mouse-yank-at-point t                   ; 不要在鼠标点击的那个地方插入剪贴板内容。我不喜欢那样，经常把我的文档搞的一团糟。我觉得先用光标定位，然后鼠标中键点击要好的多。不管你的光标在文档的那个位置，或是在 minibuffer，鼠标中键一点击，X selection 的内容就被插入到那个位置。
      mode-require-final-newline nil          ; 文件末尾不要自动插入空行
      default-major-mode 'text-mode           ; 把缺省的major mode设为text-mode
      kill-ring-max 200                       ; 用一个很大的kill ring，防止不小心删掉重要的东西
      echo-keystrokes 0.1                     ; show keystrokes in progress.
      read-quoted-char-radix 10               ; use of decimal sequences instead of octal to insert a non-graphic character (16 for hexadecimal)
      gc-cons-threshold 20000000              ; don't be so stingy on the memory, we have lots now. It's the distant future.
      global-auto-revert-non-file-buffers t   ; auto refresh dired,
      dired-dwim-target t                     ; copy from one dired dir to the next dired dir shown in a split window
      global-auto-revert-mode t               ; auto refresh buffers
      auto-revert-verbose nil                 ; but be quiet about it.
      shift-select-mode nil                   ; real emacs knights don't use shift to mark things.
      eval-expression-print-length nil        ; do not truncate messages in the echo area
      )

;; 设置有用的个人信息，这在很多地方有用
(setq user-full-name "Sai")
(setq user-mail-address "razorsniper@gmail.com")

;; 不用 TAB 字符来indent, 这会引起很多奇怪的错误。编辑 Makefile 的时候也不用担心，因为 makefile-mode 会把 TAB 键设置成真正的 TAB 字符，并且加亮显示的
(setq-default indent-tabs-mode nil
              c-basic-offset 4
              tab-width 4)
(setq default-tab-width 2)
(setq tab-stop-list (number-sequence 2 200 2))

;; Auto-indent yanked (pasted) code
(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
           (and (not current-prefix-arg)
                (member major-mode '(emacs-lisp-mode lisp-mode
                                     clojure-mode    scheme-mode
                                     haskell-mode    ruby-mode
                                     rspec-mode      python-mode
                                     c-mode          c++-mode
                                     objc-mode       latex-mode
                                     plain-tex-mode))
                (let ((mark-even-if-inactive transient-mark-mode))
                  (indent-region (region-beginning) (region-end) nil))))))

;;;
;;;=============================================================================
;;;

;; Display “lambda” as “λ”
(global-prettify-symbols-mode 1)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(setq fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Save minibuffer history
(savehist-mode 1)
(setq history-length 1000)

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't break lines for me, please
(setq-default truncate-lines t)

;; Keep cursor away from edges when scrolling up/down
(require 'smooth-scrolling)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now. It's the distant future.
(setq gc-cons-threshold 20000000)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; 80 chars is a good width.
(set-default 'fill-column 80)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Nic says eval-expression-print-level needs to be set to nil (turned off) so
;; that you can always see what's happening.
(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(provide 'sane-defaults)
