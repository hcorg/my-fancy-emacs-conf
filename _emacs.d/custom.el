; Additional packages
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

; IDE layout

(tool-bar-mode nil)
(scroll-bar-mode nil)

(show-paren-mode t) ; highlight matching parentheses
(global-hl-line-mode t) ; highlight current line

(global-subword-mode)

(setq compilation-scroll-output 'first-error)

; whitespace
(setq whitespace-line-column 100)
(setq whitespace-style '(face tabs trailing
                              lines space-before-tab
                              indentation empty
                              space-after-tab
                              tab-mark))
(global-whitespace-mode)

; disable backup and auto-save files
(setq make-backup-files nil)
(setq auto-save-default nil)

; ido mode
(require 'ido)
(ido-mode)

; global keys
(global-set-key (kbd "<f12>") 'global-whitespace-mode)
(global-set-key "\M-c" 'compile)
(global-set-key "\C-x\C-p" 'save-buffers-kill-emacs)
(global-set-key "\C-x\C-z" nil)
(global-set-key "\C-x\C-c" nil)
(global-set-key "\C-co" 'ff-find-other-file)
(global-set-key (kbd "C-c C-<down>") 'windmove-down)
(global-set-key (kbd "C-c C-<up>") 'windmove-up)
(global-set-key (kbd "C-c C-<left>") 'windmove-left)
(global-set-key (kbd "C-c C-<right>") 'windmove-right)

; find-things-fast
(require 'find-things-fast)
(global-set-key '[f1] 'ftf-find-file)
(global-set-key '[f2] 'ftf-grepsource)
(global-set-key '[f4] 'ftf-gdb)

; files to mode
(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

; load other files
(load-file "~/.emacs.d/custom-colors.el")
(load-file "~/.emacs.d/custom-yasnippet.el")
(load-file "~/.emacs.d/custom-magit.el")
(load-file "~/.emacs.d/custom-c.el")
(load-file "~/.emacs.d/custom-asn1.el")

;; Ctrl+V
(defun kill-replace (start end)
  "Replaces region with top of kill-ring"
  (interactive "r")
  (delete-and-extract-region start end)
  (yank)
  )
(global-set-key "\C-v" `kill-replace)

;; Compilation
(setq compilation-process-setup-function `toggle-truncate-lines)
(defun ding1 (a b ) "ding for a second" (ding))
(setq compilation-finish-function `ding1)

;; Thrift
(require 'thrift-mode)
