; ------------------------------------------------------------------------------
; headers as c++
; ------------------------------------------------------------------------------

(setq include-base-dir "/usr/include/")

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist (cons include-base-dir 'c++-mode))

; ------------------------------------------------------------------------------
; auto complete
; ------------------------------------------------------------------------------
(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(define-key ac-mode-map  [(control return)] 'auto-complete)
(ac-config-default)

; ------------------------------------------------------------------------------
; auto complete headers
; ------------------------------------------------------------------------------
(defun my:ac-c-headers-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers))

(add-hook 'c++-mode-hook 'my:ac-c-headers-init)
(add-hook 'c-mode-hook 'my:ac-c-headers-init)

; ------------------------------------------------------------------------------
; auto complete clang
; ------------------------------------------------------------------------------
(require 'auto-complete-clang)
(defun my-ac-cc-mode-setup ()
  (setq ac-auto-start nil)
  (setq ac-quick-help-delay 0.5)
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

; ------------------------------------------------------------------------------
; cedet
; ------------------------------------------------------------------------------

;(load-file "~/.emacs.d/site-lisp/cedet/common/cedet.el")

;; (require 'semantic-load)
;; (require 'semanticdb-system)
;; (require 'semantic-decorate-include)
;; (require 'semantic-gcc)
;; (require 'semantic-ia)
;; (require 'ede)
;; (require 'ede-locate)

;; (semantic-gcc-setup)
;; (setq semantic-load-turn-everything-on t)
;; ;(semantic-load-enable-gaudy-code-helpers)
;; (semantic-load-enable-excessive-code-helpers)
;; (semanticdb-load-system-caches)
;; (global-ede-mode t)
;; (global-semantic-idle-completions-mode nil)

;; (semantic-add-system-include include-base-dir 'c++-mode)
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_GUI_EXPORT" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("Q_CORE_EXPORT" . ""))
;; (add-to-list 'semantic-lex-c-preprocessor-symbol-map '("_OgreExport" . ""))

;; (setq semantic-idle-scheduler-idle-time 30)

;; ; turn off decoration on private and protected members
;; (semantic-toggle-decoration-style 'semantic-decoration-on-private-members -1)
;; (semantic-toggle-decoration-style 'semantic-decoration-on-protected-members -1)

;; ; gnu global
;; (when (cedet-gnu-global-version-check t)
;;   (require 'semanticdb-global)
;;   (add-to-list 'ede-locate-setup-options 'ede-locate-global)
;;   (semanticdb-enable-gnu-global-databases 'c-mode)
;;   (semanticdb-enable-gnu-global-databases 'c++-mode))

;; ; locate
;; (when (executable-find "locate")
;;   (add-to-list 'ede-locate-setup-options 'ede-locate-locate))

;; ; key bindings
;; (defun my-cedet-hook ()
;;   ;; not used anymore, auto-complete-clang is better
;;   ;(local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
;;   (define-key c-mode-base-map "\C-c?" 'semantic-ia-complete-symbol)
;;   (define-key c-mode-base-map "\C-c>" 'semantic-complete-analyze-inline)
;;   (define-key c-mode-base-map "\C-c<" 'semantic-ia-complete-tip)
;;   (define-key c-mode-base-map "\C-cp" 'semantic-analyze-proto-impl-toggle)
;;   (define-key c-mode-base-map "\C-ci" 'semantic-ia-fast-jump)
;;   (define-key c-mode-base-map "\C-cs" 'semantic-ia-show-summary)
;;   (define-key c-mode-base-map "\C-c=" 'semantic-decoration-include-visit)
;;   (define-key c-mode-base-map "\C-ct" 'eassist-switch-h-cpp)
;;   (define-key c-mode-base-map "\C-ce" 'eassist-list-methods))
;; (add-hook 'c-mode-common-hook 'my-cedet-hook)

;; ; ------------------------------------------------------------------------------
;; ; auto complete with kdcomplete
;; ; ------------------------------------------------------------------------------

;(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete-clang-brianjcj")
;;  ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/kdcomplete")

;; (require 'auto-complete)
;; ;(add-to-list 'ac-dictionary-directories
;; ;             "~/.emacs.d/site-lisp/auto-complete/dict")

;; (require 'auto-complete-config)
;; (ac-config-default)

;; (require 'auto-complete-clang)
;; (setq ac-auto-start nil)
;; (setq ac-delay 0)
;; (setq ac-quick-help-delay 0)

;; (set-face-background 'ac-clang-candidate-face "#1f1f1f")
;; (set-face-foreground 'ac-clang-candidate-face "white")
;; (set-face-background 'ac-clang-selection-face "darkgreen")
;; (set-face-foreground 'ac-clang-selection-face "white")
;; (set-face-background 'ac-candidate-face "#1f1f1f")
;; (set-face-foreground 'ac-candidate-face "white")
;; (set-face-background 'ac-selection-face "darkgreen")
;; (set-face-foreground 'ac-selection-face "white")
;; (set-face-background 'popup-tip-face "#292929")
;; (set-face-foreground 'popup-tip-face "white")

;; (defun my-ac-config ()
;;   (setq-default ac-sources
;;                 '(ac-source-abbrev
;;                   ac-source-dictionary
;;                   ac-source-words-in-same-mode-buffers))
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))

;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources (append '(ac-source-clang
;; ;;                             ac-source-semantic
;;                              ac-source-yasnippet)
;;                            ac-sources))
;;   (define-key ac-mode-map  [(control return)] 'ac-complete-clang)
;;   (add-to-list 'ac-sources 'ac-source-c-headers)
;; )

;; (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;; (my-ac-config)

;; ff-find-other-file
(setq cc-other-file-alist
      '(("\\.cpp$" (".h" ".hpp"))
        ("\\.h$" (".cpp" ".c"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.c$" (".h"))
        ))

;; (load "kdcomplete")
;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources (append '(ac-source-kdc-member
;;                              ac-source-yasnippet)
;;                            ac-sources))
;;   (define-key ac-mode-map  [(control return)] 'ac-complete-kdc-member)
;; )

;; (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

; ------------------------------------------------------------------------------
; flymake
; ------------------------------------------------------------------------------

;; (require 'flymake)
;; (require 'flymake-cursor)

;; (defvar flymake-allowed-file-name-masks nil)
;; (defvar flymake-clang-flags nil)

;; (require 'flymake-clang-c)
;; (add-hook 'c-mode-hook 'flymake-clang-c-load)
;; (require 'flymake-clang-c++)
;; (add-hook 'c++-mode-hook 'flymake-clang-c++-load)

; ------------------------------------------------------------------------------
; misc
; ------------------------------------------------------------------------------

;; C++11 keywords
;; TODO - check if new emacs still needs those
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(constexpr\\|decltype\\|nullptr\\|static_assert\\)\\>"
                           . font-lock-keyword-face)))
