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
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)

(setq ac-auto-start nil)
(setq ac-delay 0)
(setq ac-quick-help-delay 0)

(set-face-background 'ac-clang-candidate-face "#1f1f1f")
(set-face-foreground 'ac-clang-candidate-face "white")
(set-face-background 'ac-clang-selection-face "darkgreen")
(set-face-foreground 'ac-clang-selection-face "white")
(set-face-background 'ac-candidate-face "#1f1f1f")
(set-face-foreground 'ac-candidate-face "white")
(set-face-background 'ac-selection-face "darkgreen")
(set-face-foreground 'ac-selection-face "white")
(set-face-background 'popup-tip-face "#292929")
(set-face-foreground 'popup-tip-face "white")

(setq-default ac-sources
                 '(ac-source-abbrev
                   ac-source-dictionary
                   ac-source-words-in-same-mode-buffers))

; ------------------------------------------------------------------------------
; ff-find-other-file
; ------------------------------------------------------------------------------
(setq cc-other-file-alist
      '(("\\.cpp$" (".h" ".hpp"))
        ("\\.h$" (".cpp" ".c"))
        ("\\.hpp$" (".cpp" ".c"))
        ("\\.c$" (".h"))
        ))

; ------------------------------------------------------------------------------
; C++11 keywords
; ------------------------------------------------------------------------------
;; TODO - check if new emacs still needs those
(font-lock-add-keywords 'c++-mode
                        '(("\\<\\(constexpr\\|decltype\\|nullptr\\|static_assert\\)\\>"
                           . font-lock-keyword-face)))
