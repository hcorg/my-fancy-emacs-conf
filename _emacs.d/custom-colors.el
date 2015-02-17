(add-to-list 'load-path "~/.emacs.d/site-lisp/color-theme")
(add-to-list 'load-path "~/.emacs.d/site-lisp/custom-color-themes")


(load "color-theme/themes/color-theme-library")
(require 'color-theme)
(color-theme-dark-laptop)

(set-face-background 'hl-line "gray12")
(set-face-background 'show-paren-match-face "gray24")
(setq whitespace-space '(t (:foreground "gray10")))

;(load "color-theme-tm")
;(color-theme-tm)
