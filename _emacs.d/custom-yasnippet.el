(require 'yasnippet)

(setcdr yas-snippet-dirs (cons "~/.emacs.d/snippets" (rest yas-snippet-dirs)))
(yas/initialize)

(require 'dropdown-list)
(setq yas-prompt-functions '(yas-dropdown-prompt
                             yas-ido-prompt
                             yas-completing-prompt))
