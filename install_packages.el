(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(package-refresh-contents)

(defvar my-fancy-conf-packages
  '(magit)
  "A list of packages to ensure are installed at launch.")

(defun fancy-packages-installed-p ()
  (every #'package-installed-p my-fancy-conf-packages))

(unless (fancy-packages-installed-p)
  ;; check for new packages (package versions)
     (message "%s" "Emacs Prelude is now refreshing its package database...")
       (package-refresh-contents)
         (message "%s" " done.")
           ;; install the missing packages
             (dolist (p my-fancy-conf-packages)
                 (when (not (package-installed-p p))
                       (package-install p))))
