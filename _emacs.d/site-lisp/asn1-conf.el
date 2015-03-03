;;; asn1-conf.el --- Customization of faces for ASN.1:1997 mode

;; Copyright (C) 1997-2001  France Telecom R&D, Lannion

;; Authors:      Stephane LEVANT, Olivier DUBUISSON
;; Maintainer:   asn1@rd.francetelecom.fr
;; Informations: http://asn1.elibel.tm.fr/fr/outils/emacs/   (francais)
;;               http://asn1.elibel.tm.fr/en/tools/emacs/    (english)
;; Created:      1999
;; Keywords:     ASN.1, languages, major mode, protocole specifications, faces

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provide a confortable customization of faces (for user)
;; It is here for compatibility with old versions of ASN.1 mode.
;;
;; If asn1-use-default-font-lock-faces is not nil,
;; the ASN.1 will use its own faces.
;; This mode allow to customize them.

;;; Code:

;; TODO:
;; to use more than one buffer, make local variables

(require 'asn1-mode)
(eval-when-compile (require 'cl))
(require 'widget)
(require 'wid-edit)


;;; -----------------------------------------------------------------------
;;;			User Variables
;;; -----------------------------------------------------------------------

(defvar asn1-conf-list-of-color
  '("Black" "Red" "Blue" "ForestGreen" "Light Blue" "RosyBrown" 
    "Medium Purple" "RoyalBlue" "DarkOliveGreen" "FireBrick"
    "Pale Green" "Turquoise" "DarkSlateGray"  "DarkGoldenRod" 
    "CadetBlue" "Light Yellow" "Pink" "Pale Green" "grey40")
  "*A list of color used for the foreground and background choice")

(defvar asn1-conf-text "An example of this face ..."
  "*Text to show for example")

(defvar asn1-conf-hook nil)


;;; -----------------------------------------------------------------------
;;;			private variables
;;; -----------------------------------------------------------------------

(defvar asn1-conf-buffer "*ASN.1 Configuration*")

;; The Widgets
(defvar asn1-conf-w-face nil)
(defvar asn1-conf-w-bold nil)
(defvar asn1-conf-w-italic nil)
(defvar asn1-conf-w-underline nil)
(defvar asn1-conf-w-foreground nil)
(defvar asn1-conf-w-background nil)

(defvar asn1-conf-current-face nil)
(defvar asn1-conf-name-var nil)
(defvar asn1-conf-svg-faces nil)
(defvar asn1-conf-list-of-faces nil)
(defvar asn1-conf-list-of-default-faces nil)

(defvar asn1-conf-overlay nil
  "The overlay used for the color example")

(defvar asn1-conf-list-of-color-aux nil
  "The list of the color in the format used by the widget")

(defvar asn1-conf-keymap nil)

(if asn1-conf-keymap nil
  (setq asn1-conf-keymap (copy-keymap widget-keymap))
;  (define-key asn1-conf-keymap "\^n" 'asn1-conf-next)
;  (define-key widget-field-keymap "\^n" 'asn1-conf-next)
  (define-key asn1-conf-keymap "q" 'bury-buffer)
  (if asn1-win
      (define-key
	asn1-conf-keymap
	(asn1-mice-button-key asn1-mice-button)
      'widget-button-click)))


;;; -----------------------------------------------------------------------
;;;            Function to set/get properties (thanks to cut/paste...)
;;; -----------------------------------------------------------------------
;; very not optimum...

(defun asn1-conf-set-face-foreground (color)
  (if (and asn1-data-xemacs-p (null color))
      (set-face-foreground asn1-conf-current-face (face-foreground 'default))
    (set-face-foreground asn1-conf-current-face color))
  (setcar (cdr (assq asn1-conf-current-face asn1-conf-list-of-faces)) color))

(defun asn1-conf-set-face-background (color)
  (if (and asn1-data-xemacs-p (null color))
      (set-face-background asn1-conf-current-face (face-background 'default))
    (set-face-background asn1-conf-current-face color))
 (setcar (nthcdr 2 (assq asn1-conf-current-face asn1-conf-list-of-faces))
	 color))

(defun asn1-conf-set-face-stipple (p)
  (asn1-bold asn1-conf-current-face p)
  (setcar (nthcdr 3 (assq asn1-conf-current-face asn1-conf-list-of-faces)) p))

(defun asn1-conf-set-face-bold-p (b)
  (asn1-bold asn1-conf-current-face b)
  (setcar (nthcdr 4 (assq asn1-conf-current-face asn1-conf-list-of-faces)) b))

(defun asn1-conf-set-face-italic-p (b)
  (asn1-italic asn1-conf-current-face b)
  (setcar (nthcdr 5 (assq asn1-conf-current-face asn1-conf-list-of-faces)) b))

(defun asn1-conf-set-face-underline-p (b)
  (set-face-underline-p asn1-conf-current-face b)
  (setcar (nthcdr 6 (assq asn1-conf-current-face asn1-conf-list-of-faces)) b))

(defun asn1-conf-get-face-foreground ()
  (nth 1 (assq asn1-conf-current-face asn1-conf-list-of-faces)))

(defun asn1-conf-get-face-background ()
  (nth 2 (assq asn1-conf-current-face asn1-conf-list-of-faces)))

(defun asn1-conf-get-face-stipple ()
  (nth 3 (assq asn1-conf-current-face asn1-conf-list-of-faces)))

(defun asn1-conf-get-face-bold-p ()
  (nth 4 (assq asn1-conf-current-face asn1-conf-list-of-faces)))

(defun asn1-conf-get-face-italic-p ()
  (nth 5 (assq asn1-conf-current-face asn1-conf-list-of-faces)))

(defun asn1-conf-get-face-underline-p ()
  (nth 6 (assq asn1-conf-current-face asn1-conf-list-of-faces)))

(defun asn1-conf-get-face-doc ()
  (nth 7 (assq asn1-conf-current-face asn1-conf-list-of-faces)))

(defun asn1-conf-change-face ()
  "Function to call when the current-face is changed"
  (widget-value-set asn1-conf-w-bold (asn1-conf-get-face-bold-p))
  (widget-value-set asn1-conf-w-italic (asn1-conf-get-face-italic-p))
  (widget-value-set asn1-conf-w-underline (asn1-conf-get-face-underline-p))
  (widget-value-set asn1-conf-w-foreground (asn1-conf-get-face-foreground))
  (widget-value-set asn1-conf-w-background (asn1-conf-get-face-background))
  (overlay-put asn1-conf-overlay 'face asn1-conf-current-face)
  (widget-setup))

;(defun asn1-conf-modify-face (&rest ignore)
;  "Function called by the apply button"
;  (let* ((F (widget-value asn1-conf-w-foreground))
;	 (B (widget-value asn1-conf-w-background))
;	 (b (widget-value asn1-conf-w-bold))
;	 (i (widget-value asn1-conf-w-italic))
;	 (u (widget-value asn1-conf-w-underline))
;	 (face asn1-conf-current-face)
;	 (doc (asn1-conf-get-face-doc))
;	 (oldentry (assq face asn1-conf-list-of-faces))
;	 (l (list face F B nil b i u doc)))
;    (apply 'asn1-modify-face l)
;    (setcdr oldentry (cdr l))))


;;; -----------------------------------------------------------------------
;;;			Some useful functions
;;; -----------------------------------------------------------------------

;(defun asn1-conf-next ()
;  (interactive)
;  "nothing..."
;  )

(defun asn1-conf-erase (&rest ignore)
  "Erase current face : copy default-face to it"
  (interactive)
  (copy-face 'default asn1-conf-current-face)
  (setcdr (assq asn1-conf-current-face asn1-conf-list-of-faces)
	  (list nil nil nil nil nil nil (asn1-conf-get-face-doc)))
  (asn1-conf-change-face))

(defun asn1-conf-set-to-default (&rest ignore)
  "Restore the current face to it's default value"
  (interactive)
  (let ((elt (assq asn1-conf-current-face asn1-conf-list-of-faces)))
    (setcdr elt
	    (asn1-copy-list (cdr (assq asn1-conf-current-face
				       asn1-conf-list-of-default-faces))))
    (apply 'asn1-modify-face elt)
    (asn1-conf-change-face)))

(defun asn1-conf-set-to-svg (&rest ignore)
  "Restore the current face to it's precedent value"
  (interactive)
  (let ((elt (assq asn1-conf-current-face asn1-conf-list-of-faces)))
    (setcdr elt
	    (asn1-copy-list (cdr (assq asn1-conf-current-face
				       asn1-conf-svg-faces))))
    (apply 'asn1-modify-face elt)
    (asn1-conf-change-face)))

(defun asn1-conf-set-all-to-svg (&rest ignore)
  "Restore all faces to their precedent values"
  (interactive)
  (setcar asn1-conf-list-of-faces
	  (asn1-copy-list (car asn1-conf-svg-faces)))
  (setcdr asn1-conf-list-of-faces
	  (asn1-copy-list (cdr asn1-conf-svg-faces)))
  (asn1-modify-face-list asn1-conf-list-of-faces)
  (asn1-conf-change-face))

(defun asn1-conf-set-all-to-default (&rest ignore)
  "Restore all faces to their default values"
  (interactive)
  (setcar asn1-conf-list-of-faces
	  (asn1-copy-list (car asn1-conf-list-of-default-faces)))
  (setcdr asn1-conf-list-of-faces
	  (asn1-copy-list (cdr asn1-conf-list-of-default-faces)))
  (asn1-modify-face-list asn1-conf-list-of-faces)
  (asn1-conf-change-face))


;;; -----------------------------------------------------------------------
;;; 			The major function
;;; -----------------------------------------------------------------------

(defun asn1-conf-face (title list-face &optional default-list)
  "create a buffer for customize a list of faces.
To know the format of the 'LIST-FACE, see `asn1-defface-list'.
'DEFAULT-LIST is used to restore default value."
  (switch-to-buffer asn1-conf-buffer)
  (kill-all-local-variables)
  
  (let ((inhibit-read-only t))
    (erase-buffer))
  (widget-insert title)
  (widget-insert "\n\n")

  (setq asn1-conf-list-of-color-aux
	(append
	 '((item :tag "nothing" :value nil))
	 '((editable-field :menu-tag "custom"))
	 (mapcar (lambda (elt) `(choice-item ,elt))
		 asn1-conf-list-of-color)))

  (setq asn1-conf-name-var list-face)
  (setq list-face (eval list-face))
  (setq default-list (eval default-list))
  (setq asn1-conf-list-of-faces list-face)
  (setq asn1-conf-current-face (car (car list-face)))
  (setq asn1-conf-list-of-default-faces default-list)
  (setq asn1-conf-svg-faces (asn1-copy-list list-face))
  
  (let ((beg (point))
	(l list-face))
    (setq asn1-conf-w-face
	  (apply 'widget-create
		 'radio-button-choice
		 :value asn1-conf-current-face
		 :notify (lambda (widget &rest ignore)
			   (setq asn1-conf-current-face (widget-value widget))
			   (asn1-conf-change-face))
		 (mapcar (lambda (list)
			   `(item :tag "" :value ,(car list)))
			 list-face)))
    (goto-char beg)
    (while l
      (end-of-line)
      (setq beg (point))
      (widget-insert (nth 7 (car l)))
      (overlay-put (make-overlay beg (point)) 'face (car (car l)))
      (next-line 1)
      (setq l (cdr l)))
    )
  
  (widget-insert "\n")
  (let ((deb (point)))
    (widget-insert asn1-conf-text)
    (setq asn1-conf-overlay (make-overlay deb (point))))
  (widget-insert "\n\n")
  
  (setq asn1-conf-w-foreground 
	(apply 'widget-create
	       'menu-choice
	       :tag "Foreground"
	       :notify (lambda (widget &rest ignore)
			 (condition-case ()
			     (asn1-conf-set-face-foreground
			      (widget-value widget))
			   (error nil))
			 )
	       asn1-conf-list-of-color-aux))
  (setq asn1-conf-w-background
	(apply 'widget-create
	       'menu-choice
	       :tag "Background"
	       :notify (lambda (widget &rest ignore)
			 (condition-case ()
			     (asn1-conf-set-face-background
			      (widget-value widget))
			   (error nil)))
	       asn1-conf-list-of-color-aux))
  (setq asn1-conf-w-bold
	(widget-create 'checkbox
		       :notify (lambda (widget &rest ignore)
				 (asn1-conf-set-face-bold-p
				  (widget-value widget)))))
  (widget-insert " Bold\n")
  (setq asn1-conf-w-italic
	(widget-create 'checkbox
		       :notify (lambda (widget &rest ignore)
				 (asn1-conf-set-face-italic-p
				  (widget-value widget)))))
  (widget-insert " Italic\n")
  (setq asn1-conf-w-underline
	(widget-create 'checkbox
		       :notify (lambda (widget &rest ignore)
				 (asn1-conf-set-face-underline-p
				  (widget-value widget)))))
  (widget-insert " Underline\n\n")
    
  (widget-create 'push-button
 		 :notify 'asn1-conf-erase
 		 "Erase Face")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify 'asn1-conf-set-to-svg
		 "Set to precedent value")
  (widget-insert "        ")  
  (widget-create 'push-button
		 :notify 'asn1-conf-set-all-to-svg
		 "Set all faces to precedent value")
  
  (cond (default-list
	  (widget-insert "\n")
	  (widget-create 'push-button
			 :notify 'asn1-conf-set-to-default
			 "Set to default")
	  (widget-insert "                                     ")
	  (widget-create 'push-button
			 :notify 'asn1-conf-set-all-to-default
			 "Set all faces to default")))
  (widget-insert "\n\n")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore) (bury-buffer))
		 "Quit")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify (lambda (&rest ignore)
			   (if (not (fboundp 'customize-save-variable))
			       (load "cus-edit"))
			   (customize-save-variable
			    asn1-conf-name-var
			    asn1-conf-list-of-faces))
		     "Save")
  (widget-insert "\n
Click on \"Background\" or \"Foreground\" with the button-2
show a popup menu with colors.
It's also possible to type the name of the color directly.
Then, the color can be specified in RGB. e.g.: #FFFFFF for white.
A big list of colors can be obtained with M-x list-colors-display
")
  (use-local-map asn1-conf-keymap)
  (asn1-conf-change-face)
  (setq mode-name "ASN.1-Config")
  (setq major-mode 'asn1-conf)
  (widget-setup)
  (goto-char (point-min))
  (widget-forward 1)
  (run-hooks asn1-conf-hook))
