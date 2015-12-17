;;; asn1-mode.el --- ASN.1:1997 major mode for GNU Emacs and XEmacs

;; Copyright (C) 1997-2001  France Telecom R&D, Lannion

;; Authors:      Guillaume LATU, Stephane LEVANT, Olivier DUBUISSON
;; Contributors: Francois-Xavier KOWALSKI
;; Maintainer:   asn1@rd.francetelecom.fr
;; Informations: http://asn1.elibel.tm.fr/fr/outils/emacs/   (francais)
;;               http://asn1.elibel.tm.fr/en/tools/emacs/    (english)
;; Created:      1997
;; Keywords:     ASN.1, languages, major mode, protocole specifications

(defconst asn1-data-version "2.7")
(defconst asn1-data-diff t)

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

;; This is a major mode for editing ASN.1 specification.
;; This mode will work with all versions of Emacs and XEmacs > 20.
;; If you have (X)Emacs 19, you are to download the custom package.
;;
;;
;; Install: add this lines to your ~/.emacs:
;;
;;   (setq load-path (append load-path '("<directory>")))
;;   (setq auto-mode-alist
;;          (cons '("\\.[Aa][Ss][Nn][1]?$" . asn1-mode) auto-mode-alist))
;;   (autoload 'asn1-mode "asn1-mode.el"
;;      "Major mode for editing ASN.1 specifications." t)
;;
;; <directory> is the directory where the file `asn1-mode.el' is.
;;
;; See the manual for more informations.
;; In ASN.1 mode, tape C-c C-i.
;; All shorcut are given by C-h m.
;;
;; The last version of this mode is avaible here:
;;               http://asn1.elibel.tm.fr/fr/outils/emacs/   (francais)
;;               http://asn1.elibel.tm.fr/en/tools/emacs/    (english)
;;
;; See the CHANGES file for changes since the previous version

;;; Code:

;; This mode is currently avaible in English and in French.
;;; for changing langage
(custom-initialize-default
  'asn1-language
  '(if (or (equal (getenv "LANG") "fr")
	 (string-match "^fr.*" (or (getenv "LC_ALL") "")))
	 'fr 'en))

(defun asn1-lg (fr en)
  (if (eq asn1-language 'fr)
      fr
    en))

; (defmacro defun-lg (fct args docan docfr &rest body)
;   "Like defun but with two doc string : one english and one french"
;   `(defun ,fct ,args ,(asn1-lg docfr docan) ,@body))

; (defmacro defvar-lg (var value docan docfr)
;   "Like defvar but with two doc string : one english and one french"
;   `(defun ,var ,value ,(asn1-lg docfr docan)))

; (defmacro defcustom-lg (var value docan docfr &rest args)
;   "Like defcustom but with two doc string : one english and one french"
;   `(defun ,var ,value ,(asn1-lg docfr docan) ,@args))


;;;---------------------------------------------------------------------
;;;			Custom groups
;;;---------------------------------------------------------------------

(defgroup asn1 nil
  (asn1-lg
      "Mode pour l'edition des specifications ASN.1:1997"
    "Mode for editing ASN.1:1997 specifications")
  :prefix "asn1-"
  :group 'languages)

(defgroup asn1-options nil
  (asn1-lg
      "Les principales options du mode ASN.1"
    "The ASN.1 mode options")
  :prefix "asn1-"
  :group 'asn1)

(defgroup asn1-indentation nil
  (asn1-lg
      "Modifie l'indentation effectuee par le mode ASN.1"
    "Modify the indentation of asn1 mode")
  :prefix "asn1-data-"
  :group 'asn1)

(defgroup asn1-face nil
  (asn1-lg
      "Couleurs et styles d'ecriture du mode ASN.1.
Modifier la valeur de `asn1-use-standard-custom' pour utiliser ce groupe."
    "Color and faces of ASN.1 mode
Modify the value of `asn1-use-standard-custom' for using this group.")
  :prefix "asn1-"
  :group 'asn1)

(defcustom asn1-language 'en
  (asn1-lg
      "*Specifie le langage utilise pour les menus et les messages.
Deux langages sont disponibles :
'fr : francais
'en : anglais
Une modification de cette variable ne prendra reellement effet
qu'apres avoir relance emacs."
    "*Specify the language used for menus and messages.
Two languages are available :
'fr : french
'en : english
Modify this variable take effect only after restart emacs.")
  :type '(choice (const :tag "francais" fr)
		 (const :tag "english" en))
  :group 'asn1-options)


(defun asn1-presentation ()
  (interactive)
  (let ((b (get-buffer-create "*ASN.1:1997*")))
    (switch-to-buffer b)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (format "
----------------------------------------------------------------------------
   _/_/_/_/              ====> ASN.1:1997 Mode Emacs <====
  _/_/_/_/
 _/_/_/_/    (c) France Telecom R&D - LAN/DTL/MSV     [version %s]
----------------------------------------------------------------------------

 ASN.1:1997 mode for GNU Emacs and XEmacs,
 Major mode for editing%sASN.1:1997 specifications.

 Informations: http://asn1.elibel.tm.fr/fr/outils/emacs   (francais)
               http://asn1.elibel.tm.fr/en/tools/emacs    (english)
 Authors:      Guillaume LATU, Stephane LEVANT, Olivier DUBUISSON
 Contact:      asn1@rd.francetelecom.fr

 This program is free software; you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation; either version 2, or (at your option)
 any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program; see the file COPYING.  If not, write to the
 Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.
" asn1-data-version (if asn1-data-diff " and comparing " " ")))
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (sit-for 0)))


;; easymenu is used.
;; This module is compatible Emacs and XEmacs.
;; To add a menu with Emacs :
;; use easy-menu-define and modify the `current-local-map'
;; To add a menu with XEmacs : use easy-menu-add
;; To remove a menu with Emacs : modify the `current-local-map'
;; To remove a menu with XEmacs : easy-menu-remove
(require 'easymenu)

;; ASN.1 Syntax
(autoload 'asn1-load-syntax-menu "asn1-syntax")

(if (not (fboundp 'customize-save-variable)) ; compatibility emacs 19
    (autoload 'customize-save-variable "cus-edit"))

(cond (asn1-data-diff
       (autoload 'asn1-diff-compile "asn1-diff" "" t)
       (autoload 'asn1-diff-add-menu "asn1-diff" "" t)
       (autoload 'asn1-diff "asn1-diff" "" t)))

(autoload 'asn1-conf-face "asn1-conf")

(defvar asn1-win
  (not (or (not window-system) (eq window-system 'x)))
  "t if the Operating System is windows.")

(defvar asn1-data-xemacs-p
  (string-match "XEmacs\\|Lucid"  emacs-version)
  "t with Xemacs or Lucid Emacs, nil otherwise.") 



;;;---------------------------------------------------------------------
;;;		User Variables
;;;---------------------------------------------------------------------

;;; Compilation

(defcustom asn1-compile-command
  (if (and asn1-win load-file-name)
      (if (string-match "\\(.*\\)emacs-[.0-9]+/site-lisp/asn1-mode"
			load-file-name)
	  (concat (match-string 1 load-file-name) "asnp/asnp")
	"asnp")
    "asnp")
  (asn1-lg
   "*Commande lancee lors de la compilation
Pour asnp, il est impératif de donner le chemin complet.
Les commandes connues sont asnp et oss.
Si le nom de la commande contient le texte oss, l'option
\"-messageFormat emacs\" est passée à oss."
   "*Command to use when compiling
For asnp, you must give the complete path.
The known commands are oss and ansp.
If the name of the command matches oss,
the \"-messageFormat emacs\" option is passed to oss.")
  :type 'string
  :group 'asn1-options)

;; sun
;(defcustom asn1-compile-command-type 'oss 'asnp

(defcustom asn1-compile-switches
  (asn1-lg "-fr" "")
  (asn1-lg
   "*Parametres de la commande de compilation."
   "*Options to pass to the compile command.")
  :type 'string
  :group 'asn1-options)

(if asn1-data-diff
    (defcustom asn1-compile-like-diff t
      (asn1-lg
       "*Effectue la compilation comme une comparaison."
       "*Do comparilation like comparison.")
      :type 'boolean
      :group 'asn1-options)
  (defvar asn1-compile-like-diff nil))

;;; Indentation

(defcustom asn1-data-basic-offset		2
  (asn1-lg
   "*Valeur elementaire d'indentation du mode ASN.1. Emacs doit etre
redemarre pour prendre cette valeur en compte."
   "*Basic indentation value of ASN.1 mode.
Restart Emacs to take a new value into account")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-bracket-indent-text	(* 1 asn1-data-basic-offset)
  (asn1-lg
   "*Indique l'indentation utilisee a l'interieur des blocs delimites
par ( ) ou [ ] ou { }."
   "*Indentation used in the blocs (), [] and {}.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-DEFINITIONS	(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du mot-cle DEFINITIONS par rapport a la marge gauche."
   "*Indentation of the keyword DEFINITIONS compared to the left stroke.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-in-DEFINITION-bloc	(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation de l'entete du module apres le mot-cle DEFINITIONS"
   "*Indentation in a bloc beginning by DEFINITIONS.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-BEGIN		(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du mot-cle BEGIN par rapport au mot-cle DEFINITIONS qui
precede."
   "*Indentation of the keyword BEGIN compared to the keyword DEFINITIONS
which precedes.")
  :type 'integer
  :group 'asn1-indentation) 

(defcustom asn1-data-indent-in-BEGIN-bloc	(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation des definitions dans le corps d'un module."
   "*Indentation in a bloc beginning by BEGIN.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-END		(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du mot-cle END par rapport au mot-cle BEGIN qui
precede."
   "*Indentation of keyword END compared to the keyword BEGIN
which precedes.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-IMPORTS		(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du mot-cle IMPORTS par rapport au mot-cle BEGIN qui
precede."
   "*Indentation of keyword IMPORTS compared to the keyword BEGIN which
precedes.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-in-IMPORTS-bloc	(* 2 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation dans un bloc qui commence par IMPORTS."
   "*Indentation in a bloc beginning by IMPORTS.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-EXPORTS		(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du mot-cle EXPORTS par rapport au mot-cle BEGIN qui
precede." 
   "*Indentation of keyword EXPORTS compared to the keyword BEGIN which
precedes.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-in-EXPORTS-bloc	(* 2 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation dans un bloc qui commence par EXPORTS."
   "*Indentation in a bloc beginning by EXPORTS.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-FROM		(* 2 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du mot-cle FROM par rapport au mot-cle 
IMPORTS qui precede."
   "*Indentation of keyword FROM compared to the keyword IMPORTS which
precedes.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-\;		(* 2 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du symbole ; par rapport au mot-cle EXPORTS ou
IMPORTS qui precede."
   "*Indentation of symbol ; compared to the keyword EXPORTS or IMPORTS
which precedes.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-\{		(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du symbole { par rapport a l'indentation de la ligne
precedente."
   "*Indentation of symbol { compared to the precedent line indentation.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-\(		(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du symbole ( par rapport a l'indentation de la ligne
precedente."
   "*Indentation of symbol ( compared to the precedent line indentation.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-\[		(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du symbole [ par rapport a l'indentation de la ligne
precedente."
   "*Indentation of symbol [ compared to the precedent line indentation.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-of-::=		(* 0 asn1-data-basic-offset)
  (asn1-lg
   "*Indentation du symbole ::= par rapport a l'indentation de la ligne
precedente."
   "*Indentation of symbol ::= compared to the precedent line indentation.")
  :type 'integer
  :group 'asn1-indentation)

(defcustom asn1-data-indent-WITH-SYNTAX		nil
  (asn1-lg
   "*Si t, WITH SYNTAX est aligne avec CLASS,
sinon WITH SYNTAX n'est pas indente."
   "*If t, WITH SYMBOL is align with CLASS
otherwise, WITH SYNTAX is not indented.")
  :type 'boolean
  :group 'asn1-indentation)

;;; menus

(defcustom asn1-syntax-menu-autoload
  nil
  (asn1-lg
   "*t si le menu de syntaxe \"ASN.1-Syntax\" doit se charger
automatiquement au lancement du mode Emacs."
   "*If t, the \"ASN.1-Syntax\" menu is loaded at startup.")
  :type 'boolean
  :group 'asn1-options)

(defcustom asn1-mice-button
  (if asn1-win 3 2)
  (asn1-lg
   "*Bouton de la souris à utiliser au lieu du bouton 2."
   "*Mice button used to replace the button 2.")
  :type '(choice (item 1)
		 (item 2)
		 (item 3))
  :group 'asn1-options)

;;; Color

(defcustom asn1-use-standard-custom nil
  (asn1-lg
      "*Si t, la personnalisation des couleurs et des styles d'ecriture
se fait en utilisant un buffer \"customize\".
Une modification de cette variable ne prendra reellement effet
qu'apres avoir relance emacs."
    "*If t, the customization colors/faces will made with a standard
customize buffer.
Modify this variable take effect only after restart emacs.")
  :type 'boolean
  :group 'asn1-options)

(defcustom asn1-use-default-font-lock-faces nil
  (asn1-lg
   "*Si t, aucune personnalisation des couleurs et des polices n'est
possible: les couleurs configurees par font-lock sont utilisees.
Cette variable n'est re-prise en compte qu'apres un re-demarrage d'Emacs"
   "*If t, no user customization can take place from within the ASN.1
buffer: default font-lock faces are used instead.
Modify this variable take effect only after (X)Emacs restart.")
  :type 'boolean
  :group 'asn1-options)

(defconst asn1-data-faces-default
  `((asn1-skeleton-face		"dodger blue"		nil nil t	nil	nil
	,(asn1-lg "Mots-cles participant a la structure d'un module"
	   "module skeleton"))
    (asn1-type-face		"cadetblue"	nil nil t	nil	nil
	,(asn1-lg "Mots-cles designant des types" "basic types"))
    (asn1-structured-type-face	"darkgoldenrod"	nil nil t	nil	nil
	,(asn1-lg "Mots-cles concernant les types structures"
	   "structured types"))
    (asn1-option-face		"grey40"	nil nil t	nil	nil
	,(asn1-lg "Mots-cles OPTIONAL/DEFAULT" "OPTIONAL/DEFAULT"))
    (asn1-subtyping-face	"forestgreen"	nil nil t	nil	nil
	,(asn1-lg "Mots-cles ayant trait au sous-typage"
	   "subtyping keywords"))
    (asn1-exps-face		"forestgreen"	nil nil t	nil	nil
	,(asn1-lg "Expressions de sous-typage" "subtyping exps"))
    (asn1-tag-face		"grey40"	nil nil t	nil	nil
	,(asn1-lg "Mots-cles et expressions concernant les tags" "tags"))
    (asn1-values-face		"MediumAquamarine" nil nil t	nil	nil
	,(asn1-lg "Mots-cles designant des valeurs" "values"))
;     (font-lock-string-face	"grey40"	nil nil nil	nil	nil
; 	,(asn1-lg "Chaines de caracteres" "Strings"))
    (asn1-class-face		"medium purple"	nil nil t	nil	nil
	,(asn1-lg "Mots-cles concernant les classes d'objets informationnels"
	  "class keywords"))
    (asn1-with-syntax-face	"firebrick"	nil nil nil	nil	nil
	,(asn1-lg "Expression qui suit le WITH SYNTAX entre accolades"
	   "with syntax"))
;     (font-lock-comment-face	"firebrick"	nil nil nil	t	nil
; 	,(asn1-lg "Commentaires" "Comments"))
    )
  "List of faces ASN.1 use by default.
The format of an element of this list is the same than for the function
`modify-face' of emacs 20.3 (except for the DOC) :
FACE FOREGROUND BACKGROUND STIPPLE BOLD ITALIC UNDERLINE DOC

See also `asn1-data-faces' and `asn1-modify-face'.")

(defun asn1-copy-list (list1)
  "Copy recursivly a list (copy all its elements and all the elements
of the elements...) so when the list is modified with setcar or setcdr, this
copy is not modified."
  (if (atom list1)
      list1
    (mapcar
     'asn1-copy-list
     list1)))

(defvar asn1-data-faces (if (not asn1-use-default-font-lock-faces)
			    (asn1-copy-list asn1-data-faces-default)
			  nil)
	"The faces used by ASN.1.
See `asn1-data-faces-default'
This list can be incomplete (i.e. not definning all ASN.1 mode faces)")


;;;---------------------------------------------------------------------
;;;		Variables used by ASN.1 mode
;;;---------------------------------------------------------------------

(defvar asn1-tag-file "*.asn")

(defvar asn1-data-dir (if load-file-name
			  (file-name-directory load-file-name)
			""))

(defvar asn1-data-syntax-menu-loaded
  nil
  "t if the syntax menu is in the MenuBar, nil othewise.")

(defvar asn1-data-diff-menu-p nil
  "Local variable. t if diff menu is present")

(defvar asn1-mode-hook nil
  "Hook run after loading asn1-mode")

(defvar asn1-data-font-lock-keywords nil
  "List of keywords used for colour (see `font-lock-keywords').")

(defvar asn1-compile-precedent-command nil)

(defvar asn1-compile-line nil)

(defvar asn1-data-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-c\C-r"  'indent-region)
    (define-key map "\M-j"	'indent-new-comment-line)
    (define-key map "\C-c\C-b"  'asn1-indent-buffer)
    (if asn1-data-diff
	(define-key map "\C-c\C-d"  'asn1-diff))
    (define-key map "\M-i"	'tab-to-tab-stop)
    (define-key map "\M-;"	'indent-for-comment)
    (define-key map "\^j"	'newline)
    (define-key map "\r"	'asn1-reindent-then-newline-and-indent)
    (define-key map "\M-\t"     'asn1-completion)
    (define-key map "\C-c\C-f"  'asn1-create-tag-file)
    (define-key map "\C-c\C-t"  'asn1-tutorial)
    (define-key map "\C-c\C-k"  'asn1-comment-region)
    (define-key map "\C-c\C-i"  'asn1-info)
    (define-key map "\C-c\C-h"  'asn1-info)
    (define-key map "\C-c\C-y"  'asn1-load-syntax-menu)
    (define-key map "\C-c\C-c"  'asn1-compile)
    (define-key map "\C-c\C-p"  'asn1-set-compile-command)
    (define-key map "\C-c\C-o"  'asn1-set-compile-switches)
    map)
  "Shortcuts used by the ASN.1 mode.")

(defvar asn1-data-menu nil
  "The menu of the ASN.1 mode.")

(setq asn1-data-menu
  (append
   (list
   "ASN.1"
   ;; the format of one menu item :
   ;; [<name of the item>   <function to call>  <status of the item>]
   ;;   string    		function		t or nil
   (vector (asn1-lg "Indenter la Ligne" "Indent Line")
	   'indent-for-tab-command t)
   (vector (asn1-lg "Indenter la Region" "Indent Region")
	   'indent-region '(asn1-mark))	
   (vector (asn1-lg "Indenter Tout" "Indent Buffer")
	   'asn1-indent-buffer t)
   (vector (asn1-lg "Indenter un Commentaire" "Indent Comment")
	   'indent-for-comment t)
   (vector (asn1-lg "Inserer une Tabulation" "Tab-to-tab-stop")
	   'tab-to-tab-stop t)
   ["-" (lambda nil nil) nil]
   (vector (asn1-lg "Commenter une Region" "Comment Region")
           'asn1-comment-region '(asn1-mark))
   (vector (asn1-lg "Decommenter une Region" "Uncomment Region")
           'asn1-uncomment-region '(asn1-mark))
   ["--------" (lambda nil nil) nil]

   (vector (asn1-lg "Completion des Mots Cles" "Keywords Completion")
	   'asn1-completion t)
   (vector (asn1-lg "Completion des References" "References Completion")
	   'dabbrev-expand t)
   (list
    (asn1-lg "References" "References")
    (vector (asn1-lg "Creer le Fichier de References..."
		     "Create the References File...")
	    'asn1-create-tag-file t)
    (vector (asn1-lg "Trouver une Definition..." "Find Definition...")
	    'find-tag t)
    (vector (asn1-lg "Chercher une reference..." "Search reference...")
	    'tags-search t)
    (vector (asn1-lg "Chercher et remplacer..." "Search and replace...")
	    'tags-query-replace t)
    (vector (asn1-lg "Continuer la Recherche" "Repeat Search")
	    'tags-loop-continue t)
    (vector (asn1-lg "Aller au Module Precedent"
		     "Goto Previous Module")
	    'backward-page t)
    (vector (asn1-lg "Aller au Module Suivant"
		     "Goto Next Module")
	    'forward-page t)
    (vector (asn1-lg "Aller a la Definition Precedente"
		     "Goto Previous Definition")
	    'backward-paragraph t)
    (vector (asn1-lg "Aller a la Definition Suivante"
		     "Goto Next Definition")
	    'forward-paragraph t))
   ["--" 'ignore nil]

   (vector (asn1-lg "Compiler..." "Compile...")
	   'asn1-compile t)
   )

   (if asn1-data-diff
       (list
	(vector (asn1-lg "Comparer Deux Specifications..."
		  "Compare Specifications...")
		'asn1-diff t)
	(vector (asn1-lg "Afficher le Buffer Diff"
			 "Switch to Diff Buffer")
		'asn1-goto-diff-buffer '(and (featurep 'asn1-diff)
					     (buffer-live-p
					      asn1-diff-buffer))))
     '())
   
   (list
    ["---" (lambda nil nil) nil]
   
   (vector (asn1-lg "Construire le Menu Syntaxique"
		    "Build Syntax Menu")
	   'asn1-load-syntax-menu '(not asn1-data-syntax-menu-loaded))
   (vector (asn1-lg "Couleurs On/Off" "Turn On/Off Colors")
 	   'font-lock-mode t)

   (list
    (asn1-lg "Personnalisation Rapide" "Fast Customization")
    (list
     (asn1-lg "Langage" "Language")
     (vector (asn1-lg "Francais" "French")
	     '(asn1-change-language 'fr) t)
     (vector (asn1-lg "Anglais" "English")	      
	     '(asn1-change-language 'an) t)
      ["----" (lambda nil nil) nil]
     (vector (asn1-lg "Enregistrer" "Save")
	     '(customize-save-variable 'asn1-language asn1-language) t))
    (if asn1-data-diff
	(vector (asn1-lg "Afficher le Menu de Comparaison"
			 "Show Diff Menu")
		'(progn
		   (require 'asn1-diff)
		   (use-local-map asn1-diff-mode-asn1-map)
		   (asn1-menu-add-menu (current-local-map))
		   (asn1-diff-add-menu (current-local-map))
		   (redraw-display))
		'(not asn1-data-diff-menu-p))
      ["-----" (lambda nil nil) nil])
    (if asn1-data-diff
	(vector (asn1-lg "Cacher le Menu de Comparaison"
			 "Hide Diff Menu")
		'(progn
		   (use-local-map asn1-data-mode-map)
		   (easy-menu-remove asn1-diff-menu)
		   (setq asn1-data-diff-menu-p nil)
		   (redraw-display))
		'asn1-data-diff-menu-p)
      ["------" (lambda nil nil) nil])
    (vector (asn1-lg "Menu Syntaxique au Demarrage"
		     "Build Syntax Menu at Startup")
	    '(customize-save-variable 'asn1-syntax-menu-autoload t)
	    '(not asn1-syntax-menu-autoload))
    (vector (asn1-lg "Pas de Menu Syntaxique au Demarrage"
		     "Don't Build Syntax Menu at Startup")
	    '(customize-save-variable 'asn1-syntax-menu-autoload nil)
	    'asn1-syntax-menu-autoload)
    (vector (asn1-lg "Commande de Compilation"
		     "Compile Command")
	    'asn1-set-compile-command t)
    (vector (asn1-lg "Parametres de Compilation" "Compile Switches")
	    'asn1-set-compile-switches t)
    (vector (asn1-lg
		"Sauvegarder les Informations de Compilation"
		"Save Compilation Informations")
	    '(progn
	       (customize-save-variable 'asn1-compile-command
					asn1-compile-command)
	       (customize-save-variable 'asn1-compile-switches
					asn1-compile-switches)) t))
   (list
    (asn1-lg "Personnalisation" "Customization")
    (vector (asn1-lg "Styles d'Ecriture et Couleurs..."
		     "Colors/Faces...")
	    '(if asn1-use-default-font-lock-faces
		 (edit-faces)			  ;default faces editor
	       (asn1-custom-face)) t)		  ;asn1-mode faces editor
    (vector (asn1-lg "Options Generales..."
		     "General Options...")
	    'asn1-custom-options t)
    (vector (asn1-lg "Indentation..." "Indentation...")
	    'asn1-custom-indentation t))
   ["-------" (lambda nil nil) nil]
   (vector (asn1-lg "Tutorial Emacs"
		    "Emacs Tutorial") 'asn1-tutorial t)
    (vector (asn1-lg "Aide" "Help") 'asn1-info t)
    (vector (asn1-lg "A propos" "About") 'asn1-presentation t)
   ))
  )


(defvar asn1-data-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Modification of this table :
    ;;   1) See the "Emacs lisp" documentation before
    ;;   2) Don't forget to put exactly two char before the
    ;;      "1234" which indicate the start (12) et the end (34) of comments.

    (cond (asn1-data-xemacs-p
	   (modify-syntax-entry ?*  ". 23" table)
	   (modify-syntax-entry ?/  ". 14b" table)
	   (modify-syntax-entry ?-  "ww5678b" table)
	   (modify-syntax-entry ?\n "> b"     table))
	  ((not asn1-data-xemacs-p)
	   (modify-syntax-entry ?*  ". 23 b" table)
	   (modify-syntax-entry ?/  ". 14 b" table)
	   (modify-syntax-entry ?-  "ww1234" table)
	   (modify-syntax-entry ?\n ">"     table)
;; 	   (modify-syntax-entry ?*  ". 23 n" table)
;; 	   (modify-syntax-entry ?/  ". 14 n" table)
;; 	   (modify-syntax-entry ?-  "ww1234" table)
;; 	   (modify-syntax-entry ?\n "> b"     table)
	   ))

    (modify-syntax-entry ?\t " "      table)
    (modify-syntax-entry ?   " "      table)
    (modify-syntax-entry ?\" "\""     table)
    (modify-syntax-entry ?\' "\""     table)
    (modify-syntax-entry ?.  "."      table)
    (modify-syntax-entry ?\; "."      table)
    (modify-syntax-entry ?:  "."      table)
    (modify-syntax-entry ?=  "."      table)
    (modify-syntax-entry ?\[ "(]"     table)
    (modify-syntax-entry ?\] ")["     table)
    (modify-syntax-entry ?\( "()"     table)
    (modify-syntax-entry ?\) ")("     table)
    (modify-syntax-entry ?\{ "(}"     table)
    (modify-syntax-entry ?\} "){"     table)
    table)
  "Syntax table for the ASN.1 mode.")


;;;---------------------------------------------------------------------
;;;			     Font Lock
;;;---------------------------------------------------------------------

;; See the documentation of font-lock-keywords (C-h v font-lock-keywords).

(defconst asn1-data-keywords-skeleton
  (list 
   (concat "\\b\\(BEGIN\\|DEFINITIONS\\|END\\|EXPORTS\\|"
	   "EXTENSIBILITY\\s +IMPLIED\\|FROM\\|IMPORTS\\)\\b")
   1
   (if asn1-use-default-font-lock-faces
       'font-lock-preprocessor-face 'asn1-skeleton-face))
  "module skeleton")

(defconst asn1-data-keywords-class 
;  (eval-when-compile
  (list 
   (concat "\\b\\(ABSTRACT-SYNTAX\\|CLASS\\|CONSTRAINED\\s +BY\\|"
	   "INSTANCE\\(\\s +OF\\)?\\|TYPE-IDENTIFIER\\|WITH\\s +SYNTAX\\|"
	   "UNIQUE\\)\\b")
   1
   (if asn1-use-default-font-lock-faces
       'font-lock-type-face 'asn1-class-face))
  "class keywords")

(defconst asn1-data-keywords-tag
;  (eval-when-compile
  (list 
   (concat "\\(\\[\\s *\\(APPLICATION\\|UNIVERSAL\\|"
	   "PRIVATE\\|\\)\\s *[0-9]*\\s *\\]\\)")
   1
   (if asn1-use-default-font-lock-faces
       'font-lock-function-name-face 'asn1-tag-face))
  "Tags")

(defconst asn1-data-keywords-ie-tag
  (list
   "\\b\\(AUTOMATIC\\|EXPLICIT\\|IMPLICIT\\|TAGS\\)\\b"
   1
   (if asn1-use-default-font-lock-faces
       'font-lock-function-name-face 'asn1-tag-face))
  "Tags.")

(defconst asn1-data-keywords-option
  (list 
   "\\b\\(DEFAULT\\|OPTIONAL\\)\\b"
   1
   (if asn1-use-default-font-lock-faces
       'font-lock-reference-face 'asn1-option-face))
  "Keywords DEFAULT and OPTIONAL.")

;; The keyword OPTIONAL is in this class and in the previous.
;; The priority defined in asn1-data-keywords-sets will be used to color.
(defconst asn1-data-keywords-subtyping1
  (list 
   (concat "\\b\\(ALL\\|PRESENT\\|ABSENT\\|OPTIONAL\\|EXCEPT\\|"
	   "COMPONENTS\\(\\s +OF\\)?\\|EXCEPT\\|INCLUDES\\|INTERSECTION\\|"
	   "MAX\\|MIN\\|SIZE\\|UNION\\|WITH\\s +COMPONENTS?\\)\\b")
   1
   (if asn1-use-default-font-lock-faces
       'font-lock-type-face 'asn1-subtyping-face))
  "subtyping expressions")

(defconst asn1-data-keywords-subtyping2
  (list 
   "[(]\\s *\\(FROM\\)\\b"
   1
   (if asn1-use-default-font-lock-faces
       'font-lock-type-face 'asn1-subtyping-face))
  "FROM")

;;sun : XXX BUG with big files !
(defconst asn1-data-keywords-exps
;  (eval-when-compile
  (list
    (concat
    "\\b[A-Z]\\w*\\([.]\\([A-Z]\\|&\\)\\w*\\)*\\s *"
      
    "\\([{][^}{]*"
    "\\([{][^}{]*"
    "[}][^}{]*\\)*"   
    "[}]\\s *\\)?"
      
    "\\("
    "\\([(][^)(]*"
    "\\([(][^)(]*"
    "\\([(][^)(\n]*"
    "\\([(][^)(\n]*"
    "[)][^)(\n]*\\)*"    
    "[)][^)(\n]*\\)*"
    "[)][^)(]*\\)*"
    "[)]"
    "\\(\\s \\|[\n]\\)*"
    "\\)+"
    "\\)")
    5
    (if asn1-use-default-font-lock-faces
	'font-lock-variable-name-face 'asn1-exps-face))
  "subtyping expressions")

(defconst asn1-data-keywords-structured-type
  (list 
   "\\b\\(\\(CHOICE\\|SEQUENCE\\|SET\\)\\(\\s +OF\\)?\\)\\b"
   1
    (if asn1-use-default-font-lock-faces
	'font-lock-type-face 'asn1-structured-type-face))
  "structured types")

(defconst asn1-data-keywords-values
  (list
   "\\b\\(FALSE\\|MINUS-INFINITY\\|PLUS-INFINITY\\|NULL\\|TRUE\\)\\b"
   1
   (if asn1-use-default-font-lock-faces
       'font-lock-string-face 'asn1-values-face))
  "values")

(defconst asn1-data-keywords-type
  (list
   (concat
    "\\b\\(BIT\\s +STRING\\|BMPString\\|BOOLEAN\\|CHARACTER\\s +STRING\\|" 
    "EMBEDDED\\|PDV\\|ENUMERATED\\|EXTERNAL\\|GeneralizedTime\\|"
    "GeneralString\\|GraphicString\\|IA5String\\|INTEGER\\|ISO646String\\|"
    "NumericString\\|ObjectDescriptor\\|OBJECT\\s +IDENTIFIER\\|"
    "OCTET\\s +STRING\\|PrintableString\\|T61String\\|TeletexString\\|" 
    "REAL\\|RELATIVE-OID\\|UniversalString\\|UTCTime\\|UTF8String\\|"
    "VideotexString\\|VisibleString\\|NULL\\)\\b")
   1
   (if asn1-use-default-font-lock-faces
       'font-lock-type-face 'asn1-type-face))
  "basic types")

(defconst asn1-data-keywords-with-syntax
  (list
   "\\b\\(WITH\\s +SYNTAX[ \t\n\r]*[{]\\)\\([^{}:=;]*\\)[}]"
   2
   (if asn1-use-default-font-lock-faces
       'font-lock-variable-name-face 'asn1-with-syntax-face))
  "information object class")

(defconst asn1-data-keywords-sets
;  (eval-when-compile
  (list
   asn1-data-keywords-with-syntax
   asn1-data-keywords-option
   asn1-data-keywords-values
   asn1-data-keywords-class
   asn1-data-keywords-type
   asn1-data-keywords-structured-type
   asn1-data-keywords-exps
   asn1-data-keywords-subtyping1
   asn1-data-keywords-subtyping2
   asn1-data-keywords-skeleton
   asn1-data-keywords-ie-tag
   asn1-data-keywords-tag)
  "List of expressions to highlight with `font-lock'.
The priority increase from top to bottom.")


;;;---------------------------------------------------------------------
;;;			Compatibility Emacs / XEmacs
;;;---------------------------------------------------------------------

;; 'buffer-substring-without-properties in the new
;; versions of Emacs (> 19.30) is equivalent to 'buffer-substring
;; in the old versions of Emacs and in XEmacs.
(cond
 ((fboundp 'buffer-substring-no-properties)
  (defalias 'asn1-substring
    'buffer-substring-no-properties))
 ((fboundp 'buffer-substring-without-properties)
  (defalias 'asn1-substring
    'buffer-substring-without-properties))
 (t
  (defalias 'asn1-substring 'buffer-substring)))

;;; Overlays
(cond (asn1-data-xemacs-p
       ;; faster than (require 'overlay)
       ;; in emacs, there is no overlay-live-p function
       ;; and in xemacs, modify a delete overlay produce
       ;; an error. The solution choosed is to add a
       ;; `condition-case' at each time
       ;; a best choice is to do a function extent-live-p
       ;; who return always t in emacs
       (defalias 'delete-overlay 'delete-extent)
       (defalias 'make-overlay 'make-extent)
       (defalias 'overlay-put 'set-extent-property)
       (defalias 'overlay-get 'extent-property)
       (defalias 'overlay-buffer 'extent-object)
       (defalias 'overlay-start 'extent-start-position)
       (defun asn1-overlay-at (pos)
	 (extent-at pos (current-buffer) 'asn1)))
      (t
       (defun asn1-overlay-at (pos)
	 (asn1-overlay-at-aux (overlays-at pos)))
       (defun asn1-overlay-at-aux (l)
	 (if (null l)
	     nil
	   (if (overlay-get (car l) 'asn1)
	       (car l)
	     (cdr l))))))

(defun asn1-mark ()
  "return nil if there is no active mark"
  (condition-case ()
      (mark)
    (error nil)))

;; for Xemacs :
;; copy-keymap does a bug :
;; the menus don't show the key for commands
(defun asn1-copy-keymap (map-to-copy)
  (if asn1-data-xemacs-p
      (let ((map (make-sparse-keymap)))
	(map-keymap (lambda (key description)
		      (define-key map key (lookup-key map-to-copy key)))
		    map-to-copy)
	map)
    (copy-keymap map-to-copy)))

(defun asn1-menu-add-menu (map)
  "Add the \"ASN.1\" menu from the variable asn1-data-menu
in the menubar."
  (if asn1-data-xemacs-p
      (easy-menu-add asn1-data-menu map)
    (easy-menu-define asn1-mode-menu map
 		      "Menu keymap for ASN.1 mode." asn1-data-menu)))


;;;---------------------------------------------------------------------
;;;			The ASN.1 mode
;;;---------------------------------------------------------------------

;;;###autoload
(defun asn1-mode ()
  "
Major mode for editing ASN.1:1997 specifications

\\{asn1-data-mode-map}

The hook `asn1-mode-hook' is evaluated after loading the asn1-mode

Some informations about this mode are availabale in the file
`asn1-mode.ps' or by typing `M-x asn1-info'."
  (interactive)

  ;; Compatibility with old versions of asn1-mode
  (asn1-convert)

  (kill-all-local-variables)
  (use-local-map asn1-data-mode-map)
  (set-syntax-table asn1-data-mode-syntax-table)
  (setq mode-name "ASN.1")
  (setq major-mode 'asn1-mode)
  
  ;; Local variables

  ;; indentation, comment
  (make-local-variable 'indent-line-function) 
  (setq indent-line-function 'asn1-indent-line)
  (make-local-variable 'comment-multi-line) 
  (setq comment-multi-line nil)

  ;; compilation
  (make-local-variable 'asn1-compile-line)
  (asn1-set-compile t)

  (make-local-variable 'asn1-tag-file)

  ;; pb : user must compile only ASN.1 in an ASN.1 buffer.
  ;; A solution : do a (require 'compile) at the beginning.
  (set (make-local-variable 'compilation-error-regexp-alist)
       '(("^->[^(\n]+[(]Fi[a-z]+:\\([^|\n]+\\)[|]Li.. ?\\([^,\n]+\
\\),col\\.\\([^)\n]+\\)" 1 2 3)))

;; Allow moving module by module with C-x [ and C-x ].
  (make-local-variable 'page-delimiter)
  (setq page-delimiter
 	"\\(\\|\\<DEFINITIONS\\>\\)")
; 	;"^\\s *\\w+\\(\\s \\|\n\\)*{[^}]+}\\(\\s \\|\n\\)*DEFINITIONS")

  ;; To move definition by definition
  (make-local-variable 'paragraph-separate)
  (setq paragraph-separate "[\t \f]*$")
  (make-local-variable 'paragraph-start)
  (setq paragraph-start "[\t \f]*.*::=")

  ;; case is important in ASN.1
  (make-local-variable 'case-fold-search)
  (setq case-fold-search nil)
  (make-local-variable 'case-replace)	
  (setq case-replace nil)
  (make-local-variable 'completion-ignore-case)
  (setq completion-ignore-case t)
  
  ;; comment
  (make-local-variable 'comment-start)
  (setq comment-start "--")
  (make-local-variable 'comment-end)
  (setq comment-end "")
  (make-local-variable 'comment-column)
  (setq comment-column 50) ; Can be modified with set-comment-column (C-x ;)

  ;; Regexp used by font-lock to highlight comments.
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "\\(\\)\\(^\\|\\s \\)[-][-] *")    
  
  (if (not (featurep 'asn1-mode))
      (progn
	(if asn1-data-syntax-menu-loaded
	    (asn1-menu-add-syntax-menu (current-local-map)))
	(asn1-menu-add-menu (current-local-map)))

    (if asn1-syntax-menu-autoload
      (asn1-load-syntax-menu))

    ;; menu "ASN.1"
    (asn1-menu-add-menu (current-local-map))
    
    ;; font-lock
    (if (not asn1-use-default-font-lock-faces)
    (cond (asn1-use-standard-custom
	   (mapcar 'asn1-standard-defface asn1-data-faces)
	   (mapcar 'asn1-standard-defface asn1-data-faces-default))
	  (t
	   (asn1-modify-face-list asn1-data-faces)
	   (asn1-defface-list asn1-data-faces-default))))
    ;; Not very beautiful
    (setq Info-default-directory-list
	  (cons asn1-data-dir Info-default-directory-list)))
  
  ;; Keywords to highlight
  (setq asn1-data-font-lock-keywords asn1-data-keywords-sets)
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(asn1-data-font-lock-keywords nil nil nil))
  
  (run-hooks 'asn1-mode-hook))

(defun asn1-mode-version ()
  "Gives the version of this mode."
  (interactive)
  (let ((version
	 (format "Mode ASN.1:1997, version %s" 
		 asn1-data-version)))
    (if (interactive-p)
	(message "%s" version)
      version)))

(defun asn1-comment-line-p ()
  "Return t if the current line contains only a comment, nil otherwise"
  (save-excursion
    (let ((eol (end-of-line)))
      (beginning-of-line)
      (re-search-forward (concat "^[\t ]*" comment-start) eol t))))

;(defun asn1-current-indentation ()
;  "Return the current code indentation value, regardless of blank lines and
;comments."
;  (save-excursion
;    (beginning-of-line)
;    (or (while					  ;
;	    (if (= (point) (point-min))
;		0
;	      (let ((eol (progn (backward-char) (point))))
;		(beginning-of-line)
;		(re-search-forward (concat "^[\t ]*" comment-start "\\|[\t ]*$")
;				   eol t))))
;	(current-indentation))))
  
(defun asn1-calculate-indent-comment ()
  "Return the current comment indentation value. It depends on the previous
line: if it is a comment or not"
  (save-excursion
    (beginning-of-line)
    (if (= (point) (point-min))
	0
      (let ((eol (progn (backward-char) (point))))
	(beginning-of-line)
	(if (re-search-forward comment-start eol t)
	    (progn
	      (goto-char (match-beginning 0))
	      (current-column))
	  (asn1-calculate-indent-inside-outside-a-block))))))
  


;;;----------------------------------------------------------------------
;;;	               Indentation
;;;----------------------------------------------------------------------



(defun mapfilter (filter-p entry-list)
  "entry-list is a list of elements.
Returns the list of element for which
(FILTER-P element) returns a no nil value."
  (let ((temp-list nil))
    (mapcar
     '(lambda (cell)
	(if (funcall filter-p cell)
	    (setq temp-list (cons cell temp-list))))
     entry-list)
    temp-list))

(defun asn1-into-a-commentary-or-string-p ()
  "Returns t if the cursor is in a comment."
  (interactive)
  (let ((indent-point (point))
	state)
    (save-excursion
      (beginning-of-line)
      (setq state (parse-partial-sexp (point) indent-point 0))
      ;; si l'on est dans un commentaire la valeur retournee
      ;; est 4, si l'on est dans une chaine elle est de 3
      (or (nth 3 state) (nth 4 state)))))

(defun asn1-goto-beginning-of-regexp (regexp &optional limit)
  "Search backward for regexp and place the cursor at the beginning of this
regexp if found (otherwise, returns nil).
The cursor can't move before the argument limit.
The regexp found is not in a comment or in a string (of \"\" or \'\')."
  (interactive)
  (let ((found t)
	(valid nil))
    (while (and found
		(not valid))
      (setq found (re-search-backward regexp (or limit 0) 'move 1))
      (if found (setq valid (and (goto-char (match-beginning 0))
				 (not (asn1-into-a-commentary-or-string-p))))))
    (if found
	(asn1-substring (match-beginning 0) (match-end 0))
      nil)))

(defun asn1-skip-comment-and-space ()
  "Move the cursor after all comments and spaces in front of
it in the same line."
  (let ((end-line nil))
    (save-excursion
      (end-of-line)
      (setq end-line (point)))
    (forward-comment (buffer-size))
    (skip-chars-forward "\t ")
    (if (> (point) end-line)
	(goto-char end-line))
    (point)))

(defun asn1-skip-spaces ()
  "Move the cursor after all spaces in front of it in the same line."
  (let ((end-line nil))
    (save-excursion
      (end-of-line)
      (setq end-line (point)))
    (skip-chars-forward "\t ")
    (if (> (point) end-line)
	(goto-char end-line))
    (point)))

(defun asn1-beginning-of-line-of-code ()
  "Move the cursor at beginning of line, after all space and comment."
  (beginning-of-line)
  (asn1-skip-spaces))

(defun asn1-previous-line-of-code ()
  "Move backward the cursor to the first line which is not composed to only
space and comment."
  (let ((finished nil))
    (beginning-of-line)
    (setq finished (bobp))  
    (while (not finished)
      (beginning-of-line 0)
      (setq finished (bobp))  
      (asn1-beginning-of-line-of-code)
      (setq finished (or finished (not (eolp)))))
    (if (eolp)
	(beginning-of-line))))

(defun asn1-indent-to-column (col &optional simple)
  "Indent the current line to the column col.
Then, if simple is not nil, do nothing, else :
 1) If the cursor is in space or comment before code, it move at the
    beginning of the code.
 2) If the cursor is in a blank line, it move at the end of it.
 3) If the cursor is beetween 2 char of code, it won't move."
  (let* ((point-in-whitespace
	  (save-excursion
	    (<= (point) (asn1-beginning-of-line-of-code))))
	 (blank-line-p
	  (save-excursion
	    (beginning-of-line)
	    (looking-at "\\s *$"))))
    (cond ((/= col (current-indentation))
	   (save-excursion
	     (beginning-of-line 1)
	     (delete-horizontal-space)
	     (indent-to col))))
    (if (not simple)
	(cond (blank-line-p
	       (end-of-line))
	      (point-in-whitespace
	       (back-to-indentation))))))


;;;---------------------------------------------------------------------
;;;			Data for indentation
;;;---------------------------------------------------------------------

(defvar asn1-data-indentation-table
  (list (list "DEFINITIONS"	
	      asn1-data-indent-of-DEFINITIONS	
	      asn1-data-indent-in-DEFINITION-bloc
	      "\\bDEFINITIONS\\b"	nil		
	      'asn1-indent-DEFINITIONS)
	(list "BEGIN"		
	      asn1-data-indent-of-BEGIN	
	      asn1-data-indent-in-BEGIN-bloc
	      "\\bBEGIN\\b"     "\\bDEFINITIONS\\b"	
	      'asn1-indent-BEGIN)
        (list "END"		
	      asn1-data-indent-of-END
	      nil
	      "\\bEND\\b"	 "\\bBEGIN\\b"		
	      'asn1-indent-END)
	(list "IMPORTS"		
	      asn1-data-indent-of-IMPORTS
	      asn1-data-indent-in-IMPORTS-bloc
	      "\\bIMPORTS\\b"     "\\bBEGIN\\b"		
	      'asn1-indent-IMPORTS)
	(list "EXPORTS"		
	      asn1-data-indent-of-EXPORTS
	      asn1-data-indent-in-EXPORTS-bloc
	      "\\bEXPORTS\\b"     "\\bBEGIN\\b"		
	      'asn1-indent-EXPORTS)
	(list "FROM"		
	      asn1-data-indent-of-FROM	
	      nil
	      "\\bFROM\\b" "\\bIMPORTS\\b"  
	      'asn1-indent-FROM)
	(list ";"		
	      asn1-data-indent-of-\;
	      nil
	      ";"     "\\b\\(EXPORTS\\|IMPORTS\\)\\b"	
	      'asn1-indent-\;)
	(list "{"		
	      asn1-data-indent-of-\{
	      nil
	      "[{]"		"\\w"			
	      nil)
	(list "("		
	      asn1-data-indent-of-\(
	      nil
	      "[(]"		"\\w"			
	      nil)
	(list "["		
	      asn1-data-indent-of-\[
	      nil
	      "\\["		"\\w"			
	      nil)
	(list "::="		
	      asn1-data-indent-of-::=	
	      nil	
	      "::="		"\\w"			
	      'asn1-indent-::=)
	)
  "This list contains indentation information :
Elements are list :
\"(KEY NB1 NB2 REG1 REG2 FUN1)\". 

KEY is a block delimitor.
If this key is at the beginning of the line, then the line is indent like that:
The keyword REG2 is search backward.
if I1 is the indentation of the line where REG2 is, the indentation for the
line which contains the key is (I1 + NB1).

For another line of the document, one search backward the first KEY which can
be found.
Then the identation is calculate with the function FUN1, using the data NB2.

REG1 is the regexp used for searching the KEY in the document.")


;; accesseurs associes a la variable precedente asn1-data-indentation-table
(defun asn1-get-i-table-beginning-of-bloc (cell)
  "returns REG2 (see `asn1-data-indentation-table')"
  (nth 4 cell))
(defun asn1-get-i-table-keyword-indent (cell)
   "returns NB1 (see `asn1-data-indentation-table')"
 (nth 1 cell))
(defun asn1-get-i-table-bloc-indentation (cell)
   "returns NB2 (see `asn1-data-indentation-table')"
  (nth 2 cell))
(defun asn1-get-i-table-regexp (cell)
   "returns REG3 (see `asn1-data-indentation-table')"
  (nth 3 cell))
(defun asn1-get-i-table-bloc-indent-function (cell)
   "returns FUN1 (see `asn1-data-indentation-table')"
 (nth 5 cell))

(defun asn1-string2regexp (string)
  "transforms a string into a regexp which correpond to it."
  (let ((regexp nil) 
	(iter (length string)))
    (while (> iter 0)
      (setq iter (1- iter))
      (setq 
       regexp 
       (concat "[" (substring string iter (1+ iter)) "]" regexp)))
    regexp))

(defvar asn1-data-keywords-to-search
  (concat
   "\\("
   (mapconcat 
    '(lambda (cell)
       (asn1-string2regexp (car cell)))  
    asn1-data-indentation-table 
    "\\|")
   "\\)")
  "List of keywords for which one know how to indent if they begin a line.
This table is construct from the keys of `asn1-data-indentation-table'.")

(defvar asn1-data-statement-table
   (concat
   "\\("
   (mapconcat
    (lambda (cell)
      (asn1-get-i-table-regexp cell))
    (mapfilter 
     (lambda (cell) 
       (asn1-get-i-table-bloc-indent-function cell)) 
     asn1-data-indentation-table) 
    "\\)\\|\\(")
   "\\)")
  "List of keywords delimiting a block for which one know how to indent.
These keywords will be recognized if they are at the beginning of line
(while jumping spaces and comments which can be at the beginning of this line)"
)

(defun asn1-indent-IMPORTS (departure)
  "Calculate the indentation in the block next an IMPORTS"
  (forward-word 1)
  (asn1-skip-comment-and-space)
  (if (eolp)
      (+ (current-indentation) 
	 (asn1-get-i-table-bloc-indentation
	  (assoc "IMPORTS" asn1-data-indentation-table)))
      (current-column)))

(defun asn1-indent-EXPORTS (departure)
  "Calculate the indentation in the block next an EXPORTS"
  (forward-word 1)
  (asn1-skip-comment-and-space)
  (if (eolp)
      (+ (current-indentation) 
	 (asn1-get-i-table-bloc-indentation
	  (assoc "EXPORTS" asn1-data-indentation-table)))
    (current-column)))

(defun asn1-indent-FROM (departure)
  "Calculate the indentation in the block next a FROM"
  (cond ((save-excursion
	   (skip-chars-backward " \t\n")
	   (backward-char)
	   (looking-at "[(]"))
	 (asn1-goto-beginning-of-regexp "\\bBEGIN\\b")
	 (asn1-indent-BEGIN departure))
	((asn1-goto-beginning-of-regexp "\\bIMPORTS\\b")
	 (asn1-indent-IMPORTS departure))
	(t
	 0)))

(defun asn1-indent-\; (departure)
  "Calculate the indentation in the block next a ;
(end of an EXPORTS or an IMPORT)."
  (if (asn1-goto-beginning-of-regexp "\\b\\(BEGIN\\)\\b")
      (progn
	 (+ (current-indentation) 
	    (asn1-get-i-table-bloc-indentation 
	     (assoc "BEGIN"
		    asn1-data-indentation-table))))
    0)) ; if there is no BEGIN

(defun asn1-indent-BEGIN (departure)
  "Calculate the indentation in the block next a BEGIN."
  (+ (current-indentation) 
     (asn1-get-i-table-bloc-indentation (assoc "BEGIN"
		   asn1-data-indentation-table))))
  
(defun asn1-indent-END (departure)
  "Calculate the indentation in the block next an END."
  (goto-char departure)
  (beginning-of-line 0)
  (if (looking-at "\\s *$")
      (goto-char departure))
  (current-indentation))

(defun asn1-indent-DEFINITIONS (departure)
  "Calculate the indentation after the block next a DEFINITIONS."
 (+ (current-indentation) 
     (asn1-get-i-table-bloc-indentation (assoc "DEFINITIONS"
		   asn1-data-indentation-table))))

(defun asn1-indent-::= (departure)
  "Calculate the indentation in the block next a ::=."
   (let ((position (point)))
     (goto-char departure)
     (asn1-beginning-of-line-of-code)
     (if asn1-data-indent-WITH-SYNTAX
	 (progn
	   (if (looking-at  "\\b\\(WITH\\s +SYNTAX\\)\\b")
	       (if (asn1-goto-beginning-of-regexp "\\bCLASS\\b")
		   (current-column)
		 (goto-char position)
		 (current-indentation))
	   (goto-char position)
	   (current-indentation)))

(defun asn1-indent-new-comment-line (&optional soft)
  "Continue current comment on the next line."
  (interactive)
  (let* ((bolpos (save-excursion
		   (beginning-of-line)
		   (point)))
	 (cbeg (save-excursion
		 (and (re-search-backward comment-start-skip bolpos t)
		      (current-column)))))
    (if cbeg
	(progn
	  ;; Within a comment
	  (if soft (insert ?\n) (newline 1))
	  (indent-to-column cbeg)
	  (insert comment-start))
      ;; Outside comment
      (newline-and-indent))))


(defun asn1-fill-paragraph (&optional justify)
 "Fill comment paragraph. The cursor must be placed on the first comment line."
  (interactive)
  (let* ((bop (save-excursion
		(re-search-backward comment-start-skip (point-at-bol) t)))
	  (bop-col (save-excursion
		     (goto-char (or bop 0))
		     (current-column))))
    (if bop					  ;beginning-of-paragraph
      (if (< fill-column (save-excursion
			   (end-of-line)
			   (current-column)))
	(progn
	  (move-to-column fill-column)
	  (forward-word -1)			  ;Last word before fill
	  (newline 1)
	  (indent-to-column bop-col)
	  (insert comment-start)
	  (end-of-line)
	  (goto-char (or
		       (end-of-line)
		       (save-excursion
			 (forward-line)
			 (re-search-forward
			   (concat "^" comment-start-skip)
			   (point-at-eol) t)
			 (match-end 0))))
	  (delete-region (mark) (point))
	  (asn1-fill-paragraph)
	  )))))


(goto-char position)
(current-indentation))))
;       (+ asn1-data-bracket-indent-text (current-indentation)))))
; For the  ::=
;         INTEGER -> problem after : shift

(defun asn1-indent-line ()
  "Indent the current line, then move the cursor using the type of the line
where the cursor is."
  (interactive)
  (asn1-indent-to-column
   (let ((a (asn1-calculate-indent)))
     (if (number-or-marker-p a)
	 a
       0)))) ; security : if there is a problem, 0 is returned.
             ; a problem occured only if there is a syntax error.
             ; then the line won't be correctly indented.

(defun asn1-basic-indent-line ()
  "Indent the curent line."
  (asn1-indent-to-column (asn1-calculate-indent) t))

(defun asn1-reindent-then-newline-and-indent ()
  "Reindent current line, insert newline, then indent the new line."
  (interactive)
  (insert "\n")
  (save-excursion
    (beginning-of-line 0)
    (asn1-indent-line))
  (asn1-indent-line))

(defun asn1-calculate-indent-keyword-beginning-of-line ()
  "Give the indentation of a line which begin by a keyword which is a block
delimitor."
  (let   ((search-result nil)
	  associated-info)
    (setq search-result 
	  (asn1-substring 
	   (match-beginning 1) (match-end 1)))
    (setq associated-info 
	  (assoc search-result asn1-data-indentation-table))
    (if (asn1-get-i-table-beginning-of-bloc associated-info)
	(if (asn1-goto-beginning-of-regexp 
	     (asn1-get-i-table-beginning-of-bloc associated-info))
	    (progn
	      (+ (asn1-get-i-table-keyword-indent associated-info) 
		 (current-indentation)))
	  0)
      (asn1-get-i-table-keyword-indent associated-info))))

(defun asn1-calculate-indent-inside-outside-a-block ()
  "Give the indentation of a line which begin by a keyword which is not
a block delimitor."
  (let* ((original-point (point))
	 (begin-line (save-excursion
		      (asn1-beginning-of-line-of-code)
		      (point)))
	 (search-result nil)
	 associated-info
	 (text-after-parenthesis nil)
	 (parenthesis-indentation nil)
	 (base-line-indentation
	  (save-excursion
	    (condition-case ()
		(progn 
		  (beginning-of-line)
		  (up-list -1)
		  (while (asn1-into-a-commentary-or-string-p)
		    (up-list -1))
		  (setq parenthesis-indentation 
			(current-column))
		  (forward-char 1)
		  (asn1-skip-spaces)
		  (if (eolp)
		      (progn
			(setq text-after-parenthesis nil)
			(current-indentation))
		    (progn
		      (setq text-after-parenthesis t)
		      (current-column))))
	      (error nil))))
	 (end-line-p 
	  (save-excursion
	    (condition-case ()
		(progn
		  (beginning-of-line)
		  (up-list 1)
		  (while (asn1-into-a-commentary-or-string-p)
		    (up-list 1))
		  (backward-char 1)
		  (= (point) begin-line))
	      (error nil)))))
    (cond (end-line-p
	   (if text-after-parenthesis
	       (if parenthesis-indentation
		   parenthesis-indentation
		 0)
	     (if base-line-indentation
		 base-line-indentation
	       0)))
	  (text-after-parenthesis
	   (if base-line-indentation
	       base-line-indentation
	     0))
	  (base-line-indentation 
	   (+ asn1-data-bracket-indent-text
	      base-line-indentation))
	  ((progn
	     (beginning-of-line)
	     (asn1-goto-beginning-of-regexp 
	      asn1-data-statement-table))
	   (setq search-result 
		 (asn1-substring 
		  (match-beginning 0) 
		  (match-end 0)))
	   (setq associated-info 
		 (assoc search-result 
			asn1-data-indentation-table))
	   (funcall 
	    (asn1-get-i-table-bloc-indent-function associated-info) 
	    original-point))
	  (t
	   (goto-char original-point)
	   (beginning-of-line 0)
	   (if (looking-at "\\s *$")
	       (goto-char original-point))
	   (current-indentation)))))

(defun asn1-calculate-indent ()
"Give the indentation for the current line (return a number)."
(save-excursion
  (setq case-fold-search nil)
  (asn1-beginning-of-line-of-code)
  (cond ((looking-at
	  (concat asn1-data-keywords-to-search "\\(\\s \\|$\\)"))
	 (asn1-calculate-indent-keyword-beginning-of-line))
	((looking-at comment-start)
	 (asn1-calculate-indent-comment))
	(t
	 (asn1-calculate-indent-inside-outside-a-block)))))

(defun asn1-indent-buffer ()
  "Indent the buffer with `indent-region'."
  (interactive)
  (indent-region (point-min) (point-max) nil))


;;;---------------------------------------------------------------------
;;;			 Completion
;;;---------------------------------------------------------------------

(defconst asn1-data-completion-prelist
  (eval-when-compile
    (mapcar 
     (lambda (string) (list string))
     (let ((liste-rslt nil)
	   (first-list
	    '("BEGIN" "DEFINITIONS" "END" "EXPORTS" "EXTENSIBILITY" "IMPLIED"
	      "EXTENSIBILITY IMPLIED" "FROM" "IMPORTS" 
	      
	      "ABSTRACT-SYNTAX" "BY" "CLASS" "CONSTRAINED" "CONSTRAINED BY" 
	      "INSTANCE" "OF" 
	      "INSTANCE OF" "TYPE-IDENTIFIER" "WITH" "SYNTAX" "WITH SYNTAX" 
	      "UNIQUE" "CONSTRAINED BY"
	      
	      "APPLICATION" "UNIVERSAL" "PRIVATE"
	      
	      "DEFAULT" "OPTIONAL"
	      
	      "ALL" "PRESENT" "ABSENT" "OPTIONAL" "EXCEPT" "COMPONENTS" "OF"
	      "COMPONENTS OF" "EXCEPT" "INCLUDES" "INTERSECTION" "MAX" "MIN" 
	      "SIZE" "UNION" "WITH" "COMPONENT" "WITH COMPONENT"
	      "WITH" "COMPONENTS" "WITH COMPONENTS"
	      
	      "SET" "SEQUENCE" "CHOICE"
	   
	      "SET OF" "SEQUENCE OF" "ANY DEFINED BY"
	      
	      "OF"
	      
	      "FALSE" "MINUS-INFINITY" "PLUS-INFINITY" "NULL" "TRUE"
	      
	      "BIT" "STRING" "BIT STRING" "BMPString" "BOOLEAN" "CHARACTER" 
	      "CHARACTER STRING" "EMBEDDED PDV"
	      "EMBEDDED" "PDV" "ENUMERATED" "EXTERNAL" "GeneralizedTime" 
	      "GeneralString" "GraphicString" "IA5String" "INTEGER"
	      "ISO646String"
	      
	      "NumericString" "ObjectDescriptor" "OBJECT" "IDENTIFIER" 
	      "OBJECT IDENTIFIER" "OCTET" "STRING" "OCTET STRING" 
	      "PrintableString" "T61String" "TeletexString" "REAL"
	      "RELATIVE-OID"

	      "UniversalString" "UTCTime" "UTF8String" "VideotexString" 
	      "VisibleString" "NULL"
	      
	      "AUTOMATIC" "EXPLICIT" "IMPLICIT"  "TAGS"
	      "AUTOMATIC TAGS" "EXPLICIT TAGS" "IMPLICIT TAGS"
	      "HAS" "PROPERTY" "HAS PROPERTY" "IDENTIFIED BY" "IDENTIFIED" "BY"
	      
	      "itu-t(0)" "ccitt(0)" "iso(1)" "joint-iso-itu-t(2)" 
	      "joint-iso-ccitt(2)" "recommendation(0)" "question(1)" 
	      "administration(2)" "network-operator(3)" 
	      "identified-organization(4)"
	      "standard(0)" "member-body(2)" "identified-organization(3)")))

    ;; Duplicate elements must be removed
    (mapcar
     (lambda (string)
       (if (not (member string liste-rslt))
	   (setq liste-rslt (append (list string) liste-rslt))))
     first-list)
    liste-rslt))
    )
  "List of keywords used for ASN.1. completion")

(defun asn1-completion ()
  "Do a completion with `asn1-data-completion-prelist'." 
  (interactive)
  (let* ((end (point))
	 (buffer-syntax (syntax-table))
	 (beg (unwind-protect
		  (save-excursion
		    (set-syntax-table asn1-data-mode-syntax-table)
		    (skip-syntax-backward "\\w")
		    (point))
		(set-syntax-table buffer-syntax)))
	 (pattern (asn1-substring beg end))
	 (completion (try-completion pattern asn1-data-completion-prelist)))
    (cond ((eq completion t))
	  ((null completion)
	   (message (asn1-lg
		     "Pas de completion pour \"%s\""
		     "Can't find completion for \"%s\"") pattern)
	   (ding))
	  ((not (string= pattern completion))
	   (delete-region beg end)
	   (insert completion))
	  (t
	   (with-output-to-temp-buffer "*Completions*"
	     (display-completion-list
	      (sort (all-completions pattern
				     asn1-data-completion-prelist)
		    'string<)))))))


;;;---------------------------------------------------------------------
;;;				Divers
;;;---------------------------------------------------------------------

;;sun
(defun asn1-comment-dwim (arg)
  "Like `comment-dwim' but with /* and */ style for comment.")

(defun asn1-comment-region (beg end &optional arg)
  "Like `comment-region' (see this function) but transform all
comment \"--\" in the commented code to \"__\".
And for uncomment : \"__\" -> \"--\"."
  (interactive "r\nP")
  (let ((numarg (prefix-numeric-value arg)))
    (save-excursion
      (goto-char (min beg end))
      (while (search-forward (if (> numarg 0) "--" "__") (max beg end) t)
	(replace-match (if (> numarg 0) "__" "--") t t))))
  (comment-region beg end arg))

(defun asn1-uncomment-region (beg end)
  (interactive "r")
  "Call (asn1-comment-region beg end -1)"
  (asn1-comment-region beg end -1))

(defun asn1-create-tag-file ()
  (interactive)
    (setq asn1-tag-file
	  (asn1-read-shell-command
	   (asn1-lg "Fichiers ou chercher les references : "
		    "Files where find the references: ")
	   asn1-tag-file nil))
  (let ((f asn1-tag-file)
	(b (get-buffer-create "*ASN.1-Tags*")))
    (display-buffer b)
    (set-buffer b)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert (asn1-lg "Creation du fichier de references..."
		     "Creating tags file..."))
    (set-process-sentinel
     (start-process-shell-command
      "asn1-tag"
      b
      ;; XXX Emacs for windows not tested !
      (if asn1-win
	"etags --regex='\^[ //t]*//([A-Za-z][-A-Za-z0-9]*//).*::=\//1\'"
	"etags --regex='/^[ \\t]*\\([A-Za-z][-A-Za-z0-9]*\\).*::=/\\1/'")
      f)
     'asn1-tags-sentinel)))
;    (lambda (&rest ignore)
;      (message (asn1-lg "Creation du fichier de tags terminée"
; 		       "Creation of tags file finished")))))

(defun asn1-tags-sentinel (proc msg)
  (let ((buffer (process-buffer proc)))
    (if (memq (process-status proc) '(signal exit))
	(progn
	  (if (null (buffer-name buffer))
	      ;; buffer killeds
	      (set-process-buffer proc nil)
	    (let ((obuf (current-buffer)))
	      ;; save-excursion isn't the right thing if
	      ;; process-buffer is current-buffer
	      (unwind-protect
		  (progn
		    ;; Write something in the compilation buffer
		    ;; and hack its mode line.
		    (set-buffer buffer)
		    (goto-char (point-max))
		    (insert
		     (asn1-lg "\nCreation du fichier de references terminee."
			      "\nCreation of tags file finished."))
		    ;; Since the buffer and mode line will show that the
		    ;; process is dead, we can delete it now.  Otherwise it
		    ;; will stay around until M-x list-processes.
		    (delete-process proc))
		(if (buffer-live-p obuf)
		    (set-buffer obuf)))))))))

(defun asn1-mice-button-key (n)
  (vector (list (intern (concat (if asn1-data-xemacs-p
				    "button"
				  "mouse-")
				(number-to-string n))))))

(defun asn1-mice-normal-button (event arg)
  ;; other possibility : lookup key
  (if asn1-data-xemacs-p
      (cond ((eq asn1-mice-button 1)
	     (mouse-track event))
	    ((eq asn1-mice-button 2)
	     (mouse-yank event))
	    ((eq asn1-mice-button 3)
	     (popup-mode-menu)))
    (cond ((eq asn1-mice-button 1)
	   (mouse-yank-at-click event arg))
	  ((eq asn1-mice-button 2)
	   (mouse-drag-region event))
	  ((eq asn1-mice-button 3)
	   (mouse-save-then-kill event)))))

(defun asn1-info ()
  (interactive)
  (delete-other-windows)
  (info (asn1-lg
	 (if (file-exists-p (concat asn1-data-dir "asn1-mode-fr.info"))
	     (concat asn1-data-dir "asn1-mode-fr.info")
	   "asn1-mode-fr.info")
	 (if (file-exists-p (concat asn1-data-dir "asn1-mode-en.info"))
	     (concat asn1-data-dir "asn1-mode-en.info")
	   "asn1-mode-en.info"))))

(defun asn1-tutorial ()
  (interactive)
  (delete-other-windows)
  (if (eq 'fr asn1-language)
      (find-file (concat asn1-data-dir "TUTORIAL"))
    (help-with-tutorial)))

(defun asn1-custom-options ()
  (interactive)
  (delete-other-windows)
  (customize-group 'asn1-options))

(defun asn1-custom-indentation ()
  (interactive)
  (delete-other-windows)
  (customize-group 'asn1-indentation))

(defun asn1-custom-face ()
  "A simply way to custom faces"
  (interactive)
  (delete-other-windows)
  (if asn1-use-standard-custom
      (customize-group 'asn1-face)
    (asn1-conf-face
     (asn1-lg
      "Configuration des styles d'ecritures du mode ASN.1
Remarque : les couleurs des chaines de caracteres et des commentaires sont
           les meme dans tous les modes emacs."
      "Configuration of ASN.1 Faces
Note : colors for string and comment are the same for all emacs modes.")
     'asn1-data-faces
     'asn1-data-faces-default)))

(defun asn1-set-compile (&optional force)
  (let ((cmde (concat asn1-compile-command " "
		      (if (string-match "[oO][sS][sS][^/]$" asn1-compile-command)
			  "-messageFormat emacs ")
		      asn1-compile-switches " "
		      buffer-file-name)))
    (if (or force (not (equal cmde asn1-compile-precedent-command)))
	(setq asn1-compile-line cmde))
    (setq asn1-compile-precedent-command cmde)))

(defun asn1-set-compile-command ()
  (interactive)
  (setq asn1-compile-command
	(read-file-name
	 (asn1-lg
	  "Commande de compilation : "
	  "Compile Command : ")))
  (asn1-set-compile t))

(defun asn1-set-compile-switches ()
  (interactive)
  (setq asn1-compile-switches
	(read-from-minibuffer
	 (asn1-lg
	  "Parametres de compilation : "
	  "Compile Switches : ")
	 asn1-compile-switches))
  (asn1-set-compile t))

;; emacs 19.34
(if (not (fboundp 'split-string))
(defun split-string (string &optional separators)
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
If SEPARATORS is absent, it defaults to \"[ \\f\\t\\n\\r\\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that."
  (let ((rexp (or separators "[ \f\t\n\r\v]+"))
	(start 0)
	notfirst
	(list nil))
    (while (and (string-match rexp string
			      (if (and notfirst
				       (= start (match-beginning 0))
				       (< start (length string)))
				  (1+ start) start))
		(< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
	  (and (eq (match-beginning 0) (match-end 0))
	       (eq (match-beginning 0) start))
	  (setq list
		(cons (substring string start (match-beginning 0))
		      list)))
      (setq start (match-end 0)))
    (or (eq start (length string))
	(setq list
	      (cons (substring string start)
		    list)))
    (nreverse list))))

(defun asn1-change-language (arg)
  "Change language."
  (interactive "SLanguage : ")
  (set 'asn1-language arg)

  ;; Not very usual
  (load "asn1-mode")
  (if (featurep 'asn1-diff)
      (load "asn1-diff"))
  (let ((l (buffer-list)))
    (while l
      (set-buffer (car l))
      (cond ((eq major-mode 'asn1-mode)
	     (easy-menu-remove asn1-data-menu)
	     (asn1-menu-add-menu (current-local-map))
	     (if (not asn1-data-diff-menu-p) nil
	       (easy-menu-remove asn1-diff-menu)
	       (easy-menu-remove asn1-compil-menu)
	       (asn1-diff-add-menu (current-local-map))))
	    ((eq major-mode 'asn1-diff)
	     (easy-menu-remove asn1-diff-menu)
	     (easy-menu-remove asn1-compil-menu)
	     (asn1-diff-add-menu (current-local-map))))
      (setq l (cdr l))))
  (mapcar (lambda (buf) (kill-buffer (get-buffer-create buf)))
	  '("*Customize Group: Asn1 Indentation*"
	    "*Customize Group: Asn1 Options*"
	    "*Customize Group: Asn1 Diff*"
	    "*ASN.1 Configuration*"))
  (setq asn1-compile-switches
	(asn1-lg
	 (if (string-match "-fr[ \t]" asn1-compile-switches)
	     asn1-compile-switches
	   (concat "-fr " asn1-compile-switches))
	 (asn1-list-to-string
	  (delete "-fr"
		  (split-string asn1-compile-switches)))))
  (if (featurep 'asn1-diff)
  (setq asn1-diff-switches
	(asn1-lg
	 (if (string-match "-fr[ \t]" asn1-diff-switches)
	     asn1-diff-switches
	   (concat "-fr " asn1-diff-switches))
	 (asn1-list-to-string
	  (delete "-fr"
		  (split-string asn1-diff-switches))))))
  (message (asn1-lg "Langage choisi : Francais"
	     "Chosen language : English"))
  )

(defun asn1-list-to-string (list)
  (if (null list)
      ""
    (concat " " (car list)
	    (asn1-list-to-string (cdr list)))))


;;;---------------------------------------------------------------------
;;;			Compilation with asnp
;;;---------------------------------------------------------------------

;; To allow the same completion as with XEmacs in the minibuffer

(autoload 'comint-dynamic-complete "comint" "" t)
(autoload 'comint-dynamic-list-completions "comint" "" t)

(defvar asn1-read-shell-command-map
  (let ((map (asn1-copy-keymap minibuffer-local-map)))
    (define-key map "\t" 'comint-dynamic-complete)
    (define-key map "\M-\t" 'comint-dynamic-complete)
    (define-key map "\M-?" 'comint-dynamic-list-completions)
    map))

(defun asn1-read-shell-command (prompt &optional initial-input history)
  "Just like read-string, but uses asn1-read-shell-command-map:
\\{asn1-read-shell-command-map}"
  (let ((minibuffer-completion-table nil))
    (read-from-minibuffer prompt initial-input asn1-read-shell-command-map
			  nil (or history 'shell-command-history))))


(defun asn1-compile ()
  "Like compile but use the completion of comint (like XEmacs does)"
  (interactive)
  (require 'compile)
  (asn1-set-compile)
  (setq asn1-compile-line
	(if (or compilation-read-command current-prefix-arg)
	    ;; XEmacs change
	    (asn1-read-shell-command
	     (asn1-lg
	      "Commande de compilation : "
	      "Compile command: ")
	     asn1-compile-line
	     ;; #### minibuffer code should do this
	     (if (equal (car compile-history)
			asn1-compile-line)
		 '(compile-history . 1)
	       'compile-history))
	  asn1-compile-line))
  (save-some-buffers (not compilation-ask-about-save) nil)
  (if asn1-compile-like-diff
      (asn1-diff-compile asn1-compile-line)
    (compile-internal asn1-compile-line "No more errors")))


;;;---------------------------------------------------------------------
;;;		Functions deals with FACES 
;;;---------------------------------------------------------------------
;; This functions exist because they exist before...
;; defface is actually a best choice

(defun asn1-defface-list (list)
  "Apply asn1-defface to a list of definitions of faces"
  (let ((l list))
    (while l
      (apply 'asn1-defface (car l))
      (setq l (cdr l)))))

(defun asn1-modify-face-list (list)
  "Apply asn1-modify-face to a list of definitions of faces"
  (let ((l list))
    (while l
      (apply 'asn1-modify-face (car l))
      (setq l (cdr l)))))

(defun asn1-defface (face &optional foreground background stipple
			  bold-p italic-p underline-p doc)
  "Create FACE if it doesn't exist.
If FACE exist, don't modify it.
See this `asn1-modify-face' for syntax"
  (if (not (boundp face))
      (eval `(defvar ,face (quote ,face) ,(if doc doc ""))))
  (if (facep face)
      nil
    (make-face face)
    (asn1-modify-face face foreground background stipple
		      bold-p italic-p underline-p)))

(defun asn1-modify-face (face &optional foreground background stipple
			      bold-p italic-p underline-p doc)
  "modify the face FACE
FOREGROUND and BACKGROUND are color (name of color or nil, which means
the current color)
BOLD-P ITALIC-P UNDERLINE-P are nil or p"
  (if (not (boundp face))
      (eval `(defvar ,face (quote ,face) ,(if doc doc ""))))
  (if (facep face)
      nil
    (make-face face))
  (set-face-foreground face
		       (if (and asn1-data-xemacs-p (null foreground))
			   (face-foreground 'default)
			 foreground))
  (set-face-background face
		       (if (and asn1-data-xemacs-p (null background))
			   (face-background 'default)
			 background))
  (condition-case () ;; this bug with xemacs21
  (if (fboundp 'set-face-stipple)
      (set-face-stipple face
			(if (and asn1-data-xemacs-p (null stipple))
			    ""
			  stipple)))
  (error nil))
  (asn1-bold face bold-p)
  (asn1-italic face italic-p)
  (set-face-underline-p face underline-p))

(defun asn1-bold (face on)
  "if ON is NIL, put the face unbold. Put the face bold otherwise."
  (if asn1-data-xemacs-p
      (if on
	  (make-face-bold face)
	(make-face-unbold face))
    (if on
	(make-face-bold face nil 'noerr)
      (make-face-unbold face nil 'noerr))))

(defun asn1-italic (face on)
  "if ON is NIL, put the face unitalic. Put the face italic otherwise."
 (if asn1-data-xemacs-p
     (if on
	  (make-face-italic face)
	(make-face-unitalic face))
    (if on
	(make-face-italic face nil 'noerr)
      (make-face-unitalic face nil 'noerr))))

(defun asn1-standard-defface (elt)
  (eval
   (list 'defface
	 (car elt)
	 (list
	  'quote
	  (list
	   (list
	    '((class color))
	   (asn1-standard-defface-aux
	    '(:foreground :background)
	    (cdr elt)))
	   (list
	    t
	   (asn1-standard-defface-aux
	    '(:stipple :bold :italic :underline)
	    (nthcdr 3 elt)))))
	 (nth 7 elt)
	 :group 'asn1-face)))

(defun asn1-standard-defface-aux (l1 l2)
  (if (null l1)
      '()
    (if (car l2)
	(cons (car l1) (cons (car l2)
			     (asn1-standard-defface-aux (cdr l1) (cdr l2))))
      (asn1-standard-defface-aux (cdr l1) (cdr l2)))))


;;; -----------------------------------------------------------------------
;;;	      Compatibility with previous versions of asn1-mode
;;; -----------------------------------------------------------------------

(defvar asn1-data-save-file
  (concat "~" init-file-user
	  (if asn1-win "/_emacs" "/.emacs"))
  "*Name of the config file. By default :
\".emacs\" under Unix,
\"_emacs\" under Windows.")

(defun asn1-convert ()
  (if asn1-use-default-font-lock-faces
    nil
    (if (> (length (car asn1-data-faces)) 5)
      nil

      ;; conversion of the faces
      (setq
	asn1-data-faces
	(mapcar (lambda (l)
		  (let* ((elt (assq (car l) asn1-data-faces-default))
			  (doc (if elt
				 (nth 7 elt)
				 "")))
		    (if (nth 1 l)
		      (list (car l) (nth 2 l)
			nil nil (nth 3 l) (nth 4 l) nil doc)
		      (list (car l) nil nil
			nil nil nil nil doc))))
	  asn1-data-faces))
      
      (if
	(not (y-or-n-p
	       "You've used an old version of asn1-mode, updating .emacs? "))
	nil
	(message "Updating...")
	(asn1-delete-settings
	  ";; ASN.1 Menu Settings\n"
	  ";; End of ASN.1 Menu Settings\n")      
	(mapcar (lambda (var) (customize-save-variable var (eval var)))
	  '(asn1-data-faces
	     asn1-compile-command
	     asn1-compile-switches
	     asn1-syntax-menu-autoload))))))

(defun asn1-delete-settings (string-start string-end)
  (let ((output-buffer (find-file-noselect
			 (expand-file-name asn1-data-save-file))))
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-min))
      (if (re-search-forward (concat "^" string-start) nil 'move)
	(let ((p (match-beginning 0)))
	  (goto-char p)
	  (or (re-search-forward (concat "^" string-end) nil t)
	    (error "can't find END of saved state in .emacs"))
	  (delete-region p (match-end 0)))
	(save-buffer)))))

(provide 'asn1-mode)
