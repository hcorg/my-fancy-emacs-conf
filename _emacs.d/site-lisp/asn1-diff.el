;;; asn1-conf.el --- Major mode for comparing ASN.1:1997 specifications

;; Copyright (C) 1997-2001  France Telecom R&D, Lannion

;; Authors:      Stephane LEVANT, Olivier DUBUISSON
;; Maintainer:   asn1@rd.francetelecom.fr
;; Informations: http://asn1.elibel.tm.fr/fr/outils/emacs/   (francais)
;;               http://asn1.elibel.tm.fr/en/tools/emacs/    (english)
;; Created:      1999
;; Keywords:     ASN.1, comparison, protocole specifications

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

;; Allow the comparison of two ASN.1 specifications
;;
;; See the manual.

;;; Code:

(require 'asn1-mode)

(defgroup asn1-diff nil
  (asn1-lg
      "Pour comparer deux specifications ASN.1:1997"
    "To compare two ASN.1:1997 specifications")
  :prefix "asn1-diff-"
  :group 'asn1)


;;; -----------------------------------------------------------------------
;;;	 		Variables user can modify
;;; -----------------------------------------------------------------------

(defcustom asn1-diff-command
  asn1-compile-command
  (asn1-lg
      "*Commande utilisee pour comparer deux specifications ASN.1:1997.
Par defaut, utilise la valeur de `asn1-compile-command'.
Ce module est concu pour asnp.
Il est impératif de donner le chemin complet"
    "*Command used for comparing two ASN.1:1997 specifications.
By default, use the value of `asn1-compile-command'.
This module was build to used with asnp.
You must give the complete path.")
  :type 'file
  :group 'asn1-diff)

(defcustom asn1-diff-switches
  asn1-compile-switches
  (asn1-lg
   "*Parametres de la commande de comparaison."
   "*Options to pass to the compare command")
  :type 'string
  :group 'asn1-diff)

(defcustom asn1-diff-use-color t
  (asn1-lg
      "*Si t, Met automatiquement les differences en couleur dans les
modules ASN.1"
    "*If t, put the differences in color at startup")
  :type 'boolean
  :group 'asn1-diff)

(defcustom asn1-diff-ignore-warning-at-startup nil
  (asn1-lg
      "*Si t, ignore tout les \"warnings\" automatiquement"
  "*If t, mark all warnings as ignored at startup.")
  :type 'boolean
  :group 'asn1-diff)

(defcustom asn1-diff-windows-type 0
  (asn1-lg
      "*Fenetrage par defaut :
 (D designe la fenetre ou sont affiches les messages de differences et A et B
  les fenetres ou sont affichees les deux specifications ASN.1)

   standard       horizontal      vertical             frame
   
   ---------      ---------      ---------       ---------
   | A | B |      |   A   |      | A |   |       |   A   |  -------
   |   |   |      |-------|      |   |   |       |       |  |  D  |
   |-------|      |   B   |      |---| D |       |-------|  -------
   |       |      |-------|      |   |   |       |       |
   |   D   |      |   D   |      | B |   |       |   B   |
   ---------      ---------      ---------       ---------
"
    "*Type of windows
 (D is the Diff buffer and A and B are the buffers of the two
  ASN.1 specifications)

   standard       horizontal      vertical             frame
   
   ---------      ---------      ---------       ---------
   | A | B |      |   A   |      | A |   |       |   A   |  -------
   |   |   |      |-------|      |   |   |       |       |  |  D  |
   |-------|      |   B   |      |---| D |       |-------|  -------
   |       |      |-------|      |   |   |       |       |
   |   D   |      |   D   |      | B |   |       |   B   |
   ---------      ---------      ---------       ---------
")
  :type '(choice (item :tag "standard" 0)
		 (item :tag "horizontal" 1)
		 (item :tag "vertical" 2)
		 (item :tag "frame" 3))
  :group 'asn1-diff)

(defcustom asn1-diff-window-height nil
  (asn1-lg
      "*Permet de specifier la hauteur de la fenetre dans laquelle sont
affiches les messages de differences."
    "*Specify the height of the diff window.")
  :type '(choice (integer :tag "Size")
		 (item :tag "Default" nil))
  :group 'asn1-diff)

(defcustom asn1-diff-window-width nil
  (asn1-lg
      "*Permet de specifier la largeur de la fenetre dans laquelle sont
affiches les messages de differences.
Astuce : Si la configuration des fenetres est \"vertical\"
 (voir `asn1-diff-windows-type'), une valeur negative specifie la largeur
de l'autre fenetre (e.g.: -80)."
    "*Specify the width of the diff window.
If the windows configuration is \"vertical\", modify the width of the
other window (e.g.: -80).")
  :type '(choice (integer :tag "Size")
		 (item :tag "Default" nil))
  :group 'asn1-diff)

(defcustom asn1-diff-ask t
  (asn1-lg
      "*Si nil, ne demande pas de confirmation pour quitter ou
pour imprimer."
  "*If nil, don't ask \"Are you sure\" when quitting ASN.1 Diff or
when printing.")
  :type 'boolean
  :group 'asn1-diff)

(defcustom asn1-diff-ask-about-save
  (if (boundp 'compilation-ask-about-save)
      compilation-ask-about-save
    t)
  (asn1-lg
      "*Si t, propose l'enregistrement de tous les tampons avant
de lancer asnp.
Si nil, enregistre tout les tampons sans rien demander."
  "*If not nil, Diff asks which buffers to save before starting asnp.
Otherwise, it saves all modified buffers without asking.")
  :type 'boolean
  :group 'asn1-diff)

(defcustom asn1-diff-highlight t
  (asn1-lg
      "*Si t, Met les messages d'asnp en surbrillance quand la souris
passe au dessus."
    "*If t, highlight the messages of asnp")
  :type 'boolean
  :group 'asn1-diff)

(defcustom asn1-diff-sort-p t
  (asn1-lg
   "*Si t, les messages d'asnp sont tries."
   "*If t, asnp messages are sorted.")
  :type 'boolean
  :group 'asn1-diff)  

(defcustom asn1-diff-text-to-insert " --<*>-- "
  (asn1-lg
      "*texte insere par defaut par `asn1-diff-insert-text'
 (Menu \"Autres Insertions\")"
    "*Text `asn1-diff-insert-text' inserts by default.
 (Menu \"More Insert\")")
  :type 'string
  :group 'asn1-diff)

(defcustom asn1-diff-recenter nil
  (asn1-lg
      "*Si t, quand on clique sur un message de difference avec
le bouton 2 de la souris, le place en haut du tampon."
    "*If t, recenter diff buffer when click on a difference
 (like compile do)")
  :type 'boolean
  :group 'asn1-diff)

(defcustom asn1-diff-truncate-lines nil
  (asn1-lg
      "*Ne coupe pas la fin des lignes si nil
 (modifie `truncate-partial-width-windows')
Appuyer sur \"Rafraichir\" dans le menu apres modification
de cette variable."
    "*Don't truncate line if nil
 (modify `truncate-partial-width-windows' when refresh)")
  :type 'boolean
  :group 'asn1-diff)

(defcustom asn1-diff-buffer-name "*Diff-ASN.1*"
  (asn1-lg
      "*Nom du tampon dans lequel s'execute asnp"
    "*Name of the buffer where asnp is executed")
  :type 'string
  :group 'asn1-diff)

(defcustom asn1-compile-buffer-name "*Compil-ASN.1*"
  (asn1-lg
      "*Nom du tampon dans lequel s'execute asnp"
    "*Name of the buffer where asnp is executed")
  :type 'string
  :group 'asn1-diff)

(defconst asn1-diff-list-of-default-faces
  `((asn1-diff-string-face	"RoyalBlue"	nil	 nil t   nil nil
     ,(asn1-lg "Chaines de caracteres dans les messages d'asnp"
	"String differences in Diff Buffer"))
    (asn1-diff-file-face	"Sienna"	nil	 nil nil   nil nil
     ,(asn1-lg "Nom des fichiers dans les messages d'asnp"
	"Name of files in Diff Buffer"))
    (asn1-diff-mark-diff-face	"red"		nil	 nil t   nil nil
     ,(asn1-lg "Messages marques"
	  "Marked differences in Diff Buffer"))
    (asn1-diff-ignore-diff-face	"grey60"	nil	 nil nil nil nil
     ,(asn1-lg "Messages ignores"
	  "Ignore differences in Diff Buffer"))
    (asn1-diff-mark-face	"black"		"red"	 nil t   nil nil
     ,(asn1-lg "Differences correspondant aux messages marques"
	  "Mark differences"))
    (asn1-diff-ignore-face	nil		nil	 nil nil nil nil
     ,(asn1-lg "Differences correspondant aux messages ignores"
	  "Ignore differences"))
    (asn1-diff-error-1-face	"yellow"	"blue"	 nil nil nil nil
     ,(asn1-lg "Difference dans un seul fichier, correspondant a une erreur"
	  "Difference error in one buffer"))
    (asn1-diff-error-2-face	"white"		"blue"	 nil nil nil nil
     ,(asn1-lg "Difference dans deux fichers, correspondant a une erreur"
	  "Difference error in two buffer"))
    (asn1-diff-warning-1-face	"yellow"	"CornflowerBlue"
				nil nil nil nil
     ,(asn1-lg "Difference dans un seul fichier, correspondant a un warning"
	  "Difference warning in one buffer"))
    (asn1-diff-warning-2-face	"white"		"CornflowerBlue"
				nil nil nil nil
     ,(asn1-lg "Difference dans deux fichier, correspondant a un warning"
	  "Difference warning in two buffer"))
    (asn1-diff-current-face	"black"		"yellow" nil nil nil nil
     ,(asn1-lg "Difference actuellement selectionnee"
	  "Current difference")))
  "List of defaults faces")

(defvar asn1-diff-list-of-faces
  (asn1-copy-list asn1-diff-list-of-default-faces)
  "*A list of elements used by ASN1 Diff to define face
the syntax of an element is :
 (face-name foreground background &optional bold-p italic-p underline-p
documentation)")

(defun asn1-diff-restart-color ()
  (interactive)
  ;(asn1-modify-face-list asn1-diff-list-of-default-faces)
  (cond (asn1-use-standard-custom
	 (mapcar 'asn1-standard-defface asn1-diff-list-of-faces)
	 (mapcar 'asn1-standard-defface asn1-diff-list-of-default-faces))
	(t
	 (asn1-modify-face-list asn1-diff-list-of-faces)
	 (asn1-defface-list asn1-diff-list-of-default-faces))))

(if (featurep 'asn1-diff)
    nil
  (asn1-diff-restart-color))

(defvar asn1-diff-before-diff-hook nil
  "*Hook run when compilation is finished, before looking differences")

(defvar asn1-diff-after-diff-hook nil
  "*Hook run after looking differences")

(defvar asn1-diff-make-windows-hook nil
  "*Hook run after making windows")

(defvar asn1-diff-open-file-hook nil
  "*Hook run after opening an ASN.1 file")


;;; -----------------------------------------------------------------------
;;; 			Variables used by asn1-diff
;;; -----------------------------------------------------------------------

(defvar asn1-diff-success-regexp
  "\\(Taux de similitude entre les deux spécifications\\|\
Similarity rate of the two specifications\\)")

(defvar asn1-diff-real-switches
  `(switches file-A "-cmpwith" file-B)
  "*List of arguments to pass to the command `asn1-diff-command'.
Arguments are strings or the symbols 'file-A, 'file-B or 'switches.
This symbols will be replaced by the names of files or the compare switches,
i.e. the value of `asn1-diff-switches'.
ex:
    '(switches file-A \"-cmpwith\" file-B)")

;; The most important variable
(defvar asn1-diff-list nil
  "List of differences (error and warning)

The list is composed of elements of the form :
 (nbline type mark overlay overlay) ; when there are two files
 (nbline type mark overlay)         ; when there is one file
nbline is the line number in the Diff buffer.
type is the type (error or warning)
mark test if the difference is marked (value : nil, 'mark or 'ignore)
overlay gives buffer and position (see emacs documentation)."
  ;; type and mark can be retrivied using diff buffer
  ;; there are here only for speed up many functions
  ;;
  ;; overlay contain a 'asn1 property which give the line number of error
  ;; in the diff buffer. This property is very important to find
  ;; the overlay with asn1-overlay-at. It allow to select only the overlays
  ;; put by asn1.
  ;;
  ;; This list can be used directly with the diff buffer.
  ;; the car of the list is the last difference message in the diff buffer,
  ;; the last element of the list is the first difference message and so one.
  ;; see `asn1-diff-read-mark' for an example.
  )

(defvar asn1-diff-color-p asn1-diff-use-color
  "t if the differences are colored")

; oss format :
; filename:[moduleName:]line:[position:]
(eval-and-compile
(defconst asn1-diff-regexp
  ;; used by many functions
  (if (string-match "[oO][sS][sS][^/]$" asn1-compile-command)
      "^\\([^:]+\\):\\([a-zA-Z][^:]*:\\)?\\([0-9]+\\):\\(\\([0-9]+\\):\\)?"
    "[(]Fi[a-z]+:\\([^|\n]+\\)[|]Li.. ?\\([^,\n]+\\),col\\.\\([^)\n]+\\)[)]")
  "The regexp to reconize the name of file in an error message")
)


(defconst asn1-diff-regexp-1
  (eval-when-compile
    (cons (concat "^.>[^\n]+" asn1-diff-regexp)
	  (if (string-match "[oO][sS][sS][^/]$" asn1-compile-command)
	      '(1 3 5)
	    '(1 2 3))))
  "List for differences in one file only.
 This list has the form :
 (REGEXP FILE-1 LINE-1 COLUMN-1)
 If REGEXP match, the sub-expression FILE give the file name,
 LINE the line number and COLUMN the column.")

(defconst asn1-diff-regexp-2
  (eval-when-compile
    (list (concat "^.>[^\n]+" asn1-diff-regexp ":[^\n]+" asn1-diff-regexp)
	  1 2 3 4 5 6))
  "List for differences in two files.
 This list has the form :
 (REGEXP FILE-1 LINE-1 COLUMN-1 FILE-2 LINE-2 COLUMN-2)
 If REGEXP match, the sub-expression FILE give the file name,
 LINE the line number and COLUMN the column.")

(defvar asn1-diff-old-regexp
  (concat asn1-diff-regexp "\n"))

(defconst asn1-diff-warning "^.>[a-z \t]*warning"
  "Regexp which match a difference warning.")

(defvar asn1-shell-filename
  (if (not (equal shell-file-name "/bin/sh")) shell-file-name
    (cond ((file-exists-p "/bin/bash") "/bin/bash")
	  ((file-exists-p "/bin/tcsh") "/bin/tcsh")
	  ((file-exists-p "/bin/csh") "/bin/csh")
	  ((file-exists-p "/bin/ksh") "/bin/ksh")
	  ((and (condition-case ()
		    (require 'executable)
		  (error nil))
		(executable-find "bash")) "bash")
	  (t "/bin/sh")))
  "a shell with expansion of ~ (if possible)")

(defvar asn1-diff-current-overlay nil)
;; v2.2
(defvar asn1-diff-diff-overlay nil)
(defvar asn1-diff-insertion nil)

(defvar asn1-diff-list-of-overlay nil
  "The list of all the overlay in all ASN.1 buffers.
This list is not necessary (`asn1-diff-list' can be used)
but is usefull and is a faster solution.")

;; v2.2
(defvar asn1-diff-is-compile)

(defvar asn1-diff-file-A nil)
(defvar asn1-diff-file-B nil)

(defvar asn1-diff-list-of-buffers nil
  "An a-list to find the buffer-name with the filename.
 This allow to have all the ASN.1 buffers of the specifications which
 are compared.")

;; List used to have the new specification on the left and the old
;; on the right
(defvar asn1-diff-list-of-files nil
  "A list of files of the old specification")

(defvar asn1-diff-buffer nil)
(defvar asn1-diff-window-A nil)
(defvar asn1-diff-window-B nil)
(defvar asn1-diff-window-diff nil)
(defvar asn1-diff-window-configuration nil)
(defvar asn1-diff-frame nil)
(defvar asn1-diff-frame-diff nil)

(defvar asn1-diff-current nil
  "nil if there is no comparison")

(defconst asn1-diff-space-keywords
  (eval-when-compile
    (defun asn1-diff-put-off-space (a-list)
      "retires all word without space in a list"
      (if (null a-list)
	  '()
	(if (string-match "\\(\\sw+\\) \\(\\sw+\\)" (car (car a-list)))
	    (cons
	     (concat (match-string 1 (car (car a-list)))
		     "[ \t\n]+"
		     (match-string 2 (car (car a-list))))
	     (asn1-diff-put-off-space (cdr a-list)))
	  (asn1-diff-put-off-space (cdr a-list)))))
    ;; allow more recursion
    ;; the default values are too small
    (let ((max-lisp-eval-depth 2000)
	  (max-specpdl-size 2000))
      (sort (asn1-diff-put-off-space asn1-data-completion-prelist)
	    'string<)))
  "List of keywords with space. see `asn1-diff-skip'")

(defconst asn1-diff-skip "-&A-Za-z0-9"
  "When asnp gives the column of the difference,
the char forward and backward are put in color.
The only exception are given by `asn1-diff-space-keywords'")

(defvar asn1-diff-list-of-error
  '((no-more-diff "Pas d'autres messages" "No more differences")
    (no-diff-here "Pas de difference ici" "No difference here")
    (killed-buffer
     "Certains tampons ont ete detruits. lancer \"Relire les Messages\""
     "you've killed some buffer. start \"Reread Diff Buffer\"")
    )
  "List of common error messages.")

(defvar asn1-diff-font-lock-keywords
  (eval-when-compile
    (list
     '("^\\*>.*" 0 asn1-diff-mark-diff-face t)
     '("^%>.*" 0 asn1-diff-ignore-diff-face t)
     '("\"[^\"]+\"" 0 asn1-diff-string-face t)
;     '("\\<\\(error\\|warning\\|erreur\\)" . font-lock-type-face)
     (list asn1-diff-regexp
	   (list 1 'asn1-diff-file-face))
     ))
  "List of keywords used by font-lock")

(defvar asn1-diff-mode-map
  (let ((map (make-keymap)))
    ;;(suppress-keymap map) ; pb with prefix keys
    (define-key map "\^c\^d"	'asn1-diff)
    (define-key map "am"	'asn1-diff-mark-all)
    (define-key map "au"	'asn1-diff-unmark-all)
    (define-key map "ad"	'asn1-diff-ignore-all)
    (define-key map "ai"	'asn1-diff-ignore-all)
    (define-key map "aw"	'asn1-diff-ignore-all-warning)
    (define-key map "wq"	'asn1-diff-exit)
    (define-key map "wr"	'asn1-diff-make-windows)
    (define-key map "ws"	'asn1-diff-show-window)
    (define-key map "RM"	'asn1-diff-insert-messages-mark)
    (define-key map "Rm"	'asn1-diff-insert-messages)
    (define-key map "Rt"	'asn1-diff-insert-text)
    (define-key map "RT"	'asn1-diff-insert-text-mark)
    (define-key map "Ru"	'asn1-diff-undo-insert)
    (define-key map "s"		'asn1-diff-save-all-buffers)
    (define-key map "S"		'asn1-diff-reread)
    (define-key map "p"		'asn1-diff-print-all-buffers)
    (define-key map "r"		'asn1-diff-insert-messages)
    (define-key map "n"		'asn1-diff)
    (define-key map "m"		'asn1-diff-mark)
    (define-key map "u"		'asn1-diff-unmark)
    (define-key map "d"		'asn1-diff-ignore)
    (define-key map "i"		'asn1-diff-ignore)
    (define-key map "\r"	'asn1-diff-goto-message-or-difference)
    (define-key map "l"		'asn1-diff-refresh)
    (define-key map "!"		'asn1-diff-restart)
    (define-key map "c"		'asn1-diff-switch-color)
    (define-key map "o"		'asn1-diff-color-only-marked)
    (define-key map "n"		'asn1-diff-goto-next-diff)
    (define-key map " "		'asn1-diff-goto-next-diff)
    (define-key map "p"		'asn1-diff-goto-previous-diff)
    (define-key map [delete]	'asn1-diff-goto-previous-diff)
    (define-key map [(backspace)] 'asn1-diff-goto-previous-diff)
    
    (define-key map "N"		'asn1-diff-goto-next-all-diff)
    (define-key map "P"		'asn1-diff-goto-previous-all-diff)
    (define-key map "z"		'asn1-diff-exit)
    (define-key map "e"		'asn1-diff-exit)
    (define-key map "q"		'asn1-diff-quit)
    (define-key map "h"		'asn1-diff-info)
    (define-key map "?"		'asn1-diff-help)
    (define-key map [?\C-/]	'asn1-diff-undo)
    (define-key map "\^_"	'asn1-diff-undo)
    (define-key map "V"		'asn1-diff-scroll-down)
    (define-key map "v"		'asn1-diff-scroll-up)
    (define-key map "P"		'asn1-set-compare-command)
    (define-key map "O"		'asn1-set-compare-switches)
    (define-key map (asn1-mice-button-key asn1-mice-button)
      'asn1-diff-mouse-goto-diff)
    map)
  "keymap for Diff buffer")


;; Modify asn1-data-mode-map
;; If use a new keymap for ASN.1 Buffers,
;; The menus ASN.1 and ASN.1-Syntax will need to be re-add
;; (in Emacs, there is no problem with XEmacs)
(defvar asn1-diff-mode-asn1-map
  (let ((map (asn1-copy-keymap asn1-data-mode-map)))
    (define-key map "\^cb" 'asn1-goto-diff-buffer)  ; different
    (define-key map "\^cam"	'asn1-diff-mark-all)
    (define-key map "\^cau"	'asn1-diff-unmark-all)
    (define-key map "\^cad"	'asn1-diff-ignore-all)
    (define-key map "\^cai"	'asn1-diff-ignore-all)
    (define-key map "\^caw"	'asn1-diff-ignore-all-warning)
    (define-key map "\^cwd"	'asn1-diff-exit)
    (define-key map "\^cwr"	'asn1-diff-make-windows)
    (define-key map "\^cws"	'asn1-diff-show-window)
    (define-key map "\^cwq"	'asn1-diff-exit)
    (define-key map "\^cRM"	'asn1-diff-insert-messages-mark)
    (define-key map "\^cRm"	'asn1-diff-insert-messages)
    (define-key map "\^cRt"	'asn1-diff-insert-text)
    (define-key map "\^cRT"	'asn1-diff-insert-text-mark)
    (define-key map "\^cRu"	'asn1-diff-undo-insert)
    (define-key map "\^cs"	'asn1-diff-save-all-buffers)
    (define-key map "\^cS"	'asn1-diff-reread)
    (define-key map "\^cp"	'asn1-diff-print-all-buffers)
    (define-key map "\^cr"	'asn1-diff-insert-messages)
    (define-key map "\^cn"	'asn1-diff)
    (define-key map "\^cm"	'asn1-diff-mark)
    (define-key map "\^cu"	'asn1-diff-unmark)
    (define-key map "\^ci"	'asn1-diff-ignore)
    (define-key map "\^c\r"	'asn1-diff-goto-message-or-difference)

    (define-key map "\^cl"	'asn1-diff-refresh)

    (define-key map "\^c!"	'asn1-diff-restart)
    (define-key map "\^cc"	'asn1-diff-switch-color)
    (define-key map "\^co"	'asn1-diff-color-only-marked)
    (define-key map "\^cn"	'asn1-diff-goto-next-diff)
    (define-key map "\^c "	'asn1-diff-goto-next-diff)
    (define-key map "\^cp"	'asn1-diff-goto-previous-diff)
    (define-key map "\^cN"	'asn1-diff-goto-next-all-diff)
    (define-key map "\^cP"	'asn1-diff-goto-previous-all-diff)
    (define-key map [?\C-c delete]	'asn1-diff-goto-previous-diff)
    (define-key map "\^cz"	'asn1-diff-exit)
    (define-key map "\^ce"	'asn1-diff-exit)
    (define-key map "\^cq"	'asn1-diff-quit)
    (define-key map "\^ch"	'asn1-diff-info)
    (define-key map "\^c?"	'asn1-diff-help)
    (define-key map [?\C-c ?\C-/]	'asn1-diff-undo)
    (define-key map "\^c\^_"	'asn1-diff-undo)
    (define-key map "\^cV"	'asn1-diff-scroll-down)
    (define-key map "\^cv"	'asn1-diff-scroll-up)
    (define-key map "\^cP"	'asn1-set-compare-command)
    (define-key map "\^cO"	'asn1-set-compare-switches)
    (define-key map (asn1-mice-button-key asn1-mice-button)
      'asn1-diff-mouse-goto-message)
  map)
  "keymap for ASN.1 buffers while executing diff")


(defvar asn1-diff-menu nil
  "The ASN.1 Diff menu, used in ASN.1 buffers and diff buffer")

(setq asn1-diff-menu
  (list
   "ASN.1-Diff"

   (vector (asn1-lg "Montrer Message/Difference" "Show Message/Difference")
	   'asn1-diff-goto-message-or-difference '(asn1-diff-difference-p))
;    (vector (asn1-lg "Rafraichir" "Refresh")
; 	   'asn1-diff-refresh t)
   (vector (asn1-lg "Couleurs On/Off" "Turn On/Off Diff Colors")
	   'asn1-diff-switch-color t)
   (vector (asn1-lg "Colorier Uniquement les Differences Marquees"
	     "Color Only Marked Differences")
	   'asn1-diff-color-only-marked t)
   ["--------" (lambda nil nil) nil]
   
   (vector (asn1-lg "Marquer" "Mark Difference")
	   'asn1-diff-mark '(asn1-diff-difference-p))
   (vector (asn1-lg "Supprimer Marque" "Unmark Difference")
	   'asn1-diff-unmark '(asn1-diff-difference-p))
   (vector (asn1-lg "Ignorer" "Ignore Difference")
	   'asn1-diff-ignore '(asn1-diff-difference-p))
   (list
    (asn1-lg "Marquage global" "Mark More")
    (vector (asn1-lg "Ignorer Tous les \"Warnings\""
	      "Ignore all Warnings")
	    'asn1-diff-ignore-all-warning t)
    (vector (asn1-lg "Marquer Tout" "Mark all Differences")
	    'asn1-diff-mark-all t)
    (vector (asn1-lg "Supprimer Toutes les Marques"
	      "Unmark all Differences")
	    'asn1-diff-unmark-all t)
    (vector (asn1-lg "Ignorer Tout" "Ignore all Differences")
	    'asn1-diff-ignore-all t))
   ["---------" (lambda nil nil) nil]
   
   (vector (asn1-lg "Enregistrer tous les fichiers ASN.1"
	     "Save all ASN.1 buffers")
	   'asn1-diff-save-all-buffers t)
   (vector (asn1-lg "Imprimer tous les fichiers ASN.1"
	     "Print all ASN.1 buffers")
	   'asn1-diff-print-all-buffers t)
   ["----------" (lambda nil nil) nil]
   
   (vector (asn1-lg "Inserer les Messages dans les Modules"
	     "Insert Messages in Modules")
	   'asn1-diff-insert-messages t)
   (vector (asn1-lg "Annuler la Precedente Insertion"
	     "Undo Last Insert")
	   'asn1-diff-undo-insert 'asn1-diff-insertion)
   (list
    (asn1-lg "Autres Insertions" "Insert More")
    (vector (asn1-lg "Inserer Seulement les Messages Marques"
	      "Insert only Marked Messages")
	    'asn1-diff-insert-messages-mark t)
    (vector (asn1-lg "Reperer les Differences par un Symbole..."
		     "Spot Differences with a Symbol...")
	      'asn1-diff-insert-text t)
    (vector (asn1-lg "Reperer les Differences Marquees par un Symbole..."
		     "Repere Marked Differences with a Symbol...")
	    'asn1-diff-insert-text-mark t))
   ["-----------" (lambda nil nil) nil]
   
   (vector (asn1-lg "Nouvelle Comparaison..." "New Diff...")
	   '(if asn1-diff-is-compile
		(asn1-diff)
	      (asn1-compile)) t)
   (vector (asn1-lg "Relancer la Comparaison" "Restart Diff")
	   'asn1-diff-restart t)
   (vector (asn1-lg "Relire les Messages" "Reread Diff Buffer")
	   'asn1-diff-reread t)
   (vector (asn1-lg "Restaurer le Fenetrage" "Restore Windows")
	   'asn1-diff-show-window t)
   (vector (asn1-lg "Reorganiser le Fenetrage" "Restart Windows")
	   'asn1-diff-make-windows t)
   ["------------" (lambda nil nil) nil]
   
   (list
    (asn1-lg "Personnalisation Rapide" "Fast Customization")
    (vector (asn1-lg "Colorisation Automatique des Differences"
	      "Color Diff at Startup")
	    '(customize-save-variable 'asn1-diff-use-color t)
	    '(not asn1-diff-use-color))
    (vector (asn1-lg "Pas de Colorisation Automatique des Differences"
	      "Don't Color Diff at Startup")
	    '(customize-save-variable 'asn1-diff-use-color nil)
	    'asn1-diff-use-color)
    (vector (asn1-lg "Commande de Comparaison..."
	      "Compare Command...")
	    'asn1-set-compare-command t)
    (vector (asn1-lg "Parametres de Comparaison..." "Compare Switches...")
	    'asn1-set-compare-switches t)
    (vector (asn1-lg
		"Sauvegarder les Informations de Comparaison"
		"Save Comparison Informations")
	    '(progn
	       (customize-save-variable 'asn1-diff-command
					asn1-diff-command)
	       (customize-save-variable 'asn1-diff-switches
					asn1-diff-switches)) t))
   (list
    (asn1-lg "Personnalisation" "Customization")
    (vector (asn1-lg "Styles d'Ecriture et Couleurs..."
	      "Colors/Faces...")
	    'asn1-custom-diff-face t)
    (vector (asn1-lg "Options Generales..."
	      "General Options...")
	    'asn1-custom-diff-options t)    
    (vector (asn1-lg "Options d'Asnp..." "Asnp Options...")
	    'asn1-custom-asnp-options nil))
   ["-------------" (lambda nil nil) nil]
   (vector (asn1-lg "Aide" "Help")
	   'asn1-diff-info t)
   (vector (asn1-lg "Aide Rapide" "Quick Help")
	   'asn1-diff-help t)   
   ["--------------" (lambda nil nil) nil]
   (vector (asn1-lg "Quitter Momentanement" "Exit Temporarily")
	   'asn1-diff-exit t)
   (vector (asn1-lg "Quitter" "Quit")
	   'asn1-diff-quit t)
   ))


(defvar asn1-compile-menu nil
  "The ASN.1 Compile menu, used in ASN.1 buffers and compile buffer")

(setq asn1-compile-menu
  (list
   "ASN.1-Diff"

   (vector (asn1-lg "Montrer Message/Erreur" "Show Message/Error")
	   'asn1-diff-goto-message-or-difference '(asn1-diff-difference-p))
;    (vector (asn1-lg "Rafraichir" "Refresh")
; 	   'asn1-diff-refresh t)
   (vector (asn1-lg "Couleurs On/Off" "Turn On/Off Diff Colors")
	   'asn1-diff-switch-color t)
   (vector (asn1-lg "Colorier Uniquement les Erreurs Marquees"
	     "Color Only Marked Errors")
	   'asn1-diff-color-only-marked t)
   ["--------" (lambda nil nil) nil]
   
   (vector (asn1-lg "Marquer" "Mark Error")
	   'asn1-diff-mark '(asn1-diff-difference-p))
   (vector (asn1-lg "Supprimer Marque" "Unmark Error")
	   'asn1-diff-unmark '(asn1-diff-difference-p))
   (vector (asn1-lg "Ignorer" "Ignore Error")
	   'asn1-diff-ignore '(asn1-diff-difference-p))
   (list
    (asn1-lg "Marquage global" "Mark More")
    (vector (asn1-lg "Ignorer Tous les \"Warnings\""
	      "Ignore all Warnings")
	    'asn1-diff-ignore-all-warning t)
    (vector (asn1-lg "Marquer Tout" "Mark all Errors")
	    'asn1-diff-mark-all t)
    (vector (asn1-lg "Supprimer Toutes les Marques"
	      "Unmark all Errors")
	    'asn1-diff-unmark-all t)
    (vector (asn1-lg "Ignorer Tout" "Ignore all Errors")
	    'asn1-diff-ignore-all t))
   ["---------" (lambda nil nil) nil]
   
   (vector (asn1-lg "Enregistrer tous les fichiers ASN.1"
	     "Save all ASN.1 buffers")
	   'asn1-diff-save-all-buffers t)
   (vector (asn1-lg "Imprimer tous les fichiers ASN.1"
	     "Print all ASN.1 buffers")
	   'asn1-diff-print-all-buffers t)
   ["----------" (lambda nil nil) nil]
   
   (vector (asn1-lg "Inserer les Messages dans les Modules"
	     "Insert Messages in Modules")
	   'asn1-diff-insert-messages t)
   (vector (asn1-lg "Annuler la Precedente Insertion"
	     "Undo Last Insert")
	   'asn1-diff-undo-insert 'asn1-diff-insertion)
   (list
    (asn1-lg "Autres Insertions" "Insert More")
    (vector (asn1-lg "Inserer Seulement les Messages Marques"
	      "Insert only Marked Messages")
	    'asn1-diff-insert-messages-mark t)
    (vector (asn1-lg "Reperer les Erreurs par un Symbole..."
		     "Spot Errors with a Symbol...")
	      'asn1-diff-insert-text t)
    (vector (asn1-lg "Reperer les Erreurs Marquees par un Symbole..."
		     "Repere Marked Errors with a Symbol...")
	    'asn1-diff-insert-text-mark t))
   ["-----------" (lambda nil nil) nil]
   
   (vector (asn1-lg "Nouvelle Compilation..." "New Compilation...")
	   '(if asn1-diff-is-compile
		(asn1-diff)
	      (asn1-compile)) t)
   (vector (asn1-lg "Relancer la Compilation" "Restart Compilation")
	   'asn1-diff-restart t)
   (vector (asn1-lg "Relire les Messages" "Reread Compil Buffer")
	   'asn1-diff-reread t)
   (vector (asn1-lg "Restaurer le Fenetrage" "Restore Windows")
	   'asn1-diff-show-window t)
   (vector (asn1-lg "Reorganiser le Fenetrage" "Restart Windows")
	   'asn1-diff-make-windows t)
   ["------------" (lambda nil nil) nil]
   
   (list
    (asn1-lg "Personnalisation Rapide" "Fast Customization")
    (vector (asn1-lg "Colorisation Automatique des Differences"
	      "Color Diff at Startup")
	    '(customize-save-variable 'asn1-diff-use-color t)
	    '(not asn1-diff-use-color))
    (vector (asn1-lg "Pas de Colorisation Automatique des Erreurs"
	      "Don't Color at Startup")
	    '(customize-save-variable 'asn1-diff-use-color nil)
	    'asn1-diff-use-color)
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
	    'asn1-custom-diff-face t)
    (vector (asn1-lg "Options Generales..."
	      "General Options...")
	    'asn1-custom-diff-options t)    
    (vector (asn1-lg "Options d'Asnp..." "Asnp Options...")
	    'asn1-custom-asnp-options nil))
   ["-------------" (lambda nil nil) nil]
   (vector (asn1-lg "Aide" "Help")
	   'asn1-diff-info t)
   (vector (asn1-lg "Aide Rapide" "Quick Help")
	   'asn1-diff-help t)   
   ["--------------" (lambda nil nil) nil]
   (vector (asn1-lg "Quitter Momentanement" "Exit Temporarily")
	   'asn1-diff-exit t)
   (vector (asn1-lg "Quitter" "Quit")
	   'asn1-diff-quit t)
   ))



;;; -----------------------------------------------------------------------
;;;			The main functions
;;; -----------------------------------------------------------------------

;;;###autoload
(defun asn1-diff ()
  "Display the differences between two ASN.1 specifications
Uses `asn1-diff-commande', which must be asnp
For more informations, see the documentation (\"Info\" in the ASN.1 menu)"
  (interactive)
  (let (file-A file-B)
    (setq file-A
	  ;; put a ~ don't erase the line...
	   (asn1-read-shell-command
	    (asn1-lg "Nouveau(x) fichier(s) : "
	      "Diff new file(s) : ") default-directory))
    (setq file-B
	   (asn1-read-shell-command
	    (asn1-lg
		"Ancien(s) fichier(s) : "
	      "Diff old File(s) : ") default-directory))
    (asn1-diff-noninteractive file-A file-B)))

;; v2.2
;;;###autoload
(defun asn1-diff-compile (cmd)
  "Compilation with ASN.1-Diff module"
  (setq asn1-diff-current nil)
  (setq asn1-diff-file-A cmd)
  (setq asn1-diff-file-B (current-buffer))
  (setq asn1-diff-is-compile t)
  (asn1-diff-prepare-buffer)
  (let* ((process-environment (cons "EMACS=t" process-environment))
	 (shell-file-name asn1-shell-filename)
	 (proc (apply 'start-process-shell-command
		      "asnp" asn1-diff-buffer (list cmd))))
    (set-process-sentinel proc 'asn1-diff-sentinel)
    (set-process-filter proc 'asn1-diff-filter)
    (setq mode-line-process ":run")
    (force-mode-line-update)))

(defun asn1-diff-prepare-buffer ()
  ;; buffer diff is prepared
  (setq asn1-diff-buffer
	(if (buffer-live-p asn1-diff-buffer)
	    asn1-diff-buffer
	  (get-buffer-create
	    (if asn1-diff-is-compile
	      asn1-compile-buffer-name
	      asn1-diff-buffer-name))))
  (switch-to-buffer asn1-diff-buffer)
  (setq buffer-read-only nil)
  (buffer-disable-undo)
  (erase-buffer)
  (setq buffer-read-only t)

  ;; if the last compilation isn't finished
  (let ((comp-proc (get-buffer-process (current-buffer))))
    (if comp-proc
	(if (or (not (eq (process-status comp-proc) 'run))
		(y-or-n-p (asn1-lg "Un processus tourne deja; Le tuer? "
			    "A process is running; kill it? ")))
	    (condition-case ()
		(progn
		  (interrupt-process comp-proc)
		  (sit-for 1)
		  (delete-process comp-proc))
	      (error nil))
	  (error (asn1-lg "Impossible d'avoir deux processus dans `%s'"
		     "Cannot have two processes in `%s' at once")
		 (buffer-name))))))

;;;###autoload
(defun asn1-diff-noninteractive (file-A file-B)
  "Function called by `asn1-diff'"
  (delete-other-windows)
  (setq asn1-diff-current nil)
  (setq asn1-diff-is-compile nil)

  (if (eq asn1-diff-file-A nil)
      (setq asn1-diff-buffer nil))
  (setq asn1-diff-file-B file-B)
  (setq asn1-diff-file-A file-A)
;   (setq file-A (asn1-diff-expand-file-name file-A))
;   (setq file-B (asn1-diff-expand-file-name file-B))

  (save-some-buffers (not asn1-diff-ask-about-save) nil)

;   (setq asn1-diff-list-of-files nil)
;   (asn1-diff-construct-spe file-B)

  (asn1-diff-prepare-buffer)
  
  ;; start the process
  (let* ((a-liste `((file-A . ,file-A)
		    (file-B . ,file-B)
		    (switches . ,asn1-diff-switches)))
	 (process-environment (cons "EMACS=t" process-environment))
	 (shell-file-name asn1-shell-filename)
	 (proc (apply 'start-process-shell-command
		      "asnp" asn1-diff-buffer asn1-diff-command
		      (asn1-diff-list-arg a-liste asn1-diff-real-switches))))
    (set-process-sentinel proc 'asn1-diff-sentinel)
    (set-process-filter proc 'asn1-diff-filter)
    (setq mode-line-process ":run")
    (force-mode-line-update)))

; (defun asn1-diff-expand-file-name (string)
;   "Expand all the file name in the STRING."
;   (let ((b (get-buffer-create "*ASN.1-file-name*"))
; 	result)
;     (set-buffer b)
;     (erase-buffer)
;     (insert string)
;     (goto-char (point-min))
;     (while (search-forward-regexp
; 	    "\\(^\\|[ \t]\\)\\(~[^/ \t]*\\([ \t/]\\|$\\)\\)" nil t)
;       (replace-match (concat (match-string 1)
; 			     (expand-file-name (match-string 2)))))
;     (setq result (buffer-string))
;     (kill-this-buffer)
;     result))

; (defun asn1-diff-construct-spe (string)
;   (let ((b (get-buffer-create "*ASN.1-old-files-spe*"))
; 	(process-environment (cons "EMACS=t" process-environment))
; 	(shell-file-name asn1-shell-filename)
; 	proc)
;     (set-buffer b)
;     (erase-buffer)
;     (setq proc (start-process-shell-command
; 		"proc" b (concat "ls -1 -d " string)))
;     (set-process-sentinel proc 'asn1-diff-spe-sentinel)))

; (defun asn1-diff-spe-sentinel (proc msg)
;   "The sentinel function for asnp process,
; Called when process changes state."
;   (let ((buffer (process-buffer proc)))
;     (if (memq (process-status proc) '(signal exit))
; 	(progn
; 	  (if (null (buffer-name buffer))
; 	      ;; buffer killed
; 	      (set-process-buffer proc nil)
; 	    (let ((obuf (current-buffer)))
; 	      ;; save-excursion isn't the right thing if
; 	      ;; process-buffer is current-buffer
; 	      (unwind-protect
; 		  (progn
; 		    ;; Write something in the compilation buffer
; 		    ;; and hack its mode line.
; 		    (set-buffer buffer)
; 		    (asn1-diff-spe-change-status (process-status proc)
; 					     (process-exit-status proc)
; 					     msg)
; 		    ;; Since the buffer and mode line will show that the
; 		    ;; process is dead, we can delete it now.  Otherwise it
; 		    ;; will stay around until M-x list-processes.
; 		    (delete-process proc))
; 		(if (buffer-live-p obuf)
; 		    (set-buffer obuf)))))))))

; (defun asn1-diff-spe-change-status (process-status status msg)
;   (if (not (eq status 0)) nil
;     (goto-char (point-min))
;     (while (not (eobp))
;       (beginning-of-line)
;       (let ((pt (point))
; 	    file)
; 	(end-of-line)
; 	(setq file (asn1-substring pt (point)))
; 	(setq asn1-diff-list-of-files	    
; 	      (cons (file-truename
; 		     (if (eq (string-to-char file) ?/)
; 			 file
; 		     (concat default-directory file)))
; 		    asn1-diff-list-of-files)))
;       (next-line 1)))
;   (kill-this-buffer))

(defun asn1-diff-list-arg (a-list list-arg)
  "Replace some elements of LIST-ARG by using A-LIST
ex : '((file-A . \"toto\")) -> file-A is replaced by \"toto\""
  (if (null list-arg)
      '()
    (let ((elt (assq (car list-arg) a-list)))
      (cons (if (null elt)
		(car list-arg)
	      (cdr elt))
	    (asn1-diff-list-arg a-list (cdr list-arg))))))

(defun asn1-diff-restart ()
  "Restart the comparison"
  (interactive)
  (if (null asn1-diff-current)
      (error (if asn1-diff-is-compile
	       (asn1-lg "Pas de compilation a relancer"
		 "No compilation to restart")
	       (asn1-lg "Pas de comparaison a relancer"
		 "No comparison to restart"))))
  (if asn1-diff-is-compile
    (if asn1-diff-file-A
      (asn1-diff-compile asn1-diff-file-A)
      (asn1-compile))
    (if (and asn1-diff-file-A asn1-diff-file-B)
      (asn1-diff-noninteractive asn1-diff-file-A asn1-diff-file-B)
      (asn1-diff))))

(defun asn1-diff-exit ()
  "Simply delete the windows and save windows configuration"
  (interactive)
  (asn1-diff-save-config)
  (delete-other-windows)
  (let ((l asn1-diff-list-of-buffers))
    (while l
      (if (buffer-live-p (cdr (car l)))
	  (bury-buffer (cdr (car l))))
      (setq l (cdr l))))
  (if (buffer-live-p asn1-diff-buffer)
      (bury-buffer asn1-diff-buffer))
  (bury-buffer (get-buffer-create "*scratch*"))
  (switch-to-buffer (other-buffer)))

(defun asn1-diff-save-config ()
  (if (and
       (window-live-p asn1-diff-window-A)
       (window-live-p asn1-diff-window-B)
       (window-live-p asn1-diff-window-diff)
       (frame-live-p asn1-diff-frame))
      (setq asn1-diff-window-configuration
 	    (current-window-configuration asn1-diff-frame))))

(defun asn1-diff-quit ()
  "Quit diff.
Delete the color in the ASN.1 buffers."
  (interactive)
  (cond ((if asn1-diff-ask
	     (y-or-n-p (if asn1-diff-is-compile
			 (asn1-lg
			   "Etes-vous sur de vouloir quitter ASN.1 Compil ? "
			   "Are you sure you want to quit ASN.1 Compil ? ")
			 (asn1-lg
			   "Etes-vous sur de vouloir quitter ASN.1 Diff ? "
			   "Are you sure you want to quit ASN.1 Diff ? ")
			 ))
	   t)
	 (asn1-diff-init)
	 (asn1-diff-exit)
	 (setq asn1-diff-current nil))))

(defun asn1-diff-init ()
  (let ((l asn1-diff-list-of-buffers)
	b)
    (while l
      (setq b (cdr (car l)))
      (if (not (buffer-live-p b)) nil
	(set-buffer b)
	(easy-menu-remove asn1-diff-menu) ; xemacs
	(use-local-map asn1-data-mode-map)
	(setq asn1-data-diff-menu-p nil))
      (setq l (cdr l))))
  (setq asn1-diff-insertion nil)
  (setq asn1-diff-list nil)
  (asn1-diff-delete-overlay)
  (setq asn1-diff-list-of-buffers nil))



;;; -----------------------------------------------------------------------
;;;		    Creation of the list of differences
;;; -----------------------------------------------------------------------

(defun asn1-diff-filter (proc string)
  "Process filter for compilation buffers."
  (let ((b (current-buffer)))
    (set-buffer asn1-diff-buffer)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (insert string))
    (set-buffer b)))

;; inspired from "compile.el"
(defun asn1-diff-sentinel (proc msg)
  "The sentinel function for asnp process,
Called when process changes state."
  (let ((buffer (process-buffer proc)))
    (if (memq (process-status proc) '(signal exit))
	(progn
	  (if (null (buffer-name buffer))
	      ;; buffer killed
	      (set-process-buffer proc nil)
	    (let ((obuf (current-buffer)))
	      ;; save-excursion isn't the right thing if
	      ;; process-buffer is current-buffer
	      (unwind-protect
		  (progn
		    ;; Write something in the compilation buffer
		    ;; and hack its mode line.
		    (set-buffer buffer)
		    (asn1-diff-change-status (process-status proc)
					     (process-exit-status proc)
					     msg)
		    ;; Since the buffer and mode line will show that the
		    ;; process is dead, we can delete it now.  Otherwise it
		    ;; will stay around until M-x list-processes.
		    (delete-process proc))
		(set-buffer obuf))))))))

(defun asn1-diff-change-status (process-status status msg)
  "Prepare the diff buffer"
  (setq buffer-read-only nil)
  (setq mode-line-process (format ":%s [%s]" process-status status))
  (force-mode-line-update)
  (goto-char (point-max))
  (insert "\n")
  (setq major-mode 'dont-erase)
  (asn1-diff-mode asn1-diff-is-compile))

(defun asn1-diff-reread ()
  (interactive)
  (asn1-diff-verif nil)
  (if (not (eq major-mode 'asn1-diff))
      (set-buffer asn1-diff-buffer)
    (cond ((not (eq (current-buffer) asn1-diff-buffer))
	   (setq asn1-diff-file-A nil)
	   (setq asn1-diff-file-B nil))))
  (setq major-mode 'dont-erase)
  (asn1-diff-mode asn1-diff-is-compile))

(defun asn1-diff-mode2 ()
  "Major mode for the .asnd files"
  (interactive)
  (asn1-diff-mode nil))

(defun asn1-diff-mode (&optional compil)
  "Major mode for the diff buffer"
  (interactive "p")

  (setq asn1-diff-is-compile compil)
  (let ((old major-mode))
    
  (if (not (or (eq major-mode 'dont-erase)
	       (eq major-mode 'asn1-diff)))
      (progn (setq asn1-diff-file-A nil)
	     (setq asn1-diff-file-B nil)))
  
  (if (eq major-mode 'asn1-diff) nil
    
  ;; prepare buffer
  (setq buffer-read-only nil)
  (buffer-disable-undo)
  (setq asn1-diff-buffer (current-buffer))
  (run-hooks 'asn1-diff-before-diff-hook)
  (asn1-diff-look-old)
  (asn1-diff-construct-list)
  (asn1-diff-read-mark)
  
  (if (eq (selected-window)
	  (active-minibuffer-window))
      (other-window 1))
  (switch-to-buffer asn1-diff-buffer)
  
  ;; the mode
  (kill-all-local-variables)
  (setq mode-name "Diff ASN.1")
  (setq major-mode 'asn1-diff)
  ;(setq mode-line-process '(":%s"))
  (use-local-map asn1-diff-mode-map)
  (asn1-diff-add-menu (current-local-map))
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(asn1-diff-font-lock-keywords nil nil nil))

  (goto-char (point-min))
  (if (search-forward-regexp asn1-diff-success-regexp nil t) nil
    (if (not asn1-diff-is-compile)
      (message (asn1-lg
		 "Erreur(s) syntaxique(s) : pas de comparaison effectuee"
		 "Syntax error(s) : no comparaison done"))
      ))
  (goto-char (point-min))
  (run-hooks 'asn1-diff-after-diff-hook)
  (setq buffer-read-only t)
  (buffer-enable-undo)

  (setq asn1-diff-current t)
  (asn1-diff-show-window)

  (set-buffer asn1-diff-buffer)
	(search-forward-regexp "^.>" nil t)
	(beginning-of-line)
  (discard-input)

  ; not standard
;   (if (assq 'font-lock-mode minor-mode-alist)
;        (font-lock-fontify-buffer))

;; what ???
;     (cond ((and (not asn1-data-xemacs-p) (where-is-internal 'backward-char))
; 	    (forward-char 1)
; 	    (setq unread-command-events
; 	      (cons 
; 		(listify-key-sequence
; 		  (car (where-is-internal 'backward-char)))
; 		unread-command-events))))
    (if (not (eq old 'dont-erase))
      (set-buffer-modified-p nil))
  )))

(defun asn1-diff-look-old ()
  (setq asn1-diff-list-of-files nil)
  (goto-char (point-min))
  (message (asn1-lg "Analyse des messages..."
		    "Reading messages..."))
  (let (file)
    (while (search-forward-regexp asn1-diff-old-regexp nil t)
      (setq file (match-string 1))
      (if (not (member file asn1-diff-list-of-files))
	  (setq asn1-diff-list-of-files
		(cons file
		      (cons (file-truename file)
			    asn1-diff-list-of-files)))))))

(defun asn1-diff-sort-predicate (string1 string2)
  (let*
      ((m1 (string-match asn1-diff-regexp string1))
       (f1 (if m1 (match-string 1 string1)))
       (l1 (if m1 (match-string 2 string1)))
       (c1 (if m1 (match-string 3 string1)))
       (m2 (string-match asn1-diff-regexp string2))
       (f2 (if m2 (match-string 1 string2)))
       (l2 (if m2 (match-string 2 string2)))
       (c2 (if m2 (match-string 3 string2)))
       (m3 (if m1 (string-match asn1-diff-regexp string1 (1+ m1))))
       (f3 (if m3 (match-string 1 string1)))
       (l3 (if m3 (match-string 2 string1)))
       (c3 (if m3 (match-string 3 string1)))
       (m4 (if m2 (string-match asn1-diff-regexp string2 (1+ m2))))
       (f4 (if m4 (match-string 1 string2)))
       (l4 (if m4 (match-string 2 string2)))
       (c4 (if m4 (match-string 3 string2))))
    (cond ((not m1)
	    t)
	  ((not m2)
	   nil)
	  ((and (member f1 asn1-diff-list-of-files)
		(not (member f2 asn1-diff-list-of-files)))
	   t)
	  ((and (member f2 asn1-diff-list-of-files)
		(not (member f1 asn1-diff-list-of-files)))
	   nil)
	  ((string< f1 f2)
	   t)
	  ((not (string= f1 f2))
	   nil)
	  ((< (string-to-number l1) (string-to-number l2))
	   t)
	  ((> (string-to-number l1) (string-to-number l2))
	   nil)
	  ((< (string-to-number c1) (string-to-number c2))
	   t)
	  ((> (string-to-number c1) (string-to-number c2))
	   nil)
	  ((and (null m3) m4)
	   t)
	  ((null m4)
	   nil)
	  ((string< f3 f4)
	   t)
	  ((not (string= f3 f4))
	   nil)
	  ((< (string-to-number l3) (string-to-number l4))
	   t)
	  ((> (string-to-number l3) (string-to-number l4))
	   nil)
	  ((< (string-to-number c3) (string-to-number c4))
	   t)
	  ((> (string-to-number c3) (string-to-number c4))
	   nil)
	  (t
	   nil))))

(defun asn1-diff-sort ()
  "Sort the messages of asnp :
Sort with the old specification
First sort alphabeticaly with the file names
Second, sort with the line numbers
and third, sort with the column numbers"
  (interactive)
  (message (asn1-lg "Tri des messages."
		    "Sorting messages."))
  (goto-char (point-max))
  (search-backward-regexp "^.>" nil t)
  (beginning-of-line)
  (let ((first (point))
	beg
	list)
    (while (looking-at "^.>")
      (setq beg (point))
      (previous-line 1)
      (setq list
	    (cons (asn1-substring beg (point))
		  list)))
    (message (asn1-lg "Tri des messages.."
		      "Sorting messages.."))
    (delete-region first (point))
    (setq list (sort list 'asn1-diff-sort-predicate))
    (message (asn1-lg "Tri des messages..."
		      "Sorting messages..."))
    (while list
      (insert (car list))
      (setq list (cdr list)))))

(defun asn1-diff-construct-list ()
  "Construct the list of differences.
This function put some overlay (extent in XEmacs) in the ASN.1 buffers
This is necessary if the files are modified.

see `asn1-diff-list'."
  ;; reset
  (asn1-diff-init)
  (set-buffer asn1-diff-buffer)
  (setq asn1-diff-color-p asn1-diff-use-color)

  ;; To correct a "bug" of asnp
  (goto-char (point-min))
  (while (search-forward-regexp "(Fi[chierl]+:|Li[g.ne ]+0,col.0)" nil t)
    (replace-match "" t t))
    
  ;; To sort messages
  (if asn1-diff-sort-p
      (asn1-diff-sort))

  (let ((nbl 1)
	(nbl-tot (count-lines (point-min) (point-max)))
	f1 l1 c1 f2 l2 c2 list type)
    (goto-char (point-min))
    (while (not (eobp))
      (message (if asn1-diff-is-compile
		 (asn1-lg "Analyse des erreurs... %3d%%"
		 "Looking errors... %3d%%")
		 (asn1-lg "Analyse des differences... %3d%%"
		 "Looking differences... %3d%%"))
	       (/ (* 100 nbl) nbl-tot))

      (if (not (looking-at (car asn1-diff-regexp-1))) nil

	;; highlight text for the mouse
	(if asn1-diff-highlight
	    (put-text-property (+ 2 (point)) (progn (end-of-line) (point))
			       'mouse-face 'highlight))
	
	(setq f1 (match-string (nth 1 asn1-diff-regexp-1)))
	(setq l1 (string-to-number
		  (match-string (nth 2 asn1-diff-regexp-1))))
	(setq c1 (string-to-number
		  (match-string (nth 3 asn1-diff-regexp-1))))
	(beginning-of-line)

	;; type of difference
	(setq type (if (looking-at asn1-diff-warning) 'warning 'error))
	
	(cond ((looking-at (car asn1-diff-regexp-2))
	       ;; Difference with two buffers
	       (setq f1 (match-string (nth 1 asn1-diff-regexp-1)))
	       (setq l1 (string-to-number
			 (match-string (nth 2 asn1-diff-regexp-1))))
	       (setq c1 (string-to-number
			 (match-string (nth 3 asn1-diff-regexp-1))))	       
	       (setq f2 (match-string (nth 4 asn1-diff-regexp-2)))
	       (setq l2 (string-to-number
			 (match-string (nth 5 asn1-diff-regexp-2))))
	       (setq c2 (string-to-number
			 (match-string (nth 6 asn1-diff-regexp-2))))
	       (setq list
		     (list nbl type nil
			   (list
			    (asn1-diff-make-overlay f1 l1 c1 nbl)
			    (asn1-diff-make-overlay f2 l2 c2 nbl))
			   t)))
	      (t	    
	       ;; Difference with only one buffer
	       (setq list (list nbl type nil
				(list (asn1-diff-make-overlay f1 l1 c1 nbl)
				      nil)
				nil))
	      ))

	
	(if asn1-diff-use-color
	    (asn1-diff-color-overlay list))
	(setq asn1-diff-list (cons list asn1-diff-list)))
      (next-line 1)
      (beginning-of-line)
      (setq nbl (1+ nbl)) ;; Count the number of the line
      ))
  (message
    (if asn1-diff-is-compile
      (asn1-lg
	"Analyse des erreurs...terminee"
	"Looking errors...done")
      (asn1-lg
	"Analyse des differences...terminee"
	"Looking differences...done"))
    ))

;;; access
(defun asn1-diff-get-nb (elt)
  (nth 0 elt))
(defun asn1-diff-get-type (elt)
  (nth 1 elt))
(defun asn1-diff-get-mark (elt)
  (nth 2 elt))
(defun asn1-diff-get-overlays (elt)
  (nth 3 elt))
(defun asn1-diff-get-double (elt)
  (nth 4 elt))

;;; modif
(defun asn1-diff-set-mark (elt mark)
  (setcar (nthcdr 2 elt) mark))


;;; -----------------------------------------------------------------------
;;;			Management of overlays
;;; -----------------------------------------------------------------------

(defun asn1-diff-make-overlay (filename line column nbl)
  "Create and return an overlay in FILENAME, at line LINE and
near column COLUMN
 (see `asn1-diff-skip-keyword' and `asn1-diff-space-keywords')"
  (let ((b (assoc filename asn1-diff-list-of-buffers))
	o deb buf)

    ;; Openning ASN.1 files
    (if (and b (buffer-live-p (cdr b)))
	(set-buffer (cdr b))
      (message (asn1-lg "Ouverture du fichier %s" "Opening file %s")
	       filename)
      (setq buf (get-file-buffer filename))
      (if (null buf) nil
	(set-buffer buf)
	(setq buffer-read-only nil)
	(if (buffer-modified-p buf)
	    (if (not (y-or-n-p
		      (format
		       (asn1-lg "Buffer \"%s\" is modified, kill it ? "
			 "Le tampon \"%s\" est modifie, le tuer ? ")
		       (buffer-name buf))))
		(error (asn1-lg
			   "Tampon modifie, fin de la comparaison"
			 "Buffer modified, exiting diff"))
	      (set-buffer-modified-p nil)
	      (kill-buffer buf))))
      (setq asn1-diff-list-of-buffers
	    (cons (cons filename (find-file filename))
		  asn1-diff-list-of-buffers))
      (use-local-map asn1-diff-mode-asn1-map)
      (cond (asn1-data-syntax-menu-loaded
	     (asn1-menu-add-syntax-menu (current-local-map))
	     ;; utilisation of diff and syntax menu together
	     (define-key (current-local-map)
	       (asn1-mice-button-key asn1-mice-button)
	       'asn1-diff-mouse-goto-message)))
      (asn1-menu-add-menu (current-local-map))
      (asn1-diff-add-menu (current-local-map))
      (run-hooks 'asn1-diff-open-file-hook))

    (let ((pt (point)))
      (goto-line line)
      (beginning-of-line)      
      (condition-case ()
	(forward-char (1- (if (> column 0) column 1)))
	(error nil))  ;; fin du fichier (e.g. : "END" manquant)
;       (if (looking-at (concat "[" asn1-diff-skip "]"))
; 	(progn
	  (skip-chars-backward asn1-diff-skip)
	  (setq deb (point))
	  (asn1-diff-skip-keyword asn1-diff-space-keywords)
; 	  )
; 	(setq deb (point)))
      (if (equal deb (point))
	(condition-case ()
	  (forward-char 1)
	  (error nil)))
      (setq o (make-overlay deb (point)))
      (overlay-put o 'asn1 nbl)	; allow to find nbl with overlay
      (overlay-put o 'mouse-face 'highlight)
      (goto-char pt))
    (set-buffer asn1-diff-buffer)
    (setq asn1-diff-list-of-overlay
      (cons o asn1-diff-list-of-overlay))
    o))

(defun asn1-diff-skip-keyword (l)
  "Skip the space-keyword or the char in front of the cursor.
Used by `asn1-diff-make-overlay'"
  (if l
      (if (looking-at (car l))
	  (search-forward-regexp (car l))
	(asn1-diff-skip-keyword (cdr l)))
    (skip-chars-forward asn1-diff-skip)))

(defun asn1-diff-color-overlay (elt)
  "Color an overlay. Use an element of the `asn1-diff-list'"
  (if (null elt)
      nil
    (let ((o (asn1-diff-get-overlays elt))
	  (face (asn1-diff-choice-color elt)))
      (while o
	(condition-case ()
	    (progn
	      (if (null (car o)) nil
		(overlay-put (car o) 'face face)
		(overlay-put (car o) 'priority
			     (if (face-equal face 'default) 0 1))))
	  (error nil))
	(setq o (cdr o))))))

(defun asn1-diff-choice-color (elt)
  "Return the correct face which will be used for color an overlay
taken form the `asn1-diff-list'"
  (let ((double (asn1-diff-get-double elt)))
    (if asn1-diff-color-p
	(cdr
	 (if (asn1-diff-get-mark elt)
	     (assq (asn1-diff-get-mark elt)
		   '((mark . asn1-diff-mark-face)
		     (ignore . asn1-diff-ignore-face)))
	   (assq (asn1-diff-get-type elt)
		 (if double
		     '((error . asn1-diff-error-2-face)
		       (warning . asn1-diff-warning-2-face))
		   '((error . asn1-diff-error-1-face)
		     (warning . asn1-diff-warning-1-face))))))
      'default)))

(defun asn1-diff-delete-overlay ()
  "Delete all the overlay of the ASN.1 diff buffers"
  (let ((l asn1-diff-list-of-overlay))
    (while l
      (condition-case ()
	  (delete-overlay (car l))
	(error nil))
      (setq l (cdr l))))
  (sit-for 0)
  (setq asn1-diff-list-of-overlay nil)
  (setq asn1-diff-current-overlay nil))

(defun asn1-diff-switch-color ()
  "Toggle ON/OFF the usage of color to see the differences
in the ASN.1 buffers.
To (don't) color at startup, see `asn1-diff-use-color'"
  (interactive)
  (setq asn1-diff-color-p (not asn1-diff-color-p))
  (if asn1-diff-color-p
      (let ((l asn1-diff-list)
	    (lo asn1-diff-list-of-overlay))
	(while l
	  (asn1-diff-color-overlay (car l))
	  (setq l (cdr l)))
	(while lo
	  (setq lo (cdr lo))))
    (let ((lo asn1-diff-list-of-overlay))
      (while lo
	(condition-case ()
	    (progn
	      (overlay-put (car lo) 'face 'default)
	      ;; don't have priority against font-lock
	      (overlay-put (car lo) 'priority 0))
	  (error nil))
	(setq lo (cdr lo))))))

(defun asn1-diff-color-only-marked ()
  "Color only marked messages"
  (interactive)
  (if (not asn1-diff-color-p)
      (asn1-diff-switch-color))
  (let ((l asn1-diff-list)
	lo)
    (while l
      (if (eq (asn1-diff-get-mark (car l)) 'mark) nil
	(setq lo (asn1-diff-get-overlays (car l)))
	(while lo
	  (condition-case ()
	      (progn
		(if (null (car lo)) nil
		  (overlay-put (car lo) 'face 'default)
		  ;; for xemacs (have priority against font-lock)
		  (overlay-put (car lo) 'priority 0)))
	    (error nil))
	  (setq lo (cdr lo))))
      (setq l (cdr l))))
  (setq asn1-diff-current-overlay nil))



;;; -----------------------------------------------------------------------
;;;			To mark, ignore, and unmark
;;; -----------------------------------------------------------------------

(defun asn1-diff-find-diff ()
  "Place the cursor on the next diff"
  (beginning-of-line)
  (if (search-forward-regexp "^.>" nil t)
      (beginning-of-line)
    (asn1-diff-error 'no-more-diff)))

(defun asn1-diff-char (char mark)
  "Function used to mark differences :
CHAR is the char to add at the beginning of line :
CHAR             MARK
  * for mark	('mark)
  % for ignore	('ignore) 
  - for nothing	(nil)

MARK is the name of the mark in `asn1-diff-list'"
  (asn1-diff-verif nil)
  ;(asn1-diff-refresh)
  (if (eq (current-buffer) asn1-diff-buffer)
      (asn1-diff-find-diff)
    (let ((o (asn1-overlay-at (point))))
      (if (null o)
	  (asn1-diff-error 'no-diff-here))
      (set-buffer asn1-diff-buffer)
      (goto-line (overlay-get o 'asn1))))
  (setq buffer-read-only nil)
  (delete-char 1)
  (insert-char char 1)
  (let* ((nb (count-lines (point-min) (point)))
	 (elt (assq nb asn1-diff-list)))
    (if elt
	(asn1-diff-set-mark elt mark)
      (error (asn1-lg "Tampon %s modifie" "buffer %s modified")
	     (buffer-name)))
    (asn1-diff-color-overlay elt))
  (backward-char 1)
  (setq buffer-read-only t)
  (next-line 1))

(defun asn1-diff-mark ()
  "Mark the current difference"
  (interactive)
  (asn1-diff-char ?* 'mark))

(defun asn1-diff-ignore ()
  "Ignore the current difference"
  (interactive)
  (asn1-diff-char ?% 'ignore))

(defun asn1-diff-unmark ()
  "Unmark the current difference"
  (interactive)
  (asn1-diff-char ?- nil))

(defun asn1-diff-mark-all ()
  "Mark all the differences"
  (interactive)
  (asn1-diff-all 'asn1-diff-mark))

(defun asn1-diff-unmark-all ()
  "Unmark all the differences"
  (interactive)
  (asn1-diff-all 'asn1-diff-unmark))

(defun asn1-diff-ignore-all ()
  "Ignore all the differences"
  (interactive)
  (asn1-diff-all 'asn1-diff-ignore))

(defun asn1-diff-all (function)
  (asn1-diff-verif nil)
  (set-buffer asn1-diff-buffer)
  (setq buffer-read-only nil)
  (save-excursion
    (goto-char (point-min))
    (condition-case ()
	(while t
	  (apply function '()))
      (error nil)))
  (setq buffer-read-only t))

(defun asn1-diff-ignore-all-warning ()
  "Ignore all warnings."
  (interactive)
  (asn1-diff-verif nil)
  (set-buffer asn1-diff-buffer)
  (let ((l asn1-diff-list))
    (while l
      (if (not (eq (car (cdr (car l))) 'warning)) nil
	(goto-line (car l))
	(asn1-diff-ignore))
      (setq l (cdr l)))))

(defun asn1-diff-undo ()
  "Like undo"
  (interactive)
  (asn1-diff-verif nil)  
  (set-buffer asn1-diff-buffer)
  (setq buffer-read-only nil)
  (condition-case ()
      (undo)
    (error 
     (setq buffer-read-only t)
     (error "No further undo informations")))
  (setq buffer-read-only t)
  (asn1-diff-read-mark))


(defun asn1-diff-read-mark ()
  ;; Modify the mark attribute of `asn1-diff-list' with the diff buffer
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (let ((l asn1-diff-list))
      (while (not (bobp))
	(previous-line 1)
	(if (not (looking-at (car asn1-diff-regexp-1))) nil
	  (asn1-diff-set-mark
	   (car l)
	   ;; char-after : argument is not optional in emacs 19
	   (cdr (assq (char-after (point)) '((?* . mark)
					     (?- . nil)
					     (?% . ignore)))))
	  (asn1-diff-color-overlay (car l))
	  (setq l (cdr l)))))))


;;; -----------------------------------------------------------------------
;;;			The Tree Windows
;;; -----------------------------------------------------------------------

(defun asn1-diff-verif (arg)
  "Verify if the buffer diff and the windows already exist
If none, do error or redraw windows"
  (if (not asn1-diff-current)
      (error
	(if asn1-diff-is-compile
	  (asn1-lg "Pas de compilation en cours"
	    "No compilation to resume")
	  (asn1-lg "Pas de comparaison en cours"
	    "No comparison to resume"))))
  (if (not (and
	    (buffer-live-p asn1-diff-buffer)))
      (asn1-diff-error 'killed-buffer)
    (if (not arg) nil
      (if (and (window-live-p asn1-diff-window-A)
	       ;; v2.2
	       (or
		asn1-diff-is-compile
		(window-live-p asn1-diff-window-B))
	       (window-live-p asn1-diff-window-diff))
	  nil
	(asn1-diff-show-window)
	(if (not (and (window-live-p asn1-diff-window-A)
		      ;; v2.2
		      (or
		       asn1-diff-is-compile
		       (window-live-p asn1-diff-window-B))
		      (window-live-p asn1-diff-window-diff)))
	    (asn1-diff-make-windows)))
      (let ((window (selected-window)))
	(select-window asn1-diff-window-diff)
	(switch-to-buffer asn1-diff-buffer)
	(select-window window)))))

(defun asn1-diff-make-windows ()
  "Create the tree windows"
  (interactive)
  (asn1-diff-verif nil)

  (let ((a (if (window-live-p asn1-diff-window-A)
	       (window-buffer asn1-diff-window-A)))
	(b (if asn1-diff-is-compile nil
	     (if (window-live-p asn1-diff-window-B)
		 (window-buffer asn1-diff-window-B)))))

  (delete-other-windows)
  (raise-frame)

  (setq asn1-diff-window-A (selected-window))

  (condition-case ()
      (progn
  (let ((h (if (null asn1-diff-window-height)
	       nil
	     (- asn1-diff-window-height)))
	(w (if (null asn1-diff-window-width)
	       nil
	     (- asn1-diff-window-width))))

    (cond ((eq asn1-diff-windows-type 3)
	   (if (eq (selected-frame) asn1-diff-frame-diff)
	       (if (frame-live-p asn1-diff-frame)
		   (select-frame asn1-diff-frame)
		 (if (eq (other-frame -1)
			 (if asn1-data-xemacs-p
			     (selected-frame)
			   nil))
		     (select-frame (new-frame)))))
	   (setq asn1-diff-window-A (selected-window))
	   (if (frame-live-p asn1-diff-frame-diff)
	       (select-frame asn1-diff-frame-diff)
	     (if (eq (other-frame -1)
		     (if asn1-data-xemacs-p
			 (selected-frame)
		       nil))
		 (select-frame (new-frame))))
	   (setq asn1-diff-frame-diff (selected-frame))
	   (raise-frame)
	   (setq asn1-diff-window-diff (selected-window))
	   (if w (set-frame-height (selected-frame) (- h)))
	   (if w (set-frame-width (selected-frame) (- w)))
	   (select-window asn1-diff-window-A)
	   (if (not asn1-diff-is-compile)
	       (setq asn1-diff-window-B (split-window-vertically))))
	  
	  ((eq asn1-diff-windows-type 0)
	   (setq asn1-diff-window-diff (split-window-vertically h))
	   (if (not asn1-diff-is-compile)
	       (setq asn1-diff-window-B (split-window-horizontally)))
	   (if w (set-frame-width (selected-frame) (- w))))

	  ((eq asn1-diff-windows-type 1)
	   (setq asn1-diff-window-diff (split-window-vertically h))
	   (if (not asn1-diff-is-compile)
	       (setq asn1-diff-window-B (split-window-vertically)))
	   (if w (set-frame-width (selected-frame) (- w))))

	  ((eq asn1-diff-windows-type 2)
	   (setq asn1-diff-window-diff (split-window-horizontally w))
	   (if (not asn1-diff-is-compile)
	       (setq asn1-diff-window-B (split-window-vertically)))
	   (if h (set-frame-height (selected-frame) (- h))))
	  (t
	   (error ""))))
  )
    (error
     (message (asn1-lg
		  "Attention : fenetrage incorrect"
		"Warning : bad windows type"))
     (delete-other-windows)
     (setq asn1-diff-window-A (selected-window))
     (if (< (frame-height) 20)
	 (set-frame-height (selected-frame) 40))
     (if (< (frame-width) 80)
	 (set-frame-width (selected-frame) 80))
     (setq asn1-diff-window-diff (split-window-vertically))
     (if (not asn1-diff-is-compile)
	 (setq asn1-diff-window-B (split-window-horizontally)))))

    (select-window asn1-diff-window-A)
    (switch-to-buffer
      (if asn1-diff-is-compile
	(if (buffer-live-p asn1-diff-file-B)
	  asn1-diff-file-B
	  "*scratch*")
	(if a a "*scratch*")))
    (if asn1-diff-is-compile nil
      (select-window asn1-diff-window-B)
      (switch-to-buffer (if b b "*scratch*")))
    (select-window asn1-diff-window-diff)
    (switch-to-buffer asn1-diff-buffer)
    (asn1-diff-save-config)
    (setq truncate-partial-width-windows asn1-diff-truncate-lines)
    (if asn1-diff-is-compile
      (select-window asn1-diff-window-A))      
    (run-hooks 'asn1-diff-make-windows-hook)
    (sit-for 0)))

(defun asn1-diff-show-window ()
  "Redraw the tree windows"
  (interactive)
  (cond ((and (frame-live-p asn1-diff-frame) asn1-diff-window-configuration)
	 (let ((f (selected-frame))
	       (a (if (window-live-p asn1-diff-window-A)
		      (window-buffer asn1-diff-window-A)))
	       (b (if asn1-diff-is-compile nil
		    (if (window-live-p asn1-diff-window-B)
			(window-buffer asn1-diff-window-B)))))
	   (select-frame asn1-diff-frame)
	   (set-window-configuration asn1-diff-window-configuration)
	   (select-window asn1-diff-window-A)
	   (switch-to-buffer
	     (if asn1-diff-is-compile
	       (if (buffer-live-p asn1-diff-file-B)
		 asn1-diff-file-B
		 "*scratch*")
	       (if a a "*scratch*")))
	   (if asn1-diff-is-compile nil
	     (select-window asn1-diff-window-B)
	     (switch-to-buffer (if b b "*scratch*")))
	   (select-window asn1-diff-window-diff)
	   (switch-to-buffer asn1-diff-buffer)
	   (select-frame f)))
	(t
	 (asn1-diff-make-windows)))
  (setq truncate-partial-width-windows asn1-diff-truncate-lines))

(defun asn1-diff-refresh ()
  "Refresh (put off color of the current diff if necessary)"
  (interactive)
  (setq truncate-partial-width-windows asn1-diff-truncate-lines)
  (asn1-diff-color-overlay asn1-diff-current-overlay)
  (setq asn1-diff-current-overlay nil)
  ;(redraw-display)
  )

(defun asn1-goto-diff-buffer ()
  (interactive)
  (asn1-diff-verif nil)
  (switch-to-buffer asn1-diff-buffer))

(defun asn1-diff-scroll-up ()
  (interactive)
  (asn1-diff-verif t)
  (let ((w (selected-window)))
    (select-window asn1-diff-window-A)
    (condition-case ()
	(scroll-up)
      (error nil))
    (if asn1-diff-is-compile nil
      (select-window asn1-diff-window-B)
      (condition-case ()
	  (scroll-up)
	(error nil)))
    (select-window w)))
  
(defun asn1-diff-scroll-down ()
  (interactive)
  (asn1-diff-verif t)
  (let ((w (selected-window)))
    (select-window asn1-diff-window-A)
    (condition-case ()
	(scroll-down)
      (error nil))
    (if asn1-diff-is-compile nil
      (select-window asn1-diff-window-B)
      (condition-case ()
	  (scroll-down)
	(error nil)))
    (select-window w)))
  


;;; -----------------------------------------------------------------------
;;;			To show differences
;;; -----------------------------------------------------------------------

(defun asn1-diff-mouse-goto-diff (event)
  "Like `asn1-diff-goto-diff' with the mouse"
  (interactive "e")
  (asn1-diff-verif t)
  (let ((window (selected-window)))
    (mouse-set-point event)
    (asn1-diff-goto-diff 0)
    (if (window-live-p window)
	(select-window window)))
  (asn1-diff-uncolor))

(defun asn1-diff-goto-next-diff ()
  "Display the next difference"
  (interactive)
  (asn1-diff-goto-next-diff-aux "^[-*]>"))

(defun asn1-diff-goto-next-all-diff ()
  "Display the next difference (include ignored differences)"
  (interactive)
  (asn1-diff-goto-next-diff-aux "^[-*%]>"))

(defun asn1-diff-goto-next-diff-aux (regexp)
  (asn1-diff-verif t)
  (let ((window (selected-window)))
    (select-window asn1-diff-window-diff)
    (condition-case ()
	(next-line 1)
      (error nil))
    (beginning-of-line)
    (if (search-forward-regexp regexp nil t)
	(asn1-diff-goto-diff 0)
      (condition-case ()
	  (previous-line 1)
	(error nil))
      (select-window window)
      (asn1-diff-error 'no-more-diff))
    (select-window window))
  (asn1-diff-uncolor))

(defun asn1-diff-goto-previous-all-diff ()
  "Display the previous difference (include ignored differences)"
  (interactive)
  (asn1-diff-goto-previous-diff-aux "^[-*%]>"))

(defun asn1-diff-goto-previous-diff ()
  "Display the previous difference"
  (interactive)
  (asn1-diff-goto-previous-diff-aux "^[-*]>"))

(defun asn1-diff-goto-previous-diff-aux (regexp)
  (asn1-diff-verif t)
  (let ((window (selected-window)))
    (select-window asn1-diff-window-diff)
    (beginning-of-line)
    (if (search-backward-regexp regexp nil t)
	(asn1-diff-goto-diff -1)
      (select-window window)
      (asn1-diff-error 'no-more-diff))
    (select-window window))
  (asn1-diff-uncolor))
    

(defun asn1-diff-goto-diff (arg)
  "Display the current difference
 With negative arg, recenter at the bottom instead of at the top.
 (recenter only if `asn1-diff-recenter' is non-nil)"
  (interactive "P")
  (setq arg (prefix-numeric-value arg))
  (asn1-diff-verif t)
  (let ((window (selected-window))
	nb
	pt
	(lw (if asn1-diff-is-compile
		(list asn1-diff-window-A asn1-diff-window-A)
	      (list asn1-diff-window-B asn1-diff-window-A))))
    (select-window asn1-diff-window-diff)
    (beginning-of-line)
    (if (search-forward-regexp asn1-diff-regexp nil t) nil
      (previous-line 1)
      (asn1-diff-error 'no-more-diff))
    (setq nb (count-lines (point-min) (point)))
    (beginning-of-line)
    (setq pt (point))
    (if asn1-diff-recenter
	(recenter (if (< arg 0) -1 0)))
    (goto-char pt)
    (let ((elt (assq nb asn1-diff-list)))
      (if (null elt)
	  (error (asn1-lg "Tampon %s modifie" "buffer %s modified")
		 (buffer-name)))
      (asn1-diff-color-overlay asn1-diff-current-overlay)
      (setq asn1-diff-current-overlay elt)
      (setq elt (asn1-diff-get-overlays elt))
      (if (and (null (car (cdr elt)))
	       (or
		(member (file-truename (buffer-file-name
					(overlay-buffer (car elt))))
			asn1-diff-list-of-files)
		(member (buffer-file-name (overlay-buffer (car elt)))
			asn1-diff-list-of-files)))
	  (setq lw (reverse lw)))
      (while elt
	(cond ((car elt)
	       (if (not (buffer-live-p (overlay-buffer (car elt))))
		   ;;(asn1-diff-error 'killed-buffer)
		   (progn
		     (message
		      (asn1-lg
			  "Attention : certains tampons ont ete detruits."
			"Warning : you've killed some buffer."))
		     (select-window (car lw))
		     (switch-to-buffer "*scratch*")
		     )
		 (select-window (car lw))
		 (switch-to-buffer (overlay-buffer (car elt)))
		 (goto-char (overlay-start (car elt)))
		 (recenter (/ (window-height) 2))
	       
;; underline
; 	       (overlay-put
; 		(car elt) 'face
; 		(intern
; 		 (concat (symbol-name (overlay-get (car elt) 'face))
; 			 "-u")))
		 (overlay-put (car elt) 'face 'asn1-diff-current-face)
		 (overlay-put (car elt) 'priority 2)))
	       (t
		(if asn1-diff-is-compile nil
		  (select-window (car lw))
		  (switch-to-buffer "*scratch*"))))
	(setq elt (cdr elt))
	(setq lw (cdr lw))))
    (select-window window))
  ;; v2.2
  (save-excursion
    (set-buffer asn1-diff-buffer)
    (beginning-of-line)
    (setq asn1-diff-diff-overlay
	  (make-overlay (point) (+ (point) 1)))
    (overlay-put asn1-diff-diff-overlay 'face 'asn1-diff-current-face)
    (overlay-put asn1-diff-diff-overlay 'priority 2))
  (asn1-diff-save-config))

(defun asn1-diff-uncolor ()
  (message (if (and (fboundp 'current-message) (current-message))
	       (current-message)
	     ""))
  (let* ((cursor-in-echo-area nil)
	 (e (if (not asn1-data-xemacs-p)
		(read-event)
	      ;; read the current events
	      (discard-input)
	      ;; next-command event don't work :
	      ;; it take the event key-release
	      (let ((event (allocate-event)))
		(while (progn (next-event event)
			      (not (or (button-press-event-p event)
				       (key-press-event-p event)
				       (menu-event-p event))))
		  (dispatch-event event))
		event)
	      )))
    (setq unread-command-events
	  (cons e unread-command-events))
    )
  (asn1-diff-color-overlay asn1-diff-current-overlay)
  (setq asn1-diff-current-overlay nil)
  ;; v2.2
  (if asn1-diff-diff-overlay
      (delete-overlay asn1-diff-diff-overlay)
      (setq asn1-diff-diff-overlay nil))
  )


; (asn1-modify-face-list
;  `(
;     (asn1-diff-error-1-face-u	"yellow"	"blue"	 nil nil nil t
;      ,(asn1-lg "Difference dans un seul fichier, correspondant a une erreur"
; 	  "Difference error in one buffer"))
;     (asn1-diff-error-2-face-u	"white"		"blue"	 nil nil nil t
;      ,(asn1-lg "Difference dans deux fichers, correspondant a une erreur"
; 	  "Difference error in two buffer"))
;     (asn1-diff-warning-1-face-u	"yellow"	"CornflowerBlue"
; 				nil nil nil t
;      ,(asn1-lg "Difference dans un seul fichier, correspondant a un warning"
; 	  "Difference warning in one buffer"))
;     (asn1-diff-warning-2-face-u	"white"		"CornflowerBlue"
; 				nil nil nil t "rien")))

(defun asn1-diff-mouse-goto-message (event arg)
  "Like `asn1-diff-goto-message' with the mouse"
  (interactive "e\nP")
  (let ((window (selected-window)))
    (mouse-set-point event)
    (if (not (asn1-overlay-at (point)))
	;; utilisation of diff and syntax menu together
	(if asn1-data-syntax-menu-loaded
	    (asn1-syntax-edition event arg)
	  (if asn1-data-xemacs-p
	      (mouse-yank event)
	    (asn1-mice-normal-button event arg)))
      (asn1-diff-goto-message)
      (if (window-live-p window)
	  (select-window window))))
  (asn1-diff-uncolor))

(defun asn1-diff-goto-message ()
  "Show the message of difference in the diff buffer."
  (interactive)
  (let ((o (asn1-overlay-at (point)))
	(w (selected-window))
	(b (current-buffer))
	(ws (window-start)))
    (if (null o)
	(asn1-diff-error 'no-diff-here))
    (asn1-diff-verif t)
    (select-window asn1-diff-window-diff)
    (goto-line (overlay-get o 'asn1))
    (recenter 0)
    (asn1-diff-goto-diff 0)
    (cond ((window-live-p w)
	   (select-window w)
	   (if (eq b (current-buffer))
	       (set-window-start w ws))))))

(defun asn1-diff-goto-message-or-difference ()
  (interactive)
  (if (not (eq (current-buffer) asn1-diff-buffer))
      (asn1-diff-goto-message)
    (asn1-diff-goto-diff 0))
  (asn1-diff-uncolor))

(defun asn1-diff-difference-p ()
  (if (eq (current-buffer) asn1-diff-buffer)
      t
    (asn1-overlay-at (point))))
  


;;; -----------------------------------------------------------------------
;;;		Functions to insert text in ASN.1 buffers
;;; -----------------------------------------------------------------------

(defun asn1-diff-insert-text (text)
  "Insert TEXT in the ASN.1 buffers
only at the place of the marked differences"
  (interactive (list (read-string "Text to insert: "
				  asn1-diff-text-to-insert)))
  (setq asn1-diff-text-to-insert text)
  (asn1-diff-insert-text-aux t "^[-*]>" text))

(defun asn1-diff-insert-text-mark (text)
  "Insert TEXT in the ASN.1 buffers
at the place of the differences"
  (interactive (list (read-string "Text to insert: "
				  asn1-diff-text-to-insert)))
  (setq asn1-diff-text-to-insert text)
  (asn1-diff-insert-text-aux t "^[*]>" text))

(defun asn1-diff-insert-messages ()
  "Insert only the marked differences messages in the ASN.1 buffers
at the correct place"
  (interactive)
  (asn1-diff-insert-text-aux t "^[-*]>" nil))

(defun asn1-diff-insert-messages-mark ()
  "Insert the differences messages in the ASN.1 buffers at the correct place"
  (interactive)
  (asn1-diff-insert-text-aux t "^[*]>" nil))

(defun asn1-diff-undo-insert ()
  (interactive)
  (if (null asn1-diff-insertion)
      (error (asn1-lg "Pas d'insertion a annuler" "No insert to undo")))
  (asn1-diff-insert-text-aux nil
			     (car (car asn1-diff-insertion))
			     (cdr (car asn1-diff-insertion)))
  (setq asn1-diff-insertion (cdr asn1-diff-insertion)))
			     
(defun asn1-diff-insert-text-aux (arg type text)
  "Insert text in the ASN.1 buffers at the place of differences.
If TEXT is nil, insert the differences messages gives by asnp
Insert only in differences specified by TYPE :
TYPE is a regexp : ^[*]> or ^[-*]>
If ARG is t, insert. Else, undo insertion."
  (asn1-diff-verif nil)
  ;(asn1-diff-refresh)
  (if arg
      (setq asn1-diff-insertion (cons (cons type text) asn1-diff-insertion)))
  (set-buffer asn1-diff-buffer)
  (save-excursion
    (goto-char (point-max))
    (beginning-of-line)
    (let ((l asn1-diff-list)
	  (nb (length asn1-diff-list))
	  (i 0)
	  pt s o b)
      (while (not (bobp))
	(previous-line 1)
	(beginning-of-line)	
	(if (not (looking-at type)) nil
	  (forward-char 3)
	  (setq pt (point))
	  (end-of-line)
	  (setq s (concat "\n-- " (asn1-substring pt (point)) " --"))
	  (setq o (asn1-diff-get-overlays (car l)))
	  (message (if arg
		       (asn1-lg "Insertion... %3d%%" "Inserting... %3d%%")
		     (asn1-lg "Annulation insertion... %3d%%"
		       "Undo Insert... %3d%%"))
		   (/ (* 100 i) nb))
	  (setq i (1+ i))
	  (while o
	    (if (null (car o)) nil
	      (setq b (overlay-buffer (car o)))
	      (if (not (buffer-live-p b))
		  (message
		   (asn1-lg
		       "Attention : certains tampons ont ete detruits."
		     "Warning : you've killed some buffer."))
		(save-excursion
		  (set-buffer b)
		  (goto-char (overlay-start (car o)))
		  (setq buffer-read-only nil)
		  (cond (text
			 (if arg
			     (insert text)
			   (asn1-delete-string-at text)))
			(t
			 (beginning-of-line)
			 (condition-case ()
			     (backward-char 1)
			   (error nil))
			 (if arg
			     (insert s)
			   (beginning-of-line)			   
			   (condition-case ()
			       (backward-char 1)
			     (error nil))
			   (asn1-delete-string-at s)))))))
	    (setq o (cdr o))))
	(beginning-of-line)
	(if (looking-at "^.>")
	  (setq l (cdr l))))))
  (message (if arg
	       (asn1-lg "Insertion...terminee" "Inserting...done")
	     (asn1-lg "Annulation insertion...terminee"
	       "Undo insert...done"))))

(defun asn1-delete-string-at (string)
;  (condition-case ()
      (let* ((pt (point))
	     (end (+ (point) (length string))))
	(if (string-equal string
			  (asn1-substring pt end))
	    (delete-region pt end)
	  (goto-char end)
	  (if (search-backward string nil t)
	      (delete-region (point) (+ (point) (length string))))))
;    (error nil))
      )

(defun asn1-diff-save-all-buffers ()
  (interactive)
  (let ((l asn1-diff-list-of-buffers)
	b)
    (while l
      (setq b (cdr (car l)))
      (cond ((buffer-live-p b)
	     (set-buffer b)
	     (if (buffer-modified-p)
		 (save-buffer))))
      (setq l (cdr l)))))

(defun asn1-diff-print-all-buffers ()
  (interactive)
  (asn1-diff-refresh)
  (if (or (not asn1-diff-ask)
	  (y-or-n-p (asn1-lg
		     "Imprimer tous les fichiers ASN.1 des specifications ? "
		     "Print all ASN.1 files of specifications ? ")))
      (let ((l asn1-diff-list-of-buffers)
	    b)
	(while l
	  (setq b (cdr (car l)))
	  (cond ((buffer-live-p b)
		 (set-buffer b)
		 (ps-print-buffer-with-faces)))
	  (setq l (cdr l))))))


;;; -----------------------------------------------------------------------
;;;				Others
;;; -----------------------------------------------------------------------

(defun asn1-diff-info ()
  (interactive)
  (delete-other-windows)
  (info (asn1-lg
	 (if (file-exists-p (concat asn1-data-dir "asn1-diff-fr.info"))
	     (concat asn1-data-dir "asn1-diff-fr.info")
	   "asn1-diff-fr.info")
	 (if (file-exists-p (concat asn1-data-dir "asn1-diff-en.info"))
	     (concat asn1-data-dir "asn1-diff-en.info")
	   "asn1-diff-en.info"))))

(defun asn1-diff-error (fun &rest arg)
  (apply 'error
	 (nth (asn1-lg 1 2) (assq fun asn1-diff-list-of-error))
	 arg))

(defun asn1-custom-diff-face ()
  "A simply way to custom diff faces."
  (interactive)
  (delete-other-windows)
  (if asn1-use-standard-custom
      (customize-group 'asn1-face)
    (asn1-conf-face
     (asn1-lg
	 "Configuration des styles d'ecritures du mode comparaison d'ASN.1"
       "Configuration of ASN.1 Diff Faces")
     'asn1-diff-list-of-faces
     'asn1-diff-list-of-default-faces)))

(defun asn1-custom-diff-options ()
  "Customize Diff Options."
  (interactive)
  (delete-other-windows)
  (customize-group 'asn1-diff))

(defun asn1-diff-add-menu (map)
  "Add the Diff Menu in the menubar of the current buffer"
  (make-local-variable 'asn1-data-diff-menu-p)
  (let ((m (current-local-map)))
    (use-local-map map)
    (use-local-map m))
  (setq asn1-data-diff-menu-p t)
  (if asn1-data-xemacs-p
    (easy-menu-add (if asn1-diff-is-compile
		     asn1-compile-menu
		     asn1-diff-menu) map)
    (if asn1-diff-is-compile
      (easy-menu-define
	asn1-diff-mode-menu
	map
	"Menu keymap for ASN.1 mode."
	asn1-compile-menu)
      (easy-menu-define
	asn1-diff-mode-menu
	map
	"Menu keymap for ASN.1 mode."
	asn1-diff-menu))))

(defun asn1-diff-help ()
  (interactive)
  (with-output-to-temp-buffer
      "*Help ASN.1 Diff*"
    (princ (asn1-lg
		"
Principaux raccourcis clavier :
  ENTER / bouton-2 : affiche les differences correspondant aux messages d'asnp
  n / espace : affiche la difference suivante
  p / delete : affiche la difference precedente
  c : colorise ou decolorise les tampons ASN.1
  r : insere les messages d'asnp dans les tampons ASN.1
  p : imprime tous les modules ASN.1
  e : quitte provisoirement
  ? : affiche cette aide

Dans un tampon ASN.1, taper d'abord C-c et ensuite la touche.
Dans le tampon \"ASN.1-Diff\", ces touches sont accessibles directement.
"
	      "
Major keybindings :
  ENTER / mouse-2 : show current difference
  n / space  : show next difference
  p / delete : show previous difference
  c : disable/enable color of differences in ASN.1 modules
  r : insert asnp messages in module (at differences positions)
  p : print all ASN.1 modules
  e : exit temporary
  ? : show this help

In the ASN.1 buffers, type C-c and the correspondant keys in the diff buffer.
  "))))

(defun asn1-set-compare-command ()
  (interactive)
  (setq asn1-diff-command
	(read-file-name
	 (asn1-lg
	  "Commande de comparaison : "
	  "Compare Command : "))))

(defun asn1-set-compare-switches ()
  (interactive)
  (setq asn1-diff-switches
	(read-from-minibuffer
	 (asn1-lg
	  "Parametres de comparaison : "
	  "Compare Switches : ")
	 asn1-diff-switches)))

(provide 'asn1-diff)
