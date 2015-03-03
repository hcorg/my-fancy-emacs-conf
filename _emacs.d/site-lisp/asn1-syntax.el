;;; asn1-mode.el --- ASN.1:1997 major mode for GNU Emacs and XEmacs

;; Copyright (C) 1997-2001  France Telecom R&D, Lannion

;; Authors:      Guillaume LATU, Stephane LEVANT, Olivier DUBUISSON
;; Maintainer:   asn1@rd.francetelecom.fr
;; Informations: http://asn1.elibel.tm.fr/fr/outils/emacs/   (francais)
;;               http://asn1.elibel.tm.fr/en/tools/emacs/    (english)
;; Created:      1997
;; Keywords:    ASN.1, languages, major mode, protocole specifications, grammar

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

;; Create a menu with the ASN.1 grammar.
;; It allow to built an ASN.1 specification.
;;
;; See the manual.

;;; Code:

(require 'asn1-mode)

(defvar asn1-add-syntax-menu-hook nil)

;;; Variables pour les menus

(defconst asn1-data-syntax-menu-title 
  "ASN.1-Syntax"
  "Nom du menu de syntaxe du mode ASN.1")

(defconst asn1-data-syntax-menu-alist
  '("ModuleDefinition"
    "Type"
    "Value")
  "Liste des non-terminaux presents dans le menu de syntaxe ASN.1.
Cette liste peut etre modifiee, le menu sera alors construit en
fonction de la nouvelle liste.")

(defconst asn1-data-syntax-menu-orprefix 
  "->   "
  "Prefixe utilise pour signifier le debut d'une production dans le
menu de syntaxe \"ASN.1-Syntax\".")

(defconst asn1-data-epsilon
  "-nothing-"
  "Chaine de caracteres qui remplace les EPSILON places dans la grammaire,
dans les menus qui visualisent une partie de cette grammaire :
	asn1-data-syntax-menu
	asn1-data-menu-*")


;;; Variables qui concernent l'edition syntaxique

(defvar asn1-data-incorrect-WORD
  '("BIT" "BOOLEAN" "CHARACTER" "CHOICE" "EMBEDDED" "END" "ENUMERATED" 
    "EXTERNAL" "FALSE" "INSTANCE" "INTEGER" "INTERSECTION" "MINUS-INFINITY"
    "NULL" "OBJECT" "OCTET" "PLUS-INFINITY" "REAL" "SEQUENCE" "SET" "TRUE" 
    "UNION" "BMPString" "GeneralizedTime" "GeneralString" "GraphicString"
    "IA5String" "ISO646String" "NumericString" "ObjectDescriptor"
    "PrintableString" "RELATIVE-OID" "T61String" "TeletexString"
    "UniversalString"
    "UTCTime" "UTF8String" "VideotexString" "VisibleString")
  "Liste des mots que le terminal WORD ne peut pas etre.")

(defvar asn1-data-incorrect-TYPEREFERENCE
  '("ABSENT" "ABSTRACT-SYNTAX" "ALL" "APPLICATION" "AUTOMATIC" "BEGIN"
    "BIT" "BMPString"  "BOOLEAN" "BY" "CHARACTER" "CHOICE" "CLASS"
    "COMPONENT" "COMPONENTS" "CONSTRAINED" "DEFAULT" "DEFINITIONS"
    "EMBEDDED" "END" "ENUMERATED" "EXCEPT" "EXPLICIT" "EXPORTS"
    "EXTENSIBILITY" "EXTERNAL" "FALSE" "FROM" "GeneralizedTime"
    "GeneralString" "GraphicString" "IA5String" "IDENTIFIER"
    "IMPLICIT" "IMPLIED" "IMPORTS" "INCLUDES" "INSTANCE" "INTEGER"
    "INTERSECTION" "ISO646String" "MAX" "MIN" "MINUS-INFINITY" "NULL"
    "NumericString" "OBJECT" "ObjectDescriptor" "OCTET" "OF"
    "OPTIONAL" "PDV" "PLUS-INFINITY" "PRESENT" "PrintableString"
    "PRIVATE" "REAL" "RELATIVE-OID" "SEQUENCE" "SET" "SIZE" "STRING" "SYNTAX"
    "T61String" "TAGS" "TeletexString" "TRUE" "TYPE-IDENTIFIER"
    "UNION" "UNIQUE" "UNIVERSAL" "UniversalString" "UTCTime" "UTF8String"
    "VideotexString" "VisibleString" "WITH")
  "Liste des mots que le terminal TYPEREFERENCE ne peut pas etre.")
  


;;; Souris

(eval-and-compile
(defconst asn1-data-mark-NTMandatory
  '("(" ")")
  "(MDEB MFIN)
Marques de debut MDEB et marque de fin MFIN lorsque l'on ecrit un
non-terminal obligatoire (qui ne se reduit pas en le mot vide).")

(defconst asn1-data-mark-TMandatory
  '("$" "$")
"(MDEB MFIN)
Marques de debut MDEB et marque de fin MFIN lorsque l'on ecrit un
terminal.")

(defconst asn1-data-mark-NTOptional
  '("[" "]")
  "(MDEB MFIN)
Marques de debut MDEB et marque de fin MFIN lorsque l'on ecrit un
non-terminal optionnel (qui se reduit en le mot vide).")
)

(defvar asn1-data-production
  ;; L'emboitement des \\( et \\) est tres important, cela nous permet
  ;; ensuite de rappeler ce qu'il y a entre \\( \\). Cela peut permettre
  ;; de deduire par exemple si l'on a lu asn1-data-mark-NTMandatory ou
  ;; asn1-data-mark-TMandatory. 
  ;; Ainsi on pourra savoir si l'on a lu un terminal ou un non-terminal.
  (eval-when-compile
  (concat "\\([-][-]\\)"
	  "\\(" (asn1-string2regexp 
		 (nth 0 asn1-data-mark-NTMandatory)) 
	  "\\|" (asn1-string2regexp 
		 (nth 0 asn1-data-mark-NTOptional)) 
	  "\\|" (asn1-string2regexp 
		 (nth 0 asn1-data-mark-TMandatory))
	  "\\)"
	  "\\([-A-Za-z0-9]+\\)"
	   "\\(" (asn1-string2regexp 
		 (nth 1 asn1-data-mark-NTMandatory)) 
	  "\\|" (asn1-string2regexp 
		 (nth 1 asn1-data-mark-NTOptional)) 
	  "\\|" (asn1-string2regexp 
		 (nth 1 asn1-data-mark-TMandatory)) 
	  "\\)"
	  "\\([-][-]\\)"))
  "Expression reguliere d'un non-terminal ou d'un terminal.
Elle est utilisee dans asn1-locate-production et
asn1-mouse-sensible-code. ")

(defun asn1-mouse-sensible-code ()
  "Recherche et met en style d'ecriture special tous les terminaux et
non-terminaux du buffer courant"
  (let ((m (buffer-modified-p)))
    (save-excursion
      (beginning-of-buffer)
      (while (re-search-forward asn1-data-production nil t)
	(asn1-mouse-face (match-end 1) (match-end 4))))
    (set-buffer-modified-p m))
  (if (not (or (eq buffer-undo-list t) (eq buffer-undo-list nil)))
      (setq buffer-undo-list (cdr buffer-undo-list))))

(defun asn1-mouse-face (begin end)
  "Rend la chaine de caracteres delimitee par BEGIN et END dans le
buffer courant, sensible au passage de la souris. Quand le pointeur de
la souris passe au dessus de la chaine de caracteres, son style
d'ecriture change, celui-ci devient le style d'ecriture
\"highlight\". Quand le pointeur de la souris n'est plus sur la 
chaine, elle reprend son ancien style d'ecriture."
  (if asn1-data-xemacs-p
      (put-text-property begin end 'highlight t)
    (put-text-property begin end 'mouse-face 'highlight)))


;;;---------------------------------------------------------------------
;;;			Compatibilite
;;;---------------------------------------------------------------------

;; asn1-menu-popup affiche un  menu surgissant (popup menu)
;; asn1-menu-create definit des menus pouvant etre donnes en argument a
;; asn1-popup-menu.
(cond ((fboundp 'popup-menu)
       ;; On se trouve dans ce cas sous XEmacs
       (defun asn1-menu-create (menu)
	 "Stocke dans la variable (concat \"asn1-data-menu-\" (car MENU))
		le contenu de MENU."
	 (eval
	  (` (defvar
	       (, (intern (concat "asn1-data-menu-" (car menu))))
	       '(, menu)))))
       (defalias 'asn1-menu-popup 'popup-menu))

      ((fboundp 'x-popup-menu)
       ;; on se trouve dans ce cas sous GNU Emacs
        (defun asn1-menu-create (menu)
	 "Stocke dans la variable (concat \"asn1-data-menu-\" (car MENU))
		le contenu de MENU."
	 (eval
	  (` (defvar
	       (, (intern (concat "asn1-data-menu-" (car menu))))
	       '(, (asn1-convert-menu-format menu))))))
       (defun asn1-menu-popup (menu event)
	 (let* ((call-back 
		 (x-popup-menu
		  event
		  menu)))
	   ;; Si rien n'a ete selectionne dans le menu call-back
	   ;; est a nil, sinon call-back est le nom d'une fonction
	   ;; (en fait call-back est exactement ce que l'on a mis
	   ;; en construisant le menu)
	   (and call-back (call-interactively call-back)))))
      (t
       (error "no popup menu function was found !")
       ))


;;;---------------------------------------------------------------------
;;;				Menu "ASN.1-Syntax"
;;;---------------------------------------------------------------------

(eval-and-compile
(defun asn1-rule-type (rule)
  "Si RULE est une regle pour laquelle le membre droit comporte
plusieurs productions la valeur de retour est CHOICE, sinon celle-ci 
est BLOCK."
  (if (cdr (cdr rule))
      'CHOICE
    'BLOCK))
)

;; asn1-data-grammar represente la grammaire d'ASN.1.
;; asn1-data-grammar est une liste d'elements, chaque element est une
;; regle de la grammaire.
;; Une regle a la forme suivante :
;;	(NTNAME PROD1 PROD2 ... PRODn)
;;
;; Chaque regle de la grammaire commence par le nom du non-terminal
;; membre gauche de la regle : NTNAME. Ce nom est une chaine de caracteres.
;; NTNAME est suivi de toute les production de ce non-terminal :
;;	PROD1 PROD2 ...PRODn
;; Une production a la forme suivante :
;;	(ELT1 ELT2 ... ELTn)
;;
;; Une production est donc une liste d'elements. Ceux-ci peuvent
;; representer un terminal, un non-terminal ou du texte.
;; Chaque element a la forme suivante :
;;	(STRING  TYPE  COMMAND1  COMMAND2)
;;
;; TYPE peut prendre comme valeur :
;;	Terminal
;;	NonTerminal
;;	Text
;; Selon l'element que l'on veut mettre dans la production.
;;
;; STRING est une chaine de caracteres, il est le nom du terminal ou
;; non-terminal ou bien le texte de l'element.
;;
;; COMMAND1 peut avoir comme valeur :
;;	(LINE n) ou (SPACE n), avec n entier positif ou nul
;; (LINE n) insere avant l'element un saut de ligne puis n espaces.
;;	Remarque : Comme le mode ASN.1 a l'indentation automatique, le
;;		   nombre n d'espaces a inserer ne se verra pas au
;;		   final car l'indentateur va "corriger" le nombre
;;		   d'espaces presents en debut de ligne.
;; (SPACE n) insere n espaces avant l'element.
;;
;; COMMAND2 est une commande optionnelle. Si elle est presente, elle
;; doit prendre la valeur : POINT.
;; Si POINT est present dans un element, alors, lorsque la production
;; est inseree par l'utilisateur dans son buffer, le curseur se place
;; sur l'element de la production qui possede la commande POINT.
;;	Remarque : Si plusieurs elements d'une meme production
;;		   possedent la commande POINT, le curseur se place
;;		   sur le premier element qui a la commande POINT.
;;
;; Un element d'une production peut aussi avoir la forme suivante :
;;	(EPSILON)
;; Dans ce cas, cette production represente la production du mot vide.
;;

(eval-and-compile
(defconst asn1-data-grammar 
  '(("ModuleDefinition"	
     (("modulereference"		Terminal	nil POINT)
      ("DefinitiveOID"			NonTerminal	(SPACE 1))
      ("DEFINITIONS"			Text		(LINE 0))
      ("TagDefault"			NonTerminal	(SPACE 1))
      ("ExtensionDefault"		NonTerminal	(SPACE 1))
      ("::="				Text		(SPACE 1))
      ("BEGIN"				Text		(LINE 0))
      ("Modulebody"			NonTerminal	(LINE 0))
      ("END"				Text		(LINE 0))))
     
    ("TagDefault"
     (("EXPLICIT TAGS"			Text		 nil))
     (("IMPLICIT TAGS"			Text		 nil))
     (("AUTOMATIC TAGS"			Text		 nil))
     ((EPSILON)))

    ("ExtensionDefault"
     (("EXTENSIBILITY IMPLIED"		Text		 nil))
     ((EPSILON)))

    ("DefinitiveOID"	
     (("{"				Text		(LINE 0))
      ("DefOIDComponent1"		NonTerminal	(SPACE 1) POINT)
      ("DefOIDComponent2"		NonTerminal	(SPACE 1))
      ("DefOIDComponentList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1)))
     ((EPSILON)))

    ("DefOIDComponent1"	
     (("itu-t(0)"			Text		 nil))
     (("iso(1)"				Text		 nil))
     (("joint-iso-itu-t(2)"		Text		 nil))
     (("number"				Terminal	nil POINT)))

    ("DefOIDComponent2"		
     (("identifier"			Terminal	nil POINT)
      ("("				Text		(SPACE 1))
      ("number"				Terminal	(SPACE 1))
      (")"				Text	       (SPACE 1)))
     (("number"				Terminal	nil POINT))
     (("identifier"			Terminal	nil POINT))
     (("administration(2)"		Text		 nil))
     (("identified-organization"	Text		 nil))
     (("member-body(2)"			Text		 nil))
     (("network-operator(3)"		Text		 nil))
     (("question(1)"			Text		 nil))
     (("recommendation(0)"		Text		 nil))
     (("standard(0)"			Text		 nil)))

    ("DefOIDComponentList"	
     (("identifier"			Terminal	nil POINT)
      ("("				Text		(SPACE 1))
      ("number"				Terminal	(SPACE 1))
      (")"				Text		nil)
      ("DefOIDComponentList"		NonTerminal	(SPACE 1)))
     (("number"				Terminal	nil POINT)
      ("DefOIDComponentList"		NonTerminal	(SPACE 1)))
     ((EPSILON)))

    ("Modulebody"
     (("Exports"			NonTerminal	nil POINT)
      ("Imports"			NonTerminal	(LINE 0))
      (" "				Text		(LINE 0))
      ("Assignment"			NonTerminal	(LINE 0))
      ("AssignmentListTail"   		NonTerminal	(LINE 0)))
     ((EPSILON)))

    ("Exports"		
     (("EXPORTS"			Text		 nil)
      ("SymbolList"			NonTerminal	(SPACE 1) POINT)
      (";"				Text		(SPACE 1)))
     (("EXPORTS"			Text		 nil)
      (";"				Text		(SPACE 1)))
     ((EPSILON)))

    ("Imports"		
     (("IMPORTS"			Text		nil)
      ("SymbolsFromModuleList"		NonTerminal	(LINE 0) POINT)
      (";"				Text		(SPACE 1)))
     (("IMPORTS"			Text		 nil)
      (";"				Text		(SPACE 1)))
     ((EPSILON)))

    ("SymbolList"		
     (("Symbol"				NonTerminal	nil POINT)
      ("SymbolListTail"			NonTerminal	(SPACE 1)))
     (("Symbol"				NonTerminal	 nil POINT)))

    ("SymbolListTail"		
     ((","				Text		 nil)
      ("Symbol"				NonTerminal	(SPACE 1) POINT)
      ("SymbolListTail"			NonTerminal	(SPACE 1)))
     ((","				Text		  nil)
      ("Symbol"				NonTerminal	(SPACE 1) POINT))
     ((EPSILON)))

    ("SymbolsFromModuleList"		
     (("SymbolsFromModule"		NonTerminal	nil POINT)
      ("SymbolsFromModuleList"		NonTerminal	(LINE 0)))
     (("SymbolsFromModule"		NonTerminal	 nil POINT))
     ((EPSILON)))
     
    ("SymbolsFromModule"		
     (("SymbolList"			NonTerminal	nil POINT)
      ("FROM"				Text		(LINE 0))
      ("modulereference"		Terminal	(SPACE 1))
      ("AssignedIdentifier"		NonTerminal	(SPACE 1))))

    ("OIDValue"
     (("{"				Text		 nil)
      ("OIDComponent1"			NonTerminal	(SPACE 1) POINT)
      ("OIDComponentsList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1))))

    ("OIDComponent1"
     (("itu-t(0)"			Text		  nil))
     (("iso(1)"				Text		  nil))
     (("joint-iso-itu-t(2)"		Text		  nil))
     (("number"				Terminal	 nil POINT))
     (("valuereference"			Terminal	 nil POINT))
     (("modulereference"		Terminal	nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1)))
     (("valuereference"			Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("modulereference"		Terminal	 nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1))
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1))))
    
    ("OIDComponentsList"		
     (("OIDComponents"			NonTerminal	nil POINT)
      ("OIDComponentsList"		NonTerminal	(SPACE 1)))
     (("OIDComponents"			NonTerminal	 nil POINT))
     ((EPSILON)))

    ("OIDComponents"
     (("identifier"			Terminal	 nil POINT)
      ("("				Text		(SPACE 1))
      ("NumberForm"			NonTerminal	(SPACE 1))
      (")"				Text		(SPACE 1)))
     (("identifier"			Terminal	nil POINT))
     (("NumberForm"			NonTerminal	 nil POINT))
     (("valuereference"			Terminal	 nil POINT))
     (("modulereference"		Terminal	nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1)))
     (("valuereference"			Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("modulereference"		Terminal	 nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1))
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1))))


    ("NumberForm"
     (("number"				Terminal	nil POINT))
     (("valuereference"			Terminal	 nil POINT))
     (("modulereference"		Terminal	 nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1)))
     (("valuereference"			Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("modulereference"		Terminal	 nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1))
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1))))

    ("AssignedIdentifier"	
     (("{"				Text		 nil)
      ("OIDComponent1"			NonTerminal	(SPACE 1) POINT)
      ("OIDComponentsList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("valuereference"			Terminal	 nil POINT))
     (("modulereference"		Terminal	 nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1)))
     (("valuereference"			Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("modulereference"		Terminal	 nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1))
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1)))
     ((EPSILON)))

    ("Symbol"
     (("typereference"			Terminal	nil POINT))
     (("valuereference"			Terminal	 nil POINT))
     (("objectclassreference"		Terminal	 nil POINT))
     (("objectreference"		Terminal	 nil POINT))
     (("objectsetreference"		Terminal	 nil POINT))
     (("typereference"			Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("valuereference"			Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("objectclassreference"		Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("objectreference"		Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("objectsetreference"		Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("}"				Text		(SPACE 1))))

    ;;------- assign1

    ("AssignmentListTail"
     (("Assignment"			NonTerminal	(LINE 0))
      ("AssignmentListTail"		NonTerminal	(LINE 0)))
     (("Assignment"			NonTerminal	(LINE 0)))
     ((EPSILON)))
    
    ("Assignment"
     (("typereference"			Terminal	 nil POINT)
      ("::="				Text		(SPACE 1))
      ("Type"				NonTerminal	(SPACE 1)))
     (("valuereference"			Terminal	 nil POINT)
      ("Type"				NonTerminal	(SPACE 1))
      ("::="				Text		(SPACE 1))
      ("Value"				NonTerminal	(SPACE 1)))
     (("typereference"			Terminal	 nil POINT)
      ("Type"				NonTerminal	(SPACE 1))
      ("::="				Text		(SPACE 1))
      ("ValueSet"			NonTerminal	(SPACE 1)))
     (("objectclassreference"		Terminal	 nil POINT)
      ("::="				Text		(SPACE 1))
      ("ObjectClass"			NonTerminal	(SPACE 1)))
     (("objectreference"		Terminal	 nil POINT)
      ("DefinedObjectClass"		NonTerminal	(SPACE 1))
      ("::="				Text		(SPACE 1))
      ("Object"				NonTerminal	(SPACE 1)))
     (("objectsetreference"		Terminal	 nil POINT)
      ("DefinedObjectClass"		NonTerminal	(SPACE 1))
      ("::="				Text		(SPACE 1))
      ("ObjectSet"			NonTerminal	(SPACE 1)))
     (("ParameterizedAssignment"	NonTerminal	 nil POINT)))


     ("ValueSet"	
      (("{"				Text		  nil)
       ("ElementSetSpecs"		NonTerminal	(SPACE 1) POINT)
       ("}"				Text		(SPACE 1))))
     
     ("ParameterizedAssignment"
      (("typereference"			Terminal	 nil POINT)
       ("{"				Text		(SPACE 1))
       ("ParameterList"			NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1))
       ("::="				Text		(SPACE 1))
       ("Type"				NonTerminal	(SPACE 1)))
      (("valuereference"		Terminal	 nil POINT)
       ("{"				Text		(SPACE 1))
       ("ParameterList"			NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1))
       ("Type"				NonTerminal	(SPACE 1))
       ("::="				Text		(SPACE 1))
       ("Value"				NonTerminal	(SPACE 1)))
      (("typereference"			Terminal	 nil POINT)
       ("{"				Text		(SPACE 1))
       ("ParameterList"			NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1))
       ("Type"				NonTerminal	(SPACE 1))
       ("::="				Text		(SPACE 1))
       ("ValueSet"			NonTerminal	(SPACE 1)))
      (("objectclassreference"		Terminal	 nil POINT)
       ("{"				Text		(SPACE 1))
       ("ParameterList"			NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1))
       ("::="				Text		(SPACE 1))
       ("ObjectClass"			NonTerminal	(SPACE 1)))
      (("objectreference"		Terminal	 nil POINT)
       ("{"				Text		(SPACE 1))
       ("ParameterList"			NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1))
       ("DefinedObjectClass"		NonTerminal	(SPACE 1))
       ("::="				Text		(SPACE 1))
       ("Object"			NonTerminal	(SPACE 1)))
      (("objectsetreference"		Terminal	 nil POINT)
       ("{"				Text		(SPACE 1))
       ("ParameterList"			NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1))
       ("DefinedObjectClass"		NonTerminal	(SPACE 1))
       ("::="				Text		(SPACE 1))
       ("ObjectSet"			NonTerminal	(SPACE 1))))

    ;;------- type1

     ("Type"
      (("typereference"			Terminal	 nil POINT))
      (("BasicType"			NonTerminal	 nil POINT))
      (("StringAndDateTypes"		NonTerminal	 nil POINT))
      (("ConstructedType"		NonTerminal	 nil POINT))
      (("Type"				NonTerminal	 nil POINT)
       ("("				Text		(SPACE 1))
       ("Constraint"			NonTerminal	(SPACE 1))
       (")"				Text		(SPACE 1)))
      (("TaggedType"			NonTerminal	 nil POINT))
      (("ParameterizedType"		NonTerminal	 nil POINT))
      (("modulereference"		Terminal	 nil POINT)
       ("."				Text		(SPACE 1))
       ("typereference"			Terminal	(SPACE 1)))
      (("identifier"			Terminal	 nil POINT)
       ("<"				Text		(SPACE 1))
       ("Type"				NonTerminal	(SPACE 1)))
      (("DefinedObjectClass"		NonTerminal	 nil POINT)
       ("."				Text		(SPACE 1))
       ("FieldName"			NonTerminal	(SPACE 1)))
      (("ReferencedObjects"		NonTerminal	 nil POINT)
       ("."				Text		(SPACE 1))
       ("FieldName"			NonTerminal	(SPACE 1))))

     ("BasicType"
      (("BIT STRING"			Text		  nil))
      (("BIT STRING"			Text		  nil)
       ("{"				Text		(SPACE 1))
       ("NamedBit"			NonTerminal	(SPACE 1) POINT)
       ("NamedBitListTail"		NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1)))
      (("BOOLEAN"			Text		  nil))
      (("EMBEDDED PDV"			Text		  nil))
      (("ENUMERATED"			Text		  nil)
       ("{"				Text		(SPACE 1))
       ("EnumerationItem"		NonTerminal	(SPACE 1) POINT)
       ("EnumerationListTail"		NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1)))
      (("EXTERNAL"			Text		  nil))
      (("INSTANCE OF"			Text		  nil)
       ("DefinedObjectClass"		NonTerminal	(SPACE 1) POINT))
      (("INTEGER"			Text		  nil))
      (("INTEGER"			Text		  nil)
       ("{"				Text		(SPACE 1))
       ("NamedNumber"			NonTerminal	(SPACE 1) POINT)
       ("NamedNumberListTail"		NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1)))
      (("NULL"				Text		  nil))
      (("OBJECT IDENTIFIER"		Text		  nil))
      (("OCTET STRING"			Text		  nil))
      (("REAL"				Text		  nil))
      (("RELATIVE-OID"			Text		  nil)))

     ("StringAndDateTypes"
      (("BMPString"			Text		  nil))
      (("GeneralString"			Text		  nil))
      (("GraphicString"			Text		  nil))
      (("IA5String"			Text		  nil))
      (("ISO646String"			Text		  nil))
      (("NumericString"			Text		  nil))
      (("PrintableString"		Text		  nil))
      (("TeletexString"			Text		  nil))
      (("T61String"			Text		  nil))
      (("UniversalString"		Text		  nil))
      (("VideotexString"		Text		  nil))
      (("VisibleString"			Text		  nil))
      (("CHARACTER STRING"		Text		  nil))
      (("GeneralizedTime"		Text		  nil))
      (("UTCTime"			Text		  nil))
      (("UTF8String"			Text		  nil))
      (("ObjectDescriptor"		Text		  nil)))

     ; sun
;("identifier"			Terminal	 nil POINT)
     ("ConstructedType"
      (("CHOICE"			Text		  nil)
       ("{"				Text		(SPACE 1))
       ("AlternativeTypeLists"		NonTerminal	(LINE 2) POINT)
       ("}"				Text		(LINE 0)))
      (("SEQUENCE"			Text		  nil)
       ("{"				Text		(SPACE 1))
       ("ComponentTypeLists"		NonTerminal	(LINE 2) POINT)
       ("}"				Text		(LINE 0)))
      (("SEQUENCE OF"			Text		  nil)
       ("Type"				NonTerminal	(SPACE 1) POINT))
      (("SEQUENCE"			Text		  nil)
       ("("				Text		(SPACE 1))
       ("SIZE"				Text		  nil)
       ("("				Text		(SPACE 1))
       ("Constraint"			NonTerminal	(SPACE 1) POINT)
       (")"				Text		(SPACE 1))
       (")"				Text		(SPACE 1))
       ("OF"				Text		(SPACE 1))
       ("Type"				NonTerminal	(SPACE 1)))
      (("SET"				Text		  nil)
       ("{"				Text		(SPACE 1))
       ("ComponentTypeLists"		NonTerminal	(LINE 2) POINT)
       ("}"				Text		(LINE 0)))
      (("SET OF"			Text		  nil)
       ("Type"				NonTerminal	(SPACE 1) POINT))
      (("SET"				Text		  nil)
       ("("				Text		(SPACE 1))
       ("SIZE"				Text		  nil)
       ("("				Text		(SPACE 1))
       ("Constraint"			NonTerminal	(SPACE 1) POINT)
       (")"				Text		(SPACE 1))
       (")"				Text		(SPACE 1))
       ("OF"				Text		(SPACE 1))
       ("Type"				NonTerminal	(SPACE 1))))

     ("TaggedType"
      (("Tag"				NonTerminal	 nil POINT)
       ("Type"				NonTerminal	(SPACE 1)))
      (("Tag"				NonTerminal	 nil POINT)
       ("IMPLICIT"			Text		(SPACE 1))
       ("Type"				NonTerminal	(SPACE 1)))
      (("Tag"				NonTerminal	 nil POINT)
       ("EXPLICIT"			Text		(SPACE 1))
       ("Type"				NonTerminal	(SPACE 1))))

     ("NamedBit"
      (("identifier"			Terminal	 nil POINT)
       ("("				Text		(SPACE 1))
       ("number"			Terminal	(SPACE 1))
       (")"				Text		(SPACE 1)))
      (("identifier"			Terminal	 nil POINT)
       ("("				Text		(SPACE 1))
       ("DefinedValue"			NonTerminal	(SPACE 1))
       (")"				Text		(SPACE 1))))

     ("NamedBitListTail"
      (("NamedBit"			NonTerminal	(SPACE 1) POINT)
       ("NamedBitListTail"		NonTerminal	(SPACE 1)))
      (("NamedBit"			NonTerminal	(SPACE 1) POINT))
      ((EPSILON)))

     ("AlternativeTypeLists"
      (("AlternativeTypeList"		NonTerminal	 nil POINT))
      (("AlternativeTypeList"		NonTerminal	 nil POINT)
       (","				Text		(SPACE 1))
       ("ExtensionAndException"		NonTerminal	(LINE 0)))
      (("AlternativeTypeList"		NonTerminal	 nil POINT)
       (","				Text		(SPACE 1))
       ("ExtensionAndException"		NonTerminal	(LINE 0))
       (","				Text		(SPACE 1))
       ("AlternativeTypeList"		NonTerminal	(LINE 0))))
     
     ("AlternativeTypeList"
      (("identifier"			Terminal	 nil POINT)
       ("Type"				NonTerminal	(SPACE 1))
       ("AlternativeTypeListTail"	NonTerminal	(SPACE 1)))
      (("identifier"			Terminal	 nil POINT)
       ("Type"				NonTerminal	(SPACE 1))))

     ("AlternativeTypeListTail"
      ((","				Text		  nil)
       ("identifier"			Terminal	(LINE 0) POINT)
       ("Type"				NonTerminal	(SPACE 1))
       ("AlternativeTypeListTail"	NonTerminal	(SPACE 1)))
      ((","				Text		  nil)
       ("identifier"			Terminal	(LINE 0) POINT)
       ("Type"				NonTerminal	(SPACE 1)))
      ((EPSILON)))

     ("EnumerationListTail"
       ((","				Text		  nil)
	("EnumerationItem"		NonTerminal	(SPACE 1) POINT)
	("EnumerationListTail"		NonTerminal	(SPACE 1)))
       ((","				Text		  nil)
	("EnumerationItem"		NonTerminal	(SPACE 1) POINT))
       ((EPSILON)))

     ("EnumerationItem"
      (("identifier"			Terminal	 nil POINT)
       ("("				Text		(SPACE 1))
       ("number"			Terminal	(SPACE 1))
       (")"				Text		(SPACE 1)))
      (("identifier"			Terminal	 nil POINT)
       ("("				Text		(SPACE 1))
       ("-"				Text		nil)
       ("number"			Terminal	(SPACE 1))
       (")"				Text		(SPACE 1)))
      (("identifier"			Terminal	 nil POINT)
       ("("				Text		(SPACE 1))
       ("DefinedValue"			NonTerminal	(SPACE 1))
       (")"				Text		(SPACE 1)))
      (("identifier"			Terminal	 nil POINT))
      (("..."				Text		  nil)))


     ("DefinedObjectClass"
      (("modulereference"		Terminal	 nil POINT)
       ("."				Text		(SPACE 1))
       ("objectclassreference"		Terminal	(SPACE 1)))
      (("objectclassreference"		Terminal	 nil POINT))
      (("TYPE-IDENTIFIER"		Text		  nil))
      (("ABSTRACT-SYNTAX"		Text		  nil)))

     
     ("NamedNumberListTail"
       ((","				Text		  nil)
	("NamedNumber"			NonTerminal	(SPACE 1) POINT)
	("NamedNumberListTail"		NonTerminal	(SPACE 1)))
       ((","				Text		  nil)
	("NamedNumber"			NonTerminal	(SPACE 1) POINT))
       ((EPSILON)))

     ("NamedNumber"
      (("identifier"			Terminal	 nil POINT)
       ("("				Text		(SPACE 1))
       ("number"			Terminal	(SPACE 1))
       (")"				Text		(SPACE 1)))
      (("identifier"			Terminal	 nil POINT)
       ("("				Text		(SPACE 1))
       ("-"				Text		nil)
       ("number"			Terminal	(SPACE 1))
       (")"				Text		(SPACE 1)))
      (("identifier"			Terminal	 nil POINT)
       ("("				Text		(SPACE 1))
       ("DefinedValue"			NonTerminal	(SPACE 1))
       (")"				Text		(SPACE 1))))

     ("ExtensionAndException"
      (("..."				Text		  nil))
      (("..."				Text		 nil)
       ("!"				Text		(SPACE 1))
       ("ExceptionIdentification"	NonTerminal	(SPACE 1) POINT)))

     ("ExceptionIdentification"
      (("number"			Terminal	 nil POINT))
      (("-"				Text		 nil)
       ("number"			Terminal	(SPACE 1) POINT))
      (("DefinedValue"			NonTerminal	 nil POINT))
      (("Type"				NonTerminal	 nil POINT)
       (":"				Text		(SPACE 1))
       ("Value"				NonTerminal	(SPACE 1))))
     
     ("ComponentTypeLists"
      (("ComponentTypeList"		NonTerminal	 nil POINT))
      (("ComponentTypeList"		NonTerminal	 nil POINT)
       (","				Text		(SPACE 1))
       ("ExtensionAndException"		NonTerminal	(LINE 0)))
      (("ComponentTypeList"		NonTerminal	 nil POINT)
       (","				Text		(SPACE 1))
       ("ExtensionAndException"		NonTerminal	(LINE 0))
       (","				Text		(SPACE 1))
       ("ComponentTypeList"		NonTerminal	(LINE 0)))
      (("ExtensionAndException"		NonTerminal	 nil POINT)
       (","				Text		(SPACE 1))
       ("ComponentTypeList"		NonTerminal	(LINE 0))))

      ("ComponentTypeList"
       (("ComponentType"		NonTerminal	 nil POINT)
	("ComponentTypeListTail"	NonTerminal	(SPACE 1)))
       (("ComponentType"		NonTerminal	 nil POINT)))
      
     ("ComponentTypeListTail"
       ((","				Text		  nil)
	("ComponentType"		NonTerminal	(LINE 0) POINT)
	("ComponentTypeListTail"	NonTerminal	(SPACE 1)))
       ((","				Text		  nil)
	("ComponentType"		NonTerminal	(LINE 0) POINT))
      (("ExtensionAndException"		NonTerminal	nil POINT))
       ((EPSILON)))

     ("ComponentType"
      (("identifier"			Terminal	 nil POINT)
       ("Type"				NonTerminal	(SPACE 1)))
      (("identifier"			Terminal	 nil POINT)
       ("Type"				NonTerminal	(SPACE 1))
       ("OPTIONAL"			Text		(SPACE 1)))
      (("identifier"			Terminal	 nil POINT)
       ("Type"				NonTerminal	(SPACE 1))
       ("DEFAULT"			Text		(SPACE 1))
       ("Value"				NonTerminal	(SPACE 1)))
      (("COMPONENTS OF"			Text		  nil)
       ("Type"				NonTerminal	(SPACE 1) POINT)))
     
     ("Tag"
      (("["				Text		  nil)
       ("number"			Terminal	(SPACE 1) POINT)
       ("]"				Text		(SPACE 1)))
      (("["				Text		  nil)
       ("Class"				NonTerminal	(SPACE 1) POINT)
       ("number"			Terminal	(SPACE 1))
       ("]"				Text		(SPACE 1)))
      (("["				Text		  nil)
       ("DefinedValue"			NonTerminal	(SPACE 1) POINT)
       ("]"				Text		(SPACE 1)))
      (("["				Text		  nil)
       ("Class"				NonTerminal	(SPACE 1) POINT)
       ("DefinedValue"			NonTerminal	(SPACE 1))
       ("]"				Text		(SPACE 1))))
     
     ("Class"
      (("UNIVERSAL"			Text		  nil))
      (("APPLICATION"			Text		  nil))
      (("PRIVATE"			Text		  nil)))

     ("DefinedValue"		
      (("valuereference"		Terminal	 nil POINT))
      (("modulereference"		Terminal	 nil POINT)
       ("."				Text		(SPACE 1))
       ("valuereference"		Terminal	(SPACE 1)))
      (("valuereference"		Terminal	 nil POINT)
       ("{"				Text		(SPACE 1))
       ("ActualParameterList"		NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1)))
      (("modulereference"		Terminal	 nil POINT)
       ("."				Text		(SPACE 1))
       ("valuereference"		Terminal	(SPACE 1))
       ("{"				Text		(SPACE 1))
       ("ActualParameterList"		NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1))))
     
     ("ParameterizedType"
      (("typereference"			Terminal	 nil POINT)
       ("{"				Text		(SPACE 1))
       ("ActualParameterList"		NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1)))
      (("modulereference"		Terminal	 nil POINT)
       ("."				Text		(SPACE 1))
       ("typereference"			Terminal	(SPACE 1))
       ("{"				Text		(SPACE 1))
       ("ActualParameterList"		NonTerminal	(SPACE 1))
       ("}"				Text		(SPACE 1))))

      ("ReferencedObjects"
       (("objectreference"		Terminal	 nil POINT))
       (("objectsetreference"		Terminal	 nil POINT))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("objectreference"		Terminal	(SPACE 1)))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("objectsetreference"		Terminal	(SPACE 1)))
       (("objectreference"		Terminal	 nil POINT)
	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("objectsetreference"		Terminal	 nil POINT)
	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("objectreference"		Terminal	(SPACE 1))
	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("objectsetreference"		Terminal	(SPACE 1))
	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1))))
      
      ;;------ value1

      ("Value"
       (("valuereference"		Terminal	 nil POINT))
       (("BasicValue"			NonTerminal	 nil POINT))
       (("CharacterStringValue"		NonTerminal	 nil POINT))
       (("ConstructedValue"		NonTerminal	 nil POINT))
       (("valuereference"		Terminal	 nil POINT)
 	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("valuereference"		Terminal	(SPACE 1)))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("valuereference"		Terminal	(SPACE 1))
 	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("Type"				NonTerminal	 nil POINT)
 	(":"				Text		(SPACE 1))
	("Value"			NonTerminal	(SPACE 1)))
       (("ValueFromObject"		NonTerminal	 nil POINT)))

      ("BasicValue"
       (("BitStringValue"		NonTerminal	 nil POINT))
       (("BooleanValue"			NonTerminal	 nil POINT))
       (("EmbeddedPDVValue"		NonTerminal	 nil POINT))
       (("EnumeratedValue"		NonTerminal	 nil POINT))
       (("ExternalValue"		NonTerminal	 nil POINT))
       (("InstanceOfValue"		NonTerminal	 nil POINT))
       (("IntegerValue"			NonTerminal	 nil POINT))
       (("NullValue"			NonTerminal	 nil POINT))
       (("OIDValue"			NonTerminal	 nil POINT))
       (("OctetStringValue"		NonTerminal	 nil POINT))
       (("RealValue"			NonTerminal	 nil POINT)))
      
      ("ConstructedValue"
       (("ChoiceValue"			NonTerminal	 nil POINT))
       (("SequenceValue"		NonTerminal	 nil POINT))
       (("SequenceOfValue"		NonTerminal	 nil POINT))
       (("SetValue"			NonTerminal	 nil POINT))
       (("SetOfValue"			NonTerminal	 nil POINT)))

      ("BitStringValue"
       (("bitstring"			Terminal	 nil POINT))
       (("hexastring"			Terminal	 nil POINT))
       (("{"				Text		  nil)
	("identifier"			Terminal	(SPACE 1) POINT)
	("IdentifierListTail"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("{"				Text		  nil)
	("}"				Text		(SPACE 1))))

      ("IdentifierListTail"
       ((","				Text		  nil)
	("identifier"			Terminal	(SPACE 1) POINT)
	("IdentifierListTail"		NonTerminal	(SPACE 1)))
       ((","				Text		  nil)
	("identifier"			Terminal	(SPACE 1) POINT))
       ((EPSILON)))

      ("CharacterStringValue"
       (("{"				Text		  nil)
	("identification"		Text		(SPACE 1) POINT)
	("ChoiceValue"			NonTerminal	(SPACE 1))
	(","				Text		  nil)
	("string-value"			Text		(LINE 0))
	("ChoiceValue"			NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("charsstring"			Terminal	 nil POINT))
       (("{"				Text		  nil)
	("CharsString"			NonTerminal	(SPACE 1) POINT)
	("CharsStringListTail"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("{"				Text		  nil)
	("number"			Terminal	(SPACE 1) POINT)
	(","				Text		(SPACE 1))
	("number"			Terminal	(SPACE 1))
	(","				Text		(SPACE 1))
	("number"			Terminal	(SPACE 1))
	(","				Text		(SPACE 1))
	("number"			Terminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("{"				Text		  nil)
	("number"			Terminal	(SPACE 1) POINT)
	(","				Text		(SPACE 1))
	("number"			Terminal	(SPACE 1))
	("}"				Text		(SPACE 1))))

      ("CharsStringListTail"
       ((","				Text		  nil)
	("CharsString"			NonTerminal	(SPACE 1) POINT)
	("CharsStringListTail"		NonTerminal	(SPACE 1)))
       ((","				Text		  nil)
	("CharsString"			NonTerminal	(SPACE 1) POINT))
       ((EPSILON)))

      ("CharsString"
       (("charsstring"			Terminal	 nil POINT))
       (("valuereference"		Terminal	 nil POINT))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("valuereference"		Terminal	(SPACE 1)))
       (("valuereference"		Terminal	 nil POINT)
	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("valuereference"		Terminal	(SPACE 1))
	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1))))

      ("BooleanValue"
       (("TRUE"				Text		  nil))
       (("FALSE"			Text		  nil)))

      ("ChoiceValue"
       (("identifier"			Terminal	 nil POINT)
	(":"				Text		(SPACE 1))
	("Value"			NonTerminal	(SPACE 1))))

      ("EmbeddedPDVValue"
       (("{"				Text		  nil)
	("identification"		Text		(SPACE 1) POINT)
	("ChoiceValue"			NonTerminal	(SPACE 1))
	(","				Text		  nil)
	("data-value"			Text		(LINE 0))
	("ChoiceValue"			NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1))))

      ("EnumeratedValue"
       (("identifier"			Terminal	 nil POINT)))
       
      ("ExternalValue"
       (("{"				Text		  nil)
	("type-id"			Text		(SPACE 1) POINT)
	("OIDValue"			NonTerminal	(SPACE 1))
	(","				Text		  nil)
	("base"				Text		(LINE 0))
	("Value"			NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1))))

      ("IntegerValue"
       (("number"			Terminal	 nil POINT))
       (("-"				Text		  nil)
	("number"			Terminal	(SPACE 1) POINT))
       (("identifier"			Terminal	 nil POINT)))

      ("NullValue"
       (("NULL"				Text		  nil)))

      ("OctetStringValue"
       (("bitstring"			Terminal	 nil POINT))
       (("hexastring"			Terminal	 nil POINT)))

      ("RealValue"
       (("0"				Text		  nil))
       (("{"				Text		  nil)
	("mantissa"			Text		(SPACE 1))
	("IntegerValue"			NonTerminal	(SPACE 1) POINT)
	(","				Text		  nil)
	("base"				Text		(LINE 0))
	("Base"				NonTerminal	(SPACE 1))
	(","				Text		  nil)
	("exponent"			Text		(LINE 0))
	("IntegerValue"			NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("PLUS-INFINITY"		Text		(SPACE 1)))
       (("MINUS-INFINITY"		Text		(SPACE 1))))

      ("Base"
       (("2"				Text		  nil))
       (("10"				Text		  nil))
       (("valuereference"		Terminal	 nil POINT))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("valuereference"		Terminal	(SPACE 1)))
       (("valuereference"		Terminal	 nil POINT)
	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("modulereference"		Terminal	 nil POINT)
	("."				Text		(SPACE 1))
	("valuereference"		Terminal	(SPACE 1))
	("{"				Text		(SPACE 1))
	("ActualParameterList"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("Type"				NonTerminal	 nil POINT)
	(":"				Text		(SPACE 1))
	("Value"			NonTerminal	(SPACE 1)))
       (("ValueFromObject"		NonTerminal	 nil POINT)))

    ("RelativeOIDValue"
     (("{"				Text		 nil)
      ("RelativeOIDComponents"		NonTerminal	(SPACE 1) POINT)
      ("RelativeOIDComponentsList"	NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1))))

    ("RelativeOIDComponentsList"		
     (("RelativeOIDComponents"		NonTerminal	nil POINT)
      ("RelativeOIDComponentsList"	NonTerminal	(SPACE 1)))
     (("RelativeOIDComponents"		NonTerminal	 nil POINT))
     ((EPSILON)))

    ("RelativeOIDComponents"
     (("identifier"			Terminal	 nil POINT)
      ("("				Text		(SPACE 1))
      ("NumberForm"			NonTerminal	(SPACE 1))
      (")"				Text		(SPACE 1)))
     (("NumberForm"			NonTerminal	 nil POINT))
     (("valuereference"			Terminal	 nil POINT))
     (("modulereference"		Terminal	nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1)))
     (("valuereference"			Terminal	 nil POINT)
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1)))
     (("modulereference"		Terminal	 nil POINT)
      ("."				Text		(SPACE 1))
      ("valuereference"			Terminal	(SPACE 1))
      ("{"				Text		(SPACE 1))
      ("ActualParameterList"		NonTerminal	(SPACE 1))
      ("}"				Text		(SPACE 1))))

    ("SequenceValue"
       (("{"				Text		  nil)
	("identifier"			Terminal	(SPACE 1) POINT)
	("Value"			NonTerminal	(SPACE 1))
	("ComponentValueListTail"	NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("{"				Text		  nil)
	("}"				Text		(SPACE 1))))

      
      ("ComponentValueListTail"
       ((","				Text		  nil)
	("identifier"			Terminal	(LINE 0) POINT)
	("Value"			NonTerminal	(SPACE 1))
	("ComponentValueListTail"	NonTerminal	(SPACE 1)))
       ((","				Text		  nil)
	("identifier"			Terminal	(LINE 0) POINT)
	("Value"			NonTerminal	(SPACE 1)))
       ((EPSILON)))

      ("SequenceOfValue"
       (("{"				Text		  nil POINT)
	("Value"			NonTerminal	(SPACE 1))
	("ValueListTail"		NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("{"				Text		  nil POINT)
	("}"				Text		(SPACE 1))))

      ("SetValue"
       (("{"				Text		  nil)
	("identifier"			Terminal	(SPACE 1)  POINT)
	("Value"			NonTerminal	(SPACE 1))
	("ComponentValueListTail"	NonTerminal	(SPACE 1))
	("}"				Text		(SPACE 1)))
       (("{"				Text		  nil)
	("}"				Text		(SPACE 1))))

       ("SetOfValue"
	(("{"				Text		  nil)
	 ("Value"			NonTerminal	(SPACE 1) POINT)
	 ("ValueListTail"		NonTerminal	(SPACE 1))
	 ("}"				Text		(SPACE 1)))
	(("{"				Text		  nil)
	 ("}"				Text		(SPACE 1))))
       
       
       ("ValueListTail"
	((","				Text		  nil)
	 ("Value"			NonTerminal	(SPACE 1) POINT)
	 ("ValueListTail"		NonTerminal	(SPACE 1)))
	((","				Text		  nil)
	 ("Value"			NonTerminal	(SPACE 1) POINT))
       ((EPSILON)))

       ("ValueFromObject"
	(("ReferencedObjects"		NonTerminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("FieldName"			NonTerminal	(SPACE 1))))

       ;----- setconst1

       ("ElementSetSpecs"
	(("ElementSetSpec"		NonTerminal	 nil POINT))
	(("ElementSetSpec"		NonTerminal	 nil POINT)
	 (","				Text		(SPACE 1))
	 ("..."				Text		(LINE 0)))
	(("ElementSetSpec"		NonTerminal	 nil POINT)
	 (","				Text		(SPACE 1))
	 ("..."				Text		(LINE 0))
	 (","				Text		(SPACE 1))
	 ("ElementSetSpec"		NonTerminal	(LINE 0))))

       ("ElementSetSpec"
	(("ElementSetSpec"		NonTerminal	 nil POINT)
	 ("|"				Text		(SPACE 1))
	 ("ElementSetSpec"		NonTerminal	(SPACE 1)))
	(("ElementSetSpec"		NonTerminal	 nil POINT)
	 ("UNION"			Text		(SPACE 1))
	 ("ElementSetSpec"		NonTerminal	(SPACE 1)))
	(("ElementSetSpec"		NonTerminal	 nil POINT)
	 ("^"				Text		(SPACE 1))
	 ("ElementSetSpec"		NonTerminal	(SPACE 1)))
	(("ElementSetSpec"		NonTerminal	 nil POINT)
	 ("INTERSECTION"		Text		(SPACE 1))
	 ("ElementSetSpec"		NonTerminal	(SPACE 1)))
	(("Elements"			NonTerminal	 nil POINT)
	 ("EXCEPT"			Text		(SPACE 1))
	 ("Elements"			NonTerminal	(SPACE 1)))
	(("ALL EXCEPT"			Text		  nil)
	 ("Elements"			NonTerminal	(SPACE 1) POINT))
	(("SubtypeElements"		NonTerminal	 nil POINT))
	(("ObjectSetElements"		NonTerminal	 nil POINT))
	(("("				Text		  nil)
	 ("ElementSetSpec"		NonTerminal	(SPACE 1) POINT)
	 (")"				Text		(SPACE 1))))

       ("Elements"
	(("SubtypeElements"		NonTerminal	 nil POINT))
	(("ObjectSetElements"		NonTerminal	 nil POINT))
	(("("				Text		  nil)
	 ("ElementSetSpec"		NonTerminal	(SPACE 1) POINT)
	 (")"				Text		(SPACE 1))))

       ("SubtypeElements"
	(("Value"			NonTerminal	 nil POINT))
	(("INCLUDES"			Text		  nil)
	 ("Type"			NonTerminal	(SPACE 1) POINT))
	(("Type"			NonTerminal	 nil POINT))
	(("LowerEndPoint"		NonTerminal	 nil POINT)
	 (".."				Text		(SPACE 1))
	 ("UpperEndPoint"		NonTerminal	(SPACE 1)))
	(("FROM"			Text		  nil)
	 ("("				Text		(SPACE 1))
	 ("Constraint"			NonTerminal	(SPACE 1) POINT)
	 (")"				Text		(SPACE 1)))
	(("SIZE"			Text		  nil)
	 ("("				Text		(SPACE 1))
	 ("Constraint"			NonTerminal	(SPACE 1) POINT)
	 (")"				Text		(SPACE 1)))
	(("WITH COMPONENT"		Text		  nil)
	 ("("				Text		(SPACE 1))
	 ("Constraint"			NonTerminal	(SPACE 1) POINT)
	 (")"				Text		(SPACE 1)))
	(("WITH COMPONENTS"		Text		  nil)
	 ("{"				Text		  nil)
	 ("MultipleTypeConstraints"	NonTerminal	(SPACE 1) POINT)
	 ("}"				Text		(SPACE 1))))

       ("LowerEndPoint"
	 (("Value"			NonTerminal	 nil POINT))
	 (("MIN"			Text		  nil))
	 (("Value"			NonTerminal	 nil POINT)
	  ("<"				Text		(SPACE 1)))
	 (("MIN"			Text		  nil)
	  ("<"				Text		(SPACE 1))))

       ("UpperEndPoint"
	 (("Value"			NonTerminal	 nil POINT))
	 (("MAX"			Text		  nil))
	 (("<"				Text		  nil)
	  ("Value"			NonTerminal	(SPACE 1) POINT))
	 (("<"				Text		  nil)
	  ("MAX"			Text		(SPACE 1))))

       ("MultipleTypeConstraints"
	(("NamedConstraint"		NonTerminal	(SPACE 1) POINT)
	 ("TypeConstraintListTail"	NonTerminal	(SPACE 1)))
	(("..."				Text		(SPACE 1))
	 (","				Text		(SPACE 1))
	 ("NamedConstraint"		NonTerminal	(LINE 0) POINT)
	 ("TypeConstraintListTail"	NonTerminal	(SPACE 1))))

       ("TypeConstraintListTail"
	((","				Text		  nil)
	 ("NamedConstraint"		NonTerminal	(LINE 0) POINT)
	 ("TypeConstraintListTail"	NonTerminal	(SPACE 1)))
	((","				Text		  nil)
	 ("NamedConstraint"		NonTerminal	(LINE 0) POINT))
       ((EPSILON)))

       ("NamedConstraint"
	(("identifier"			Terminal	 nil POINT)
	 ("PresenceConstraint"		NonTerminal	(SPACE 1)))
	(("identifier"			Terminal	 nil POINT)
	 ("("				Text		(SPACE 1))
	 ("Constraint"			NonTerminal	(SPACE 1))
	 (")"				Text		(SPACE 1))
	 ("PresenceConstraint"		NonTerminal	(SPACE 1))))

       ("PresenceConstraint"
	(("PRESENT"			Text		  nil))
	(("ABSENT"			Text		  nil)	)
	(("OPTIONAL"			Text		  nil))
	((EPSILON)))

       ("ObjectSetSpec"
	(("ElementSetSpec"		NonTerminal	 nil POINT))
	(("ElementSetSpec"		NonTerminal	 nil POINT)
	 (","				Text		(SPACE 1))
	 ("..."				Text		(LINE 0)))
	(("ElementSetSpec"		NonTerminal	 nil POINT)
	 (","				Text		(SPACE 1))
	 ("..."				Text		(LINE 0))
	 (","				Text		(SPACE 1))
	 ("ElementSetSpec"		NonTerminal	(LINE 0)))	
	(("..."				Text		  nil))
	(("..."				Text		  nil)
	 (","				Text		(SPACE 1))
	 ("ElementSetSpec"		NonTerminal	(LINE 0) POINT)))

       ;---- const1 

       ("ExceptionSpec"
	(("!"				Text		  nil)
	 ("ExceptionIdentification"	NonTerminal	(SPACE 1)))
	((EPSILON)))
       
       ("Constraint"
	(("ConstraintSpec"		NonTerminal	 nil POINT))
	(("ConstraintSpec"		NonTerminal	 nil POINT)
	 ("ExceptionSpec"		NonTerminal	(SPACE 1))))

       ("ConstraintSpec"
	(("ElementSetSpecs"		NonTerminal	 nil POINT))
	(("ElementSetSpec"		NonTerminal	 nil POINT)
	 ("|"				Text		(SPACE 1))
	 ("ElementSetSpec"		NonTerminal	(SPACE 1)))
	(("Value"			NonTerminal	 nil POINT))
	(("Type"			NonTerminal	 nil POINT))
	(("INCLUDES"			Text		  nil)
	 ("Type"			NonTerminal	(SPACE 1) POINT))
	(("LowerEndPoint"		NonTerminal	 nil POINT)
	 (".."				Text		(SPACE 1))
	 ("UpperEndPoint"		NonTerminal	(SPACE 1)))
	(("FROM"			Text		  nil)
	 ("("				Text		(SPACE 1))
	 ("Constraint"			NonTerminal	(SPACE 1) POINT)
	 (")"				Text		(SPACE 1)))
	(("SIZE"			Text		  nil)
	 ("("				Text		(SPACE 1))
	 ("Constraint"			NonTerminal	(SPACE 1) POINT)
	 (")"				Text		(SPACE 1)))
	(("WITH COMPONENT"		Text		  nil)
	 ("("				Text		(SPACE 1))
	 ("Constraint"			NonTerminal	(SPACE 1) POINT)
	 (")"				Text		(SPACE 1)))
	(("WITH COMPONENTS"		Text		  nil)
	 ("{"				Text		(SPACE 1))
	 ("MultipleTypeConstraints"	NonTerminal	(SPACE 1) POINT)
	 ("}"				Text		(SPACE 1)))
	(("CONSTRAINED BY"		Text		  nil)
	 ("{"				Text		(SPACE 1))
	 ("UserDefinedConstraintList"	NonTerminal	(SPACE 1) POINT)
	 ("}"				Text		(SPACE 1)))
	(("TableConstraint"		NonTerminal	 nil POINT)))

       ("UserDefinedConstraintList"
	(("UserDefinedConstraintParameter" NonTerminal	 nil POINT)
	 ("UserDefinedConstraintListTail" NonTerminal	(SPACE 1)))
	(("UserDefinedConstraintParameter" NonTerminal	 nil POINT))
       ((EPSILON)))

       ("UserDefinedConstraintListTail"
	((","				Text		  nil)
	 ("UserDefinedConstraintParameter" NonTerminal	(SPACE 1) POINT)
	 ("UserDefinedConstraintListTail" NonTerminal	(SPACE 1)))
	((","				Text		  nil)
	 ("UserDefinedConstraintParameter" NonTerminal	(SPACE 1) POINT))
       ((EPSILON)))

       ("UserDefinedConstraintParameter"
	(("Governor"			NonTerminal	 nil POINT)
	 (":"				Text		(SPACE 1))
	 ("ActualParameter"		NonTerminal	(SPACE 1)))
	(("ActualParameter"		NonTerminal	 nil POINT)))

       ("TableConstraint"
	(("SimpleTableConstraint"	NonTerminal	 nil POINT))
	(("ComponentRelationConstraint"	NonTerminal	 nil POINT)))
       
       ("SimpleTableConstraint"
	(("{"				Text		  nil)
	 ("ObjectSetSpec"		NonTerminal	(SPACE 1) POINT)
	 ("}"				Text		(SPACE 1))))

       ("ComponentRelationConstraint"
	(("{"				Text		  nil)
	 ("DefinedObjectSet"		NonTerminal	(SPACE 1) POINT)
	 ("}"				Text		(SPACE 1))
	 ("{"				Text		(SPACE 1))
	 ("AtNotation"			NonTerminal	(SPACE 1))
	 ("AtNotationListTail"		NonTerminal	(SPACE 1))
	 ("}"				Text		(SPACE 1))))
	
       ("AtNotationListTail"
	((","				Text		  nil)
	 ("AtNotation"			NonTerminal	(SPACE 1) POINT)
	 ("AtNotationListTail"		NonTerminal	(SPACE 1)))
	((","				Text		  nil)
	 ("AtNotation"			NonTerminal	(SPACE 1) POINT))
	((EPSILON)))

       ("AtNotation"
	(("@"				Text		  nil)
	 ("ComponentIdList"		NonTerminal	(SPACE 1) POINT))
	(("@."				Text		  nil)
	 ("ComponentIdList"		NonTerminal	(SPACE 1) POINT)))

       ("ComponentIdList"
	(("identifier"			Terminal	 nil POINT)
	 ("ComponentIdListTail"		NonTerminal	(SPACE 1)))
	(("identifier"			Terminal	 nil POINT)))
       
       ("ComponentIdListTail"
	(("."				Text		  nil)
	 ("identifier"			Terminal	(SPACE 1) POINT)
	 ("ComponentIdListTail"		NonTerminal	(SPACE 1)))
	(("."				Text		  nil)
	 ("identifier"			Terminal	(SPACE 1) POINT))
	((EPSILON)))

       ;---- param1
       

       ("Setting"
	(("Type"			NonTerminal	 nil POINT))
	(("Value"			NonTerminal	 nil POINT))
	(("ValueSet"			NonTerminal	 nil POINT))
	(("Object"			NonTerminal	 nil POINT))
	(("ObjectSet"			NonTerminal	 nil POINT)))

       ("ParameterList"
	(("Parameter" 			NonTerminal	 nil POINT)
	 ("ParameterListTail" 		NonTerminal	(SPACE 1)))
	(("Parameter"			NonTerminal	 nil POINT)))

       ("ParameterListTail"
	((","				Text		  nil)
	 ("Parameter"			NonTerminal	(SPACE 1) POINT)
	 ("ParameterListTail"		NonTerminal	(SPACE 1)))
	((","				Text		  nil)
	 ("Parameter"			NonTerminal	(SPACE 1) POINT))
	((EPSILON)))

       ("Parameter"
	(("Reference"			NonTerminal	 nil POINT))
	(("Governor"			NonTerminal	 nil POINT)
	 (":"				Text		(SPACE 1))
	 ("Reference"			NonTerminal	(SPACE 1)))
	(("Reference"			NonTerminal	 nil POINT)
	 (":"				Text		(SPACE 1))
	 ("Reference"			NonTerminal	(SPACE 1))))

       ("ValueSet"
	(("{"				Text		  nil)
	 ("ElementSetSpecs"		NonTerminal	(SPACE 1) POINT)
	 ("}"				Text		(SPACE 1))))

       ("Governor"
	(("Type"			NonTerminal	 nil POINT))
	(("DefinedObjectClass"		NonTerminal	 nil POINT)))

       ("Reference"
	(("typereference"		Terminal	 nil POINT))
	(("valuereference"		Terminal	 nil POINT))
	(("objectclassreference"	Terminal	 nil POINT))
	(("objectreference"		Terminal	 nil POINT))
	(("objectsetreference"		Terminal	 nil POINT)))

       ("ActualParameterList"
	(("ActualParameter"		NonTerminal	 nil POINT)
	 ("ActualParameterListTail"	NonTerminal	(SPACE 1)))
	(("ActualParameter"		NonTerminal	 nil POINT)))

       ("ActualParameterListTail"
	((","				Text		  nil)
	 ("ActualParameter"		NonTerminal	(SPACE 1) POINT)
	 ("ActualParameterListTail"	NonTerminal	(SPACE 1)))
	((","				Text		  nil)
	 ("ActualParameter"		NonTerminal	(SPACE 1) POINT))
	((EPSILON)))

       ("ActualParameter"
	(("Type"			NonTerminal	 nil POINT))
	(("Value"			NonTerminal	 nil POINT))
	(("ValueSet"			NonTerminal	 nil POINT))
	(("Object"			NonTerminal	 nil POINT))
	(("ObjectSet"			NonTerminal	 nil POINT))
	(("DefinedObjectClass"		NonTerminal	 nil POINT)))
	

       ;---- object1

       ("Object"
	(("objectreference"		Terminal	 nil POINT))
	(("modulereference"		Terminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("objectreference"		Terminal	(SPACE 1)))
	(("DefaultSyntax"		NonTerminal	 nil POINT))
	(("DefinedSyntax"		NonTerminal	 nil POINT))
	(("ObjectFromObject"		NonTerminal	 nil POINT))
	(("ParameterizedObject"		NonTerminal	 nil POINT)))

;;    ("ObjectDefn"
;;     (("DefaultSyntax"		NonTerminal	 nil POINT))
;;     (("DefinedSyntax"		NonTerminal	 nil POINT)))

       ("DefaultSyntax"
	(("{"				Text		  nil)
	 ("FieldSettingList"		NonTerminal	(LINE 2) POINT)
	 ("}"				Text		(LINE 0))))

       ("FieldSettingList"
	(("FieldSetting"		NonTerminal	 nil POINT)
	 ("FieldSettingListTail"	NonTerminal	(SPACE 1)))
	(("FieldSetting"		NonTerminal	 nil POINT))
	 ((EPSILON)))

       ("FieldSettingListTail"
	((","				Text		  nil)
	 ("FieldSetting"		NonTerminal	(LINE 0) POINT)
	 ("FieldSettingListTail"	NonTerminal	(SPACE 1)))
	((","				Text		  nil)
	 ("FieldSetting"		NonTerminal	(LINE 0) POINT))
	((EPSILON)))
	 

       ("FiedSetting"
	(("PrimitiveFieldName"		NonTerminal	 nil POINT)
	 ("Setting"			NonTerminal	(SPACE 1))))

       ("DefinedSyntax"
	(("{"				Text		  nil)
	 ("DefinedSyntaxTokenList"	NonTerminal	(LINE 2) POINT)
	 ("}"				Text		(LINE 0))))

       ("DefinedSyntaxTokenList"
	(("DefinedSyntaxToken"		NonTerminal	 nil POINT)
	 ("DefinedSyntaxTokenListTail"	NonTerminal	(SPACE 1)))
	(("DefinedSyntaxToken"		NonTerminal	 nil POINT))
	((EPSILON)))

       ("DefinedSyntaxTokenListTail"
	(("DefinedSyntaxToken"		NonTerminal	nil POINT)
	 ("DefinedSyntaxTokenListTail"	NonTerminal	(SPACE 1)))
	(("DefinedSyntaxToken"		NonTerminal	nil POINT))
	((EPSILON)))

       ("DefinedSyntaxToken"
	(("word"			Terminal	 nil POINT))
	((","				Text		  nil)
	 (""				Text		(LINE 0)))
	;; la ligne precedente permet de faire un retour chariot apres
	;; la virgule
	(("Type"			NonTerminal	 nil POINT))
	(("Value"			NonTerminal	 nil POINT))
	(("ValueSet"			NonTerminal	 nil POINT))
	(("Object"			NonTerminal	 nil POINT))
	(("ObjectSet"			NonTerminal	 nil POINT)))

       ("ObjectSet"
	(("{"				Text		  nil)
	 ("ObjectSetSpec"		NonTerminal	(SPACE 1) POINT)
	 ("}"				Text		(SPACE 1))))

       ("ObjectSetElements"
	(("Object"			NonTerminal	 nil POINT))
	(("DefinedObjectSet"		NonTerminal	 nil POINT))
	(("ObjectSetFromObjects"	NonTerminal	 nil POINT))
	(("ParameterizedObjectSet"	NonTerminal	 nil POINT)))

       ("ObjectFromObject"
	(("ReferencedObjects"		NonTerminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("FieldName"			NonTerminal	(SPACE 1))))
	
       ("ObjectSetFromObjects"
	(("ReferencedObjects"		NonTerminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("FieldName"			NonTerminal	(SPACE 1))))

       ("ReferencedObjects"
	(("DefinedObject"		NonTerminal	 nil POINT))
	(("ParameterizedObject"		NonTerminal	 nil POINT))
	(("DefinedObjectSet"		NonTerminal	 nil POINT))
	(("ParameterizedObjectSet"	NonTerminal	 nil POINT)))

       ("DefinedObject"
	(("objectreference"		Terminal	 nil POINT))
	(("modulereference"		Terminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("objectreference"		Terminal	(SPACE 1))))
	
       ("ParameterizedObject"
	(("objectreference"		Terminal	 nil POINT)
	 ("{"				Text		(SPACE 1))
	 ("ActualParameterList"		NonTerminal	(SPACE 1))
	 ("}"				Text		(SPACE 1)))
	(("modulereference"		Terminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("objectreference"		Terminal	(SPACE 1))
	 ("{"				Text		(SPACE 1))
	 ("ActualParameterList"		NonTerminal	(SPACE 1))
	 ("}"				Text		(SPACE 1))))

       ("DefinedObjectSet"
	(("objectsetreference"		Terminal	 nil POINT))
	(("modulereference"		Terminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("objectsetreference"		Terminal	(SPACE 1))))
	
       ("ParameterizedObjectSet"
	(("objectsetreference"		Terminal	 nil POINT)
	 ("{"				Text		(SPACE 1))
	 ("ActualParameterList"		NonTerminal	(SPACE 1))
	 ("}"				Text		(SPACE 1)))
	(("modulereference"		Terminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("objectsetreference"		Terminal	(SPACE 1))
	 ("{"				Text		(SPACE 1))
	 ("ActualParameterList"		NonTerminal	(SPACE 1))
	 ("}"				Text		(SPACE 1))))

       ("ObjectClass"
	(("CLASS"			Text		  nil)
	 ("{"				Text		(SPACE 1))
	 ("FieldSpec"			NonTerminal	(LINE 2) POINT)
	 ("FieldSpecListTail"		NonTerminal	(SPACE 1))
	 ("}"				Text		(LINE 0)))
	(("CLASS"			Text		  nil)
	 ("{"				Text		(SPACE 1))
	 ("FieldSpec"			NonTerminal	(LINE 2) POINT)
	 ("FieldSpecListTail"		NonTerminal	(SPACE 1))
	 ("}"				Text		(LINE 0))
	 ("WITH SYNTAX"			Text		(LINE 0))
	 ("SyntaxList"			NonTerminal	(SPACE 1)))
	(("objectclassreference"	Terminal	 nil POINT))
	(("modulereference"		Terminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("objectclassreference"	Terminal	(SPACE 1)))
	(("TYPE-IDENTIFIER"		Text		  nil))
	(("ABSTRACT-SYNTAX"		Text		  nil))
	(("objectclassreference"	Terminal	 nil POINT)
	 ("{"				Text		(SPACE 1))
	 ("ActualParameterList"		NonTerminal	(SPACE 1))
	 ("}"				Text		(SPACE 1)))
	(("modulereference"		Terminal	 nil POINT)
	 ("."				Text		(SPACE 1))
	 ("objectclassreference"	Terminal	(SPACE 1))
	 ("{"				Text		(SPACE 1))
	 ("ActualParameterList"		NonTerminal	(SPACE 1))
	 ("}"				Text		(SPACE 1)))
	(("TYPE-IDENTIFIER"		Text		  nil)
	 ("{"				Text		(SPACE 1))
	 ("ActualParameterList"		NonTerminal	(SPACE 1) POINT)
	 ("}"				Text		(SPACE 1)))
	(("ABSTRACT-SYNTAX"		Text		  nil)
	 ("{"				Text		(SPACE 1))
	 ("ActualParameterList"		NonTerminal	(SPACE 1) POINT)
	 ("}"				Text		(SPACE 1))))

       ("FieldSpecListTail"
	((","				Text		  nil)
	 ("FieldSpec"			NonTerminal	(LINE 0) POINT)
	 ("FieldSpecListTail"		NonTerminal	(SPACE 1)))
	((","				Text		  nil)
	 ("FieldSpec"			NonTerminal	(LINE 0) POINT))
	((EPSILON)))

       ("SyntaxList"
	(("{"				Text		  nil)
	 ("TokenOrGroupSpec"		NonTerminal	(LINE 2) POINT)
	 ("TokenOrGroupSpecListTail"	NonTerminal	(SPACE 1))
	 ("}"				Text		(LINE 0))))
       
       ("TokenOrGroupSpecListTail"
	(("TokenOrGroupSpec"		NonTerminal	nil POINT)
	 ("TokenOrGroupSpecListTail"	NonTerminal	(SPACE 1)))
	(("TokenOrGroupSpec"		NonTerminal	nil POINT))
	((EPSILON)))

       ("TokenOrGroupSpec"
	(("word"			Terminal	 nil POINT))
	((","				Text		  nil)
	 (""				Text		(LINE 0)))
	;; la ligne precedente permet de faire un retour chariot apres
	;; la virgule
	(("typefieldreference"		Terminal	 nil POINT))
	(("valuefieldreference"		Terminal	 nil POINT))
	(("valuesetfieldreference"	Terminal	 nil POINT))
	(("objectfieldreference"	Terminal	 nil POINT))
	(("objectsetfieldreference"	Terminal	 nil POINT))
	(("["				Text		  nil)
	 ("TokenOrGroupSpec"		NonTerminal	(SPACE 1) POINT)
	 ("TokenOrGroupSpecListTail"	NonTerminal	(SPACE 1))
	 ("]"				Text		(SPACE 1))))

       ("PrimitiveFieldName"
	(("typefieldreference"		Terminal	 nil POINT))
	(("valuefieldreference"		Terminal	 nil POINT))
	(("valuesetfieldreference"	Terminal	 nil POINT))
	(("objectfieldreference"	Terminal	 nil POINT))
	(("objectsetfieldreference"	Terminal	 nil POINT)))

       ("FieldSpec"
	(("TypeFieldSpec"		NonTerminal	 nil POINT))
	(("FixedTypeValueFieldSpec"	NonTerminal	 nil POINT))
	(("VariableTypeValueFieldSpec"	NonTerminal	 nil POINT))
	(("FixedTypeValueSetFieldSpec"	NonTerminal	 nil POINT))
	(("VariableTypeValueSetFieldSpec" NonTerminal	 nil POINT))
	(("ObjectFieldSpec"		NonTerminal	 nil POINT))
	(("ObjectSetFieldSpec"		NonTerminal	 nil POINT)))

       ("TypeFieldSpec"
	(("typefieldreference"		Terminal	 nil POINT))
	(("typefieldreference"		Terminal	 nil POINT)
	 ("OPTIONAL"			Text		(SPACE 1)))
	(("typefieldreference"		Terminal	 nil POINT)
	 ("DEFAULT"			Text		(SPACE 1))
	 ("Type"			NonTerminal	(SPACE 1))))
	
       ("FixedTypeValueFieldSpec"
	(("valuefieldreference"		Terminal	 nil POINT)
	 ("Type"			NonTerminal	(SPACE 1)))
	(("valuefieldreference"		Terminal	 nil POINT)
	 ("Type"			NonTerminal	(SPACE 1))
	 ("UNIQUE"			Text		(SPACE 1)))
	(("valuefieldreference"		Terminal	 nil POINT)
	 ("Type"			NonTerminal	(SPACE 1))
	 ("UNIQUE OPTIONAL"		Text		(SPACE 1)))
	(("valuefieldreference"		Terminal	 nil POINT)
	 ("Type"			NonTerminal	(SPACE 1))
	 ("OPTIONAL"			Text		(SPACE 1)))
	(("valuefieldreference"		Terminal	 nil POINT)
	 ("Type"			NonTerminal	(SPACE 1))
	 ("DEFAULT"			Text		(SPACE 1))
	 ("Value"			NonTerminal	(SPACE 1))))
	
       ("VariableTypeValueFieldSpec"
	(("valuefieldreference"		Terminal	 nil POINT)
	 ("FieldName"			NonTerminal	(SPACE 1)))
	(("valuefieldreference"		Terminal	 nil POINT)
	 ("FieldName"			NonTerminal	(SPACE 1))
	 ("OPTIONAL"			Text		(SPACE 1)))
	(("valuefieldreference"		Terminal	 nil POINT)
	 ("FieldName"			NonTerminal	(SPACE 1))
	 ("DEFAULT"			Text		(SPACE 1))
	 ("Value"			NonTerminal	(SPACE 1))))


       ("FixedTypeValueSetFieldSpec"
	(("valuesetfieldreference"	Terminal	 nil POINT)
	 ("Type"			NonTerminal	(SPACE 1)))
	(("valuesetfieldreference"	Terminal	 nil POINT)
	 ("Type"			NonTerminal	(SPACE 1))
	 ("OPTIONAL"			Text		(SPACE 1)))
	(("valuesetfieldreference"	Terminal	 nil POINT)
	 ("Type"			NonTerminal	(SPACE 1))
	 ("DEFAULT"			Text		(SPACE 1))
	 ("ValueSet"			NonTerminal	(SPACE 1))))

       ("VariableTypeValueSetFieldSpec"
	(("valuesetfieldreference"	Terminal	 nil POINT)
	 ("FieldName"			NonTerminal	(SPACE 1)))
	(("valuesetfieldreference"	Terminal	 nil POINT)
	 ("FieldName"			NonTerminal	(SPACE 1))
	 ("OPTIONAL"			Text		(SPACE 1)))
	(("valuesetfieldreference"	Terminal	 nil POINT)
	 ("FieldName"			NonTerminal	(SPACE 1))
	 ("DEFAULT"			Text		(SPACE 1))
	 ("ValueSet"			NonTerminal	(SPACE 1))))

       
       ("ObjectFieldSpec"
	(("objectfieldreference"	Terminal	 nil POINT)
	 ("DefinedObjectClass"		NonTerminal	(SPACE 1)))
	(("objectfieldreference"	Terminal	 nil POINT)
	 ("DefinedObjectClass"		NonTerminal	(SPACE 1))
	 ("OPTIONAL"			Text		(SPACE 1)))
	(("objectfieldreference"	Terminal	 nil POINT)
	 ("DefinedObjectClass"		NonTerminal	(SPACE 1))
	 ("DEFAULT"			Text		(SPACE 1))
	 ("Object"			NonTerminal	(SPACE 1))))

       ("ObjectSetFieldSpec"
	(("objectsetfieldreference"	Terminal	 nil POINT)
	 ("DefinedObjectClass"		NonTerminal	(SPACE 1)))
	(("objectsetfieldreference"	Terminal	 nil POINT)
	 ("DefinedObjectClass"		NonTerminal	(SPACE 1))
	 ("OPTIONAL"			Text		(SPACE 1)))
	(("objectsetfieldreference"	Terminal	 nil POINT)
	 ("DefinedObjectClass"		NonTerminal	(SPACE 1))
	 ("DEFAULT"			Text		(SPACE 1))
	 ("ObjectSet"			NonTerminal	(SPACE 1))))

       ("FieldName"
	(("PrimitiveFieldName"		NonTerminal	 nil POINT)
	 ("FieldNameTail"		NonTerminal	(SPACE 1)))
	(("PrimitiveFieldName"		NonTerminal	 nil POINT)))	

       ("FieldNameTail"
	(("."				Text		 nil)
	 ("PrimitiveFieldName"		NonTerminal	(SPACE 1) POINT)
	 ("FieldNameTail"		NonTerminal	(SPACE 1)))
	(("."				Text		 nil)
	 ("PrimitiveFieldName"		NonTerminal	(SPACE 1) POINT))
	((EPSILON)))

       )
  " asn1-data-grammar represente la grammaire d'ASN.1.
 asn1-data-grammar est une liste d'elements, chaque element est une
 regle de la grammaire.
 Une regle a la forme suivante :
	(NTNAME PROD1 PROD2 ... PRODn)

 Chaque regle de la grammaire commence par le nom du non-terminal
 membre gauche de la regle : NTNAME. Ce nom est une chaine de caracteres.
 NTNAME est suivi de toute les production de ce non-terminal :
	PROD1 PROD2 ...PRODn
 Une production a la forme suivante :
	(ELT1 ELT2 ... ELTn)

 Une production est donc une liste d'elements. Ceux-ci peuvent
 representer un terminal, un non-terminal ou du texte.
 Chaque element a la forme suivante :
	(STRING  TYPE  COMMAND1  COMMAND2)

 TYPE peut prendre comme valeur :
	Terminal
	NonTerminal
	Text
 Selon l'element que l'on veut mettre dans la production.

 STRING est une chaine de caracteres, il est le nom du terminal ou
 non-terminal ou bien le texte de l'element.

 COMMAND1 peut avoir comme valeur :
	(LINE n) ou (SPACE n), avec n entier positif ou nul
 (LINE n) insere avant l'element un saut de ligne puis n espaces.
	Remarque : Comme le mode ASN.1 a l'indentation automatique, le
		   nombre n d'espaces a inserer ne se verra pas au
		   final car l'indentateur va \"corriger\" le nombre
		   d'espaces presents en debut de ligne.
 (SPACE n) insere n espaces avant l'element.

 COMMAND2 est une commande optionnelle. Si elle est presente, elle
 doit prendre la valeur : POINT.
 Si POINT est present dans un element, alors, lorsque la production
 est inseree par l'utilisateur dans son buffer, le curseur se place
 sur l'element de la production qui possede la commande POINT.
	Remarque : Si plusieurs elements d'une meme production
		   possedent la commande POINT, le curseur se place
		   sur le premier element qui a la commande POINT.
 Un element d'une production peut aussi avoir la forme suivante :
	(EPSILON)
 Dans ce cas, cette production represente la production du mot vide.
")
)

(defvar asn1-data-non-terminals
  (eval-when-compile
  (let ((liste-rslt nil)
	name
	type
	epsilon
	(iter 0))
    (mapcar
     (lambda (rule)
       (setq name (car rule))
       (setq type (asn1-rule-type rule))
       (setq epsilon nil)
       (mapcar
	(lambda (cell)
	  (setq epsilon 
		(or epsilon 
		    (eq (car (car cell)) 'EPSILON))))
	(cdr rule))
       (setq liste-rslt 
	     (append liste-rslt (list (list name type iter epsilon))))
       (setq iter (1+ iter)))
     asn1-data-grammar)
    liste-rslt))
  "Liste des non-terminaux.
Cette liste est construite a partir de la grammaire asn1-data-grammar.
La liste a des elements de la forme :
	(NON-TERMINAL TYPE NB MOT-VIDE)
NON-TERMINAL est une chaine de caracteres, c'est le nom du
non-terminal.
TYPE prend les valeurs BLOCK ou CHOICE, si respectivement le
non-terminal donne lieu a une seule production ou de multiples
productions.
NB est le numero de la regle dans asn1-data-grammar dont NON-TERMINAL
est le membre gauche (lors du developpement d'un non-terminal, on peut
retrouver rapidement la regle associee grace a ce numero).
MOT-VIDE prend les valeurs t ou nil, selon que le non-terminal se
reduit ou non au mot vide (s'il est optionnel ou s'il ne l'est pas).")

(defvar asn1-data-terminals 
  '(("modulereference"		UPPER)
    ("typereference"		UPPER)
    ("objectsetreference"	UPPER)
    ("objectclassreference"	CLASSREF)
    ("word"			WORD)
    ("identifier"		LOWER)
    ("valuereference"		LOWER)
    ("objectreference"		LOWER)
    ("typefieldreference"	UPPERFIELD)
    ("valuesetfieldreference"	UPPERFIELD)
    ("objectsetfieldreference"	UPPERFIELD)
    ("valuefieldreference"	LOWERFIELD)
    ("objectfieldreference"	LOWERFIELD)
    ("bitstring"		BITSTRING)
    ("hexastring"		HEXASTRING)
    ("charsstring"		CHARSSTRING)
    ("number"			NUMBER))
    "Liste des terminaux, suivi de leur type.
Le type peut etre :
UPPER, CLASSREF, WORD, LOWER, UPPERFIELD, LOWERFIELD, BITSTRING,
HEXASTRING, CHARSSTRING, NUMBER, NONE.
Voir la fonction asn1-syntax-edition-aux-T qui exploite ce typage des
terminaux.")

(defun asn1-locate-production ()
  "Recherche si le curseur est sur un non-terminal ou un terminal.
La valeur de retour est soit nil si le curseur n'est pas sur un
terminal ou un non-terminal, soit une liste :
	(NAME DEBUT FIN TYPE)

NAME est le nom du terminal ou non-terminal.
DEBUT est la position dans le buffer du terminal ou non-terminal.
FIN est la position de la fin  du terminal ou non-terminal dans le
buffer.
TYPE est soit Terminal, soit NonTerminal, selon l'element repere.

Cette fonction est utilisee par asn1-delete-production et
asn1-syntax-edition."
  (interactive)
  (let ((selection
	 (let ((here (point))
	       (bol (save-excursion 
		      (beginning-of-line)
		      (point)))
	       v1 v2)
	   (setq v1 (progn (forward-char 2)
			   (re-search-backward "\\([-][-]\\)" bol t)))
	   (setq v2 (looking-at asn1-data-production))
	   (if (or (not v1)
		   (not v2))
	       (progn
		 (goto-char here)
		 nil)
	     (list (asn1-substring (match-beginning 3) (match-end 3))
		   (match-beginning 0)
		   (match-end 0)
		   (cond ((assoc (asn1-substring
				  (match-beginning 3) (match-end 3))
				 asn1-data-terminals)
			  'Terminal)
			 ((assoc (asn1-substring
				  (match-beginning 3) (match-end 3))
				 asn1-data-non-terminals)
			  'NonTerminal)
			 (t
			  nil)))))))

    (cond ((not selection)
	   nil)
	  ((nth 3 selection)
	   selection)
	  (t 
	   nil))))

(defun asn1-delete-terminal (&optional selection)
  "Detruit le terminal sur lequel le curseur se trouve ou bien
detruit le terminal localise' par SELECTION (le format de SELECTION
est celui de la valeur de retour de asn1-locate-production). En plus
de `asn1-delete-production', cette fonction regarde si des espaces sont
presents apres ou avant le terminal et les enleve si cela est
necessaire. "
  (let ((2-chars-before-production "\\([.]\\|[(]\\|\\[\\|\\b[-]\\) ")
	(2-chars-after-production " \\([,]\\|[.]\\|[(]\\|[)]\\|\\]\\)")
	(3-chars-before-production " [,] "))
    (asn1-delete-production selection)
    (condition-case ()
	(progn
	  (backward-char 2)
	  (if (looking-at 2-chars-before-production)
	      (progn
		(forward-char 1)
		(delete-char 1))
	    (backward-char 1)
	    (if (looking-at 3-chars-before-production)
		(progn
		  (delete-char 1)
		  (forward-char 2))
	      (forward-char 3))))
      (error nil))
    (condition-case ()
	(progn
	  (if (looking-at 2-chars-after-production)
	      (progn
		(delete-char 1)))))))
     
(defun asn1-delete-production (&optional selection)
  "Detruit le non-terminal ou terminal sur lequel le curseur se
trouve ou bien detruit le terminal ou non-terminal localise par
SELECTION (le format de SELECTION est celui de la valeur de retour de
asn1-locate-production)."
  (interactive)
  (if (not selection)
      (setq selection (asn1-locate-production)))
  (if selection
      (progn
	(kill-region (nth 1 selection) (nth 2 selection))
	t)
    nil))

(defun asn1-syntax-production-of (string type &optional priority)
"STRING est une chaine de caracteres, TYPE est un type parmi 'Text
'Terminal 'NonTerminal, PRIORITY est t si STRING fait reference a un
non-terminal optionnel, nil dans tous les autres cas. La valeur de
retour est une chaine de caracteres, representation d'un
terminal, d'un non-terminal ou d'un simple texte dans un buffer en
mode ASN.1"
  (cond ((eq type 'Text)
	 string)
	((eq type 'Terminal)
	 (concat (nth 0 asn1-data-mark-TMandatory)
		 string 
		 (nth 1 asn1-data-mark-TMandatory)))
	((eq type 'NonTerminal)
	 (cond (priority
		(concat (nth 0 asn1-data-mark-NTOptional)
			string 
			(nth 1 asn1-data-mark-NTOptional)))
	       (t
		(concat (nth 0 asn1-data-mark-NTMandatory)
			string 
			(nth 1 asn1-data-mark-NTMandatory)))))
	(t
	 (error (format 
		 "asn1-syntax-production-of : wrong type -> %s"
		 type))
	 nil)))

(defun asn1-insert-syntax-production (string type &optional priority)
"Insere une chaine de caracteres produite par `asn1-syntax-production-of' 
(meme arguments) dans le buffer courant. Si ce qui est a inserer est
un terminal ou un non-terminal, alors le texte est rendu sensible au
passage de la souris sur lui."
  (let ((syntax-production (asn1-syntax-production-of string type priority)))
    (cond ((not syntax-production)
	   (error "asn1-syntax-production-of : wrong arguments"))
	  ((eq type 'Text)
	   (insert syntax-production))
	  (t
	   (insert "--")
	   (asn1-insert-mouse-text syntax-production)
	   (insert "--")))))

(defun asn1-find-priority (non-terminal)
"Renvoie la valeur du 3eme champ d'un element de la liste de
asn1-data-non-terminals"
  (nth 3 (assoc non-terminal asn1-data-non-terminals)))

(defun asn1-insert-production (production column)
"Insere la production PRODUCTION issue d'une certaine regle de
asn1-data-grammar, dans le buffer courant, selon les regles de mise en
forme qu'elle comporte."
  (interactive)
  (let* ((item		(car production))
	 (string	(nth 0 item))
	 (type		(nth 1 item))
	 (priority	(asn1-find-priority string))
	 (indent	(nth 2 item))
	 (fpoint	(nth 3 item))
	 (item-list	(cdr production))
	 new-column)
    (if (not type)
	nil
      (cond ((eq (car indent) 'LINE)
	     (insert "\n")
	     (setq new-column (+ (nth 1 indent) column))
	     (indent-to new-column))
	    ((eq (car indent) 'SPACE)
	     (setq new-column column)
	     (insert (make-string (nth 1 indent) ? )))
	    (t
	     (setq new-column column)))
      (if fpoint
	  (setq fpoint (point)))
      (asn1-insert-syntax-production string type priority)
      (if (or (eq (car indent) 'LINE)
	      (eq type 'Text))
	  (asn1-basic-indent-line)
	)
	
      (asn1-insert-production item-list new-column)
      (if fpoint
	  (goto-char fpoint)))))


;;;---------------------------------------------------------------------
;;;			Construction du menu : syntax menu
;;;---------------------------------------------------------------------

(defconst asn1-data-syntax-menu-blank 
  (make-string (length asn1-data-syntax-menu-orprefix) ? )
  "Chaine de caracteres constituee d'espaces, qui precede les items
dans les menus de syntaxe.")

(defconst asn1-data-roots-length
  3
  "Profondeur des derivations dans le menu de syntaxe \"ASN.1-Syntax\".")

(defun asn1-build-syntax-menu-aux (production &optional path orprefix)
  "Fonction auxiliaire de `asn1-build-syntax-menu'."
  (let* ((item		(car production))
	 (string	(nth 0 item))
	 (title		nil)
	 (type		(nth 1 item))
	 (menu		nil))

    (cond ((eq string 'EPSILON)
	   (setq string asn1-data-epsilon)
	   (if orprefix
	       (setq title (concat asn1-data-syntax-menu-orprefix string))
	     (setq title (concat asn1-data-syntax-menu-blank string)))
	   (setq menu (list (`[ (, title)
				(message (, string))
				t ]))))


	  ((eq type 'Text)
	   (if orprefix
	       (setq title (concat asn1-data-syntax-menu-orprefix string))
	     (setq title (concat asn1-data-syntax-menu-blank string)))
	   
	   (eval
	    (` (if (fboundp '(, (intern 
				 (concat "asn1-insert-syntax-text-" string))))
		   nil
		 (defun
		   (, (intern 
		       (concat "asn1-insert-syntax-text-" string)))
		   ()
		   (interactive)
		   (asn1-insert-syntax-production (, string) 
						  (quote (, type)))))))
	    
	   (setq menu (list (`[ (, title)
				(, (intern 
				    (concat 
				     "asn1-insert-syntax-text-" 
				     string)))
				t ]))))
	   

	  ((eq 'Terminal type)
	   (if orprefix
	       (setq title (concat asn1-data-syntax-menu-orprefix 
				   (asn1-syntax-production-of string type)))
	     (setq title (concat asn1-data-syntax-menu-blank 
				  (asn1-syntax-production-of string type))))
	   (eval
	    (` (if (fboundp 
		    '(, (intern 
			 (concat "asn1-insert-syntax-Terminal-" string))))
		   nil
		 (defun
		   (, (intern 
		       (concat "asn1-insert-syntax-Terminal-" string)))
		   ()
		   (interactive)
		   (asn1-insert-syntax-production (, string) 
						  (quote (, type)))))))
	    
	   (setq menu (list (`[ (, title)
				(, (intern 
				    (concat 
				     "asn1-insert-syntax-Terminal-" 
				     string)))
				t ]))))


	  ((eq 'NonTerminal type)
	   (setq menu (list (asn1-build-syntax-menu string path orprefix))))

	  (t
	   (error 
	    (format
	     "asn1-build-syntax-menu-aux : bad production -> %s"
	     production))))


    (if (cdr production)
	(setq 
	 menu 
	 (append menu 
		 (asn1-build-syntax-menu-aux (cdr production) path))))
    menu))

(defun asn1-build-syntax-menu (NTerminal &optional path orprefix)
  "Construit a partir d'un non-terminal NTerminal un menu de syntaxe,
qui est la valeur de retour de la fonction."
  (let (right-side
	title
	menu
	priority
	(associated-info (assoc NTerminal asn1-data-non-terminals)))  
    (setq priority (nth 3 associated-info))
    (if orprefix
	(setq 
	 title
	 (concat 
	  asn1-data-syntax-menu-orprefix
	  (asn1-syntax-production-of NTerminal 'NonTerminal priority)))
      (setq 
       title
       (concat 
	asn1-data-syntax-menu-blank
	(asn1-syntax-production-of NTerminal 'NonTerminal priority))))
		      
    (cond ((not associated-info)
	   (`( (, title)
	       ["bad link" (progn nil) nil])))


	  ((member NTerminal path)
	   (`( (, title)
	       ["non terminal already developped" (progn nil) nil])))


	  (t
	   (setq path (cons NTerminal path))
   (eval
	    (` (if (fboundp '(, (intern 
				(concat "asn1-insert-syntax-NTerminal-" 
					NTerminal))))
		   nil
		 (defun
		   (, (intern 
		       (concat "asn1-insert-syntax-NTerminal-" NTerminal)))
		   ()
		   (interactive)
		   (asn1-insert-syntax-production (, NTerminal) 
						  'NonTerminal 
						  (, priority)))))
	   )
	   

	   (if (= (length path) asn1-data-roots-length)
	       (setq menu
		     (`( (, title)
			 [ (, (concat "insert"))
			   (, (intern 
			       (concat 
				"asn1-insert-syntax-NTerminal-" 
				NTerminal)))
			   t])))
	     
	     (setq menu
		   (`( (, title)
		       [ (, (concat ""		;asn1-data-syntax-menu-blank
				    "insert"))
			 (, (intern 
			     (concat 
			      "asn1-insert-syntax-NTerminal-" 
			      NTerminal)))
			 t]
		       ["------" (progn nil) t])))
	     
	     (cond ((eq (nth 1 associated-info) 'CHOICE)
		    (setq 
		     right-side 
		     (cdr (nth (nth 2 associated-info) asn1-data-grammar)))
		    (while right-side
		    (setq 
		     menu 
		     (append 
		      menu 
		      (asn1-build-syntax-menu-aux (car right-side) path t)))
		    (setq right-side (cdr right-side)))
		    menu)
		   
		   ((eq (nth 1 associated-info) 'BLOCK)
		    (setq right-side 
			  (cdr (nth (nth 2 associated-info) 
				    asn1-data-grammar)))
		    (setq 
		     menu 
		     (append 
		      menu 
		      (asn1-build-syntax-menu-aux (car right-side) path)))
		    menu)
		   
		   (t
		    (error "asn1-build-syntax-menu : wrong format of grammar")
		    )))))))
 
(defvar asn1-data-syntax-menu
  (list
    ;; le format d'un item de menu :
    ;; [<nom de l'item>   <fonction a appeler>   <activite de l'item>]
    ;;   chaine de car		fonction		t ou nil
    
    ["--"			(lambda nil nil)	nil]
    (vector (asn1-lg "Supprimer un Element Syntaxique"
		     "Delete Syntax Element")
	    'asn1-insert-epsilon t)
    (vector (asn1-lg "Completion d'Element Syntaxique"
		     "Syntax Element Completion")
	    'asn1-syntax-completion t))
  "Menu de syntaxe ASN.1")
  
(defvar asn1-menu-emacs nil
  "Menu au format emacs")

(defun asn1-menu-really-build-syntax-menu ()
  (setq asn1-data-syntax-menu
	(append 
	 (mapcar
	  '(lambda (cell)
	     (asn1-build-syntax-menu cell))
	  asn1-data-syntax-menu-alist)
	 asn1-data-syntax-menu))
  ;; Si l'on est sous Emacs le menu contenu dans
  ;; asn1-data-syntax-menu (qui est au format XEmacs) est
  ;; converti dans le format Emacs grace a la fonction
  ;; asn1-convert-menu-format-2 
  (if asn1-data-xemacs-p nil
    (setq asn1-menu-emacs
	  (cdr
	   (asn1-convert-menu-format-2
	    (`((, asn1-data-syntax-menu-title)
	       (,@ asn1-data-syntax-menu))))))))

(defun asn1-menu-add-syntax-menu (map)
  "Ajoute le menu de syntaxe ASN.1  dans la barre des menus (menubar
en anglais)."
  ;; Rend les non-terminaux et les terminaux du buffer sensibles au
  ;; passage de la souris sur eux.
  (asn1-mouse-sensible-code)

  ;; Raccourcis souris et clavier
  (define-key map "\^c\^s"		'asn1-syntax-completion)
  (define-key map "\^c\^e"		'asn1-insert-epsilon)
  (if (not (lookup-key map "\M-["))
      (define-key map "\M-["		'asn1-syntax-completion))
  (if (not (lookup-key map "\M-n"))
      (define-key map "\M-n"		'asn1-insert-epsilon))
  (define-key map
    (asn1-mice-button-key asn1-mice-button)
    'asn1-syntax-edition)

  (cond (asn1-data-xemacs-p
	 (set-buffer-menubar (copy-sequence current-menubar)) 
	 (add-menu nil asn1-data-syntax-menu-title
		   asn1-data-syntax-menu))
	(t
	 (define-key map [menu-bar syntax-menu]
	   asn1-menu-emacs)))
  (run-hooks 'asn1-add-syntax-menu-hook))


;;;---------------------------------------------------------------------
;;;			menus surgissants (popup-menu)
;;;---------------------------------------------------------------------


(defvar asn1-data-convert-iterator 0
"Variable temporaire utilisee par `asn1-convert-menu-format-2'." )

(defun asn1-convert-menu-format-2 (menu)
  "Conversion d'un menu (de la barre des menus) du format XEmacs vers
le format Emacs. On ne peut utiliser le module `easy-menu' car il
n'autorise pas pour la conversion deux items dans un menu portant le
meme nom, ce dont nous avons besoin."
  (setq asn1-data-convert-iterator 
	(1+ asn1-data-convert-iterator))
  (cond ((listp menu)
	 (let ((title (car menu))
	       (rest (cdr menu))
	       (liste-reslt nil))
	   (if (not (stringp title))
	       (error "asn1-convert-menu-format-2 : title not a string")
	     (while rest
	       (setq liste-reslt
		     (append liste-reslt
			     (list (asn1-convert-menu-format-2 (car rest)))))
	       (setq rest (cdr rest)))
	     (` ((, (concat "menu" (int-to-string asn1-data-convert-iterator)))
		 (, title) keymap (, title)
		 (,@ liste-reslt))))))
	((vectorp menu)
	 (if (listp (elt menu 1))
	     (` ((, (concat "menu" (int-to-string asn1-data-convert-iterator)))
		 (, (elt menu 0))  
		 (nil) . (lambda nil (interactive)(, (elt menu 1)))))
	   (` ((, (concat "menu" (int-to-string asn1-data-convert-iterator)))
	       (, (elt menu 0))  
	       (nil) . (, (elt menu 1))))))
	(t
	 (error 
	  (format "asn1-convert-menu-format-2 :"
		  "bad menu format -> "
		  (car menu))))))

(defun asn1-menu-build-aux (rule iter)
  "Fonction auxiliaire de `asn1-menu-build'."
  (let ((name (car rule))
	(type (asn1-rule-type rule))
	(production-list (cdr rule))
	(liste-rslt nil)
	(iter2 0)
	(local-write
	 (lambda (production) 
	   (let* ((item		(car production))
		  (type-item	(nth 1 item))
		  (string	(car item))
		  (item-list	(cdr production))
		  syntax-string)

	     (cond ((eq string 'EPSILON)
		    (setq string asn1-data-epsilon))
		   (t
		    (setq 
		     syntax-string
		     (if (eq type-item 'NonTerminal)
			 (asn1-syntax-production-of 
			  string 
			  type-item
			  (nth 3 (assoc string asn1-data-non-terminals)))
		       (asn1-syntax-production-of 
			string 
			type-item)))
		    (if item-list
			(concat 
			 syntax-string
			 "  " 
			 (apply local-write (list item-list)))
		      syntax-string)))))))
	
    (if (not (eq type 'CHOICE) )
	nil
      (while production-list
	(if (eq 'EPSILON (car (car (car production-list))))
	    (setq liste-rslt
		  (append
		   liste-rslt
		   (`([ (, (apply local-write (list (car production-list))))
			asn1-insert-epsilon
			t]))))
	(eval
	 (` (defun
	      (, (intern (concat "asn1-insert-syntax-"
				 (int-to-string iter)
				 "-"
				 (int-to-string iter2))))
	      ()
	      (interactive)
	      (asn1-delete-production)
	      (asn1-insert-production
	       (nth (, iter2) 
		    (cdr (nth (, iter) asn1-data-grammar)))
	       (current-column)))))
	(setq liste-rslt
	      (append
	       liste-rslt
	       (`([ (, (apply local-write (list (car production-list))))
		    (, (intern 
			(concat "asn1-insert-syntax-"
				(int-to-string iter)
				"-"
				(int-to-string iter2))))
		    t])))))
	(setq production-list (cdr production-list))
	(setq iter2 (1+ iter2)))
      (append (list name) liste-rslt))))

(defun asn1-menu-build ()
  "Construit l'ensemble des menus surgissants (pop-up menus) qui
apparaissent lorsque l'on clique sur un non-terminal."
  (let ((rules asn1-data-grammar)
	menu
	(iter	0))
    (while rules
      (setq menu (asn1-menu-build-aux (car rules) iter))
      (if menu
	  (asn1-menu-create menu))
      (setq iter (1+ iter))
      (setq rules (cdr rules)))))

(defun asn1-insert-epsilon ()
"L'insertion du mot vide enleve un espace apres le curseur s'il y en a
un."
  (interactive)
  (if (asn1-delete-production)
      (if (looking-at " ")
	  (delete-char 1))))

(defun asn1-convert-menu-format (menu)
  "Convertit un menu surgissant (pop-up menu) au format XEmacs en un menu
au format GNU Emacs."
  (list "Menu" (asn1-convert-menu-format-aux menu)))

(defun asn1-convert-menu-format-aux (menu)
 "Fonction auxiliaire de asn1-convert-menu-format"
  (cond ((listp menu)
	 (let ((title (car menu))
	       (rest (cdr menu))
	       (liste-reslt nil))
	   (if (not (stringp title))
	       (error "asn1-convert-menu-format-aux : title not a string")
	     (while rest
	       (setq liste-reslt
		     (append liste-reslt
			     (list (asn1-convert-menu-format-aux (car rest)))))
	       (setq rest (cdr rest)))
	     (cons title liste-reslt))))
	((vectorp menu)
	 (cons (elt menu 0) (elt menu 1)))
	(t
	 (error 
	  (format "asn1-convert-menu-format-aux :"
		  "bad menu format -> "
		  (car menu))))))

;;;###autoload
(defun asn1-syntax-edition (event arg)
"Regarde ce qu'il y a sous le curseur. Si c'est un non-terminal ou un
terminal, on insere a la place de l'utilisateur un certain texte ou
une production dans le buffer courant."
  (interactive "e\nP")
  (mouse-set-point event)
  (let ((highlight-p (get-text-property (point) 'mouse-face))
	(selection (condition-case ()
		       (asn1-locate-production)
		     (error nil)))
	(end nil))
    (cond ((eq 'Terminal (nth 3 selection))
	   (asn1-syntax-edition-aux-T selection))
	  ((eq 'NonTerminal (nth 3 selection))
	   (asn1-syntax-edition-aux-NT selection))
	  (t
	   (if highlight-p
	       (message "Terminal or non terminal not recognized")
	     (if asn1-data-xemacs-p
		 (mouse-yank event)
	       (asn1-mice-normal-button event arg)))
	   (setq end t)))))
  
(defvar asn1-data-TYPEREFERENCE-regexp 
  "\\([A-za-z][A-Za-z0-9]*\\([-][A-Za-z0-9]+\\)*\\)$"
  "Expression reguliere d'un terminal du type typereference (cf norme
ASN.1).")

(defvar asn1-data-WORD-regexp
  "\\([A-Za-z]+\\([-][A-Za-z]+\\)*\\)$"
  "Expression reguliere d'un terminal du type word.")

(defvar asn1-data-NUMBER-regexp
  "\\(\\([-][0]*\\([[1-9][0]*\\)+\\)\\|[0-9]+\\)$"
  "Expression reguliere d'un terminal du type number.")
 
(defun asn1-syntax-edition-aux-T (selection)
  "SELECTION est une valeur retournee par `asn1-locate-production',
SELECTION doit faire reference a un terminal du buffer. Selon le type
de terminal il s'agit, on controle ce que l'utilisateur entre au
clavier. On corrige si necessaire ce qu'il a entre au clavier ou bien
on lui demande de rentrer quelque chose au clavier.

Fonction appelee par `asn1-syntax-edition'."
   (let* ((associated-info 
	   (assoc (car selection)
		  asn1-data-terminals))
	  production
	  (type (nth 1 associated-info)))
    
     (if (not associated-info)
	 (message "Terminal name incorrect")
       (cond ((eq type 'UPPER)
	      (setq 
	       production 
	       (read-string (concat " " (nth 0 selection) " : ")))
	      (while
		  (cond ((member production asn1-data-incorrect-TYPEREFERENCE)
			 (message (format (concat "The word \"%s\" is a "
						  "keyword, you can't use it.")
					  production))
			 (sleep-for 2)
			 (setq 
			  production 
			  (read-string (concat " " (nth 0 selection) " : ")
				       production))
			 t)
			((equal (string-match 
				 asn1-data-TYPEREFERENCE-regexp
				 production)
				0)
			 (aset production 0 (upcase (aref production 0)))
			 (asn1-delete-terminal selection)
			 (insert production)
			 nil)
			(t
			 (message (format "This is not a correct \"%s\""
					  (car selection)))
			 (sleep-for 2)
			 (setq 
			  production 
			  (read-string (concat " " (nth 0 selection) " : ")
				       production))
			 t))))
	      ((eq type 'LOWER)
	      (setq 
	       production 
	       (read-string (concat " " (nth 0 selection) " : ")))
	      (while
		  (cond ((member production asn1-data-incorrect-TYPEREFERENCE)
			 (message (format (concat "The word \"%s\" is a "
						  "keyword, you can't use it.")
					  production))
			 (sleep-for 2)
			 (setq 
			  production 
			  (read-string (concat " " (nth 0 selection) " : ")
				       production))
			 t)
			((equal (string-match 
				 asn1-data-TYPEREFERENCE-regexp
				 production)
				0)
			 (aset production 0 (downcase (aref production 0)))  
			 (asn1-delete-terminal selection)
			 (insert production)
			 nil)
			(t
			 (message (format "This is not a correct \"%s\""
					  (car selection)))
			 (sleep-for 2)
			 (setq 
			  production 
			  (read-string (concat " " (nth 0 selection) " : ")
				       production))
			 t))))
	     ((eq type 'UPPERFIELD)
	      (setq 
	       production 
	       (read-string (concat " " (nth 0 selection) " : ") "&"))
	      (while
		  (cond ((member (substring production 1)
				 asn1-data-incorrect-TYPEREFERENCE)
			 (message (format (concat "The word \"%s\" is a "
						  "keyword, you can't use it.")
					  (substring production 1)))
			 (sleep-for 2)
			 (setq 
			  production 
			  (read-string (concat " " (nth 0 selection) " : ")
				       production))
			 t)
			((equal (string-match 
				 (concat 
				  "&"
				  asn1-data-TYPEREFERENCE-regexp)
				 production)
				0)
			 (aset production 1 (upcase (aref production 1)))  
			 (asn1-delete-terminal selection)
			 (insert production)
			 nil)
			(t
			 (message (format "This is not a correct \"%s\""
					  (car selection)))
			 (sleep-for 2)
			 (setq 
			  production 
			  (read-string (concat " " (nth 0 selection) " : ")
				       production))
			 t))))
	     ((eq type 'LOWERFIELD)
	      (setq 
	       production 
	       (read-string (concat " " (nth 0 selection) " : ") "&"))
	      (while
		  (cond ((member (substring production 1)
				 asn1-data-incorrect-TYPEREFERENCE)
			 (message (format (concat "The word \"%s\" is a "
						  "keyword, you can't use it.")
					  (substring production 1)))
			 (sleep-for 2)
			 (setq 
			  production 
			  (read-string (concat " " (nth 0 selection) " : ")
				       production))
			 t)
			((equal (string-match
				 (concat
				  "&"
				  asn1-data-TYPEREFERENCE-regexp)
				 production)
				0)
			 (aset production 1 (downcase (aref production 1)))  
			 (asn1-delete-terminal selection)
			 (insert production)
			 nil)
			(t
			 (message (format "This is not a correct \"%s\""
					  (car selection)))
			 (sleep-for 2)
			 (setq 
			  production 
			  (read-string (concat " " (nth 0 selection) " : ")
				       production))
			 t))))
	     ((eq type 'CLASSREF)
	      (setq 
	       production 
	       (upcase (read-string (concat " " (nth 0 selection) " : "))))
	      (while
		  (cond ((member production asn1-data-incorrect-TYPEREFERENCE)
			 (message (format (concat "The word \"%s\" is a "
						  "keyword, you can't use it.")
					  production))
			 (sleep-for 2)
			 (setq 
			  production 
			  (upcase (read-string 
				   (concat " " (nth 0 selection) " : ")
				   production)))
			 t)
			((equal (string-match 
				 asn1-data-TYPEREFERENCE-regexp
				 production)
				0)
			 (aset production 0 (upcase (aref production 0)))
			 (asn1-delete-terminal selection)
			 (insert production)
			 nil)
			(t
			 (message (format "This is not a correct \"%s\""
					  (car selection)))
			 (sleep-for 2)
			 (setq 
			  production 
			  (upcase (read-string 
				   (concat " " (nth 0 selection) " : ")
				   production)))
			 t))))
	     ((eq type 'BITSTRING)
	      (setq 
	       production 
	       (read-string (concat " " (nth 0 selection) " : ") 
			    '("''B" . 2)))
	       (asn1-delete-terminal selection)
	       (insert production))
	     ((eq type 'HEXASTRING)
	      (setq 
	       production 
	       (read-string (concat " " (nth 0 selection) " : ") 
			    '("''H" . 2)))
	      (asn1-delete-terminal selection)
	      (insert production))
	     ((eq type 'CHARSSTRING)
	      (setq 
	       production 
	       (read-string (concat " " (nth 0 selection) " : ") 
			    '("\"\"" . 2)))
	      (asn1-delete-terminal selection)
	      (insert production))
	     ((eq type 'WORD)
	      (setq 
	       production 
	       (upcase (read-string (concat " " (nth 0 selection) " : "))))
	      (cond ((member production asn1-data-incorrect-WORD)
		     (message (format (concat "You can not use \"%s\""
					      " for the terminal %s")
				      production
				      selection))
		     (sleep-for 2)
		     (asn1-syntax-edition-aux-T selection))
		    ((equal (string-match 
			     (concat asn1-data-WORD-regexp ) 
			     production) 
			    0)
		     (asn1-delete-terminal selection)
		     (insert production))
		    (t
		     (message "The word you typed is not correct")
		     (sleep-for 2)
		     (asn1-syntax-edition-aux-T selection))))
	     ((eq type 'NUMBER)
	      (setq 
	       production 
	       (upcase (read-string (concat " " (nth 0 selection) " : "))))
	      (cond ((equal (string-match 
			     (concat asn1-data-NUMBER-regexp ) 
			     production) 
			    0)
		     (asn1-delete-terminal selection)
		     (insert production))
		    (t
		     (message "The number you typed is not a correct number")
		     (sleep-for 2)
		     (asn1-syntax-edition-aux-T selection))))
				      
	     ((eq type 'NONE)
	      (setq 
	       production 
	       (read-string (concat " " (nth 0 selection) " : ")))

	      (asn1-delete-terminal selection)
	      (insert production))
	     (t
	      (error "asn1-syntax-edition-aux-T : wrong format of grammar")))
      )))

(defun asn1-syntax-edition-aux-NT (selection)
    "SELECTION est une valeur retournee par `asn1-locate-production',
SELECTION doit faire reference a un non-terminal du buffer. Selon le type
de non-terminal il s'agit (si la regle de` asn1-data-grammar' associee
n'a qu'une ou plusieurs productions), soit la production est inseree
dans le buffer, soit un menu surgissant apparait et l'utilisateur
choisit une des alternatives proposees."
  (let ((associated-info 
	 (assoc (car selection)
		asn1-data-non-terminals)))
    (if (not associated-info)
	(message "Non terminal name incorrect")
      (cond ((eq (nth 1 associated-info) 'CHOICE)
	     (let ((menu (if (boundp  
			      (intern (concat "asn1-data-menu-" 
					      (car selection))))
			     (eval (intern (concat "asn1-data-menu-" 
						   (car selection))))
			   (message "Syntax menu not loaded.")
			   nil)))
	       (if menu
		   (asn1-menu-popup 
		    menu
		    event))))
	    ((eq (nth 1 associated-info) 'BLOCK)
	     (asn1-delete-production selection)
	     (asn1-insert-production
	      (car (cdr (nth (nth 2 associated-info) asn1-data-grammar)))
	      (current-column)))
	    (t
	     (error "asn1-syntax-edition-aux-NT : wrong format of grammar"))
	    ))))

(defun asn1-insert-mouse-text (string)
  "Insere la chaine de caracteres STRING dans le buffer courant, rendue
sensible au passage de la souris. Quand le pointeur de
la souris passera au dessus de la 
chaine de caracteres, son style
d'ecriture changera, celui-ci devient le style d'ecriture
\"highlight\". Quand le pointeur de la souris quitte la 
chaine, elle reprend son ancien style d'ecriture."
   (let ((begin (point)))
    (insert string)
    (asn1-mouse-face begin (point))))

;;;###autoload
(defun asn1-load-syntax-menu ()
  "Chargement du menu de syntaxe \"ASN1-Syntax\""
  (interactive)
  (let ((b (current-buffer))) ; for asn1-presentation
    (asn1-presentation)
    (message "Building syntax-menu.")
    (asn1-menu-build)
    (message "Building syntax-menu..")
    (asn1-menu-really-build-syntax-menu)
    (message "Building syntax-menu...")
    ;(if (not (eq (current-local-map) asn1-data-mode-map))
    (if (current-local-map)
	(asn1-menu-add-syntax-menu (current-local-map)))
    (asn1-menu-add-syntax-menu asn1-data-mode-map)
    (if (featurep 'asn1-diff)
	(asn1-menu-add-syntax-menu asn1-diff-mode-asn1-map))
    ;; Pour xemacs : on ajoute partout
    (save-excursion
      (let ((l (buffer-list)))
	(while l
	  (set-buffer (car l))
	  (if (eq major-mode 'asn1-mode)
	      (asn1-menu-add-syntax-menu (current-local-map)))
	  (setq l (cdr l)))))
    (setq asn1-data-syntax-menu-loaded t)
    (switch-to-buffer b)
    (redraw-display))
  (message "Building syntax-menu...done"))

(defvar asn1-data-syntax-completion-list 
  nil
  "Liste servant a la completion des \"elements syntaxiques\" : 
les terminaux et les non-terminaux.
Celle-ci est construite par la fonction asn1-define-syntax-completion,
appelee lors de la construction du menu de syntaxe : \"ASN.1-Syntax\"")

(defun asn1-define-syntax-completion ()
  "Construction de la liste  des \"elements syntaxiques\":
ensemble des terminaux et des non-terminaux
(asn1-data-syntax-completion-list), qui sert a la completion."
  (setq asn1-data-syntax-completion-list
	(append
	 (mapcar 
	  'car
	  asn1-data-terminals)
	 (mapcar 
	  'car
	  asn1-data-non-terminals))))

(defun asn1-syntax-completion ()
   "Effectue une completion sur la liste de mots contenus dans
asn1-data-syntax-completion-list." 
   (interactive)
   (if (not asn1-data-syntax-completion-list)
       (asn1-define-syntax-completion))

   (let ((proposition
	  (completing-read 
	   (format "research of a syntax element (use Tab to complete) : " )
	   (mapcar 
	    (lambda (string) (list string)) asn1-data-syntax-completion-list)
	   nil t)))
     (if proposition
	   (cond ((assoc proposition asn1-data-terminals)
		  (asn1-insert-syntax-production 
		   proposition
		   'Terminal))
		 ((assoc proposition asn1-data-non-terminals)
		  (asn1-insert-syntax-production 
		   proposition
		   'NonTerminal
		   (asn1-find-priority proposition)))
		 (t
		  (error "asn1-syntax-completion : error"))))))
