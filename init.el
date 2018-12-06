;;; package --- Summary
;;; Commentary:

;; Correspondance des parenthèses :
;; Avec ceci, positionnez le curseur sur une parenthèse ouvrante ou
;; une parenthèse fermante, Emacs met en couleur la paire de
;; parenthèses.
;;; Code:
(show-paren-mode 1)
 
;; Utiliser UTF-8 comme codage de caractères par défaut.
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
 
;; Afficher les numéros de lignes dans la mode-line (barre du bas de
;; fenêtre) :
(line-number-mode t)
(column-number-mode t)
 
;; Faire clignoter l'écran au lieu de faire « beep ».
(setq visible-bell t)
 
;; Ne pas afficher le message d'accueil
(setq inhibit-startup-message t)
 

;; Pour une interface graphique un peu dépouillée
(menu-bar-mode -1)
;(scroll-bar-mode -1)
(tool-bar-mode -1)
;(blink-cursor-mode -1)
 
;; Définir des touches pour se déplacer rapidement :
;; Aller à la parenthèse ouvrante correspondante :
(global-set-key [M-right] 'forward-sexp)
;; Aller à la parenthèse Fermante correspondante :
(global-set-key [M-left] 'backward-sexp)

;;gagner du temps sur les confirmations
;(fset 'yes-or-no-p 'y)

;;afficher numéro de slignes :
(global-linum-mode 1)

;;plus de fichier ~ de sauvgarde :
(setq make-backup-files nil)

;;pour les packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(add-hook 'after-init-hook #'global-flycheck-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 
 ;; Met le theme tango dark
 '(custom-enabled-themes (quote (tango-dark)))
 ;; selectionne les packages mentionés dans la dernière parenthèses
 '(package-selected-packages (quote (multiple-cursors flycheck)))
  ;; full screen au démarrage
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 ) ; end of custom-vet-variable

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.

;; le sous répertoire contenant les .el (doit être présent...)
(add-to-list 'load-path "~/.emacs.d/elFiles")

;; parentèse crochets etc... auto
(electric-pair-mode t)

;; split screen vertical au lancement
(split-window-right)

;; ;; POUR LATEX
;; les packages uniquement en mode lateX
 (add-hook 'LaTeX-mode-hook '(package-selected-packages (quote (auctex auctex-latexmk))))
;; En mode lateX, enlève le full screen et garde un seul buffer ouvert.
(add-hook 'LaTeX-mode-hook 'delete-other-windows)
;; mode mathe en mode lateX
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; active flyspell
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

;; smart tab (ident first then complet)
;; (require 'smart-tab)
;; (global-smart-tab-mode 1)

;; copier la ligne courrante ;;;;;;;;;;
(defun copy-line (arg)
  "Copy lines (as many as prefix ARG) in the kill ring.
Ease of use features:
- Move to start of next line.
- Appends the copy on sequential calls.
- Use newline as last char even on the last line of the buffer.
- If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
	(end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
	  (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
	(setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
	(kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

;; optional key binding
(global-set-key "\C-c\C-k" 'copy-line)
;;;;;;;;;;;;;;;;;;;;;

;; permet d'avoir un curseur sur chaque ligne selectionné
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; spell check décomanter tout et ajouter le dico pour que ça marche
;; (setq-default ispell-program-name "chemin vers aspell...")
;; (custom-set-variables '(ispell-dictionary "fr")) ;charge le dico fr
;; (add-hook 'text-mode-hook 'flyspell-mode)

;; j'ai oublié ce que ça faisait, mais c'est utile...
(global-auto-revert-mode t)

;; limit les commentaire à 80 caratères C
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (auto-fill-mode 1)
	    (set (make-local-variable 'fill-nobreak-predicate)
		 (lambda ()
		   (not (eq (get-text-property (point) 'face)
			    'font-lock-comment-face))))))
;; active flyspell dans les commentaire C
  (add-hook 'c-mode-common-hook
          (lambda ()
            (flyspell-prog-mode)
            ; ...
          ))

;; commenter la ligne active (C-x C-; ne fonctionne pas pour toutes les versions)
(defun toggle-comment-on-line ()
  "COMMENT OR UNCOMMENT CURRENT LINE."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  (forward-line))

(global-set-key (kbd "C-x C-;") 'toggle-comment-on-line)

;; pour avoir pdflatex comme comande de compile de base pour lateX
(setq TeX-PDF-mode t)

;; active l'auto completion (petite GUI qui affiche les completions posisbles)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'latex-mode)	; besoin d'une activation explicite pour lateX

(provide 'init)
;;; init ends here

