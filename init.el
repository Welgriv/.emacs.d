;; package --- Summary
;;; Commentary:

;; Rappel de raccourcis basique :
;; M-l met le prochain mot en minuscule
;; M-g n ouvrir le fichier et aller à la prochaine erreur
;; C-c C-c quiter
;; C-g quitter la commande en cours
;; M-; commenter le block de code
;; C-h v voir la valeur d'une variable
;; en gdb commande de base :
;; C-x C-a C-b : mettre un bp sur la ligne du fichier où on est
;;           s : step
;;           r : run
;;           n : next

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
 '(LaTeX-command-style
   (quote
	(("" "%(PDF)%(latex) -shell-escape %(file-line-error) %(extraopts) %S%(PDFout)"))))
 '(custom-enabled-themes (quote (tango-dark)))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages (quote (auctex auctex-latexmk)))
 '(safe-local-variable-values (quote ((ispell-dictionary . "fr")))))
 ;; end of custom-vet-variable

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; le sous répertoire contenant les .el (doit être présent...)
(add-to-list 'load-path "~/.emacs.d/elFiles")

;; parentèse crochets etc... auto
(electric-pair-mode t)

;; split screen vertical au lancement si 2 ficihiers
(defun 2-windows-vertical-to-horizontal ()
  "Trnasform vertical split to horizontal split."
  (let ((buffers (mapcar 'window-buffer (window-list))))
    (when (= 2 (length buffers))
      (delete-other-windows)
      (set-window-buffer (split-window-horizontally) (cadr buffers)))))

(add-hook 'emacs-startup-hook '2-windows-vertical-to-horizontal)

;; ---- LATEX
;; ;; enlève le full screen et garde un seul buffer ouvert.
;; (add-hook 'LaTeX-mode-hook 'delete-other-windows)
;; mode mathe en mode lateX
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
;; active flyspell
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; pour avoir pdflatex comme comande de compile de base pour lateX
;; (setq TeX-PDF-mode t) ; ne semble pas necessaire...
;; Change la touche de gestion des erreurs de compile
(defun rebind-tex-next-error ()
  "Rebind the function TeX-next-error."
  (global-unset-key "\C-c `")
  (global-set-key (kbd "C-c =") 'TeX-next-error))

(add-hook 'LaTeX-mode-hook 'rebind-tex-next-error)

;; change la touche pour l'autocompletion en latex : ctrl tab
(add-hook 'LaTeX-mode-hook
      (lambda()
        (local-set-key [C-tab] 'TeX-complete-symbol)))

;; appel la fonction clean de lateX à la fermetur d'un buffer
(add-hook 'LaTeX-mode-hook
          (lambda ()
	    (add-hook 'kill-buffer-hook 'TeX-clean nil 'make-it-local)))
;; pareil si on kill emacs
(add-hook 'LaTeX-mode-hook
          (lambda ()
	    (add-hook 'kill-emacs-hook 'TeX-clean nil 'make-it-local)))
;; désactive flycheck
(setq flycheck-global-modes '(not LaTeX-mode latex-mode))

(setq-default TeX-master nil)
(setq TeX-parse-self t)
;; auto-paire pour les $
(setq TeX-electric-math (cons "$" "$"))
;; ---- LATEX specific termine ici

;; spell check décomanter tout et ajouter le dico pour que ça marche
(setq-default ispell-program-name "/usr/bin/aspell")
;; (custom-set-variables '(ispell-dictionary "fr")) ;charge le dico fr
;; (add-hook 'text-mode-hook 'flyspell-mode)

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
(global-set-key "\C-x\C-k" 'copy-line)
;;;;;;;;;;;;;;;;;;;;;

;; permet d'avoir un curseur sur chaque ligne selectionné
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; j'ai oublié ce que ça faisait, mais c'est utile...
(global-auto-revert-mode t)

;; limit les commentaire à 80 caratères C
;; (add-hook 'c-mode-common-hook
;; 	  (lambda ()
;; 	    (auto-fill-mode 1)
;; 	    (set (make-local-variable 'fill-nobreak-predicate)
;; 		 (lambda ()
;; 		   (not (eq (get-text-property (point) 'face)
;; 			    'font-lock-comment-face))))))
;; active flyspell dans les commentaire C
;; (add-hook 'c-mode-common-hook (lambda () (flyspell-prog-mode)))

;; RAPPEL : C-u M-; kill le commentaire en fin de ligne

;; c++ mode pour les .tpp
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

;; commenter la ligne active (C-x C-; ne fonctionne pas pour toutes les versions)
(defun toggle-comment-on-line ()
  "COMMENT OR UNCOMMENT CURRENT LINE."
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position))
  (forward-line))

(global-set-key (kbd "C-x C-;") 'toggle-comment-on-line)

;; active l'auto completion (petite GUI qui affiche les completions posisbles)
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)
;; (add-to-list 'ac-modes 'latex-mode)	; besoin d'une activation explicite pour lateX

;; LUSTRE

;; Set the load-path variable :
(setq load-path
     (append load-path
	      '("~/.emacs.d/lustre.el")))

(setq auto-mode-alist (cons '("\\.lus$" . lustre-mode) auto-mode-alist))
(autoload 'lustre-mode "lustre" "Edition de code lustre" t)

;; COMPILATION
;; Rappel : M-g n pour aller à l'erreur suivante après compile

;; Saute automatiquement dans le buffer de compile
(add-hook 'compilation-finish-functions 'switch-to-buffer-other-window 'compilation)
;; Place le curseur à la première erreur
(setq compilation-auto-jump-to-first-error 't)
;; Pour faire scroller le buffer de compile en bas
(setq compilation-scroll-output 'first-error)
;; compile avec f5, recompile avec f6, debug avec f7
(global-set-key [f5] 'compile)
(global-set-key [f6] 'recompile)
(global-set-key [f7] 'gdb)

;; Fait apparaitre les tabluations comme 4 espaces (mais c'est bien un tab qui sera inséré dans le fichier !)
(setq-default tab-width 4)

;; Enlève les espaces de fin de ligne à l'enregistrement d'un fichier.
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; permet d'avoir toutes les commandes du shell en mode windowed sous unix
(setq shell-command-switch "-ic")

;;;; ---- .dir-locals file related: (from https://emacs.stackexchange.com/a/13096/18823) ---- ;;;;
;; This function will re-read the dir-locals file and set the new values for the current buffer:
(defun my-reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

;; And if you want to reload dir-locals for every buffer in your current buffer's directory:
(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

;; You could have all the dir locals refresh every time you save a dir-locals file by adding an after-save-hook to .dir-locals.el buffers.
(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook (make-variable-buffer-local 'after-save-hook)
                        'my-reload-dir-locals-for-all-buffer-in-this-directory))))

;; bindings window moove to super key
(windmove-default-keybindings 'super)

(provide 'init)
;;; init ends here
