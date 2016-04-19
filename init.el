;; Setting up package manager for Emacs
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository

;; For loading packages from the Emacs Lisp Package Archive (ELPA)
(defun package (package)
    (when (not (package-installed-p package))
      (package-install package))
    (personal package))

;; For loading personal configurations
(defun personal (library)
  (load (concat "~/.emacs.d/personal/" (symbol-name library)) 'noerror))


(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org babel load languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (sh . t)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Toggle line mode
(global-set-key [(meta t)] 'toggle-truncate-lines)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(package 'ag)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; gradle mode
(package 'gradle-mode)

;; mode notes
;; C-c C-g r : runs "$ gradle "user-supplied-task"
;; C-c C-g d : runs "$ gradle "user-supplied-task" --daemon
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Add magit
;; (package 'magit) ;; currently doesn't work. don't know why. TODO
;; So currently its installed with package-install magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable subword mode
(global-subword-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smartscan
(package-install 'smartscan)
(smartscan-mode 1) ;; enabling smartscan

;; mode notes: Smart Scan will try to infer the symbol your point is on and let you jump to other, identical, symbols elsewhere in your current buffer with a single key stroke. The advantage over isearch is its unintrusiveness; there are no menus, prompts or other UI elements that require your attention.
;; usage: M-n and M-p move between symbols and type M-' to replace all symbols in the buffer matching the one under point, and C-u M-' to replace symbols in your current defun only (as used by narrow-to-defun.)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme and display settings..

(package 'solarized-theme) ;; select solarized theme
(setq initial-frame-alist (quote ((fullscreen . maximized)))) ;; full screen on launch


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(package 'smex)

(require 'smex) ; Not needed if you use package.el
(smex-initialize) ; Can be omitted. This might cause a (minimal) delay
					; when Smex is auto-initialized on its first run.

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; mode notes: Smex is a M-x enhancement for Emacs. Built on top of Ido, it provides a convenient interface to your recently and most frequently used commands. And to all the other commands, too.
;; The commands are displayed in an Ido completion buffer, ordered by relevance. The 7 most recently executed commands come first, the rest are sorted by frequency of use, command length and in alphabetical order.
;;Ido completion in 10 seconds: Typing selects matching commands: e.g. 'lnmd' matches 'line-number-mode'. C-s/C-r switches to the next/previous match. Enter executes the selected command.
;; smex-major-mode-commands runs Smex, limited to commands that are relevant to the active major mode. Try it with Dired or Magit. TODO. I should try to do it when I am dealing with these major modes.
;; Ch-f in smex defaults to the function on the current line
;; Ch-w shows keybinding for selected command via where-is
;; I think Smex with M-. can go to definition of a function, yet to see how it works. TODO.
;; smex-show-unbound-commands shows commands that are frequently used but have no key binding for them.
;; file ~/.emacs.d/smex-items is supposed to have its state saved between different emacs session.
;; this is available in variable smex-save-file
;; smex link: https://www.emacswiki.org/emacs/Smex


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Gotta see matching parens
(show-paren-mode t)

;; For emacsclient
(server-start)

;; For duplicating a line
(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
  )

(global-set-key [(control d)] 'duplicate-line)


;; Enable global auto indentation
(define-key global-map (kbd "RET") 'newline-and-indent)
;; Mode notes
;; Should be turned off in a local mode when not required

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lets require Ido
(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; I am unsure what this does as it worked before it as well
(setq ido-use-filename-at-point 'guess)

;; Always create new buffer without any confirmation
(setq ido-create-new-buffer 'always) ;; and this is not helping TODO.

;; Mode notes
;; Some people find Ido’s find-files support a bit intrusive. Just remember that if you type C-f Ido will revert to the old-style completion engine!
;; Can enter Cj instead of enter if we don't want to press enter twice when creating new buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Global line numbers in all modes
(global-linum-mode 1)

;; Make the current line more visible...
(global-hl-line-mode 1)

;; Hoping to see a visible bell instead of sound one
(setq visible-bell nil)
(setq ring-bell-function (lambda ()
			   (invert-face 'mode-line)
			   (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;;Setting default directory for emacs
(setq default-directory "/Users/dhruvk/Projects")

;; Setting up initial buffer choice
(setq initial-buffer-choice "/Users/dhruvk/Projects")

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;;; I prefer cmd key for meta
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight incremental search
(setq search-highlight t)
(transient-mark-mode t)

;; all lines
(global-visual-line-mode 1)

;; Navigate between windows using alt-1 alt-2 shift-left shift-right shift-up
(windmove-default-keybindings)

;; Goto emacs init file directly
(defun goto-emacs-file ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-x x") 'goto-emacs-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Org mode
;; Enable org-mode
(require 'org)

;; For setting up the todo worklow
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

;; global key binding for seeing agenda's. I think this should have already been bound, not sure why it is not bound
(global-set-key "\C-ca" 'org-agenda)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copy pasting stuff from terminal emulator to mac clipboard
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For the backup stategy for emacs

(setq version-control t     ;; Use version numbers for backups.
      kept-new-versions 10  ;; Number of newest versions to keep.
      kept-old-versions 0   ;; Number of oldest versions to keep.
      delete-old-versions t ;; Don't ask to delete excess backup versions.
      backup-by-copying t)  ;; Copy all files, don't rename them.


;; now also backup versioned files, we don't check in on every commit right? (Not sure if I understand what this shit means)

(setq vc-make-backup-files t)

;; Default and per-save backups go here:
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/per-save")))

(defun force-backup-of-buffer ()
  ;; Make a special "per session" backup at the first save of each
  ;; emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups.
    (let ((backup-directory-alist '(("" . "~/.emacs.d/backup/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save.  The first save results in
  ;; both a per-session and a per-save backup, to keep the numbering
  ;; of per-save backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

(add-hook 'before-save-hook  'force-backup-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuring eshell to open from a buffer

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (height (/ (window-total-height) 3))
	 (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-c C-e") 'eshell-here)

;; My function, x exits that shell and closes that window.

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; From the custom thingy.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For ESS

(add-to-list 'load-path "/usr/local/self/ESS-15.09/lisp/")
(load "ess-site")

;;;;;;;;
;; What is the better place for me to write shortcuts than the first file itself

;; The C keys
;; Ch m : To display all the minor modes
;; Ch v : describe variable
;; Ch f : describe function
;; Ch k : describe function invoked by the keystrokes
;; Ch i : Emacs manual
;; Ch a : Search for a command
;; Ch Ch a : Shows help
;; C-8 Cn : Go 8 lines forward
;; Cb,Cf : Move by characters
;; Cv : scroll down
;; Cu CSpace : Return back to the pointer or mark
;; Cs,Cr : Search
;; CxCx : Toggle between mark and point

;; The M keys
;; Mx eval-region
;; Mx eval-buffer
;; My : Paste older things... TODO. I don't understand it yet
;; MgMg : Goto a line
;; Mx occur
;; Mc capitalize Word
;; M- Mc : Will capitalize the word just typed
;; Mb, Mf : Move by words
;; Me, Ma : Move by paragraphs
;; Mv : Scroll up
;; M< : Move to the beginning of the buffer
;; M> : Move to the end of the buffer
;; Mm : move back to indentation.
;; Mx ffap : Find fine at point

;; The C M keys
;; CMx : evaluate a single expression
;; CMf , CMb : move by s expression forward or before (like quotes or balanced parenthesis)
;; CMs : regex search

;; There is Mx shell and Mx term and Mx ansi-term...
;; The Mx ansi-term has two modes activated by CcCj which is line mode and CcCk which is char mode.
;; In line mode the terminal behaves much like emacs buffer and can be controlled via normal ways while in char mode it behaves like a terminal emulator and can almost run things like top.


;;;;;;;;

;; TODO : Its hard to read what custom set things have done and it becomes a global mess very soon - I think I'll change it to move it into separate sectiona nd configure it myself.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
