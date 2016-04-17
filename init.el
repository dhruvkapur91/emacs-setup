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

;; Goto emacs file directly
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
