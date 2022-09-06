;; Set lazy loading
(setq package-enable-at-startup nil ; don't auto-initialize!
      ;; don't add that `custom-set-variables' block to my init.el!
      package--init-file-ensured t)

;; Remap leader key
(setq doom-leader-key ","
      doom-localleader-key "\\")

(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(evil-define-key 'normal ibuffer-mode-map
        (kbd "f c") 'ibuffer-filter-by-content
        (kbd "f d") 'ibuffer-filter-by-directory
        (kbd "f f") 'ibuffer-filter-by-filename
        (kbd "f m") 'ibuffer-filter-by-mode
        (kbd "f n") 'ibuffer-filter-by-name
        (kbd "f x") 'ibuffer-filter-disable
        (kbd "g h") 'ibuffer-do-kill-lines
        (kbd "g H") 'ibuffer-update)

(use-package dashboard
        :init      ;; tweak dashboard config before loading it
        (setq dashboard-set-heading-icons t)
        (setq dashboard-set-file-icons t)
        ;;(setq dashboard-startup-banner 'logo) ;; use standard emacs logo as banner
        (setq dashboard-startup-banner "~/.config/doom/doom-emacs-dash.png")  ;; use custom image as banner
        (setq dashboard-center-content t) ;; set to 't' for centered content
        (setq dashboard-items '((recents . 5)
                                (agenda . 5 )
                                (bookmarks . 5)
                                (projects . 5)
                                (registers . 5)))
        :config
        (dashboard-setup-startup-hook)
        (dashboard-modify-heading-icons '((recents . "file-text")
                                          (bookmarks . "book"))))

(setq doom-fallback-buffer-name "*dashboard*")
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(map! :leader
      (:prefix ("d" . "dired")
       :desc "Open dired" "d" #'dired
       :desc "Dired jump to current" "j" #'dired-jump)
      (:after dired
       (:map dired-mode-map
        :desc "Peep-dired image previews" "d p" #'peep-dired
        :desc "Dired view file" "d v" #'dired-view-file)))

(evil-define-key 'normal dired-mode-map
  (kbd "M-RET") 'dired-display-file
  (kbd "h") 'dired-up-directory
  (kbd "l") 'dired-open-file ; use dired-find-file instead of dired-open.
  (kbd "m") 'dired-mark
  (kbd "t") 'dired-toggle-marks
  (kbd "u") 'dired-unmark
  (kbd "C") 'dired-do-copy
  (kbd "D") 'dired-do-delete
  (kbd "J") 'dired-goto-file
  (kbd "M") 'dired-do-chmod
  (kbd "O") 'dired-do-chown
  (kbd "P") 'dired-do-print
  (kbd "R") 'dired-do-rename
  (kbd "T") 'dired-do-touch
  (kbd "Y") 'dired-copy-filenamecopy-filename-as-kill ; copies filename to kill ring.
  (kbd "+") 'dired-create-directory
  (kbd "-") 'dired-up-directory
  (kbd "% l") 'dired-downcase
  (kbd "% u") 'dired-upcase
  (kbd "; d") 'epa-dired-do-decrypt
  (kbd "; e") 'epa-dired-do-encrypt)
;; Get file icons in dired
;; (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "nsxiv")
                              ("jpg" . "nsxiv")
                              ("png" . "nsxiv")
                              ("pdf" . "zathura")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(setq doom-theme 'doom-gruvbox)
(map! :leader
      :desc "Load new theme" "h t" #'counsel-load-theme)

(use-package emojify
  :hook (after-init . global-emojify-mode))

(setq doom-font (font-spec :size 18)
      ;; doom-variable-pitch-font (font-spec :size 18)
      doom-big-font (font-spec :size 24))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic))

(setq ivy-posframe-display-functions-alist
      '((swiper                     . ivy-posframe-display-at-point)
        (complete-symbol            . ivy-posframe-display-at-point)
        (counsel-M-x                . ivy-display-function-fallback)
        (counsel-esh-history        . ivy-posframe-display-at-window-center)
        (counsel-describe-function  . ivy-display-function-fallback)
        (counsel-describe-variable  . ivy-display-function-fallback)
        (counsel-find-file          . ivy-display-function-fallback)
        (counsel-recentf            . ivy-display-function-fallback)
        (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
        (dmenu                      . ivy-posframe-display-at-frame-top-center)
        (nil                        . ivy-posframe-display))
      ivy-posframe-height-alist
      '((swiper . 20)
        (dmenu . 20)
        (t . 10)))
(ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.

(map! :leader
      (:prefix ("v" . "Ivy")
       :desc "Ivy push view" "v p" #'ivy-push-view
       :desc "Ivy switch view" "v s" #'ivy-switch-view))

(setq display-line-numbers-type 'relative)

;; (custom-set-faces
;;  '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family "Monospace"))))
;;  '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.7))))
;;  '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.6))))
;;  '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.5))))
;;  '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.4))))
;;  '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.3))))
;;  '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.2)))))

(set-face-attribute 'mode-line nil :font "Monospace")
(setq doom-modeline-height 25     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(xterm-mouse-mode 1)

(after! neotree
  (setq neo-smart-open t
        neo-window-fixed-size nil))
(after! doom-themes
  (setq doom-neotree-enable-variable-pitch nil))
(map! :leader
      :desc "Toggle neotree file viewer" "t n" #'neotree-toggle
      :desc "Open directory in neotree" "d n" #'neotree-dir)

(map! :leader
      (:prefix ("r" . "registers")
       :desc "Copy to register" "c" #'copy-to-register
       :desc "Frameset to register" "f" #'frameset-to-register
       :desc "Insert contents of register" "i" #'insert-register
       :desc "Jump to register" "j" #'jump-to-register
       :desc "List registers" "l" #'list-registers
       :desc "Number to register" "n" #'number-to-register
       :desc "Interactively choose a register" "r" #'counsel-register
       :desc "View a register" "v" #'view-register
       :desc "Window configuration to register" "w" #'window-configuration-to-register
       :desc "Increment register" "+" #'increment-register
       :desc "Point to register" "SPC" #'point-to-register))

(setq shell-file-name "/bin/zsh"
      vterm-max-scrollback 5000)
(setq eshell-aliases-file "~/.config/doom/eshell/aliases"
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands'("bash" "htop" "ssh" "top" "zsh"))
(map! :leader
      :desc "Eshell" "e s" #'eshell
      :desc "Eshell popup toggle" "e t" #'+eshell/toggle
      :desc "Counsel eshell history" "e h" #'counsel-esh-history
      :desc "Vterm popup toggle" "t t" #'+vterm/toggle)

(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

(map! :leader
      (:prefix ("w" . "window")
       :desc "Winner redo" "<right>" #'winner-redo
       :desc "Winner undo" "<left>" #'winner-undo))

(map! :leader
      :desc "Zap to char" "z" #'zap-to-char
      :desc "Zap up to char" "Z" #'zap-up-to-char)

(+global-word-wrap-mode +1)

(defun my-c-hook-settings ()
  (setq +format-on-save-enabled-modes nil)
  #'lsp)

(add-hook 'c-mode-hook #'my-c-hook-settings)

(map! :after evil
      :map evil-normal-state-map
      "ZZ"      #'doom/save-and-kill-buffer
      "ZQ"      #'kill-current-buffer)

(map! :leader
      :desc "Previous workspace" "TAB h" #'+workspace/switch-left
      :desc "Previous workspace" "TAB l" #'+workspace/switch-right
      :desc "Toggle syntax highlighting" "t h" #'tree-sitter-hl-mode)

(map! :leader
      :desc "Quit Emacs" "q e" #'save-buffers-kill-terminal
      :desc "Delete frame" "q q" #'delete-frame)

(global-tree-sitter-mode)
;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(setq c-tab-always-indent nil)
(evil-define-key 'insert c-mode-map (kbd "TAB"), nil)

(defun sxhkd-restart-on-save ()
    "Restart sxhkd daemon"
    ;; (when (eq major-mode 'conf-space-mode)
      (if (string= buffer-file-name "/home/anon/.config/sxhkd/sxhkdrc")
          (shell-command "kill -SIGUSR1 \"$(pidof sxhkd)\"")))

(defun sxhkd-hook ()
  "Add hook for sxhkd file"
  (add-hook 'after-save-hook #'sxhkd-restart-on-save))

(add-hook 'conf-space-mode-hook #'sxhkd-hook)
;; (add-hook 'after-save-hook #'sxhkd-restart-on-save)

(setq company-minimum-prefix-length 2
      company-idle-delay 0.0) ;; default is 0.2

(evil-global-set-key 'insert (kbd "M-v") 'evil-paste-after)
