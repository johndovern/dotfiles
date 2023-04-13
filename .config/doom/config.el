;; Remap leader key
(setq doom-leader-key ","
      doom-leader-alt-key "M-,"
      doom-localleader-key "SPC"
      doom-localleader-alt-key "M-SPC")
(map! :leader
      (:prefix ("b". "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))
(load! "funcs.el")
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
;; (use-package dashboard
;;         :init      ;; tweak dashboard config before loading it
;;         (setq dashboard-set-heading-icons t)
;;         (setq dashboard-set-file-icons t)
;;         (setq dashboard-startup-banner "~/.config/doom/doom-emacs-dash.png")  ;; use custom image as banner
;;         (setq dashboard-center-content t) ;; set to 't' for centered content
;;         (setq dashboard-items '((recents . 5)
;;                                 (agenda . 5 )
;;                                 (bookmarks . 5)
;;                                 (projects . 5)
;;                                 (registers . 5)))
;;         :config
;;         (dashboard-setup-startup-hook)
;;         (dashboard-modify-heading-icons '((recents . "file-text")
;;                                           (bookmarks . "book"))))
;; (setq doom-fallback-buffer-name "*dashboard*")
;; Usefull if running emacs as a daemon
;; (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq +doom-dashboard-pwd-policy "~/"
      fancy-splash-image "~/.config/doom/doom-emacs-dash.png")
(map! :leader
      (:prefix ("d" . "dired")
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
(add-hook! 'dired-mode-hook
           'all-the-icons-dired-mode
           'dired-hide-details-mode)
;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "nsxiv")
                              ("jpg" . "nsxiv")
                              ("png" . "nsxiv")
                              ("pdf" . "llpp")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))
(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)
(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")
(setq doom-theme 'doom-one)
(map! :leader
      :desc "Load new theme" "H t" #'consult-theme)
(use-package emojify
  :hook (after-init . global-emojify-mode))
(setq doom-font (font-spec :font "Monospace" :size 20)
      doom-big-font (font-spec :font "Monospace" :size 36)
      doom-variable-pitch-font (font-spec :font "Sans" :size 20)
      doom-unicode-font (font-spec :font "Monospace" :size 20)
      doom-serif-font (font-spec :font "Monospace" :size 20))
(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-theme 'doom-one
        doom-one-light-brighter-comments t
        doom-one-brighter-comments t))
(after! treemacs
  (setq! treemacs-width 20
         treemacs-show-cursor t))
(custom-set-faces!
  '(font-lock-comment-face :slant italic))
;; (setq ivy-posframe-display-functions-alist
;;       '((swiper                     . ivy-posframe-display-at-point)
;;         (complete-symbol            . ivy-posframe-display-at-point)
;;         (counsel-M-x                . ivy-display-function-fallback)
;;         (counsel-esh-history        . ivy-posframe-display-at-window-center)
;;         (counsel-describe-function  . ivy-display-function-fallback)
;;         (counsel-describe-variable  . ivy-display-function-fallback)
;;         (counsel-find-file          . ivy-display-function-fallback)
;;         (counsel-recentf            . ivy-display-function-fallback)
;;         (counsel-register           . ivy-posframe-display-at-frame-bottom-window-center)
;;         (dmenu                      . ivy-posframe-display-at-frame-top-center)
;;         (nil                        . ivy-posframe-display))
;;       ivy-posframe-height-alist
;;       '((swiper . 20)
;;         (dmenu . 20)
;;         (t . 10)))
;; (ivy-posframe-mode 1) ; 1 enables posframe-mode, 0 disables it.
;; (map! :leader
;;       (:prefix ("v" . "Ivy")
;;        :desc "Ivy push view" "v p" #'ivy-push-view
;;        :desc "Ivy switch view" "v s" #'ivy-switch-view))
(setq display-line-numbers-type 'relative)
(set-face-attribute 'mode-line nil :font "Monospace")
(setq doom-modeline-height 25     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name
(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))
(xterm-mouse-mode 1)
(map! :leader
      (:prefix ("R" . "registers")
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
(require 'dap-cpptools)
(require 'dap-lldb)
(require 'dap-gdb-lldb)
(setq dap-ui-locals-expand-depth t)
(setq dap-auto-show-output nil)
(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))
(map! :map dap-mode-map
      :leader
      :prefix ("d" . "dap")
      ;; basics
      :desc "dap next"          "n" #'dap-next
      :desc "dap step in"       "i" #'dap-step-in
      :desc "dap step out"      "o" #'dap-step-out
      :desc "dap continue"      "c" #'dap-continue
      :desc "dap hydra"         "h" #'dap-hydra
      :desc "dap debug restart" "r" #'dap-debug-restart
      :desc "dap debug"         "s" #'dap-debug

      ;; debug
      :prefix ("dd" . "Debug")
      :desc "dap debug recent"  "r" #'dap-debug-recent
      :desc "dap debug last"    "l" #'dap-debug-last

      ;; eval
      :prefix ("de" . "Eval")
      :desc "eval"                "e" #'dap-eval
      :desc "eval region"         "r" #'dap-eval-region
      :desc "eval thing at point" "s" #'dap-eval-thing-at-point
      :desc "add expression"      "a" #'dap-ui-expressions-add
      :desc "remove expression"   "d" #'dap-ui-expressions-remove

      :prefix ("db" . "Breakpoint")
      :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
      :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
      :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
      :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message
      ;; debug
      :prefix ("dt" . "Template")
      :desc "dap edit template"  "e" #'dap-debug-edit-template)
(map! :leader
      :desc "Eshell" "e s" #'eshell
      :desc "Eshell popup toggle" "t e" #'+eshell/toggle
      :desc "Counsel eshell history" "e h" #'counsel-esh-history
      :desc "Vterm popup toggle" "t t" #'+vterm/toggle
      :desc "Open vterm" "t v" #'my-open-vterm)
(defun prefer-horizontal-split ()
  (set-variable 'split-height-threshold nil t)
  (set-variable 'split-width-threshold 40 t)) ; make this as low as needed
(add-hook 'markdown-mode-hook 'prefer-horizontal-split)
(map! :leader
      :prefix ("w". "window")
      :desc "Window enlargen" "i" #'doom/window-enlargen
      :desc "balance windows" "y" #'balance-windows)
(setq hscroll-margin 6)
(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)
(map! :leader
      (:prefix ("w" . "window")
       :desc "Winner redo" "<right>" #'winner-redo
       :desc "Winner undo" "<left>" #'winner-undo))
(map! :leader
      :desc "Zap to char" "z" #'zap-to-char
      :desc "Zap up to char" "Z" #'zap-up-to-char)
;; (+global-word-wrap-mode +1)
(setq +word-wrap-extra-indent nil)
(defun my-c-hook-settings ()
  (setq-local +format-on-save-enabled-modes nil
              c-basic-offset 4))
              ;; +tree-sitter-hl-enabled-modes t)
  ;; (add-hook! 'lsp-mode-hook #'tree-sitter-hl-mode)
  ;; #'lsp-restart-workspace)
;; (defun my-c-hook-settings ()
;;   (setq-local +tree-sitter-hl-enabled-modes t)
;;   (add-hook! 'lsp-mode-hook #'tree-sitter-hl-mode)
;;   #'lsp-restart-workspace)
  ;; #'lsp!
  ;; #'tree-sitter-hl-mode)
(add-hook! '(c-mode-hook c++-mode-hook)
           #'my-c-hook-settings)
(map! :after evil
      :map evil-normal-state-map
      "ZZ"      #'doom/save-and-kill-buffer
      "ZQ"      #'kill-current-buffer)
(defun toggle-my-theme ()
  "Toggle light and dark themes"
  (interactive)
  (if (eq doom-theme 'doom-one)
      (load-theme 'doom-one-light t)
    (load-theme 'doom-one t)))
(map! :leader
      :desc "Previous workspace" "TAB h" #'+workspace/switch-left
      :desc "Previous workspace" "TAB l" #'+workspace/switch-right
      :desc "Toggle syntax highlighting" "t h" #'tree-sitter-hl-mode
      :desc "Toggle treemacs" "t r" #'treemacs
      :desc "Toggle theme" "t d" #'toggle-my-theme)
(map! :leader
      :desc "Quit Emacs" "q e" #'save-buffers-kill-terminal
      :desc "Delete frame" "q q" #'save-buffers-kill-emacs)
(after! tree-sitter
  (global-tree-sitter-mode))
(setq +tree-sitter-hl-enabled-modes t)
;; (add-hook! 'python-mode-local-vars-hook '(lsp! tree-sitter-hl-mode))
(setq c-tab-always-indent nil)
;; (evil-define-key 'insert c-mode-map (kbd "TAB"), nil)
(add-hook! 'conf-unix-mode-hook
  (when (stringp buffer-file-name)
      (when (string-match-p "/keysrc$" buffer-file-name)
          (add-hook! 'after-save-hook :local
            (shell-command-to-string "kill -SIGUSR1 \"$(pidof wkx)\"")))
      (when (string-match-p "/bindsrc$" buffer-file-name)
        (add-hook! 'after-save-hook :local
          (run-shell-command-split-window "wkx-update")))))
(add-to-list 'auto-mode-alist '("xresources" . conf-mode))
(add-hook! 'conf-mode-hook
  (when (stringp buffer-file-name)
    (when (string-match-p "/xresources$" buffer-file-name)
      (add-hook! 'after-save-hook :local
        (shell-command "xrdb \"$HOME/.config/x11/xresources\"")))))
(add-hook! 'conf-unix-mode-hook
  (when (stringp buffer-file-name)
      (if (string-match-p "/dunstrc$" buffer-file-name)
          (add-hook! 'after-save-hook :local
            (shell-command-to-string "systemctl --user restart dunst.service")))))
(map! :localleader
      :map org-mode-map
      (:prefix ("m" . "my maps")
       (:prefix ("e" . "export")
        :desc "Export to gfm" "g" #'org-pandoc-export-to-gfm
        :desc "Export as gfm" "G" #'org-pandoc-export-as-gfm)))
;; (setq company-statistics-mode t)
(setq company-minimum-prefix-length 2
      company-idle-delay 0.0) ;; default is 0.2
(setq lsp-signature-doc-lines 5)
(setq +lsp-company-backends
      '(:separate company-files company-capf company-yasnippet company-dabbrev-code company-dabbrev))
(setq company-backends
      '((:separate company-files company-capf company-yasnippet company-dabbrev-code company-dabbrev)))
(add-hook! 'lsp-completion-mode-hook
           (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (basic)))))
(map! :leader
      :desc "Toggle line numbers" "t L" #'doom/toggle-line-numbers
      :desc "Toggle lsp server (restart)" "t l" #'lsp-workspace-restart)
(evil-global-set-key 'insert (kbd "M-v") 'evil-paste-before)
(evil-global-set-key 'insert (kbd "C-e") 'evil-scroll-line-to-center)
(map! :after evil
      :map evil-normal-state-map
      "Q"       #'evil-fill-and-move)
(modify-syntax-entry ?_ "w")
(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 5)                            ; It's nice to maintain a little margin
(setq evil-vsplit-window-right t
      evil-split-window-below t)
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))
(map! :leader
      "h" nil
      "H" help-map
      "h" #'evil-window-left
      "j" #'evil-window-down
      "k" #'evil-window-up
      "l" #'evil-window-right)
(map! :leader
      :desc "List workspaces" "TAB TAB" #'+workspace/switch-to
      :desc "Last workspace"  "TAB '"   #'+workspace/other
      :desc "Display tab bar" "TAB ."   #'+workspace/display)
(setq c-default-style "linux"
      c-basic-offset 4
      tab-width 4)
(defun my-c++-mode-hook ()
  (c-set-offset 'access-label '-4))
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
(setq org-roam-directory "~/ewiki")
(map! :leader
      (:prefix ("r" . "roam")
       (:prefix ("n" . "node")
        :desc "Find node" "f" #'org-roam-node-find
        :desc "Insert node" "i" #'org-roam-node-insert)
       (:prefix ("l" . "links")
        :desc "Yank link" "y" #'org-store-link
        :desc "Paste link" "p" #'org-insert-link)))
(map! :leader
      :desc "Find node" "f n" #'org-roam-node-find
      "n l" nil
      (:prefix ("n" . "notes")
       (:prefix ("l" . "+links")
        :desc "Yank link" "y" #'org-store-link
        :desc "Paste link" "p" #'org-insert-link)))
;; (setq spell-fu-ignore-modes '(org-mode org-roam-mode))
(add-hook! '(org-mode-hook org-roam-mode-hook)
           #'auto-fill-mode
           (setq-local fill-column 60)
           (spell-fu-mode -1))
           ;; (lambda () (spell-fu-mode -1)))
(setf (alist-get '(markdown-mode org-mode org-roam-mode) +spell-excluded-faces-alist)
      '(markdown-code-face
        markdown-reference-face
        markdown-link-face
        markdown-url-face
        markdown-markup-face
        markdown-html-attr-value-face
        markdown-html-attr-name-face
        markdown-html-tag-name-face))
(map! :localleader
      (:prefix ("l" . "+links")
       (:prefix ("r" . "+references")
        :desc "URL" "u" #'org-insert-link-from-clipboard
        :desc "ID" "i" #'org-add-id-link
        :desc "ID +desc" "I" #'org-add-id-link-desc
        :desc "Header" "h" #'org-add-header-link
        :desc "Header +desc" "H" #'org-add-header-link-desc)))
(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("n" "ncmpcpp" plain "\n\n* ${title}\n%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags:\"ncmpcpp_notes\" \"${title}\" \"ncurses\"\n#+startup: show2levels")
           :unnarrowed t))))
(map! :leader
      :desc "Get files" "c g" (lambda () (interactive) (run-command-in-vterm "grep -R")))
(setq +workspaces-main "master")
;; close dap-output on exit
(add-hook 'dap-terminated-hook #'debug-cleanup-output)
(dap-register-debug-template
 "cpptools::Run"
 (list :type "cppdbg"
       :request "launch"
       :name "cpptools::Run"
       :MIMode "gdb"
       :program "${workspaceFolder}/"
       :cwd     "${workspaceFolder}"))
