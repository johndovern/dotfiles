(load! "funcs.el")

(use-package! emojify
  :hook (after-init . global-emojify-mode))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq c-tab-always-indent nil)
(defconst my-c++-style
  '("bsd"
    (c-basic-offset . 4)
    (c-hanging-braces-alist . ((arglist-close before)))
    (c-offsets-alist . ((namespace-open . 0)
                        (innamespace . 0)
                        (label . -)
                        (access-label . -)
                        (inclass . +)
                        (substatement-label . 0)
                        (case-lable . 0)
                        (arglist-intro . +)
                        ;; (arglist-cont . +)
                        ;; (arglist-cont-nonempty . +)
                        (arglist-close . 0)))))
(defun my-c++-mode-hook ()
  (c-add-style "my-c++-style" my-c++-style)
  (c-set-style "my-c++-style")
  (setq! c-default-style "my-c++-style"
         lsp-ui-sideline-enable t
         c-basic-offset 4
         tab-width 4))
(add-hook! 'c++-mode-hook 'my-c++-mode-hook)
(c-add-style "my-c++-style" my-c++-style)
(setq! c-default-style "my-c++-style"
       lsp-ui-sideline-enable t
       c-basic-offset 4
       tab-width 4)

;; close dap-output on exit
(add-hook 'dap-terminated-hook #'debug-cleanup-output)
(after! dap-mode
  (dap-register-debug-template
   "cpptools::Run"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run"
         :args []
         :MIMode "gdb"
         :program "${workspaceFolder}/"
         :cwd     "${workspaceFolder}"))
  (dap-register-debug-template
   "cpptools::Run::clox::cli"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run::clox::cli"
         :args []
         :MIMode "gdb"
         :program "${workspaceFolder}/clox"
         :cwd "${workspaceFolder}"))
  (dap-register-debug-template
   "cpptools::Run::clox::script"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run::clox::script"
         :args ["./script.lox"]
         :MIMode "gdb"
         :program "${workspaceFolder}/clox"
         :cwd "${workspaceFolder}"))
  (dap-register-debug-template
   "cpptools::Run::wk::args"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run::wk::args"
         :args ["--fg"]
         :MIMode "gdb"
         :program "${workspaceFolder}/wk"
         :cwd "${workspaceFolder}"))
  (dap-register-debug-template
   "cpptools::Run::wk::chordFile"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run::wk::chordFile"
         :args ["-D" "--chords" "./example.wks"]
         :MIMode "gdb"
         :program "${workspaceFolder}/wk"
         :cwd "${workspaceFolder}"))
  (dap-register-debug-template
   "cpptools::Run::wk::main"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run::wk::main"
         :args []
         :MIMode "gdb"
         :program "${workspaceFolder}/wk"
         :cwd "${workspaceFolder}"))
  (dap-register-debug-template
   "cpptools::Run::wkx-parser::binds"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run::wkx-parser::debug"
         :args ["-b" "./examplerc"]
         :MIMode "gdb"
         :program "${workspaceFolder}/wkx-parser"
         :cwd "${workspaceFolder}"))
  (dap-register-debug-template
   "cpptools::Run::wkx-parser::debug"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run::wkx-parser::debug"
         :args ["-d" "./examplerc"]
         :MIMode "gdb"
         :program "${workspaceFolder}/wkx-parser"
         :cwd "${workspaceFolder}")))

(setq +doom-dashboard-pwd-policy "~/"
      fancy-splash-image "~/.config/doom/doom-emacs-dash.png")

(after! org (add-to-list 'org-modules 'ol-info))

(setq delete-by-moving-to-trash t
      trash-directory "~/.local/share/Trash/files/")

(setq +word-wrap-extra-indent nil)

(after! tree-sitter
  (global-tree-sitter-mode))
(setq +tree-sitter-hl-enabled-modes t)

;; (setq company-statistics-mode t)
(setq company-minimum-prefix-length 2
      company-idle-delay 0.2) ;; default is 0.2

(setq lsp-signature-doc-lines 5)
(after! company
  (setq +company-backend-alist
        '((company-files
           company-capf
           :with
           company-yasnippet
           :separate))))
(after! lsp
  (setq +lsp-company-backends
        '(company-files
          company-capf
          :with
          company-yasnippet
          :separate)))
(add-hook! 'prog-mode-hook
  (setq +lsp-company-backends
        '(company-files
          company-capf
          :with
          company-yasnippet
          :separate)))
(setq company-backends
      '((company-files
         company-capf
         :with
         company-yasnippet
         company-keywords
         company-dict
         company-dabbrev-code
         company-dabbrev
         :separate)))
(add-hook! 'lsp-completion-mode-hook
  (setf (alist-get 'lsp-capf completion-category-defaults) '((styles . (basic)))))
;; (remove-hook 'lsp-completion-mode-hook '+lsp-init-company-backends-h)
;; (add-hook 'lsp-completion-mode-hook
;;           (defun +lsp-init-company-backends-h ()
;;             (if lsp-completion-mode
;;                 (progn
;;                   ;; (setq +lsp-company-backends '(company-capf :with company-yasnippet))
;;                   (setq +lsp-company-backends '(:separate company-files company-capf))
;;                   (set
;;                    (make-local-variable 'company-backends)
;;                    (cons +lsp-company-backends
;;                          (remove +lsp-company-backends
;;                                  (->> company-backends
;;                                       (remq 'company-yasnippet)
;;                                       (remq 'company-capf)))))))))
;; (setq +company-backend-alist
;;       '((text-mode (:separate company-dabbrev company-yasnippet company-ispell))
;;         (prog-mode company-capf company-yasnippet)
;;         (conf-mode company-capf company-dabbrev-code company-yasnippet)))
;; (after! company
;;   (setq +company-backend-alist
;;         '((:separate company-files company-capf))))
;; (after! lsp
;;   (if (modulep! :editor snippets)
;;       (setq +lsp-company-backends '(:separate company-files company-capf company-yasnippet))
;;     (setq +lsp-company-backends '(:separate company-files company-capf))))
;; (after! lsp
;;   (setq +lsp-company-backends
;;         '(:separate company-files company-capf)))
;; (add-hook! 'prog-mode-hook
;;   (setq +lsp-company-backends '(:separate company-files company-capf)))
;; (defvar +lsp-company-backends
;;   (if (modulep! :editor snippets)
;;       '(:separate company-capf company-yasnippet)
;;     'company-capf)
;; (setq +lsp-company-backends
;;       '(company-files
;;         company-capf
;;         company-dabbrev-code
;;         company-dabbrev
;;         company-yasnippet))
;; (setq company-backends
;;       '((:separate
;;          company-files
;;          company-keywords
;;          company-dict
;;          company-yasnippet
;;          company-dabbrev-code
;;          company-dabbrev)))

;; (after! lsp-pylsp
;;   (setq lsp-pylsp-plugins-pydocstyle-ignore
;;         ["D100" ; Missing docstring in public module
;;          "D101"
;;          "D102"
;;          "D103" ; Missing docstring in public function
;;          "D105"
;;          "D107"
;;          "D212" ; Multi-line docstring summary should start at the second line
;;          "D413" ; Missing blank line after last section
;;          "D401" ; First line should be in imperative mood
;;          "D403"]
;;         lsp-pylsp-plugins-rope-autoimport-enabled t
;;         lsp-pylsp-plugins-jedi-completion-include-params nil))

(modify-syntax-entry ?_ "w")

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      scroll-preserve-screen-position 'always     ; Don't have `point' jump around
      scroll-margin 5)                            ; It's nice to maintain a little margin

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(setq org-roam-directory "~/ewiki")

(setq +workspaces-main "master")

(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
           :unnarrowed t)
          ("n" "ncmpcpp" plain "\n\n* ${title}\n%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+filetags:\"ncmpcpp_notes\" \"${title}\" \"ncurses\"\n#+startup: show2levels")
           :unnarrowed t))))

(setf (alist-get '(markdown-mode org-mode org-roam-mode) +spell-excluded-faces-alist)
      '(markdown-code-face
        markdown-reference-face
        markdown-link-face
        markdown-url-face
        markdown-markup-face
        markdown-html-attr-value-face
        markdown-html-attr-name-face
        markdown-html-tag-name-face))

;; With dired-open plugin, you can launch external programs for certain extensions
;; For example, I set all .png files to open in 'sxiv' and all .mp4 files to open in 'mpv'
(setq dired-open-extensions '(("gif" . "nsxiv")
                              ("jpg" . "nsxiv")
                              ("png" . "nsxiv")
                              ("pdf" . "sioyek")
                              ("mkv" . "mpv")
                              ("mp4" . "mpv")))

;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-kanagawa)
;; (add-hook! 'highlight-indent-guides-mode-hook
;;   (doom/reload-theme))
;; (add-hook! 'highlight-indent-guides-mode-hook
;;   (remove-hook! 'highlight-indent-guides-mode-hook
;;     #'doom/reload-theme))
;; (setq my-theme-reloaded nil)
(add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
           #'my-reload-theme)
;; (add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
;;            #'doom/reload-theme)
;; (add-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
;;   (remove-hook! '(prog-mode-hook text-mode-hook conf-mode-hook)
;;     #'doom/reload-theme))
;; (after! indent-guides
;;   (setq doom-theme 'doom-kanagawa))

;; Get file icons in dired
;; "-ahl -v --group-directories-first"
(add-hook! 'dired-mode-hook
           ;; 'all-the-icons-dired-mode
           'dired-hide-details-mode)

(setq doom-font (font-spec :font "Monospace" :size 15.0)
      doom-big-font (font-spec :font "Monospace" :size 36)
      doom-variable-pitch-font (font-spec :font "Sans" :size 15.0)
      doom-symbol-font (font-spec :font "Monospace" :size 15.0)
      doom-serif-font (font-spec :font "Monospace" :size 15.0))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t
        doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-treemacs-theme 'doom-kanagawa
        doom-kanagawa-brighter-comments t
        doom-kanagawa-red-cursor t
        doom-one-light-brighter-comments t
        doom-one-brighter-comments t))

(after! treemacs
  (setq! treemacs-width 20
         treemacs-show-cursor t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic))

(setq display-line-numbers-type nil)

(set-face-attribute 'mode-line nil :font "Monospace")

(setq doom-modeline-height 25     ;; sets modeline height
      doom-modeline-bar-width 5   ;; sets right bar width
      doom-modeline-persp-name t  ;; adds perspective name to modeline
      doom-modeline-persp-icon t) ;; adds folder icon next to persp name

(xterm-mouse-mode 1)

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
(add-hook 'dap-terminated-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(setq hscroll-margin 6)

(run-after-saving-unix-mode "/bindsrc$" "wkx-update --binds")
(run-after-saving-unix-mode "/keysrc$" "wkx-update --keys")

(add-to-list 'auto-mode-alist '("xresources" . conf-mode))
(run-after-saving 'conf-mode-hook "/xresources$"
                  "xrdb \"$HOME/.config/x11/xresources\"")
;; (maybe-run-after-saving 'c-mode-hook "/config\.h$" "dwmup")
;; (add-hook! 'conf-mode-hook
;;   (when (stringp buffer-file-name)
;;     (when (string-match-p "/xresources$" buffer-file-name)
;;       (add-hook! 'after-save-hook :local
;;         (shell-command "xrdb \"$HOME/.config/x11/xresources\"")))))

(run-after-saving-unix-mode "/dunstrc$" "pkill dunst; systemctl --user restart dunst.service")
;; (add-hook! 'conf-unix-mode-hook
;;   (when (stringp buffer-file-name)
;;       (if (string-match-p "/dunstrc$" buffer-file-name)
;;           (add-hook! 'after-save-hook :local
;;             (shell-command-to-string "systemctl --user restart dunst.service")))))

;; (setq spell-fu-ignore-modes '(org-mode org-roam-mode))
;; (after! (:or org org-roam)
(add-hook! '(org-mode-hook org-roam-mode-hook)
            #'auto-fill-mode
            (setq-local fill-column 60)
            (spell-fu-mode -1))
(add-hook! doom-switch-buffer
  (when (eq major-mode 'vterm-mode)
    (evil-collection-vterm-insert)))
;; (advice-add '+vterm/toggle :around
;;             (lambda (fn &rest args) (apply fn args)
;;               (when (eq major-mode 'vterm-mode)
;;                 (evil-collection-vterm-insert))))

(setq doom-leader-key ","
      doom-leader-alt-key "M-,"
      doom-localleader-key "SPC"
      doom-localleader-alt-key "M-SPC")

(map! :leader
      (:prefix ("b" . "buffer")
       :desc "List bookmarks" "L" #'list-bookmarks
       :desc "Save current bookmarks to bookmark file" "w" #'bookmark-save))

(evil-define-key 'normal ibuffer-mode-map
        (kbd "f c") 'ibuffer-filter-by-content
        (kbd "f d") 'ibuffer-filter-by-directory
        (kbd "f f") 'ibuffer-filter-by-filename
        (kbd "f m") 'ibuffer-filter-by-mode
        (kbd "f n") 'ibuffer-filter-by-name
        (kbd "f x") 'ibuffer-filter-disable
        (kbd "g h") 'ibuffer-do-kill-lines
        (kbd "g H") 'ibuffer-update)

(after! dired
  (setq dired-listing-switches "-ahvl --group-directories-first"))
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

(evil-define-key 'normal peep-dired-mode-map
  (kbd "j") 'peep-dired-next-file
  (kbd "k") 'peep-dired-prev-file)
(add-hook 'peep-dired-hook 'evil-normalize-keymaps)

(map! :leader
      :desc "Load new theme" "H t" #'consult-theme)

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

(map! :map dap-mode-map
      :leader
      (:prefix ("d" . "dap")
       ;; basics
       :desc "dap next"          "n" #'dap-next
       :desc "dap step in"       "i" #'dap-step-in
       :desc "dap step out"      "o" #'dap-step-out
       :desc "dap continue"      "c" #'dap-continue
       :desc "dap hydra"         "h" #'dap-hydra
       :desc "dap debug restart" "r" #'dap-debug-restart
       :desc "dap debug"         "s" #'dap-debug
       ;; debug
       (:prefix ("d" . "Debug")
        :desc "dap debug recent"  "r" #'dap-debug-recent
        :desc "dap debug last"    "l" #'dap-debug-last)
       ;; eval
       (:prefix ("e" . "Eval")
        :desc "eval"                "e" #'dap-eval
        :desc "eval region"         "r" #'dap-eval-region
        :desc "eval thing at point" "s" #'dap-eval-thing-at-point
        :desc "add expression"      "a" #'dap-ui-expressions-add
        :desc "remove expression"   "d" #'dap-ui-expressions-remove)
       ;; breakpoint
       (:prefix ("b" . "Breakpoint")
        :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
        :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
        :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
        :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)
       ;; debug
       (:prefix ("t" . "Template")
        :desc "dap edit template" "e" #'dap-debug-edit-template)))

(map! :leader
      (:prefix ("e" . "Eshell")
       :desc "Eshell" "e s" #'eshell
       :desc "Counsel eshell history" "e h" #'counsel-esh-history))

(map! :leader
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

(map! :leader
      :desc "Clone indirect buffer other window" "b c" #'clone-indirect-buffer-other-window)

(map! :leader
      (:prefix ("w" . "window")
       :desc "Winner redo" "<right>" #'winner-redo
       :desc "Winner undo" "<left>" #'winner-undo))

(map! :leader
      :desc "Zap to char" "z" #'zap-to-char
      :desc "Zap up to char" "Z" #'zap-up-to-char)

(defun my-c-hook-settings ()
  (setq-local +format-with-lsp nil)
  (setq c-basic-offset 4))
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
      :desc "Previous workspace"         "TAB h" #'+workspace/switch-left
      :desc "Previous workspace"         "TAB l" #'+workspace/switch-right
      :desc "Toggle syntax highlighting" "t h"   #'tree-sitter-hl-mode
      :desc "Toggle treemacs"            "t r"   #'treemacs)
      ;; :desc "Toggle theme"               "t d"   #'toggle-my-theme)

(map! :leader
      :desc "Quit Emacs"   "q e" #'save-buffers-kill-terminal
      :desc "Delete frame" "q q" #'save-buffers-kill-emacs)

;; (map! :localleader
;;       :map org-mode-map
;;       (:prefix ("m" . "my maps")
;;        (:prefix ("e" . "export")
;;         :desc "Export to gfm" "g" #'org-pandoc-export-to-gfm
;;         :desc "Export as gfm" "G" #'org-pandoc-export-as-gfm)))

(map! :leader
      (:prefix ("t" . "toggle")
       :desc "Toggle line numbers" "L" #'doom/toggle-line-numbers
       :desc "Toggle lsp server"   "l" #'lsp-workspace-restart))

(evil-global-set-key 'insert (kbd "M-v") 'evil-paste-before)
(evil-global-set-key 'insert (kbd "C-e") 'evil-scroll-line-to-center)

(map! :after evil
      :map evil-normal-state-map
      "C-z"     nil
      "q q"     #'evil-fill-and-move
      "Q"       #'evil-fill-and-move)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (consult-buffer))

(global-set-key (kbd "C-z") nil)
(map! :im "C-z" nil)
(map! :leader
      "h" nil
      :desc "Help" "H" help-map
      "h" #'evil-window-left
      "j" #'evil-window-down
      "k" #'evil-window-up
      "l" #'evil-window-right)

(map! :leader
      (:prefix ("TAB" . "workspace")
       "s" nil
       (:prefix ("s" . "switch")
        :desc "Switch to 1st workspace" "a" #'+workspace/switch-to-0
        :desc "Switch to 2nd workspace" "r" #'+workspace/switch-to-1
        :desc "Switch to 3rd workspace" "s" #'+workspace/switch-to-2
        :desc "Switch to 4th workspace" "t" #'+workspace/switch-to-3
        :desc "Switch to 5th workspace" "n" #'+workspace/switch-to-4
        :desc "Switch to 6th workspace" "e" #'+workspace/switch-to-5
        :desc "Switch to 7th workspace" "i" #'+workspace/switch-to-6
        :desc "Switch to 8th workspace" "o" #'+workspace/switch-to-7)
       :desc "Save to workspace file"     "S"   #'+workspace/save
       :desc "Switch to last workspace"   "TAB" #'+workspace/other
       :desc "Display tab bar"            "."   #'+workspace/display
       :desc "List workspaces"            "o"   #'+workspace/switch-to))

;; (map! :leader
;;       (:prefix ("r" . "roam")
;;        (:prefix ("n" . "node")
;;         :desc "Find node" "f" #'org-roam-node-find
;;         :desc "Insert node" "i" #'org-roam-node-insert)
;;        (:prefix ("l" . "links")
;;         :desc "Yank link" "y" #'org-store-link
;;         :desc "Paste link" "p" #'org-insert-link)))

(map! :leader
      :desc "Find node" "f n" #'org-roam-node-find
      "n l" nil
      (:prefix ("n" . "notes")
       (:prefix ("l" . "links")
        :desc "Yank link"  "y" #'org-store-link
        :desc "Paste link" "p" #'org-insert-link)))

(map! :localleader
      (:prefix ("l" . "links")
       (:prefix ("r" . "references")
        :desc "URL"          "u" #'org-insert-link-from-clipboard
        :desc "ID"           "i" #'org-add-id-link
        :desc "ID +desc"     "I" #'org-add-id-link-desc
        :desc "Header"       "h" #'org-add-header-link
        :desc "Header +desc" "H" #'org-add-header-link-desc)))

(map! :leader
      :desc "Get files" "c g" (lambda () (interactive) (run-command-in-vterm "grep -R")))
