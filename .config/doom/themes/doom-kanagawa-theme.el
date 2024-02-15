;;; doom-kanagawa-theme.el --- inspired by rebelot/kanagawa.nvim and others -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: June 4 2023
;; Author: Anskrevy <https://github.com/Anskrevy
;; Maintainer:
;; Source: ????
;;
;;; Commentary:
;;; Original theme by rebelot see: https://github.com/rebelot/kanagawa.nvim
;;; Inspiration taken from modified version in https://github.com/NvChad/base46
;;; and konrad1977 https://github.com/konrad1977/emacs .
;;; fi
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-kanagawa-theme nil
  "Options for the `doom-kanagawa' theme."
  :group 'doom-themes)

(defcustom doom-kanagawa-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-kanagawa-theme
  :type 'boolean)

(defcustom doom-kanagawa-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-kanagawa-theme
  :type 'boolean)

(defcustom doom-kanagawa-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-kanagawa-theme
  :type '(choice integer boolean))

(defcustom doom-kanagawa-red-cursor nil
  "If non-nil, cursor will be red."
  :group 'doom-kanagawa-theme
  :type 'boolean)


;;
;;; Theme definition

(def-doom-theme doom-kanagawa
  "A dark theme inspired by rebelot/kanagawa.nvim and others."

  ;; name        default   256           16
  ((bg         '("#181616" "black"       "black"  ))
   (fg         '("#DCD7BA" "#DCD7BA"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#1D1C19" "black"       "black"        ))
   (fg-alt     '("#C8C093" "#C8C093"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#191922" "black"       "black"        ))
   (base1      '("#28282E" "#28282E"     "brightblack"  ))
   (base2      '("#222129" "#222129"     "brightblack"  ))
   (base3      '("#26252C" "#26252C"     "brightblack"  ))
   (base4      '("#37363A" "#37363A"     "brightblack"  ))
   (base5      '("#464546" "#464546"     "brightblack"  ))
   (base6      '("#545451" "#545451"     "brightblack"  ))
   (base7      '("#727169" "#727169"     "brightblack"  ))
   (base8      '("#BABDB9" "#BABDB9"     "white"        ))

   (grey       base4)
   (red        '("#FF5D62" "#FF5D62" "red"          ))
   (orange     '("#FFA066" "#FFA066" "brightred"    ))
   (green      '("#98BB6C" "#98BB6C" "green"        ))
   (teal       '("#7AA89F" "#7AA89F" "brightgreen"  ))
   (yellow     '("#E6C384" "#E6C384" "yellow"       ))
   (blue       '("#7FB4CA" "#7FB4CA" "brightblue"   ))
   (dark-blue  '("#7E9CD8" "#7E9CD8" "blue"         ))
   (magenta    '("#957FB8" "#957FB8" "brightmagenta"))
   (violet     '("#9CABCA" "#9CABCA" "magenta"      ))
   (cyan       '("#A3D4D5" "#A3D4D5" "brightcyan"   ))
   (dark-cyan  '("#658594" "#658594" "cyan"         ))

   ;; Custom colors
   (oldWhite      '("#C8C093" "#C8C093" "oldWhite"))
   (fujiWhite     '("#DCD7BA" "#DCD7BA" "fujiWhite"))
   (fujiGray      '("#727169" "#727169" "fujiGray"))

   (oniViolet     '("#957FB8" "#957FB8" "oniViolet"))
   (oniViolet2    '("#b8b4d0" "#b8b4d0" "oniViolet2"))
   (crystalBlue   '("#7E9CD8" "#7E9CD8" "crystalBlue"))
   (springViolet1 '("#938AA9" "#938AA9" "springViolet1"))
   (springViolet2 '("#9CABCA" "#9CABCA" "springViolet2"))
   (springBlue    '("#7FB4CA" "#7FB4CA" "springBlue"))
   (lightBlue     '("#A3D4D5" "#A3D4D5" "lightBlue"))
   (waveAqua2     '("#7AA89F" "#7AA89F" "waveAqua2"))

   (springGreen   '("#98BB6C" "#98BB6C" "springGreen"))
   (boatYellow1   '("#938056" "#938056" "boatYellow1"))
   (boatYellow2   '("#C0A36E" "#C0A36E" "boatYellow2"))
   (carpYellow    '("#E6C384" "#E6C384" "carpYellow"))

   (sakuraPink    '("#D27E99" "#D27E99" "sakuraPink"))
   (waveRed       '("#E46876" "#E46876" "waveRed"))
   (peachRed      '("#FF5D62" "#FF5D62" "peachRed"))
   (surimiOrange  '("#FFA066" "#FFA066" "surimiOrange"))
   (katanaGray    '("#717C7C" "#717C7C" "katanaGray"))

   (dragonBlack0  '("#0d0c0c" "#0d0c0c" "dragonBlack0"))
   (dragonBlack1  '("#12120f" "#12120f" "dragonBlack1"))
   (dragonBlack2  '("#1D1C19" "#1D1C19" "dragonBlack2"))
   (dragonBlack3  '("#181616" "#181616" "dragonBlack3"))
   (dragonBlack4  '("#282727" "#282727" "dragonBlack4"))
   (dragonBlack5  '("#393836" "#393836" "dragonBlack5"))
   (dragonBlack6  '("#625e5a" "#625e5a" "dragonBlack6"))

   (dragonWhite   '("#c5c9c5" "#c5c9c5" "dragonWhite"))
   (dragonGreen   '("#87a987" "#87a987" "dragonGreen"))
   (dragonGreen2  '("#8a9a7b" "#8a9a7b" "dragonGreen2"))
   (dragonPink    '("#a292a3" "#a292a3" "dragonPink"))
   (dragonOrange  '("#b6927b" "#b6927b" "dragonOrange"))
   (dragonOrange2 '("#b98d7b" "#b98d7b" "dragonOrange2"))
   (dragonGray    '("#a6a69c" "#a6a69c" "dragonGray"))
   (dragonGray2   '("#9e9b93" "#9e9b93" "dragonGray2"))
   (dragonGray3   '("#7a8382" "#7a8382" "dragonGray3"))
   (dragonBlue2   '("#8ba4b0" "#8ba4b0" "dragonBlue2"))
   (dragonViolet  '("#8992a7" "#8992a7" "dragonViolet"))
   (dragonRed     '("#c4746e" "#c4746e" "dragonRed"))
   (dragonAqua    '("#8ea4a2" "#8ea4a2" "dragonAqua"))
   (dragonAsh     '("#737c73" "#737c73" "dragonAsh"))
   (dragonTeal    '("#949fb5" "#949fb5" "dragonTeal"))
   (dragonYellow  '("#c4b28a" "#c4b28a" "dragonYellow"))


   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      (doom-darken dark-blue 0.5))
   (builtin        magenta)
   (comments       (if doom-kanagawa-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-kanagawa-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      magenta)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      (doom-lighten magenta 0.4))
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; (highlight      fg-alt)
   ;; (vertical-bar   (doom-lighten bg 0.05))
   ;; (selection      (doom-darken dark-blue 0.5))
   ;; (builtin        dragonViolet)
   ;; (comments       (if doom-kanagawa-brighter-comments dark-cyan base5))
   ;; (doc-comments   (doom-lighten (if doom-kanagawa-brighter-comments dark-cyan base5) 0.25))
   ;; (constants      orange)
   ;; (functions      dragonBlue2)
   ;; (keywords       dragonViolet)
   ;; (methods        blue)
   ;; (operators      dragonRed)
   ;; (type           yellow)
   ;; (strings        dragonGreen2)
   ;; (variables      base8)
   ;; (numbers        orange)
   ;; (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   ;; (error          red)
   ;; (warning        yellow)
   ;; (success        green)
   ;; (vc-modified    orange)
   ;; (vc-added       green)
   ;; (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-kanagawa-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-kanagawa-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-kanagawa-padded-modeline
      (if (integerp doom-kanagawa-padded-modeline) doom-kanagawa-padded-modeline 4))))


  ;;;; Base theme face overrides
  ((cursor :background (if doom-kanagawa-red-cursor red fg-alt))
   ((line-number &override) :foreground base5)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-kanagawa-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-kanagawa-brighter-modeline base8 highlight))

   ;;;; indent-guides
   (highlight-indent-guides-character-face :foreground base4)
   (highlight-indent-guides-top-character-face :foreground base4)
   (highlight-indent-guides-stack-character-face :foreground base4)

   ;;;; evil-snipe
   (evil-snipe-first-match-face :foreground (if doom-kanagawa-red-cursor red blue) :background bg-alt)

   ;;;; treesitter
   ;; (tree-sitter-hl-face:function.call   :foreground dragonOrange)
   ;; (tree-sitter-hl-face:number          :foreground dragonPink)
   ;; (tree-sitter-hl-face:operator        :foreground dragonRed)

   ;;;; dashboard
   ;; (doom-dashboard-menu-desc    :foreground dragonOrange)
   ;; (doom-dashboard-menu-title   :foreground violet)

   ;; ;;;; whichkey
   ;; (which-key-key-face                  :foreground dragonRed)
   ;; (which-key-command-description-face  :foreground dark-blue)
   ;; (which-key-group-description-face    :foreground dragonRed)
   ;; (which-key-separator-face            :foreground fujiGray)

   ;; ;;;; vertico
   ;; (vertico-current :inherit 'region :foreground carpYellow)

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-kanagawa-brighter-modeline modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)
   ;;;; elscreen
   (elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")
   ;;;; ivy
   (ivy-current-match :background dark-blue :distant-foreground base0 :weight 'normal)
   ;;;; LaTeX-mode
   (font-latex-math-face :foreground green)
   ;;;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   ((markdown-code-face &override) :background (doom-lighten base3 0.05))
   ;;;; rjsx-mode
   (rjsx-tag :foreground red)
   (rjsx-attr :foreground orange)
   ;;;; solaire-mode
   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-alt)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-alt))))

  ;;;; Base theme variable overrides-
  ())

;;; doom-kanagawa-theme.el ends here
