;;; doom-kanagawa-day-theme.el --- inspired by rebelot/kanagawa.nvim and others -*- lexical-binding: t; no-byte-compile: t; -*-
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

(defgroup doom-kanagawa-day-theme nil
  "Options for the `doom-kanagawa' theme."
  :group 'doom-themes)

(defcustom doom-kanagawa-day-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-kanagawa-day-theme
  :type 'boolean)

(defcustom doom-kanagawa-day-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-kanagawa-day-theme
  :type 'boolean)

(defcustom doom-kanagawa-day-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'doom-kanagawa-day-theme
  :type '(choice integer boolean))


;;
;;; Theme definition

(def-doom-theme doom-kanagawa-day
  "A dark theme inspired by rebelot/kanagawa.nvim and others."

  ;; name        default   256           16
  ((bg         '("#dcd7ba" "black"       "black"  ))
   (fg         '("#1d1c1c" "#1d1c1c"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#d5cea3" "black"       "black"        ))
   (fg-alt     '("#181616" "#181616"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#f2ecbc" "black"       "black"        ))
   (base1      '("#e7dba0" "#e7dba0"     "brightblack"  ))
   (base2      '("#e5ddb0" "#e5ddb0"     "brightblack"  ))
   (base3      '("#e4d794" "#e4d794"     "brightblack"  ))
   (base4      '("#8a8980" "#8a8980"     "brightblack"  ))
   (base5      '("#716e61" "#716e61"     "brightblack"  ))
   (base6      '("#545451" "#545451"     "brightblack"  ))
   (base7      '("#464546" "#464546"     "brightblack"  ))
   (base8      '("#37363A" "#37363A"     "white"        ))

   (grey       base4)
   (red        '("#c84053" "#c84053" "red"          ))
   (orange     '("#cc6d00" "#cc6d00" "brightred"    ))
   (green      '("#6f894e" "#6f894e" "green"        ))
   (teal       '("#4e8ca2" "#4e8ca2" "brightgreen"  ))
   (yellow     '("#de9800" "#de9800" "yellow"       ))
   (blue       '("#5d57a3" "#5d57a3" "brightblue"   ))
   (dark-blue  '("#4d699b" "#4d699b" "blue"         ))
   (magenta    '("#766b90" "#766b90" "brightmagenta"))
   (violet     '("#624c83" "#624c83" "magenta"      ))
   (cyan       '("#6693bf" "#6693bf" "brightcyan"   ))
   (dark-cyan  '("#5a7785" "#5a7785" "cyan"         ))

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   ;; fi
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       (if doom-kanagawa-day-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-kanagawa-day-brighter-comments dark-cyan base5) 0.25))
   (constants      violet)
   (functions      (doom-darken magenta 1))
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        green)
   (variables      magenta)
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; These are extra color variables used only in this theme; i.e. they aren't
   ;; mandatory for derived themes.
   (modeline-fg              fg)
   (modeline-fg-alt          base5)
   (modeline-bg              (if doom-kanagawa-day-brighter-modeline
                                 (doom-darken blue 0.45)
                               (doom-darken bg-alt 0.1)))
   (modeline-bg-alt          (if doom-kanagawa-day-brighter-modeline
                                 (doom-darken blue 0.475)
                               `(,(doom-darken (car bg-alt) 0.15) ,@(cdr bg))))
   (modeline-bg-inactive     `(,(car bg-alt) ,@(cdr base1)))
   (modeline-bg-inactive-alt `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg)))

   (-modeline-pad
    (when doom-kanagawa-day-padded-modeline
      (if (integerp doom-kanagawa-day-padded-modeline) doom-kanagawa-padded-modeline 4))))


  ;;;; Base theme face overrides
  (((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)
   ((font-lock-comment-face &override)
    :background (if doom-kanagawa-day-brighter-comments (doom-lighten bg 0.05)))
   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis :foreground (if doom-kanagawa-day-brighter-modeline base8 highlight))

   ;;;; css-mode <built-in> / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)
   ;;;; doom-modeline
   (doom-modeline-bar :background (if doom-kanagawa-day-brighter-modeline modeline-bg highlight))
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

;;; doom-kanagawa-day-theme.el ends here
