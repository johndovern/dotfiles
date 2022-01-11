# Bindings for normal mode
# Binds for mpv and yt-dlp
config.bind('.mh', 'hint links spawn --detach mpv --loop-playlist=no --keep-open=no {hint-url}')
config.bind('.uh', 'hint links spawn --detach umpv {hint-url}')
config.bind('.mn', 'spawn --detach mpv --loop-playlist=no --keep-open=no {url}')
config.bind('.un', 'spawn --detach umpv {url}')
config.bind('.ur', 'hint --rapid links spawn --detach umpv {hint-url}')
config.bind('.mps', 'hint links spawn --detach mpv --no-video --shuffle --loop-playlist=no --keep-open=no {hint-url}')
config.bind('.mf', 'hint links spawn --detatch fmpv --loop-playlist=no --keep-open=no {hint-url}')
config.bind('.dyph', 'hint links spawn --output-messages yt-dlp --config-location ~/.config/yt-dlp/audio-webms-playlist.conf {hint-url}')
config.bind('.dypn', 'spawn --output-messages yt-dlp --config-location ~/.config/yt-dlp/audio-webms-playlist.conf {url}')
config.bind('.dywh', 'hint links spawn --output-messages yt-dlp --config-location ~/.config/yt-dlp/audio-webms.conf {hint-url}')
config.bind('.dywn', 'spawn --output-messages yt-dlp --config-location ~/.config/yt-dlp/audio-webms.conf {url}')
config.bind('.dyvh', 'hint links spawn --output-messages yt-dlp --config-location ~/.config/yt-dlp/video.conf {hint-url}')
config.bind('.dyvn', 'spawn --output-messages yt-dlp --config-location ~/.config/yt-dlp/video.conf {url}')
config.bind('.dscn', 'spawn --output-messages yt-dlp --config-location ~/.config/yt-dlp/audio-soundcloud.conf {url}')
config.bind('.dsch', 'hint links spawn --output-messages yt-dlp --config-location ~/.config/yt-dlp/audio-soundcloud.conf {hint-url}')

# open 4chan webms/pics in mpv
config.bind('.wh', 'hint links spawn --detach wsg2mpv {hint-url}')
config.bind('.wn', 'spawn --detach wsg2mpv {url}')
config.bind('.dwsg', 'spawn --detach --output-messages wsg2ytdlp-qute {url}')

# Custom dot commands
config.unbind('.')
config.bind('.zc', 'spawn --output-messages qt-cookies')
config.bind('.bb', 'config-cycle statusbar.show always in-mode')
config.bind('.tt', 'config-cycle tabs.show always switching')
config.bind('.xx', 'config-cycle statusbar.show always in-mode;; config-cycle tabs.show always switching')
config.bind('.ce', 'spawn st -e nvim /home/anon/.config/qutebrowser/config.py')
config.bind('.zz', 'zoom 133')
config.bind('.ab', 'adblock-update')

# Binds for opening websites
config.bind('chg', 'open -t https://4chan.org/g')
config.bind('wsg', 'open -t https://4chan.org/wsg')
config.bind('.iv', 'open -t https://yewtu.be')
config.bind('.yi', 'open -t https://yandex.com/images')

# Rebinding defaults for qutebrowser
config.bind('dw', 'hint links download')
config.bind('scp', 'config-source')
config.bind('.zh', 'history-clear')
config.bind('k', 'scroll-page 0 -0.5')
config.bind('j', 'scroll-page 0 0.5')
config.bind('U', 'undo')
config.bind('J', 'scroll left')
config.bind('d', 'scroll down')
config.bind('u', 'scroll up')
config.bind('K', 'scroll right')
config.bind('x', 'tab-close')
config.bind('l', 'tab-next')
config.bind('h', 'tab-prev')
config.bind('t', 'set-cmd-text -s :open -t')
config.bind('e', 'hint all tab')
config.bind('m', 'tab-move')
config.bind('F', 'set-cmd-text -sr :tab-focus')
config.unbind('q')
config.unbind('gm')

# Bind chanined commands
def bind_chained(key, *commands):
    config.bind(key, ' ;; '.join(commands))
bind_chained('.cc', 'history-clear', 'spawn --output-messages qt-cookies')

# Set editor
c.editor.command = ["st", "-e", "nvim", "'{}'"]

# Search engines which can be used via the address bar.  Maps a search
# engine name (such as `DEFAULT`, or `ddg`) to a URL with a `{}`
# placeholder. The placeholder will be replaced by the search term, use
# `{{` and `}}` for literal `{`/`}` braces.  The following further
# placeholds are defined to configure how special characters in the
# search terms are replaced by safe characters (called 'quoting'):  *
# `{}` and `{semiquoted}` quote everything except slashes; this is the
# most   sensible choice for almost all search engines (for the search
# term   `slash/and&amp` this placeholder expands to `slash/and%26amp`).
# * `{quoted}` quotes all characters (for `slash/and&amp` this
# placeholder   expands to `slash%2Fand%26amp`). * `{unquoted}` quotes
# nothing (for `slash/and&amp` this placeholder   expands to
# `slash/and&amp`).  The search engine named `DEFAULT` is used when
# `url.auto_search` is turned on and something else than a URL was
# entered to be opened. Other search engines can be used by prepending
# the search engine name to the search term, e.g. `:open google
# qutebrowser`.
# Type: Dict
c.url.searchengines = {'DEFAULT': 'https://anon.sx/search?q={}', 'ddg': 'https://duckduckgo.com/?q={}', 'am': 'https://www.amazon.com/s?k={}', 'ams': 'https://www.smile.amazon.com/s?k={}', 'aw': 'https://wiki.archlinux.org/?search={}', 'goog': 'https://www.google.com/search?q={}', 'hoog': 'https://hoogle.haskell.org/?hoogle={}', 'yt': 'https://www.youtube.com/results?search_query={}', 'tk': 'https://searx.tiekoetter.com/search?q={}', 'iv': 'https://yewtu.be/search?q={}', 'ny': 'https://nyaa.si/?q={}', 'yi': 'https://yandex.com/images/search?text={}'}

# Directory to save downloads to. If unset, a sensible OS-specific
# default is used.
# Type: Directory
c.downloads.location.directory = '~/'
c.downloads.remove_finished = 1000

# Zoom for chan
#config.set('zoom', '125', 'https://4channel.org/*')

# Disable autoplay
config.set('content.autoplay', False)

# When to show the tab bar.
# Type: String
# Valid values:
#   - always: Always show the tab bar.
#   - never: Always hide the tab bar.
#   - multiple: Hide the tab bar if only one tab is open.
#   - switching: Show the tab bar when switching tabs.
c.tabs.show = 'switching'
c.tabs.show_switching_delay = 2500

# When to show the status bar.
# Type: String
# Valid values:
#   - always: Always show the statusbar.
#   - never: Always hide the statusbar.
#   - in-mode: Bar shows when not in normal mode.
c.statusbar.show = 'in-mode'

# Setting default page for when opening new tabs or new windows with
# commands like :open -t and :open -w .
c.url.start_pages = 'https://anon.sx/'

# Setting default page for when opening new tabs or new windows with
# commands like :open -t and :open -w .
c.url.default_page = 'https://anon.sx/'

# Adblocking lists
c.content.blocking.adblock.lists = [
            "https://easylist.to/easylist/easylist.txt",
            "https://easylist.to/easylist/easyprivacy.txt",
            "https://secure.fanboy.co.nz/fanboy-cookiemonster.txt",
            "https://easylist.to/easylist/fanboy-social.txt",
            "https://secure.fanboy.co.nz/fanboy-annoyance.txt",
            "https://raw.githubusercontent.com/notracking/hosts-blocklists/master/adblock/adblock.txt",
            ]
# Set default zoom
c.zoom.default = '150%'

# Set spellcheck language
c.spellcheck.languages = ['en-US']

# Do not save cookies
c.content.cookies.store = False

# Do not save histroy
c.completion.web_history.max_items = 0

# Disable Javascript
config.bind('.tsh', 'config-cycle -p -t -u *://{url:host}/* content.javascript.enabled ;; reload')
config.bind('.tSh', 'config-cycle -p -u *://{url:host}/* content.javascript.enabled ;; reload')
config.bind('.tsH', 'config-cycle -p -t -u *://*.{url:host}/* content.javascript.enabled ;; reload')
config.bind('.tSH', 'config-cycle -p -u *://*.{url:host}/* content.javascript.enabled ;; reload')
config.bind('.tsu', 'config-cycle -p -t -u {url} content.javascript.enabled ;; reload')
config.bind('.tSu', 'config-cycle -p -u {url} content.javascript.enabled ;; reload')
c.content.javascript.enabled = False

# Autogenerated config.py
#
# NOTE: config.py is intended for advanced users who are comfortable
# with manually migrating the config file on qutebrowser upgrades. If
# you prefer, you can also configure qutebrowser using the
# :set/:bind/:config-* commands without having to write a config.py
# file.
#
# Documentation:
#   qute://help/configuring.html
#   qute://help/settings.html

# Change the argument to True to still load settings configured via autoconfig.yml
config.load_autoconfig(False)

# Which cookies to accept. With QtWebEngine, this setting also controls
# other features with tracking capabilities similar to those of cookies;
# including IndexedDB, DOM storage, filesystem API, service workers, and
# AppCache. Note that with QtWebKit, only `all` and `never` are
# supported as per-domain values. Setting `no-3rdparty` or `no-
# unknown-3rdparty` per-domain on QtWebKit will have the same effect as
# `all`. If this setting is used with URL patterns, the pattern gets
# applied to the origin/first party URL of the page making the request,
# not the request URL. With QtWebEngine 5.15.0+, paths will be stripped
# from URLs, so URL patterns using paths will not match. With
# QtWebEngine 5.15.2+, subdomains are additionally stripped as well, so
# you will typically need to set this setting for `example.com` when the
# cookie is set on `somesubdomain.example.com` for it to work properly.
# To debug issues with this setting, start qutebrowser with `--debug
# --logfilter network --debug-flag log-cookies` which will show all
# cookies being set.
# Type: String
# Valid values:
#   - all: Accept all cookies.
#   - no-3rdparty: Accept cookies from the same origin only. This is known to break some sites, such as GMail.
#   - no-unknown-3rdparty: Accept cookies from the same origin only, unless a cookie is already set for the domain. On QtWebEngine, this is the same as no-3rdparty.
#   - never: Don't accept cookies at all.
config.set('content.cookies.accept', 'all', 'devtools://*')

# Value to send in the `Accept-Language` header. Note that the value
# read from JavaScript is always the global value.
# Type: String
config.set('content.headers.accept_language', '', 'https://matchmaker.krunker.io/*')

# User agent to send.  The following placeholders are defined:  *
# `{os_info}`: Something like "X11; Linux x86_64". * `{webkit_version}`:
# The underlying WebKit version (set to a fixed value   with
# QtWebEngine). * `{qt_key}`: "Qt" for QtWebKit, "QtWebEngine" for
# QtWebEngine. * `{qt_version}`: The underlying Qt version. *
# `{upstream_browser_key}`: "Version" for QtWebKit, "Chrome" for
# QtWebEngine. * `{upstream_browser_version}`: The corresponding
# Safari/Chrome version. * `{qutebrowser_version}`: The currently
# running qutebrowser version.  The default value is equal to the
# unchanged user agent of QtWebKit/QtWebEngine.  Note that the value
# read from JavaScript is always the global value. With QtWebEngine
# between 5.12 and 5.14 (inclusive), changing the value exposed to
# JavaScript requires a restart.
# Type: FormatString
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/{webkit_version} (KHTML, like Gecko) {upstream_browser_key}/{upstream_browser_version} Safari/{webkit_version}', 'https://web.whatsapp.com/')

# User agent to send.  The following placeholders are defined:  *
# `{os_info}`: Something like "X11; Linux x86_64". * `{webkit_version}`:
# The underlying WebKit version (set to a fixed value   with
# QtWebEngine). * `{qt_key}`: "Qt" for QtWebKit, "QtWebEngine" for
# QtWebEngine. * `{qt_version}`: The underlying Qt version. *
# `{upstream_browser_key}`: "Version" for QtWebKit, "Chrome" for
# QtWebEngine. * `{upstream_browser_version}`: The corresponding
# Safari/Chrome version. * `{qutebrowser_version}`: The currently
# running qutebrowser version.  The default value is equal to the
# unchanged user agent of QtWebKit/QtWebEngine.  Note that the value
# read from JavaScript is always the global value. With QtWebEngine
# between 5.12 and 5.14 (inclusive), changing the value exposed to
# JavaScript requires a restart.
# Type: FormatString
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}; rv:90.0) Gecko/20100101 Firefox/90.0', 'https://accounts.google.com/*')

# User agent to send.  The following placeholders are defined:  *
# `{os_info}`: Something like "X11; Linux x86_64". * `{webkit_version}`:
# The underlying WebKit version (set to a fixed value   with
# QtWebEngine). * `{qt_key}`: "Qt" for QtWebKit, "QtWebEngine" for
# QtWebEngine. * `{qt_version}`: The underlying Qt version. *
# `{upstream_browser_key}`: "Version" for QtWebKit, "Chrome" for
# QtWebEngine. * `{upstream_browser_version}`: The corresponding
# Safari/Chrome version. * `{qutebrowser_version}`: The currently
# running qutebrowser version.  The default value is equal to the
# unchanged user agent of QtWebKit/QtWebEngine.  Note that the value
# read from JavaScript is always the global value. With QtWebEngine
# between 5.12 and 5.14 (inclusive), changing the value exposed to
# JavaScript requires a restart.
# Type: FormatString
config.set('content.headers.user_agent', 'Mozilla/5.0 ({os_info}) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/99 Safari/537.36', 'https://*.slack.com/*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'chrome-devtools://*')

# Load images automatically in web pages.
# Type: Bool
config.set('content.images', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome-devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'devtools://*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'chrome://*/*')

# Enable JavaScript.
# Type: Bool
config.set('content.javascript.enabled', True, 'qute://*/*')

# Text color of the completion widget. May be a single color to use for
# all columns or a list of three colors, one for each column.
# Type: List of QtColor, or QtColor
c.colors.completion.fg = ['#9cc4ff', 'white', 'white']

# Background color of the completion widget for odd rows.
# Type: QssColor
c.colors.completion.odd.bg = '#1c1f24'

# Background color of the completion widget for even rows.
# Type: QssColor
c.colors.completion.even.bg = '#232429'

# Foreground color of completion widget category headers.
# Type: QtColor
c.colors.completion.category.fg = '#e1acff'

# Background color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.bg = 'qlineargradient(x1:0, y1:0, x2:0, y2:1, stop:0 #000000, stop:1 #232429)'

# Top border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.border.top = '#3f4147'

# Bottom border color of the completion widget category headers.
# Type: QssColor
c.colors.completion.category.border.bottom = '#3f4147'

# Foreground color of the selected completion item.
# Type: QtColor
c.colors.completion.item.selected.fg = '#0c0d0e'

# Background color of the selected completion item.
# Type: QssColor
c.colors.completion.item.selected.bg = '#ecbe7b'

# Foreground color of the matched text in the selected completion item.
# Type: QtColor
c.colors.completion.item.selected.match.fg = '#c678dd'

# Foreground color of the matched text in the completion.
# Type: QtColor
c.colors.completion.match.fg = '#c678dd'

# Color of the scrollbar handle in the completion view.
# Type: QssColor
c.colors.completion.scrollbar.fg = 'white'

# Background color for the download bar.
# Type: QssColor
c.colors.downloads.bar.bg = '#0c0d0e'

# Background color for downloads with errors.
# Type: QtColor
c.colors.downloads.error.bg = '#ff6c6b'

# Font color for hints.
# Type: QssColor
c.colors.hints.bg = '#0c0d0e'

# Font color for hints.
# Type: QssColor
c.colors.hints.fg = '#fcfdfe'

# Font color for the matched part of hints.
# Type: QtColor
c.colors.hints.match.fg = '#ff6c6b'

# Font color for border of hints
# Type: QtColor
c.hints.border = '1px solid #fcfdfe'

# Background color of an info message.
# Type: QssColor
c.colors.messages.info.bg = '#0c0d0e'

# Background color of the statusbar.
# Type: QssColor
c.colors.statusbar.normal.bg = '#0c0d0e'

# Foreground color of the statusbar in insert mode.
# Type: QssColor
c.colors.statusbar.insert.fg = 'white'

# Background color of the statusbar in insert mode.
# Type: QssColor
c.colors.statusbar.insert.bg = '#01a252'

# Background color of the statusbar in passthrough mode.
# Type: QssColor
c.colors.statusbar.passthrough.bg = '#01a0e4'

# Background color of the statusbar in command mode.
# Type: QssColor
c.colors.statusbar.command.bg = '#0c0d0e'

# Foreground color of the URL in the statusbar when there's a warning.
# Type: QssColor
c.colors.statusbar.url.warn.fg = 'yellow'

# Background color of the tab bar.
# Type: QssColor
c.colors.tabs.bar.bg = '#0c0d0e'

# Foreground color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.fg = '#fcfdfe'

# Background color of unselected odd tabs.
# Type: QtColor
c.colors.tabs.odd.bg = '#0c0d0e'

# Foreground color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.fg = '#fcfdfe'

# Background color of unselected even tabs.
# Type: QtColor
c.colors.tabs.even.bg = '#0c0d0e'

# Foreground color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.fg = '#FF3D00'

# Background color of selected odd tabs.
# Type: QtColor
c.colors.tabs.selected.odd.bg = '#0c0d0e'

# Foreground color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.fg = '#FF3D00'

# Background color of selected even tabs.
# Type: QtColor
c.colors.tabs.selected.even.bg = '#0c0d0e'

# Background color of pinned unselected odd tabs.
# Type: QtColor
c.colors.tabs.pinned.odd.bg = 'seagreen'

# Background color of pinned unselected even tabs.
# Type: QtColor
c.colors.tabs.pinned.even.bg = 'darkseagreen'

# Background color of pinned selected odd tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.odd.bg = '#0c0d0e'

# Background color of pinned selected even tabs.
# Type: QtColor
c.colors.tabs.pinned.selected.even.bg = '#0c0d0e'

# Default font families to use. Whenever "default_family" is used in a
# font setting, it's replaced with the fonts listed here. If set to an
# empty value, a system-specific monospace default is used.
# Type: List of Font, or Font
c.fonts.default_family = '"FantasqueSansMono Nerd Font Mono"'

# Default font size to use. Whenever "default_size" is used in a font
# setting, it's replaced with the size listed here. Valid values are
# either a float value with a "pt" suffix, or an integer value with a
# "px" suffix.
# Type: String
c.fonts.default_size = '11pt'

# Font used in the completion widget.
# Type: Font
c.fonts.completion.entry = '11pt "FantasqueSansMono Nerd Font Mono"'

# Font used for the debugging console.
# Type: Font
c.fonts.debug_console = '11pt "FantasqueSansMono Nerd Font Mono"'

# Font used for prompts.
# Type: Font
c.fonts.prompts = 'default_size sans-serif'

# Font used in the statusbar.
# Type: Font
c.fonts.statusbar = '11pt "FantasqueSansMono Nerd Font Mono"'

# Font used for hints
# Type: Font
c.fonts.hints = '14 "FantasqueSansMono Nerd Font Mono"'

# Padding around text for tabs
c.tabs.padding = ({"bottom": 3, "left": 5, "right": 5, "top": 3})
