-------------------------------------------------
-- DASHBOARD
-------------------------------------------------

require'dashboard'.setup {
    default_banner = {
        '',
        '',
        ' ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗',
        ' ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║',
        ' ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║',
        ' ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║',
        ' ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║',
        ' ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝',
        '',
        ' [ TIP: To exit Neovim, just power off your computer. ] ',
        '',
    },
    -- linux
    --preview_command = 'ueberzug'
    --
    --preview_file_path = home .. '/.config/nvim/static/neovim.cat'
    preview_file_height = 11,
    preview_file_width = 70,
    custom_center = {
        {
            icon = '  ',
            desc = 'Recent sessions                         ',
            shortcut = ', s l',
            action ='SessionLoad'
        },
        {
            icon = '  ',
            desc = 'Find recent files                       ',
            action = 'Telescope oldfiles',
            shortcut = ', f r'
        },
        {
            icon = '  ',
            desc = 'Find files                              ',
            action = 'Telescope find_files find_command=rg,--hidden,--files',
            shortcut = ', f f'
        },
        {
            icon = '  ',
            desc ='File browser                            ',
            action =  'Telescope file_browser',
            shortcut = ', f b'
        },
        {
            icon = '  ',
            desc = 'Find word                               ',
            action = 'Telescope live_grep',
            shortcut = ', f w'
        },
        {
            icon = '  ',
            desc = 'Load new theme                          ',
            action = 'Telescope colorscheme',
            shortcut = ', t h'
        },
    },
    custom_footer = { '', '🎉 If I\'m using Neovim, then my Emacs config must be broken!' },
    session_directory = "/home/anon/.config/nvim/session",
}
