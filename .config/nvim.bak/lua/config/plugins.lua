-------------------------------------------------
-- PLUGINS
-------------------------------------------------

local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
    -- Packer can manage itself
    use 'wbthomason/packer.nvim'

    -- Dashboard is a nice start screen for nvim
    use {
        'glepnir/dashboard-nvim',
        event = 'VimEnter',
        config = function()
            require'config/dashboard'
        end,
        requires = {'nvim-tree/nvim-web-devicons'}
    }

    -- Telescope and related plugins --
    use {
        'nvim-telescope/telescope.nvim', tag = '0.1.1',
        requires = { {'nvim-lua/plenary.nvim'} }
    }
    use {
        "nvim-telescope/telescope-file-browser.nvim",
        requires = { "nvim-telescope/telescope.nvim", "nvim-lua/plenary.nvim" },
        config = function()
            require'telescope'.setup {
                extensions = {
                    file_browser = {
                        theme = "ivy",
                        -- disables netrw and use telescope-file-browser in its place
                        hijack_netrw = true,
                        hidden = false,
                        respect_gitignore = false,
                        use_fd = true,
                        mappings = {
                            ["i"] = {
                                -- your custom insert mode mappings
                            },
                            ["n"] = {
                                -- your custom normal mode mappings
                            },
                        },
                    },
                },
            }
        end
    }
    -- To get telescope-file-browser loaded and working with telescope,
    -- you need to call load_extension, somewhere after setup function:
    require'telescope'.load_extension "file_browser"

    -- Treesitter --
    use {
        'nvim-treesitter/nvim-treesitter',
        config = function()
            require'nvim-treesitter.configs'.setup {
            -- If TS highlights are not enabled at all, or disabled via `disable` prop,
            -- highlighting will fallback to default Vim syntax highlighting
                highlight = {
                    enable = true,
                    additional_vim_regex_highlighting = {'org'}, -- Required for spellcheck, some LaTex highlights and code block highlights that do not have ts grammar
                },
                ensure_installed = {'org'}, -- Or run :TSUpdate org
            }
        end
    }

    -- Productivity --
    use 'vimwiki/vimwiki'
    use 'jreybert/vimagit'
    use {
        'nvim-orgmode/orgmode',
        config = function()
            require'orgmode'.setup {
                org_agenda_files = {'~/nc/Org/agenda.org'},
                org_default_notes_file = '~/nc/Org/notes.org',
            }
        end
    }
    require('orgmode').setup_ts_grammar()

    -- Which key
    use {
        "folke/which-key.nvim",
        config = function()
            require'which-key'.setup {
                -- your configuration comes here
                -- or leave it empty to use the default settings
            }
        end
    }

    -- A better status line --
    use {
        'nvim-lualine/lualine.nvim',
        requires = { 'kyazdani42/nvim-web-devicons', opt = true }
    }
    require('lualine').setup()

    -- File management --
    use 'vifm/vifm.vim'
    use 'scrooloose/nerdtree'
    use 'tiagofumo/vim-nerdtree-syntax-highlight'
    use 'ryanoasis/vim-devicons'

    -- Tim Pope Plugins --
    -- use 'tpope/vim-surround'
    use {
        "kylechui/nvim-surround",
        tag = "*", -- Use for stability; omit to use `main` branch for the latest features
        config = function()
            require'nvim-surround'.setup {
                -- Configuration here, or leave empty to use defaults
            }
        end
    }

    -- Syntax Highlighting and Colors --
    use 'PotatoesMaster/i3-vim-syntax'
    use 'kovetskiy/sxhkd-vim'
    use 'vim-python/python-syntax'
    use 'ap/vim-css-color'

    -- Junegunn Choi Plugins --
    use 'junegunn/goyo.vim'
    use 'junegunn/limelight.vim'
    use 'junegunn/vim-emoji'

    -- Colorschemes --
    use 'RRethy/nvim-base16'
    use 'kyazdani42/nvim-palenight.lua'

    -- Other stuff --
    use 'frazrepo/vim-rainbow'

    -- Comment Plugin --
    use {
        'numToStr/Comment.nvim',
        config = function()
            require'Comment'.setup()
        end
    }

    -- Snippets Plugins --
    use 'saadparwaiz1/cmp_luasnip' -- Snippets source for nvim-cmp
    use 'L3MON4D3/LuaSnip' -- Snippets plugin

    -- LSP Plugins --
    use 'neovim/nvim-lspconfig' -- Collection of configurations for built-in LSP client
    use 'hrsh7th/cmp-nvim-lsp' -- LSP source for nvim-cmp

    -- Autocompletion Plugin --
    use {
        'hrsh7th/nvim-cmp',  -- Autocompletion plugin
        requires = {
            'hrsh7th/cmp-path',
            'hrsh7th/cmp-cmdline',
            'hrsh7th/cmp-buffer',
        },
        config = function ()
            require'config/cmp'
        end
    }

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
        require('packer').sync()
    end
end)
