local g = vim.g
local o = vim.o
local cmd = vim.cmd

-- Map <leader> to ,
g.mapleader = ','
g.maplocalleader = ','

-- Set some sane defaults
o.title = true
o.go = 'a'
o.mouse = 'a'
cmd[[set nohlsearch]]
o.clipboard = 'unnamedplus'
g.noshowmode = true
g.noruler = true
o.laststatus = 0
g.noshowcmd = true
o.tabstop = 4
o.softtabstop = 4
o.expandtab = true
o.shiftwidth = 4
o.autoindent = true
cmd[[filetype plugin indent on]]
o.cursorline = true
g.noswapfile = true
o.exrc = true
o.secure = true
o.background = 'dark'
g.nocompatible = true
cmd[[syntax on]]
o.encoding = 'utf-8'
o.number = true
o.relativenumber = true
g.wildmode = { 'longest', 'list', 'full' }
o.splitbelow = true
o.splitright = true
cmd[[hi Visual ctermfg=Grey ctermbg=Black]]
o.foldmethod = 'indent'
o.scrolloff = 8

-- Set snippet dir
g.vsnip_snippet_dir = vim.fn.expand('$XDG_CONFIG_HOME/vsnip')
