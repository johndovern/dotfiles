let mapleader =","

if ! filereadable(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim"'))
  echo "Downloading junegunn/vim-plug to manage plugins..."
  silent !mkdir -p ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/
  silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ${XDG_CONFIG_HOME:-$HOME/.config}/nvim/autoload/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin(system('echo -n "${XDG_CONFIG_HOME:-$HOME/.config}/nvim/plugged"'))
  Plug 'tpope/vim-surround'
  Plug 'preservim/nerdtree'
  Plug 'junegunn/goyo.vim'
  Plug 'jreybert/vimagit'
  Plug 'lukesmithxyz/vimling'
  Plug 'vimwiki/vimwiki'
  Plug 'vim-airline/vim-airline'
  " Plug 'tpope/vim-commentary'
  Plug 'numToStr/Comment.nvim'
  Plug 'ap/vim-css-color'
  Plug 'jiangmiao/auto-pairs'
  Plug 'easymotion/vim-easymotion'
  Plug 'tpope/vim-repeat'
  Plug 'tridactyl/vim-tridactyl'
  Plug 'morhetz/gruvbox'

  " Lsp, treesitter, and lsp installer
  Plug 'neovim/nvim-lspconfig'
  Plug 'nvim-treesitter/nvim-treesitter'
  Plug 'williamboman/nvim-lsp-installer'

  " Auto completion
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/cmp-cmdline'
  Plug 'hrsh7th/nvim-cmp'

  " For vsnip users.
  Plug 'hrsh7th/cmp-vsnip'
  Plug 'hrsh7th/vim-vsnip'
  Plug 'rafamadriz/friendly-snippets'

call plug#end()

lua require('config')

set title
set go=a
set mouse=a
set nohlsearch
set clipboard+=unnamedplus
set noshowmode
set noruler
set laststatus=0
set noshowcmd
set tabstop=2
set shiftwidth=2
set expandtab
set exrc
set secure
set background=dark
nnoremap <leader><leader>r :source $XDG_CONFIG_HOME/nvim/init.vim<CR>

" Expand
  " imap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'
  " smap <expr> <C-j>   vsnip#expandable()  ? '<Plug>(vsnip-expand)'         : '<C-j>'

" Expand or jump
  " imap <expr> <C-e>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-e>'
  " smap <expr> <C-e>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-e>'

" Jump forward or backward
  imap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
  smap <expr> <Tab>   vsnip#jumpable(1)   ? '<Plug>(vsnip-jump-next)'      : '<Tab>'
  imap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'
  smap <expr> <S-Tab> vsnip#jumpable(-1)  ? '<Plug>(vsnip-jump-prev)'      : '<S-Tab>'

" Set snippet dir to ~/.config
  let g:vsnip_snippet_dir = get(g:, 'vsnip_snippet_dir', expand('~/.config/vsnip'))

" Some basics:
  nnoremap c "_c
  set nocompatible
  filetype plugin on
  syntax on
  set encoding=utf-8
  set number relativenumber
" Enable autocompletion:
  set wildmode=longest,list,full
" Disables automatic commenting on newline:
  autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o
" Perform dot commands over visual blocks:
  vnoremap . :normal .<CR>
" Goyo plugin makes text more readable when writing prose:
  map <leader>f :Goyo \| set linebreak<CR>
" Spell-check set to <leader>o, 'o' for 'orthography':
  map <leader>o :setlocal spell! spelllang=en_us<CR>
" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
  set splitbelow splitright

" Nerd tree
  map <leader>n :NERDTreeToggle<CR>
  autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif
    if has('nvim')
        let NERDTreeBookmarksFile = stdpath('data') . '/NERDTreeBookmarks'
    else
        let NERDTreeBookmarksFile = '~/.vim' . '/NERDTreeBookmarks'
    endif

" vimling:
  nm <leader><leader>d :call ToggleDeadKeys()<CR>
  imap <leader><leader>d <esc>:call ToggleDeadKeys()<CR>a
  nm <leader><leader>i :call ToggleIPA()<CR>
  imap <leader><leader>i <esc>:call ToggleIPA()<CR>a
  nm <leader><leader>q :call ToggleProse()<CR>

" Shortcutting split navigation, saving a keypress:
  map <C-h> <C-w>h
  map <C-j> <C-w>j
  map <C-k> <C-w>k
  map <C-l> <C-w>l
  map <C-n> <C-w>=

" Better movement
  noremap <expr> j v:count ? 'j' : 'gj'
  noremap <expr> k v:count ? 'k' : 'gk'

" Better highlighting
  hi Visual ctermfg=Grey ctermbg=Black

" Replace ex mode with gq
  map Q gq

" Check file in shellcheck:
  map <leader>s :!clear && shellcheck -x %<CR>

" Open my bibliography file in split
  map <leader>b :vsp<space>$BIB<CR>
  map <leader>r :vsp<space>$REFER<CR>

" Replace all is aliased to S.
  nnoremap S :%s//g<Left><Left>

" Compile document, be it groff/LaTeX/markdown/etc.
  map <leader>c :w! \| !compiler "<c-r>%"<CR>

" Open corresponding .pdf/.html or preview
  map <leader>p :!opout <c-r>%<CR><CR>

" Runs a script that cleans out tex build files whenever I close out of a .tex file.
  autocmd VimLeave *.tex !texclear %

" Ensure files are read as what I want:
  let g:vimwiki_ext2syntax = {'.Rmd': 'markdown', '.rmd': 'markdown','.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
  map <leader>v :VimwikiIndex<CR>
  let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]
  " map <leader>v :VimwikiIndex
  " let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]
  autocmd BufRead,BufNewFile /tmp/calcurse*,~/.calcurse/notes/* set filetype=markdown
  autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
  autocmd BufRead,BufNewFile *.tex set filetype=tex
  autocmd BufRead,BufNewFile *.kbd set filetype=lisp

" Add folding to bash scripts
  autocmd FileType sh set foldmethod=indent
  autocmd FileType sh nnoremap ; zA

" Save file as sudo on files that require root permission
  cnoremap w!! execute 'silent! write !sudo tee % >/dev/null' <bar> edit!

" Enable Goyo by default for mutt writing
  autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width=80
  autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo | set bg=light
  autocmd BufRead,BufNewFile /tmp/neomutt* map ZZ :Goyo\|x!<CR>
  autocmd BufRead,BufNewFile /tmp/neomutt* map ZQ :Goyo\|q!<CR>

" Automatically deletes all trailing whitespace and newlines at end of file on save. & reset cursor position
  autocmd BufWritePre * let currPos = getpos(".")
  autocmd BufWritePre * %s/\s\+$//e
  autocmd BufWritePre * %s/\n\+\%$//e
  autocmd BufWritePre *.[ch] %s/\%$/\r/e
  autocmd BufWritePre * cal cursor(currPos[1], currPos[2])

" When shortcut files are updated, renew bash and ranger configs with new material:
  autocmd BufWritePost bm-files,bm-dirs !shortcuts
" Run xrdb whenever Xdefaults or Xresources are updated.
  autocmd BufRead,BufNewFile Xresources,Xdefaults,xresources,xdefaults set filetype=xdefaults
  autocmd BufWritePost Xresources,Xdefaults,xresources,xdefaults !xrdb %
" Restart sxhkd after saving
" Weird bug need a second line with a comment
  autocmd BufWritePost sxhkdrc !kill -SIGUSR1 "$(pidof sxhkd)"

" Autocenter screen when entering insert mode
  autocmd InsertEnter * norm zz

" Fix indenting visual block
  vmap > >gv
  vmap < <gv

" s{char}{char} to move to {char}{char}
  nmap s <Plug>(easymotion-overwin-f)

" Turns off highlighting on the bits of code that are changed, so the line that is changed is highlighted but the actual text that has changed stands out on the line and is readable.
if &diff
    highlight! link DiffText MatchParen
endif

" Function for toggling the bottom statusbar:
let s:hidden_all = 0
function! ToggleHiddenAll()
    if s:hidden_all  == 0
        let s:hidden_all = 1
        set noshowmode
        set noruler
        set laststatus=0
        set noshowcmd
    else
        let s:hidden_all = 0
        set showmode
        set ruler
        set laststatus=2
        set showcmd
    endif
endfunction
nnoremap <leader>h :call ToggleHiddenAll()<CR>
