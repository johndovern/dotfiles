local a = vim.api.nvim_create_autocmd
local function ac(e, p, c)
    a(e, {
        pattern = p,
        command = c
    })
end

-- Center screen on input
ac('InsertEnter', '*', 'norm zz')

-- Disable automatic commenting on newline
ac('FileType', '*', [[
setlocal formatoptions-=c formatoptions-=r formatoptions-=o
]])

-- Read X11 files appropriately
ac({ 'BufRead', 'BufNewFile' },
    { 'Xresources', 'Xdefaults', 'xresources', 'xdefaults'},
    'set filetype=xdefaults')

-- Run xrdb whenever Xdefaults or Xresources are updated.
ac('BufWritePost',
    { 'Xresources', 'Xdefaults', 'xresources', 'xdefaults'},
    '!xrdb %')

-- Restart sxhkd after saving
ac('BufWritePost', 'sxhkdrc', '!kill -SIGUSR1 $(pidof sxhkd)')

-- NERDTree bufautocmd
ac('bufenter', '*', 'if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif')

-- When shortcut files are updated, renew bash and ranger configs with new material
ac('BufWritePost', { 'bm-files', 'bm-dirs' }, '!shortcuts')
ac('BufWritePre', '*', 'let currPos = getpos(".")')
ac('BufWritePre', '*', [[%s/\s\+$//e]])
ac('BufWritePre', '*', [[%s/\n\+\%$//e]])
ac('BufWritePre', '*.[ch]', [[%s/\%$/\r/e]])
ac('BufWritePre', '*', [[cal cursor(currPos[1], currPos[2])]])

-- Enable Goyo by default for mutt writing
ac({ 'BufRead', 'BufNewFile' }, '/tmp/neomutt*', [[let g:goyo_width=80]])
ac({ 'BufRead', 'BufNewFile' }, '/tmp/neomutt*', [[:Goyo | set bg=light]])
ac({ 'BufRead', 'BufNewFile' }, '/tmp/neomutt*', [[map ZZ :Goyo\|x!<CR>]])
ac({ 'BufRead', 'BufNewFile' }, '/tmp/neomutt*', [[map ZQ :Goyo\|q!<CR>]])

-- Enable lisp filetype on *.kbd files
ac({'BufRead', 'BufNewFile'}, '*.kbd', 'set filetype=lisp')
