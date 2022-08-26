local function map(m, k, v)
    vim.keymap.set(m, k, v, o)
end

local d = { silent = true, noremap = true }

-- Fix n and N to keep cursor in center
map('n', 'n', 'nzz', d)
map('n', 'N', 'Nzz', d)

-- leader-o/O inserts blank line below/above
map('n', '<leader>o', 'o<ESC>', d)
map('n', '<leader>O', 'O<ESC>', d)

-- Source init.lua
map('n', '<leader><leader>r', ':source $XDG_CONFIG_HOME/nvim/init.lua<CR>', d)

-- Set c to behave a bit better
map('n', 'c', '"_c', d)

-- Perform dot commands over visual blocks
map('v', '.', ':normal .<CR>', d)

-- Goyo makes text more readable when writing prose
map('n', '<leader>f', ':Goyo | set linebreak<CR>', d)

-- Toggle spell-check
map('n', '<leader>to', ':setlocal spell! spelling=en_us<CR>', d)

-- Toggle Nerd tree
map('n', '<leader>tn', ':NERDTreeToggle<CR>', d)

-- Better navigation
map('n', '<C-H>', '<C-W>h', d)
map('n', '<C-J>', '<C-W>j', d)
map('n', '<C-K>', '<C-W>k', d)
map('n', '<C-L>', '<C-W>l', d)
map('n', '<C-N>', '<C-W>n', d)

-- Replace ex mode with gq
map('n', 'Q', 'gq', d)

-- Toggle shellcheck
map('n', '<leader>ts', ':!clear && shellcheck -x %<CR>', d)

-- Set S to replace all
map('n', 'S', ':%s//g<Left><Left>', { noremap = true })

-- Easy unfolding
map('n', ';', 'zA', d)

-- Set binds to toggle and resize terminal split
map('n', '<leader>tt', ':split | resize 15 | terminal<CR>i', d)
map('n', '<leader><leader>o', ':resize 15<CR>', d)

-- Fix indenting visual block
map('v', '>', '>gv', d)
map('v', '<', '<gv', d)

-- Easymotion mapping, may not work
map('n', 's', '<Plug>(easymotion-overwin-f2)', d)

-- Enable moving up and down over visual lines
vim.keymap.set('n', 'j', function()
    return vim.v.count == 1 and 'j' or 'gj' end,
    { expr = true })
vim.keymap.set('n', 'k', function()
    return vim.v.count == 1 and 'k' or 'gk' end,
    { expr = true })

-- Jump forward or backward in snippets
vim.keymap.set('i', '<Tab>', function()
    return vim.fn.call('vsnip#jumpable', {1}) == 1
        and '<Plug>(vsnip-jump-next)' or '<Tab>' end,
    { expr = true })
vim.keymap.set('s', '<Tab>', function()
    return vim.fn.call('vsnip#jumpable', {1}) == 1
        and '<Plug>(vsnip-jump-next)' or '<Tab>' end,
    { expr = true })
vim.keymap.set('i', '<S-Tab>', function()
    return vim.fn.call('vsnip#jumpable', {-1}) == 1
        and '<Plug>(vsnip-jump-prev)' or '<S-Tab>' end,
    { expr = true })
vim.keymap.set('s', '<S-Tab>', function()
    return vim.fn.call('vsnip#jumpable', {-1}) == 1
        and '<Plug>(vsnip-jump-prev)' or '<S-Tab>' end,
    { expr = true })
