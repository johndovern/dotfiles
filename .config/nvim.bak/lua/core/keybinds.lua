--local function map(m, k, v)
--   vim.keymap.set(m, k, v, { silent = true })
--end

local function map(m, k, v, o)
    -- vim.keymap.set(m, k, v, o)
    vim.api.nvim_set_keymap(m, k, v, o)
end

local d = { silent = true, noremap = true }
local opt = { noremap = true }

-- Mimic shell movements
map('i', '<C-E>', '<ESC>A', d)
map('i', '<C-A>', '<ESC>I', d)

-- Load recent sessions
map('n', '<leader>sl', ':SessionLoad<CR>', d)

-- Keybindings for telescope
map('n', '<leader>fr', ':Telescope oldfiles<CR>', d)
map('n', '<leader>ff', ':Telescope find_files<CR>', d)
map('n', '<leader>fb', ':Telescope file_browser path=%:p:h select_buffer=true<CR>', d)
map('n', '<leader>fw', ':Telescope live_grep<CR>', d)
map('n', '<leader>dt', ':Telescope colorscheme<CR>', d)

map('n', 'gd', ':lua vim.lsp.buf.definition()<cr>', opt)
map('n', 'gD', ':lua vim.lsp.buf.declaration()<cr>', opt)
map('n', 'gi', ':lua vim.lsp.buf.implementation()<cr>', opt)
map('n', 'gw', ':lua vim.lsp.buf.document_symbol()<cr>', opt)
map('n', 'gw', ':lua vim.lsp.buf.workspace_symbol()<cr>', opt)
map('n', 'gr', ':lua vim.lsp.buf.references()<cr>', opt)
map('n', 'gt', ':lua vim.lsp.buf.type_definition()<cr>', opt)
map('n', 'K', ':lua vim.lsp.buf.hover()<cr>', opt)
map('n', '<c-k>', ':lua vim.lsp.buf.signature_help()<cr>', opt)
map('n', '<leader>af', ':lua vim.lsp.buf.code_action()<cr>', opt)
map('n', '<leader>rn', ':lua vim.lsp.buf.rename()<cr>', opt)

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
--map('n', '<leader>f', ':Goyo | set linebreak<CR>', d)

-- Toggle spell-check
map('n', '<leader>to', ':setlocal spell! spelling=en_us<CR>', d)

-- Toggle Nerd tree
map('n', '<leader>tn', ':NERDTreeToggle<CR>', d)

-- Better navigation
map('n', '<leader>h', '<C-W>h', d)
map('n', '<leader>j', '<C-W>j', d)
map('n', '<leader>k', '<C-W>k', d)
map('n', '<leader>l', '<C-W>l', d)
map('n', '<leader>n', '<C-W>n', d)

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
--map('n', 's', '<Plug>(easymotion-overwin-f2)', d)

-- Enable moving up and down over visual lines
vim.keymap.set('n', 'j', function()
    return vim.v.count == 1 and 'j' or 'gj' end,
    { expr = true })
vim.keymap.set('n', 'k', function()
    return vim.v.count == 1 and 'k' or 'gk' end,
    { expr = true })

