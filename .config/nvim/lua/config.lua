local configs = require'nvim-treesitter.configs'
configs.setup {
  ensure_installed = "all",
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
  }
}

-- vim.opt.foldmethod = "expr"
-- vim.opt.foldexpr = "nvim_treesitter#foldexpr()"

local lsp_installer = require("nvim-lsp-installer")
lsp_installer.on_server_ready(function(server)
  local opts = {}
  if server.name == "sumneko_lua" then
    opts = {
      settings = {
        Lua = {
          diagnostics = {
            globals = { 'vim', 'use', 'mp' }
          },
        }
      }
    }
  end
  server:setup(opts)
end)

local keymap = vim.api.nvim_set_keymap
local opts = { noremap = true }

local function nkeymap(key, map)
  keymap('n', key, map, opts)
end

nkeymap('gd', ':lua vim.lsp.buf.definition()<cr>')
nkeymap('gD', ':lua vim.lsp.buf.declaration()<cr>')
nkeymap('gi', ':lua vim.lsp.buf.implementation()<cr>')
nkeymap('gw', ':lua vim.lsp.buf.document_symbol()<cr>')
nkeymap('gw', ':lua vim.lsp.buf.workspace_symbol()<cr>')
nkeymap('gr', ':lua vim.lsp.buf.references()<cr>')
nkeymap('gt', ':lua vim.lsp.buf.type_definition()<cr>')
nkeymap('K', ':lua vim.lsp.buf.hover()<cr>')
nkeymap('<c-k>', ':lua vim.lsp.buf.signature_help()<cr>')
nkeymap('<leader>af', ':lua vim.lsp.buf.code_action()<cr>')
nkeymap('<leader>rn', ':lua vim.lsp.buf.rename()<cr>')

vim.g.colors_name = 'gruvbox'

-- local has_words_before = function()
--   local line, col = unpack(vim.api.nvim_win_get_cursor(0))
--   return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
-- end

-- local feedkey = function(key, mode)
--   vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(key, true, true, true), mode, true)
-- end

-- Setup nvim-cmp.
local cmp = require'cmp'

cmp.setup({
  snippet = {
    -- REQUIRED - you must specify a snippet engine
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
      -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
      -- require('snippy').expand_snippet(args.body) -- For `snippy` users.
      -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    -- documentation = cmp.config.window.bordered(),
  },
  mapping = cmp.mapping.preset.insert({
    ['<C-b>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.abort(),
    ['<CR>'] = cmp.mapping.confirm({ select = true }),
    -- ["<Tab>"] = cmp.mapping(function(fallback)
    --   if cmp.visible() then
    --     cmp.select_next_item()
    --   elseif vim.fn["vsnip#available"](1) == 1 then
    --     feedkey("<Plug>(vsnip-expand-or-jump)", "")
    --   elseif has_words_before() then
    --     cmp.complete()
    --   else
    --     fallback() -- The fallback function sends a already mapped key. In this
    --                -- case, it's probably `<Tab>`.
    --   end
    -- end, { "i", "s" }),
    -- ["<S-Tab>"] = cmp.mapping(function()
    --   if cmp.visible() then
    --     cmp.select_prev_item()
    --   elseif vim.fn["vsnip#jumpable"](-1) == 1 then
    --     feedkey("<Plug>(vsnip-jump-prev)", "")
    --   end
    -- end, { "i", "s" }),
  }),
  sources = cmp.config.sources({
    -- { name = 'luasnip' }, -- For luasnip users.
    -- { name = 'snippy' }, -- For snippy users.
    { name = 'vsnip' }, -- For vsnip users.
    { name = 'nvim_lsp' },
    -- { name = 'ultisnips' },
    { name = 'path' },
  }, {
    { name = 'buffer' },
  }),
  experimental = {
    ghost_text = true,
  },
})

-- Set configuration for specific filetype.
cmp.setup.filetype('gitcommit', {
  sources = cmp.config.sources({
    { name = 'cmp_git' }, -- You can specify the `cmp_git` source if you were installed it.
  }, {
    { name = 'buffer' },
  })
})

-- Use buffer source for `/` (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline('/', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = {
    { name = 'buffer' }
  }
})

-- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
cmp.setup.cmdline(':', {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = 'path' }
  }, {
    { name = 'cmdline' }
  })
})

-- Setup lspconfig.
local capabilities = require('cmp_nvim_lsp').update_capabilities(vim.lsp.protocol.make_client_capabilities())
-- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
require('lspconfig')['ccls'].setup {
  capabilities = capabilities
}
