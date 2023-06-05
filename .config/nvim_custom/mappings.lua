---@type MappingsTable
local M = {}

M.general = {
  n = {
    [";"] = { ":", "enter command mode", opts = { nowait = true } },
  },
}

-- more keybinds!

-- M.Vimwiki = {
--   plugin = true,
--   n = {
--     ["<Tab>"] = {":bNext<CR>", "Next buffer"},
--     ["<S-Tab>"] = {":bprevious<CR>", "Previous buffer"},
--   }
-- }
--
return M
