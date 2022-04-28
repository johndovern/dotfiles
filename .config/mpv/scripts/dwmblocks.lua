-- dwmblocks.lua update dwmblocks based on mpv activity

local mp = require 'mp'
local options = require 'mp.options'
local utils = require 'mp.utils'

local o = {
  enabled = "yes",
  music = "no",
}
options.read_options(o, "dwmblocks")

function update_dwmblocks()
    if o.music == "yes" then
        os.execute("kill -55 $(pidof dwmblocks)")
    else
        os.execute("kill -56 $(pidof dwmblocks)")
    end
end

if o.enabled == "yes" then
    mp.register_event("file-loaded", update_dwmblocks)
    mp.register_event("end-file", update_dwmblocks)
    mp.register_event("shutdown", update_dwmblocks)
end

mp.add_key_binding(nil, "update-dwmblocks", function() update_dwmblocks() end)
