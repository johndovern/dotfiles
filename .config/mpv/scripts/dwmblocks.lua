-- mpvSockets, one socket per instance, removes socket on exit

local utils = require 'mp.utils'
local mp = require("mp")

function update_dwmblocks()
    os.execute("kill -55 $(pidof dwmblocks)")
end

if mp.get_opt("mpv-music") == "yes" then
    mp.register_event("file-loaded", update_dwmblocks)
    mp.register_event("end-file", update_dwmblocks)
end
