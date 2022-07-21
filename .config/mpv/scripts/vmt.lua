local mp = require 'mp'
local options = require 'mp.options'

local o = {
    enabled = "no",
}
options.read_options(o, "vmt")

local function vmt_scan()
    local trackPath = mp.get_property_native("path")
    if os.execute("vmt -t \"" .. trackPath .. "\"") == 0 then
        mp.osd_message("Library updated")
    end
end

if o.enabled == "yes" then
    mp.register_event("file-loaded", vmt_scan)
end
