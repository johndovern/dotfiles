local mp = require 'mp'

local function show_path()
    local trackPath = mp.get_property_native("path")
    mp.osd_message(trackPath)
end

mp.add_key_binding("alt+M", "show-path", function() show_path() end)
