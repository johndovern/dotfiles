local mp = require 'mp'
local options = require 'mp.options'

local o = {
    enabled = "no",
    remove  = "eof",
}
options.read_options(o, "dmenuplaylist")

FileName = nil

function set_file_name()
    FileName = mp.get_property_native("path")
    print("Path .... " .. FileName)
    if is_url() then
        FileName = mp.get_property_native("media-title")
        print("Media-Title .... " .. FileName)
    end
    print("FileName set to .... " .. FileName)
end

function is_url()
    -- Function to test if fileName is a url
    local parts = split(FileName, "://", 1)
    return #parts > 1
end

function split(str, sep, num)
    -- Function to split a string on each delimiter and return a table of each value
    local t = {}
    for substr in str:gsub(sep, "\0", num):gmatch("([^%z]+)") do
        table.insert(t, substr)
    end
    return t
end

function remove_from_list(t)
    if not test_remove_event(t.reason) then return end
    if FileName ~= nil then
        print("Removing file .... " .. FileName)
        os.execute("dmenuplaylist --delete \"" .. FileName .. "\"")
    end
end

function test_remove_event(event)
    local t = split(o.remove, ",")
    for _, v in ipairs(t) do
        if v == event then return true end
    end
    return false
end

if o.enabled == "yes" then
    mp.register_event("file-loaded", set_file_name)
    mp.register_event("end-file", remove_from_list)
end
