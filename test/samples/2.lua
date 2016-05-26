-- The constructor makes a tuple definition, which has its own intern table
-- tuples may have an optional binding to alias a key to a given tuple index

--[[
Tuples are immutable lists that can be used as table keys because they have
value semantics, that is, the tuple constructor returns the same identity for
the exact same list of identities

tuples are composed of only immutable values

TODO allow tuples to contain other tuples?
]]

local tuple = {}

local NAN = setmetatable({a = function() return 5 end})

local NIL = setmetatable({}, {__tostring = function() return "NIL" end})

local allowed_primitive_types = {
    number  = true,
    string  = true,
    ["nil"] = true,
}


local function value_to_key(value)
    if value == nil then
        return NIL
    end

    -- We rely on the property that NaN is the only value that doesn't equal itself
    if value ~= value then
        return NAN
    end

    return value
end

-- use a weak table?
local function create_index()
    return {}
end

local function insert_tuple(tuple_def, index, raw_tuple)
    local tuple = setmetatable({}, tuple_def.tuple_mt)
    tuple_def.tuples[index] = tuple
    tuple_def.raw_tuples[tuple] = raw_tuple
    return tuple
end

local function get_tuple(values, index)
    return values[index]
end

local function find_tuple_n(values, cur_index, count, value, ...)
    if count == 0 then
        return get_tuple(values, cur_index)
    end

    local key = value_to_key(value)
    cur_index = cur_index[key]
    if not cur_index then
        return nil
    end

    return find_tuple_n(values, cur_index, count - 1, ...)
end

local function find_tuple(tuple_def, ...)
    local cur_index = tuple_def.index
    local values = tuple_def.tuples
    local count = select('#', ...)
    assert(count <= tuple_def.size)
    return find_tuple_n(values, cur_index, count, ...)
end



local function new_tuple_n(tuple_def, cur_index, raw_tuple, count, value, ...)
    if count == 0 then
        return insert_tuple(tuple_def, cur_index, raw_tuple)
    end

    local key = value_to_key(value)
    local next_index = cur_index[key]
    if not next_index then
        next_index = create_index()
        cur_index[key] = next_index
    end

    local ndx = #raw_tuple + 1

    local value_type = type(value)
    assertf(allowed_primitive_types[value_type] or tuple.is_a(value),
        "invalid tuple value %s (at position %d)", tostring(value), ndx)
    raw_tuple[ndx] = value

    return new_tuple_n(tuple_def, next_index, raw_tuple, count - 1, ...)
end

local function new_tuple(tuple_def, ...)
    local cur_index = tuple_def.index
    assert(select('#', ...) == tuple_def.size)
    local raw_tuple = {}
    return new_tuple_n(tuple_def, cur_index, raw_tuple, tuple_def.size, ...)
end

local function tuple_to_string(tuple_def, tuple)
    local tuple_strs = {}
    for i=1,#tuple_def.raw_tuples[tuple] do
        tuple_strs[i] = tostring(tuple_def.raw_tuples[tuple][i])
    end
    -- TODO memoize this value in the tuple?
    local tuple_content = table.concat(tuple_strs, ",")
    return ("%s<%s>"):format(tuple_def.data_type, tuple_content)
end

function tuple.is_a(inst)
    local mt = getmetatable(inst)
    return mt and mt.__tuple
end

--- define a tuple
-- @tparam number size number of elements in the tuple
-- @tparam table binding list of keys to use as an alias for the tuple index
function tuple.define(data_type, size, binding)
    local wrapper = {}
    binding = binding or {}
    assert(#binding == 0 or #binding == size)
    for tuple_ndx, alias_key in ipairs(binding) do
        assert(type(alias_key) == "string")
        wrapper[alias_key] = tuple_ndx
    end

    local tuple_def = {
        data_type = data_type,
        size = size,
        raw_tuples = {},      -- tuple lookup to raw tuple
        tuples = {},          -- element N (last X) lookup to tuple
        index = {},           -- element X lookup to element X + 1
        lookup_alias = wrapper,
    }

    -- use the same metatable for all tuples
    tuple_def.tuple_mt = {
        __index = function(t, k)
            local raw = tuple_def.raw_tuples[t]
            return raw[k] or raw[tuple_def.lookup_alias[k]]
        end,
        __data_type = tuple_def.data_type,
        __tuple     = true,
        __newindex  = false,
        __tostring  = function(t)
            return tuple_to_string(tuple_def, t)
        end
    }

    return tuple_def
end

--- create an instance of `tuple_def` populated from the vararg
function tuple.new(tuple_def, ...)
    return find_tuple(tuple_def, ...) or new_tuple(tuple_def, ...)
end

return tuple
