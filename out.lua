
local function dump(value)
  if type(value) == 'table' then
    local result = '{'
    for k, v in pairs(value) do
      result = result .. '[' .. dump(k) .. '] = ' .. dump(v) .. ', '
    end
    return result .. '}'
  elseif type(value) == 'string' then
    return '"' .. value .. '"'
  else
    return tostring(value)
  end
end

local function make_true()
  local value = {}
  setmetatable(value, { __type_tags = { ['true'] = true } })
  return value
end

local function make_false()
  local value = {}
  setmetatable(value, { __type_tags = { ['false'] = true } })
  return value
end

local function make_none()
  local value = {}
  setmetatable(value, { __type_tags = { ['none'] = true } })
  return value
end

local function make_bool(value)
  if value then
    return make_true()
  else
    return make_false()
  end
end

local function make_int(value)
  local value = { value = value }
  setmetatable(value, { __type_tags = { ['int'] = true } })
  return value
end

local function make_float(value)
  local value = { value = value }
  setmetatable(value, { __type_tags = { ['float'] = true } })
  return value
end

local function make_str(value)
  local value = { value = value }
  setmetatable(value, { __type_tags = { ['str'] = true } })
  return value
end

local function add_type_tag(value, tag)
  getmetatable(value).__type_tags[tag] = true
  return value
end

local function has_type_tag(value, tag)
  return type(value) == 'table' and getmetatable(value).__type_tags[tag] ~= nil
end

local M = {}

M['list'] = function()
  return function(super)
    return add_type_tag(super, 'list')
  end
end

M['one'] = function()
return function(arg0)
  local var0 = arg0
  local _ = nil
  local tmp0 = {head = var0, tail = M['list']()(make_none())}
  setmetatable(tmp0, { __type_tags = {} })
  return M['list']()(tmp0)
end
end

M['prepend'] = function()
return function(arg0)
return function(arg1)
  local var0 = arg1
  local var1 = arg0
  local _ = nil
  local tmp0 = {head = var1, tail = var0}
  setmetatable(tmp0, { __type_tags = {} })
  return M['list']()(tmp0)
end
end
end

M['map'] = function()
return function(arg0)
return function(arg1)
  local var0 = arg1
  local var1 = arg0
  local _ = nil
  local tmp0 = var0
  local tmp1
  if has_type_tag(tmp0, 'none') then
  tmp1 = M['list']()(make_none())
  elseif has_type_tag(tmp0, 'list') then
  local var2 = tmp0
  local _ = nil
  local tmp2 = {head = var1(var2.head), tail = M['map']()(var1)(var2.tail)}
  setmetatable(tmp2, { __type_tags = {} })
  tmp1 = M['list']()(tmp2)
  end
  return tmp1
end
end
end

M['mul'] = function()
return function(arg0)
return function(arg1)
  local var0 = arg1
  local var1 = arg0
  local _ = nil
  return make_int(var1.value * var0.value)
end
end
end

M['main'] = function()
  local _ = nil
  return M['map']()(M['mul']()(make_int(2)))(M['prepend']()(make_int(3))(M['prepend']()(make_int(2))(M['one']()(make_int(1)))))
end

print(dump(M.main()))