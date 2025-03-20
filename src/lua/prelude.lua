local function deep_clone(value)
  if type(value) == 'table' then
    local result = {}
    for k, v in pairs(value) do
      result[deep_clone(k)] = deep_clone(v)
    end
    return result
  else
    return value
  end
end

local function make_tag(name)
  local value = {}
  setmetatable(value, {
    __type_tags = { [name] = true },
    __tostring = function() return name end,
    __eq = function(_, _) return true end,
  })
  return value
end

local function make_bool(value)
  if value then
    return make_tag('true')
  else
    return make_tag('false')
  end
end

local function make_int(value)
  value = { value = value }
  setmetatable(value, {
    __type_tags = { ['int'] = true },
    __tostring = function() return tostring(value.value) end,
    __eq = function(a, b) return a.value == b.value end,
  })

  return value
end

local function make_float(value)
  value = { value = value }
  setmetatable(value, {
    __type_tags = { ['float'] = true },
    __tostring = function() return tostring(value.value) end,
    __eq = function(a, b) return a.value == b.value end,
  })

  return value
end

local function make_str(value)
  value = { value = value }
  setmetatable(value, {
    __type_tags = { ['str'] = true },
    __tostring = function() return '"' .. value.value .. '"' end,
    __eq = function(a, b) return a.value == b.value end,
  })

  return value
end

local function make_tuple(...)
  local value = { ... }
  setmetatable(value, {
    __type_tags = {},
    __tostring = function()
      local result = '('

      for i, v in ipairs(value) do
        if i > 1 then
          result = result .. ', '
        end

        result = result .. tostring(v)
      end

      return result .. ')'
    end,
  })

  return value
end

local function make_record(value)
  setmetatable(value, {
    __type_tags = {},
    __tostring = function()
      local result = '{'

      for k, v in pairs(value) do
        if #result > 1 then
          result = result .. ';'
        end

        result = result .. ' ' .. k .. ': ' .. tostring(v)
      end

      if #result > 1 then
        result = result .. ' '
      end

      return result .. '}'
    end,
  })

  return value
end

local function make_list(...)
  local value = { ... }

  setmetatable(value, {
    __type_tags = {},
    __tostring = function()
      local result = '['

      for i, v in ipairs(value) do
        if i > 1 then
          result = result .. ', '
        end

        result = result .. tostring(v)
      end

      return result .. ']'
    end,
  })

  return value
end

local function add_tag(tag, value)
  getmetatable(value).__type_tags[tag] = true
  return value
end

local function has_tag(tag, value)
  return type(value) == 'table' and getmetatable(value).__type_tags[tag] ~= nil
end

local M = {}
