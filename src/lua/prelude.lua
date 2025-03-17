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

local function make_true()
  local value = {}
  setmetatable(value, {
    __type_tags = { ['true'] = true },
    __tostring = function() return 'true' end,
    __eq = function(a, b) return true end,
  })

  return value
end

local function make_false()
  local value = {}
  setmetatable(value, {
    __type_tags = { ['false'] = true },
    __tostring = function() return 'false' end,
    __eq = function(a, b) return true end,
  })

  return value
end

local function make_none()
  local value = {}
  setmetatable(value, {
    __type_tags = { ['none'] = true },
    __tostring = function() return 'none' end,
    __eq = function(a, b) return true end,
  })

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
  setmetatable(value, {
    __type_tags = { ['int'] = true },
    __tostring = function() return tostring(value.value) end,
    __eq = function(a, b) return a.value == b.value end,
  })

  return value
end

local function make_float(value)
  local value = { value = value }
  setmetatable(value, {
    __type_tags = { ['float'] = true },
    __tostring = function() return tostring(value.value) end,
    __eq = function(a, b) return a.value == b.value end,
  })

  return value
end

local function make_str(value)
  local value = { value = value }
  setmetatable(value, {
    __type_tags = { ['str'] = true },
    __tostring = function() return value.value end,
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

local function add_type_tag(value, tag)
  getmetatable(value).__type_tags[tag] = true
  return value
end

local function has_type_tag(value, tag)
  return type(value) == 'table' and getmetatable(value).__type_tags[tag] ~= nil
end

local M = {}
