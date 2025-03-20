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

local function add_tags(tags, value)
  for _, tag in ipairs(tags) do
    getmetatable(value).__type_tags[tag] = true
  end

  return value
end

local function has_tag(tag, value)
  return type(value) == 'table' and getmetatable(value).__type_tags[tag] ~= nil
end

local function brad_debug_format(value)
  return make_str(tostring(value))
end

local function brad_list_len(list)
  return make_int(#list)
end

local function brad_print(str)
  io.stdout:write(str.value)
  return make_tag('none')
end

local function brad_str_concat(a, b)
  return make_str(a.value .. b.value)
end

local function brad_os_exit(code)
  os.exit(code.value)
  return make_tag('none')
end

local function brad_file_open(path, mode)
  local file = io.open(path.value, mode.value)
  if file == nil then
    local tags = { 'std::io::error::file-not-found', 'std::io::error', 'std::error' }
    return add_tags(tags, make_tag('none'))
  else
    local value = { file = file }

    setmetatable(value, {
      __type_tags = { ['std::io::file'] = true },
      __tostring = function() return 'std::fs::file' end,
      __eq = function(a, b) return a == b end,
    })

    return add_tag('std::fs::file', value)
  end
end

local function brad_file_close(file)
  file.file:close()
  return make_tag('none')
end

local function brad_file_read(file)
  local contents = file.file:read("*all")
  if contents == nil then
    local tags = { 'std::io::error::end-of-file', 'std::io::error', 'std::error' }
    return add_tags(tags, make_tag('none'))
  else
    return make_str(contents)
  end
end

local function brad_file_write(file, str)
  file.file:write(str.value)
end

local function brad_file_flush(file)
  file.file:flush()
  return make_tag('none')
end

local M = {}
