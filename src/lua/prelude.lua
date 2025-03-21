local function deep_clone(value)
  if type(value) == 'table' then
    local result = {}

    for k, v in pairs(value) do
      result[deep_clone(k)] = deep_clone(v)
    end

    setmetatable(result, getmetatable(value))

    return result
  else
    return value
  end
end

local function shallow_clone(value)
  if type(value) == 'table' then
    local result = {}

    setmetatable(result, getmetatable(value))

    for k, v in pairs(value) do
      result[k] = v
    end

    return result
  else
    return value
  end
end

local tag_metatable = {
  __tostring = function(tag) return tag['__tag_name'] end,
  __eq = function(a, b) return a['__type_tags'] == b['__type_tags'] end,
}

local int_metatable = {
  __tostring = function(value) return tostring(value.value) end,

  __eq = function(a, b) return a.value == b.value end,
}

local float_metatable = {
  __tostring = function(value) return tostring(value.value) end,

  __eq = function(a, b) return a.value == b.value end,
}

local str_metatable = {
  __tostring = function(value) return value.value end,

  __eq = function(a, b) return a.value == b.value end,
}

local tuple_metatable = {
  __tostring = function(value)
    local result = '('

    for i, v in ipairs(value) do
      if i > 1 then
        result = result .. ', '
      end

      result = result .. tostring(v)
    end

    return result .. ')'
  end,

  __eq = function(a, b)
    if #a ~= #b then
      return false
    end

    for i, v in ipairs(a) do
      if v ~= b[i] then
        return false
      end
    end

    return true
  end,
}

local record_metatable = {
  __tostring = function(value)
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

  __eq = function(a, b)
    for k, v in pairs(a) do
      if b[k] ~= v then
        return false
      end
    end

    for k, v in pairs(b) do
      if a[k] ~= v then
        return false
      end
    end

    return true
  end,
}

local array_metatable = {
  __tostring = function(value)
    local result = '['

    for i, v in ipairs(value) do
      if i > 1 then
        result = result .. ', '
      end

      result = result .. tostring(v)
    end

    return result .. ']'
  end,

  __eq = function(a, b)
    if #a ~= #b then
      return false
    end

    for i, v in ipairs(a) do
      if v ~= b[i] then
        return false
      end
    end

    return true
  end,
}

local function make_tag(name)
  local value = {
    ["__type_tags"] = { [name] = true },
    ["__tag_name"] = name,
  }

  setmetatable(value, tag_metatable)

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
  value = {
    ["__type_tags"] = { ['int'] = true },
    value = value,
  }

  setmetatable(value, int_metatable)

  return value
end

local function make_float(value)
  value = {
    ["__type_tags"] = { ['float'] = true },
    value = value,
  }

  setmetatable(value, float_metatable)

  return value
end

local function make_str(value)
  value = {
    ["__type_tags"] = { ['str'] = true },
    value = value
  }

  setmetatable(value, str_metatable)

  return value
end

local function make_tuple(...)
  local value = {
    ['__type_tags'] = {},
    ...,
  }

  setmetatable(value, tuple_metatable)

  return value
end

local function make_record(value)
  value['__type_tags'] = {}
  setmetatable(value, record_metatable)
  return value
end

local function make_array(...)
  local value = {
    ['__type_tags'] = {},
    ...,
  }
  setmetatable(value, array_metatable)
  return value
end

local function add_tag(tag, value)
  value['__type_tags'][tag] = true
  return value
end

local function add_tags(tags, value)
  for _, tag in ipairs(tags) do
    value['__type_tags'][tag] = true
  end

  return value
end

local function has_tag(tag, value)
  return type(value) == 'table' and value['__type_tags'][tag] ~= nil
end

local function brad_debug_format(value)
  return make_str(tostring(value))
end

local function brad_array_len(list)
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
