utils = require 'pandoc'

-- SHA1
------------------------------------------------------------------------
sha1 = utils.sha1'Hello, World!' == '0a0a9f2a6772942557ab5355d76af442f8f65e01'

-- Pipe
------------------------------------------------------------------------
-- check commands exist
local sed_pipe = true
local fh_sed = io.popen 'sed --version'
if fh_sed then
  fh_sed:close()
  sed_pipe = (utils.pipe('sed', {'-e', 's/a/b/'}, 'abc') == 'bbc')
end

local false_pipe = true
local fh_false = io.popen 'false'
if fh_false then
  fh_false:close()
  res, err = pcall(utils.pipe, 'false', {}, 'abc')
  false_pipe = not res and
    err.command == 'false' and
    err.error_code == 1 and
    err.output == ''
end

-- Return result
------------------------------------------------------------------------
function to_res(x)
  return x and "OK" or "FAIL"
end

function Para (el)
  return {
    pandoc.Plain{pandoc.Str("sha1: " .. to_res(sha1))},
    pandoc.Plain{pandoc.Str("sed pipe: " .. to_res(sed_pipe))},
    pandoc.Plain{pandoc.Str("false pipe: " .. to_res(false_pipe))},
  }
end
