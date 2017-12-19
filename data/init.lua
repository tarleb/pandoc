-- This Lua script is run every time the Lua interpreter is started when running
-- a Lua filter. It can be customized to load additional modules or to alter the
-- default modules.

pandoc = require 'pandoc'
pandoc.mediabag = require 'pandoc.mediabag'
pandoc.utils = require 'pandoc.utils'

function deprecated(fn, msg)
  msg = msg or 'This function is deprecated.'
  return function (...)
    io.stderr:write('DEPRECATION WARNING: ' .. msg .. '\n')
    return fn(...)
  end
end

pandoc.sha1 = deprecated(
  pandoc.utils.sha1,
  'the function sha1 has been moved to pandoc.utils.sha1.'
)
