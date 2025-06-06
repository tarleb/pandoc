local tasty = require 'tasty'

local test = tasty.test_case
local group = tasty.test_group
local assert = tasty.assert

local mediabag = require 'pandoc.mediabag'

return {
  group 'insert' {
    test('insert adds an item to the mediabag', function ()
      local fp = "media/hello.txt"
      local mt = "text/plain"
      local contents = "Hello, World!"
      assert.are_same(mediabag.list(), {})
      mediabag.insert(fp, mt, contents)
      assert.are_same(
        mediabag.list(),
        {{['path'] = fp, ['type'] = mt, ['length'] = 13}}
      )
      mediabag.empty() -- clean up
    end),
    test('is idempotent', function ()
      local fp = "media/hello.txt"
      local mt = "text/plain"
      local contents = "Hello, World!"
      mediabag.insert(fp, mt, contents)
      mediabag.insert(fp, mt, contents)
      assert.are_same(
        mediabag.list(),
        {{['path'] = fp, ['type'] = mt, ['length'] = 13}}
      )
      mediabag.empty() -- clean up
    end),
  },

  group 'delete' {
    test('removes an item', function ()
      assert.are_same(mediabag.list(), {})
      mediabag.insert('test.html', 'text/html', '<aside>Who cares?</aside>')
      mediabag.insert('test.css', 'text/plain', 'aside { color: red; }')
      assert.are_equal(#mediabag.list(), 2)
      mediabag.delete('test.html')
      assert.are_same(
        mediabag.list(),
        {{['path'] = 'test.css', ['type'] = 'text/plain', ['length'] = 21}}
      )
      mediabag.empty() -- clean up
    end),
  },

  group 'fetch' {
    test('populates media bag', function ()
      local filename = 'lua/module/sample.svg'
      local mime, contents = mediabag.fetch(filename)
      assert.are_equal(mime, 'image/svg+xml')
      assert.are_equal(contents:sub(1,5), '<?xml')
      mediabag.empty() -- clean up
    end),
  },

  group 'fill' {
    test('populates media bag', function ()
      local filename = 'lua/module/sample.svg'
      local doc = pandoc.Pandoc {
        pandoc.Image('testing', filename)
      }
      mediabag.fill(doc)
      local mime, contents = mediabag.lookup(filename)
      assert.are_equal(mime, 'image/svg+xml')
      assert.are_equal(contents:sub(1,5), '<?xml')
      mediabag.empty() -- clean up
    end),
  },

  group 'items' {
    test('iterates over all items', function ()
      local input_items = {
        ['test.html'] = {'text/html', '<aside>Really?</aside>'},
        ['test.css'] = {'text/plain', 'aside { color: red; }'},
        ['test.js'] = {'application/javascript', 'alert("HI MOM!")'}
      }
      -- fill mediabag
      for name, v in pairs(input_items) do
        mediabag.insert(name, v[1], v[2])
      end

      local seen_items = {}
      for fp, mt, c in mediabag.items() do
        seen_items[fp] = {mt, c}
      end
      assert.are_same(seen_items, input_items)
      mediabag.empty() -- clean up
    end)
  },

  group 'lookup' {
    test('returns MIME type and contents', function ()
      mediabag.insert('test.html', 'text/html', '<aside>Who cares?</aside>')
      local mime, contents = mediabag.lookup('test.html')
      assert.are_equal(mime, 'text/html')
      assert.are_equal(contents, '<aside>Who cares?</aside>')
      mediabag.empty() -- clean up
    end),
  },

  group 'make_data_uri' {
    test('returns a data URI', function ()
      local uri = mediabag.make_data_uri('text/plain', 'foo')
      assert.are_equal(uri:sub(1,5), 'data:')
    end),
    test('URI specifies the given MIME type', function ()
      local mimetype = 'text/plain'
      local uri = mediabag.make_data_uri(mimetype, 'foo')
      assert.are_equal(uri:sub(6, 5 + #mimetype), mimetype)
    end),
  }
}
