local tasty = require 'tasty'
local template = require 'pandoc.template'
local doclayout = require 'pandoc.doclayout'

local assert = tasty.assert
local test = tasty.test_case
local group = tasty.test_group

return {
  group 'template' {
    group 'apply_template' {
      test('simple context', function ()
        local tmplt = 'Hello, $name$!'
        local context = {name = 'Jane'}
        assert.are_equal(
          template.apply_template('', tmplt, context),
          doclayout.concat {
            'Hello, ',
            'Jane',
            '!'
          }
        )
      end),
      test('list context', function ()
        local tmplt = '$for(name)$Hello $it$!\n$endfor$'
        local context = {name = {'Jane', 'John', 'Joseph'}}
        assert.are_equal(
          doclayout.render(template.apply_template('', tmplt, context)),
          'Hello Jane!\nHello John!\nHello Joseph!\n'
        )
      end),
      test('map context', function ()
        local tmplt = 'Employees: $for(employee)$$it.name$$sep$, $endfor$'
        local context = {
          employee = {
            {name = 'Sara', salary = 30000},
            {name = 'Omar', salary = 60000}
          },
          ignored = true
        }
        assert.are_equal(
          doclayout.render(template.apply_template('', tmplt, context)),
          'Employees: Sara, Omar'
        )
      end),
    },
  }
}
