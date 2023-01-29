_ENV = pandoc
local math_elements = List{}
return {
-- first document pass
{Math = function (m)

   local comment = "math start"
    math_elements:insert(comment)
    math_elements:insert(m)
 end},
-- second document pass
{Pandoc = function (_) return Pandoc(math_elements:map(Plain)) end},
}
