--[[
   Interpolating Search on a String

   LUA 5.1 compatible

   Can only search sorted tables with value string

   table.intsearchstr( table, value ), for searching a normal sorted table
   table.intsearchstrrev( table, value ), for searching a reversed sorted table

   If the  value is found:
      it returns a table holding all the matching indices (e.g. { startindice,endindice } )
      endindice may be the same as startindice if only one matching indice was found
   Return value:
      on success: a table holding matching indices (e.g. { startindice,endindice } )
      on failure: nil
]]--
do
   -- Avoid heap allocs for performance
   local getbytevalue = function( value )
      if value then
         local num,mul = 0,1
         -- set bytedept, 4 or 5 seems appropriate
         for i = 4,1,-1 do
            local byte = string.byte( string.sub( value,i,i ) ) or -1
            byte,num,mul = byte + 1,num + mul*byte,mul*257
         end
         return num
      end
   end
   -- Load the optimised binary function
   -- Avoid heap allocs for performance
   local fcompf = function( a,b ) return a < b end
   local fcompr = function( a,b ) return a > b end
   local function tablebinsearch( t,value,reversed,iStart,iEnd )
      -- Initialise functions
      local fcomp = reversed and fcompr or fcompf
      --  Initialise numbers
      local iStart,iEnd,iMid = iStart or 1,iEnd or #t,0
      -- Binary Search
      while iStart <= iEnd do
         -- calculate middle
         iMid = math.floor( (iStart+iEnd)/2 )
         -- get compare value
         local value2 = t[iMid]
         -- get all values that match
         if value == value2 then
            local tfound,num = { iMid,iMid },iMid - 1
            while value == t[num] do
               tfound[1],num = num,num - 1
            end
            num = iMid + 1
            while value == t[num] do
               tfound[2],num = num,num + 1
            end
            return tfound
         -- keep searching
         elseif fcomp( value,value2 ) then
            iEnd = iMid - 1
         else
            iStart = iMid + 1
         end
      end
   end

   -- The interpolationg Search Function on a String
   function table.intsearchstr( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow < _ihigh do
         -- get interpolated position
         local pos = math.floor( (_value-_ilow)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value < t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,_,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,_,ilow,ihigh )
   end
   -- The interpolationg Search Function on a String
   function table.intsearchstrrev( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow > _ihigh do
         -- get interpolated position
         local pos = math.floor( (_ihigh-_value)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value > t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,1,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,1,ilow,ihigh )
   end
end
-- CHILLCODE™

--[[
   Interpolating Search on a String

   LUA 5.1 compatible

   Can only search sorted tables with value string

   table.intsearchstr( table, value ), for searching a normal sorted table
   table.intsearchstrrev( table, value ), for searching a reversed sorted table

   If the  value is found:
      it returns a table holding all the matching indices (e.g. { startindice,endindice } )
      endindice may be the same as startindice if only one matching indice was found
   Return value:
      on success: a table holding matching indices (e.g. { startindice,endindice } )
      on failure: nil
]]--
do
   -- Avoid heap allocs for performance
   local getbytevalue = function( value )
      if value then
         local num,mul = 0,1
         -- set bytedept, 4 or 5 seems appropriate
         for i = 4,1,-1 do
            local byte = string.byte( string.sub( value,i,i ) ) or -1
            byte,num,mul = byte + 1,num + mul*byte,mul*257
         end
         return num
      end
   end
   -- Load the optimised binary function
   -- Avoid heap allocs for performance
   local fcompf = function( a,b ) return a < b end
   local fcompr = function( a,b ) return a > b end
   local function tablebinsearch( t,value,reversed,iStart,iEnd )
      -- Initialise functions
      local fcomp = reversed and fcompr or fcompf
      --  Initialise numbers
      local iStart,iEnd,iMid = iStart or 1,iEnd or #t,0
      -- Binary Search
      while iStart <= iEnd do
         -- calculate middle
         iMid = math.floor( (iStart+iEnd)/2 )
         -- get compare value
         local value2 = t[iMid]
         -- get all values that match
         if value == value2 then
            local tfound,num = { iMid,iMid },iMid - 1
            while value == t[num] do
               tfound[1],num = num,num - 1
            end
            num = iMid + 1
            while value == t[num] do
               tfound[2],num = num,num + 1
            end
            return tfound
         -- keep searching
         elseif fcomp( value,value2 ) then
            iEnd = iMid - 1
         else
            iStart = iMid + 1
         end
      end
   end

   -- The interpolationg Search Function on a String
   function table.intsearchstr( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow < _ihigh do
         -- get interpolated position
         local pos = math.floor( (_value-_ilow)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value < t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,_,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,_,ilow,ihigh )
   end
   -- The interpolationg Search Function on a String
   function table.intsearchstrrev( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow > _ihigh do
         -- get interpolated position
         local pos = math.floor( (_ihigh-_value)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value > t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,1,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,1,ilow,ihigh )
   end
end
-- CHILLCODE™

--[[
   Interpolating Search on a String

   LUA 5.1 compatible

   Can only search sorted tables with value string

   table.intsearchstr( table, value ), for searching a normal sorted table
   table.intsearchstrrev( table, value ), for searching a reversed sorted table

   If the  value is found:
      it returns a table holding all the matching indices (e.g. { startindice,endindice } )
      endindice may be the same as startindice if only one matching indice was found
   Return value:
      on success: a table holding matching indices (e.g. { startindice,endindice } )
      on failure: nil
]]--
do
   -- Avoid heap allocs for performance
   local getbytevalue = function( value )
      if value then
         local num,mul = 0,1
         -- set bytedept, 4 or 5 seems appropriate
         for i = 4,1,-1 do
            local byte = string.byte( string.sub( value,i,i ) ) or -1
            byte,num,mul = byte + 1,num + mul*byte,mul*257
         end
         return num
      end
   end
   -- Load the optimised binary function
   -- Avoid heap allocs for performance
   local fcompf = function( a,b ) return a < b end
   local fcompr = function( a,b ) return a > b end
   local function tablebinsearch( t,value,reversed,iStart,iEnd )
      -- Initialise functions
      local fcomp = reversed and fcompr or fcompf
      --  Initialise numbers
      local iStart,iEnd,iMid = iStart or 1,iEnd or #t,0
      -- Binary Search
      while iStart <= iEnd do
         -- calculate middle
         iMid = math.floor( (iStart+iEnd)/2 )
         -- get compare value
         local value2 = t[iMid]
         -- get all values that match
         if value == value2 then
            local tfound,num = { iMid,iMid },iMid - 1
            while value == t[num] do
               tfound[1],num = num,num - 1
            end
            num = iMid + 1
            while value == t[num] do
               tfound[2],num = num,num + 1
            end
            return tfound
         -- keep searching
         elseif fcomp( value,value2 ) then
            iEnd = iMid - 1
         else
            iStart = iMid + 1
         end
      end
   end

   -- The interpolationg Search Function on a String
   function table.intsearchstr( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow < _ihigh do
         -- get interpolated position
         local pos = math.floor( (_value-_ilow)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value < t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,_,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,_,ilow,ihigh )
   end
   -- The interpolationg Search Function on a String
   function table.intsearchstrrev( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow > _ihigh do
         -- get interpolated position
         local pos = math.floor( (_ihigh-_value)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value > t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,1,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,1,ilow,ihigh )
   end
end
-- CHILLCODE™

--[[
   Interpolating Search on a String

   LUA 5.1 compatible

   Can only search sorted tables with value string

   table.intsearchstr( table, value ), for searching a normal sorted table
   table.intsearchstrrev( table, value ), for searching a reversed sorted table

   If the  value is found:
      it returns a table holding all the matching indices (e.g. { startindice,endindice } )
      endindice may be the same as startindice if only one matching indice was found
   Return value:
      on success: a table holding matching indices (e.g. { startindice,endindice } )
      on failure: nil
]]--
do
   -- Avoid heap allocs for performance
   local getbytevalue = function( value )
      if value then
         local num,mul = 0,1
         -- set bytedept, 4 or 5 seems appropriate
         for i = 4,1,-1 do
            local byte = string.byte( string.sub( value,i,i ) ) or -1
            byte,num,mul = byte + 1,num + mul*byte,mul*257
         end
         return num
      end
   end
   -- Load the optimised binary function
   -- Avoid heap allocs for performance
   local fcompf = function( a,b ) return a < b end
   local fcompr = function( a,b ) return a > b end
   local function tablebinsearch( t,value,reversed,iStart,iEnd )
      -- Initialise functions
      local fcomp = reversed and fcompr or fcompf
      --  Initialise numbers
      local iStart,iEnd,iMid = iStart or 1,iEnd or #t,0
      -- Binary Search
      while iStart <= iEnd do
         -- calculate middle
         iMid = math.floor( (iStart+iEnd)/2 )
         -- get compare value
         local value2 = t[iMid]
         -- get all values that match
         if value == value2 then
            local tfound,num = { iMid,iMid },iMid - 1
            while value == t[num] do
               tfound[1],num = num,num - 1
            end
            num = iMid + 1
            while value == t[num] do
               tfound[2],num = num,num + 1
            end
            return tfound
         -- keep searching
         elseif fcomp( value,value2 ) then
            iEnd = iMid - 1
         else
            iStart = iMid + 1
         end
      end
   end

   -- The interpolationg Search Function on a String
   function table.intsearchstr( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow < _ihigh do
         -- get interpolated position
         local pos = math.floor( (_value-_ilow)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value < t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,_,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,_,ilow,ihigh )
   end
   -- The interpolationg Search Function on a String
   function table.intsearchstrrev( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow > _ihigh do
         -- get interpolated position
         local pos = math.floor( (_ihigh-_value)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value > t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,1,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,1,ilow,ihigh )
   end
end
-- CHILLCODE™

--[[
   Interpolating Search on a String

   LUA 5.1 compatible

   Can only search sorted tables with value string

   table.intsearchstr( table, value ), for searching a normal sorted table
   table.intsearchstrrev( table, value ), for searching a reversed sorted table

   If the  value is found:
      it returns a table holding all the matching indices (e.g. { startindice,endindice } )
      endindice may be the same as startindice if only one matching indice was found
   Return value:
      on success: a table holding matching indices (e.g. { startindice,endindice } )
      on failure: nil
]]--
do
   -- Avoid heap allocs for performance
   local getbytevalue = function( value )
      if value then
         local num,mul = 0,1
         -- set bytedept, 4 or 5 seems appropriate
         for i = 4,1,-1 do
            local byte = string.byte( string.sub( value,i,i ) ) or -1
            byte,num,mul = byte + 1,num + mul*byte,mul*257
         end
         return num
      end
   end
   -- Load the optimised binary function
   -- Avoid heap allocs for performance
   local fcompf = function( a,b ) return a < b end
   local fcompr = function( a,b ) return a > b end
   local function tablebinsearch( t,value,reversed,iStart,iEnd )
      -- Initialise functions
      local fcomp = reversed and fcompr or fcompf
      --  Initialise numbers
      local iStart,iEnd,iMid = iStart or 1,iEnd or #t,0
      -- Binary Search
      while iStart <= iEnd do
         -- calculate middle
         iMid = math.floor( (iStart+iEnd)/2 )
         -- get compare value
         local value2 = t[iMid]
         -- get all values that match
         if value == value2 then
            local tfound,num = { iMid,iMid },iMid - 1
            while value == t[num] do
               tfound[1],num = num,num - 1
            end
            num = iMid + 1
            while value == t[num] do
               tfound[2],num = num,num + 1
            end
            return tfound
         -- keep searching
         elseif fcomp( value,value2 ) then
            iEnd = iMid - 1
         else
            iStart = iMid + 1
         end
      end
   end

   -- The interpolationg Search Function on a String
   function table.intsearchstr( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow < _ihigh do
         -- get interpolated position
         local pos = math.floor( (_value-_ilow)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value < t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,_,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,_,ilow,ihigh )
   end
   -- The interpolationg Search Function on a String
   function table.intsearchstrrev( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow > _ihigh do
         -- get interpolated position
         local pos = math.floor( (_ihigh-_value)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value > t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,1,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,1,ilow,ihigh )
   end
end
-- CHILLCODE™

--[[
   Interpolating Search on a String

   LUA 5.1 compatible

   Can only search sorted tables with value string

   table.intsearchstr( table, value ), for searching a normal sorted table
   table.intsearchstrrev( table, value ), for searching a reversed sorted table

   If the  value is found:
      it returns a table holding all the matching indices (e.g. { startindice,endindice } )
      endindice may be the same as startindice if only one matching indice was found
   Return value:
      on success: a table holding matching indices (e.g. { startindice,endindice } )
      on failure: nil
]]--
do
   -- Avoid heap allocs for performance
   local getbytevalue = function( value )
      if value then
         local num,mul = 0,1
         -- set bytedept, 4 or 5 seems appropriate
         for i = 4,1,-1 do
            local byte = string.byte( string.sub( value,i,i ) ) or -1
            byte,num,mul = byte + 1,num + mul*byte,mul*257
         end
         return num
      end
   end
   -- Load the optimised binary function
   -- Avoid heap allocs for performance
   local fcompf = function( a,b ) return a < b end
   local fcompr = function( a,b ) return a > b end
   local function tablebinsearch( t,value,reversed,iStart,iEnd )
      -- Initialise functions
      local fcomp = reversed and fcompr or fcompf
      --  Initialise numbers
      local iStart,iEnd,iMid = iStart or 1,iEnd or #t,0
      -- Binary Search
      while iStart <= iEnd do
         -- calculate middle
         iMid = math.floor( (iStart+iEnd)/2 )
         -- get compare value
         local value2 = t[iMid]
         -- get all values that match
         if value == value2 then
            local tfound,num = { iMid,iMid },iMid - 1
            while value == t[num] do
               tfound[1],num = num,num - 1
            end
            num = iMid + 1
            while value == t[num] do
               tfound[2],num = num,num + 1
            end
            return tfound
         -- keep searching
         elseif fcomp( value,value2 ) then
            iEnd = iMid - 1
         else
            iStart = iMid + 1
         end
      end
   end

   -- The interpolationg Search Function on a String
   function table.intsearchstr( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow < _ihigh do
         -- get interpolated position
         local pos = math.floor( (_value-_ilow)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value < t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,_,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,_,ilow,ihigh )
   end
   -- The interpolationg Search Function on a String
   function table.intsearchstrrev( t,value )
      -- Inistialise numbers
      local ilow,ihigh = 1,#t
      -- return on empty table
      if not t[ilow] then return end
      -- get byte values values of indices and searched value
      local _ilow,_ihigh,_value = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),getbytevalue(value)
      local oldpos = -1
      -- make sure slope cannot become 0
      while _ilow and _ilow > _ihigh do
         -- get interpolated position
         local pos = math.floor( (_ihigh-_value)*(ihigh-ilow)/(_ihigh-_ilow) ) + ilow
         -- check for out of range
         if pos < ilow or pos > ihigh then return end
         -- check for real match
         if value == t[pos] then
            -- insert found position
            local tfound,num = { pos,pos },pos-1
            -- get all values that match
            while value == t[num] do
               tfound[1],num = num,num-1
            end
            num = pos+1
            while value == t[num] do
               tfound[2],num = num,num+1
            end
            return tfound
         -- keep searching,
         -- left part of the table,check for real lower
         elseif value > t[pos] then
            ihigh = pos-1
         else
            ilow = pos+1
         end
         -- check if new position is further than 1 away
         if oldpos+1 == pos or oldpos-1 == pos then
            -- start binary search
            return tablebinsearch( t,value,1,ilow,ihigh )
         end
         _ilow,_ihigh,oldpos = getbytevalue(t[ilow]),getbytevalue(t[ihigh]),pos
      end
      -- initiate binary seach function here since we could be in a flat for the interpolating search
      return tablebinsearch( t,value,1,ilow,ihigh )
   end
end
-- CHILLCODE™
