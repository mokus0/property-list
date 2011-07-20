module CloseEnough where

import Control.Monad.Identity
import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.PropertyList
import Data.PropertyList.Algebra
import Data.Time

infix 4 ~=

-- this class has only one purpose:  I know dates will not be preserved
-- properly when round-tripping through the binary plist format, because
-- the binary plist format simply does not provide as much resolution 
-- as UTCTime.  This class implements a custom equality test which is
-- identical to (==) except on 'UTCTime's.
class CloseEnough x where
    (~=) :: x -> x -> Bool

instance CloseEnough UTCTime where
    x ~= y  = at x ~= at y
        where
            epoch = UTCTime (fromGregorian 2001 1 1) 0
            at :: UTCTime -> Double
            at t = realToFrac (diffUTCTime t epoch)

instance (CloseEnough a, CloseEnough b) => CloseEnough (Either a b) where
    Left  x ~= Left  y  = x ~= y
    Right x ~= Right y  = x ~= y
    _       ~= _        = False

instance (CloseEnough a, CloseEnough b) => CloseEnough (a, b) where
    (x1, x2) ~= (y1, y2) = x1 ~= y1 && x2 ~= y2

instance (CloseEnough k, CloseEnough v) => CloseEnough (M.Map k v) where
    x ~= y  = M.toAscList x ~= M.toAscList y

instance CloseEnough a => CloseEnough [a] where
    [] ~= []    = True
    (x:xs) ~= (y:ys)    = x ~= y && xs ~= ys

instance CloseEnough Bool       where (~=) = (==)
instance CloseEnough Char       where (~=) = (==)
instance CloseEnough Integer    where (~=) = (==)
instance CloseEnough Double     where (~=) = (==)
instance CloseEnough ByteString where (~=) = (==)

instance CloseEnough a => CloseEnough (PropertyListS a) where
    PLArray  x ~= PLArray  y    = x ~= y
    PLData   x ~= PLData   y    = x ~= y
    PLDate   x ~= PLDate   y    = x ~= y
    PLDict   x ~= PLDict   y    = x ~= y
    PLReal   x ~= PLReal   y    = x ~= y
    PLInt    x ~= PLInt    y    = x ~= y
    PLString x ~= PLString y    = x ~= y
    PLBool   x ~= PLBool   y    = x ~= y

instance CloseEnough PropertyList where
    x ~= y  = runIdentity (plistCoalgebra x) ~= runIdentity (plistCoalgebra y)
