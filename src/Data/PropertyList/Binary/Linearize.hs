module Data.PropertyList.Binary.Linearize
    ( linearize
    , absolutize
    , intern
    , delinearize
    ) where

import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Maybe
import Data.PropertyList.Types
import Data.PropertyList.Algebra
import Data.PropertyList.Binary.Algebra ({- instances-})
import Data.PropertyList.Binary.Types
import Data.Sequence as S
import Prelude as P

-- |Flatten a 'PropertyList' to a sequence of 'BPListRecords'.  The resulting records will
-- use absolute addressing and will not have any duplicates.
linearize :: PropertyList -> BPListRecords Abs
linearize = intern . absolutize . fromPlist

-- |Take some 'BPListRecords' using relative addressing and change them to use absolute addressing
absolutize :: BPListRecords Rel -> BPListRecords Abs
absolutize (BPListRecords root recs) =
    BPListRecords root (S.mapWithIndex (shiftRec . fromIntegral) recs)
    where
        shiftRec i = mapObjRefs (i+)

-- |Take some 'BPListRecords' using absolute addressing and eliminate 
-- all duplicate records, compact the table and update all internal
-- references.
--
-- Does not necessarily yield a totally deduplicated table; The process
-- of interning can introduce duplicate records (because it alters arrays,
-- dicts and sets).  All other node types will be deduplicated in one pass,
-- though, which is usually sufficient.
intern :: BPListRecords Abs -> BPListRecords Abs
intern (BPListRecords root recs) = BPListRecords (reloc root) recs'
    where
        reloc i'
            | i < 0 || i >= n   = error ("intern: reference out of bounds: " ++ show i)
            | otherwise         = S.index relocs i
            where i = fromIntegral i'; n = S.length recs
        
        (_, relocs, recs') =
            F.foldl updateRec (M.empty, S.empty, S.empty) recs
        
        updateRec (index, relocs, recs) x = 
            case M.lookup x index of
                Nothing -> 
                    let nRecs = fromIntegral (S.length recs)
                     in ( M.insert x nRecs index
                        , relocs |> nRecs
                        , recs   |> mapObjRefs reloc x
                        )
                Just loc ->
                    ( index, relocs |> loc, recs)

-- TODO: check for cycles?
-- |Reconstruct a property list from a collection of 'BPListRecords'
delinearize :: BPListRecords Abs -> PartialPropertyList UnparsedBPListRecord
delinearize = toPlist

