module Data.PropertyList.Binary.Linearize
    ( linearize
    , absolutize
    , intern
    , delinearize
    ) where

import Control.Monad
import Control.Monad.ST.Lazy
import qualified Data.Map as M
import Data.Maybe
import Data.PropertyList.Types
import Data.PropertyList.Algebra
import Data.PropertyList.Binary.Algebra ({- instances-})
import Data.PropertyList.Binary.Types
import Data.Sequence as S
import Data.STRef.Lazy
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
intern :: BPListRecords Abs -> BPListRecords Abs
intern (BPListRecords root recs) = BPListRecords (reloc root) recs'
    where
        reloc i = M.findWithDefault noReloc i relocs
            where noReloc = error ("intern: internal error: no relocation for index " ++ show i)
        (relocs, recs') = runST $ do
            index   <- newSTRef M.empty
            relocs  <- newSTRef M.empty
            
            let updateRec n' x = do
                    let n = fromIntegral n'
                    recTable <- readSTRef index
                    case M.lookup x recTable of
                        Nothing     -> do
                            let nRecs = fromIntegral (M.size recTable)
                            modifySTRef relocs (M.insert n nRecs)
                            writeSTRef index (M.insert x nRecs recTable)
                            return (Just (mapObjRefs reloc x))
                        Just loc    -> do
                            modifySTRef relocs (M.insert n loc)
                            return Nothing
            recs <- updateWithIndexM updateRec recs
            
            relocs <- readSTRef relocs
            return (relocs, recs)

updateWithIndexM :: Monad m => (Int -> a -> m (Maybe b)) -> Seq a -> m (Seq b)
updateWithIndexM f = S.foldrWithIndex g (return S.empty)
    where
        g n x xs = do
            mbX <- f n x
            case mbX of
                Nothing -> xs
                Just x'  -> liftM (x' <|) xs

-- TODO: check for cycles?
-- |Reconstruct a property list from a collection of 'BPListRecords'
delinearize :: BPListRecords Abs -> PartialPropertyList UnparsedBPListRecord
delinearize = toPlist

