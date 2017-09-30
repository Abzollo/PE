import qualified Data.Set as Set
import Data.List (sort)
import Data.Array.IArray

main = print sol
sol = sum . map fst $ elems effExponTable
maxK = 200

effExponTable :: Array Int (Int, Set.Set (Set.Set Int))
effExponTable = listArray (1,maxK) [effExp k | k <- [1..maxK]]

effExp 1 = (0, Set.singleton (Set.singleton 1))
effExp k = compress $
           [effExp' (ks, mults_ks, used_ks_instance) (kb, mults_kb, used_kb_instance)|
                ks <- [1..div k 2], let kb = k - ks,
                let (mults_ks, used_ks_set) = effExponTable ! ks,
                let (mults_kb, used_kb_set) = effExponTable ! kb,
                used_ks_instance <- Set.elems used_ks_set,
                used_kb_instance <- Set.elems used_kb_set]
    where effExp' (ks, mults_ks, used_ks) (kb, mults_kb, used_kb)
              | ks `Set.member` used_kb = (1 + mults_kb, (ks+kb) `Set.insert` used_kb)
              | otherwise = (mults_ks + mults_kb, (ks+kb) `Set.insert` (used_ks `Set.union` used_kb))
          compress l = compress' l (fst $ minimum l)
          compress' l m = (m, Set.fromList . map snd . filter (\t -> m == fst t) $ l)