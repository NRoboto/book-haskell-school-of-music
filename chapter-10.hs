import Euterpea

type SNote = (Dur, AbsPitch)
data Cluster = Cluster SNote [Cluster]

selfSim :: [SNote] -> Cluster
selfSim pat = Cluster (0, 0) (map mkCluster pat)
  where mkCluster note = Cluster note $ map (mkCluster . addMult note) pat
        addMult :: SNote -> SNote -> SNote
        addMult (d0, p0) (d1, p1) = (d0 * d1, p0 + p1)

fringe :: Int -> Cluster -> [SNote]
fringe 0 (Cluster note _) = [note]
fringe n (Cluster note cls) = concatMap (fringe $ n - 1) cls

mkNote :: (Dur, AbsPitch) -> Music Pitch
mkNote (d, ap) = note d $ pitch ap

simToMusic :: [SNote] -> Music Pitch 
simToMusic = line . map mkNote

ss :: [SNote] -> Int -> AbsPitch -> Dur -> Music Pitch
ss pat n tr te = transpose tr $ tempo te $ simToMusic $ fringe n $ selfSim pat

m0 :: [SNote]
m0 = [(1, 2), (1, 0), (1, 5), (1, 7)]
tm0 = instrument Vibraphone $ ss m0 4 50 20

m1 :: [SNote]
m1 = [(1, 0), (0.5, 0), (0.5, 0)]
tm1 = instrument Percussion $ ss m1 4 43 2