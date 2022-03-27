import Euterpea

t251 :: Music Pitch
t251 =
  let dMinor = d 4 wn :=: f 4 wn :=: a 4 wn
      gMajor = g 4 wn :=: b 4 wn :=: a 4 wn
      cMajor = c 4 bn :=: e 4 bn :=: g 4 bn
   in dMinor :+: gMajor :+: cMajor

-- Not entirely correct, trans shifts by a semi-tone, I don't know enough about music to make it shift to not a sharp/flat
twoFiveOne :: Pitch -> Dur -> Music Pitch
twoFiveOne pitch dur =
  let triad :: Pitch -> Dur -> Music Pitch
      triad p dur' = note dur' p :=: note dur' (trans 3 p) :=: note dur' (trans 3 p)
   in triad (trans 2 pitch) dur :+: triad (trans 7 pitch) dur :+: triad pitch (dur / 2)

data BluesPitchClass = Ro | Mt | Fo | Fi | MS

type BluesPitch = (BluesPitchClass, Octave)

ro, mt, fo, fi, ms :: Octave -> Dur -> Music BluesPitch
ro o d = note d (Ro, o)
mt o d = note d (Mt, o)
fo o d = note d (Fo, o)
fi o d = note d (Fi, o)
ms o d = note d (MS, o)

fromBlues :: Music BluesPitch -> Music Pitch
fromBlues (Prim (Note d (bpc, o))) =
  let bluesToPitchClass :: BluesPitchClass -> PitchClass
      bluesToPitchClass bpc' = case bpc' of Ro -> C; Mt -> Ef; Fo -> F; Fi -> G; MS -> Bf
   in note d (bluesToPitchClass bpc, o)
fromBlues (Prim (Rest d)) = rest d
fromBlues (m :+: m') = fromBlues m :+: fromBlues m'
fromBlues (m :=: m') = fromBlues m :=: fromBlues m'
fromBlues (Modify c m) = Modify c $ fromBlues m

transM :: AbsPitch -> Music Pitch -> Music Pitch
transM ap (Prim (Note d p)) = note d $ trans ap p
transM ap (Prim (Rest d)) = rest d
transM ap (m1 :+: m2) = transM ap m1 :+: transM ap m2
transM ap (m1 :=: m2) = transM ap m1 :=: transM ap m2
transM ap (Modify c m) = transM ap m