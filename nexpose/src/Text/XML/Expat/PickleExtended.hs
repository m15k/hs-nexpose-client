-- | Simple Utility Library for hexpat-pickle.  

module Text.XML.Expat.PickleExtended where

import Text.XML.Expat.Pickle
import Text.XML.Expat.Tree

-- | Convert XML text \<-\> a 7-tuple using the seven arguments.
xp7Tuple :: PU [t] a -> PU [t] b -> PU [t] c -> PU [t] d -> PU [t] e -> PU [t] f -> PU [t] g -> PU [t] (a,b,c,d,e,f,g)
xp7Tuple pua pub puc pud pue puf pug = PU {
        unpickleTree = \t -> (unpickleTree pua t, unpickleTree pub t, unpickleTree puc t,
                  unpickleTree pud t, unpickleTree pue t, unpickleTree puf t, unpickleTree pug t),
        unpickleTree' = \t ->
            case (unpickleTree' pua t, unpickleTree' pub t, unpickleTree' puc t,
                  unpickleTree' pud t, unpickleTree' pue t, unpickleTree' puf t, 
                  unpickleTree' pug t) of
                (Right a, Right b, Right c, Right d, Right e, Right f, Right g) -> Right (a,b,c,d,e,f,g)
                (Left err, _, _, _, _, _, _) -> Left $ "in 1st of 7-tuple, "++err
                (_, Left err, _, _, _, _, _) -> Left $ "in 2nd of 7-tuple, "++err
                (_, _, Left err, _, _, _, _) -> Left $ "in 3rd of 7-tuple, "++err
                (_, _, _, Left err, _, _, _) -> Left $ "in 4th of 7-tuple, "++err
                (_, _, _, _, Left err, _, _) -> Left $ "in 5th of 7-tuple, "++err
                (_, _, _, _, _, Left err, _) -> Left $ "in 6th of 7-tuple, "++err
                (_, _, _, _, _, _, Left err) -> Left $ "in 7th of 7-tuple, "++err,
        pickleTree = \(a, b, c, d, e, f, g) ->
            pickleTree pua a ++
            pickleTree pub b ++
            pickleTree puc c ++
            pickleTree pud d ++
            pickleTree pue e ++
            pickleTree puf f ++
            pickleTree pug g
    }

-- | Convert XML text \<-\> a 8-tuple using the eight arguments.
xp8Tuple :: PU [t] a -> PU [t] b -> PU [t] c -> PU [t] d -> PU [t] e -> PU [t] f ->
            PU [t] g -> PU [t] h -> PU [t] (a,b,c,d,e,f,g,h)
xp8Tuple pua pub puc pud pue puf pug puh = PU {
        unpickleTree = \t -> (unpickleTree pua t, unpickleTree pub t, unpickleTree puc t,
                  unpickleTree pud t, unpickleTree pue t, unpickleTree puf t, unpickleTree pug t,
                  unpickleTree puh t),
        unpickleTree' = \t ->
            case (unpickleTree' pua t, unpickleTree' pub t, unpickleTree' puc t,
                  unpickleTree' pud t, unpickleTree' pue t, unpickleTree' puf t, 
                  unpickleTree' pug t, unpickleTree' puh t) of
                (Right a, Right b, Right c, Right d, Right e, Right f, Right g,
                 Right h) -> Right (a,b,c,d,e,f,g,h)
                (Left err, _, _, _, _, _, _, _) -> Left $ "in 1st of 8-tuple, "++err
                (_, Left err, _, _, _, _, _, _) -> Left $ "in 2nd of 8-tuple, "++err
                (_, _, Left err, _, _, _, _, _) -> Left $ "in 3rd of 8-tuple, "++err
                (_, _, _, Left err, _, _, _, _) -> Left $ "in 4th of 8-tuple, "++err
                (_, _, _, _, Left err, _, _, _) -> Left $ "in 5th of 8-tuple, "++err
                (_, _, _, _, _, Left err, _, _) -> Left $ "in 6th of 8-tuple, "++err
                (_, _, _, _, _, _, Left err, _) -> Left $ "in 7th of 8-tuple, "++err
                (_, _, _, _, _, _, _, Left err) -> Left $ "in 8th of 8-tuple, "++err,
        pickleTree = \(a, b, c, d, e, f, g, h) ->
            pickleTree pua a ++
            pickleTree pub b ++
            pickleTree puc c ++
            pickleTree pud d ++
            pickleTree pue e ++
            pickleTree puf f ++
            pickleTree pug g ++
            pickleTree puh h
    }

-- | Convert XML text \<-\> a 9-tuple using the nine arguments.
xp9Tuple :: PU [t] a -> PU [t] b -> PU [t] c -> PU [t] d -> PU [t] e -> PU [t] f ->
            PU [t] g -> PU [t] h -> PU [t] i -> PU [t] (a,b,c,d,e,f,g,h,i)
xp9Tuple pua pub puc pud pue puf pug puh pui = PU {
        unpickleTree = \t -> (unpickleTree pua t, unpickleTree pub t, unpickleTree puc t,
                  unpickleTree pud t, unpickleTree pue t, unpickleTree puf t, unpickleTree pug t,
                  unpickleTree puh t, unpickleTree pui t),
        unpickleTree' = \t ->
            case (unpickleTree' pua t, unpickleTree' pub t, unpickleTree' puc t,
                  unpickleTree' pud t, unpickleTree' pue t, unpickleTree' puf t, 
                  unpickleTree' pug t, unpickleTree' puh t, unpickleTree' pui t) of
                (Right a, Right b, Right c, Right d, Right e, Right f, Right g,
                 Right h, Right i) -> Right (a,b,c,d,e,f,g,h,i)
                (Left err, _, _, _, _, _, _, _, _) -> Left $ "in 1st of 9-tuple, "++err
                (_, Left err, _, _, _, _, _, _, _) -> Left $ "in 2nd of 9-tuple, "++err
                (_, _, Left err, _, _, _, _, _, _) -> Left $ "in 3rd of 9-tuple, "++err
                (_, _, _, Left err, _, _, _, _, _) -> Left $ "in 4th of 9-tuple, "++err
                (_, _, _, _, Left err, _, _, _, _) -> Left $ "in 5th of 9-tuple, "++err
                (_, _, _, _, _, Left err, _, _, _) -> Left $ "in 6th of 9-tuple, "++err
                (_, _, _, _, _, _, Left err, _, _) -> Left $ "in 7th of 9-tuple, "++err
                (_, _, _, _, _, _, _, Left err, _) -> Left $ "in 8th of 9-tuple, "++err
                (_, _, _, _, _, _, _, _, Left err) -> Left $ "in 9th of 9-tuple, "++err,
        pickleTree = \(a, b, c, d, e, f, g, h, i) ->
            pickleTree pua a ++
            pickleTree pub b ++
            pickleTree puc c ++
            pickleTree pud d ++
            pickleTree pue e ++
            pickleTree puf f ++
            pickleTree pug g ++
            pickleTree puh h ++
            pickleTree pui i
    }

-- | Convert XML text \<-\> a 10-tuple using the ten arguments.
xp10Tuple :: PU [t] a -> PU [t] b -> PU [t] c -> PU [t] d -> PU [t] e -> PU [t] f ->
             PU [t] g -> PU [t] h -> PU [t] i -> PU [t] j -> PU [t] (a,b,c,d,e,f,g,h,i,j)
xp10Tuple pua pub puc pud pue puf pug puh pui puj = PU {
        unpickleTree = \t -> (unpickleTree pua t, unpickleTree pub t, unpickleTree puc t,
                  unpickleTree pud t, unpickleTree pue t, unpickleTree puf t, unpickleTree pug t,
                  unpickleTree puh t, unpickleTree pui t,unpickleTree puj t),
        unpickleTree' = \t ->
            case (unpickleTree' pua t, unpickleTree' pub t, unpickleTree' puc t,
                  unpickleTree' pud t, unpickleTree' pue t, unpickleTree' puf t, 
                  unpickleTree' pug t, unpickleTree' puh t, unpickleTree' pui t,
                  unpickleTree' puj t) of
                (Right a, Right b, Right c, Right d, Right e, Right f, Right g,
                 Right h, Right i, Right j) -> Right (a,b,c,d,e,f,g,h,i,j)
                (Left err, _, _, _, _, _, _, _, _, _) -> Left $ "in 1st of 10-tuple, "++err
                (_, Left err, _, _, _, _, _, _, _, _) -> Left $ "in 2nd of 10-tuple, "++err
                (_, _, Left err, _, _, _, _, _, _, _) -> Left $ "in 3rd of 10-tuple, "++err
                (_, _, _, Left err, _, _, _, _, _, _) -> Left $ "in 4th of 10-tuple, "++err
                (_, _, _, _, Left err, _, _, _, _, _) -> Left $ "in 5th of 10-tuple, "++err
                (_, _, _, _, _, Left err, _, _, _, _) -> Left $ "in 6th of 10-tuple, "++err
                (_, _, _, _, _, _, Left err, _, _, _) -> Left $ "in 7th of 10-tuple, "++err
                (_, _, _, _, _, _, _, Left err, _, _) -> Left $ "in 8th of 10-tuple, "++err
                (_, _, _, _, _, _, _, _, Left err, _) -> Left $ "in 9th of 10-tuple, "++err
                (_, _, _, _, _, _, _, _, _, Left err) -> Left $ "in 10th of 10-tuple, "++err,
        pickleTree = \(a, b, c, d, e, f, g, h, i, j) ->
            pickleTree pua a ++
            pickleTree pub b ++
            pickleTree puc c ++
            pickleTree pud d ++
            pickleTree pue e ++
            pickleTree puf f ++
            pickleTree pug g ++
            pickleTree puh h ++
            pickleTree pui i ++
            pickleTree puj j
    }