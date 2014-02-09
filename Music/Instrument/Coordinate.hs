module Music.Instrument.Coordinate where

multiLineTextExample  = unlines $ replicate 10 (replicate 10 'x')
multiLineTextExample'  = unlines $ replicate 3 (replicate 3 'x')

textCooridinateToUnary text (x,y) = gridCoordinateToUnary (map (undefined:) $ lines text) (x,y)
gridCoordinateToUnary grid (x,y) = coordinateToUnary (length (head grid)) (x,y)
coordinateToUnary width (x,y) = (width * y) + x


unaryTextToCoordinate text x = unaryGridToCoordinate (map (undefined:) $ lines text) x
unaryGridToCoordinate grid x = unaryToCoordinate (length (head grid)) x
unaryToCoordinate width x = (m,d)
  where (d,m) = divMod x width

replaceAt i v xs = map (\(x,i') -> if i==i' then v else x) $  zip xs [0..]

replaceAtText (x,y) v text = replaceAt (textCooridinateToUnary text (x,y)) v text
