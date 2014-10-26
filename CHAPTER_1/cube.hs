cubeiter guess x 
    | good guess x = guess
    | otherwise = cubeiter im x
        where
           im = improve guess x

good guess x = abs((guess * guess * guess) - x) < 0.001

improve guess x = ((x / (guess * guess)) + (2 * guess)) / 3 

cube x = cubeiter 1.0 x

