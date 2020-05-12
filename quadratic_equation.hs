module Main where
import Data.List
import Data.Complex
import Data.Either

countD :: Float -> Float -> Float -> Float
countD 0 0 0 = 0
countD a b c = b * b - 4 * a * c

countComplexSqrtD :: Float -> Complex Float
countComplexSqrtD 0 = (0.0 :+ 0.0)
countComplexSqrtD n = sqrt (n :+ 0.0)

countComplexX :: Float -> Float -> Complex Float -> [Complex Float]
countComplexX a b sd = [x1, x2]
  where x1 = ((-1) * (b :+ 0.0) + sd ) / (2 * (a :+ 0.0))
        x2 = ((-1) * (b :+ 0.0) - sd ) / (2 * (a :+ 0.0))
       

countX :: Float -> Float -> Float -> [Float]
countX 0 0 0 = []
countX a b c | d <  0    = []
             | d == 0    = [x]
             | otherwise = [x1, x2]
  where d  = b * b - 4 * a * c
        x  = ((-1) * b) / (2 * a)
        x1 = ((-1) * b + sqrt(d) ) / (2 * a) 
        x2 = ((-1) * b - sqrt(d) ) / (2 * a)

main :: IO ()
main = do
	let a = 1
	let b = 0
	let c = 1

	let d = countD a b c 
	if d < 0 then 
		let sd = countComplexSqrtD d in
		print $ countComplexX a b sd
	else 
		print $ countX a b c