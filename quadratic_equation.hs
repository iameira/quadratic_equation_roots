module Main where
import Data.List

countD :: Float -> Float -> Float -> Float 
countD a b c = b*b - 4*a*c

countX :: Float -> Float -> Float -> [[Float]]
countX d a b = do
	if d < 0 then
		return [0/0]
	else
		if d == 0 then do
			let x = ( (-1) * b) / (2 * a)
			return [x, x]
		else do
			let x1 = ( (-1) * b + sqrt(d) ) / (2 * a)
			let x2 = ( (-1) * b - sqrt(d) ) / (2 * a)
			return [x1, x2]


main :: IO ()
main = do
	let a = 1
	let b = 7
	let c = 2

	let d = countD a b c
	putStr $ unlines $ map (unwords . map show) $ countX d a b 
