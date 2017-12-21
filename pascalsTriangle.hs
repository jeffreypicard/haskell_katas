import System.Environment  
import System.IO  

main = do
    (nStr:_) <- getArgs  

    let n = read nStr :: Int

    putStrLn $ show $ pascalsTriangle n

pascalsTriangle :: Int -> [Int]
pascalsTriangle n
              | (n <= 0) = []
              | (n == 1) = [1]
              | otherwise = doPascals [1] (n-1) []

doPascals :: [Int] -> Int -> [Int] -> [Int]
doPascals curRow n acc
          | (n == 0) = acc ++ curRow
          | otherwise = doPascals (nextRow curRow) (n-1) (acc ++ curRow)

nextRow :: [Int] -> [Int]
nextRow x = [1] ++ (sumPairs $ everyTwo x) ++ [1]

sumPairs :: [(Int,Int)] -> [Int]
sumPairs q = [x+y | (x,y) <- q]

everyTwo :: [Int] -> [(Int,Int)]
everyTwo x = zip x $ drop 1 x
