module Main (main) where

{- Calculates a list of all permutations of an input list -}
perms :: [a] -> [[a]]
perms [e] = [[e]]
perms l = concatMap (\(h,t) -> map (h:) (perms t)) (picks l)
    where picks [] = [] {- All the ways to pick a first item from a list -}
          picks (h:t) = (h,t) : map (\(e,r) -> (e,h:r)) (picks t)

{- Performs close to zero computation from NS puzzle -}
calc :: Num a => [a] -> a
calc [a,b,c,d,e,f,g,h,i,j] = (a*100 + b*10 + c) * (d*10 + e) - ((f*100 + g*10 + h) * (i*10+j))

main :: IO ()
main = do
    let zeros = filter (\p -> calc p == 0) (perms [0..9])
    print zeros
    print (length zeros)
