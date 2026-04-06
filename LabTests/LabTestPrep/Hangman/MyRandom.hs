module MyRandom(RandGen,getRandGen,nextRandom,randomInRange) where

import COSC4P41
                     
type Seed           = Long
type RandomFunction = Long -> Double

newtype RandGen     = R (Seed,RandomFunction)

myRandF :: RandomFunction
myRandF x = let a = x * 15485863 in fromIntegral (a*a `mod` 2038074743) / 2038074743

createRandGen :: Long -> RandGen
createRandGen seed = R (seed,myRandF)

getRandGen :: IO RandGen
getRandGen = do seed <- nanosSinceEpoch
                return $ createRandGen seed

nextRandom :: RandGen -> (Double,RandGen)
nextRandom (R (seed,randF)) = (randF seed,R (seed+1,randF))

randomInRange :: Enum a => (a,a) -> RandGen -> (Int,RandGen) 
randomInRange (min,max) gen = (toEnum $ floor (d*(maxInt-minInt+1)+minInt),nGen)
    where (d,nGen) = nextRandom gen
          minInt   = fromIntegral $ fromEnum min
          maxInt   = fromIntegral $ fromEnum max