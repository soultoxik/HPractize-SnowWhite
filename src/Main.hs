{-# LANGUAGE FlexibleInstances, RankNTypes #-}
module Main where
import Data.List
import Control.Monad

data Gnome = Gnome {    number :: Integer,
                        toSleepTime :: Integer,
                        sleepTime :: Integer } deriving (Eq)

instance Ord Gnome where
        compare a b = if sumSleep a > sumSleep b then LT else GT        
                where sumSleep gnome = toSleepTime gnome + sleepTime gnome
                
instance Show Gnome where
        show = show.number

parseGnomeParams :: String -> (Integer, Integer)
parseGnomeParams str = (tsTime, sTime)
                        where   tsTime = read (head splitted) :: Integer
                                sTime = read (splitted !! 1) :: Integer
                                splitted = words str

getGnomes :: forall a. (Eq a, Num a) => Integer -> a -> IO [Gnome]
getGnomes _ 0 = return []
getGnomes acc cnt = do
                        line <- getLine
                        let params = parseGnomeParams line
                            gnome = [uncurry (Gnome acc) params]
                        fmap (gnome ++ ) (getGnomes (acc + 1) (cnt - 1))
                        
getGnomesBase :: Integer -> IO [Gnome]
getGnomesBase = getGnomes 0

main::IO()
main = do
        gnomesCount <- readLn
        gnomes <- getGnomesBase gnomesCount
        let sortedGnomes = sort gnomes
        forM_ sortedGnomes print