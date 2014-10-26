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

getIOGnome :: Integer -> IO Gnome
getIOGnome num = do
                        timeString <- getLine
                        let     times = words timeString
                                stTime = (read.(!!0)) times
                                sTime = (read.(!!1)) times
                        return (Gnome num stTime sTime)

main::IO()
main = do
        gnomesCount <- readLn
        let nums = [1..gnomesCount]
        gnomes <- mapM getIOGnome nums
        forM_ (sort gnomes) print