-- import qualified turtle
import System.Environment
import System.IO
-- import Data.Dynamic

main :: IO ()
main = do
    args <- getArgs
    let path = head args
    fileSize <- getFileSize path
    let (reducedSize,unitIndex) = reduceFileSize fileSize 0
    if length args == 1
    then do
        putStrLn $ show (reducedSize) ++ " " ++ (fileUnitFromIndex unitIndex)
    else putStrLn "Error: Invalid amount of arguments.\n$ prvw <filepath>"

getFileSize :: String -> IO Integer
getFileSize path = withFile path ReadMode hFileSize

reduceFileSize :: Integer->Integer->(Integer, Integer)
reduceFileSize size unitIndex
    | size > 1000 = reduceFileSize (size `quot` 1000) (unitIndex + 1)
    | otherwise = (size, unitIndex)

fileUnitFromIndex :: Integer -> String
fileUnitFromIndex suffixIndex
    | suffixIndex == 0 = "B"
    | suffixIndex == 1 = "KB"
    | suffixIndex == 2 = "MB"
    | suffixIndex == 3 = "GB"
    | suffixIndex == 4 = "TB"
    | suffixIndex == 5 = "PB"
    | suffixIndex == 6 = "EB"
    | suffixIndex == 7 = "ZB"
    | suffixIndex == 8 = "YB"
    | otherwise = "UNIT ERROR"