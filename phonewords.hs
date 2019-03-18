{-
    This program will build on dictionary.hs and wordsToPhone from a previous
    assignment. You can copy your wordsToPhone source code here or you can simply
    include the line:
    
    import PTfuncsyntax
    
    and run this program in the same directory with your PFfuncsyntax.hs file.
    
    This program will ask the user to enter a 4-digit number. It will then list 
    off all of the english words that can be formed from that number on a standard 
    telephone keypad.
    
    Example of use:
    
    *Main> main
    Type a four-digit number:
    2376
    "Afro"
    "Bern"
    "berm"
    *Main> 

-}

charToPhoneDigit :: Char -> Int
charToPhoneDigit character
  | character `elem` "abcABC" = 2
  | character `elem` "defDEF" = 3
  | character `elem` "ghiGHI" = 4
  | character `elem` "jklJKL" = 5
  | character `elem` "mnoMNO" = 6
  | character `elem` "pqrsPQRS" = 7
  | character `elem` "tuvTUV" = 8
  | character `elem` "wxyzWXYZ" = 9
  | otherwise = 0
  
numListToNum :: [Int] -> Int
numListToNum nums = read (inAString nums)
inAString [] = []
inAString nums = show (head nums) ++ inAString (tail nums)

wordsToPhone :: String -> Int
wordsToPhonelist w = [charToPhoneDigit(x)|x<-w]
wordsToPhone w = numListToNum (wordsToPhonelist w)

main = do 
    putStrLn ("Type a four-digit number: ")
    num <- readLn
    dictio <- readFile "/usr/share/dict/american-english"
    let dict = words dictio
    mapM_ (putStrLn) [show x|x<-dict, wordsToPhone(x) == num]
    
