{-# LANGUAGE GADTs #-}

data Typ a where
    TInt    :: Typ Int
    TString :: Typ String
    TPair   :: (Typ a, Typ b) -> Typ (a, b)

to_string :: Typ a -> a -> String 
to_string TInt x = show x 
to_string TString x = "s:" ++ x 
to_string (TPair (x, y)) (tx, ty) = "(" 
            ++ (to_string x tx) 
            ++ "," 
            ++ (to_string y ty) 
            ++ ")"


main :: IO ()
main = do 
    let s = TPair (TInt, TString)
        t = (1, "a")
    print $ to_string s t 
