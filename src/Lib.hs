module Lib
    ( someFunc
    ) where

import System.IO

fsplit :: Char -> String -> (String, String)
fsplit _ "" = ("", "")
fsplit seperator (c:cs) | c == seperator = ([], cs)
              | otherwise = (c : n, ns) where
                (n, ns) = fsplit seperator cs

split :: Char -> String -> [String]
split _ "" = []
split seperator text = word : split seperator rest where
    (word, rest) = fsplit seperator text

someFunc :: IO ()
someFunc = do 
    putStr "Input some text: "
    hFlush stdout
    inp <- getLine
    print (split ' ' inp)
    return ()


type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) = st

instance Functor ST where
    fmap g st = S (\s -> let (x, s') = app st s in (g x, s'))

instance Applicative ST where
    pure x = S (\s -> (x, s))
    stf <*> stx = S (\s  ->
        let (f, s') = app stf s
            (x, s'') = app stx s' in (f x, s''))

instance Monad ST where
    (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S(\s -> let (x, s') = app st s in app (f x) s')

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving Show

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')


fresh :: ST Int
fresh = S (\n -> (n, n + 1))

alabel (Leaf _) = Leaf <$> fresh
alabel (Node l r) = Node <$> alabel l <*> alabel r
