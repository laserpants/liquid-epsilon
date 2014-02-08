{-# LANGUAGE FlexibleInstances #-}
module Util.JSON 
    ( module Control.Applicative
    , FromJSON
    , ToJSON
    , Object
    , Value(Object, Str)
    , parse
    , json
    , fromJSON
    , toJSON
    , mapTo
    , mapFrom
    , obj
    , stringify
    , valueToStr
    , toStr
    , fromStr
    , (.!)
    , (|=)
    ) where

import Control.Applicative      ( (<$>), (<*>) )
import Data.List                ( intersperse )

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) = p 

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 = P $ \cs -> 
    case parse p1 cs of
      [] -> parse p2 cs
      ps -> ps

instance Monad Parser where
    p1 >>= fp2  = P $ \cs -> do 
        (a, cs') <- parse p1 cs 
        parse (fp2 a) cs'
    return x   = P $ \cs -> [(x, cs)]
    fail _     = P $ \_  -> []

instance Functor Parser where
    fmap f p = p >>= \x -> return (f x) 

parseChar :: Parser Char
parseChar = P $ \cs -> case cs of
    (x:xs) -> [(x, xs)]
    []     -> []

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do 
    c <- parseChar
    if p c 
        then return c 
        else fail ""

char :: Char -> Parser Char
char = satisfy . (==) 

space :: Parser Char
space = satisfy (==' ')

chars :: String -> Parser String
chars = mapM char

blank :: Parser Char
blank = satisfy $ flip elem " \n\r" 

-- | Parses zero or more instances of p.
many :: Parser a -> Parser [a]
many p = rest p <|> (return [])

-- | Parses one or more instances of p.
rest :: Parser a -> Parser [a]
rest p = do x  <- p
            xs <- many p
            return (x:xs)

-- | Parses zero or one instance of p.
opt :: Parser a -> Parser [a]
opt p = (p >>= return . return) <|> (return [])

-- | A JSON object is implemented as an association list.
newtype Object = Obj [(String, Value)]

-- | JSON value represented as a Haskell type.
data Value = Object !Object
           | Array  ![Value]
           | Str    !String
           | Number !Double
           | Bool   !Bool
           | Null

object :: Parser Value
object = do
    trim '{'             -- Expect an opening left brace.
    p <- opt pair         -- First pair is not preceded by a comma.
    pairs <- many $ do    -- Zero or more key/value pairs.
        trim ','
        pair  
    trim '}'             -- Closing right brace.
    return $ obj (p ++ pairs)

pair :: Parser (String, Value)
pair = do
    many space
    key <- string'        -- Expect a key (i.e. a string).
    trim ':'             -- Colon, possibly with some surrounding spaces.
    val <- value
    return (key, val)

string :: Parser Value
string = string' >>= return . Str 

string' :: Parser String
string' = do
    many space    -- Strip spaces before the initial quote.
    char '"'      -- Start with a double quote.
    str <- many p  -- Here comes the payload.
    char '"'      -- String needs to end with a quote.
    return str
  where p :: Parser Char
        p = do c <- parseChar
               case c of
                 '"'  -> P $ const []
                 '\\' -> parseChar
                 _    -> return c

trim :: Char -> Parser String
trim c = let spc = many blank in spc >> char c >> spc

array :: Parser Value
array = do
    trim '['              -- An array starts with a left bracket.
    v <- opt value
    values <- many $ do    -- Zero or more key/value pairs.
        trim ','          -- Zero or more comma-separated values.
        value
    trim ']'              -- Closing right bracket.
    return $ Array (v ++ values)

value :: Parser Value
value =  object 
     <|> array 
     <|> string 
     <|> number 
     <|> bool 
     <|> null_

-- Parse a double value by piggybacking on Read.
number :: Parser Value
number = do
    many space
    s <- rest $ satisfy p
    case reads s of
        [(d, "")] -> return $ Number d 
        _         -> P $ const []
  where p :: Char -> Bool
        p c = elem c "-+eE.0123456789"

bool :: Parser Value
bool =  (chars "true"  >> (return $ Bool True))
    <|> (chars "false" >> (return $ Bool False))

null_ :: Parser Value
null_ = chars "null" >> return Null

json :: Parser Value
json = object <|> array

-- FromJSON

class FromJSON a where
    fromJSON :: Value -> Maybe a

instance FromJSON Int where
    fromJSON (Number v) = Just $ truncate v
    fromJSON _ = Nothing

instance FromJSON Integer where
    fromJSON (Number v) = Just $ truncate v
    fromJSON _ = Nothing

instance FromJSON Double where
    fromJSON (Number v) = Just v
    fromJSON _ = Nothing

instance FromJSON Float where
    fromJSON (Number v) = Just $ realToFrac v
    fromJSON _ = Nothing

instance FromJSON Bool where
    fromJSON (Bool v) = Just v
    fromJSON _ = Nothing

instance FromJSON Char where
    fromJSON (Str [v]) = Just v
    fromJSON _ = Nothing

mapFrom :: (FromJSON a) => Value -> Maybe [a]
mapFrom (Array xs) = sequence $ fmap fromJSON xs
mapFrom _ = Nothing

instance FromJSON [Int] where
    fromJSON = mapFrom 

instance FromJSON [Integer] where
    fromJSON = mapFrom 

instance FromJSON [Double] where
    fromJSON = mapFrom 

instance FromJSON [Float] where
    fromJSON = mapFrom 

instance FromJSON [Bool] where
    fromJSON = mapFrom 

instance FromJSON [Char] where
    fromJSON (Str v) = Just v
    fromJSON _ = Nothing

(.!) :: (FromJSON a) => Object -> String -> Maybe a
(Obj obj) .! key = 
    case lookup key obj of
      Just v  -> fromJSON v
      Nothing -> Nothing

-- ToJSON

class ToJSON a where
    toJSON :: a -> Value

instance ToJSON Int where
    toJSON = Number . fromIntegral

instance ToJSON Integer where
    toJSON = Number . fromIntegral

instance ToJSON Double where
    toJSON = Number

instance ToJSON Float where
    toJSON = Number . realToFrac

instance ToJSON Bool where
    toJSON = Bool

instance ToJSON Char where
    toJSON c = Str [c]

mapTo :: (ToJSON a) => [a] -> Value
mapTo = Array . map toJSON

instance ToJSON [Int] where
    toJSON = mapTo

instance ToJSON [Integer] where
    toJSON = mapTo

instance ToJSON [Double] where
    toJSON = mapTo

instance ToJSON [Float] where
    toJSON = mapTo

instance ToJSON [Bool] where
    toJSON = mapTo

instance ToJSON [Char] where
    toJSON = Str 

obj :: [(String, Value)] -> Value
obj = Object . Obj 

(|=) :: ToJSON a => String -> a -> (String, Value)
s |= v = (s, toJSON v) 

-- Stringify
 
stringify :: Object -> String
stringify (Obj o) = "{" ++ sprinkle pairToStr o ++ "}"

pairToStr :: (String, Value) -> String
pairToStr (key, val) = valueToStr (Str key) 
                     ++ ":" ++ valueToStr val

valueToStr :: Value -> String
valueToStr (Object o)   = stringify o
valueToStr (Array vs)   = "[" ++ sprinkle valueToStr vs ++ "]"
valueToStr (Str str)    = "\"" ++ str ++ "\""
valueToStr (Number n)   = show n
valueToStr (Bool True)  = "true"
valueToStr (Bool False) = "false"
valueToStr Null         = "null"

sprinkle :: (a -> String) -> [a] -> String
sprinkle f = concat . intersperse "," . map f

toStr :: (ToJSON a) => a -> String
toStr = valueToStr . toJSON

fromStr :: (FromJSON a) => String -> Maybe a
fromStr s = case parse json s of
              []       -> Nothing
              [(a, _)] -> fromJSON a

