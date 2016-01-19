import qualified Data.Char as C
import Control.Monad
import Control.Applicative

newtype Parser a = P (String -> [(a, String)])

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
   pure v = P (\inp -> [(v, inp)])
   (<*>) = ap

instance Alternative Parser where
  (<|>) = mplus
  empty = mzero

instance Monad Parser where
   return = pure
   p >>= f = P (\inp -> case parse p inp of
                       [] -> []
                       [(v, out)] -> parse (f v) out)

instance MonadPlus Parser where
  mzero = P (\inp -> [])
  p `mplus` q = P (\inp -> case parse p inp of
                        [(v, out)] -> [(v, out)]
                        otherwise -> parse q inp) -- fallback on q

-- | Combined parser, first tries parser p and if that fails tries q
p +++ q = p `mplus` q

-- | Applies a parser to an input string
parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

-- | Parser that parses one characted,
-- always succeds if input is non empty, fails on the empty string
item :: Parser Char
item = P (\inp -> case inp of
               [] -> []
               (x:xs) -> [(x, xs)])

-- | Parser that always fails
failure :: Parser a
failure = mzero

pmany :: Parser a -> Parser [a]
pmany p = pmany1 p +++ return []

pmany1 :: Parser a -> Parser [a]
pmany1 p = do v <- p
              vs <- pmany p
              return (v:vs)

-- | Parser that parses a char satisfying a predicate
sat :: (Char -> Bool) -> Parser Char
sat pred = do i <- item
              if pred i then return i else failure

int :: Parser Integer
int = do numString <- pmany1 (sat C.isDigit)
         return (read numString)

char :: Char -> Parser Char
char c = sat (c ==)

pr = 1000000007

-- | Computes x^m % p
powMod x 0 p = 1
powMod x 1 p = x
powMod x m p = let r = rem m 2
                   tmp = powMod x (m `div` 2) p
                   sub = (tmp * tmp) `mod` p
                in if r == 0
                      then sub
                      else ((sub * x) `mod` p)

mulOrDiv :: Parser (Integer -> Integer -> Integer)
mulOrDiv = do c <- item
              case c of
                  '*' -> return (\x y -> (x * y) `mod` pr)
                  '/' -> return (\x y -> (x * (powMod y (pr - 2) pr)) `mod` pr)
                  _ -> failure

addOrSub :: Parser (Integer -> Integer -> Integer)
addOrSub = do c <- item
              case c of
                  '+' -> return (\x y -> (x + y) `mod` pr)
                  '-' -> return (\x y -> (x - y + pr) `mod` pr)
                  _ -> failure


negFactor :: Parser Integer
negFactor = do char '-'
               x <- factor
               return (((-x `mod` pr) + pr) `mod` pr)

posFactor :: Parser Integer
posFactor = do char '+'
               factor

parenthesisExpr :: Parser Integer
parenthesisExpr = do char '('
                     x <- expression
                     char ')'
                     return (x `mod` pr)

intPr = do i <- int
           return (i `mod` pr)

factor :: Parser Integer
factor = intPr +++ negFactor +++ posFactor +++ parenthesisExpr


term :: Parser Integer
term = do f1 <- factor
          do op <- mulOrDiv
             f2 <- term
             return (op f1 f2)
           +++ return f1


expression :: Parser Integer
expression = do t1 <- term
                do op <- addOrSub
                   t2 <- expression
                   return (op t1 t2)
                 +++ return t1


solve :: String -> Integer
solve inp = let [(v, _)] = parse expression inp
             in v

removeBlanks xs = filter (\x -> not (C.isSpace x)) xs

main = do expr <- removeBlanks <$> getContents
          let r = solve expr
          print r
