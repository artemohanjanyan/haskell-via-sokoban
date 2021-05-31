{-# LANGUAGE LambdaCase #-}

module Parser where

import Control.Monad (void)
import Control.Applicative (Alternative (..))
import Data.Bifunctor (first)
import Data.Char (isAlphaNum, isSpace)
import Data.Maybe (catMaybes)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

parse :: Parser a -> String -> Maybe a
parse p input = do
  (res, rest) <- runParser p input
  case rest of
    [] -> Just res
    _ -> Nothing

instance Functor Parser where
  fmap f p = Parser $ \input -> fmap (first f) $ runParser p input

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  ff <*> fx = Parser $ \input -> do
    (f, input') <- runParser ff input
    (x, input'') <- runParser fx input'
    pure (f x, input'')

instance Monad Parser where
  fa >>= f = Parser $ \input -> do
    (a, input') <- runParser fa input
    runParser (f a) input'

instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) a b = Parser $ \input -> runParser a input <|> runParser b input

eof :: Parser ()
eof = Parser $ \case
  [] -> Just ((), [])
  _ -> Nothing

anyChar :: Parser Char
anyChar = Parser $ \case
  [] -> Nothing
  (x:xs) -> Just (x, xs)

char :: Char -> Parser ()
char c = void $ sat (== c)

anyCharBut :: Char -> Parser Char
anyCharBut c = sat (/= c)

sat :: (Char -> Bool) -> Parser Char
sat p = do
  parsedC <- anyChar
  if p parsedC
    then pure parsedC
    else empty

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p sep = ((:) <$> p <*> many (sep *> p)) <|> pure []

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
      char '"'
      content <- many (anyCharBut '"')
      char '"'
      pure content

newtype Identifier = Identifier String deriving Show
newtype Declaration = Declaration (Identifier, String) deriving Show
newtype Section = Section (Identifier, [Declaration]) deriving Show
newtype IniFile = IniFile [Section] deriving Show

parseIni :: Parser IniFile
parseIni = IniFile <$> many section
  where
    identifier = Identifier <$> some (sat isAlphaNum)
    declaration = do
      i <- identifier
      void $ spaces *> char '=' *> spaces
      v <- value
      pure $ Declaration (i, v)
    section = do
      header <- char '[' *> identifier <* char ']' <* br
      let maybeLine =
            Just <$> declaration <|>
            pure Nothing <* comment <|>
            pure Nothing <* blank
      declarations <- catMaybes <$> many (maybeLine <* br)
      pure $ Section (header, declarations)

    spaces = many (sat isSpace)
    value = many (anyCharBut '\n')
    br = char '\n'
    comment = char '#' <* value
    blank = pure ()
