> {-# LANGUAGE DeriveDataTypeable #-}
> {-# LANGUAGE QuasiQuotes #-}

> module Ypnos.Expr.Expr where

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import qualified Text.ParserCombinators.Parsec.Token as Token
> import Text.ParserCombinators.Parsec.Language
> import Language.Haskell.TH

> import Data.Generics
> import Data.Either

Parser

Data structures
===============

> type Var = String
> type HaskExp = String

> data VarP' = PatternVar Var | PatternBlank
>    deriving (Show, Data, Typeable)

> data VarP = Cursor VarP' | NonCursor VarP'
>    deriving (Show, Data, Typeable)

> data DimTag = X | Y | Z | T
>    deriving (Show, Data, Typeable)

> data GridPattern = 
>    GridPattern1D DimTag [VarP] |
>    GridPattern2D DimTag DimTag[[VarP]] 
>    deriving (Show, Data, Typeable)

> data GridFun = GridFun GridPattern String
>    deriving (Show, Data, Typeable)

> data DimVector = MkDimVector [(String, Integer)]
>    deriving (Show, Data, Typeable)

> data GridDef =
>     MkGridDef String DimVector String
>    deriving (Show, Data, Typeable)

General parser combinators
==========================

> parens =  Token.parens lexer
> maybeParens x = parens (maybeParens x) <|> x
> constructor = do { c <- upper; cs <- many idchar; return (c:cs) }
> word = (many1 letter) <|> string "_"
> small = lower <|> char '_'
> idchar = small <|> upper <|> digit <|> char '\''
> ident  =  do{ c <- small ; cs <- many idchar; return (c:cs) }
> natural = Token.natural lexer

> tillEndOfLine = do { eof;
>                      return "" }
>                 <|>
>                 do { c <- anyChar;
>                      if (c=='\n') then return ""
>                        else do cs <- tillEndOfLine
>                                return (c:cs) }


Parse boundaries
================

> data SubRegionDescriptor = Inner Var | Negative Int | Positive Int
>     deriving (Show, Data, Typeable, Eq)

> type RegionDescriptor = [SubRegionDescriptor]

> data BoundaryCase =   Range RegionDescriptor RegionDescriptor HaskExp
>                      | Specific RegionDescriptor HaskExp
>                      | Parameterised RegionDescriptor Var HaskExp
>                        
>     deriving (Show, Data, Typeable)

> data BoundaryDef = BoundaryDef String [BoundaryCase] 
>     deriving (Show, Data, Typeable)

> subRegionDescriptor :: Parser SubRegionDescriptor
> subRegionDescriptor = (do char '+'
>                           n <- natural
>                           return $ Positive $ fromInteger n) <|>
>                       (do char '-'
>                           n <- natural
>                           return $ Negative $ fromInteger n) <|> 
>                       (do char '*'
>                           v <- ident
>                           return $ Inner v)

> regionDescriptor :: Parser RegionDescriptor
> regionDescriptor = try ((do sr <- subRegionDescriptor
>                             return [sr])) <|>
>                    (do char '('
>                        srs <- sepBy (subRegionDescriptor) (do {spaces; char ','; spaces}) 
>                        char ')'
>                        return srs)

> boundaryCase :: Parser BoundaryCase
> boundaryCase = (try 
>                 (do spaces
>                     i1 <- regionDescriptor
>                     spaces
>                     x <- (try (do var <- ident
>                                   spaces
>                                   string "->"
>                                   exp <- tillEndOfLine
>                                   return $ Parameterised i1 var exp)) <|>
>                             (do string "->"
>                                 exp <- tillEndOfLine
>                                 return $ Specific i1 exp)
>                     return x)) <|>
>                  (do spaces
>                      string "from"
>                      spaces
>                      i1 <- regionDescriptor
>                      spaces
>                      string "to"
>                      spaces
>                      i2 <- regionDescriptor
>                      spaces
>                      string "->"
>                      exp <- tillEndOfLine
>                      return $ Range i1 i2 exp)
                     
> parseBoundaryDef :: Parser BoundaryDef
> parseBoundaryDef = do elementType <- constructor
>                       cases <- many boundaryCase 
>                       eof                        
>                       return $ BoundaryDef elementType cases                       


Parse grid definitions (OLD - to be removed)
======================

> parseDimVector :: Parser DimVector
> parseDimVector = do pairs <- sepBy parseDimSize (do {char ','; spaces;})
>                     return $ MkDimVector pairs
  
> parseDimSize :: Parser (String, Integer)
> parseDimSize = do dimId <- many1 upper
>                   char '='
>                   size <- natural
>                   return (dimId, size)

> parseGridDef :: Parser GridDef
> parseGridDef = do --before <- manyTill anyChar (try (string "<"))
>                   spaces
>                   string "grid"
>                   spaces
>                   string "<"
>                   vec <- parseDimVector
>                   string ">"
>                   after <- manyTill anyChar(try eof)
>                   return $ MkGridDef undefined vec after


Parse grid patterns
===================

> varPattern :: Parser VarP
> varPattern = try $ do {
>                      string "@";
>                      p <- word;
>                      return (case p of
>                              "_" -> Cursor (PatternBlank)
>                              x -> Cursor (PatternVar x));
>                } <|>
>              do {
>                p <- word;
>                return (case p of
>                        "_" -> NonCursor (PatternBlank)
>                        x -> NonCursor (PatternVar x));
>               }

> spaces' :: Parser ()
> spaces' = skipMany (char ' ' <|> char '\t')

> elementsPattern :: Parser [VarP]
> elementsPattern = do spaces;
>                      string "|";
>                      spaces;
>                      ps <- manyTill (do {v <- varPattern; spaces; return v}) (string "|");
>                      spaces';
>                      return ps;

> gridPattern :: Parser GridPattern
> gridPattern = do { d1 <- dim;
>                    do {
>                      string "*";
>                      d2 <- dim;
>                      string ":";
>                      pss <- elementsPattern `sepBy` newline;
>                      return (GridPattern2D d1 d2 pss);
>                    } <|>
>                    do {
>                      string ":";
>                      ps <- elementsPattern;
>                       return (GridPattern1D d1 ps);
>                    }}


> dim :: Parser DimTag
> dim = do { string "X"; return X; } <|>
>       do { string "Y"; return Y; } <|>
>       do { string "Z"; return Z; } <?> "X, Y, or Z as dimensions"

Parse grid functions
====================

> gridFun :: Parser GridFun 
> gridFun = do  spaces             
>               g <- gridPattern <?> "grid pattern"
>               spaces
>               string "->"
>               spaces
>               body <- (many anyChar)
>               spaces
>               eof
>               return (GridFun g body)

Lex and parse routines
======================

> lexer :: Token.TokenParser ()
> lexer = haskell

> parseExpr :: Monad m => (Parser a) -> (String, Int, Int) -> String -> m a
> parseExpr parser (file, line, col) input =
>     case (runParser p () "" input) of
>       Left err  -> fail $ show err
>       Right x   -> return x
>   where
>     p = do { pos <- getPosition;
>              setPosition $
>              (flip setSourceName) file $
>              (flip setSourceLine) line $
>              (flip setSourceColumn) col $ pos;
>              spaces;
>              x <- parser;
>              return x; }

> debug :: Show a => Parser a -> String -> IO ()
> debug p input = 
>     case (parse p' "" input) of
>          Left err -> do { putStr "parse error at "; print err }
>          Right x -> print x
>     where p' = do { spaces;
>                     x <- p;
>                     return x }

> debug2 :: Show a => Parser a -> String -> Maybe a
> debug2 p input = 
>     case (parse p' "" input) of
>          Left err -> Nothing
>          Right x -> Just x
>     where p' = do { spaces;
>                     x <- p;
>                     return x }
