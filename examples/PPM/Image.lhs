> module PPM.Image where

> import Debug.Trace

> import Data.Bits
> import Data.Char

> import System.Directory
> import System.IO

> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import qualified Text.ParserCombinators.Parsec.Token as Token
> import Text.ParserCombinators.Parsec.Language

> write_ppm :: String -> Int -> Int -> [Double] -> IO ()
> write_ppm outname imax jmax xs =
>     do let filename = outname ++ ".ppm"
>        file <- openBinaryFile filename WriteMode
>        minv <- return $ minimum xs
>        maxv <- return $ maximum xs
>        hPutStr file $ "P6 " ++ (show imax) ++ " " ++ (show jmax) ++ " 255\n"
>        mapM (\x -> writeBW file ((normalise minv maxv x)*255.0)) xs
>        hClose file

> normalise mi ma x = if x<0 then 0 else if x>1 then 1 else x

> writeBW file b = writeRGB file b b b
> writeRGB file r g b = do hPutChar file $ chr $ truncate r
>                          hPutChar file $ chr $ truncate g
>                          hPutChar file $ chr $ truncate b

> read_ppm :: String -> IO (Int, Int, [Double])
> read_ppm outname = 
>     do let filename = outname ++ ".ppm"
>        file <- openBinaryFile filename ReadMode
>        dats <- hGetContents file
>        (x, y, dats) <- parseExpr process dats 
>        hClose file
>        return (x, y, dats)

> process :: Parser (Int, Int, [Double])
> process = do string "P6"
>              char (chr 0x0a)
>              parseUntil (chr 0x0a)
>              x <- natural
>              y <- natural
>              string "255"
>              char (chr 0x0a)
>              dats <- parseDataToBW
>              return (fromInteger x, fromInteger y, dats)

> parseDataToBW :: Parser [Double]
> parseDataToBW = (try $ do r <- anyChar
>                           g <- anyChar
>                           b <- anyChar
>                           x <- return $ (fromIntegral $ (ord r) + (ord g) + (ord b))/(3.0*255.0)
>                           xs <- parseDataToBW
>                           return $ x:xs)
>                 <|> do { many anyChar; eof; return []; }
>                    

> lexer :: Token.TokenParser ()
> lexer = haskell
> natural = Token.natural lexer

> parseUntil :: Char -> Parser ()
> parseUntil c = do c' <- anyChar
>                   if (c==c') then return ()
>                      else parseUntil c


> parseExpr :: Monad m => (Parser a) -> String -> m a
> parseExpr parser input =
>     case (runParser p () "" input) of
>       Left err  -> fail $ show err
>       Right x   -> return x
>   where
>     p = do { pos <- getPosition;
>              setPosition $
>              (flip setSourceName) "" $
>              (flip setSourceLine) 0 $
>              (flip setSourceColumn) 0 $ pos;
>              spaces;
>              x <- parser;
>              return x; }