import Data.Either
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)

lexer = makeTokenParser javaStyle

expr    = buildExpressionParser table term
        <?> "expression"

term    =  parens lexer expr 
        <|> natural lexer
        <?> "simple expression"

table   =  [[binary "+" (+) AssocLeft]
        , [binary "*" (*) AssocLeft]
        ]

binary  name fun assoc = Infix (do{ reservedOp lexer name; return fun }) assoc

main = do
  contents <- readFile "day18_input"
  let input = lines contents
  print $ sum $ rights $ parse expr "(unknown)" <$> input
