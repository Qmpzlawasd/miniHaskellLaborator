module REPLCommand where

import Control.Applicative (Alternative, many, some, (<|>))
import Text.Parsec (Parsec, anyChar)
import Text.Parsec.Language (LanguageDef, emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token
import Text.Parsec.Token (GenLanguageDef (reservedNames))

data REPLCommand
  = Quit
  | Load String
  | Eval String

replDef :: LanguageDef st
replDef =
  emptyDef
    { reservedNames = [":load", ":quit"],
      reservedOpNames = [":l", ":q"]
    }

repl :: TokenParser st
repl = makeTokenParser replDef

-- replLoad :: Parser REPLCommand
-- replLoad = do
--   (reservedOp repl ":q" <|> reserved ":quit")

replQuit :: Parser REPLCommand
replQuit = pure Quit <$> (reservedOp repl ":q" <|> reserved ":quit")

replEval :: Parser REPLCommand
replEval = Eval <$> many anyChar

replLoad :: Parser REPLCommand
replLoad = do
  reservedOp repl ":l" <|> reserved repl ":load"
  s <- many anyChar
  return (Load s)

replCommand :: Parser REPLCommand
replCommand = replQuit <|> replLoad <|> replEval
