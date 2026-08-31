{-# LANGUAGE 
      OverloadedLists
#-}
module JavascriptBench.Gigaparsec.Configured.LexerIntCfg where
import Text.Gigaparsec.Token.Patterns (IntegerParserConfig (..), emptyIntegerParserConfig, IntLitBase (Decimal, Hexadecimal, Octal))


jsIntCfg :: IntegerParserConfig
jsIntCfg = emptyIntegerParserConfig {
    bases = [Decimal, Hexadecimal, Octal],
    includeUnbounded = True,
    collatedParser = Just "jsInteger"
  }
