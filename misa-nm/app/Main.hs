module Main (main) where


import Names

import qualified Data.ByteString as B
import ObjectFile
import Text.Megaparsec (runParser, errorBundlePretty)
import Options.Applicative
import Options.Applicative.Help.Pretty (Doc, vcat, pretty, fillSep)


versionString :: String
versionString = "misa-nm version 0.1.0.0"


progDescString :: String
progDescString =
  "misa-nm lists the symbols from the object files [objfile...]. If no object files are listed " ++
  "as arguments, misa-nm will produce no output."                                                ++
  "\n\n"                                                                                         ++
  "For each symbol in each object file, misa-nm shows:"                                          ++
  "\n\n"                                                                                         ++
  "• The symbol type, indicating whether the symbol is provided or required by the object file " ++
  "during linking:"                                                                              ++
  "\n\n"                                                                                         ++
  "\"<-\" The symbol is defined in and provided by the object file.\n"                           ++
  "\"->\" The symbol is required by the object file for linking."                                ++
  "\n\n"                                                                                         ++
  "• The symbol or relocation address within the object file, in hexadecimal.\n"                 ++
  "• The name of the symbol as specified in the object file.\n"                                  ++
  "• If the symbol is required (->), whether the relocation address is the high or low word of " ++
  "the symbol's value:"                                                                          ++
  "\n\n"                                                                                         ++
  "\"(hi)\" The relocation uses bits 15:8 of the symbol.\n"                                      ++
  "\"(lo)\" The relocation uses bits 7:0 of the symbol."


formattedProgDescString :: Doc
formattedProgDescString = vcat . map (fillSep . map pretty . words) . lines $ progDescString


listNames :: FilePath -> IO ()
listNames inputPath = do
  content <- B.readFile inputPath
  case runParser unpackBinaryObject inputPath content of
    Left  err -> putStrLn (errorBundlePretty err)
    Right obj -> do
      putStrLn ("File " ++ (show inputPath) ++ "\n")
      let names = unlines (map ("  " ++) (lines (formatNames obj)))
      putStrLn names


data Options = Options
  {
    files   :: [FilePath],
    version :: Bool
  }


optionsParser :: Parser Options
optionsParser = Options
  <$> many (argument str (metavar "objfile..."))
  <*> switch (long "version" <>
              short 'V'      <>
              help "Show the misa-nm version and exit")


options :: ParserInfo Options
options = info (optionsParser <**> helper)
  (
    fullDesc <>
    progDescDoc (Just formattedProgDescString) <>
    header "misa-nm - list symbols from MISA object files"
  )


main :: IO ()
main = do
  cliArgs <- execParser options

  if version cliArgs then
    putStrLn versionString
  else
    mapM_ listNames (files cliArgs)
