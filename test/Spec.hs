import Language.Ruby.Parser.Parser

import Control.Monad
import Data.Either
import System.Directory
import System.FilePath
import Test.Hspec
import qualified Data.Text.IO as T

main :: IO ()
main =
    hspec $
    do describe "Parser passes" $
           do rbFiles <-
                  runIO $
                  filter (\f -> takeExtension f == ".rb") <$>
                  getDirectoryContents "test-data/parser/passing"
              forM_ rbFiles $ \f ->
                  it ("Parses " ++ show f) $
                  do ct <- T.readFile ("test-data/parser/passing" </> f)
                     shouldSatisfy (execParser f parseProgram ct) isRight
