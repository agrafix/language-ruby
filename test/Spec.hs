import Language.Ruby.Parser.Parser

import Control.Monad
import Data.Either
import System.Directory
import System.FilePath
import System.Process
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
                  do let fullPath = "test-data/parser/passing" </> f
                     -- check that the ruby implementation can parse the file
                     callCommand $ "ruby -c " ++ fullPath
                     -- check that our parser can parse it
                     ct <- T.readFile fullPath
                     shouldSatisfy (execParser f parseProgram ct) isRight
