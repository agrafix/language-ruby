import Language.Ruby.Interpreter.Eval
import Language.Ruby.Parser.AST
import Language.Ruby.Parser.Parser

import Control.Monad
import Data.Either
import System.Directory
import System.FilePath
import System.Process
import Test.Hspec
import qualified Data.Text as T
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
                     let ast = execParser f parseProgram ct
                     print ast
                     shouldSatisfy ast isRight
       describe "Interpreter passes" $
           do rbFiles <-
                  runIO $
                  filter (\f -> takeExtension f == ".rb") <$>
                  getDirectoryContents "test-data/interpreter/passing"
              forM_ rbFiles $ \f ->
                  it ("Parses " ++ show f) $
                  do let fullPath = "test-data/interpreter/passing" </> f
                     -- check that the ruby implementation can run it.
                     -- TODO: get the actual output
                     callCommand $ "ruby " ++ fullPath
                     -- get our output
                     ct <- T.readFile fullPath
                     let ast = execParser f parseProgram ct
                     case ast of
                       Left err -> fail (T.unpack err)
                       Right prog ->
                           do result <- runProgram prog
                              result `shouldBe` Right LVoid
                              -- TODO: compare outputs...
