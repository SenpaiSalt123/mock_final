import           Control.Exception
import           Control.Monad
import           Test.Tasty
import           Common
import           Language.Elsa
import           Data.List (isInfixOf)
import qualified FileSystem            as FS
import qualified Types                 as L
import qualified Logging               as L
import           System.FilePath

main :: IO ()
main = runTests
  [ matryoshka 
  , filesystem
  , logging
  ]

-------------------------------------------------------------------------------
-- | Problem 1 ----------------------------------------------------------------
-------------------------------------------------------------------------------

matryoshka :: Score -> TestTree
matryoshka sc = testGroup "Problem 1"
  [ scoreTest
      (check "Matryoshka.lc")
      "isz_zero"
      True
      1
      "isz_zero"
    , scoreTest
      (check "Matryoshka.lc")
      "isz_one"
      True
      1
      "isz_one"      
    , scoreTest
      (check "Matryoshka.lc")
      "isz_two"
      True
      1
      "isz_two"      
    , scoreTest
      (check "Matryoshka.lc")
      "isz_three"
      True
      1
      "isz_three"
    , scoreTest
      (check "Matryoshka.lc")
      "isz_four"
      True
      1
      "isz_four"
    , scoreTest
      (check "Matryoshka.lc")
      "dec_one"
      True
      1
      "dec_one"
    , scoreTest
      (check "Matryoshka.lc")
      "dec_zero"
      True
      1
      "dec_zero"
    , scoreTest
      (check "Matryoshka.lc")
      "dec_two"
      True
      1
      "dec_two"
    , scoreTest
      (check "Matryoshka.lc")
      "dec_three"
      True
      1
      "dec_three"
    , scoreTest
      (check "Matryoshka.lc")
      "dec_four"
      True
      1
      "dec_four"
    , scoreTest
      (check "Matryoshka.lc")
      "from_church_zero"
      True
      1
      "from_church_zero"
    , scoreTest
      (check "Matryoshka.lc")
      "from_church_one"
      True
      1
      "from_church_one"
    , scoreTest
      (check "Matryoshka.lc")
      "from_church_two"
      True
      1
      "from_church_two"
    , scoreTest
      (check "Matryoshka.lc")
      "from_church_three"
      True
      1
      "from_church_three"
    , scoreTest
      (check "Matryoshka.lc")
      "from_church_four"
      True
      1
      "from_church_four"
    , scoreTest
      (check "Matryoshka.lc")
      "to_church_zero"
      True
      2
      "to_church_zero"
    , scoreTest
      (check "Matryoshka.lc")
      "to_church_one"
      True
      2
      "to_church_one"
    , scoreTest
      (check "Matryoshka.lc")
      "to_church_two"
      True
      2
      "to_church_two"
    , scoreTest
      (check "Matryoshka.lc")
      "to_church_three"
      True
      2
      "to_church_three"
    , scoreTest
      (check "Matryoshka.lc")
      "to_church_four"
      True
      2
      "to_church_four"
  ]
  where
    mkTest :: (Show b, Eq b) => (a -> IO b) -> a -> b -> String -> TestTree
    mkTest = mkTest' sc

    scoreTest :: (Show b, Eq b) => (a -> IO b) -> a -> b -> Int -> String -> TestTree
    scoreTest f x r points name = scoreTest' sc (f, x, r, points, name)

    check :: FilePath -> Id -> IO Bool
    check f x = do
      r <- runElsaId (testDir </> f) x
      return (r == Just (OK (Bind x ())))

    testDir :: FilePath
    testDir = "src"

-------------------------------------------------------------------------------
-- | Problem 2 ----------------------------------------------------------------
-------------------------------------------------------------------------------

filesystem :: Score -> TestTree
filesystem sc = testGroup "Problem 2"
  [ mkTest
      FS.size
      (FS.File "todo" 256)
      256
      "size 1"
  ,  mkTest
      FS.size
      (FS.Dir "haskell-jokes" [])
      0
      "size 2"
  ,  mkTest
      FS.size
      FS.homedir
      4683
      "size 3"
  ,  mkTest
      (uncurry FS.remove)
      (\e -> FS.nameOf e == "Makefile", FS.homedir)
      rm1
      "remove 1"
  ,  mkTest
      (uncurry FS.remove)
      (\e -> FS.nameOf e == "HW1", FS.homedir)
      rm2
      "remove 2"
  ,  mkTest
      (uncurry FS.remove)
      (\e -> FS.nameOf e == "home", FS.homedir)
      FS.homedir
      "remove 3"
  ,  mkTest
      FS.cleanup
      (FS.Dir "temp" [FS.Dir "drafts" [], FS.File "todo" 256])
      (FS.Dir "temp" [FS.File "todo" 256])
      "cleanup 1"
  ,  mkTest
      FS.cleanup
      (FS.File "todo" 256)
      (FS.File "todo" 256)
      "cleanup 2"
  ,  mkTest
      FS.cleanup
      (FS.Dir "drafts" [])
      (FS.Dir "drafts" [])
      "cleanup 3"
  ]
  where
    rm1 = FS.Dir "home"
            [ FS.File "todo" 256
            , FS.Dir  "HW0" []
            , FS.Dir  "HW1" [FS.File "HW1.hs" 3007]
            ]

    rm2 = FS.Dir "home"
            [ FS.File "todo" 256
            , FS.Dir  "HW0" [ FS.File "Makefile" 575 ]
            ]


    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest f = mkTest' sc (return . f)

-------------------------------------------------------------------------------
-- | Problem 3 ----------------------------------------------------------------
-------------------------------------------------------------------------------
logging :: Score -> TestTree
logging sc = testGroup "Problem 3"
  [ scoreTest
      ((return 3 :: L.Logging Int) >>=)
      (\x -> return (x + 5))
      (L.Logging [] 8)
      2
      "do x <- return 3 :: Logging Int ; return x + 5"
  , scoreTest
      (L.log "hello" >>=)
      (\_ -> L.log "goodbye")
      (L.Logging ["hello", "goodbye"] ())
      2
      "do log \"hello\"; log \"goodbye\""
  , scoreTest
      (L.log "blah" >>=)
      (\_ -> return 5)
      (L.Logging ["blah"] 5)
      2
      "do log \"blah\" ; return 5"      
  , scoreTest
      (L.Logging ["hello"] 3 >>=)
      (\x -> L.Logging ["goodbye"] (x + 5))
      (L.Logging ["hello", "goodbye"] 8)
      2
      "do x <- Logging \"hello\" 3; Logging \"goodbye\" (x + 5)"
  , scoreTest
      (L.Logging ["hello", "it's me"] 3 >>=)
      (\x -> L.Logging ["I'm your PhD advisor", "For the next 6 years maybe"] (x + 5))
      (L.Logging ["hello", "it's me", "I'm your PhD advisor", "For the next 6 years maybe"] 8)
      2
      "hello from the ivory tower"

  , scoreTest
      (L.eval [])
      L.e1
      (L.Logging [] (L.VInt 5))
      1
      "2 + 3"
  , scoreTest
      (L.eval [])
      L.e2
      (L.Logging ["I saw 2"] (L.VInt 5))
      1
      "(log (\"I saw 2\") 2) + 3"
  , scoreTest
      (L.eval [])
      L.e3
      (L.Logging ["I saw 2", "I saw 3"] (L.VInt 5))
      1
      "(log (\"I saw 2\") 2) + (log (\"I saw 3\") 3)"
  , scoreTestIO
      L.testString
      "let x = log 'computing x' 1 in let y = log 'computing y' 5 in 2 * y"
      "computing x\ncomputing y\n10\n"
      1
      "let x = log 'computing x' 1 in let y = log 'computing y' 5 in 2 * y"
 , scoreTestIO
      L.testString
      "log 'one' (log 'two' 0)"
      "one\ntwo\n0\n"
      1
      "log 'one' (log 'two' 0)"      
  , scoreTestIO
      L.testFile
      "tests/input/t2.hs"
      "computing x\ncomputing y\n100\n"
      1
      "t2.hs"
  , scoreTestIO
      L.testFile
      "tests/input/t3.hs"
      "computing x\ncomputing y\n0\n"
      1
      "t3.hs"
  , scoreTestIO
      L.testFile
      "tests/input/t4.hs"
      "10\n"
      1
      "t4.hs"
  , scoreTestIO
      L.testFile
      "tests/input/t5.hs"
      "computing y\n11\n"
      1
      "t5.hs"
  , scoreTestIO
      L.testFile
      "tests/input/t6.hs"
      "11\n"
      1
      "t6.hs"

  , scoreTestIO
      L.testStringLazy
      "let x = log 'computing x' 1 in 1"
      "1\n"
      1
      "let x = log 'computing x' 1 in 1"
  , scoreTestIO
      L.testStringLazy
      "let x = log 'computing x' 1 in x + 1"
      "computing x\n2\n"
      1
      "let x = log 'computing x' 1 in x + 1"
  , scoreTestIO
      L.testStringLazy
      "let x = log 'computing x' 1 in let y = x + 1 in 1"
      "1\n"
      1
      "let x = log 'computing x' 1 in let y = x + 1 in 1"
  , scoreTestIO
      L.testFileLazy
      "tests/input/t1.hs"
      "10\n"
      2
      "t1.hs"
  , scoreTestIO
      L.testFileLazy
      "tests/input/t2.hs"
      "computing x\ncomputing y\n100\n"
      2
      "t2.hs"
  , scoreTestIO
      L.testFileLazy
      "tests/input/t3.hs"
      "computing x\ncomputing y\n0\n"
      2
      "t3.hs"
  , scoreTestIO
      L.testFileLazy
      "tests/input/t4.hs"
      "10\n"
      2
      "t4.hs"
  , scoreTestIO
      L.testFileLazy
      "tests/input/t5.hs"
      "11\n"
      2
      "t5.hs"
  , scoreTestIO
      L.testFileLazy
      "tests/input/t6.hs"
      "11\n"
      2
      "t6.hs"
  ]
  where
    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest f = mkTest' sc (return . f)

    scoreTest :: (Show b, Eq b) => (a -> b) -> a -> b -> Int -> String -> TestTree
    scoreTest f x r points name = scoreTest' sc ((\x -> return (f x)), x, r, points, name)

    scoreTestIO :: (Show b, Eq b) => (a -> IO b) -> a -> b -> Int -> String -> TestTree
    scoreTestIO f x r points name = scoreTest' sc (f, x, r, points, name)