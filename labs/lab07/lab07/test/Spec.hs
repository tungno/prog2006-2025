import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/UsingBonusLib.hs", "app/Main.hs"]