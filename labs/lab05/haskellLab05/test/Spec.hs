-- test/Spec.hs
import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Task2.hs", "src/Task3.hs", "src/Task4.hs", "src/Bonus.hs"]