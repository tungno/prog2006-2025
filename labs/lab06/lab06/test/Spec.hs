-- lab06/test/Spec.hs
import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "app/Main.hs"]
