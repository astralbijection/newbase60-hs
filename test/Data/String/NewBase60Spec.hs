module Data.String.NewBase60Spec
  ( spec,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor.Identity (Identity (Identity))
import Data.String.NewBase60
import Test.Hspec

simpleCases =
  [ ("N", 22),
    ("H", 17),
    ("10", 60),
    ("NH", 1337),
    ("asc", 129157),
    ("1", 1),
    ("0", 0),
    ("sadfui9fasjf", 1908097676891172549880)
  ]

surjectiveCases =
  [ ("N̷̛͎̝͕͓͙̟̺͎̳̯̙͙̦̋͗͒̀̐̏̅͗̎̕̚͘ͅͅH̴̭̳͉͚͂̀̀̔", 1337),
    ("I", 1),
    ("l", 1),
    ("O", 0),
    ("|", 0),
    (",", 0),
    ("🥺", 0),
    ("a̶̹͓̿̇̈́̔͗͝s̴̒̓̈͌̍̈́̀̐͂͛̑̊̿̑̈͜ͅc̷̢͙͔͈̠͎̱̭̭̏ͅ", 129157)
  ]

spec :: Spec
spec = do
  describe "numToSxg" $
    let numToSxgCase (rep, num) =
          it ("encodes " ++ show num ++ " to " ++ rep) $
            numToSxg num `shouldBe` rep
     in foldl (>>) (pure ()) (map numToSxgCase simpleCases)

  describe "sxgToNum" $
    let sxgToNumCase (rep, num) =
          it ("decodes " ++ rep ++ " to " ++ show num) $
            sxgToNum rep `shouldBe` num
     in foldl (>>) (pure ()) (map sxgToNumCase (simpleCases ++ surjectiveCases))

  it "performs round-trip conversions" $
    let roundTripCase num = (sxgToNum . numToSxg) num `shouldBe` num
     in foldl (>>) (pure ()) (map roundTripCase [0 .. 100000])