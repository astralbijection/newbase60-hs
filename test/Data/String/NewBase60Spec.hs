module Data.String.NewBase60Spec
  ( spec,
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Functor.Identity (Identity (Identity))
import Data.String.NewBase60
import Test.Hspec

numToSxgCase :: ([Char], Integer) -> SpecWith ()
numToSxgCase (rep, num) =
  it ("converts " ++ show num ++ " to " ++ rep) $
    numToSxg num `shouldBe` rep

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
    foldl (>>) (pure ()) (map numToSxgCase simpleCases)

--let cases = [(22, "N")]
--    y =
--      foldl
--        (>>)
--        (numToSxg 0 `shouldBe` 0)
--        ( map
--            ( \(expected, actual) ->
--                it ("converts " ++ show expected ++ " to " ++ show actual) $ do
--                  numToSxg expected `shouldBe` actual
--            )
--            cases
--        )
-- in y