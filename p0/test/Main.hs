module Main (main) where

import Data.List (foldl', sort)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import Hedgehog.Internal.Range (lowerBound)
import Hedgehog.Internal.Show (Value (Char))
import qualified Hedgehog.Range as Range
import MyLib
import Test.Tasty
import Test.Tasty.HUnit hiding (assert)
import Test.Tasty.Hedgehog


main :: IO ()
main = defaultMain tests


prop_rev :: Property
prop_rev = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
  rev (rev xs) === xs


prop_opaque :: Property
prop_opaque = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 10) Gen.alpha
  let op = Opaque xs
  op /== op


prop_div10000 :: Property
prop_div10000 = property $ do
  x <- forAll $ Gen.int (Range.linear 1 100000)
  x `mod` 10000 /== 0


tests :: TestTree
tests =
  testGroup
    "All Tests"
    [ testGroup
        "Unit Tests"
        [ testCase "3*5 == 15" $ 3 * 5 @?= 15
        , testCase "2*2 == 4" $ 4 @=? 2 * 2
        , testCase "rev []" $ rev [] @?= ([] :: [Int])
        , testCase "rev [1,2,3]" $ rev [1, 2, 3] @?= [3, 2, 1]
        ]
      , testProperty "reverse works" $ prop_rev
      , testProperty "strange opaque value" prop_opaque
      , testProperty "all numbers do not divide 10000" $ prop_div10000
      , treeTests
      , zipLongTests
    ]


zipLongTests :: TestTree
zipLongTests =
  testGroup
    "ZipLong function tests"
    [ 
      testCase "zipLong as zip" $ zipLong [1, 2, 3] "abc" @?= [(1, 'a'), (2, 'b'), (3, 'c')]
    , testCase "not equal arguments length" $ zipLong [1, 2] "abcd" @?= [(1, 'a'), (2, 'b'), (1, 'c'), (2, 'd')]
    ]


treeTests :: TestTree
treeTests =
  testGroup
    "Tree Tests"
    [ 
      traversalTests
    , insertTests
    , rotationTests
    ]


traversalTests :: TestTree
traversalTests = testGroup "traversal"
  [ testCase "empty" $ traversal empty @?= ([] :: [Int])
  , testCase "single elt" $ traversal (Node Nothing 1 Nothing) @?= [1]
  , testCase "three elts" $
    traversal (Node (Just $ leaf 1) 2 (Just $ leaf 3)) @?= [1,2,3]
  ]

isCorrect :: (Ord a) => Tree a -> Bool
isCorrect Empty = True
isCorrect (Node ml v mr) =
  maybe True isCorrect ml
    && maybe True isCorrect mr
    && all (< v) (maybe [] traversal ml)
    && all (>= v) (maybe [] traversal mr)


-- Поменять генератор двоичных деревьев поиска так,
-- чтобы задавался диапазон элементов дерева и
-- в левом и в правом поддереве генерировались только
-- нужные элементы (чтобы BST было корректным)
arbitraryTree :: Size -> Int -> Int -> Gen (Tree Int)
arbitraryTree 0 _ _ = pure empty
arbitraryTree size a b = do
  leftSize <- Size <$> Gen.int (Range.linear 0 $ unSize size - 1)
  let rightSize = size - leftSize - 1
  v <- Gen.int $ Range.linear a b
  l <- if leftSize == 0
       then pure Nothing
       else Just <$> arbitraryTree leftSize a (v - 1)
  r <- if rightSize == 0
       then pure Nothing
       else Just <$> arbitraryTree rightSize (v + 1) b
  pure $ Node l v r


-- treeGen :: Gen (Tree Int)
-- treeGen = Gen.sized arbitraryTree


prop_bst :: Property
prop_bst = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 30) $ Gen.int (Range.linear 0 1000)
  let t = foldl' (\tree elt -> insert elt tree) empty xs
  assert $ isCorrect t


prop_inserts :: Property
prop_inserts = property $ do
  xs <- forAll $ Gen.list (Range.linear 0 30) $ Gen.int (Range.linear 0 1000)
  let t = foldl' (\tree elt -> insert elt tree) empty xs
  sort xs === traversal t


insertTests :: TestTree
insertTests = testGroup "insert"
  [ testProperty "BST is correct" prop_bst
  , testProperty "Insert really inserts" prop_inserts
  , testCase "1" $ isCorrect (leaf 1) @? "leaf 1"
  , testCase "3" $
    isCorrect (Node (Just $ leaf 1) 2 (Just $ leaf 2)) @? "tree 3"
  ]


prop_rotate_left :: Property
prop_rotate_left = property $ do
    tree <- forAll $ arbitraryTree 5 100 10000
    isCorrect (rotateLeft tree) === True


prop_rotate_right :: Property
prop_rotate_right = property $ do
    tree <- forAll $ arbitraryTree 5 100 10000
    isCorrect (rotateRight tree) === True


rotationTests :: TestTree
rotationTests =
  testGroup
    "rotations functions tests"
    [ 
      testProperty "left rotation" prop_rotate_left
    , testProperty "right rotation" prop_rotate_right
    ]
