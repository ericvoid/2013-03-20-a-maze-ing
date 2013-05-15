module Main (main) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Maze
import qualified Data.Map as M
import Test.QuickCheck

instance Arbitrary Direction where
  arbitrary = arbitraryBoundedEnum


main :: IO ()
main = hspec $ do
  describe "Maze" $ do
    it "compiles!" (return () :: IO ())

  describe "is inside bounds" $ do
    it "is inside bounds" $ do
      isInsideBounds (empty (1, 1)) (0, 0) `shouldBe` True

    it "is inside bounds 2" $ do
      isInsideBounds (empty (3, 3)) (1, 1) `shouldBe` True

    it "is inside bounds 3" $ do
      isInsideBounds (empty (10, 10)) (9, 9) `shouldBe` True

    prop "is not inside X bound" $ 
      \s -> isInsideBounds (empty s) (-1, 0) == False

    prop "is not inside Y bound" $ 
      \s -> isInsideBounds (empty s) (0, -1) == False

    prop "is not inside X bound 2" $ 
      \s -> isInsideBounds (empty s) (fst s, snd s - 1) == False

    prop "is not inside Y bound 2" $ 
      \s -> isInsideBounds (empty s) (fst s - 1, snd s) == False


  describe "move" $ do
    it "can move north" $ do
      move N (1, 1) `shouldBe` (1, 0)

    prop "going direction and going back  leads to the same place" $ 
      \p d -> move (opposite d) (move d p) == p


  describe "canMoveTo " $ do
    prop "cat't move outside bounds" $
      \d -> isMovingOutsideBounds (empty (1, 1)) (0, 0) d == True

    it "can not move South " $ do
      canMoveTo (empty (1, 1)) (0, 0) S `shouldBe` False

    it "can move South " $ do
      canMoveTo (empty (2, 2)) (0, 0) S `shouldBe` True

    prop "can move " $ 
      \d -> canMoveTo (empty (3, 3)) (1, 1) d == True


  describe "can insert walls " $ do
    it "cant move into a wall" $ do
      canMoveTo (insertWall (empty (2,2)) (0, 0) S) (0, 0) S `shouldBe` False
      
    prop "cant move into random walls" $ 
      \p d -> canMoveTo (insertWall (empty (fst p + 2, snd p + 2)) p d) p d == False
