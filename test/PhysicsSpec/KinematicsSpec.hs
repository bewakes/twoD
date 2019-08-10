module PhysicsSpec.KinematicsSpec
where

import Physics.Kinematics

import Test.Hspec

m1v1 = (2, (0,0))
m2v2 = (2, (1,0))

spec :: Spec
spec = describe "collide" $
        it "collides bodies (2, (0,0)) and (2,(1,0)" $
            -- collide (2, (0,0)) ((2, (1, 0)) `shouldBe` ((0.5, 0), (0.5, 0))
            collide m1v1 m2v2 `shouldBe` ((0.5, 0), (0.5, 0))
