{-# LANGUAGE TemplateHaskell #-}
{-#LANGUAGE QuasiQuotes #-}
{-#LANGUAGE KindSignatures #-}
{-#LANGUAGE FlexibleInstances #-}
import Test.Hspec

import Template
import Data.Text(pack)
import qualified Data.Map as M

spec :: Spec
spec = do
  describe "raw" $ do
   it "should parse raw data" $ do
    parseOnly raw (pack "raw data") `shouldBe` (Right $ Raw "raw data")

   it "should keep all spaces" $ do
     parseOnly raw (pack "  2 spaces  ") `shouldBe` (Right $ Raw "  2 spaces  ")

   it "should parse a raw string and ignore the packed one " $ do
     parseOnly raw (pack "raw data{{title}}") `shouldBe` (Right $ Raw "raw data")

     
  describe "packed" $ do
   it "should parse variable" $ do
    parseOnly packed (pack "{{title}}") `shouldBe` (Right $ Var "title" )

   it "should parse an include" $ do
     parseOnly packed (pack "{{include header.html}}") `shouldBe` (Right $ Include "header.html" )
     
   it "should parse a loop" $ do
     parseOnly packed (pack "{{foreach year}}anything") `shouldBe` (Right $ Loop "year")

   it "should trim spaces" $ do
     parseOnly packed (pack "{{include     header.html}}") `shouldBe` (Right $ Include "header.html")


  describe "fragments" $ do
    it "should parse raw and packed data" $
      parseOnly fragments (pack "normal text{{title}}{{include header.html}}{{foreach year}}")
      `shouldBe` (Right $ [Raw "normal text",Var "title",Include "header.html",Loop "year"])
      

  describe "parse" $ do        
    it "should be able to substitute the variable with its value" $ do
      let template = "<b>the title is:</b>{{title}}"
      let context  = M.fromList [("title",StringValue "first title")] 
      let result   = "<b>the title is:</b>first title"
      parse template context `shouldBe` result
        
    it "should be able to embed an include" $ do
      let template = "{{include header.html}}"
      let context  = M.fromList [("header.html",Template "<h1> Page Header </h1>")]
      let result = "<h1> Page Header </h1>"
      parse template context `shouldBe` result

    it "should keep the input intact when it is raw data" $ do
      let template = "<p> raw data </p>"
      let context = M.fromList []
      let result  = "<p> raw data </p>"
      parse template context `shouldBe` result

    it "should be able to substitute foreach data" $ do
      let template = "{{foreach posts}}<p>{{post-title}}</p>{{end}}"
      let posts    = [M.fromList [("post-title",StringValue "first-post-title")]
                     ,M.fromList [("post-title",StringValue "second-post-title")]]
      let context  = M.fromList [("posts", ListValue posts)]
      let result = "<p>first-post-title</p><p>second-post-title</p>"
      parse template context `shouldBe` result 

    
    

     
main :: IO ()
main = hspec spec -- putStrLn "Test suite not yet implemented"
