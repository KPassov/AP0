import CurvySyntax
import CurveAST

testStrings :: [String]
testStrings = ["c = (4.0,3)", -- Testing Curve, indent and tests the number can recognise doubles with and without decimals
               "a = a", -- Indent = ID Indent, this should be recognised
               "penis123 = (4.2 + 4.2 * 0.0,1.0 * 2.0 + 0.0)", -- Testing presedence inside a Curve and strings as Indents
               "c = (4,3) ++ (5,3)      rot 4", -- Define and rotate a curve (checking presedence)
               "c = (4,3) d = (5,4) e =      (5,6)" -- Multiple Defs
              ]

failStrings :: [String]
failStrings = ["where = (4.0,2.0)", -- where is not a valid indent
               "(4.9,2.1)", -- trying to define a curve without Ident 
               "a = (3.2,5.1", -- parser error
               "a = (4.3,2.4) ++ 5 + 2" -- type error
              ]

resultStrings :: [Either Error Program]
resultStrings = [Right [Def "c" (Single (Point (Const 4.0) (Const 3.0))) []],
                 Right [Def "a" (Id "a") []],
                 Right [Def "penis123" (Single (Point (Add (Const 4.2) (Mult (Const 4.2) (Const 0.0))) 
                                                      (Add (Mult (Const 1.0) (Const 2.0)) (Const 0.0)))) []],
                 Right [Def "c" (Connect (Single (Point (Const 4.0) (Const 3.0))) (Rot (Single (Point (Const 5.0) (Const 3.0))) (Const 4.0))) []],
                 Right [Def "c" (Single (Point (Const 4.0) (Const 3.0))) [],Def "d" (Single (Point (Const 5.0) (Const 4.0))) [],Def "e" (Single (Point (Const 5.0) (Const 6.0))) []]
                ]


test :: Bool
test = foldl (\ x y -> x && y) True list 
      where list = (runTests testStrings resultStrings) ++ 
                   (runTests failStrings $ take 4 $ repeat $ Left "parse error")

runTests :: [String] -> [Either Error Program] -> [Bool]
runTests tests results = zipWith (\x y -> (parseString x) == y) tests results
