-- This is how we import only a few definitions from Test.Tasty
import Test.Tasty (defaultMain, testGroup, TestTree)
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Log
import LogInstances

-- import everything *except* `main` from LogAnalysis
import LogAnalysis hiding (main)

tests :: TestTree
tests = testGroup "unit tests"
  [ testCase "parseMessage Info"
    ( parseMessage "I 6 Completed armadillo processing" @?=
      LogMessage Info 6 "Completed armadillo processing" )
    -- If you don't have a function called parseMessage, change the
    -- test to match your code.

    , testCase "parseMessage Error 70"
    ( parseMessage "E 70 3 Way too many pickles" @?=
      LogMessage (Error 70) 3 "Way too many pickles" ) 
    
    , testCase "parseMessage Warning"
    ( parseMessage "W 5 Flange is due for a checkup" @?=
      LogMessage Warning 5 "Flange is due for a checkup" )  

    , testCase "parseMessage Unknown"
    ( parseMessage "123456789" @?=
      Unknown "123456789" )

    -- Add at least 3 more test cases for 'parseMessage', including
    -- one with an error, one with a warning, and one with an Unknown

    -- We should also test the smaller parts.  Change the test below
    -- to match the code you actually wrote.
  , testCase "parseMessageType I"
    ( parseMessageType "I 6 Completed armadillo processing"
      @?= Just Info)

    -- Add at least 3 more tests for MessageType parsing in isolation.

    , testCase "parseMessageType W"
    ( parseMessageType "W 5 Flange is due for a checkup"
      @?= Just Warning)

    , testCase "parseMessageType E"
    ( parseMessageType "E 70 3 Way too many pickles"
      @?= Just (Error 70))

    , testCase "parseMessageType None"
    ( parseMessageType "123456789"
      @?= Nothing)

    -- Add tests for timestamp parsing.  Think in particular about
    -- what the function does if the input doesn't start with a digit,
    -- or has some spaces followed by digits.

    , testCase "parseTimeStamp I"
    ( parseTimeStamp "I 6 Completed armadillo processing"
      @?= Just 6)

    , testCase "parseTimeStamp W"
    ( parseTimeStamp "W 5 Flange is due for checkup"
      @?= Just 5)

    , testCase "parseTimeStamp E"
    ( parseTimeStamp "E 70 3 Way too many pickles"
      @?= Just 3)

    , testCase "parseTimeStamp"
    ( parseTimeStamp "123456789"
      @?= Nothing)

    -- How many tests do you think is enough?  Write at least 3
    -- sentences explaining your decision.

    --I think one test for each possible kind of error is enough. If my parse functions can process a single error in a
    --certain format, they can process any error that shares that format. This is true regardless of the specific timestamp or
    --message in any given error.

    -- Write at least 5 tests for 'insert', with sufficiently
    -- different inputs to test most of the cases.  Look at your code
    -- for 'insert', and any bugs you ran into while writing it.

    , testCase "insert unknownMessage"
    ( insert (Unknown "123456789") Leaf
      @?= Leaf)

    , testCase "insert leaf"
    ( insert (LogMessage Info 6 "Completed armadillo processing") Leaf
      @?= Node Leaf (LogMessage Info 6 "Completed armadillo processing") Leaf)

    , testCase "insert leafError"
    ( insert (LogMessage (Error 70) 3 "Way too many pickles") Leaf
      @?= Node Leaf (LogMessage (Error 70) 3 "Way too many pickles") Leaf)

    , testCase "insert greaterTimeStamp"
    ( insert (LogMessage Warning 5 "Flange is due for checkup")  (Node Leaf (LogMessage (Error 70) 3 "Way too many pickles") Leaf)
      @?= Node Leaf (LogMessage (Error 70) 3 "Way too many pickles") (insert (LogMessage Warning 5 "Flange is due for checkup") Leaf))

    , testCase "insert lowerTimeStamp"
    ( insert (LogMessage Warning 5 "Flange is due for checkup") (Node Leaf (LogMessage Info 6 "Completed armadillo processing") Leaf)
      @?= Node (insert (LogMessage Warning 5 "Flange is due for checkup") Leaf) (LogMessage Info 6 "Completed armadillo processing") Leaf)

    -- Next week we'll have the computer write more tests, to help us
    -- be more confident that we've tested all the tricky bits and
    -- edge cases.  There are also tools to make sure that our tests
    -- actually run every line of our code (called "coverage"), but we
    -- won't learn those this year.

    -- Write tests for 'inOrder'.  Remember that the input tree is
    -- meant to already be sorted, so it's fine to only test such
    -- inputs.  You may want to reuse MessageTrees from the tests on
    -- 'insert' above.  You may even want to move them elsewhere in
    -- the file and give them names, to more easiely reuse them.

    --, testCase "inOrder " 

    , testCase "inOrder leaf"
    ( inOrder Leaf
      @?= [])

    , testCase "inOrder node"
    ( inOrder (Node Leaf (LogMessage Warning 5 "Flange is due for a checkup") Leaf)
      @?= inOrder Leaf ++ [LogMessage Warning 5 "Flange is due for a checkup"] ++ inOrder Leaf)

    , testProperty "build sorted"
    (\msgList -> isSorted (inOrder (build msgList)))

    -- show :: Int -> String
    -- gives the String representation of an Int
    -- Use show to test your code to parse Ints

    , testProperty "parse boolMessageType"
    (boolMessageType)

    -- Write a function that takes a MessageType, and makes a String
    -- with the same format as the log file:
    -- stringMessageType :: MessageType -> String
    -- Use this to test your code that parses MessageType
  ]

stringMessageType :: MessageType -> String
stringMessageType Info = "I"
stringMessageType Warning = "W"
stringMessageType (Error i) = "E" ++ (show i)

boolMessageType :: MessageType -> Bool
boolMessageType mt = (parseMessageType (stringMessageType mt))==Just mt

    -- Make another function that makes a String from a whole LogMessage
    -- stringLogMessage :: LogMessage -> String
    -- Use it to test parseMessage

stringLogMessage :: LogMessage -> String
stringLogMessage (LogMessage t ts s) = show(t) ++ show(ts) ++ s
stringLogMessage (Unknown s) = s

main = defaultMain tests
