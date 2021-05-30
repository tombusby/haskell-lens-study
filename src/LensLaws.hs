module LensLaws (runTests) where

import Control.Lens

data Err
  = ReallyBadError { _msg :: String }
  | ExitCode { _code :: Int }
  deriving (Eq, Show)

runTests :: IO ()
runTests = do
  testErrMsgLensLaws

msg :: Lens' Err String
msg = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    -- Hrmm, I guess we just return ""?
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    -- Nowhere to set it, I guess we do nothing?
    setMsg (ExitCode n) newMessage = ExitCode n

-- This should pass set-get and set-set but fail get-set
msg2 :: Lens' Err String
msg2 = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    -- Hrmm, I guess we just return ""?
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) newMessage = ReallyBadError newMessage
    -- Nowhere to set it, I guess we do nothing?
    setMsg (ExitCode _) newMessage = ReallyBadError newMessage

-- This should fail all laws
msg3 :: Lens' Err String
msg3 = lens getMsg setMsg
  where
    getMsg (ReallyBadError message) = message
    -- Hrmm, I guess we just return ""?
    getMsg (ExitCode _) = ""
    setMsg (ReallyBadError _) _ = ExitCode 123
    -- Nowhere to set it, I guess we do nothing?
    setMsg (ExitCode _) _ = ReallyBadError "foo"

-- get-set law: setting what you get is a NOOP
testGetSet :: Lens' Err String -> Err -> Bool
testGetSet msg' struct = set msg' (view msg' struct) struct == struct

-- set-get law: you get back what you set
testSetGet :: Lens' Err String -> Err -> Bool
testSetGet msg' struct =
  let newVal = "foobar"
  in  view msg' (set msg' newVal struct) == newVal

-- set-set law: setting twice is the same as setting only once (with last)
testSetSet :: Lens' Err String -> Err -> Bool
testSetSet msg' struct =
  let lastVal = "bar"
      lostVal = "foo"
  in  set msg' lastVal (set msg' lostVal struct) == set msg' lastVal struct

testErrMsgLensLaws :: IO ()
testErrMsgLensLaws = do
    let struct1 = ReallyBadError "blah"
        struct2 = ExitCode 1
    putStrLn "get-set law: setting what you get is a NOOP"
    putStrLn "set-get law: you get back what you set"
    putStrLn
      "set-set law: setting twice is the same as setting only once (with last)"
    putStrLn ""

    putStrLn "Tests for msg lens"
    putStrLn $ "ReallyBadError getset: " ++ show (testGetSet msg struct1)
    putStrLn $ "ReallyBadError setget: " ++ show (testSetGet msg struct1)
    putStrLn $ "ReallyBadError setset: " ++ show (testSetSet msg struct1)
    putStrLn $ "ExitCode getset: " ++ show (testGetSet msg struct2)
    putStrLn $ "ExitCode setget: " ++ show (testSetGet msg struct2)
    putStrLn $ "ExitCode setset: " ++ show (testSetSet msg struct2)
    putStrLn ""

    putStrLn "Tests for msg2 lens"
    putStrLn $ "ReallyBadError getset: " ++ show (testGetSet msg2 struct1)
    putStrLn $ "ReallyBadError setget: " ++ show (testSetGet msg2 struct1)
    putStrLn $ "ReallyBadError setset: " ++ show (testSetSet msg2 struct1)
    putStrLn $ "ExitCode getset: " ++ show (testGetSet msg2 struct2)
    putStrLn $ "ExitCode setget: " ++ show (testSetGet msg2 struct2)
    putStrLn $ "ExitCode setset: " ++ show (testSetSet msg2 struct2)
    putStrLn ""

    putStrLn "Tests for msg3 lens"
    putStrLn $ "ReallyBadError getset: " ++ show (testGetSet msg3 struct1)
    putStrLn $ "ReallyBadError setget: " ++ show (testSetGet msg3 struct1)
    putStrLn $ "ReallyBadError setset: " ++ show (testSetSet msg3 struct1)
    putStrLn $ "ExitCode getset: " ++ show (testGetSet msg3 struct2)
    putStrLn $ "ExitCode setget: " ++ show (testSetGet msg3 struct2)
    putStrLn $ "ExitCode setset: " ++ show (testSetSet msg3 struct2)
    putStrLn ""
