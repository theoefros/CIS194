import Log

-- Parse an individual message
parseMessage :: String -> LogMessage
parseMessage m = 
  case mesType of "I" -> LogMessage Info st ms
                  "W" -> LogMessage Warning st ms
                  "E" -> LogMessage (Error st) et  (unwords $ drop 3 tokens)
                  _   -> Unknown m
  where tokens = words m
        mesType = head tokens
        st = read (head $ drop 1 tokens)
        ms = (unwords $ drop 2 tokens)
        et = read (head $ drop 2 tokens)

-- Parse entire logfile (file is given as a string)
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Inserts a new LogMessage into an existing MessageTree, producing a new
-- MessageTree.
-- insert assumes that it is given a sorted MessageTree, and must produce a 
-- new sorted MessageTree containing the new LogMessage in addition to the 
-- contents of the original MessageTree. However, note that if insert is given a 
-- LogMessage which is Unknown, it should return the MessageTree unchanged.
-- A MessageTree should 
-- be sorted by timestamp: that is, the timestamp of a LogMessage in any Node 
-- should be greater than all timestamps of any LogMessage in the left subtree, 
-- and less than all timestamps of any LogMessage in the right child. 
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert msg@(LogMessage _ mTime _) (Node left tmsg@(LogMessage _ tTime _) right)
    | mTime < tTime = Node (insert msg left) tmsg right
    | mTime > tTime = Node left tmsg (insert msg right)

-- Builds a messagetree from a list of messages
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Creates a sorted list of messages from messagetree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l msg@(LogMessage _ t _) r) = inOrder l ++ [msg] ++ inOrder r

--Write a function
  --whatWentWrong :: [LogMessage] -> [String]
--which takes an unsorted list of LogMessages, and returns a list of the messages
--corresponding to any errors with a severity of 50 or greater, sorted by 
--timestamp
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getLogMessage . filter isSevere . inOrder . build . filter isErr
    where isErr (LogMessage (Error _) _ _) = True
          isErr _ = False
          isSevere (LogMessage (Error s) _ _) = s >= 50

whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' = map getLogMessage . filter isSevere . inOrder . build . filter isErr
    where isErr (LogMessage (Error _) _ _) = True
          isErr _ = False
          isSevere (LogMessage (Error s) _ _) = s < 50
          
whatInfo :: [LogMessage] -> [String]
whatInfo = map getLogMessage . inOrder . build . filter isInfo
    where isInfo (LogMessage Info _ _) = True
          isInfo _ = False
whatWarning :: [LogMessage] -> [String]
whatWarning = map getLogMessage . inOrder . build . filter isWarning
    where isWarning (LogMessage Warning _ _) = True
          isWarning _ = False

whatUnknown :: [LogMessage] -> [String]
whatUnknown = map getLogMessage . inOrder . build . filter isUnknown
    where isUnknown (Unknown _) = True
          isUnknown _ = False

getLogMessage :: LogMessage -> String
getLogMessage (LogMessage _ _ m) = m
getLogMessage (Unknown m) = m

getLogMessage' :: LogMessage -> String
getLogMessage' (LogMessage Info _ m) = "I " ++ m
getLogMessage' (LogMessage Warning _ m) = "W " ++  m
getLogMessage' (LogMessage (Error _) _ m) = "E " ++ m
getLogMessage' (Unknown m) = "U " ++ m