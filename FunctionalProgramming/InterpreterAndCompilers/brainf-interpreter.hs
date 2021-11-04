{-# LANGUAGE NumericUnderscores, OverloadedStrings #-}

import Data.Char     (chr, ord)
import Data.Map      (Map)
import Data.Maybe    (mapMaybe)
import Data.Sequence (Seq(..))
import Data.Text     (Text)

import qualified Data.Map      as Map
import qualified Data.Sequence as Seq
import qualified Data.Text     as Text

data Command = IncrPointer | DecrPointer | IncrByte | DecrByte | OutputChar
             | ReadByte | LoopOpen | LoopClose | EndProgram deriving (Eq, Show)

parseCommand :: Char -> Maybe Command
parseCommand '>' = return IncrPointer
parseCommand '<' = return DecrPointer
parseCommand '+' = return IncrByte
parseCommand '-' = return DecrByte
parseCommand '.' = return OutputChar
parseCommand ',' = return ReadByte
parseCommand '[' = return LoopOpen
parseCommand ']' = return LoopClose
parseCommand _   = Nothing

parseCommands :: String -> Seq Command
parseCommands = Seq.fromList . (++ [EndProgram]) . mapMaybe parseCommand

maxCommands :: Int
maxCommands = 100_000

-- I could have used a monad to keep track of this stuff and chain operations
-- TODO: maybe some day?
data Memory = Memory 
  { memoryContent :: Map Int Int
  , memoryPointer :: Int
  } deriving (Show)

emptyMemory :: Memory
emptyMemory = Memory Map.empty 0

-- a similar function might exist already in Data.Map but I couldn't find it easily
insertOrAdjust :: (Ord k) => (a -> a) -> k -> a -> Map k a -> Map k a
insertOrAdjust f k d mp
  | Map.member k mp = Map.adjust f k mp
  | otherwise       = Map.insert k d mp

writeAtMemory :: (Int -> Int) -> Int -> Memory -> Memory
writeAtMemory f d m =
  m { memoryContent = insertOrAdjust f k d mp }
  where k = memoryPointer m
        mp = memoryContent m

incrMemory, decrMemory :: Memory -> Memory
incrMemory = writeAtMemory (\x -> (x + 1) `mod` 256) 1
decrMemory = writeAtMemory (\x -> (x - 1) `mod` 256) 255

insertInMemory :: Int -> Memory -> Memory
insertInMemory x = writeAtMemory (const x) x

incrPointer, decrPointer :: Memory -> Memory
incrPointer m = m { memoryPointer = memoryPointer m + 1 }
decrPointer m = m { memoryPointer = memoryPointer m - 1 }

readMemory :: Memory -> Int
readMemory m = Map.findWithDefault 0 (memoryPointer m) (memoryContent m)

moveToNextLoopClose :: Seq Command -> Seq Command
moveToNextLoopClose = doMoveToNextLoopClose 0
  where
    doMoveToNextLoopClose :: Int -> Seq Command -> Seq Command
    doMoveToNextLoopClose 0 commands@(LoopClose :<| _) = commands
    doMoveToNextLoopClose n (c :<| cs)
      | c == LoopClose = doMoveToNextLoopClose (n-1) commands'
      | c == LoopOpen  = doMoveToNextLoopClose (n+1) commands'
      | otherwise      = doMoveToNextLoopClose n     commands'
      where commands' = cs Seq.|> c

moveToPreviousLoopOpen :: Seq Command -> Seq Command
moveToPreviousLoopOpen = doMoveToPreviousLoopOpen 0
  where
    doMoveToPreviousLoopOpen :: Int -> Seq Command -> Seq Command  
    doMoveToPreviousLoopOpen 0 commands@(LoopOpen :<| _) = commands
    doMoveToPreviousLoopOpen n (cs :|> c)
      | c == LoopClose = doMoveToPreviousLoopOpen (n+1) commands'
      | c == LoopOpen  = doMoveToPreviousLoopOpen (n-1) commands'
      | otherwise      = doMoveToPreviousLoopOpen n commands'
      where commands' = c Seq.<| cs

interpret :: Text -> Seq Command -> String
interpret entry commands
  | Seq.null commands = error "No commands to run." 
  | otherwise         = doInterpret [] maxCommands emptyMemory entry commands
  where
    doInterpret :: String -> Int -> Memory -> Text -> Seq Command -> String
    doInterpret acc _ _ _ (EndProgram :<| _) = acc
    doInterpret acc 0 _ _ _ =
      acc ++ "\n" ++ "PROCESS TIME OUT. KILLED!!!"
    doInterpret acc remaining memory entry (command :<| commands) =
      case command of
        IncrPointer -> doInterpret acc remaining' (incrPointer memory) entry commands'
        DecrPointer -> doInterpret acc remaining' (decrPointer memory) entry commands'
        IncrByte    -> doInterpret acc remaining' (incrMemory memory) entry commands'
        DecrByte    -> doInterpret acc remaining' (decrMemory memory) entry commands'
        OutputChar  -> 
          let output = [chr (readMemory memory)]
          in doInterpret (acc ++ output) remaining' memory entry commands'
        ReadByte    ->
          -- the entry should never been read when empty according to the problem statement
          let byte   = ord . Text.head $ entry -- so we can do that safely
              entry' = Text.tail entry
          in doInterpret acc remaining' (insertInMemory byte memory) entry' commands'
        LoopOpen    ->
          if readMemory memory == 0
            then doInterpret acc remaining' memory entry (moveToNextLoopClose commands')
            else doInterpret acc remaining' memory entry commands'
        LoopClose   ->
          if readMemory memory /= 0
            then doInterpret acc remaining' memory entry (moveToPreviousLoopOpen commands')
            else doInterpret acc remaining' memory entry commands'
      where remaining' = remaining - 1
            commands'  = commands Seq.|> command

main :: IO ()
main = do
  (l:ls) <- tail . lines <$> getContents
  let entry    = Text.pack . init $ l
      commands = parseCommands . unlines $ ls
  putStrLn $ interpret entry commands