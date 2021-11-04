{-# LANGUAGE OverloadedStrings #-}

import Data.Char     (chr, ord)
import Data.Map      (Map)
import Data.Maybe    (mapMaybe)
import Data.Sequence (Seq(..))
import Data.Text     (Text)
import qualified Data.Map      as Map
import qualified Data.Sequence as Seq
import qualified Data.Text     as Text

data Command = IncrPointer | DecrPointer | IncrByte | DecrByte | OutputChar
             | ReadByte | LoopOpen | LoopClose | EndProgram deriving (Show)

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
maxCommands = 100000

interpret :: Text -> Seq Command -> String
interpret entry commands
  | Seq.null commands = error "No commands to run." 
  | otherwise         = doInterpret [] maxCommands emptyMemory entry commands

-- This is might be transformed into a monad
data Memory = Memory 
  { memoryContent :: Map Int Int
  , memoryPointer :: Int
  }

emptyMemory :: Memory
emptyMemory = Memory Map.empty 0

-- A similar function might exist already in Data.Map but I couldn't find it easily
insertOrAdjust :: (Ord k) => (a -> a) -> k -> a -> Map k a -> Map k a
insertOrAdjust f k d m
  | Map.member k m = Map.adjust f k m
  | otherwise      = Map.insert k d m

incrMemory, decrMemory :: Memory -> Memory
incrMemory m = m { memoryContent = content' }
  where k = memoryPointer m
        content = memoryContent m
        content' = insertOrAdjust (\x -> (x + 1) `mod` 256) k 1 content
decrMemory m = m { memoryContent = content' }
  where k = memoryPointer m
        content = memoryContent m
        content' = insertOrAdjust (\x -> (x - 1) `mod` 256) k 255 content

incrPointer, decrPointer :: Memory -> Memory
incrPointer m = m { memoryPointer = memoryPointer m + 1 }
decrPointer m = m { memoryPointer = memoryPointer m - 1 }

readMemory :: Memory -> Int
readMemory m = Map.findWithDefault 0 (memoryPointer m) (memoryContent m)

insertInMemory :: Memory -> Int -> Memory
insertInMemory m x =
  m { memoryContent = Map.insert (memoryPointer m) x (memoryContent m) }

moveToNextLoopClose :: Seq Command -> Seq Command
moveToNextLoopClose commands@(LoopClose :<| _) = commands
moveToNextLoopClose commands@(c :<| cs) = moveToNextLoopClose (cs Seq.|> c)

moveToPreviousLoopOpen :: Seq Command -> Seq Command
moveToPreviousLoopOpen commands@(LoopOpen :<| _) = commands
moveToPreviousLoopOpen commands@(cs :|> c) = moveToPreviousLoopOpen (c Seq.<| cs)

doInterpret :: String -> Int -> Memory -> Text -> Seq Command -> String
doInterpret acc 0 _ _ _ =
  acc ++ "\n" ++ "PROCESS TIME OUT. KILLED!!!"
doInterpret acc remainingCommands memory entry (command :<| commands) = do
  case command of
    IncrPointer -> doInterpret acc remaining' (incrPointer memory) entry commands'
    DecrPointer -> doInterpret acc remaining' (decrPointer memory) entry commands'
    IncrByte    -> doInterpret acc remaining' (incrMemory memory) entry commands'
    DecrByte    -> doInterpret acc remaining' (decrMemory memory) entry commands'
    OutputChar  -> 
      let char = [chr (readMemory memory)]
      in doInterpret (acc ++ char) remaining' memory entry commands'
    ReadByte    ->
      if Text.null entry then error "The whole entry has been consumed already."
      else
        let byte   = ord . Text.head $ entry
            entry' = Text.tail entry
        in doInterpret acc remaining' (insertInMemory memory byte) entry' commands'
    LoopOpen    ->
      if readMemory memory == 0
        then doInterpret acc remaining' memory entry (moveToNextLoopClose commands')
        else doInterpret acc remaining' memory entry commands'
    LoopClose   ->
      if readMemory memory /= 0
        then doInterpret acc remaining' memory entry (moveToPreviousLoopOpen commands')
        else doInterpret acc remaining' memory entry commands'
    EndProgram  -> acc
  where remaining' = remainingCommands - 1
        commands'  = commands Seq.|> command

main :: IO ()
main = do
  (l:ls) <- tail . lines <$> getContents
  let entry = Text.pack . init $ l
      commands = parseCommands . unlines $ ls
  putStrLn $ interpret entry commands