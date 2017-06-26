{-# LANGUAGE RecordWildCards #-}


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )

import LexTMLang
import ParTMLang
import SkelTMLang
import PrintTMLang hiding (render,Doc)
import AbsTMLang

import ErrM

import Text.PrettyPrint
import Data.Maybe
import Control.Monad

import Prelude
import Data.List

-- Using http://morphett.info/turing/turing.html
-- Syntax:
-- Each line should contain one tuple of the form '<current state> <current symbol> <new symbol> <direction> <new state>'.
-- You can use any number or word for <current state> and <new state>, eg. 10, a, state1. State labels are case-sensitive.
-- You can use any character for <current symbol> and <new symbol>, or '_' to represent blank (space). Symbols are case-sensitive.
-- <direction> should be 'l', 'r' or '*', denoting 'move left', 'move right' or 'do not move', respectively.
-- Anything after a ';' is a comment and is ignored.
-- The machine halts when it reaches any state starting with 'halt', eg. halt, halt-accept.
-- <current state> <current symbol> <new symbol> <direction> <new state>

type Tape = [TMChar]
type TMChar = Maybe Bool
data TuringMachine = TM { leftTape  :: Tape
                        , curentPosition :: TMChar
                        , rightTape :: Tape
                        } deriving (Eq)
instance Show TuringMachine where
    show = render . ppTM

ppTM :: TuringMachine -> Doc
ppTM TM{..} = text "{" <+> prTape (reverse leftTape) 
          <+> text "[" <> prettyTMChar curentPosition <> text "]" 
          <+> prTape rightTape <+> text "}" 
  where 
    prettyTMChar Nothing = text "_"
    prettyTMChar (Just True) = text "1"
    prettyTMChar (Just False) = text "0"
    prTape = hcat . punctuate (text " ") . map prettyTMChar

moveLeft :: TuringMachine -> TuringMachine
moveLeft (TM [] curentPosition rightTape) = TM [] Nothing (curentPosition:rightTape)
moveLeft (TM (lc:leftTape) curentPosition (rightTape)) = TM leftTape lc (curentPosition:rightTape)

moveRight :: TuringMachine -> TuringMachine
moveRight (TM leftTape curentPosition []) = TM (curentPosition:leftTape) Nothing []
moveRight (TM leftTape curentPosition (rc:rightTape)) = TM (curentPosition:leftTape) rc rightTape

moveStay :: TuringMachine -> TuringMachine
moveStay = id

write2Tm :: TMChar -> TuringMachine -> TuringMachine
write2Tm newChar tm = tm{curentPosition = newChar}

-------Load Program
type ParseFun a = [Token] -> Err a
myLLexer = myLexer
runFile :: (Print a, Show a) => ParseFun a -> FilePath -> IO a
runFile p f = readFile f >>= run p
run :: (Print a, Show a) => ParseFun a -> String -> IO a
run p s = let ts = myLLexer s in case p ts of
           Bad s    -> error "File incorrect"
           Ok  tree -> return tree


-- Run Program

selectInstruction :: Program -> State -> [Instruction]
selectInstruction (Program []) _ = []
selectInstruction (Program instructions) stateID = [ possibleCommand | possibleCommand <- instructions, stateID == getID possibleCommand ]
    
getID (Instruction stID _ _ _ _) = stID

executeInstruction :: Instruction -> TuringMachine -> Maybe (NextCommand, TuringMachine)
executeInstruction (Instruction curState SWild newSymbol direction nextState ) tm = Just (nextState, (exeDirection direction . exeWrite newSymbol) tm)
executeInstruction (Instruction curState desiredSymbol newSymbol direction nextState ) tm 
                 = if symbol2TMChar desiredSymbol == curentPosition tm
                   then Just (nextState, (exeDirection direction . exeWrite newSymbol) tm)
                   else Nothing

exeDirection :: Direction -> TuringMachine -> TuringMachine
exeDirection DStay = moveStay
exeDirection DLeft = moveLeft
exeDirection DRight = moveRight

exeWrite :: Symbol -> TuringMachine -> TuringMachine
exeWrite SWild = id  
exeWrite SBlank = write2Tm Nothing
exeWrite STrue = write2Tm (Just True)
exeWrite SFalse = write2Tm (Just False)

symbol2TMChar :: Symbol -> TMChar
symbol2TMChar SBlank = Nothing
symbol2TMChar STrue = Just True
symbol2TMChar SFalse = Just False
symbol2TMChar _ = error "Not a vaild TM character"

tryAllInstructions :: [Instruction] -> TuringMachine -> (NextCommand, TuringMachine)
tryAllInstructions [] _ = error "No vaild instructions by that name"
tryAllInstructions instructions tm | length nextCommand >= 1 = head nextCommand 
                                   | nextCommand == [] = error "There are to many vaild branches" 
                                   | otherwise = error "Not all paths are covered"
  where
    nextCommand :: [(NextCommand, TuringMachine)]
    nextCommand = catMaybes [ executeInstruction instruction tm | instruction <- instructions ]

getNextCommand :: NextCommand -> State
getNextCommand (NContinue state) = state
getNextCommand _ = error "No command"


-------Examples
tm = (TM [Just True,Just True,Just True] (Just True) [])

fooTm = (moveRight.moveRight.moveLeft. write2Tm (Just False). moveLeft.moveLeft.moveLeft) (TM [] (Just True) [])

compareID id1 id2 = (getID id1) == (getID id2)
mergeCommands (Program loadedProgram) = [ ((state.head) x , x) | x <- (groupBy compareID loadedProgram)]
commands = mergeCommands <$> loadProgram
makeExecutionList (Program loadedProgram) = [ ((state.head) x , tryAllInstructions x) | x <- (groupBy compareID loadedProgram)]
exeList = makeExecutionList <$> loadProgram

exeTM :: State -> [(State, TuringMachine -> (NextCommand, TuringMachine))] -> TuringMachine -> IO ()
exeTM _ [] _ = error "No program loaded"
exeTM startID pro turingMachine = do       
                      putStrLn $ render $ ppTM turingMachine
                      let (nextInstruct,newTM) = head [ executable turingMachine | (nodeID, executable) <- pro , nodeID == startID]
                      if nextInstruct == NAccept || nextInstruct == NReject 
                      then do putStrLn $ render $ ppTM turingMachine
                              putStrLn (show nextInstruct)
                      else exeTM (getNextCommand nextInstruct) pro newTM 

defList = TM [] (Nothing) []

foo = join $ (\x -> exeTM (State (TMIdent "a")) x defList) <$> exeList   



loadProgram :: IO Program
-- loadProgram = runFile pProgram "turingTest.tm"
loadProgram = runFile pProgram "turingTest2.tm"


-- defTm = TM [] (Just True) []

-- main = join $ (\x -> runTMProgram (State (TMIdent "d0")) x defTm) <$> loadProgram  


