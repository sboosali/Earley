{-# LANGUAGE RecursiveDo #-}

{-|

stack build :earley-ordered-results && stack exec -- earley-ordered-results

-}

import Control.Applicative
import Text.Earley

data Edit   = Edit Action Slice Region     deriving (Show)
data Action = Copy | Delete | Cut          deriving (Show)
data Slice  = Whole | Backwards | Forwards deriving (Show)
data Region = Char | Word | Line           deriving (Show)

type Text = String

edit :: Grammar r (Prod r String Text Edit)
edit = do

 action <- rule $ "action"
   <=> Copy        <$ token "copy"
   <|> Delete      <$ token "delete"
   <|> Cut         <$ token "cut"

 slice <- rule $ "slice"
   <=> Whole     <$ token "whole"
   <|> Backwards <$ token "backwards"
   <|> Forwards  <$ token "forwards"

 region <- rule $ "region"
   <=> Char <$ token "char"
   <|> Word <$ token "word"
   <|> Line <$ token "line"

 edit' <- rule $ "edit"
   <=> Edit <$> action            <*> (slice -?- Whole) <*> (region -?- Line)
   <|> Edit <$> (action -?- Copy) <*> slice             <*> region
   -- <=> Edit <$> action <*> slice <*> region

 return edit'

data Command
 = Editing Edit
 | Insertion [Text]
 deriving (Show)

command :: Grammar r (Prod r String Text Command)
command = do

  edit' <- edit

  insertion' <- rule $ some anyWord

  command' <- rule $ "command"
    <=> Editing <$> edit'
    <|> Insertion <$> insertion'

  return command'

anyWord :: Prod z String Text Text
anyWord = satisfy (const True)

parseCommand :: [Text] -> ([Command], Report String [Text])
parseCommand s = fullParses (parser command) s

(<=>) :: e -> Prod r e t a -> Prod r e t a
(<=>) = flip (<?>)
infix 2 <=>

(-?-) :: Prod r e t a -> a -> Prod r e t a
(-?-) p x = maybe x id <$> optional p

printParses :: (Show a, Show e, Show ts) => (ts -> ([a], Report e ts)) -> ts -> IO ()
printParses theParser theSentence = do
 let (theResults, theReport) = theParser theSentence
 putStrLn""
 print theSentence
 _ <- traverse print theResults
 print theReport

main :: IO ()
main = do

 putStrLn""
 printParses parseCommand (words "line")
 printParses parseCommand (words "copy")
 printParses parseCommand (words "copy whole")
 printParses parseCommand (words "whole line")
 printParses parseCommand (words "copy whole line")
 printParses parseCommand (words "not an edit")

{-

e.g. given:

          Editing <$> edit
    <|> Insertion <$> insertion

we get:

["whole","line"]
Insertion ["whole","line"]
Editing (Edit Copy Whole Line)

rather than:

["whole","line"]
Editing (Edit Copy Whole Line)
Insertion ["whole","line"]

-}

{- OrderedResults2 outputs:

["line"]
Insertion ["line"]
Report {position = 1, expected = [], unconsumed = []}

["copy"]
Editing (Edit Copy Whole Line)
Insertion ["copy"]
Report {position = 1, expected = ["region","slice"], unconsumed = []}

["copy","whole"]
Editing (Edit Copy Whole Line)
Insertion ["copy","whole"]
Report {position = 2, expected = ["region"], unconsumed = []}

["whole","line"]
Editing (Edit Copy Whole Line)
Insertion ["whole","line"]
Report {position = 2, expected = [], unconsumed = []}

["copy","whole","line"]
Editing (Edit Copy Whole Line)
Editing (Edit Copy Whole Line)
Insertion ["copy","whole","line"]
Report {position = 3, expected = [], unconsumed = []}

["not","an","edit"]
Insertion ["not","an","edit"]
Report {position = 3, expected = [], unconsumed = []}

-}

{- OrderedResults outputs:

["line"]
Insertion ["line"]
Report {position = 1, expected = [], unconsumed = []}

["copy"]
Editing (Edit Copy Whole Line)
Insertion ["copy"]
Report {position = 1, expected = ["region","slice"], unconsumed = []}

["copy","whole"]
Insertion ["copy","whole"]
Editing (Edit Copy Whole Line)
Report {position = 2, expected = ["region"], unconsumed = []}

["whole","line"]
Insertion ["whole","line"]
Editing (Edit Copy Whole Line)
Report {position = 2, expected = [], unconsumed = []}

["copy","whole","line"]
Editing (Edit Copy Whole Line)
Editing (Edit Copy Whole Line)
Insertion ["copy","whole","line"]
Report {position = 3, expected = [], unconsumed = []}

["not","an","edit"]
Insertion ["not","an","edit"]
Report {position = 3, expected = [], unconsumed = []}

-}

{-

master outputs:

["line"]
Insertion ["line"]
Report {position = 1, expected = [], unconsumed = []}

["copy"]
Editing (Edit Copy Whole Line)
Insertion ["copy"]
Report {position = 1, expected = ["region","slice"], unconsumed = []}

["copy","whole"]
Insertion ["copy","whole"]
Editing (Edit Copy Whole Line)
Report {position = 2, expected = ["region"], unconsumed = []}

["whole","line"]
Insertion ["whole","line"]
Editing (Edit Copy Whole Line)
Report {position = 2, expected = [], unconsumed = []}

["copy","whole","line"]
Editing (Edit Copy Whole Line)
Editing (Edit Copy Whole Line)
Insertion ["copy","whole","line"]
Report {position = 3, expected = [], unconsumed = []}

["not","an","edit"]
Insertion ["not","an","edit"]
Report {position = 3, expected = [], unconsumed = []}

-}
