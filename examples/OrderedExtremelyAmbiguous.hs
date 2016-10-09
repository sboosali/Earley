{-# LANGUAGE ScopedTypeVariables, RecursiveDo, PostfixOperators, TemplateHaskell, LambdaCase, OverloadedStrings, OverloadedLists, NoMonomorphismRestriction #-}

{-|

stack build :earley-ordered-extremely-ambiguous && stack exec -- earley-ordered-extremely-ambiguous

-}

import Text.Earley
import qualified Data.Text as T
import Data.Text (Text)

import Control.Applicative
import GHC.Exts (IsString (..),IsList (..))
import Data.Char
import Control.Arrow ((>>>))

--------------------------------------------------------------------------------

main = do
  printParsedPhrase "camel start date"

printParsedPhrase :: Text -> IO ()
printParsedPhrase = printParses parsePhrase . T.words

printParses :: (Show a, Show e, Show ts) => (ts -> ([a], Report e ts)) -> ts -> IO ()
printParses theParser theSentence = do
 let (theResults, theReport) = theParser theSentence
 putStrLn""
 print theSentence
 _ <- traverse print theResults
 print theReport

--------------------------------------------------------------------------------

type G r a = Grammar r (P r a)

type P r = Prod r String Text

(-:) = (,)

(<=>) :: (IsString e) => String -> Prod r e t a -> Grammar r (Prod r e t a)
(<=>) l r = rule $ r <?> fromString l 
infix 2 <=>

-- | zero or one.
(-?) :: Prod r e t a -> Prod r e t (Maybe a)
(-?) = optional
{-# INLINEABLE (-?) #-}

-- | zero or one.
(-?-) :: Prod r e t a -> a -> Prod r e t a
(-?-) p x = maybe x id <$> optional p
{-# INLINEABLE (-?-) #-}

-- | zero or more.
(-*) :: Prod r e t a -> Prod r e t [a]
(-*) = many
{-# INLINEABLE (-*) #-}

-- -- | one or more.
-- (-+) :: Prod r e t a -> Prod r e t (NonEmpty a)
-- (-+) = Some id
-- {-# INLINEABLE (-+) #-}

-- | one or more ("downcasted" to a list).
(-++) :: Prod r e t a -> Prod r e t [a]
(-++) = some
{-# INLINEABLE (-++) #-}

-- | choice. a heterogeneous binary 'Alter'.
(-|-) :: Prod r e t a -> Prod r e t b -> Prod r e t (Either a b)
(-|-) l r = Left <$> l <|> Right <$> r
{-# INLINEABLE (-|-) #-}

vocab :: (IsString t, Show t, Eq t) => [(String, a)] -> Prod r e t a
vocab
 = foldMap (\(s,x) -> x <$ token (fromString s))

anyWords :: Prod r e t [t]
anyWords = some anyWord

anyWord :: Prod r e t t
anyWord = terminal Just

anyLetter :: Prod r String Text Text
anyLetter = (satisfy (T.all isUpper)) <?> "letter"

anyLetters :: Prod r String Text Text
anyLetters = T.concat <$> some (satisfy isSingleLetter) <?> "letters"
 where
 isSingleLetter = T.uncons >>> \case
  Nothing -> False
  Just (c, _) -> isAlphaNum c

--------------------------------------------------------------------------------

newtype Phrase = Phrase [Phrase_]
 deriving(Show,Read,Eq,Ord)

instance IsString Phrase where
 fromString = word2phrase

data Phrase_
 = Escaped_  Keyword    -- ^ atom-like (wrt 'Sexp').
 | Quoted_   Dictation  -- ^ list-like.
 | Pasted_              -- ^ atom-like. the clipboard contents, with munging.
 | Clipboard_           -- ^ atom-like. the clipboard contents, without munging.
 | Blank_               -- ^ atom-like.
 | Separated_ Separator -- ^ like a "close paren".
 | Bonked_              -- ^ like a "close paren".
 | Cased_      Casing   -- ^ function-like (/ "open paren").
 | Joined_     Joiner   -- ^ function-like (/ "open paren").
 | Surrounded_ Brackets -- ^ function-like (/ "open paren").
 | Splitted_ Splitter   -- ^ function-like (/ "open paren").
 | Spelled_  [Char]     -- ^ atom-like.
 | Capped_   [Char]     -- ^ atom-like.
 | Symbol_   [Char]     -- ^ atom-like.
 | Dictated_ Dictation  -- ^ list-like.
 deriving (Show,Read,Eq,Ord)

instance IsString Phrase_ where
 fromString = word2phrase_

newtype Dictation = Dictation [String]
  deriving(Show,Read,Eq,Ord)

data Casing = UpperCase | LowerCase | CapCase deriving (Show,Read,Eq,Ord,Enum,Bounded)

data Joiner = Joiner String | CamelJoiner | ClassJoiner | ShrinkJoiner deriving (Show,Read,Eq,Ord)

data Brackets = Brackets String String deriving (Show,Read,Eq,Ord)

data Splitter = Splitter deriving (Show,Read,Eq,Ord)

newtype Separator = Separator String  deriving (Show,Read,Eq,Ord)

newtype Keyword = Keyword String  deriving (Show,Read,Eq,Ord)

newtype Letters = Letters [Char] deriving (Show,Read,Eq,Ord)

instance IsString Letters where fromString = Letters

--------------------------------------------------------------------------------

-- | e.g. bracket \'|\'
bracket :: Char -> Brackets
bracket c = Brackets [c] [c]

word2phrase_ :: String -> Phrase_
word2phrase_ = Dictated_ . Dictation . (:[])

word2phrase :: String -> Phrase
word2phrase = fromPhrase_ . word2phrase_

fromPhrase_ :: Phrase_ -> Phrase
fromPhrase_ = Phrase . (:[])

letters2dictation :: Letters -> Dictation
letters2dictation (Letters cs) = Dictation (map (:[]) cs)

--------------------------------------------------------------------------------

parsePhrase :: [Text] -> ([Phrase], Report String [Text])
parsePhrase s = fullParses (parser phrase) s

phrase :: G r Phrase
phrase = mdo

  _phrase :: P r Phrase <- "phrase" <=> Phrase <$>
   ((\xs x -> xs ++ [x]) <$> ((phraseA <|> phraseB <|> phraseW)-*) <*> (phraseB <|> phraseC <|> phraseD))

  -- | a sub-phrase where a phrase to the right is certain.
  --
  -- this ordering prioritizes the escaping Escaped_/Quoted_ over the
  -- escaped, e.g. "quote greater equal unquote".
  phraseA :: P r Phrase_ <- "phraseA" <=> empty
   <|> pasted
   <|> Blank_      <$ "blank"
   -- <|> Spelled_    <$ "spell" # (character-++)
   -- <|> Spelled_    <$ "lets" # letters -- (letter-++)
   <|> Bonked_     <$  "smack"   -- (like saying "break" enough times)
   <|> Separated_  <$> separator
   <|> Cased_      <$> casing
   <|> Joined_     <$> joiner
   <|> Surrounded_ <$> brackets
   <|> Splitted_   <$> splitter

  -- | a sub-phrase where a phrase to the right is possible.
  phraseB :: P r Phrase_ <- "phraseB" <=> empty
   <|> Escaped_  <$ "litter"            <*> keyword         -- abbreviation for "literally"
   <|> Quoted_   <$ "quote"             <*> dictation <* "unquote"

   -- <|> Spelled_  <$ ("let's" <|> "let") <*> (character-++)  -- conflicts with "let" in Haskell
   <|> Spelled_  <$ "let's" <*> (character-++)  -- abbreviation for "letters"
   <|> Capped_   <$ "caps"              <*> (character-++)  -- abbreviation for "capital letters"
   <|> Capped_   <$ "shrimp"            <*> (character-++)  -- abbreviation for "symbol", that's frequent

   <|> pasted
   <|> Blank_     <$ "blank"

   <|> Spelled_  <$> (phoneticAlphabet-++)  -- last, since it's unprefixed

  -- | a sub-phrase where a phrase to the right is impossible.
  phraseC :: P r Phrase_ <- "phraseC" <=> Dictated_ <$ "say" <*> dictation

  -- | injects word_ into phrase
  phraseW :: P r Phrase_ <- "phraseW" <=> word2phrase_ <$> word_

  -- | injects dictation into phrase_
  phraseD :: P r Phrase_ <- "phraseD" <=> Dictated_ <$> dictation

  pasted :: P r Phrase_ <- "pasted" <=> empty
   <|> Pasted_     <$ "pasted"    -- "yank"
   <|> Clipboard_  <$ "clip"    --

  separator :: P r Separator <- "separator" <=> empty
   <|> Separator ""  <$ "break" --TODO separation should depend on context i.e. blank between symbols, a space between words, space after a comma but not before it. i.e. the choice is delayed until munging.
   <|> Separator " " <$ "space"
   <|> Separator "," <$ "comma"

  casing :: P r Casing <- "casing" <=> empty
   <|> LowerCase <$ "lower"
   <|> UpperCase <$ "upper"
   <|> CapCase   <$ "copper"              -- "capper"

  joiner :: P r Joiner <- "joiner" <=> empty
   <|> (\c -> Joiner [c]) <$ "join" <*> character
   <|> Joiner "_" <$ "snake"
   <|> Joiner "-" <$ "dash"
   -- <|> Joiner "/" <$ "file"
   <|> Joiner ""  <$ "squeeze"
   <|> CamelJoiner <$ "camel"    -- "cam"
   <|> ClassJoiner <$ "class"
   <|> ShrinkJoiner <$ "shrink"  -- "shrink plug-in" -> "plugin"

  brackets :: P r Brackets <- "brackets" <=> empty
   <|> bracket          <$ "round" <*> character
   <|> Brackets "(" ")" <$ "par"
   <|> Brackets "[" "]" <$ "square"
   <|> Brackets "{" "}" <$ "curl"
   <|> Brackets "<" ">" <$ "angle"
   <|> bracket '"'      <$ "string"
   <|> bracket '\''     <$ "ticked"
   <|> bracket '|'      <$ "norm"
   -- <|> Brackets "**" "**" <$ "bold"

  splitter :: P r Splitter <- "splitter" <=> empty
   <|> Splitter <$ "split"

  character :: P r Char <- "character" <=> empty
   <|> punctuation
   <|> englishNumeric
   <|> literalNumeric
   <|> phoneticAlphabet

  englishNumeric :: P r Char <- "englishNumeric" <=> vocab
   [ "zero"-: '0'
   , "one"-: '1'
   , "two"-: '2'
   ]

  literalNumeric :: P r Char <- "literalNumeric" <=> vocab
   [ "0"-: '0'
   , "1"-: '1'
   , "2"-: '2'
   ]

  phoneticAlphabet :: P r Char <- "phoneticAlphabet" <=> vocab
   [ "alpha"-: 'a'
   , "bravo"-: 'b'
   , "charlie"-: 'c'
   ]

  literalAlphabet :: P r Char <- "literalAlphabet" <=> vocab
   [ "a"-: 'a'
   , "b"-: 'b'
   , "c"-: 'c'
   ]

  punctuation :: P r Char <- "punctuation" <=> vocab
   [ "grave"-: '`'
   , "till"-: '~'
   , "bang"-: '!'
   , "axe"-: '@'
   , "pound"-: '#'
   , "doll"-: '$'
   , "purse"-: '%'
   , "care"-: '^'
   , "amp"-: '&'
   , "star"-: '*'
   , "lore"-: '('
   , "roar"-: ')'
   , "hit"-: '-'                  -- during a Phrase,Dragon recognizes "dash" literally as "-"
   , "score"-: '_'
   , "equal"-: '='
   , "plus"-: '+'
   , "lack"-: '['
   , "lace"-: '{'
   , "rack"-: ']'
   , "race"-: '}'
   , "stroke"-: '\\'
   , "pipe"-: '|'
   , "semi"-: ';'
   , "coal"-: ':'
   , "tick"-: '\''
   , "quote"-: '"'
   , "com"-: ','
   , "less"-: '<'
   , "dot"-: '.'
   , "great"-: '>'
   , "slash"-: '/'
   , "quest"-: '?'
   , "tab"-: '\t'
   , "ace"-: ' '
   , "ret"-: '\n'  -- "line" conflicts with (Line :: Region)
   ]

  dictation :: P r Dictation <- "dictation" <=> (Dictation . fmap T.unpack) <$> anyWords

  word_ :: P r String <- "word_" <=> (T.unpack) <$> anyWord

  letters :: P r Letters <- "letters" <=> (Letters . T.unpack) <$> anyLetters

  keyword :: P r Keyword <- "keyword" <=> (Keyword . T.unpack) <$> empty

  return _phrase

--------------------------------------------------------------------------------
