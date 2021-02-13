{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-} -- simplifies \ x -> case x of to just \ case
module Language.Lexer
( LexerError(..), Token(..)
, lexer )
where
  import Eve
  import Language.Markdown.Tokens ( MarkdownToken(..), JSONToken(..) )

  import Control.Applicative ( Alternative(..), liftA2 )
  import Control.Monad.Except ( ExceptT, MonadError (throwError), runExceptT )
  import Control.Monad.State ( State, gets, modify', runState )
  import Data.Char ( isAlphaNum, isDigit, isLower, isSpace, isUpper )
  import Data.List ( foldl', foldl1' )
  import Data.Maybe ( listToMaybe, maybeToList )

  data LexerError
    = Unexpected Char
    | UnexpectedEOF
    deriving ( Eq, Show )

  -- If there's no input left, and we're in an unexpected situation, then we
  -- weren't expecting the input to end so early, and should produce an
  -- UnexpectedEOF error, otherwise seeing whatever character is at the front of
  -- our input was the problem
  unexpected :: String -> LexerError
  unexpected [] = UnexpectedEOF
  unexpected (c:_) = Unexpected c

  -- So a Lexer is nothing more than a wrapper around a function taking in some
  -- input, and either failing with a LexerError, or producing a value of type a,
  -- and the remaining string that we have yet to consume.
  newtype Lexer a = Lexer { runLexer :: String -> Either LexerError (a, String) }

  instance Functor Lexer where
    -- fmap :: (a -> b) -> Lexer a -> Lexer b
    -- Functor Laws Proof
    --   fmap id                  = id
    --   Given: `first id         ≡ id` (from Data.Bifunctor)
    --   fmap id (Lexer l)        = Lexer (l >>> fmap (first id)) = Lexer (l >>> id)
    --                            = Lexer l
    --   fmap (f . g)             = fmap f . fmap g
    -- Since Lexer is a wrapper around a function, use the output of the function
    --   as the target of the `fmap`. Since the output is a tuple (when successful),
    --   use the `Data.Bifunctor.first` to manipulate it.
    fmap f (Lexer l) = Lexer (l >>> fmap (first f))

  instance Applicative Lexer where
    -- pure :: a -> f a
    pure a = Lexer (\input -> Right (a, input))
    -- (<*>) :: f (a -> b) -> f a -> f b
    -- Makes Lexer composable, using the example from the blog post, a lexer
    --   matching "A" composed with a Lexer matching "B" should match "AB". With
    --   the example in the blog post, `(,) <$> char 'A' <*> char 'B'`, `fmap`
    --   `(,)` into `char 'A'`, before applying the output of `char 'B'`, which
    --   would match `(A,B)`.
    Lexer f <*> Lexer l = Lexer <| \input -> do
      -- f is of type `(a -> b)` while rest is of type String
      (f', rest) <- f input
      -- Run the next Lexer with unlexed string
      (a, s)     <- l rest
      -- Fix the composition of `a -> b` with `f'`
      return (f' a, s)

  -- For <|>, we pattern match on both the results given the same input, at the same
  -- time. If one of them fails, we rely on the other result. If both succeed, we need
  -- to dive in a bit more, and compare the lengths of the remaining inputs. We pick
  -- the result that has less input remaining, i.e. that has consumed more input,
  -- i.e. that is the longest match. On ties, when the remaining inputs have equal
  -- length, we still choose a. This means that we're biased towards the left lexer,
  -- as we talked about earlier.
  instance Alternative Lexer where
    -- empty :: f a
    -- Should work such that `empty <|> f == f`, where `f` is the instance derived
    --   from `Applicative f => Alternative f`, or equivalently the right handed
    --   operator of the infix.
    empty = Lexer (Left <<< unexpected)
    -- (<|>) :: f a -> f a -> f a
    -- As explained above, since it's using a greedy policy, the `Lexer` used is
    --   the one which consumed the most characters, which only occurs if both
    --   `Lexer`s have an output (`Right`), while if one of the `Lexer`s have an
    --   error (`Left`), use the one that has an output.
    Lexer l1 <|> Lexer l2 = Lexer <| \input -> case (l1 input, l2 input) of
      (res, Left _) -> res
      (Left _, res) -> res
      (a@(Right (_, restA)), b@(Right (_, restB))) ->
        if length restA <= length restB
          then a
          else b

  satisfies :: (Char -> Bool) -> Lexer Char
  satisfies pred = Lexer <| \case
    c : cs | pred c -> Right (c, cs)
    rest            -> Left (unexpected rest)

  char :: Char -> Lexer Char
  char target = satisfies (== target)

  string :: String -> Lexer String -- or Lexer [Char]
  string = traverse char
  -- traverse :: Applicative f => (a -> f b) -> [a] -> f [b]
  -- This uses the very cool traverse, which in our case, first makes a bunch of
  -- lexers recognizing each respective character in a given string, and then
  -- sequences them to recognize the entire string, one character at a time.

  data Token
    = Header String
    | BlockQuote String
    | BoldText String | ItalicText String | BoldItalicText String | Text String
    deriving ( Eq, Show )

  oneOf :: Alternative f => [f a] -> f a
  oneOf = foldl1' (<|>)

  -- token :: Lexer (Token, String)
  -- token = layout <|> text
  --   where
  --     -- layout :: Lexer (Token, String)
  --     layout = header <|> blockquote
  --       where
  --         -- header :: Lexer (Token, String)
  --         header = Header <$> (many (satisfies (=='#')) <*> satisfies (==' ') *> many (satisfies (/='\n')))
  --         -- blockquote :: Lexer (Token, String)
  --         blockquote = BlockQuote <$> (many (satisfies (=='>')) <*> satisfies (==' ') *> many (satisfies (/='\n')))
  --     -- text :: Lexer (Token, String)
  --     text = bold <|> italic <|> bolditalic <|> vanilla
  --       where
  --         -- bold :: Lexer (Token, String)
  --         bold = BoldText <$> (string "**" *> many (satisfies (/='\n')) <* string "**")
  --           <|> (string "__" *> many (satisfies (/='\n')) <* string "__")
  --         -- italic :: Lexer (Token, String)
  --         italic = ItalicText <$> (char '*' *> many (satisfies (/='\n')) <* char '*')
  --           <|> (char '-' *> many (satisfies (/='\n')) <* char '-')
  --         -- bolditalic :: Lexer (Token, String)
  --         bolditalic = BoldItalicText <$> (string "***" *> many (satisfies (/='\n')) <* string "***")
  --           <|> (string "___" *> many (satisfies (/='\n')) <* string "___")
  --           <|> (string "__*" *> many (satisfies (/='\n')) <* string "*__")
  --           <|> (string "**_" *> many (satisfies (/='\n')) <* string "_**")
  --         -- vanilla :: Lexer (Token, String)
  --         vanilla = many (satisfies (/= '\n')) |> fmap (\x -> (Text x, x))

  -- token :: Lexer (Token, String)
  -- token = literal
  --   where
  --     with :: Functor f => b -> f a -> f (b, a)
  --     with b = fmap (\x -> (b, x))

  --     literal :: Lexer (Token, String)
  --     literal = many (satisfies (/= '"')) |> fmap (\x -> (Literal x, x))

  lexer :: String -> Either LexerError [Token]
  lexer _ = Left UnexpectedEOF
  -- lexer = runLexer (some token)

  -- data RawToken
  --   = VerticalWhitespace
  --   | HorizontalWhitespace String
  --   | Comment String
  --   | RawToken Token String

  -- rawLexer :: Lexer [RawToken]
  -- rawLexer = some (whitespace <|> comment <|> rawToken)
  --   where
  --     rawToken = fmap (uncurry RawToken) token
  --     comment = Comment <$> (string "<!--" *> many (satisfies (/= '\n')) <* string "-->")
  --     whitespace = verticalwhitespace <|> horizontalwhitespace
  --     verticalwhitespace = VerticalWhitespace <$ char '\n'
  --     horizontalwhitespace = HorizontalWhitespace <$> some (satisfies (\x -> isSpace x && x /= '\n'))
