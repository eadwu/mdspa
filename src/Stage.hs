{-# LANGUAGE NoImplicitPrelude #-}
module Stage
()
where
  import Eve
  import qualified Language.Lexer as L

  import Control.Monad ( (>=>) )
  import System.Exit ( exitFailure )

  type StageName = String
  data StagedError = StagedError StageName String

  stageEither :: Show e => String -> Either e o -> Either StagedError o
  stageEither name res = case res of
    -- Either contains a Left, which means there's an error, so convert to
    --   StagedError
    -- Left e -> Left (StagedError name (show e))
    -- Since |> has a fixity of 1 compared to <|'s fixity of 0, it is bound
    --   tighter, so this is equivalent to the above
    Left e -> Left <| StagedError name <| e |> show
    -- No error, so just return the output
    Right o -> Right o

  printStagedError :: StagedError -> IO ()
  printStagedError (StagedError name error) = do
    putStr name
    putStrLn " Error:"
    putStr error
    putStrLn ""

  data Stage i o = Stage
    { name :: String -- Stage name
    , runStage :: i -> Either StagedError o -- Either completes successfully with the transformed output or errors out
    }

  (>->) :: Stage a b -> Stage b c -> Stage a c
  (>->) (Stage n1 r1) (Stage n2 r2) = Stage (n1 ++ " >-> " ++ n2) (r1 >=> r2)
  -- (.)   ::            (b ->   c) -> (a ->   b) -> a ->   c
  -- (<=<) :: Monad m => (b -> m c) -> (a -> m b) -> a -> m c
  -- (>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
  -- Basically combining two stages, `>=>` combines the two runStages, like
  --   composition, internally explained with `(bs >=> cs) a` as
  --     do b <- bs a
  --        cs b

  makeStage :: Show e => String -> (i -> Either e o) -> Stage i o
  makeStage stageName runStage = Stage stageName <|
    runStage >>> stageEither stageName

  printStage :: Show b => Stage a b -> a -> IO ()
  printStage (Stage name r) a = case r a of
    Left err -> do
      printStagedError err
      exitFailure
    Right o -> do
      putStrLn <| name ++ ":"
      putStr <| show o

  lexerStage :: Stage StageName [L.Token]
  lexerStage = makeStage "Lexer" L.lexer
