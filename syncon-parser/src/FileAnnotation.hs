module FileAnnotation (annotate, annotateOne, putInTextTemplate, putInTemplate) where

import Pre

import Data.List (span)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text as Text
import qualified Data.HashMap.Strict as M

import P1Lexing.Types (Range(..), Position(..), firstPosition, stepPosition)

data AnnotationState = AnnotationState
  { position :: !Position
  , commands :: ![(Position, Maybe Text)]
  }

type AnnoM = State AnnotationState

-- | Takes a bunch of source files (map from filepath to contents) and annotations,
-- and creates one piece of HTML, annotated as with 'annotateOne', but with a <h1>
-- header for each file, and a little bit of vertical whitespace inbetween.
annotate :: HashMap Text Text -> [(Range, Text)] -> Text
annotate sources annotations = M.intersectionWith annotateOne sources splitAnnotations
  & M.toList
  & sortBy (comparing fst)
  & foldMap mkFileSection
  where
    splitAnnotations = mapFromFoldable (fst >>> getFilename) annotations
    getFilename (Range f _ _) = f
    getFilename Nowhere = ""
    mkFileSection (path, annotated) = "<h1>" <> (escape path & Builder.toLazyText & toS) <> "</h1>\n" <> annotated <> "\n\n"

-- TODO: assumes all ranges are well-nested, either check for this, or (ideally), implement support for non-nested annotations
-- | Takes some source code and a list of (well-nested) annotations
-- and produces well-formatted HTML.
-- Each annotated section is formatted as follows:
-- <span class="tagged"><div class="tag">annotation-content</div>section-content</span>
-- All text is properly escaped, and whitespace is preserved.
annotateOne :: Text -> [(Range, Text)] -> Text
annotateOne source annotations = flip evalState initState $ do
  start <- dumpCurrentCommands
  source' <- Text.unpack source & traverse step & fmap mconcat
  extras <- dumpRemainingCommands
  return $ toS $ Builder.toLazyText $ start <> source' <> extras
  where
    initState = AnnotationState { position = firstPosition, commands }
    commands :: [(Position, Maybe Text)]
    commands = sortBy comp $ do
      (Range _ start end, annotation) <- annotations
      [(start, Just annotation), (end, Nothing)]
    comp (p1, command1) (p2, command2) = compare p1 p2 <> comparing void command1 command2

-- | Read the provided file, replace the text "$prettyprint$" with the second argument, then return
-- the result. Presumably, the template should contain whatever styling and other wrapping is required
-- to make the result of 'annotate' presentable.
putInTemplate :: FilePath -> Text -> IO Text
putInTemplate templatePath html =
  readFile templatePath <&> Text.replace "$prettyprint$" html

putInTextTemplate :: Text -> Text -> Text
putInTextTemplate template html = Text.replace "$prettyprint$" html template

step :: Char -> AnnoM Builder
step c = do
  prevPosition <- gets position
  modify $ \st -> st { position = stepPosition prevPosition c }
  (escapeChar c <>) <$> dumpCurrentCommands

dumpCurrentCommands :: AnnoM Builder
dumpCurrentCommands = do
  st@AnnotationState{position, commands} <- get
  let (now, later) = span (fst >>> (<= position)) commands
  let tags = foldMap (snd >>> dumpCommand) now
  put $ st { commands = later }
  return tags

dumpRemainingCommands :: AnnoM Builder
dumpRemainingCommands = do
  as@AnnotationState{commands} <- get
  put $ as { commands = [] }
  return $ foldMap (snd >>> dumpCommand) commands

dumpCommand :: Maybe Text -> Builder
dumpCommand (Just t) = "<span class=\"tagged\"><div class=\"tag\">" <> escape t <> "</div>"
dumpCommand Nothing = "</span>"

escape :: Text -> Builder
escape = Text.foldr (\c b -> escapeChar c <> b) mempty

escapeChar :: Char -> Builder
escapeChar '<'  = Builder.fromText "&lt;"
escapeChar '>'  = Builder.fromText "&gt;"
escapeChar '&'  = Builder.fromText "&amp;"
escapeChar '"'  = Builder.fromText "&quot;"
escapeChar '\'' = Builder.fromText "&#39;"
escapeChar x    = Builder.singleton x
