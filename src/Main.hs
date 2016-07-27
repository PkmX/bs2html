{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Prelude hiding (FilePath)
import Control.Applicative
import Control.Monad
import Control.Lens hiding (argument, (<.>), (#))
import Codec.Text.IConv
import Data.Attoparsec.ByteString as Parsec.BS
import Data.Attoparsec.Binary
import Data.Attoparsec.Combinator (lookAhead)
import Data.Attoparsec.Text as Parsec.Text
import Data.Bits
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS hiding (take)
import qualified Data.ByteString.Lazy as BL
import Data.Char
import Data.Default.Class
import Data.Functor
import Data.Int
import Data.Maybe
import Data.Monoid
import Data.String
import Data.Word
import qualified Data.Map.Lazy as M
import qualified Data.String.Interpolate as I
import qualified Data.String.Interpolate.Util as I
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Filesystem.Path.CurrentOS as Path
import Numeric.Lens (binary)
import Options.Applicative as Optparse
import System.Directory
import System.IO hiding (FilePath)
import System.ProgressBar
import Text.Blaze.Internal
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import Clay ((?), (-:))
import qualified Clay as C
import qualified Clay.Flexbox
import qualified Clay.Text
import qualified Clay.Selector
import Language.Javascript.JMacro

data HDR = HDR { hdrTimestamp :: UTCTime
               , hdrXmode :: Word32
               , hdrXname :: Text
               , hdrOwner :: Text
               , hdrNick :: Text
               , hdrScore :: Int8
               , hdrDate :: Text
               , hdrTitle :: Text
               } deriving Show

hdrIsRestrict :: HDR -> Bool
hdrIsRestrict HDR{..} = (hdrXmode .&. 0x00000800) /= 0

instance ToJExpr HDR where
  toJExpr HDR{..} = toJExpr $ M.fromList [ ("score" :: String, toJExpr (fromIntegral hdrScore :: Int))
                                         , ("owner", toJExpr hdrOwner)
                                         , ("nick", toJExpr hdrNick)
                                         , ("date", toJExpr hdrDate)
                                         , ("title", toJExpr hdrTitle)
                                         , ("xname", toJExpr hdrXname)
                                         ]

-- // CHAR_BIT == 8, char == signed char, sizeof(time_t) == sizeof(int) == 4, little-endian
--
-- typedef struct {
--   time_t chrono;
--   int    mode;
--   int    xid; // unused
--   char   xname[32];
--   char   owner[80];
--   char   nick[49];
--   char   score;
--   char   date[9];
--   char   title[73];
-- } HDR;
hdrParser :: Parsec.BS.Parser HDR
hdrParser = do
    hdrTimestamp <- posixSecondsToUTCTime . realToFrac . (fromIntegral :: Word32 -> Int32) <$> anyWord32le
    hdrXmode <- anyWord32le
    void $ Parsec.BS.take 4
    hdrXname <- big5CharArray 32
    hdrOwner <- big5CharArray 80
    hdrNick <- big5CharArray 49
    hdrScore <- fromIntegral <$> Parsec.BS.anyWord8
    hdrDate <- stripEndNuls . T.decodeUtf8 <$> Parsec.BS.take 9
    hdrTitle <- big5CharArray 73
    return HDR{..}
  where
    big5CharArray n = stripEndNuls . big5Decoder <$> Parsec.BS.take n
    stripEndNuls = T.dropWhileEnd (== '\NUL')

big5Decoder :: ByteString -> Text
big5Decoder = T.decodeUtf8 . BL.toStrict . convertFuzzy Transliterate "BIG5" "UTF-8" . BL.fromStrict

data Color8 = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White deriving (Eq, Enum, Show)
newtype BgColor = BgColor Color8 deriving (Eq, Enum, Show)
data Bold = Bold | NotBold deriving (Eq, Enum, Show)
data FgColor = FgColor { bold :: Bold, color8 :: Color8 } deriving (Eq, Show)
data Blinking = Blinking | NotBlinking deriving (Eq, Enum, Show)
data QuoteLine = Quote | Quoter deriving (Eq, Enum, Show)

data TextState = TextState { bgColor :: BgColor, fgColor :: FgColor, blinking :: Blinking, quoteLine :: Maybe QuoteLine } deriving Show

class CssClassy c where
  toCssClass :: c -> CssClass
  toAttrValue :: c -> AttributeValue
  toAttrValue = fromString . unCssClass . toCssClass
  toAttr :: c -> Attribute
  toAttr = A.class_ . toAttrValue
  toSelector :: c -> C.Selector
  toSelector = Clay.Selector.text . T.pack . ('.' <|) . unCssClass . toCssClass

newtype CssClass = CssClass { unCssClass :: String } deriving Show
instance CssClassy CssClass where
  toCssClass = id

instance CssClassy FgColor where
  toCssClass FgColor{..} | Bold <- bold    = CssClass $ "fg-bold-" <> showLower color8
  toCssClass FgColor{..} | NotBold <- bold = CssClass $ "fg-"      <> showLower color8

instance CssClassy BgColor where
  toCssClass (BgColor color8) = CssClass $ "bg-" <> showLower color8

instance Default TextState where
  def = TextState { bgColor = BgColor Black, fgColor = FgColor NotBold White, blinking = NotBlinking, quoteLine = Nothing }

metalineParser :: Parsec.Text.Parser Html
metalineParser = do
    authorText :: Text <- ("作者: " $> " 作者 ") <|> ("發信人: " $> "發信人")
    author <- Parsec.Text.takeWhile (`notElem` ['站', '看'])
    boardText :: Text <- ("站內: " $> " 站內 ") <|> ("看板: " $> " 看板 ")
    board <- Parsec.Text.takeWhile (/= '\n') <* endOfLine
    _ <- "標題: "
    title <- Parsec.Text.takeWhile (/= '\n') <* endOfLine
    _ <- "時間: "
    date <- Parsec.Text.takeWhile (/= '\n') <* endOfLine
    pure $ do
      flexRowDiv $ do
        H.div & blueOnWhite $ H.toHtml authorText
        (H.div & whiteOnBlue) ! flex $ H.toHtml $ " " <> author
        H.div & blueOnWhite $ H.toHtml boardText
        H.div & whiteOnBlue $ H.toHtml $ " " <> board <> " "
      flexRowDiv $ do
        H.div & blueOnWhite $ " 標題 "
        (H.div & whiteOnBlue) ! flex $ H.toHtml $ " " <> title
      flexRowDiv $ do
        H.div & blueOnWhite $ " 時間 "
        (H.div & whiteOnBlue) ! flex $ H.toHtml $ " " <> date
  where
        blueOnWhite = (`setClasses` [ toCssClass (FgColor NotBold Blue), toCssClass (BgColor White) ])
        whiteOnBlue = (`setClasses` [ toCssClass (FgColor NotBold White), toCssClass (BgColor Blue) ])
        flexRowDiv = H.div ! A.class_ "flex-row"
        flex = A.style "flex-grow: 1"

data PushKind = Push | Boo | Arrow deriving Show
instance CssClassy PushKind where
  toCssClass k = CssClass $ "push-" ++ showLower k

pushParser :: Parsec.Text.Parser Html
pushParser = do
    pushKind <- choice [ esc 'A' $> Push, esc 'B' $> Boo, esc 'C' $> Arrow]
    pushText <- takeTillEsc
    esc 'D'
    pusher <- takeTillEsc
    esc 'E'
    _ <- takeTillEsc
    esc 'F'
    pushBody <- takeTillEsc
    esc 'G'
    pushTime <- takeTillEsc
    _ <- "\ESC[m"

    pure $ H.div ! A.class_ "push" $ do
      H.div ! toAttr pushKind $ H.toHtml pushText
      H.div ! A.class_ "pusher" $ H.toHtml pusher
      H.div ! A.class_ "push-sep" $ "："
      H.div ! A.class_ "push-body" $ H.toHtml pushBody
      H.div ! A.class_ "push-time" $ H.toHtml pushTime

  where esc :: Char -> Parsec.Text.Parser ()
        esc c = void $ Parsec.Text.string "\ESC*" >> char c
        takeTillEsc = Parsec.Text.takeWhile (/= '\ESC')

setClasses :: Attributable h => h -> [CssClass] -> h
p `setClasses` clses = p !? (not (Prelude.null clses), A.class_ $ fromString $ unwords $ map unCssClass clses)

textStateClasses :: TextState -> [CssClass]
textStateClasses TextState{..} =
    case quoteLine of
      Nothing -> [ toCssClass bgColor | bgColor /= BgColor Black ] ++
                 [ toCssClass fgColor | fgColor /= FgColor NotBold White ] ++
                 [ CssClass (showLower blinking) | blinking == Blinking ]
      Just q -> [ CssClass (showLower q) ]

bbsParser :: Parsec.Text.Parser Html
bbsParser = do
    metaline <- fromMaybe "" <$> optional metalineParser
    (metaline <>) <$> go def True
  where go ts showBr = do
          html <- do (H.toHtml -> inner) <- Parsec.Text.takeWhile (`notElem` ['\ESC', '\n'])
                     let clses = textStateClasses ts
                     if Prelude.null clses then
                       pure inner
                     else
                       pure $ H.span ! A.class_ (fromString $ unwords $ map unCssClass clses) $ inner
          choice [ do ts' <- ($ ts) <$> escParser
                      (html <>) <$> go ts' True
                 , do _ <- char '\n'
                      let html' = if showBr then html <> H.br else html
                      choice [ do _ <- lookAhead "※"
                                  (html' <>) <$> go ( def { quoteLine = Just Quoter } ) True
                             , do _ <- lookAhead ">"
                                  (html' <>) <$> go ( def { quoteLine = Just Quote } ) True
                             , do pushHtml <- pushParser
                                  ((html' <> pushHtml) <>) <$> go (ts { quoteLine = Nothing }) False
                             , (html' <>) <$> go (ts { quoteLine = Nothing }) True
                             ]
                 , endOfInput $> html
                 ]

escParser :: Parsec.Text.Parser (TextState -> TextState)
escParser = do
    _ <- char '\ESC'
    anyChar >>= \case
      '[' -> do -- CSI
        params <- optional decimal `sepBy` char ';'
        anyChar >>= \case
          'm' -> pure $ case mapM sgrToFn (catMaybes params) of
                          Just [] -> const def -- \ESC[m, \ESC[;m, ...
                          Just fs -> foldr (.) id fs
                          Nothing -> id
          _   -> pure id -- Ignore unknown CSI codes
      '*' -> anyChar $> id -- Ignore BBS control character (handled in pushParser)
      _   -> pure id -- Consume and ignore all other control codes
  where
     sgrToFn 0 = Just $ const def
     sgrToFn 1 = Just $ \s -> s { fgColor = (fgColor s) { bold = Bold } }
     sgrToFn 5 = Just $ \s -> s { blinking = Blinking }
     sgrToFn n | n >= 30 && n <= 37 = Just $ \s -> s { fgColor = (fgColor s) { color8 = toEnum (fromEnum Black + (n - 30)) } }
     sgrToFn n | n >= 40 && n <= 47 = Just $ \s -> s { bgColor = BgColor $ toEnum $ fromEnum Black + (n - 40) }
     sgrToFn _ = Nothing

bbs2Html :: HDR -> Text -> TL.Text
bbs2Html HDR{..} src = renderHtml $
  H.docTypeHtml $ do
    H.head $ do
      H.meta ! A.charset "utf-8"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
      H.title (H.toHtml hdrTitle)
      H.link ! A.rel "stylesheet" ! A.href "https://fonts.googleapis.com/css?family=Inconsolata"
      H.style $ H.toHtml $ C.render $ do
        C.html <> C.body ? do
          C.margin C.nil C.nil C.nil C.nil
          C.width (C.pct 100)
          C.height (C.pct 100)
          C.backgroundColor "#000000"
          C.color "#EEEEEC"
          C.display C.flex
          Clay.Flexbox.justifyContent C.center

        ".container" ? do
          C.margin (C.em 1) C.nil (C.em 1) C.nil
          C.fontFamily ["細明體", "Inconsolata"] [C.monospace]
          C.whiteSpace Clay.Text.preWrap
          C.fontSize (C.vw 1.8)
          C.lineHeight (C.pct 110)
          C.width (C.em 40)

        let toCssColor :: Color8 -> Bold -> C.Color
            toCssColor Black   NotBold = "#000000"
            toCssColor Red     NotBold = "#CC0000"
            toCssColor Green   NotBold = "#4E9A06"
            toCssColor Yellow  NotBold = "#C4A000"
            toCssColor Blue    NotBold = "#3465A4"
            toCssColor Magenta NotBold = "#75507B"
            toCssColor Cyan    NotBold = "#06989A"
            toCssColor White   NotBold = "#D3D7CF"
            toCssColor Black   Bold = "#555753"
            toCssColor Red     Bold = "#EF2929"
            toCssColor Green   Bold = "#8AE234"
            toCssColor Yellow  Bold = "#FCE94F"
            toCssColor Blue    Bold = "#729FCF"
            toCssColor Magenta Bold = "#AD7FA8"
            toCssColor Cyan    Bold = "#34E2E2"
            toCssColor White   Bold = "#EEEEEC"

        forM_ [Black .. White] $ \color -> do
          toSelector (BgColor color) ?
            C.backgroundColor (toCssColor color NotBold)
          forM_ [Bold, NotBold] $ \bold ->
            toSelector (FgColor bold color) ?
              C.color (toCssColor color bold)

        ".quoter" ? C.color (toCssColor Cyan Bold)
        ".quote"  ? C.color (toCssColor Cyan NotBold)

        C.keyframes "blinker" [ (50, C.opacity 0) ]
        ".blinking" ? C.animation "blinker" (C.sec 2) (C.other "step-end") (C.sec 0) C.infinite (C.other "normal") (C.other "none")

        ".flex-row" <> ".push" ? do
          C.display C.flex
          Clay.Flexbox.flexDirection Clay.Flexbox.row

        forM_ [ Push, Boo, Arrow ] $ \push ->
          toSelector push ? do
            C.color $ case push of Push  -> toCssColor Red NotBold
                                   Boo   -> toCssColor Black Bold
                                   Arrow -> toCssColor Yellow NotBold
            C.width (C.em 1.5)

        ".pusher"     ? C.color (toCssColor Yellow Bold)
        ".push-sep"   ? C.color (toCssColor White NotBold)
        ".push-body"  ? do
          C.color (toCssColor White Bold)
          Clay.Flexbox.flexGrow 1
        ".push-time"  ? do
          C.color (toCssColor White NotBold)
          Clay.Flexbox.alignSelf Clay.Flexbox.flexEnd


    H.body $
      H.div ! A.class_ "container" $
        case Parsec.Text.parseOnly bbsParser src of
          Left e -> H.toHtml ("An error occured while generating this article: " <> e)
          Right html -> html

indexHtml :: [HDR] -> Html
indexHtml hdrs = H.docTypeHtml $ do
  H.head $ do
    H.meta ! A.charset "utf-8"
    H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
    H.title ""
    H.script ! A.src "https://cdnjs.cloudflare.com/ajax/libs/webcomponentsjs/0.7.22/webcomponents-lite.min.js" $ mempty
    H.script $ H.toHtml $ showText $ renderJs [jmacro| window.Polymer = { dom: 'shadow' } |]
    H.link ! A.rel "import" ! A.href "https://cdn.vaadin.com/vaadin-elements/latest/vaadin-grid/vaadin-grid.html"
    H.link ! A.rel "stylesheet" ! A.href "https://fonts.googleapis.com/css?family=Roboto+Condensed" -- :300,400|Roboto+Slab:300"
    H.style ! customAttribute "is" "custom-style" $ H.toHtml $ C.render $ do
      C.html <> C.body ? do
        C.margin C.nil C.nil C.nil C.nil
        C.width (C.pct 100)
        C.height (C.pct 100)
        C.display C.flex
        Clay.Flexbox.flexDirection Clay.Flexbox.column
        C.backgroundColor "#f1f1f1"

      "vaadin-grid" ? do
        C.width (C.pct 100)
        C.maxWidth (C.em 80)
        C.height (C.pct 100)
        Clay.Flexbox.flexGrow 1
        Clay.Flexbox.alignSelf C.center
        "--vaadin-grid-row-cell" -: "{ font-family: monospace; }"

      "footer" ? do
        C.width (C.vw 100)
        C.fontSize (C.pct 90)
        C.lineHeight (C.unitless 0.5)
        C.textAlign C.center
        C.backgroundColor C.white
        C.borderTop C.solid (C.px 1) "#e2e2e2"
        C.fontFamily ["Roboto Condensed"] [C.sansSerif]

      "footer a" ?
        C.color "#2196f3"

  H.body $ do
    customParent "vaadin-grid" $
      H.table $ do
        H.colgroup $ do
          let flex = customAttribute "flex"
              makeCol (width :: Int) field = H.col ! flex (fromString $ show width) ! A.name field
          makeCol 1 "id" >> makeCol 1 "score" >> makeCol 2 "date" >> makeCol 3 "author" >> makeCol 10 "title"
        H.thead $
          H.tr $ H.th "編號" >> H.th "評分" >> H.th "日期" >> H.th "作者" >> H.th "文章標題"
        H.tbody mempty
    H.footer $
      H.p $ do
        H.toHtml ("Generated by " :: String)
        H.a ! A.href "https://www.github.com/pkmx/bs2html" $ "BS2HTML"
    H.script $ H.toHtml $ showText $ renderJs
      [jmacro|
        var jhdrs = `(toJExpr hdrs)`;
        for (var i = 0; i < jhdrs.length; ++i) {
          var hdr = jhdrs[i];
          hdr.id = (i + 1);
          hdr.author = hdr.owner + " (" + hdr.nick + ")";
        };
        var grid = document.querySelector('vaadin-grid');
        grid.items = jhdrs;
        grid.addEventListener('selected-items-changed', \ {
          var hdr = jhdrs[grid.selection.selected()];
          if (hdr !== undefined) {
            window.open(hdr.xname + ".html", "_blank");
            grid.selection.clear();
          }
        });
      |]

data Args = Args { inputDir :: FilePath
                 , outputDir :: FilePath
                 , showHidden :: Bool
                 , debug :: Bool
                 } deriving Show

argsParser :: Optparse.Parser Args
argsParser = Args <$> (Path.decodeString <$> argument str (metavar "INPUT" <> help "Directory to bs2.to backup"))
                  <*> (Path.decodeString <$> argument str (metavar "OUTPUT" <> help "Output directory (created if non-existent)"))
                  <*> switch (long "show-hidden" <> help "Include hidden posts")
                  <*> switch (long "debug" <> help "Print debug information")

main :: IO ()
main = do
  Args{..} <- Optparse.execParser $ info (helper <*> argsParser) (fullDesc <> progDesc "Convert bs2.to backups to a static website")
  dir <- BS.readFile $ Path.encodeString (inputDir </> ".DIR")
  ePutStr "Parsing .DIR ..."
  case Parsec.BS.parseOnly (many hdrParser) dir of
    Left e -> ePutStrLn $ " Parse error: " <> e
    Right allHdrs -> do
      let hdrs = if showHidden then allHdrs else filter (not . hdrIsRestrict) allHdrs
      ePutStrLn " done"
      createDirectoryIfMissing True (Path.encodeString outputDir)
      when debug $ forM_ hdrs $ T.putStrLn . ppHDR
      iforM_ hdrs $ \i hdr@HDR{..} -> do
        ePutStr $ mkProgressBar (msg "\rConverting articles to HTML") exact 60 (fromIntegral (i + 1)) (fromIntegral (length hdrs))
        case hdrXname of
          _ :> c -> do
            let fileName = inputDir </> Path.decodeString [c] </> Path.fromText hdrXname
            body <- big5Decoder <$> BS.readFile (Path.encodeString fileName)
            TL.writeFile (Path.encodeString $ outputDir </> Path.fromText hdrXname <.> "html") (bbs2Html hdr body)
          _      -> ioError $ userError "Empty xname"
      ePutStrLn "\nGenerating index.html"
      TL.writeFile (Path.encodeString $ outputDir </> "index.html") (renderHtml (indexHtml hdrs))

ePutStr :: String -> IO ()
ePutStr = hPutStr stderr

ePutStrLn :: String -> IO ()
ePutStrLn = hPutStrLn stderr

showText :: Show a => a -> Text
showText = T.pack . show

showLower :: Show a => a -> String
showLower = map toLower . show

ppHDR :: HDR -> Text
ppHDR HDR{..} = T.pack $ I.unindent
  [I.i|
    ID: #{hdrXname}
    Title: #{hdrTitle}
    Author: #{hdrOwner} (#{hdrNick})
    Date: #{hdrDate}
    Score: #{hdrScore}
    Timestamp: #{hdrTimestamp}
    XMode: #{hdrXmode ^. re binary}
  |]

