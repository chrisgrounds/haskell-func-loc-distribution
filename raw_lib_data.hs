module Data.Aeson.Decoding (
    decode,
    eitherDecode,
    throwDecode,
    decodeStrict,
    eitherDecodeStrict,
    throwDecodeStrict,
    decodeStrictText,
    eitherDecodeStrictText,
    throwDecodeStrictText,
    toEitherValue,
    unescapeText,
) where

import           Control.Monad.Catch                 (MonadThrow (..))
import           Data.Aeson.Types.Internal           (AesonException (..), formatError)

import qualified Data.Aeson.Types                    as A
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Text                           as T

import           Data.Aeson.Decoding.ByteString
import           Data.Aeson.Decoding.ByteString.Lazy
import           Data.Aeson.Decoding.Text
import           Data.Aeson.Decoding.Conversion
import           Data.Aeson.Internal.Unescape        (unescapeText)


decodeStrict :: (A.FromJSON a) => BS.ByteString -> Maybe a
decodeStrict bs = unResult (toResultValue (bsToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> Just x
        | otherwise   -> Nothing
    A.IError _ _      -> Nothing

eitherDecodeStrict :: (A.FromJSON a) => BS.ByteString -> Either String a
eitherDecodeStrict bs = unResult (toResultValue (bsToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> Right x
        | otherwise   -> Left "Trailing garbage"
    A.IError path msg -> Left $ formatError path msg

throwDecodeStrict :: forall a m. (A.FromJSON a, MonadThrow m) => BS.ByteString -> m a
throwDecodeStrict bs = unResult (toResultValue (bsToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> pure x
        | otherwise   -> throwM $ AesonException "Trailing garbage"
    A.IError path msg -> throwM $ AesonException $ formatError path msg


decode :: (A.FromJSON a) => LBS.ByteString -> Maybe a
decode bs = unResult (toResultValue (lbsToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Just x
        | otherwise    -> Nothing
    A.IError _ _       -> Nothing

eitherDecode :: (A.FromJSON a) => LBS.ByteString -> Either String a
eitherDecode bs = unResult (toResultValue (lbsToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Right x
        | otherwise    -> Left "Trailing garbage"
    A.IError path msg  -> Left $ formatError path msg

throwDecode :: forall a m. (A.FromJSON a, MonadThrow m) => LBS.ByteString -> m a
throwDecode bs = unResult (toResultValue (lbsToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs'  -> pure x
        | otherwise    -> throwM $ AesonException "Trailing garbage"
    A.IError path msg  -> throwM $ AesonException $ formatError path msg


decodeStrictText :: (A.FromJSON a) => T.Text -> Maybe a
decodeStrictText bs = unResult (toResultValue (textToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | textSpace bs' -> Just x
        | otherwise     -> Nothing
    A.IError _ _        -> Nothing

eitherDecodeStrictText :: (A.FromJSON a) => T.Text -> Either String a
eitherDecodeStrictText bs = unResult (toResultValue (textToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | textSpace bs' -> Right x
        | otherwise     -> Left "Trailing garbage"
    A.IError path msg   -> Left $ formatError path msg

throwDecodeStrictText :: forall a m. (A.FromJSON a, MonadThrow m) => T.Text -> m a
throwDecodeStrictText bs = unResult (toResultValue (textToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | textSpace bs' -> pure x
        | otherwise     -> throwM $ AesonException "Trailing garbage"
    A.IError path msg   -> throwM $ AesonException $ formatError path msg

module Data.Aeson.Decoding.ByteString (
    bsToTokens,
) where

import           Data.ByteString              (ByteString)
import           Data.Char                    (chr)
import           Data.Integer.Conversion      (byteStringToInteger)
import           Data.Text                    (Text)
import           Data.Word                    (Word8)

import qualified Data.Aeson.Key               as Key
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Unsafe       as BS.Unsafe
import qualified Data.Scientific              as Sci
import qualified Data.Word8.Patterns          as W8

import           Data.Aeson.Decoding.Internal
import           Data.Aeson.Decoding.Tokens
import           Data.Aeson.Internal.Text     (unsafeDecodeASCII)
import           Data.Aeson.Internal.Unescape (unescapeText)

bsToTokens :: ByteString -> Tokens ByteString String
bsToTokens bs0 = goT bs0 id where
    goT :: Parser Tokens k
    goT (skipSpace -> bs) k = case BS.uncons bs of
        Nothing         -> tkErr "Unexpected end-of-input, expecting JSON value"
        Just (!w, !bs1) -> tokenCase w bs1 bs k

    tokenCase
        :: Word8              -- head
        -> ByteString         -- tail
        -> ByteString         -- whole input, needed for number parsing
        -> (ByteString -> k)  -- continuation
        -> Tokens k String
    tokenCase W8.LEFT_CURLY   !bs !_   k      = TkRecordOpen (goR bs k)
    tokenCase W8.LEFT_SQUARE   bs  _   k      = TkArrayOpen (goA bs k)
    tokenCase W8.DOUBLE_QUOTE  bs  _   k      = scanStringLiteral (\t bs' -> TkText t (k bs')) tkErr bs
    tokenCase W8.HYPHEN        bs  _   k      = scanNumberLiteral (\n bs' -> TkNumber (negateNumber n) (k bs')) tkErr bs
    tokenCase w                _   wbs k
        | W8.DIGIT_0 <= w, w <= W8.DIGIT_9    = scanNumberLiteral (\n bs' -> TkNumber n (k bs')) tkErr wbs
    tokenCase W8.LOWER_N       bs  _   k
        | Just bs1 <- stripPrefix "ull" 3 bs  = TkLit LitNull (k bs1)
    tokenCase W8.LOWER_T       bs  _   k
        | Just bs1 <- stripPrefix "rue" 3 bs  = TkLit LitTrue (k bs1)
    tokenCase W8.LOWER_F       bs  _   k
        | Just bs1 <- stripPrefix "alse" 4 bs = TkLit LitFalse (k bs1)
    tokenCase _                _   wbs _      = tkErr $ "Unexpected " ++ showBeginning wbs ++ ", expecting JSON value"

    goA :: Parser TkArray k
    goA (skipSpace -> bs) k = case BS.uncons bs of
        Nothing         -> tkErrEOF "JSON value or ]"
        Just (W8.RIGHT_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (w,  !bs1) -> TkItem $ tokenCase w bs1 bs $ \bs2 -> goA1 bs2 k

    goA1 :: Parser TkArray k
    goA1 (skipSpace -> bs) k = case BS.uncons bs of
        Nothing                      -> tkErrEOF ", or ]"
        Just (W8.RIGHT_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (W8.COMMA, !bs1)        -> TkItem $ goT bs1 $ \bs2 -> goA1 bs2 k
        _                            -> tkErrBS bs ", or ]"

    goR :: Parser TkRecord k
    goR (skipSpace -> bs) k = case BS.uncons bs of
        Nothing                       -> tkErrEOF "record key literal or }"
        Just (W8.DOUBLE_QUOTE,  !bs1) -> goRK bs1 k           -- "
        Just (W8.RIGHT_CURLY, !bs1)   -> TkRecordEnd (k bs1)  -- }
        Just _                        -> tkErrBS bs "record key literal or }"

    goR1 :: Parser TkRecord k
    goR1 (skipSpace -> bs) k = case BS.uncons bs of
        Nothing                           -> tkErr "Unexpected end-of-input, expecting , or }"
        Just (W8.COMMA, !bs1) -> case BS.uncons (skipSpace bs1) of
            Nothing                      -> tkErrEOF "key literal"
            Just (W8.DOUBLE_QUOTE, !bs2) -> goRK bs2 k
            Just _                       -> tkErrBS bs "key literal"
        Just (W8.RIGHT_CURLY, !bs1)       -> TkRecordEnd (k bs1)
        _                                 -> tkErr $ "Unexpected " ++ showBeginning bs ++ ", expecting , or }"

    goRK :: Parser TkRecord k
    goRK bs1 k = scanStringLiteral (\t bs -> goRK' t bs k) tkErr bs1

    goRK' :: Text -> Parser TkRecord k
    goRK' t (skipSpace -> bs) k = case BS.uncons bs of
        Nothing               -> tkErrEOF ":"
        Just (W8.COLON, !bs3) -> TkPair (Key.fromText t) $ goT bs3 $ \bs4 -> goR1 bs4 k
        Just _                -> tkErrBS bs ":"

stripPrefix :: ByteString -> Int -> ByteString -> Maybe ByteString
stripPrefix pfx n bs | BS.isPrefixOf pfx bs = Just (BS.Unsafe.unsafeDrop n bs)
                     | otherwise            = Nothing

type Parser tk k = ByteString -> (ByteString -> k) -> tk k String

showBeginning :: ByteString -> String
showBeginning = show . BS.take 30

skipSpace :: ByteString -> ByteString
skipSpace = BS.dropWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

tkErrEOF :: AsError t =>String ->  t k String
tkErrEOF expected = tkErr $
    "Unexpected end-of-input, expecting " ++ expected

tkErrBS :: AsError t => ByteString -> String ->  t k String
tkErrBS bs expected = tkErr $
    "Unexpected " ++ showBeginning bs ++ ", expecting " ++ expected


scanStringLiteral
    :: forall r. (Text -> ByteString -> r)
    -> (String -> r)
    -> ByteString
    -> r
scanStringLiteral ok err bs0 = go 0 bs0 where
    go :: Int -> ByteString -> r
    go !n !bs = case BS.uncons bs of
        Nothing          -> errEnd
        Just (34, _)     -> ok (unsafeDecodeASCII (BS.Unsafe.unsafeTake n bs0)) (BS.Unsafe.unsafeDrop (n + 1) bs0)
        Just (92, bs')   -> goSlash (n + 1) bs'
        Just (w8, bs')
            | w8 < 0x20  -> errCC
            | w8 >= 0x80 -> goEsc (n + 1) bs'
            | otherwise  -> go (n + 1) bs'

    goEsc :: Int -> ByteString -> r
    goEsc !n !bs = case BS.uncons bs of
        Nothing        -> errEnd
        Just (34, _)   -> case unescapeText (BS.Unsafe.unsafeTake n bs0) of
            Right t -> ok t (BS.drop (n + 1) bs0)
            Left e  -> err (show e)
        Just (92, bs') -> goSlash (n + 1) bs'
        Just (_,  bs') -> goEsc (n + 1) bs'

    goSlash :: Int -> ByteString -> r
    goSlash !n !bs = case BS.uncons bs of
        Nothing       -> errEnd
        Just (_, bs') -> goEsc (n + 1) bs'

    errEnd = err "Unexpected end-of-input while parsing string literal"
    errCC  = err "Unespected control character while parsing string literal"


scanNumberLiteral
    :: forall r. (Number -> ByteString -> r)
    -> (String -> r)
    -> ByteString
    -> r
scanNumberLiteral kont err bs0 = state_start bs0 where
    state_start :: ByteString -> r
    state_start !bs = case BS.uncons bs of
        Nothing                      -> errEnd
        Just (w8, bs')
            | W8.DIGIT_0 < w8, w8 <= W8.DIGIT_9 -> state_i1 1 bs'
            | W8.DIGIT_0 == w8                  -> state_after0 bs'
            | otherwise                         -> err $ "Unexpected " ++ show w8 ++ " while parsing number literal"

    state_after0 :: ByteString -> r
    state_after0 !bs = case BS.uncons bs of
        Nothing                         -> kont (NumInteger 0) bs
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9   -> err "Number literal with leading zero"
            | W8.PERIOD == w8                      -> go_dec 0 bs'
            | W8.LOWER_E == w8 || W8.UPPER_E == w8 -> go_sci 0 0 bs'
            | otherwise                            -> kont (NumInteger 0) bs

    state_i1 :: Int -> ByteString -> r
    state_i1 !n !bs = case BS.uncons bs of
        Nothing                         -> kont (NumInteger int) bs
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9   -> state_i1 (n + 1) bs'
            | W8.PERIOD == w8                      -> go_dec int bs'
            | W8.LOWER_E == w8 || W8.UPPER_E == w8 -> go_sci int 0 bs'
            | otherwise                            -> kont (NumInteger int) bs
      where
        int = byteStringToInteger (BS.Unsafe.unsafeTake n bs0)

    go_dec :: Integer -> ByteString -> r
    go_dec !int !bs1 = case BS.uncons bs1 of
        Nothing                       -> errEnd
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9 -> state_dec 1 bs'
            | otherwise                          -> err $ "Unexpected " ++ show w8 ++ " while parsing number literal"
      where
        state_dec :: Int -> ByteString -> r
        state_dec !n !bs = case BS.uncons bs of
            Nothing                         -> kont (NumDecimal dec) bs
            Just (w8, bs')
                | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9    -> state_dec (n + 1) bs'
                | W8.LOWER_E == w8 || W8.UPPER_E == w8  -> go_sci coef (negate n) bs'
                | otherwise                             -> kont (NumDecimal dec) bs
          where
            frac = byteStringToInteger (BS.Unsafe.unsafeTake n bs1)
            coef = int * 10 ^ n + frac
            dec  = Sci.scientific coef (negate n)

    go_sci :: Integer -> Int -> ByteString -> r
    go_sci !coef !exp10 !bs2 = case BS.uncons bs2 of
        Nothing                       -> errEnd
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9 -> go_sci_pos coef exp10 bs2 1 bs'
            | W8.PLUS == w8                      -> case BS.uncons bs' of
                Nothing                          -> errEnd
                Just (w8', bs'')
                    | W8.DIGIT_0 <= w8', w8' <= W8.DIGIT_9 -> go_sci_pos coef exp10 bs' 1 bs''
                    | otherwise                            -> errUnx w8'
            | W8.HYPHEN == w8         -> case BS.uncons bs' of
                Nothing               -> errEnd
                Just (w8', bs'')
                    | W8.DIGIT_0 <= w8', w8' <= W8.DIGIT_9 -> go_sci_neg coef exp10 bs' 1 bs''
                    | otherwise                            -> errUnx w8'
            | otherwise                                    -> errUnx w8

    go_sci_pos :: Integer -> Int -> ByteString -> Int -> ByteString -> r
    go_sci_pos !coef !exp10 !bs2 !n !bs = case BS.uncons bs of
        Nothing                       -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9 -> go_sci_pos coef exp10 bs2 (n + 1) bs'
            | otherwise                          -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (byteStringToInteger (BS.Unsafe.unsafeTake n bs2))
        sci = Sci.scientific coef (exp10 + exp10')

    go_sci_neg :: Integer -> Int -> ByteString -> Int -> ByteString -> r
    go_sci_neg !coef !exp10 !bs2 !n !bs = case BS.uncons bs of
        Nothing                       -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9  -> go_sci_neg coef exp10 bs2 (n + 1) bs'
            | otherwise               -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (byteStringToInteger (BS.Unsafe.unsafeTake n bs2))
        sci = Sci.scientific coef (exp10 - exp10')

    errEnd    = err "Unexpected end-of-input while parsing number literal"
    errUnx w8 = err $ "Unexpected " ++ show (chr (fromIntegral w8)) ++ " while parsing number literal"

module Data.Aeson.Decoding.ByteString.Lazy (
    lbsToTokens,
) where

import           Data.ByteString.Lazy         (ByteString)
import           Data.Char                    (chr)
import           Data.Integer.Conversion      (byteStringToInteger)
import           Data.Text                    (Text)
import           Data.Word                    (Word8)

import qualified Data.Aeson.Key               as Key
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Scientific              as Sci
import qualified Data.Word8.Patterns          as W8

import           Data.Aeson.Decoding.Internal
import           Data.Aeson.Decoding.Tokens
import           Data.Aeson.Internal.Text     (unsafeDecodeASCII)
import           Data.Aeson.Internal.Unescape (unescapeText)

lbsToTokens :: ByteString -> Tokens ByteString String
lbsToTokens bs0 = goT bs0 id where
    goT :: Parser Tokens k
    goT (skipSpace -> bs) k = case LBS.uncons bs of
        Nothing         -> tkErr "Unexpected end-of-input, expecting JSON value"
        Just (!w, !bs1) -> tokenCase w bs1 bs k

    tokenCase
        :: Word8              -- head
        -> ByteString         -- tail
        -> ByteString         -- whole input, needed for number parsing
        -> (ByteString -> k)  -- continuation
        -> Tokens k String
    tokenCase W8.LEFT_CURLY   !bs !_   k      = TkRecordOpen (goR bs k)
    tokenCase W8.LEFT_SQUARE   bs  _   k      = TkArrayOpen (goA bs k)
    tokenCase W8.DOUBLE_QUOTE  bs  _   k      = scanStringLiteral (\t bs' -> TkText t (k bs')) tkErr bs
    tokenCase W8.HYPHEN        bs  _   k      = scanNumberLiteral (\n bs' -> TkNumber (negateNumber n) (k bs')) tkErr bs
    tokenCase w                _   wbs k
        | W8.DIGIT_0 <= w, w <= W8.DIGIT_9    = scanNumberLiteral (\n bs' -> TkNumber n (k bs')) tkErr wbs
    tokenCase W8.LOWER_N       bs  _   k
        | Just bs1 <- stripPrefix "ull" 3 bs  = TkLit LitNull (k bs1)
    tokenCase W8.LOWER_T       bs  _   k
        | Just bs1 <- stripPrefix "rue" 3 bs  = TkLit LitTrue (k bs1)
    tokenCase W8.LOWER_F       bs  _   k
        | Just bs1 <- stripPrefix "alse" 4 bs = TkLit LitFalse (k bs1)
    tokenCase _                _   wbs _      = tkErr $ "Unexpected " ++ showBeginning wbs ++ ", expecting JSON value"

    goA :: Parser TkArray k
    goA (skipSpace -> bs) k = case LBS.uncons bs of
        Nothing         -> tkErrEOF "JSON value or ]"
        Just (W8.RIGHT_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (w,  !bs1) -> TkItem $ tokenCase w bs1 bs $ \bs2 -> goA1 bs2 k

    goA1 :: Parser TkArray k
    goA1 (skipSpace -> bs) k = case LBS.uncons bs of
        Nothing                      -> tkErrEOF ", or ]"
        Just (W8.RIGHT_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (W8.COMMA, !bs1)        -> TkItem $ goT bs1 $ \bs2 -> goA1 bs2 k
        _                            -> tkErrBS bs ", or ]"

    goR :: Parser TkRecord k
    goR (skipSpace -> bs) k = case LBS.uncons bs of
        Nothing                       -> tkErrEOF "record key literal or }"
        Just (W8.DOUBLE_QUOTE,  !bs1) -> goRK bs1 k           -- "
        Just (W8.RIGHT_CURLY, !bs1)   -> TkRecordEnd (k bs1)  -- }
        Just _                        -> tkErrBS bs "record key literal or }"

    goR1 :: Parser TkRecord k
    goR1 (skipSpace -> bs) k = case LBS.uncons bs of
        Nothing                           -> tkErr "Unexpected end-of-input, expecting , or }"
        Just (W8.COMMA, !bs1) -> case LBS.uncons (skipSpace bs1) of
            Nothing                      -> tkErrEOF "key literal"
            Just (W8.DOUBLE_QUOTE, !bs2) -> goRK bs2 k
            Just _                       -> tkErrBS bs "key literal"
        Just (W8.RIGHT_CURLY, !bs1)       -> TkRecordEnd (k bs1)
        _                                 -> tkErr $ "Unexpected " ++ showBeginning bs ++ ", expecting , or }"

    goRK :: Parser TkRecord k
    goRK bs1 k = scanStringLiteral (\t bs -> goRK' t bs k) tkErr bs1

    goRK' :: Text -> Parser TkRecord k
    goRK' t (skipSpace -> bs) k = case LBS.uncons bs of
        Nothing               -> tkErrEOF ":"
        Just (W8.COLON, !bs3) -> TkPair (Key.fromText t) $ goT bs3 $ \bs4 -> goR1 bs4 k
        Just _                -> tkErrBS bs ":"

stripPrefix :: ByteString -> Int -> ByteString -> Maybe ByteString
stripPrefix pfx n bs | LBS.isPrefixOf pfx bs = Just (LBS.drop (fromIntegral n) bs)
                     | otherwise             = Nothing

type Parser tk k = ByteString -> (ByteString -> k) -> tk k String

showBeginning :: ByteString -> String
showBeginning = show . LBS.take 30

skipSpace :: ByteString -> ByteString
skipSpace = LBS.dropWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09

tkErrEOF :: AsError t =>String ->  t k String
tkErrEOF expected = tkErr $
    "Unexpected end-of-input, expecting " ++ expected

tkErrBS :: AsError t => ByteString -> String ->  t k String
tkErrBS bs expected = tkErr $
    "Unexpected " ++ showBeginning bs ++ ", expecting " ++ expected

lbsTake :: Int -> ByteString -> BS.ByteString
lbsTake n bs = LBS.toStrict (LBS.take (fromIntegral n) bs)

lbsDrop :: Int -> ByteString -> ByteString
lbsDrop n = LBS.drop (fromIntegral n)


scanStringLiteral
    :: forall r. (Text -> ByteString -> r)
    -> (String -> r)
    -> ByteString
    -> r
scanStringLiteral ok err bs0 = go 0 bs0 where
    go :: Int -> ByteString -> r
    go !n !bs = case LBS.uncons bs of
        Nothing          -> errEnd
        Just (34, _)     -> ok (unsafeDecodeASCII (lbsTake n bs0)) (lbsDrop  (n + 1) bs0)
        Just (92, bs')   -> goSlash (n + 1) bs'
        Just (w8, bs')
            | w8 < 0x20  -> errCC
            | w8 >= 0x80 -> goEsc (n + 1) bs'
            | otherwise  -> go (n + 1) bs'

    goEsc :: Int -> ByteString -> r
    goEsc !n !bs = case LBS.uncons bs of
        Nothing        -> errEnd
        Just (34, _)   -> case unescapeText (lbsTake n bs0) of
            Right t -> ok t (lbsDrop (n + 1) bs0)
            Left e  -> err (show e)
        Just (92, bs') -> goSlash (n + 1) bs'
        Just (_,  bs') -> goEsc (n + 1) bs'

    goSlash :: Int -> ByteString -> r
    goSlash !n !bs = case LBS.uncons bs of
        Nothing       -> errEnd
        Just (_, bs') -> goEsc (n + 1) bs'

    errEnd = err "Unexpected end-of-input while parsing string literal"
    errCC  = err "Unespected control character while parsing string literal"


scanNumberLiteral
    :: forall r. (Number -> ByteString -> r)
    -> (String -> r)
    -> ByteString
    -> r
scanNumberLiteral kont err bs0 = state_start bs0 where
    state_start :: ByteString -> r
    state_start !bs = case LBS.uncons bs of
        Nothing                      -> errEnd
        Just (w8, bs')
            | W8.DIGIT_0 < w8, w8 <= W8.DIGIT_9  -> state_i1 1 bs'
            | W8.DIGIT_0 == w8             -> state_after0 bs'
            | otherwise              -> err $ "Unexpected " ++ show w8 ++ " while parsing number literal"

    state_after0 :: ByteString -> r
    state_after0 !bs = case LBS.uncons bs of
        Nothing                         -> kont (NumInteger 0) bs
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9   -> err "Number literal with leading zero"
            | W8.PERIOD == w8                      -> go_dec 0 bs'
            | W8.LOWER_E == w8 || W8.UPPER_E == w8 -> go_sci 0 0 bs'
            | otherwise                            -> kont (NumInteger 0) bs

    state_i1 :: Int -> ByteString -> r
    state_i1 !n !bs = case LBS.uncons bs of
        Nothing                         -> kont (NumInteger int) bs
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9   -> state_i1 (n + 1) bs'
            | W8.PERIOD == w8                      -> go_dec int bs'
            | W8.LOWER_E == w8 || W8.UPPER_E == w8 -> go_sci int 0 bs'
            | otherwise                            -> kont (NumInteger int) bs
      where
        int = byteStringToInteger (lbsTake n bs0)

    go_dec :: Integer -> ByteString -> r
    go_dec !int !bs1 = case LBS.uncons bs1 of
        Nothing                       -> errEnd
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9 -> state_dec 1 bs'
            | otherwise                          -> err $ "Unexpected " ++ show w8 ++ " while parsing number literal"
      where
        state_dec :: Int -> ByteString -> r
        state_dec !n !bs = case LBS.uncons bs of
            Nothing                         -> kont (NumDecimal dec) bs
            Just (w8, bs')
                | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9   -> state_dec (n + 1) bs'
                | W8.LOWER_E == w8 || W8.UPPER_E == w8 -> go_sci coef (negate n) bs'
                | otherwise                            -> kont (NumDecimal dec) bs
          where
            frac = byteStringToInteger (lbsTake n bs1)
            coef = int * 10 ^ n + frac
            dec  = Sci.scientific coef (negate n)

    go_sci :: Integer -> Int -> ByteString -> r
    go_sci !coef !exp10 !bs2 = case LBS.uncons bs2 of
        Nothing                       -> errEnd
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9  -> go_sci_pos coef exp10 bs2 1 bs'
            | W8.PLUS == w8           -> case LBS.uncons bs' of
                Nothing               -> errEnd
                Just (w8', bs'')
                    | W8.DIGIT_0 <= w8', w8' <= W8.DIGIT_9  -> go_sci_pos coef exp10 bs' 1 bs''
                    | otherwise       ->  errUnx w8'
            | W8.HYPHEN == w8         -> case LBS.uncons bs' of
                Nothing               -> errEnd
                Just (w8', bs'')
                    | W8.DIGIT_0 <= w8', w8' <= W8.DIGIT_9  -> go_sci_neg coef exp10 bs' 1 bs''
                    | otherwise       ->  errUnx w8'
            | otherwise               -> errUnx w8

    go_sci_pos :: Integer -> Int -> ByteString -> Int -> ByteString -> r
    go_sci_pos !coef !exp10 !bs2 !n !bs = case LBS.uncons bs of
        Nothing                       -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9  -> go_sci_pos coef exp10 bs2 (n + 1) bs'
            | otherwise               -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (byteStringToInteger (lbsTake n bs2))
        sci = Sci.scientific coef (exp10 + exp10')

    go_sci_neg :: Integer -> Int -> ByteString -> Int -> ByteString -> r
    go_sci_neg !coef !exp10 !bs2 !n !bs = case LBS.uncons bs of
        Nothing                       -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W8.DIGIT_0 <= w8, w8 <= W8.DIGIT_9  -> go_sci_neg coef exp10 bs2 (n + 1) bs'
            | otherwise               -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (byteStringToInteger (lbsTake n bs2))
        sci = Sci.scientific coef (exp10 - exp10')

    errEnd    = err "Unexpected end-of-input while parsing number literal"
    errUnx w8 = err $ "Unexpected " ++ show (chr (fromIntegral w8)) ++ " while parsing number literal"

module Data.Aeson.Decoding.Text (
    textToTokens,
) where

import           Data.Char                            (chr)
import           Data.Integer.Conversion              (textToInteger)
import           Data.Text.Internal                   (Text (..))

import qualified Data.Aeson.Key                       as Key
import qualified Data.Scientific                      as Sci
import qualified Data.Text                            as T
import qualified Data.Text.Array                      as A

import           Data.Aeson.Decoding.Internal
import           Data.Aeson.Decoding.Tokens
import           Data.Aeson.Internal.Prelude
import           Data.Aeson.Internal.UnescapeFromText (unescapeFromText)

#if MIN_VERSION_text(2,0,0)
import qualified Data.Word8.Patterns as W
#else
import qualified Data.Word16.Patterns as W
#endif

#if MIN_VERSION_text(2,0,0)
type Point = Word8
#else
type Point = Word16
#endif


textToTokens :: Text -> Tokens Text String
textToTokens bs0 = goT bs0 id where
    goT :: Parser Tokens k
    goT (skipSpace -> bs) k = case unconsPoint bs of
        Nothing         -> tkErr "Unexpected end-of-input, expecting JSON value"
        Just (!w, !bs1) -> tokenCase w bs1 bs k

    tokenCase
        :: Point              -- head
        -> Text               -- tail
        -> Text               -- whole input, needed for number parsing
        -> (Text -> k)        -- continuation
        -> Tokens k String
    tokenCase W.LEFT_CURLY   !bs !_   k       = TkRecordOpen (goR bs k)
    tokenCase W.LEFT_SQUARE   bs  _   k       = TkArrayOpen (goA bs k)
    tokenCase W.DOUBLE_QUOTE  bs  _   k       = scanStringLiteral (\t bs' -> TkText t (k bs')) tkErr bs
    tokenCase W.HYPHEN        bs  _   k       = scanNumberLiteral (\n bs' -> TkNumber (negateNumber n) (k bs')) tkErr bs
    tokenCase w                _   wbs k
        | W.DIGIT_0 <= w, w <= W.DIGIT_9      = scanNumberLiteral (\n bs' -> TkNumber n (k bs')) tkErr wbs
    tokenCase W.LOWER_N       bs  _   k
        | Just bs1 <- stripPrefix "ull" 3 bs  = TkLit LitNull (k bs1)
    tokenCase W.LOWER_T       bs  _   k
        | Just bs1 <- stripPrefix "rue" 3 bs  = TkLit LitTrue (k bs1)
    tokenCase W.LOWER_F       bs  _   k
        | Just bs1 <- stripPrefix "alse" 4 bs = TkLit LitFalse (k bs1)
    tokenCase _          _   wbs _            = tkErr $ "Unexpected " ++ showBeginning wbs ++ ", expecting JSON value"
    goA :: Parser TkArray k
    goA (skipSpace -> bs) k = case unconsPoint bs of
        Nothing         -> tkErrEOF "JSON value or ]"
        Just (W.RIGHT_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (w,  !bs1) -> TkItem $ tokenCase w bs1 bs $ \bs2 -> goA1 bs2 k

    goA1 :: Parser TkArray k
    goA1 (skipSpace -> bs) k = case unconsPoint bs of
        Nothing                      -> tkErrEOF ", or ]"
        Just (W.RIGHT_SQUARE, !bs1) -> TkArrayEnd (k bs1)
        Just (W.COMMA, !bs1)        -> TkItem $ goT bs1 $ \bs2 -> goA1 bs2 k
        _                            -> tkErrBS bs ", or ]"

    goR :: Parser TkRecord k
    goR (skipSpace -> bs) k = case unconsPoint bs of
        Nothing                       -> tkErrEOF "record key literal or }"
        Just (W.DOUBLE_QUOTE,  !bs1) -> goRK bs1 k           -- "
        Just (W.RIGHT_CURLY, !bs1)   -> TkRecordEnd (k bs1)  -- }
        Just _                        -> tkErrBS bs "record key literal or }"

    goR1 :: Parser TkRecord k
    goR1 (skipSpace -> bs) k = case unconsPoint bs of
        Nothing                           -> tkErr "Unexpected end-of-input, expecting , or }"
        Just (W.COMMA, !bs1) -> case unconsPoint (skipSpace bs1) of
            Nothing                      -> tkErrEOF "key literal"
            Just (W.DOUBLE_QUOTE, !bs2) -> goRK bs2 k
            Just _                       -> tkErrBS bs "key literal"
        Just (W.RIGHT_CURLY, !bs1)       -> TkRecordEnd (k bs1)
        _                                 -> tkErr $ "Unexpected " ++ showBeginning bs ++ ", expecting , or }"

    goRK :: Parser TkRecord k
    goRK bs1 k = scanStringLiteral (\t bs -> goRK' t bs k) tkErr bs1

    goRK' :: Text -> Parser TkRecord k
    goRK' t (skipSpace -> bs) k = case T.uncons bs of
        Nothing          -> tkErrEOF ":"
        Just (':', !bs3) -> TkPair (Key.fromText t) $ goT bs3 $ \bs4 -> goR1 bs4 k
        Just _           -> tkErrBS bs ":"

stripPrefix :: Text -> Int -> Text -> Maybe Text
stripPrefix pfx _ bs = T.stripPrefix pfx bs

type Parser tk k = Text -> (Text -> k) -> tk k String

showBeginning :: Text -> String
showBeginning = show . T.take 30

skipSpace :: Text -> Text
skipSpace = T.dropWhile $ \w -> w == '\x20' || w == '\x0a' || w == '\x0d' || w == '\x09'

tkErrEOF :: AsError t => String ->  t k String
tkErrEOF expected = tkErr $
    "Unexpected end-of-input, expecting " ++ expected

tkErrBS :: AsError t => Text -> String ->  t k String
tkErrBS bs expected = tkErr $
    "Unexpected " ++ showBeginning bs ++ ", expecting " ++ expected


scanStringLiteral
    :: forall r. (Text -> Text -> r)
    -> (String -> r)
    -> Text
    -> r
scanStringLiteral ok err bs0 = go 0 bs0 where
    go :: Int -> Text -> r
    go !n !bs = case unconsPoint bs of
        Nothing          -> errEnd
        Just (34, _)     -> ok (unsafeTakePoints n bs0) (unsafeDropPoints (n + 1) bs0)
        Just (92, bs')   -> goSlash (n + 1) bs'
        Just (w8, bs')
            | w8 < 0x20  -> errCC
            | otherwise  -> go (n + 1) bs'

    goEsc :: Int -> Text -> r
    goEsc !n !bs = case unconsPoint bs of
        Nothing        -> errEnd
        Just (34, _)   -> case unescapeFromText (unsafeTakePoints n bs0) of
            Right t -> ok t (unsafeDropPoints (n + 1) bs0)
            Left e  -> err (show e)
        Just (92, bs') -> goSlash (n + 1) bs'
        Just (_,  bs') -> goEsc (n + 1) bs'

    goSlash :: Int -> Text -> r
    goSlash !n !bs = case unconsPoint bs of
        Nothing       -> errEnd
        Just (_, bs') -> goEsc (n + 1) bs'

    errEnd = err "Unexpected end-of-input while parsing string literal"
    errCC  = err "Unespected control character while parsing string literal"


scanNumberLiteral
    :: forall r. (Number -> Text -> r)
    -> (String -> r)
    -> Text
    -> r
scanNumberLiteral kont err bs0 = state_start bs0 where
    state_start :: Text -> r
    state_start !bs = case unconsPoint bs of
        Nothing                                   -> errEnd
        Just (w8, bs')
            | W.DIGIT_0 < w8, w8 <= W.DIGIT_9     -> state_i1 1 bs'
            | W.DIGIT_0 == w8                     -> state_after0 bs'
            | otherwise                           -> errUnx w8

    state_after0 :: Text -> r
    state_after0 !bs = case unconsPoint bs of
        Nothing                                   -> kont (NumInteger 0) bs
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9    -> err "Number literal with leading zero"
            | W.PERIOD == w8                      -> go_dec 0 bs'
            | W.LOWER_E == w8 || W.UPPER_E == w8  -> go_sci 0 0 bs'
            | otherwise                           -> kont (NumInteger 0) bs

    state_i1 :: Int -> Text -> r
    state_i1 !n !bs = case unconsPoint bs of
        Nothing                                   -> kont (NumInteger int) bs
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9    -> state_i1 (n + 1) bs'
            | W.PERIOD == w8                      -> go_dec int bs'
            | W.LOWER_E == w8 || W.UPPER_E == w8  -> go_sci int 0 bs'
            | otherwise                           -> kont (NumInteger int) bs
      where
        int = textToInteger (unsafeTakePoints n bs0)

    go_dec :: Integer -> Text -> r
    go_dec !int !bs1 = case unconsPoint bs1 of
        Nothing                                   -> errEnd
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9    -> state_dec 1 bs'
            | otherwise                           -> errUnx w8
      where
        state_dec :: Int -> Text -> r
        state_dec !n !bs = case unconsPoint bs of
            Nothing                                   -> kont (NumDecimal dec) bs
            Just (w8, bs')
                | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9    -> state_dec (n + 1) bs'
                | W.LOWER_E == w8 || W.UPPER_E == w8  -> go_sci coef (negate n) bs'
                | otherwise                           -> kont (NumDecimal dec) bs
          where
            frac = textToInteger (unsafeTakePoints n bs1)
            coef = int * 10 ^ n + frac
            dec  = Sci.scientific coef (negate n)

    go_sci :: Integer -> Int -> Text -> r
    go_sci !coef !exp10 !bs2 = case unconsPoint bs2 of
        Nothing                                           -> errEnd
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9            -> go_sci_pos coef exp10 bs2 1 bs'
            | W.PLUS == w8 -> case unconsPoint bs' of
                Nothing                                   -> errEnd
                Just (w8', bs'')
                    | W.DIGIT_0 <= w8', w8' <= W.DIGIT_9  -> go_sci_pos coef exp10 bs' 1 bs''
                    | otherwise                           -> errUnx w8'
            | W.HYPHEN == w8 -> case unconsPoint bs' of
                Nothing                                   -> errEnd
                Just (w8', bs'')
                    | W.DIGIT_0 <= w8', w8' <= W.DIGIT_9  -> go_sci_neg coef exp10 bs' 1 bs''
                    | otherwise                           -> errUnx w8'
            | otherwise                                   -> errUnx w8

    go_sci_pos :: Integer -> Int -> Text -> Int -> Text -> r
    go_sci_pos !coef !exp10 !bs2 !n !bs = case unconsPoint bs of
        Nothing                                 -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9  -> go_sci_pos coef exp10 bs2 (n + 1) bs'
            | otherwise                         -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (textToInteger (unsafeTakePoints n bs2))
        sci = Sci.scientific coef (exp10 + exp10')

    go_sci_neg :: Integer -> Int -> Text -> Int -> Text -> r
    go_sci_neg !coef !exp10 !bs2 !n !bs = case unconsPoint bs of
        Nothing                                 -> kont (NumScientific sci) bs
        Just (w8, bs')
            | W.DIGIT_0 <= w8, w8 <= W.DIGIT_9  -> go_sci_neg coef exp10 bs2 (n + 1) bs'
            | otherwise                         -> kont (NumScientific sci) bs
      where
        exp10' = fromInteger (textToInteger (unsafeTakePoints n bs2))
        sci = Sci.scientific coef (exp10 - exp10')

    errEnd    = err "Unexpected end-of-input while parsing number literal"
    errUnx w8 = err $ "Unexpected " ++ show (chr (fromIntegral w8)) ++ " while parsing number literal"


unconsPoint :: Text -> Maybe (Point, Text)
unconsPoint (Text arr off len)
    | len <= 0  = Nothing
    | otherwise = Just (w8, Text arr (off + 1) (len - 1))
  where
    w8 = A.unsafeIndex arr off

unsafeTakePoints :: Int -> Text -> Text
unsafeTakePoints n (Text arr off _len) = Text arr off n

unsafeDropPoints :: Int -> Text -> Text
unsafeDropPoints n (Text arr off len) = Text arr (off + n) (len - n)

module Data.Aeson.Decoding.Tokens (
    Tokens (..),
    Lit (..),
    Number (..),
    TkArray (..),
    TkRecord (..),
) where

import           Data.Aeson.Key            (Key)
import           Data.Bifoldable           (Bifoldable (..))
import           Data.Bifunctor            (Bifunctor (..))
import           Data.Bitraversable        (Bitraversable (..), bifoldMapDefault, bimapDefault)
import           Data.Scientific           (Scientific)
import           Data.Text                 (Text)

data Tokens k e
    = TkLit !Lit k
    | TkText !Text k
    | TkNumber !Number k
    | TkArrayOpen (TkArray k e)
    | TkRecordOpen (TkRecord k e)
    | TkErr e
  deriving (Eq, Show)

data Lit = LitNull | LitTrue | LitFalse
  deriving (Eq, Show)

data Number
    = NumInteger !Integer  -- ^ e.g. @123@
    | NumDecimal !Scientific  -- ^ e.g. @123.456@
    | NumScientific !Scientific -- ^ e.g. @123e456@, @123e-456@ or @123.456E-967@
  deriving (Eq, Show)

data TkArray k e
    = TkItem (Tokens (TkArray k e) e)
    | TkArrayEnd k
    | TkArrayErr e
  deriving (Eq, Show)

data TkRecord k e
    = TkPair !Key (Tokens (TkRecord k e) e)
    | TkRecordEnd k
    | TkRecordErr e
  deriving (Eq, Show)

instance Functor (Tokens k) where fmap = second
instance Functor (TkArray k) where fmap = second
instance Functor (TkRecord k) where fmap = second

instance Foldable (Tokens k) where foldMap = bifoldMap (const mempty)
instance Foldable (TkArray k) where foldMap = bifoldMap (const mempty)
instance Foldable (TkRecord k) where foldMap = bifoldMap (const mempty)

instance Traversable (Tokens k) where traverse = bitraverse pure
instance Traversable (TkArray k) where traverse = bitraverse pure
instance Traversable (TkRecord k) where traverse = bitraverse pure

instance Bifunctor Tokens where bimap = bimapDefault
instance Bifunctor TkArray where bimap = bimapDefault
instance Bifunctor TkRecord where bimap = bimapDefault

instance Bifoldable Tokens where bifoldMap = bifoldMapDefault
instance Bifoldable TkArray where bifoldMap = bifoldMapDefault
instance Bifoldable TkRecord where bifoldMap = bifoldMapDefault

instance Bitraversable Tokens where
    bitraverse f _ (TkLit l k)       = TkLit l <$> f k
    bitraverse f _ (TkText t k)      = TkText t <$> f k
    bitraverse f _ (TkNumber n k)    = TkNumber n <$> f k
    bitraverse f g (TkArrayOpen ts)  = TkArrayOpen <$> bitraverse f g ts
    bitraverse f g (TkRecordOpen ts) = TkRecordOpen <$> bitraverse f g ts
    bitraverse _ g (TkErr e)         = TkErr <$> g e

instance Bitraversable TkArray where
    bitraverse f g (TkItem ts)    = TkItem <$> bitraverse (bitraverse f g) g ts
    bitraverse f _ (TkArrayEnd k) = TkArrayEnd <$> f k
    bitraverse _ g (TkArrayErr e) = TkArrayErr <$> g e

instance Bitraversable TkRecord where
    bitraverse f g (TkPair k ts)   = TkPair k <$> bitraverse (bitraverse f g) g ts
    bitraverse f _ (TkRecordEnd k) = TkRecordEnd <$> f k
    bitraverse _ g (TkRecordErr e) = TkRecordErr <$> g e


module Data.Aeson.Encoding.Internal
    (
      Encoding' (..)
    , Encoding
    , encodingToLazyByteString
    , unsafeToEncoding
    , retagEncoding
    , Series (..)
    , pairs
    , pair
    , pairStr
    , unsafePairSBS
    , pair'
    , nullEncoding
    , emptyArray_
    , emptyObject_
    , wrapObject
    , wrapArray
    , null_
    , bool
    , key
    , text
    , lazyText
    , shortText
    , string
    , list
    , dict
    , tuple
    , (>*<)
    , InArray
    , empty
    , (><)
    , econcat
    , int8, int16, int32, int64, int
    , word8, word16, word32, word64, word
    , integer, float, double, scientific
    , int8Text, int16Text, int32Text, int64Text, intText
    , word8Text, word16Text, word32Text, word64Text, wordText
    , integerText, floatText, doubleText, scientificText
    , day
    , month
    , quarter
    , localTime
    , utcTime
    , timeOfDay
    , zonedTime
    , value
    , comma, colon, openBracket, closeBracket, openCurly, closeCurly
    ) where

import Data.Aeson.Internal.Prelude hiding (empty)

import Data.Aeson.Types.Internal (Value, Key)
import Data.ByteString.Builder (Builder, char7, toLazyByteString)
import Data.ByteString.Short (ShortByteString)
import qualified Data.Aeson.Key as Key
import Data.Time (Day, LocalTime, TimeOfDay, ZonedTime)
import Data.Time.Calendar.Month.Compat (Month)
import Data.Time.Calendar.Quarter.Compat (Quarter)
import qualified Data.Aeson.Encoding.Builder as EB
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Short as ST

newtype Encoding' tag = Encoding {
      fromEncoding :: Builder
    } deriving (Typeable)

type Encoding = Encoding' Value

unsafeToEncoding :: Builder -> Encoding' a
unsafeToEncoding = Encoding

encodingToLazyByteString :: Encoding' a -> BSL.ByteString
encodingToLazyByteString = toLazyByteString . fromEncoding

retagEncoding :: Encoding' a -> Encoding' b
retagEncoding = Encoding . fromEncoding


instance Show (Encoding' a) where
    show (Encoding e) = show (toLazyByteString e)

instance Eq (Encoding' a) where
    Encoding a == Encoding b = toLazyByteString a == toLazyByteString b

instance Ord (Encoding' a) where
    compare (Encoding a) (Encoding b) =
      compare (toLazyByteString a) (toLazyByteString b)

instance IsString (Encoding' a) where
  fromString = string

data Series = Empty
            | Value (Encoding' Series)
            deriving (Typeable)

pair :: Key -> Encoding -> Series
pair name val = pair' (key name) val

pairStr :: String -> Encoding -> Series
pairStr name val = pair' (string name) val

pair' :: Encoding' Key -> Encoding -> Series
pair' name val = Value $ retagEncoding $ retagEncoding name >< colon >< val

unsafePairSBS :: ShortByteString -> Encoding -> Series
unsafePairSBS k v = Value $ retagEncoding $ Encoding (B.shortByteString k) >< v

instance Semigroup Series where
    Empty   <> a = a
    Value a <> b = Value $ a >< case b of
        Empty   -> empty
        Value x -> comma >< x

instance Monoid Series where
    mempty  = Empty
    mappend = (<>)

nullEncoding :: Encoding' a -> Bool
nullEncoding = BSL.null . toLazyByteString . fromEncoding

emptyArray_ :: Encoding
emptyArray_ = Encoding EB.emptyArray_

emptyObject_ :: Encoding
emptyObject_ = Encoding EB.emptyObject_

wrapArray :: Encoding' a -> Encoding
wrapArray e = retagEncoding $ openBracket >< e >< closeBracket

wrapObject :: Encoding' a -> Encoding
wrapObject e = retagEncoding $ openCurly >< e >< closeCurly

null_ :: Encoding
null_ = Encoding EB.null_

bool :: Bool -> Encoding
bool True = Encoding "true"
bool False = Encoding "false"

pairs :: Series -> Encoding
pairs (Value v) = openCurly >< retagEncoding v >< closeCurly
pairs Empty     = emptyObject_

list :: (a -> Encoding) -> [a] -> Encoding
list _  []     = emptyArray_
list to' (x:xs) = openBracket >< to' x >< commas xs >< closeBracket
  where
    commas = foldr (\v vs -> comma >< to' v >< vs) empty

dict
    :: (k -> Encoding' Key)                           -- ^ key encoding
    -> (v -> Encoding)                                -- ^ value encoding
    -> (forall a. (k -> v -> a -> a) -> a -> m -> a)  -- ^ @foldrWithKey@ - indexed fold
    -> m                                              -- ^ container
    -> Encoding
dict encodeKey encodeVal foldrWithKey = pairs . foldrWithKey go mempty
  where
    go k v c = Value (encodeKV k v) <> c
    encodeKV k v = retagEncoding (encodeKey k) >< colon >< retagEncoding (encodeVal v)

data InArray

infixr 6 >*<
(>*<) :: Encoding' a -> Encoding' b -> Encoding' InArray
a >*< b = retagEncoding a >< comma >< retagEncoding b

empty :: Encoding' a
empty = Encoding mempty

econcat :: [Encoding' a] -> Encoding' a
econcat = foldr (><) empty

infixr 6 ><
(><) :: Encoding' a -> Encoding' a -> Encoding' a
Encoding a >< Encoding b = Encoding (a <> b)

tuple :: Encoding' InArray -> Encoding
tuple b = retagEncoding $ openBracket >< b >< closeBracket

key :: Key -> Encoding' a
key = text . Key.toText

text :: Text -> Encoding' a
text = Encoding . EB.text

lazyText :: LT.Text -> Encoding' a
lazyText t = Encoding $
    B.char7 '"' <>
    LT.foldrChunks (\x xs -> EB.unquoted x <> xs) (B.char7 '"') t

shortText :: ST.ShortText -> Encoding' a
shortText t = Encoding $
    B.char7 '"' <>
    EB.unquoted (ST.toText t) <> B.char7 '"'

string :: String -> Encoding' a
string = Encoding . EB.string


comma, colon, openBracket, closeBracket, openCurly, closeCurly :: Encoding' a
comma        = Encoding $ char7 ','
colon        = Encoding $ char7 ':'
openBracket  = Encoding $ char7 '['
closeBracket = Encoding $ char7 ']'
openCurly    = Encoding $ char7 '{'
closeCurly   = Encoding $ char7 '}'


int8 :: Int8 -> Encoding
int8 = Encoding . B.int8Dec

int16 :: Int16 -> Encoding
int16 = Encoding . B.int16Dec

int32 :: Int32 -> Encoding
int32 = Encoding . B.int32Dec

int64 :: Int64 -> Encoding
int64 = Encoding . B.int64Dec

int :: Int -> Encoding
int = Encoding . B.intDec

word8 :: Word8 -> Encoding
word8 = Encoding . B.word8Dec

word16 :: Word16 -> Encoding
word16 = Encoding . B.word16Dec

word32 :: Word32 -> Encoding
word32 = Encoding . B.word32Dec

word64 :: Word64 -> Encoding
word64 = Encoding . B.word64Dec

word :: Word -> Encoding
word = Encoding . B.wordDec

integer :: Integer -> Encoding
integer = Encoding . B.integerDec

float :: Float -> Encoding
float = realFloatToEncoding $ Encoding . B.floatDec

double :: Double -> Encoding
double = realFloatToEncoding $ Encoding . B.doubleDec

scientific :: Scientific -> Encoding
scientific = Encoding . EB.scientific

realFloatToEncoding :: RealFloat a => (a -> Encoding) -> a -> Encoding
realFloatToEncoding e d
    | isNaN d      = null_
    | isInfinite d = if d > 0 then Encoding "\"+inf\"" else Encoding "\"-inf\""
    | otherwise    = e d


int8Text :: Int8 -> Encoding' a
int8Text = Encoding . EB.quote . B.int8Dec

int16Text :: Int16 -> Encoding' a
int16Text = Encoding . EB.quote . B.int16Dec

int32Text :: Int32 -> Encoding' a
int32Text = Encoding . EB.quote . B.int32Dec

int64Text :: Int64 -> Encoding' a
int64Text = Encoding . EB.quote . B.int64Dec

intText :: Int -> Encoding' a
intText = Encoding . EB.quote . B.intDec

word8Text :: Word8 -> Encoding' a
word8Text = Encoding . EB.quote . B.word8Dec

word16Text :: Word16 -> Encoding' a
word16Text = Encoding . EB.quote . B.word16Dec

word32Text :: Word32 -> Encoding' a
word32Text = Encoding . EB.quote . B.word32Dec

word64Text :: Word64 -> Encoding' a
word64Text = Encoding . EB.quote . B.word64Dec

wordText :: Word -> Encoding' a
wordText = Encoding . EB.quote . B.wordDec

integerText :: Integer -> Encoding' a
integerText = Encoding . EB.quote . B.integerDec

floatText :: Float -> Encoding' a
floatText d
    | isInfinite d = if d > 0 then Encoding "\"+inf\"" else Encoding "\"-inf\""
    | otherwise = Encoding . EB.quote . B.floatDec $ d

doubleText :: Double -> Encoding' a
doubleText d
    | isInfinite d = if d > 0 then Encoding "\"+inf\"" else Encoding "\"-inf\""
    | otherwise = Encoding . EB.quote . B.doubleDec $ d

scientificText :: Scientific -> Encoding' a
scientificText = Encoding . EB.quote . EB.scientific


day :: Day -> Encoding' a
day = Encoding . EB.quote . EB.day

month :: Month -> Encoding' a
month = Encoding . EB.quote . EB.month

quarter :: Quarter -> Encoding' a
quarter = Encoding . EB.quote . EB.quarter

localTime :: LocalTime -> Encoding' a
localTime = Encoding . EB.quote . EB.localTime

utcTime :: UTCTime -> Encoding' a
utcTime = Encoding . EB.quote . EB.utcTime

timeOfDay :: TimeOfDay -> Encoding' a
timeOfDay = Encoding . EB.quote . EB.timeOfDay

zonedTime :: ZonedTime -> Encoding' a
zonedTime = Encoding . EB.quote . EB.zonedTime


value :: Value -> Encoding
value = Encoding . EB.encodeToBuilder



module Data.Aeson.Key (
    Key,
    fromString,
    toString,
    toText,
    fromText,
    coercionToText,
    toShortText,
    fromShortText,
) where

import Prelude (Eq, Ord, (.), Show (..), String, Maybe (..))

import Control.Applicative ((<$>))
import Control.DeepSeq (NFData(..))
import Data.Data (Data)
import Data.Hashable (Hashable(..))
import Data.Monoid (Monoid(mempty, mappend))
import Data.Semigroup (Semigroup((<>)))
import Data.Text (Text)
import Data.Type.Coercion (Coercion (..))
import Data.Typeable (Typeable)
import Text.Read (Read (..))

import qualified Data.String
import qualified Data.Text as T
import qualified Data.Text.Short as ST
import qualified Language.Haskell.TH.Syntax as TH
import qualified Test.QuickCheck as QC

newtype Key = Key { unKey :: Text }
  deriving (Typeable, Data)

fromString :: String -> Key
fromString = Key . T.pack

toString :: Key -> String
toString (Key k) = T.unpack k

fromText :: Text -> Key
fromText = Key

toText :: Key -> Text
toText = unKey

coercionToText :: Maybe (Coercion Key Text)
coercionToText = Just Coercion

toShortText :: Key -> ST.ShortText
toShortText = ST.fromText . unKey

fromShortText :: ST.ShortText -> Key
fromShortText = Key . ST.toText


instance Read Key where
    readPrec = fromString <$> readPrec

instance Show Key where
    showsPrec d (Key k) = showsPrec d k

instance Data.String.IsString Key where
    fromString = fromString

deriving newtype instance Eq Key
deriving newtype instance Ord Key
deriving newtype instance Hashable Key

instance NFData Key where
    rnf (Key k) = rnf k

instance Semigroup Key where
    Key x <> Key y = Key (x <> y)

instance Monoid Key where
    mempty = Key mempty
    mappend = (<>)

instance TH.Lift Key where
#if MIN_VERSION_text(1,2,4)
    lift (Key k) = [| Key k |]
#else
    lift k = [| fromString k' |] where k' = toString k
#endif

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

instance QC.Arbitrary Key where
    arbitrary = fromString <$> QC.arbitrary
    shrink k  = fromString <$> QC.shrink (toString k)

instance QC.CoArbitrary Key where
    coarbitrary = QC.coarbitrary . toString

instance QC.Function Key where
    function = QC.functionMap toString fromString



module Data.Aeson.KeyMap (
    KeyMap,

    null,
    lookup,
    (!?),
    size,
    member,

    empty,
    singleton,

    insert,
    insertWith,

    delete,

    alterF,

    difference,
    union,
    unionWith,
    unionWithKey,
    intersection,
    intersectionWith,
    intersectionWithKey,
    alignWith,
    alignWithKey,

    fromList,
    fromListWith,
    toList,
    toAscList,
    elems,

    fromHashMap,
    toHashMap,
    fromHashMapText,
    toHashMapText,
    coercionToHashMap,
    fromMap,
    toMap,
    fromMapText,
    toMapText,
    coercionToMap,

    map,
    mapWithKey,
    mapKeyVal,
    traverse,
    traverseWithKey,

    foldr,
    foldr',
    foldl,
    foldl',
    foldMapWithKey,
    foldrWithKey,

    keys,

    filter,
    filterWithKey,
    mapMaybe,
    mapMaybeWithKey,

    Key,
) where

import Prelude (Eq(..), Ord((>)), Int, Bool(..), Maybe(..))
import Prelude ((.), ($))
import Prelude (Functor(fmap), Monad(..))
import Prelude (Show, showsPrec, showParen, shows, showString)

import Control.Applicative (Applicative)
import Control.DeepSeq (NFData(..))
import Data.Aeson.Key (Key)
import Data.Bifunctor (first)
import Data.Data (Data)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Monoid (Monoid(mempty, mappend))
import Data.Semigroup (Semigroup((<>)))
import Data.Text (Text)
import Data.These (These (..))
import Data.Type.Coercion (Coercion (..))
import Data.Typeable (Typeable)
import Text.Read (Read (..), Lexeme(..), readListPrecDefault, prec, lexP, parens)

import qualified Data.Aeson.Key as Key
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import qualified Data.HashMap.Strict as H
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.Foldable.WithIndex    as WI (FoldableWithIndex (..))
import qualified Data.Functor.WithIndex     as WI (FunctorWithIndex (..))
import qualified Data.Traversable.WithIndex as WI (TraversableWithIndex (..))
import qualified Data.Semialign as SA
import qualified Data.Semialign.Indexed as SAI
import qualified GHC.Exts
import qualified Test.QuickCheck as QC
import qualified Witherable as W

#ifdef USE_ORDEREDMAP


newtype KeyMap v = KeyMap { unKeyMap :: Map Key v }
  deriving (Eq, Ord, Typeable, Data, Functor)


empty :: KeyMap v
empty = KeyMap M.empty

null :: KeyMap v -> Bool
null = M.null . unKeyMap

size :: KeyMap v -> Int
size = M.size . unKeyMap

singleton :: Key -> v -> KeyMap v
singleton k v = KeyMap (M.singleton k v)

member :: Key -> KeyMap a -> Bool
member t (KeyMap m) = M.member t m

delete :: Key -> KeyMap v -> KeyMap v
delete k (KeyMap m) = KeyMap (M.delete k m)

alterF :: Functor f => (Maybe v -> f (Maybe v)) -> Key -> KeyMap v -> f (KeyMap v)
alterF f k = fmap KeyMap . M.alterF f k . unKeyMap

lookup :: Key -> KeyMap v -> Maybe v
lookup t tm = M.lookup t (unKeyMap tm)

insert :: Key -> v -> KeyMap v -> KeyMap v
insert k v tm = KeyMap (M.insert k v (unKeyMap tm))

insertWith :: (a -> a -> a) -> Key -> a -> KeyMap a -> KeyMap a
insertWith f k v m = KeyMap (M.insertWith f k v (unKeyMap m))

map :: (a -> b) -> KeyMap a -> KeyMap b
map = fmap

mapWithKey :: (Key -> a -> b) -> KeyMap a -> KeyMap b
mapWithKey f (KeyMap m) = KeyMap (M.mapWithKey f m)

foldMapWithKey :: Monoid m => (Key -> a -> m) -> KeyMap a -> m
foldMapWithKey f (KeyMap m) = M.foldMapWithKey f m

foldr :: (a -> b -> b) -> b -> KeyMap a -> b
foldr f z (KeyMap m) = M.foldr f z m

foldr' :: (a -> b -> b) -> b -> KeyMap a -> b
foldr' f z (KeyMap m) = M.foldr' f z m

foldl :: (b -> a -> b) -> b -> KeyMap a -> b
foldl f z (KeyMap m) = M.foldl f z m

foldl' :: (b -> a -> b) -> b -> KeyMap a -> b
foldl' f z (KeyMap m) = M.foldl' f z m

foldrWithKey :: (Key -> v -> a -> a) -> a -> KeyMap v -> a
foldrWithKey f a = M.foldrWithKey f a . unKeyMap

traverse :: Applicative f => (v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverse f = fmap KeyMap . T.traverse f . unKeyMap

traverseWithKey :: Applicative f => (Key -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseWithKey f = fmap KeyMap . M.traverseWithKey f  . unKeyMap

fromListWith :: (v -> v -> v) ->  [(Key, v)] -> KeyMap v
fromListWith op = KeyMap . M.fromListWith op

fromList :: [(Key, v)] -> KeyMap v
fromList = KeyMap . M.fromList

toList :: KeyMap v -> [(Key, v)]
toList = M.toList . unKeyMap

elems :: KeyMap v -> [v]
elems = M.elems . unKeyMap

toAscList :: KeyMap v -> [(Key, v)]
toAscList = M.toAscList . unKeyMap

difference :: KeyMap v -> KeyMap v' -> KeyMap v
difference tm1 tm2 = KeyMap (M.difference (unKeyMap tm1) (unKeyMap tm2))

union :: KeyMap v -> KeyMap v -> KeyMap v
union (KeyMap x) (KeyMap y) = KeyMap (M.union x y)

unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith f (KeyMap x) (KeyMap y) = KeyMap (M.unionWith f x y)

unionWithKey :: (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey f (KeyMap x) (KeyMap y) = KeyMap (M.unionWithKey f x y)

intersection :: KeyMap a -> KeyMap b -> KeyMap a
intersection (KeyMap x) (KeyMap y) = KeyMap (M.intersection x y)

intersectionWith :: (a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWith f (KeyMap x) (KeyMap y) = KeyMap (M.intersectionWith f x y)

intersectionWithKey :: (Key -> a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWithKey f (KeyMap x) (KeyMap y) = KeyMap (M.intersectionWithKey f x y)

keys :: KeyMap v -> [Key]
keys = M.keys . unKeyMap

toHashMap :: KeyMap v -> HashMap Key v
toHashMap = H.fromList . toList

fromHashMap :: HashMap Key v -> KeyMap v
fromHashMap = fromList . H.toList

toMap :: KeyMap v -> Map Key v
toMap = unKeyMap

fromMap :: Map Key v -> KeyMap v
fromMap = KeyMap

coercionToHashMap :: Maybe (Coercion (HashMap Key v) (KeyMap v))
coercionToHashMap = Nothing

coercionToMap :: Maybe (Coercion (Map Key v) (KeyMap v))
coercionToMap = Just Coercion

mapKeyVal :: (Key -> Key) -> (v1 -> v2)
          -> KeyMap v1 -> KeyMap v2
mapKeyVal fk kv = foldrWithKey (\k v -> insert (fk k) (kv v)) empty

filter :: (v -> Bool) -> KeyMap v -> KeyMap v
filter f (KeyMap m) = KeyMap (M.filter f m)

filterWithKey :: (Key -> v -> Bool) -> KeyMap v -> KeyMap v
filterWithKey f (KeyMap m) = KeyMap (M.filterWithKey f m)

mapMaybe :: (a -> Maybe b) -> KeyMap a -> KeyMap b
mapMaybe f (KeyMap m) = KeyMap (M.mapMaybe f m)

mapMaybeWithKey :: (Key -> v -> Maybe u) -> KeyMap v -> KeyMap u
mapMaybeWithKey f (KeyMap m) = KeyMap (M.mapMaybeWithKey f m)

#else


import Data.List (sortBy)
import Data.Ord (comparing)
import Prelude (fst)

newtype KeyMap v = KeyMap { unKeyMap :: HashMap Key v }
  deriving (Eq, Ord, Typeable, Data, Functor)

empty :: KeyMap v
empty = KeyMap H.empty

null :: KeyMap v -> Bool
null = H.null . unKeyMap

size :: KeyMap v -> Int
size = H.size . unKeyMap

singleton :: Key -> v -> KeyMap v
singleton k v = KeyMap (H.singleton k v)

member :: Key -> KeyMap a -> Bool
member t (KeyMap m) = H.member t m

delete :: Key -> KeyMap v -> KeyMap v
delete k (KeyMap m) = KeyMap (H.delete k m)

alterF :: Functor f => (Maybe v -> f (Maybe v)) -> Key -> KeyMap v -> f (KeyMap v)
alterF f k = fmap KeyMap . H.alterF f k . unKeyMap

lookup :: Key -> KeyMap v -> Maybe v
lookup t tm = H.lookup t (unKeyMap tm)

insert :: Key -> v -> KeyMap v -> KeyMap v
insert k v tm = KeyMap (H.insert k v (unKeyMap tm))

insertWith :: (a -> a -> a) -> Key -> a -> KeyMap a -> KeyMap a
insertWith f k v m = KeyMap (H.insertWith f k v (unKeyMap m))

map :: (a -> b) -> KeyMap a -> KeyMap b
map = fmap

mapWithKey :: (Key -> a -> b) -> KeyMap a -> KeyMap b
mapWithKey f (KeyMap m) = KeyMap (H.mapWithKey f m)

foldMapWithKey :: Monoid m => (Key -> a -> m) -> KeyMap a -> m
foldMapWithKey f (KeyMap m) = H.foldMapWithKey f m

foldr :: (a -> b -> b) -> b -> KeyMap a -> b
foldr f z (KeyMap m) = H.foldr f z m

foldr' :: (a -> b -> b) -> b -> KeyMap a -> b
foldr' f z (KeyMap m) = H.foldr' f z m

foldl :: (b -> a -> b) -> b -> KeyMap a -> b
foldl f z (KeyMap m) = H.foldl f z m

foldl' :: (b -> a -> b) -> b -> KeyMap a -> b
foldl' f z (KeyMap m) = H.foldl' f z m

foldrWithKey :: (Key -> v -> a -> a) -> a -> KeyMap v -> a
foldrWithKey f a = H.foldrWithKey f a . unKeyMap

traverse :: Applicative f => (v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverse f = fmap KeyMap . T.traverse f . unKeyMap

traverseWithKey :: Applicative f => (Key -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseWithKey f = fmap KeyMap . H.traverseWithKey f  . unKeyMap

fromListWith :: (v -> v -> v) ->  [(Key, v)] -> KeyMap v
fromListWith op = KeyMap . H.fromListWith op

fromList :: [(Key, v)] -> KeyMap v
fromList = KeyMap . H.fromList

toList :: KeyMap v -> [(Key, v)]
toList = H.toList . unKeyMap

elems :: KeyMap v -> [v]
elems = H.elems . unKeyMap

toAscList :: KeyMap v -> [(Key, v)]
toAscList = sortBy (comparing fst) . toList

difference :: KeyMap v -> KeyMap v' -> KeyMap v
difference tm1 tm2 = KeyMap (H.difference (unKeyMap tm1) (unKeyMap tm2))

union :: KeyMap v -> KeyMap v -> KeyMap v
union (KeyMap x) (KeyMap y) = KeyMap (H.union x y)

unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith f (KeyMap x) (KeyMap y) = KeyMap (H.unionWith f x y)

unionWithKey :: (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey f (KeyMap x) (KeyMap y) = KeyMap (H.unionWithKey f x y)

intersection :: KeyMap a -> KeyMap b -> KeyMap a
intersection (KeyMap x) (KeyMap y) = KeyMap (H.intersection x y)

intersectionWith :: (a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWith f (KeyMap x) (KeyMap y) = KeyMap (H.intersectionWith f x y)

intersectionWithKey :: (Key -> a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWithKey f (KeyMap x) (KeyMap y) = KeyMap (H.intersectionWithKey f x y)

keys :: KeyMap v -> [Key]
keys = H.keys . unKeyMap

toHashMap :: KeyMap v -> HashMap Key v
toHashMap = unKeyMap

fromHashMap :: HashMap Key v -> KeyMap v
fromHashMap = KeyMap

toMap :: KeyMap v -> Map Key v
toMap = M.fromList . toList

fromMap :: Map Key v -> KeyMap v
fromMap = fromList . M.toList

coercionToHashMap :: Maybe (Coercion (HashMap Key v) (KeyMap v))
coercionToHashMap = Just Coercion

coercionToMap :: Maybe (Coercion (Map Key v) (KeyMap v))
coercionToMap = Nothing

mapKeyVal :: (Key -> Key) -> (v1 -> v2)
          -> KeyMap v1 -> KeyMap v2
mapKeyVal fk kv = foldrWithKey (\k v -> insert (fk k) (kv v)) empty

filter :: (v -> Bool) -> KeyMap v -> KeyMap v
filter f (KeyMap m) = KeyMap (H.filter f m)

filterWithKey :: (Key -> v -> Bool) -> KeyMap v -> KeyMap v
filterWithKey f (KeyMap m) = KeyMap (H.filterWithKey f m)

mapMaybe :: (a -> Maybe b) -> KeyMap a -> KeyMap b
mapMaybe f (KeyMap m) = KeyMap (H.mapMaybe f m)

mapMaybeWithKey :: (Key -> v -> Maybe u) -> KeyMap v -> KeyMap u
mapMaybeWithKey f (KeyMap m) = KeyMap (H.mapMaybeWithKey f m)

#endif


(!?) :: KeyMap v -> Key -> Maybe v
(!?) m k = lookup k m

alignWith :: (These a b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
alignWith f (KeyMap x) (KeyMap y) = KeyMap (SA.alignWith f x y)

alignWithKey :: (Key -> These a b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
alignWithKey f (KeyMap x) (KeyMap y) = KeyMap (SAI.ialignWith f x y)

toHashMapText :: KeyMap v -> HashMap Text  v
toHashMapText = H.fromList . L.map (first Key.toText) . toList

fromHashMapText :: HashMap Text v -> KeyMap v
fromHashMapText = fromList . L.map (first Key.fromText) . H.toList

toMapText :: KeyMap v -> Map Text v
toMapText = M.fromList . L.map (first Key.toText) . toList

fromMapText :: Map Text v -> KeyMap v
fromMapText = fromList . L.map (first Key.fromText) . M.toList



instance Read v => Read (KeyMap v) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      xs <- readPrec
      return (fromList xs)

    readListPrec = readListPrecDefault

instance Show v => Show (KeyMap v) where
    showsPrec d m = showParen (d > 10) $
      showString "fromList " . shows (toAscList m)

instance F.Foldable KeyMap where
    foldMap f = foldMapWithKey (\ _k v -> f v)
    foldr = foldr
    foldr' = foldr'
    foldl = foldl
    foldl' = foldl'
    null = null
    length = size

instance T.Traversable KeyMap where
    traverse = traverse

instance Semigroup (KeyMap v) where
    (<>) = union

instance Monoid (KeyMap v) where
    mempty = empty
    mappend = (<>)

instance GHC.Exts.IsList (KeyMap v) where
    type Item (KeyMap v) = (Key, v)
    fromList = fromList
    toList   = toAscList


instance TH.Lift v => TH.Lift (KeyMap v) where
    lift m = [| fromList m' |] where m' = toList m

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif


instance Hashable v => Hashable (KeyMap v) where
#ifdef USE_ORDEREDMAP
    hashWithSalt salt (KeyMap m) = M.foldlWithKey'
        (\acc k v -> acc `hashWithSalt` k `hashWithSalt` v)
        (hashWithSalt salt (M.size m)) m
#else
    hashWithSalt salt (KeyMap hm) = hashWithSalt salt hm
#endif


instance NFData v => NFData (KeyMap v) where
    rnf (KeyMap hm) = rnf hm


instance WI.FunctorWithIndex Key KeyMap where
    imap = mapWithKey

instance WI.FoldableWithIndex Key KeyMap where
    ifoldr   = foldrWithKey

instance WI.TraversableWithIndex Key KeyMap where
    itraverse = traverseWithKey


instance SA.Zip KeyMap where
    zipWith = intersectionWith

instance SAI.ZipWithIndex Key KeyMap where
    izipWith = intersectionWithKey

instance SA.Semialign KeyMap where
    alignWith = alignWith

instance SAI.SemialignWithIndex Key KeyMap where
    ialignWith = alignWithKey

instance SA.Align KeyMap where
    nil = empty


instance W.Filterable KeyMap where
    filter = filter
    mapMaybe = mapMaybe

instance W.Witherable KeyMap where

instance W.FilterableWithIndex Key KeyMap where
    ifilter = filterWithKey
    imapMaybe = mapMaybeWithKey

instance W.WitherableWithIndex Key KeyMap where


instance QC.Arbitrary1 KeyMap where
    liftArbitrary a  = fmap fromList (QC.liftArbitrary (QC.liftArbitrary a))
    liftShrink shr m = fmap fromList (QC.liftShrink (QC.liftShrink shr) (toList m))

instance QC.Arbitrary v => QC.Arbitrary (KeyMap v) where
    arbitrary = QC.arbitrary1
    shrink    = QC.shrink1

instance QC.CoArbitrary v => QC.CoArbitrary (KeyMap v) where
    coarbitrary = QC.coarbitrary . toList

instance QC.Function v => QC.Function (KeyMap v) where
    function = QC.functionMap toList fromList

module Data.Aeson.QQ.Simple (aesonQQ) where

import           Data.Aeson
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (Lift (..))

aesonQQ :: QuasiQuoter
aesonQQ = QuasiQuoter
    { quoteExp  = aesonExp
    , quotePat  = const $ error "No quotePat defined for jsonQQ"
    , quoteType = const $ error "No quoteType defined for jsonQQ"
    , quoteDec  = const $ error "No quoteDec defined for jsonQQ"
    }

aesonExp :: String -> ExpQ
aesonExp txt =
  case eitherDecodeStrict $ TE.encodeUtf8 $ T.pack txt of
    Left err  -> error $ "Error in aesonExp: " ++ show err
    Right val -> lift (val :: Value)


module Data.Aeson.Types
    (
      Value(..)
    , Key
    , Encoding
    , unsafeToEncoding
    , fromEncoding
    , Series
    , Array
    , emptyArray
    , Pair
    , Object
    , emptyObject
    , DotNetTime(..)
    , typeMismatch
    , unexpected
    , Parser
    , Result(..)
    , FromJSON(..)
    , fromJSON
    , parse
    , parseEither
    , parseMaybe
    , parseFail
    , modifyFailure
    , prependFailure
    , parserThrowError
    , parserCatchError
    , IResult (..)
    , ifromJSON
    , iparse
    , iparseEither
    , ToJSON(..)
    , KeyValue(..)
    , KeyValueOmit(..)

    , ToJSONKey(..)
    , ToJSONKeyFunction(..)
    , toJSONKeyText
    , toJSONKeyKey
    , contramapToJSONKeyFunction
    , FromJSONKey(..)
    , FromJSONKeyFunction(..)
    , fromJSONKeyCoerce
    , coerceFromJSONKeyFunction
    , mapFromJSONKeyFunction

    , GToJSONKey()
    , genericToJSONKey
    , GFromJSONKey()
    , genericFromJSONKey

    , FromJSON1(..)
    , parseJSON1
    , omittedField1
    , FromJSON2(..)
    , parseJSON2
    , omittedField2
    , ToJSON1(..)
    , toJSON1
    , toEncoding1
    , omitField1
    , ToJSON2(..)
    , toJSON2
    , toEncoding2
    , omitField2

    , GFromJSON
    , FromArgs
    , GToJSON
    , GToEncoding
    , GToJSON'
    , ToArgs
    , Zero
    , One
    , genericToJSON
    , genericLiftToJSON
    , genericToEncoding
    , genericLiftToEncoding
    , genericParseJSON
    , genericLiftParseJSON

    , withObject
    , withText
    , withArray
    , withScientific
    , withBool
    , withEmbeddedJSON

    , pairs
    , foldable
    , (.:)
    , (.:?)
    , (.:!)
    , (.!=)
    , (.:?=)
    , (.:!=)
    , object
    , parseField
    , parseFieldMaybe
    , parseFieldMaybe'
    , parseFieldOmit
    , parseFieldOmit'
    , explicitParseField
    , explicitParseFieldMaybe
    , explicitParseFieldMaybe'
    , explicitParseFieldOmit
    , explicitParseFieldOmit'

    , listEncoding
    , listValue
    , listParser

    , Options

    , fieldLabelModifier
    , constructorTagModifier
    , allNullaryToStringTag
    , omitNothingFields
    , allowOmittedFields
    , sumEncoding
    , unwrapUnaryRecords
    , tagSingleConstructors
    , rejectUnknownFields

    , SumEncoding(..)
    , camelTo
    , camelTo2
    , defaultOptions
    , defaultTaggedObject

    , JSONKeyOptions
    , keyModifier
    , defaultJSONKeyOptions

    , AesonException (..)

    , (<?>)
    , JSONPath
    , JSONPathElement(..)
    , formatPath
    , formatRelativePath
    , formatError
    ) where

import Data.Aeson.Encoding (Encoding, unsafeToEncoding, fromEncoding, Series, pairs)
import Data.Aeson.Types.Class
import Data.Aeson.Types.Internal
import Data.Foldable (toList)

foldable :: (Foldable t, ToJSON a) => t a -> Encoding
foldable = toEncoding . toList



module Data.Aeson.Text
    (
      encodeToLazyText
    , encodeToTextBuilder
    ) where

import Data.Aeson.Internal.Prelude

import Data.Aeson.Types (Value(..), ToJSON(..))
import Data.Aeson.Encoding (encodingToLazyByteString)
import qualified Data.Aeson.KeyMap as KM
import Data.Scientific (FPFormat(..), base10Exponent)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as TB
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)
import Numeric (showHex)
import qualified Data.Aeson.Key as Key
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Vector as V

encodeToLazyText :: ToJSON a => a -> LT.Text
encodeToLazyText = LT.decodeUtf8 . encodingToLazyByteString . toEncoding

encodeToTextBuilder :: ToJSON a => a -> Builder
encodeToTextBuilder =
    go . toJSON
  where
    go Null       = "null"
    go (Bool b)   = if b then "true" else "false"
    go (Number s) = fromScientific s
    go (String s) = string s
    go (Array v)
        | V.null v = "[]"
        | otherwise = 
                      TB.singleton '[' <>
                      go (V.unsafeHead v) <>
                      V.foldr f (TB.singleton ']') (V.unsafeTail v)
      where f a z = TB.singleton ',' <> go a <> z
    go (Object m) = 
        case KM.toList m of
          (x:xs) -> TB.singleton '{' <> one x <> foldr f (TB.singleton '}') xs
          _      -> "{}"
      where f a z     = TB.singleton ',' <> one a <> z
            one (k,v) = string (Key.toText k) <> TB.singleton ':' <> go v

string :: T.Text -> Builder
string s = TB.singleton '"' <> quote s <> TB.singleton '"'
  where
    quote q = case T.uncons t of
                Nothing      -> TB.fromText h
                Just (!c,t') -> TB.fromText h <> escape c <> quote t'
        where (h,t) = T.break isEscape q
    isEscape c = c == '\"' ||
                 c == '\\' ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"

    escape c
        | c < '\x20' = TB.fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = TB.singleton c
        where h = showHex (fromEnum c) ""

fromScientific :: Scientific -> Builder
fromScientific s = formatScientificBuilder format prec s
  where
    (format, prec)
      | base10Exponent s < 0 = (Generic, Nothing)
      | otherwise            = (Fixed,   Just 0)


Module:      Data.Aeson.TH
Copyright:   (c) 2011-2016 Bryan O'Sullivan
             (c) 2011 MailRank, Inc.
License:     BSD3
Stability:   experimental
Portability: portable

Functions to mechanically derive 'ToJSON' and 'FromJSON' instances. Note that
you need to enable the @TemplateHaskell@ language extension in order to use this
module.

An example shows how instances are generated for arbitrary data types. First we
define a data type:

@
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq
@

Next we derive the necessary instances. Note that we make use of the
feature to change record field names. In this case we drop the first 4
characters of every field name. We also modify constructor names by
lower-casing them:

@
$('deriveJSON' 'defaultOptions'{'fieldLabelModifier' = 'drop' 4, 'constructorTagModifier' = map toLower} ''D)
@

Now we can use the newly created instances.

@
d :: D 'Int'
d = Record { testOne = 3.14159
           , testTwo = 'True'
           , testThree = Product \"test\" \'A\' 123
           }
@

@
fromJSON (toJSON d) == Success d
@

This also works for data family instances, but instead of passing in the data
family name (with double quotes), we pass in a data family instance
constructor (with a single quote):

@
data family DF a
data instance DF Int = DF1 Int
                     | DF2 Int Int
                     deriving Eq

$('deriveJSON' 'defaultOptions' 'DF1)
@

Please note that you can derive instances for tuples using the following syntax:

@
$('deriveJSON' 'defaultOptions' ''(,,,))
@

If you derive `ToJSON` for a type that has no constructors, the splice will
require enabling @EmptyCase@ to compile.

module Data.Aeson.TH
    (
      Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject

    , deriveJSON
    , deriveJSON1
    , deriveJSON2

    , deriveToJSON
    , deriveToJSON1
    , deriveToJSON2
    , deriveFromJSON
    , deriveFromJSON1
    , deriveFromJSON2

    , mkToJSON
    , mkLiftToJSON
    , mkLiftToJSON2
    , mkToEncoding
    , mkLiftToEncoding
    , mkLiftToEncoding2
    , mkParseJSON
    , mkLiftParseJSON
    , mkLiftParseJSON2
    ) where


import Data.Aeson.Internal.Prelude

import Data.Char (ord)
import Data.Aeson (Object, (.:), FromJSON(..), FromJSON1(..), FromJSON2(..), ToJSON(..), ToJSON1(..), ToJSON2(..))
import Data.Aeson.Types (Options(..), Parser, SumEncoding(..), Value(..), defaultOptions, defaultTaggedObject)
import Data.Aeson.Types.Internal ((<?>), JSONPathElement(Key))
import Data.Aeson.Types.ToJSON (fromPairs, pair)
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (foldr')
import Data.List (genericLength, intercalate, union)
import Data.List.NonEmpty ((<|), NonEmpty((:|)))
import Data.Map (Map)
import qualified Data.Monoid as Monoid
import Data.Set (Set)
import Language.Haskell.TH hiding (Arity)
import Language.Haskell.TH.Datatype
import Text.Printf (printf)
import qualified Data.Aeson.Encoding.Internal as E
import qualified Data.List.NonEmpty as NE (length, reverse)
import qualified Data.Map as M (fromList, keys, lookup , singleton, size)
import qualified Data.Set as Set (empty, insert, member)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Vector as V (unsafeIndex, null, length, create, empty)
import qualified Data.Vector.Mutable as VM (unsafeNew, unsafeWrite)
import qualified Data.Text.Short as ST
import Data.ByteString.Short (ShortByteString)
import Data.Aeson.Internal.ByteString
import Data.Aeson.Internal.TH


deriveJSON :: Options
           -> Name
           -> Q [Dec]
deriveJSON = deriveJSONBoth deriveToJSON deriveFromJSON

deriveJSON1 :: Options
            -> Name
            -> Q [Dec]
deriveJSON1 = deriveJSONBoth deriveToJSON1 deriveFromJSON1

deriveJSON2 :: Options
            -> Name
            -> Q [Dec]
deriveJSON2 = deriveJSONBoth deriveToJSON2 deriveFromJSON2


TODO: Don't constrain phantom type variables.

data Foo a = Foo Int
instance (ToJSON a) ⇒ ToJSON Foo where ...

The above (ToJSON a) constraint is not necessary and perhaps undesirable.

deriveToJSON :: Options
             -> Name
             -> Q [Dec]
deriveToJSON = deriveToJSONCommon toJSONClass

deriveToJSON1 :: Options
              -> Name
              -> Q [Dec]
deriveToJSON1 = deriveToJSONCommon toJSON1Class

deriveToJSON2 :: Options
              -> Name
              -> Q [Dec]
deriveToJSON2 = deriveToJSONCommon toJSON2Class

deriveToJSONCommon :: JSONClass
                   -> Options
                   -> Name
                   -> Q [Dec]
deriveToJSONCommon = deriveJSONClass [ (ToJSON,     \jc _ -> consToValue Value jc)
                                     , (ToEncoding, \jc _ -> consToValue Encoding jc)
                                     ]

mkToJSON :: Options -- ^ Encoding options.
         -> Name -- ^ Name of the type to encode.
         -> Q Exp
mkToJSON = mkToJSONCommon toJSONClass

mkLiftToJSON :: Options -- ^ Encoding options.
             -> Name -- ^ Name of the type to encode.
             -> Q Exp
mkLiftToJSON = mkToJSONCommon toJSON1Class

mkLiftToJSON2 :: Options -- ^ Encoding options.
              -> Name -- ^ Name of the type to encode.
              -> Q Exp
mkLiftToJSON2 = mkToJSONCommon toJSON2Class

mkToJSONCommon :: JSONClass -- ^ Which class's method is being derived.
               -> Options -- ^ Encoding options.
               -> Name -- ^ Name of the encoded type.
               -> Q Exp
mkToJSONCommon = mkFunCommon (\jc _ -> consToValue Value jc)

mkToEncoding :: Options -- ^ Encoding options.
             -> Name -- ^ Name of the type to encode.
             -> Q Exp
mkToEncoding = mkToEncodingCommon toJSONClass

mkLiftToEncoding :: Options -- ^ Encoding options.
                 -> Name -- ^ Name of the type to encode.
                 -> Q Exp
mkLiftToEncoding = mkToEncodingCommon toJSON1Class

mkLiftToEncoding2 :: Options -- ^ Encoding options.
                  -> Name -- ^ Name of the type to encode.
                  -> Q Exp
mkLiftToEncoding2 = mkToEncodingCommon toJSON2Class

mkToEncodingCommon :: JSONClass -- ^ Which class's method is being derived.
                   -> Options -- ^ Encoding options.
                   -> Name -- ^ Name of the encoded type.
                   -> Q Exp
mkToEncodingCommon = mkFunCommon (\jc _ -> consToValue Encoding jc)

type LetInsert = ShortByteString -> ExpQ

consToValue :: ToJSONFun
            -> JSONClass
            -> Options
            -> [Type]
            -> [ConstructorInfo]
            -> Q Exp

consToValue _ _ _ _ [] =
    [| \x -> case x of {} |]

consToValue target jc opts instTys cons = autoletE liftSBS $ \letInsert -> do
    value <- newName "value"
    os    <- newNameList "_o"   $ arityInt jc
    tjs   <- newNameList "_tj"  $ arityInt jc
    tjls  <- newNameList "_tjl" $ arityInt jc
    let zippedTJs      = zip3 os tjs tjls
        interleavedTJs = flatten3 zippedTJs
        lastTyVars     = map varTToName $ drop (length instTys - arityInt jc) instTys
        tvMap          = M.fromList $ zip lastTyVars zippedTJs
    lamE (map varP $ interleavedTJs ++ [value]) $
        caseE (varE value) (matches letInsert tvMap)
  where
    matches letInsert tvMap = case cons of
      [con] | not (tagSingleConstructors opts) -> [argsToValue letInsert target jc tvMap opts False con]
      _ | allNullaryToStringTag opts && all isNullary cons ->
              [ match (conP conName []) (normalB $ conStr target opts conName) []
              | con <- cons
              , let conName = constructorName con
              ]
        | otherwise -> [argsToValue letInsert target jc tvMap opts True con | con <- cons]

conStr :: ToJSONFun -> Options -> Name -> Q Exp
conStr Value opts = appE [|String|] . conTxt opts
conStr Encoding opts = appE [|E.text|] . conTxt opts

conTxt :: Options -> Name -> Q Exp
conTxt opts = appE [|T.pack|] . stringE . conString opts

conString :: Options -> Name -> String
conString opts = constructorTagModifier opts . nameBase

isNullary :: ConstructorInfo -> Bool
isNullary ConstructorInfo { constructorVariant = NormalConstructor
                          , constructorFields  = tys } = null tys
isNullary _ = False

opaqueSumToValue :: LetInsert -> ToJSONFun -> Options -> Bool -> Bool -> Name -> ExpQ -> ExpQ
opaqueSumToValue letInsert target opts multiCons nullary conName value =
  sumToValue letInsert target opts multiCons nullary conName
    value
    pairs
  where
    pairs contentsFieldName = pairE letInsert target contentsFieldName value

recordSumToValue :: LetInsert -> ToJSONFun -> Options -> Bool -> Bool -> Name -> ExpQ -> ExpQ
recordSumToValue letInsert target opts multiCons nullary conName pairs =
  sumToValue letInsert target opts multiCons nullary conName
    (fromPairsE target pairs)
    (const pairs)

sumToValue
  :: LetInsert
  -> ToJSONFun
  -> Options
  -> Bool
  -> Bool
  -> Name
  -> ExpQ
  -> (String -> ExpQ)
  -> ExpQ
sumToValue letInsert target opts multiCons nullary conName value pairs
    | multiCons =
        case sumEncoding opts of
          TwoElemArray ->
            array target [conStr target opts conName, value]
          TaggedObject{tagFieldName, contentsFieldName} ->
            let tag = pairE letInsert target tagFieldName (conStr target opts conName)
                content = pairs contentsFieldName
            in fromPairsE target $
              if nullary then tag else infixApp tag [|(Monoid.<>)|] content
          ObjectWithSingleField ->
            objectE letInsert target [(conString opts conName, value)]
          UntaggedValue | nullary -> conStr target opts conName
          UntaggedValue -> value
    | otherwise = value

argsToValue :: LetInsert -> ToJSONFun -> JSONClass -> TyVarMap -> Options -> Bool -> ConstructorInfo -> Q Match

argsToValue letInsert target jc tvMap opts multiCons
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = argTys } = do
    argTys' <- mapM resolveTypeSynonyms argTys
    let len = length argTys'
    args <- newNameList "arg" len
    let js = case [ dispatchToJSON target jc conName tvMap argTy
                      `appE` varE arg
                  | (arg, argTy) <- zip args argTys'
                  ] of
               [e] -> e
               es -> array target es

    match (conP conName $ map varP args)
          (normalB $ opaqueSumToValue letInsert target opts multiCons (null argTys') conName js)
          []

argsToValue letInsert target jc tvMap opts multiCons
  info@ConstructorInfo { constructorName    = conName
                       , constructorVariant = RecordConstructor fields
                       , constructorFields  = argTys } =
    case (unwrapUnaryRecords opts, not multiCons, argTys) of
      (True,True,[_]) -> argsToValue letInsert target jc tvMap opts multiCons
                                     (info{constructorVariant = NormalConstructor})
      _ -> do

        argTys' <- mapM resolveTypeSynonyms argTys
        args <- newNameList "arg" $ length argTys'
        let argCons = zip3 (map varE args) argTys' fields

            toPair (arg, argTy, fld) =
              let fieldName = fieldLabel opts fld
                  toValue = dispatchToJSON target jc conName tvMap argTy

                  omitFn :: Q Exp
                  omitFn
                    | omitNothingFields opts = dispatchOmitField jc conName tvMap argTy
                    | otherwise = [| const False |]

              in condE
                (omitFn `appE` arg)
                [| mempty |]
                (pairE letInsert target fieldName (toValue `appE` arg))

            pairs = mconcatE (map toPair argCons)

        match (conP conName $ map varP args)
              (normalB $ recordSumToValue letInsert target opts multiCons (null argTys) conName pairs)
              []

argsToValue letInsert target jc tvMap opts multiCons
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = InfixConstructor
                  , constructorFields  = argTys } = do
    [alTy, arTy] <- mapM resolveTypeSynonyms argTys
    al <- newName "argL"
    ar <- newName "argR"
    match (infixP (varP al) conName (varP ar))
          ( normalB
          $ opaqueSumToValue letInsert target opts multiCons False conName
          $ array target
              [ dispatchToJSON target jc conName tvMap aTy
                  `appE` varE a
              | (a, aTy) <- [(al,alTy), (ar,arTy)]
              ]
          )
          []

(<^>) :: ExpQ -> ExpQ -> ExpQ
(<^>) a b = infixApp a [|(E.><)|] b
infixr 6 <^>

(<%>) :: ExpQ -> ExpQ -> ExpQ
(<%>) a b = a <^> [|E.comma|] <^> b
infixr 4 <%>

array :: ToJSONFun -> [ExpQ] -> ExpQ
array Encoding [] = [|E.emptyArray_|]
array Value [] = [|Array V.empty|]
array Encoding es = [|E.wrapArray|] `appE` foldr1 (<%>) es
array Value es = do
  mv <- newName "mv"
  let newMV = bindS (varP mv)
                    ([|VM.unsafeNew|] `appE`
                      litE (integerL $ fromIntegral (length es)))
      stmts = [ noBindS $
                  [|VM.unsafeWrite|] `appE`
                    varE mv `appE`
                      litE (integerL ix) `appE`
                        e
              | (ix, e) <- zip [(0::Integer)..] es
              ]
      ret = noBindS $ [|return|] `appE` varE mv
  [|Array|] `appE`
             (varE 'V.create `appE`
               doE (newMV:stmts++[ret]))

objectE :: LetInsert -> ToJSONFun -> [(String, ExpQ)] -> ExpQ
objectE letInsert target = fromPairsE target . mconcatE . fmap (uncurry (pairE letInsert target))

mconcatE :: [ExpQ] -> ExpQ
mconcatE [] = [|Monoid.mempty|]
mconcatE [x] = x
mconcatE (x : xs) = infixApp x [|(Monoid.<>)|] (mconcatE xs)

fromPairsE :: ToJSONFun -> ExpQ -> ExpQ
fromPairsE _ = ([|fromPairs|] `appE`)

pairE :: LetInsert -> ToJSONFun -> String -> ExpQ -> ExpQ
pairE letInsert Encoding k v = [| E.unsafePairSBS |] `appE` letInsert k' `appE` v
  where
    k' = ST.toShortByteString $ ST.pack $ "\"" ++ concatMap escapeAscii k ++ "\":"

    escapeAscii '\\' = "\\\\"
    escapeAscii '\"' = "\\\""
    escapeAscii '\n' = "\\n"
    escapeAscii '\r' = "\\r"
    escapeAscii '\t' = "\\t"
    escapeAscii c
      | ord c < 0x20 = "\\u" ++ printf "%04x" (ord c)
    escapeAscii c    = [c]

pairE _letInsert Value    k v = [| pair (Key.fromString k) |] `appE` v


deriveFromJSON :: Options
               -> Name
               -> Q [Dec]
deriveFromJSON = deriveFromJSONCommon fromJSONClass

deriveFromJSON1 :: Options
                -> Name
                -> Q [Dec]
deriveFromJSON1 = deriveFromJSONCommon fromJSON1Class

deriveFromJSON2 :: Options
                -> Name
                -> Q [Dec]
deriveFromJSON2 = deriveFromJSONCommon fromJSON2Class

deriveFromJSONCommon :: JSONClass
                     -> Options
                     -> Name
                     -> Q [Dec]
deriveFromJSONCommon = deriveJSONClass [(ParseJSON, consFromJSON)]

mkParseJSON :: Options -- ^ Encoding options.
            -> Name -- ^ Name of the encoded type.
            -> Q Exp
mkParseJSON = mkParseJSONCommon fromJSONClass

mkLiftParseJSON :: Options -- ^ Encoding options.
                -> Name -- ^ Name of the encoded type.
                -> Q Exp
mkLiftParseJSON = mkParseJSONCommon fromJSON1Class

mkLiftParseJSON2 :: Options -- ^ Encoding options.
                 -> Name -- ^ Name of the encoded type.
                 -> Q Exp
mkLiftParseJSON2 = mkParseJSONCommon fromJSON2Class

mkParseJSONCommon :: JSONClass -- ^ Which class's method is being derived.
                  -> Options -- ^ Encoding options.
                  -> Name -- ^ Name of the encoded type.
                  -> Q Exp
mkParseJSONCommon = mkFunCommon consFromJSON

consFromJSON :: JSONClass
             -> Name
             -> Options
             -> [Type]
             -> [ConstructorInfo]
             -> Q Exp

consFromJSON _ _ _ _ [] =
    [| \_ -> fail "Attempted to parse empty type" |]

consFromJSON jc tName opts instTys cons = do
  value <- newName "value"
  os    <- newNameList "_o"   $ arityInt jc
  pjs   <- newNameList "_pj"  $ arityInt jc
  pjls  <- newNameList "_pjl" $ arityInt jc
  let zippedPJs      = zip3 os pjs pjls
      interleavedPJs = flatten3 zippedPJs
      lastTyVars     = map varTToName $ drop (length instTys - arityInt jc) instTys
      tvMap          = M.fromList $ zip lastTyVars zippedPJs
  lamE (map varP $ interleavedPJs ++ [value]) $ lamExpr value tvMap

  where
    checkExi tvMap con = checkExistentialContext jc tvMap
                                                 (constructorContext con)
                                                 (constructorName con)

    lamExpr value tvMap = case cons of
      [con]
        | not (tagSingleConstructors opts)
            -> checkExi tvMap con $ parseArgs jc tvMap tName opts con (Right value)
      _ | sumEncoding opts == UntaggedValue
            -> parseUntaggedValue tvMap cons value
        | otherwise
            -> caseE (varE value) $
                   if allNullaryToStringTag opts && all isNullary cons
                   then allNullaryMatches
                   else mixedMatches tvMap

    allNullaryMatches =
      [ do txt <- newName "txtX"
           match (conP 'String [varP txt])
                 (guardedB $
                  [ liftM2 (,) (normalG $
                                  infixApp (varE txt)
                                           [|(==)|]
                                           (conTxt opts conName)
                               )
                               ([|pure|] `appE` conE conName)
                  | con <- cons
                  , let conName = constructorName con
                  ]
                  ++
                  [ liftM2 (,)
                      (normalG [|otherwise|])
                      ( [|noMatchFail|]
                        `appE` litE (stringL $ show tName)
                        `appE` ([|T.unpack|] `appE` varE txt)
                      )
                  ]
                 )
                 []
      , do other <- newName "other"
           match (varP other)
                 (normalB $ [|noStringFail|]
                    `appE` litE (stringL $ show tName)
                    `appE` ([|valueConName|] `appE` varE other)
                 )
                 []
      ]

    mixedMatches tvMap =
        case sumEncoding opts of
          TaggedObject {tagFieldName, contentsFieldName} ->
            parseObject $ parseTaggedObject tvMap tagFieldName contentsFieldName
          UntaggedValue -> error "UntaggedValue: Should be handled already"
          ObjectWithSingleField ->
            parseObject $ parseObjectWithSingleField tvMap
          TwoElemArray ->
            [ do arr <- newName "array"
                 match (conP 'Array [varP arr])
                       (guardedB
                        [ liftM2 (,) (normalG $ infixApp ([|V.length|] `appE` varE arr)
                                                         [|(==)|]
                                                         (litE $ integerL 2))
                                     (parse2ElemArray tvMap arr)
                        , liftM2 (,) (normalG [|otherwise|])
                                     ([|not2ElemArray|]
                                       `appE` litE (stringL $ show tName)
                                       `appE` ([|V.length|] `appE` varE arr))
                        ]
                       )
                       []
            , do other <- newName "other"
                 match (varP other)
                       ( normalB
                         $ [|noArrayFail|]
                             `appE` litE (stringL $ show tName)
                             `appE` ([|valueConName|] `appE` varE other)
                       )
                       []
            ]

    parseObject f =
        [ do obj <- newName "obj"
             match (conP 'Object [varP obj]) (normalB $ f obj) []
        , do other <- newName "other"
             match (varP other)
                   ( normalB
                     $ [|noObjectFail|]
                         `appE` litE (stringL $ show tName)
                         `appE` ([|valueConName|] `appE` varE other)
                   )
                   []
        ]

    parseTaggedObject tvMap typFieldName valFieldName obj = do
      conKey <- newName "conKeyX"
      doE [ bindS (varP conKey)
                  (infixApp (varE obj)
                            [|(.:)|]
                            ([|Key.fromString|] `appE` stringE typFieldName))
          , noBindS $ parseContents tvMap conKey (Left (valFieldName, obj)) 'conNotFoundFailTaggedObject [|Key.fromString|] [|Key.toString|]
          ]

    parseUntaggedValue tvMap cons' conVal =
        foldr1 (\e e' -> infixApp e [|(<|>)|] e')
               (map (\x -> parseValue tvMap x conVal) cons')

    parseValue _tvMap
        ConstructorInfo { constructorName    = conName
                        , constructorVariant = NormalConstructor
                        , constructorFields  = [] }
        conVal = do
      str <- newName "str"
      caseE (varE conVal)
        [ match (conP 'String [varP str])
                (guardedB
                  [ liftM2 (,) (normalG $ infixApp (varE str) [|(==)|] (conTxt opts conName)
                               )
                               ([|pure|] `appE` conE conName)
                  ]
                )
                []
        , matchFailed tName conName "String"
        ]
    parseValue tvMap con conVal =
      checkExi tvMap con $ parseArgs jc tvMap tName opts con (Right conVal)


    parse2ElemArray tvMap arr = do
      conKey <- newName "conKeyY"
      conVal <- newName "conValY"
      let letIx n ix =
              valD (varP n)
                   (normalB ([|V.unsafeIndex|] `appE`
                               varE arr `appE`
                               litE (integerL ix)))
                   []
      letE [ letIx conKey 0
           , letIx conVal 1
           ]
           (caseE (varE conKey)
                  [ do txt <- newName "txtY"
                       match (conP 'String [varP txt])
                             (normalB $ parseContents tvMap
                                                      txt
                                                      (Right conVal)
                                                      'conNotFoundFail2ElemArray
                                                      [|T.pack|] [|T.unpack|]
                             )
                             []
                  , do other <- newName "other"
                       match (varP other)
                             ( normalB
                               $ [|firstElemNoStringFail|]
                                     `appE` litE (stringL $ show tName)
                                     `appE` ([|valueConName|] `appE` varE other)
                             )
                             []
                  ]
           )

    parseObjectWithSingleField tvMap obj = do
      conKey <- newName "conKeyZ"
      conVal <- newName "conValZ"
      caseE ([e|KM.toList|] `appE` varE obj)
            [ match (listP [tupP [varP conKey, varP conVal]])
                    (normalB $ parseContents tvMap conKey (Right conVal) 'conNotFoundFailObjectSingleField [|Key.fromString|] [|Key.toString|])
                    []
            , do other <- newName "other"
                 match (varP other)
                       (normalB $ [|wrongPairCountFail|]
                                  `appE` litE (stringL $ show tName)
                                  `appE` ([|show . length|] `appE` varE other)
                       )
                       []
            ]

    parseContents tvMap conKey contents errorFun pack unpack =
        caseE (varE conKey)
              [ match wildP
                      ( guardedB $
                        [ do g <- normalG $ infixApp (varE conKey)
                                                     [|(==)|]
                                                     (pack `appE`
                                                        conNameExp opts con)
                             e <- checkExi tvMap con $
                                  parseArgs jc tvMap tName opts con contents
                             return (g, e)
                        | con <- cons
                        ]
                        ++
                        [ liftM2 (,)
                                 (normalG [e|otherwise|])
                                 ( varE errorFun
                                   `appE` litE (stringL $ show tName)
                                   `appE` listE (map ( litE
                                                     . stringL
                                                     . constructorTagModifier opts
                                                     . nameBase
                                                     . constructorName
                                                     ) cons
                                                )
                                   `appE` (unpack `appE` varE conKey)
                                 )
                        ]
                      )
                      []
              ]

parseNullaryMatches :: Name -> Name -> [Q Match]
parseNullaryMatches tName conName =
    [ do arr <- newName "arr"
         match (conP 'Array [varP arr])
               (guardedB
                [ liftM2 (,) (normalG $ [|V.null|] `appE` varE arr)
                             ([|pure|] `appE` conE conName)
                , liftM2 (,) (normalG [|otherwise|])
                             (parseTypeMismatch tName conName
                                (litE $ stringL "an empty Array")
                                (infixApp (litE $ stringL "Array of length ")
                                          [|(++)|]
                                          ([|show . V.length|] `appE` varE arr)
                                )
                             )
                ]
               )
               []
    , matchFailed tName conName "Array"
    ]

parseUnaryMatches :: JSONClass -> TyVarMap -> Type -> Name -> [Q Match]
parseUnaryMatches jc tvMap argTy conName =
    [ do arg <- newName "arg"
         match (varP arg)
               ( normalB $ infixApp (conE conName)
                                    [|(<$>)|]
                                    (dispatchParseJSON jc conName tvMap argTy
                                      `appE` varE arg)
               )
               []
    ]

parseRecord :: JSONClass
            -> TyVarMap
            -> [Type]
            -> Options
            -> Name
            -> Name
            -> [Name]
            -> Name
            -> Bool
            -> ExpQ
parseRecord jc tvMap argTys opts tName conName fields obj inTaggedObject =
    (if rejectUnknownFields opts
     then infixApp checkUnknownRecords [|(>>)|]
     else id) $
    foldl' (\a b -> infixApp a [|(<*>)|] b)
           (infixApp (conE conName) [|(<$>)|] x)
           xs
    where
      lookupField :: Type -> Q Exp
      lookupField argTy
        | allowOmittedFields opts = [| lookupFieldOmit |] `appE` dispatchOmittedField jc conName tvMap argTy
        | otherwise               = [| lookupFieldNoOmit |]

      tagFieldNameAppender =
          if inTaggedObject then (tagFieldName (sumEncoding opts) :) else id
      knownFields = appE [|KM.fromList|] $ listE $
          map (\knownName -> tupE [appE [|Key.fromString|] $ litE $ stringL knownName, [|()|]]) $
              tagFieldNameAppender $ map (fieldLabel opts) fields
      checkUnknownRecords =
          caseE (appE [|KM.keys|] $ infixApp (varE obj) [|KM.difference|] knownFields)
              [ match (listP []) (normalB [|return ()|]) []
              , newName "unknownFields" >>=
                  \unknownFields -> match (varP unknownFields)
                      (normalB $ appE [|fail|] $ infixApp
                          (litE (stringL "Unknown fields: "))
                          [|(++)|]
                          (appE [|show|] (varE unknownFields)))
                      []
              ]
      x:xs = [ lookupField argTy
               `appE` dispatchParseJSON jc conName tvMap argTy
               `appE` litE (stringL $ show tName)
               `appE` litE (stringL $ constructorTagModifier opts $ nameBase conName)
               `appE` varE obj
               `appE` ( [|Key.fromString|] `appE` stringE (fieldLabel opts field)
                      )
             | (field, argTy) <- zip fields argTys
             ]

getValField :: Name -> String -> [MatchQ] -> Q Exp
getValField obj valFieldName matches = do
  val <- newName "val"
  doE [ bindS (varP val) $ infixApp (varE obj)
                                    [|(.:)|]
                                    ([|Key.fromString|] `appE`
                                       litE (stringL valFieldName))
      , noBindS $ caseE (varE val) matches
      ]

matchCases :: Either (String, Name) Name -> [MatchQ] -> Q Exp
matchCases (Left (valFieldName, obj)) = getValField obj valFieldName
matchCases (Right valName)            = caseE (varE valName)

parseArgs :: JSONClass -- ^ The FromJSON variant being derived.
          -> TyVarMap -- ^ Maps the last type variables to their decoding
          -> Name -- ^ Name of the type to which the constructor belongs.
          -> Options -- ^ Encoding options.
          -> ConstructorInfo -- ^ Constructor for which to generate JSON parsing code.
          -> Either (String, Name) Name -- ^ Left (valFieldName, objName) or
          -> Q Exp
parseArgs _ _ _ _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = [] }
  (Left _) =
    [|pure|] `appE` conE conName
parseArgs _ _ tName _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = [] }
  (Right valName) =
    caseE (varE valName) $ parseNullaryMatches tName conName

parseArgs jc tvMap _ _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = [argTy] }
  contents = do
    argTy' <- resolveTypeSynonyms argTy
    matchCases contents $ parseUnaryMatches jc tvMap argTy' conName

parseArgs jc tvMap tName _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = argTys }
  contents = do
    argTys' <- mapM resolveTypeSynonyms argTys
    let len = genericLength argTys'
    matchCases contents $ parseProduct jc tvMap argTys' tName conName len

parseArgs jc tvMap tName opts
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = RecordConstructor fields
                  , constructorFields  = argTys }
  (Left (_, obj)) = do
    argTys' <- mapM resolveTypeSynonyms argTys
    parseRecord jc tvMap argTys' opts tName conName fields obj True
parseArgs jc tvMap tName opts
  info@ConstructorInfo { constructorName    = conName
                       , constructorVariant = RecordConstructor fields
                       , constructorFields  = argTys }
  (Right valName) =
    case (unwrapUnaryRecords opts,argTys) of
      (True,[_])-> parseArgs jc tvMap tName opts
                             (info{constructorVariant = NormalConstructor})
                             (Right valName)
      _ -> do
        obj <- newName "recObj"
        argTys' <- mapM resolveTypeSynonyms argTys
        caseE (varE valName)
          [ match (conP 'Object [varP obj]) (normalB $
              parseRecord jc tvMap argTys' opts tName conName fields obj False) []
          , matchFailed tName conName "Object"
          ]

parseArgs jc tvMap tName _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = InfixConstructor
                  , constructorFields  = argTys }
  contents = do
    argTys' <- mapM resolveTypeSynonyms argTys
    matchCases contents $ parseProduct jc tvMap argTys' tName conName 2

parseProduct :: JSONClass -- ^ The FromJSON variant being derived.
             -> TyVarMap -- ^ Maps the last type variables to their decoding
             -> [Type] -- ^ The argument types of the constructor.
             -> Name -- ^ Name of the type to which the constructor belongs.
             -> Name -- ^ 'Con'structor name.
             -> Integer -- ^ 'Con'structor arity.
             -> [Q Match]
parseProduct jc tvMap argTys tName conName numArgs =
    [ do arr <- newName "arr"
         let x:xs = [ dispatchParseJSON jc conName tvMap argTy
                      `appE`
                      infixApp (varE arr)
                               [|V.unsafeIndex|]
                               (litE $ integerL ix)
                    | (argTy, ix) <- zip argTys [0 .. numArgs - 1]
                    ]
         match (conP 'Array [varP arr])
               (normalB $ condE ( infixApp ([|V.length|] `appE` varE arr)
                                           [|(==)|]
                                           (litE $ integerL numArgs)
                                )
                                ( foldl' (\a b -> infixApp a [|(<*>)|] b)
                                         (infixApp (conE conName) [|(<$>)|] x)
                                         xs
                                )
                                ( parseTypeMismatch tName conName
                                    (litE $ stringL $ "Array of length " ++ show numArgs)
                                    ( infixApp (litE $ stringL "Array of length ")
                                               [|(++)|]
                                               ([|show . V.length|] `appE` varE arr)
                                    )
                                )
               )
               []
    , matchFailed tName conName "Array"
    ]


matchFailed :: Name -> Name -> String -> MatchQ
matchFailed tName conName expected = do
  other <- newName "other"
  match (varP other)
        ( normalB $ parseTypeMismatch tName conName
                      (litE $ stringL expected)
                      ([|valueConName|] `appE` varE other)
        )
        []

parseTypeMismatch :: Name -> Name -> ExpQ -> ExpQ -> ExpQ
parseTypeMismatch tName conName expected actual =
    foldl appE
          [|parseTypeMismatch'|]
          [ litE $ stringL $ nameBase conName
          , litE $ stringL $ show tName
          , expected
          , actual
          ]

lookupFieldOmit :: Maybe a -> (Value -> Parser a) -> String -> String -> Object -> Key -> Parser a
lookupFieldOmit maybeDefault pj tName rec obj key =
    case KM.lookup key obj of
      Nothing ->
        case maybeDefault of
          Nothing -> unknownFieldFail tName rec (Key.toString key)
          Just x -> pure x
      Just v  -> pj v <?> Key key

lookupFieldNoOmit :: (Value -> Parser a) -> String -> String -> Object -> Key -> Parser a
lookupFieldNoOmit pj tName rec obj key =
    case KM.lookup key obj of
      Nothing -> unknownFieldFail tName rec (Key.toString key)
      Just v  -> pj v <?> Key key

unknownFieldFail :: String -> String -> String -> Parser fail
unknownFieldFail tName rec key =
    fail $ printf "When parsing the record %s of type %s the key %s was not present."
                  rec tName key

noArrayFail :: String -> String -> Parser fail
noArrayFail t o = fail $ printf "When parsing %s expected Array but got %s." t o

noObjectFail :: String -> String -> Parser fail
noObjectFail t o = fail $ printf "When parsing %s expected Object but got %s." t o

firstElemNoStringFail :: String -> String -> Parser fail
firstElemNoStringFail t o = fail $ printf "When parsing %s expected an Array of 2 elements where the first element is a String but got %s at the first element." t o

wrongPairCountFail :: String -> String -> Parser fail
wrongPairCountFail t n =
    fail $ printf "When parsing %s expected an Object with a single tag/contents pair but got %s pairs."
                  t n

noStringFail :: String -> String -> Parser fail
noStringFail t o = fail $ printf "When parsing %s expected String but got %s." t o

noMatchFail :: String -> String -> Parser fail
noMatchFail t o =
    fail $ printf "When parsing %s expected a String with the tag of a constructor but got %s." t o

not2ElemArray :: String -> Int -> Parser fail
not2ElemArray t i = fail $ printf "When parsing %s expected an Array of 2 elements but got %i elements" t i

conNotFoundFail2ElemArray :: String -> [String] -> String -> Parser fail
conNotFoundFail2ElemArray t cs o =
    fail $ printf "When parsing %s expected a 2-element Array with a tag and contents element where the tag is one of [%s], but got %s."
                  t (intercalate ", " cs) o

conNotFoundFailObjectSingleField :: String -> [String] -> String -> Parser fail
conNotFoundFailObjectSingleField t cs o =
    fail $ printf "When parsing %s expected an Object with a single tag/contents pair where the tag is one of [%s], but got %s."
                  t (intercalate ", " cs) o

conNotFoundFailTaggedObject :: String -> [String] -> String -> Parser fail
conNotFoundFailTaggedObject t cs o =
    fail $ printf "When parsing %s expected an Object with a tag field where the value is one of [%s], but got %s."
                  t (intercalate ", " cs) o

parseTypeMismatch' :: String -> String -> String -> String -> Parser fail
parseTypeMismatch' conName tName expected actual =
    fail $ printf "When parsing the constructor %s of type %s expected %s but got %s."
                  conName tName expected actual


deriveJSONBoth :: (Options -> Name -> Q [Dec])
               -> (Options -> Name -> Q [Dec])
               -> Options
               -> Name
               -> Q [Dec]
deriveJSONBoth dtj dfj opts name =
    liftM2 (++) (dtj opts name) (dfj opts name)

deriveJSONClass :: [(JSONFun, JSONClass -> Name -> Options -> [Type]
                                        -> [ConstructorInfo] -> Q Exp)]
                -> JSONClass
                -> Options
                -> Name
                -> Q [Dec]
deriveJSONClass consFuns jc opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTys
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      (instanceCxt, instanceType)
        <- buildTypeInstance parentName jc ctxt instTys variant
      (:[]) <$> instanceD (return instanceCxt)
                          (return instanceType)
                          (methodDecs parentName instTys cons)
  where
    methodDecs :: Name -> [Type] -> [ConstructorInfo] -> [Q Dec]
    methodDecs parentName instTys cons = flip map consFuns $ \(jf, jfMaker) ->
      funD (jsonFunValName jf (arity jc))
           [ clause []
                    (normalB $ jfMaker jc parentName opts instTys cons)
                    []
           ]

mkFunCommon :: (JSONClass -> Name -> Options -> [Type] -> [ConstructorInfo] -> Q Exp)
            -> JSONClass
            -> Options
            -> Name
            -> Q Exp
mkFunCommon consFun jc opts name = do
  info <- reifyDatatype name
  case info of
    DatatypeInfo { datatypeContext   = ctxt
                 , datatypeName      = parentName
                 , datatypeInstTypes = instTys
                 , datatypeVariant   = variant
                 , datatypeCons      = cons
                 } -> do
      !_ <- buildTypeInstance parentName jc ctxt instTys variant
      consFun jc parentName opts instTys cons

data FunArg = Omit | Single | Plural deriving (Eq)

dispatchFunByType :: JSONClass
                  -> JSONFun
                  -> Name
                  -> TyVarMap
                  -> FunArg -- Plural if we are using the function argument that works
                  -> Type
                  -> Q Exp
dispatchFunByType _ jf _ tvMap list (VarT tyName) =
    varE $ case M.lookup tyName tvMap of
                Just (tfjoExp, tfjExp, tfjlExp) -> case list of
                    Omit -> tfjoExp
                    Single -> tfjExp 
                    Plural -> tfjlExp
                Nothing                   -> jsonFunValOrListName list jf Arity0
dispatchFunByType jc jf conName tvMap list (SigT ty _) =
    dispatchFunByType jc jf conName tvMap list ty
dispatchFunByType jc jf conName tvMap list (ForallT _ _ ty) =
    dispatchFunByType jc jf conName tvMap list ty
dispatchFunByType jc jf conName tvMap list ty = do
    let tyCon :: Type
        tyArgs :: [Type]
        tyCon :| tyArgs = unapplyTy ty

        numLastArgs :: Int
        numLastArgs = min (arityInt jc) (length tyArgs)

        lhsArgs, rhsArgs :: [Type]
        (lhsArgs, rhsArgs) = splitAt (length tyArgs - numLastArgs) tyArgs

        tyVarNames :: [Name]
        tyVarNames = M.keys tvMap

        args :: [Q Exp]
        args
            | list == Omit = map     (dispatchFunByType jc jf conName tvMap  Omit)                        rhsArgs
            | otherwise    = zipWith (dispatchFunByType jc jf conName tvMap) (cycle [Omit,Single,Plural]) (triple rhsArgs)

    itf <- isInTypeFamilyApp tyVarNames tyCon tyArgs
    if any (`mentionsName` tyVarNames) lhsArgs || itf
       then outOfPlaceTyVarError jc conName
       else if any (`mentionsName` tyVarNames) rhsArgs
            then appsE $ varE (jsonFunValOrListName list jf $ toEnum numLastArgs) : args
            else varE $ jsonFunValOrListName list jf Arity0

dispatchToJSON :: ToJSONFun -> JSONClass -> Name -> TyVarMap -> Type -> Q Exp
dispatchToJSON target jc n tvMap = dispatchFunByType jc (targetToJSONFun target) n tvMap Single

dispatchOmitField :: JSONClass -> Name -> TyVarMap -> Type -> Q Exp
dispatchOmitField jc n tvMap = dispatchFunByType jc ToJSON n tvMap Omit

dispatchParseJSON :: JSONClass -> Name -> TyVarMap -> Type -> Q Exp
dispatchParseJSON  jc n tvMap = dispatchFunByType jc ParseJSON  n tvMap Single

dispatchOmittedField :: JSONClass -> Name -> TyVarMap -> Type -> Q Exp
dispatchOmittedField jc n tvMap = dispatchFunByType jc ParseJSON n tvMap Omit


buildTypeInstance :: Name
                  -> JSONClass
                  -> Cxt
                  -> [Type]
                  -> DatatypeVariant
                  -> Q (Cxt, Type)
buildTypeInstance tyConName jc dataCxt varTysOrig variant = do
    varTysExp <- mapM resolveTypeSynonyms varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - arityInt jc

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    when (remainingLength < 0 || elem NotKindStar droppedStarKindStati) $
      derivingKindError jc tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        droppedTyVarNames :: [Name]
        droppedTyVarNames = freeVariables droppedTysExpSubst

    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError jc tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        (preds, kvNames) = unzip $ map (deriveConstraint jc) remainingTysExpSubst
        kvNames' = concat kvNames

        remainingTysExpSubst' :: [Type]
        remainingTysExpSubst' =
          map (substNamesWithKindStar kvNames') remainingTysExpSubst

        remainingTysOrigSubst :: [Type]
        remainingTysOrigSubst =
          map (substNamesWithKindStar (droppedKindVarNames `union` kvNames'))
            $ take remainingLength varTysOrig

        isDataFamily :: Bool
        isDataFamily = case variant of
                         Datatype        -> False
                         Newtype         -> False
                         DataInstance    -> True
                         NewtypeInstance -> True
                         Language.Haskell.TH.Datatype.TypeData -> False

        remainingTysOrigSubst' :: [Type]
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceCxt :: Cxt
        instanceCxt = catMaybes preds

        instanceType :: Type
        instanceType = AppT (ConT $ jsonClassName jc)
                     $ applyTyCon tyConName remainingTysOrigSubst'

    when (any (`predMentionsName` droppedTyVarNames) dataCxt) $
      datatypeContextError tyConName instanceType
    unless (canEtaReduce remainingTysExpSubst' droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceCxt, instanceType)

deriveConstraint :: JSONClass -> Type -> (Maybe Pred, [Name])
deriveConstraint jc t
  | not (isTyVar t) = (Nothing, [])
  | hasKindStar t   = (Just (applyCon (jcConstraint Arity0) tName), [])
  | otherwise = case hasKindVarChain 1 t of
      Just ns | jcArity >= Arity1
              -> (Just (applyCon (jcConstraint Arity1) tName), ns)
      _ -> case hasKindVarChain 2 t of
           Just ns | jcArity == Arity2
                   -> (Just (applyCon (jcConstraint Arity2) tName), ns)
           _ -> (Nothing, [])
  where
    tName :: Name
    tName = varTToName t

    jcArity :: Arity
    jcArity = arity jc

    jcConstraint :: Arity -> Name
    jcConstraint = jsonClassName . JSONClass (direction jc)

Note [Kind signatures in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

It is possible to put explicit kind signatures into the derived instances, e.g.,

  instance C a => C (Data (f :: * -> *)) where ...

But it is preferable to avoid this if possible. If we come up with an incorrect
kind signature (which is entirely possible, since Template Haskell doesn't always
have the best track record with reifying kind signatures), then GHC will flat-out
reject the instance, which is quite unfortunate.

Plain old datatypes have the advantage that you can avoid using any kind signatures
at all in their instances. This is because a datatype declaration uses all type
variables, so the types that we use in a derived instance uniquely determine their
kinds. As long as we plug in the right types, the kind inferencer can do the rest
of the work. For this reason, we use unSigT to remove all kind signatures before
splicing in the instance context and head.

Data family instances are trickier, since a data family can have two instances that
are distinguished by kind alone, e.g.,

  data family Fam (a :: k)
  data instance Fam (a :: * -> *)
  data instance Fam (a :: *)

If we dropped the kind signatures for C (Fam a), then GHC will have no way of
knowing which instance we are talking about. To avoid this scenario, we always
include explicit kind signatures in data family instances. There is a chance that
the inferred kind signatures will be incorrect, but if so, we can always fall back
on the mk- functions.

Note [Type inference in derived instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Type inference is can be tricky to get right, and we want to avoid recreating the
entirety of GHC's type inferencer in Template Haskell. For this reason, we will
probably never come up with derived instance contexts that are as accurate as
GHC's. But that doesn't mean we can't do anything! There are a couple of simple
things we can do to make instance contexts that work for 80% of use cases:

1. If one of the last type parameters is polykinded, then its kind will be
   specialized to * in the derived instance. We note what kind variable the type
   parameter had and substitute it with * in the other types as well. For example,
   imagine you had

     data Data (a :: k) (b :: k)

   Then you'd want to derived instance to be:

     instance C (Data (a :: *))

   Not:

     instance C (Data (a :: k))

2. We naïvely come up with instance constraints using the following criteria:

   (i)   If there's a type parameter n of kind *, generate a ToJSON n/FromJSON n
         constraint.
   (ii)  If there's a type parameter n of kind k1 -> k2 (where k1/k2 are * or kind
         variables), then generate a ToJSON1 n/FromJSON1 n constraint, and if
         k1/k2 are kind variables, then substitute k1/k2 with * elsewhere in the
         types. We must consider the case where they are kind variables because
         you might have a scenario like this:

           newtype Compose (f :: k2 -> *) (g :: k1 -> k2) (a :: k1)
             = Compose (f (g a))

         Which would have a derived ToJSON1 instance of:

           instance (ToJSON1 f, ToJSON1 g) => ToJSON1 (Compose f g) where ...
   (iii) If there's a type parameter n of kind k1 -> k2 -> k3 (where k1/k2/k3 are
         * or kind variables), then generate a ToJSON2 n/FromJSON2 n constraint
         and perform kind substitution as in the other cases.

checkExistentialContext :: JSONClass -> TyVarMap -> Cxt -> Name
                        -> Q a -> Q a
checkExistentialContext jc tvMap ctxt conName q =
  if (any (`predMentionsName` M.keys tvMap) ctxt
       || M.size tvMap < arityInt jc)
       && not (allowExQuant jc)
     then existentialContextError conName
     else q

Note [Matching functions with GADT type variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When deriving ToJSON2, there is a tricky corner case to consider:

  data Both a b where
    BothCon :: x -> x -> Both x x

Which encoding functions should be applied to which arguments of BothCon?
We have a choice, since both the function of type (a -> Value) and of type
(b -> Value) can be applied to either argument. In such a scenario, the
second encoding function takes precedence over the first encoding function, so the
derived ToJSON2 instance would be something like:

  instance ToJSON2 Both where
    liftToJSON2 tj1 tj2 p (BothCon x1 x2) = Array $ create $ do
      mv <- unsafeNew 2
      unsafeWrite mv 0 (tj1 x1)
      unsafeWrite mv 1 (tj2 x2)
      return mv

This is not an arbitrary choice, as this definition ensures that
liftToJSON2 toJSON = liftToJSON for a derived ToJSON1 instance for
Both.

type TyVarMap = Map Name (Name, Name, Name)

hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
hasKindStar (SigT _ StarT) = True
hasKindStar _              = False

isStarOrVar :: Kind -> Bool
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
isStarOrVar _      = False

newNameList :: String -> Int -> Q [Name]
newNameList prefix len = mapM newName [prefix ++ show n | n <- [1..len]]

hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (NE.length uk - 1 == kindArrows) && all isStarOrVar uk
        then Just (concatMap freeVariables uk)
        else Nothing

tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

varTToNameMaybe :: Type -> Maybe Name
varTToNameMaybe (VarT n)   = Just n
varTToNameMaybe (SigT t _) = varTToNameMaybe t
varTToNameMaybe _          = Nothing

varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!") . varTToNameMaybe

flatten3 :: [(a,a,a)] -> [a]
flatten3 = foldr (\(a,b,c) xs -> a:b:c:xs) []

triple :: [a] -> [a]
triple = foldr (\x xs -> x:x:x:xs) []

applyTyCon :: Name -> [Type] -> Type
applyTyCon = foldl' AppT . ConT

isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

isInTypeFamilyApp :: [Name] -> Type -> [Type] -> Q Bool
isInTypeFamilyApp names tyFun tyArgs =
  case tyFun of
    ConT tcName -> go tcName
    _           -> return False
  where
    go :: Name -> Q Bool
    go tcName = do
      info <- reify tcName
      case info of
        FamilyI (OpenTypeFamilyD (TypeFamilyHead _ bndrs _ _)) _
          -> withinFirstArgs bndrs
        FamilyI (ClosedTypeFamilyD (TypeFamilyHead _ bndrs _ _) _) _
          -> withinFirstArgs bndrs
        _ -> return False
      where
        withinFirstArgs :: [a] -> Q Bool
        withinFirstArgs bndrs =
          let firstArgs = take (length bndrs) tyArgs
              argFVs    = freeVariables firstArgs
          in return $ any (`elem` argFVs) names

unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t _k)  names = go t names
                              || go _k names
    go (VarT n)     names = n `elem` names
    go _            _     = False

predMentionsName :: Pred -> [Name] -> Bool
predMentionsName = mentionsName

unapplyTy :: Type -> NonEmpty Type
unapplyTy = NE.reverse . go
  where
    go :: Type -> NonEmpty Type
    go (AppT t1 t2)    = t2 <| go t1
    go (SigT t _)      = go t
    go (ForallT _ _ t) = go t
    go t               = t :| []

uncurryTy :: Type -> (Cxt, NonEmpty Type)
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (ctxt, tys) = uncurryTy t2
  in (ctxt, t1 <| tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT _ ctxt t) =
  let (ctxt', tys) = uncurryTy t
  in (ctxt ++ ctxt', tys)
uncurryTy t = ([], t :| [])

uncurryKind :: Kind -> NonEmpty Kind
uncurryKind = snd . uncurryTy

createKindChain :: Int -> Kind
createKindChain = go starK
  where
    go :: Kind -> Int -> Kind
    go k 0 = k
    go k !n = go (AppT (AppT ArrowT StarT) k) (n - 1)

conNameExp :: Options -> ConstructorInfo -> Q Exp
conNameExp opts = litE
                . stringL
                . constructorTagModifier opts
                . nameBase
                . constructorName

fieldLabel :: Options -- ^ Encoding options
           -> Name
           -> String
fieldLabel opts = fieldLabelModifier opts . nameBase

valueConName :: Value -> String
valueConName (Object _) = "Object"
valueConName (Array  _) = "Array"
valueConName (String _) = "String"
valueConName (Number _) = "Number"
valueConName (Bool   _) = "Boolean"
valueConName Null       = "Null"

applyCon :: Name -> Name -> Pred
applyCon con t = AppT (ConT con) (VarT t)

canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct droppedNames -- Make sure not to pass something of type [Type], since Type
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped


applySubstitutionKind :: Map Name Kind -> Type -> Type
applySubstitutionKind = applySubstitution

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = applySubstitutionKind (M.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (`substNameWithKind` starK) t ns


derivingKindError :: JSONClass -> Name -> Q a
derivingKindError jc tyConName = fail
  . showString "Cannot derive well-kinded instance of form ‘"
  . showString className
  . showChar ' '
  . showParen True
    ( showString (nameBase tyConName)
    . showString " ..."
    )
  . showString "‘\n\tClass "
  . showString className
  . showString " expects an argument of kind "
  . showString (pprint . createKindChain $ arityInt jc)
  $ ""
  where
    className :: String
    className = nameBase $ jsonClassName jc

etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

datatypeContextError :: Name -> Type -> Q a
datatypeContextError dataName instanceType = fail
    . showString "Can't make a derived instance of ‘"
    . showString (pprint instanceType)
    . showString "‘:\n\tData type ‘"
    . showString (nameBase dataName)
    . showString "‘ must not have a class context involving the last type argument(s)"
    $ ""

outOfPlaceTyVarError :: JSONClass -> Name -> a
outOfPlaceTyVarError jc conName = error
    . showString "Constructor ‘"
    . showString (nameBase conName)
    . showString "‘ must only use its last "
    . shows n
    . showString " type variable(s) within the last "
    . shows n
    . showString " argument(s) of a data type"
    $ ""
  where
    n :: Int
    n = arityInt jc

existentialContextError :: Name -> a
existentialContextError conName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must be truly polymorphic in the last argument(s) of the data type"
  $ ""


data Arity = Arity0 | Arity1 | Arity2
  deriving (Enum, Eq, Ord)

data Direction = To | From

data JSONFun = ToJSON | ToEncoding | ParseJSON

data ToJSONFun = Value | Encoding

targetToJSONFun :: ToJSONFun -> JSONFun
targetToJSONFun Value = ToJSON
targetToJSONFun Encoding = ToEncoding

data JSONClass = JSONClass { direction :: Direction, arity :: Arity }

toJSONClass, toJSON1Class, toJSON2Class,
    fromJSONClass, fromJSON1Class, fromJSON2Class :: JSONClass
toJSONClass    = JSONClass To   Arity0
toJSON1Class   = JSONClass To   Arity1
toJSON2Class   = JSONClass To   Arity2
fromJSONClass  = JSONClass From Arity0
fromJSON1Class = JSONClass From Arity1
fromJSON2Class = JSONClass From Arity2

jsonClassName :: JSONClass -> Name
jsonClassName (JSONClass To   Arity0) = ''ToJSON
jsonClassName (JSONClass To   Arity1) = ''ToJSON1
jsonClassName (JSONClass To   Arity2) = ''ToJSON2
jsonClassName (JSONClass From Arity0) = ''FromJSON
jsonClassName (JSONClass From Arity1) = ''FromJSON1
jsonClassName (JSONClass From Arity2) = ''FromJSON2

jsonFunOmitName :: JSONFun -> Arity -> Name
jsonFunOmitName ToJSON     Arity0 = 'omitField
jsonFunOmitName ToJSON     Arity1 = 'liftOmitField
jsonFunOmitName ToJSON     Arity2 = 'liftOmitField2
jsonFunOmitName ToEncoding Arity0 = 'omitField
jsonFunOmitName ToEncoding Arity1 = 'liftOmitField
jsonFunOmitName ToEncoding Arity2 = 'liftOmitField2
jsonFunOmitName ParseJSON  Arity0 = 'omittedField
jsonFunOmitName ParseJSON  Arity1 = 'liftOmittedField
jsonFunOmitName ParseJSON  Arity2 = 'liftOmittedField2

jsonFunValName :: JSONFun -> Arity -> Name
jsonFunValName ToJSON     Arity0 = 'toJSON
jsonFunValName ToJSON     Arity1 = 'liftToJSON
jsonFunValName ToJSON     Arity2 = 'liftToJSON2
jsonFunValName ToEncoding Arity0 = 'toEncoding
jsonFunValName ToEncoding Arity1 = 'liftToEncoding
jsonFunValName ToEncoding Arity2 = 'liftToEncoding2
jsonFunValName ParseJSON  Arity0 = 'parseJSON
jsonFunValName ParseJSON  Arity1 = 'liftParseJSON
jsonFunValName ParseJSON  Arity2 = 'liftParseJSON2

jsonFunListName :: JSONFun -> Arity -> Name
jsonFunListName ToJSON     Arity0 = 'toJSONList
jsonFunListName ToJSON     Arity1 = 'liftToJSONList
jsonFunListName ToJSON     Arity2 = 'liftToJSONList2
jsonFunListName ToEncoding Arity0 = 'toEncodingList
jsonFunListName ToEncoding Arity1 = 'liftToEncodingList
jsonFunListName ToEncoding Arity2 = 'liftToEncodingList2
jsonFunListName ParseJSON  Arity0 = 'parseJSONList
jsonFunListName ParseJSON  Arity1 = 'liftParseJSONList
jsonFunListName ParseJSON  Arity2 = 'liftParseJSONList2

jsonFunValOrListName :: FunArg -- e.g., toJSONList if True, toJSON if False
                     -> JSONFun -> Arity -> Name
jsonFunValOrListName Omit   = jsonFunOmitName
jsonFunValOrListName Single = jsonFunValName
jsonFunValOrListName Plural = jsonFunListName

arityInt :: JSONClass -> Int
arityInt = fromEnum . arity

allowExQuant :: JSONClass -> Bool
allowExQuant (JSONClass To _) = True
allowExQuant _                = False


data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t = case t of
    _ | hasKindStar t -> KindStar
    SigT _ (VarT k) -> IsKindVar k
    _ -> NotKindStar

starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName



module Data.Vector (
  Vector, MVector,


  length, null,

  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  indexM, headM, lastM,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  slice, init, tail, take, drop, splitAt, uncons, unsnoc,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,


  empty, singleton, replicate, generate, iterateN,

  replicateM, generateM, iterateNM, create, createT,

  unfoldr, unfoldrN, unfoldrExactN,
  unfoldrM, unfoldrNM, unfoldrExactNM,
  constructN, constructrN,

  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  cons, snoc, (++), concat,

  force,


  (//), update, update_,
  unsafeUpd, unsafeUpdate, unsafeUpdate_,

  accum, accumulate, accumulate_,
  unsafeAccum, unsafeAccumulate, unsafeAccumulate_,

  reverse, backpermute, unsafeBackpermute,

  modify,


  indexed,

  map, imap, concatMap,

  mapM, imapM, mapM_, imapM_, forM, forM_,
  iforM, iforM_,

  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,

  zipWithM, izipWithM, zipWithM_, izipWithM_,

  unzip, unzip3, unzip4, unzip5, unzip6,


  filter, ifilter, filterM, uniq,
  mapMaybe, imapMaybe,
  mapMaybeM, imapMaybeM,
  catMaybes,
  takeWhile, dropWhile,

  partition, unstablePartition, partitionWith, span, break, spanR, breakR, groupBy, group,

  elem, notElem, find, findIndex, findIndexR, findIndices, elemIndex, elemIndices,

  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',
  foldMap, foldMap',

  all, any, and, or,
  sum, product,
  maximum, maximumBy, maximumOn,
  minimum, minimumBy, minimumOn,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  foldM, ifoldM, foldM', ifoldM',
  fold1M, fold1M',foldM_, ifoldM_,
  foldM'_, ifoldM'_, fold1M_, fold1M'_,

  sequence, sequence_,

  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  iscanl, iscanl',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',
  iscanr, iscanr',

  eqBy, cmpBy,


  toList, Data.Vector.fromList, Data.Vector.fromListN,

  toArray, fromArray, toArraySlice, unsafeFromArraySlice,

  G.convert,

  freeze, thaw, copy, unsafeFreeze, unsafeThaw, unsafeCopy
) where

import Data.Vector.Mutable  ( MVector(..) )
import Data.Primitive.Array
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Generic as G

import Control.DeepSeq ( NFData(rnf)
#if MIN_VERSION_deepseq(1,4,3)
                       , NFData1(liftRnf)
#endif
                       )

import Control.Monad ( MonadPlus(..), liftM, ap )
#if !MIN_VERSION_base(4,13,0)
import Control.Monad (fail)
#endif
import Control.Monad.ST ( ST, runST )
import Control.Monad.Primitive
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix ( MonadFix (mfix) )
import Control.Monad.Zip
import Data.Function ( fix )

import Prelude
  ( Eq, Ord, Num, Enum, Monoid, Functor, Monad, Show, Bool, Ordering(..), Int, Maybe, Either
  , compare, mempty, mappend, mconcat, return, showsPrec, fmap, otherwise, id, flip, const
  , (>>=), (+), (-), (<), (<=), (>), (>=), (==), (/=), (&&), (.), ($) )

import Data.Functor.Classes (Eq1 (..), Ord1 (..), Read1 (..), Show1 (..))
import Data.Typeable  ( Typeable )
import Data.Data      ( Data(..) )
import Text.Read      ( Read(..), readListPrecDefault )
import Data.Semigroup ( Semigroup(..) )

import qualified Control.Applicative as Applicative
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

import qualified GHC.Exts as Exts (IsList(..))


data Vector a = Vector {-# UNPACK #-} !Int
        deriving ( Typeable )

liftRnfV :: (a -> ()) -> Vector a -> ()
liftRnfV elemRnf = foldl' (\_ -> elemRnf) ()

instance NFData a => NFData (Vector a) where
  rnf = liftRnfV rnf

#if MIN_VERSION_deepseq(1,4,3)
instance NFData1 Vector where
  liftRnf = liftRnfV
#endif

instance Show a => Show (Vector a) where
  showsPrec = G.showsPrec

instance Read a => Read (Vector a) where
  readPrec = G.readPrec
  readListPrec = readListPrecDefault

instance Show1 Vector where
    liftShowsPrec = G.liftShowsPrec

instance Read1 Vector where
    liftReadsPrec = G.liftReadsPrec

instance Exts.IsList (Vector a) where
  type Item (Vector a) = a
  fromList = Data.Vector.fromList
  fromListN = Data.Vector.fromListN
  toList = toList

instance Data a => Data (Vector a) where
  gfoldl       = G.gfoldl
  toConstr _   = G.mkVecConstr "Data.Vector.Vector"
  gunfold      = G.gunfold
  dataTypeOf _ = G.mkVecType "Data.Vector.Vector"
  dataCast1    = G.dataCast

type instance G.Mutable Vector = MVector

instance G.Vector Vector a where
  basicUnsafeFreeze (MVector i n marr)
    = Vector i n `liftM` unsafeFreezeArray marr

  basicUnsafeThaw (Vector i n arr)
    = MVector i n `liftM` unsafeThawArray arr

  basicLength (Vector _ n _) = n

  basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr

  basicUnsafeIndexM (Vector i _ arr) j = indexArrayM arr (i+j)

  basicUnsafeCopy (MVector i n dst) (Vector j _ src)
    = copyArray dst i src j n

instance Eq a => Eq (Vector a) where
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

instance Ord a => Ord (Vector a) where
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT

instance Eq1 Vector where
  liftEq eq xs ys = Bundle.eqBy eq (G.stream xs) (G.stream ys)

instance Ord1 Vector where
  liftCompare cmp xs ys = Bundle.cmpBy cmp (G.stream xs) (G.stream ys)

instance Semigroup (Vector a) where
  (<>) = (++)

  sconcat = G.concatNE

instance Monoid (Vector a) where
  mempty = empty

  mappend = (<>)

  mconcat = concat

instance Functor Vector where
  fmap = map

  (<$) = map . const

instance Monad Vector where
  return = Applicative.pure

  (>>=) = flip concatMap

#if !(MIN_VERSION_base(4,13,0))
  fail = Fail.fail -- == \ _str -> empty
#endif

instance Fail.MonadFail Vector where
  fail _ = empty

instance MonadPlus Vector where
  mzero = empty

  mplus = (++)

instance MonadZip Vector where
  mzip = zip

  mzipWith = zipWith

  munzip = unzip

instance MonadFix Vector where
  mfix f
    | null v0 = empty
    | otherwise = runST $ do
        h <- headM v0
        return $ cons h $
          generate (lv0 - 1) $
            \i -> fix (\a -> f a ! (i + 1))
    where
      v0 = fix (f . head)
      !lv0 = length v0

instance Applicative.Applicative Vector where
  pure = singleton

  (<*>) = ap

instance Applicative.Alternative Vector where
  empty = empty

  (<|>) = (++)

instance Foldable.Foldable Vector where
  foldr = foldr

  foldl = foldl

  foldr1 = foldr1

  foldl1 = foldl1

  foldr' = foldr'

  foldl' = foldl'

  toList = toList

  length = length

  null = null

  elem = elem

  maximum = maximum

  minimum = minimum

  sum = sum

  product = product

instance Traversable.Traversable Vector where
  traverse f xs =
      let !n = G.length xs
      in  Data.Vector.fromListN n Applicative.<$> Traversable.traverse f (toList xs)

  mapM = mapM

  sequence = sequence


length :: Vector a -> Int
length = G.length

null :: Vector a -> Bool
null = G.null


(!) :: Vector a -> Int -> a
(!) = (G.!)

(!?) :: Vector a -> Int -> Maybe a
(!?) = (G.!?)

head :: Vector a -> a
head = G.head

last :: Vector a -> a
last = G.last

unsafeIndex :: Vector a -> Int -> a
unsafeIndex = G.unsafeIndex

unsafeHead :: Vector a -> a
unsafeHead = G.unsafeHead

unsafeLast :: Vector a -> a
unsafeLast = G.unsafeLast


indexM :: Monad m => Vector a -> Int -> m a
indexM = G.indexM

headM :: Monad m => Vector a -> m a
headM = G.headM

lastM :: Monad m => Vector a -> m a
lastM = G.lastM

unsafeIndexM :: Monad m => Vector a -> Int -> m a
unsafeIndexM = G.unsafeIndexM

unsafeHeadM :: Monad m => Vector a -> m a
unsafeHeadM = G.unsafeHeadM

unsafeLastM :: Monad m => Vector a -> m a
unsafeLastM = G.unsafeLastM


slice :: Int   -- ^ @i@ starting index
                 -> Int   -- ^ @n@ length
                 -> Vector a
                 -> Vector a
slice = G.slice

init :: Vector a -> Vector a
init = G.init

tail :: Vector a -> Vector a
tail = G.tail

take :: Int -> Vector a -> Vector a
take = G.take

drop :: Int -> Vector a -> Vector a
drop = G.drop

splitAt :: Int -> Vector a -> (Vector a, Vector a)
splitAt = G.splitAt

uncons :: Vector a -> Maybe (a, Vector a)
uncons = G.uncons

unsnoc :: Vector a -> Maybe (Vector a, a)
unsnoc = G.unsnoc

unsafeSlice :: Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector a
                       -> Vector a
unsafeSlice = G.unsafeSlice

unsafeInit :: Vector a -> Vector a
unsafeInit = G.unsafeInit

unsafeTail :: Vector a -> Vector a
unsafeTail = G.unsafeTail

unsafeTake :: Int -> Vector a -> Vector a
unsafeTake = G.unsafeTake

unsafeDrop :: Int -> Vector a -> Vector a
unsafeDrop = G.unsafeDrop


empty :: Vector a
empty = G.empty

singleton :: a -> Vector a
singleton = G.singleton

replicate :: Int -> a -> Vector a
replicate = G.replicate

generate :: Int -> (Int -> a) -> Vector a
generate = G.generate

iterateN :: Int -> (a -> a) -> a -> Vector a
iterateN = G.iterateN


unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
unfoldr = G.unfoldr

unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
unfoldrN = G.unfoldrN

unfoldrExactN  :: Int -> (b -> (a, b)) -> b -> Vector a
unfoldrExactN = G.unfoldrExactN

unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrM = G.unfoldrM

unfoldrNM :: (Monad m) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector a)
unfoldrNM = G.unfoldrNM

unfoldrExactNM :: (Monad m) => Int -> (b -> m (a, b)) -> b -> m (Vector a)
unfoldrExactNM = G.unfoldrExactNM

constructN :: Int -> (Vector a -> a) -> Vector a
constructN = G.constructN

constructrN :: Int -> (Vector a -> a) -> Vector a
constructrN = G.constructrN


enumFromN :: Num a => a -> Int -> Vector a
enumFromN = G.enumFromN

enumFromStepN :: Num a => a -> a -> Int -> Vector a
enumFromStepN = G.enumFromStepN

enumFromTo :: Enum a => a -> a -> Vector a
enumFromTo = G.enumFromTo

enumFromThenTo :: Enum a => a -> a -> a -> Vector a
enumFromThenTo = G.enumFromThenTo


cons :: a -> Vector a -> Vector a
cons = G.cons

snoc :: Vector a -> a -> Vector a
snoc = G.snoc

infixr 5 ++
(++) :: Vector a -> Vector a -> Vector a
(++) = (G.++)

concat :: [Vector a] -> Vector a
concat = G.concat


replicateM :: Monad m => Int -> m a -> m (Vector a)
replicateM = G.replicateM

generateM :: Monad m => Int -> (Int -> m a) -> m (Vector a)
generateM = G.generateM

iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (Vector a)
iterateNM = G.iterateNM

create :: (forall s. ST s (MVector s a)) -> Vector a
create p = G.create p

createT :: Traversable.Traversable f => (forall s. ST s (f (MVector s a))) -> f (Vector a)
createT p = G.createT p




force :: Vector a -> Vector a
force = G.force


(//) :: Vector a   -- ^ initial vector (of length @m@)
                -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                -> Vector a
(//) = (G.//)

update :: Vector a        -- ^ initial vector (of length @m@)
       -> Vector (Int, a) -- ^ vector of index/value pairs (of length @n@)
       -> Vector a
update = G.update

update_ :: Vector a   -- ^ initial vector (of length @m@)
        -> Vector Int -- ^ index vector (of length @n1@)
        -> Vector a   -- ^ value vector (of length @n2@)
        -> Vector a
update_ = G.update_

unsafeUpd :: Vector a -> [(Int, a)] -> Vector a
unsafeUpd = G.unsafeUpd

unsafeUpdate :: Vector a -> Vector (Int, a) -> Vector a
unsafeUpdate = G.unsafeUpdate

unsafeUpdate_ :: Vector a -> Vector Int -> Vector a -> Vector a
unsafeUpdate_ = G.unsafeUpdate_


accum :: (a -> b -> a) -- ^ accumulating function @f@
      -> Vector a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector a
accum = G.accum

accumulate :: (a -> b -> a)  -- ^ accumulating function @f@
            -> Vector a       -- ^ initial vector (of length @m@)
            -> Vector (Int,b) -- ^ vector of index/value pairs (of length @n@)
            -> Vector a
accumulate = G.accumulate

accumulate_ :: (a -> b -> a) -- ^ accumulating function @f@
            -> Vector a      -- ^ initial vector (of length @m@)
            -> Vector Int    -- ^ index vector (of length @n1@)
            -> Vector b      -- ^ value vector (of length @n2@)
            -> Vector a
accumulate_ = G.accumulate_

unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
unsafeAccum = G.unsafeAccum

unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
unsafeAccumulate = G.unsafeAccumulate

unsafeAccumulate_
  :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
unsafeAccumulate_ = G.unsafeAccumulate_


reverse :: Vector a -> Vector a
reverse = G.reverse

backpermute :: Vector a -> Vector Int -> Vector a
backpermute = G.backpermute

unsafeBackpermute :: Vector a -> Vector Int -> Vector a
unsafeBackpermute = G.unsafeBackpermute


modify :: (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
modify p = G.modify p


indexed :: Vector a -> Vector (Int,a)
indexed = G.indexed


map :: (a -> b) -> Vector a -> Vector b
map = G.map

imap :: (Int -> a -> b) -> Vector a -> Vector b
imap = G.imap

concatMap :: (a -> Vector b) -> Vector a -> Vector b
concatMap = G.concatMap


mapM :: Monad m => (a -> m b) -> Vector a -> m (Vector b)
mapM = G.mapM

imapM :: Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
imapM = G.imapM

mapM_ :: Monad m => (a -> m b) -> Vector a -> m ()
mapM_ = G.mapM_

imapM_ :: Monad m => (Int -> a -> m b) -> Vector a -> m ()
imapM_ = G.imapM_

forM :: Monad m => Vector a -> (a -> m b) -> m (Vector b)
forM = G.forM

forM_ :: Monad m => Vector a -> (a -> m b) -> m ()
forM_ = G.forM_

iforM :: Monad m => Vector a -> (Int -> a -> m b) -> m (Vector b)
iforM = G.iforM

iforM_ :: Monad m => Vector a -> (Int -> a -> m b) -> m ()
iforM_ = G.iforM_


zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
zipWith = G.zipWith

zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
zipWith3 = G.zipWith3

zipWith4 :: (a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
zipWith4 = G.zipWith4

zipWith5 :: (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
zipWith5 = G.zipWith5

zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
zipWith6 = G.zipWith6

izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
izipWith = G.izipWith

izipWith3 :: (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
izipWith3 = G.izipWith3

izipWith4 :: (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
izipWith4 = G.izipWith4

izipWith5 :: (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
izipWith5 = G.izipWith5

izipWith6 :: (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
izipWith6 = G.izipWith6

zip :: Vector a -> Vector b -> Vector (a, b)
zip = G.zip

zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
zip3 = G.zip3

zip4 :: Vector a -> Vector b -> Vector c -> Vector d
     -> Vector (a, b, c, d)
zip4 = G.zip4

zip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e
     -> Vector (a, b, c, d, e)
zip5 = G.zip5

zip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
     -> Vector (a, b, c, d, e, f)
zip6 = G.zip6


unzip :: Vector (a, b) -> (Vector a, Vector b)
unzip = G.unzip

unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
unzip3 = G.unzip3

unzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
unzip4 = G.unzip4

unzip5 :: Vector (a, b, c, d, e)
       -> (Vector a, Vector b, Vector c, Vector d, Vector e)
unzip5 = G.unzip5

unzip6 :: Vector (a, b, c, d, e, f)
       -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
unzip6 = G.unzip6


zipWithM :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
zipWithM = G.zipWithM

izipWithM :: Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
izipWithM = G.izipWithM

zipWithM_ :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m ()
zipWithM_ = G.zipWithM_

izipWithM_ :: Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
izipWithM_ = G.izipWithM_


filter :: (a -> Bool) -> Vector a -> Vector a
filter = G.filter

ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
ifilter = G.ifilter

uniq :: (Eq a) => Vector a -> Vector a
uniq = G.uniq

mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
mapMaybe = G.mapMaybe

imapMaybe :: (Int -> a -> Maybe b) -> Vector a -> Vector b
imapMaybe = G.imapMaybe

catMaybes :: Vector (Maybe a) -> Vector a
catMaybes = mapMaybe id

filterM :: Monad m => (a -> m Bool) -> Vector a -> m (Vector a)
filterM = G.filterM

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
mapMaybeM = G.mapMaybeM

imapMaybeM :: Monad m => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
imapMaybeM = G.imapMaybeM

takeWhile :: (a -> Bool) -> Vector a -> Vector a
takeWhile = G.takeWhile

dropWhile :: (a -> Bool) -> Vector a -> Vector a
dropWhile = G.dropWhile


partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
partition = G.partition

partitionWith :: (a -> Either b c) -> Vector a -> (Vector b, Vector c)
partitionWith = G.partitionWith

unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
unstablePartition = G.unstablePartition

span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
span = G.span

break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
break = G.break

spanR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
spanR = G.spanR

breakR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
breakR = G.breakR

groupBy :: (a -> a -> Bool) -> Vector a -> [Vector a]
groupBy = G.groupBy

group :: Eq a => Vector a -> [Vector a]
group = G.groupBy (==)


infix 4 `elem`
elem :: Eq a => a -> Vector a -> Bool
elem = G.elem

infix 4 `notElem`
notElem :: Eq a => a -> Vector a -> Bool
notElem = G.notElem

find :: (a -> Bool) -> Vector a -> Maybe a
find = G.find

findIndex :: (a -> Bool) -> Vector a -> Maybe Int
findIndex = G.findIndex

findIndexR :: (a -> Bool) -> Vector a -> Maybe Int
findIndexR = G.findIndexR

findIndices :: (a -> Bool) -> Vector a -> Vector Int
findIndices = G.findIndices

elemIndex :: Eq a => a -> Vector a -> Maybe Int
elemIndex = G.elemIndex

elemIndices :: Eq a => a -> Vector a -> Vector Int
elemIndices = G.elemIndices


foldl :: (a -> b -> a) -> a -> Vector b -> a
foldl = G.foldl

foldl1 :: (a -> a -> a) -> Vector a -> a
foldl1 = G.foldl1

foldl' :: (a -> b -> a) -> a -> Vector b -> a
foldl' = G.foldl'

foldl1' :: (a -> a -> a) -> Vector a -> a
foldl1' = G.foldl1'

foldr :: (a -> b -> b) -> b -> Vector a -> b
foldr = G.foldr

foldr1 :: (a -> a -> a) -> Vector a -> a
foldr1 = G.foldr1

foldr' :: (a -> b -> b) -> b -> Vector a -> b
foldr' = G.foldr'

foldr1' :: (a -> a -> a) -> Vector a -> a
foldr1' = G.foldr1'

ifoldl :: (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl = G.ifoldl

ifoldl' :: (a -> Int -> b -> a) -> a -> Vector b -> a
ifoldl' = G.ifoldl'

ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr = G.ifoldr

ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
ifoldr' = G.ifoldr'

foldMap :: (Monoid m) => (a -> m) -> Vector a -> m
foldMap = G.foldMap

foldMap' :: (Monoid m) => (a -> m) -> Vector a -> m
foldMap' = G.foldMap'

all :: (a -> Bool) -> Vector a -> Bool
all = G.all

any :: (a -> Bool) -> Vector a -> Bool
any = G.any

and :: Vector Bool -> Bool
and = G.and

or :: Vector Bool -> Bool
or = G.or

sum :: Num a => Vector a -> a
sum = G.sum

product :: Num a => Vector a -> a
product = G.product

maximum :: Ord a => Vector a -> a
maximum = G.maximum

maximumBy :: (a -> a -> Ordering) -> Vector a -> a
maximumBy = G.maximumBy

maximumOn :: Ord b => (a -> b) -> Vector a -> a
maximumOn = G.maximumOn

minimum :: Ord a => Vector a -> a
minimum = G.minimum

minimumBy :: (a -> a -> Ordering) -> Vector a -> a
minimumBy = G.minimumBy

minimumOn :: Ord b => (a -> b) -> Vector a -> a
minimumOn = G.minimumOn

maxIndex :: Ord a => Vector a -> Int
maxIndex = G.maxIndex

maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
maxIndexBy = G.maxIndexBy

minIndex :: Ord a => Vector a -> Int
minIndex = G.minIndex

minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
minIndexBy = G.minIndexBy


foldM :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
foldM = G.foldM

ifoldM :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM = G.ifoldM

fold1M :: Monad m => (a -> a -> m a) -> Vector a -> m a
fold1M = G.fold1M

foldM' :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
foldM' = G.foldM'

ifoldM' :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
ifoldM' = G.ifoldM'

fold1M' :: Monad m => (a -> a -> m a) -> Vector a -> m a
fold1M' = G.fold1M'

foldM_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
foldM_ = G.foldM_

ifoldM_ :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM_ = G.ifoldM_

fold1M_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
fold1M_ = G.fold1M_

foldM'_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
foldM'_ = G.foldM'_

ifoldM'_ :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
ifoldM'_ = G.ifoldM'_

fold1M'_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
fold1M'_ = G.fold1M'_


sequence :: Monad m => Vector (m a) -> m (Vector a)
sequence = G.sequence

sequence_ :: Monad m => Vector (m a) -> m ()
sequence_ = G.sequence_


prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
prescanl = G.prescanl

prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
prescanl' = G.prescanl'

postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
postscanl = G.postscanl

postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
postscanl' = G.postscanl'

scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl = G.scanl

scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
scanl' = G.scanl'

iscanl :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl = G.iscanl

iscanl' :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
iscanl' = G.iscanl'

scanl1 :: (a -> a -> a) -> Vector a -> Vector a
scanl1 = G.scanl1

scanl1' :: (a -> a -> a) -> Vector a -> Vector a
scanl1' = G.scanl1'

prescanr :: (a -> b -> b) -> b -> Vector a -> Vector b
prescanr = G.prescanr

prescanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
prescanr' = G.prescanr'

postscanr :: (a -> b -> b) -> b -> Vector a -> Vector b
postscanr = G.postscanr

postscanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
postscanr' = G.postscanr'

scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr = G.scanr

scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
scanr' = G.scanr'

iscanr :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr = G.iscanr

iscanr' :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
iscanr' = G.iscanr'

scanr1 :: (a -> a -> a) -> Vector a -> Vector a
scanr1 = G.scanr1

scanr1' :: (a -> a -> a) -> Vector a -> Vector a
scanr1' = G.scanr1'


eqBy :: (a -> b -> Bool) -> Vector a -> Vector b -> Bool
eqBy = G.eqBy

cmpBy :: (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
cmpBy = G.cmpBy


toList :: Vector a -> [a]
toList = G.toList

fromList :: [a] -> Vector a
fromList = G.fromList

fromListN :: Int -> [a] -> Vector a
fromListN = G.fromListN


fromArray :: Array a -> Vector a
fromArray arr = Vector 0 (sizeofArray arr) arr

toArray :: Vector a -> Array a
toArray (Vector offset len arr)
  | offset == 0 && len == sizeofArray arr = arr
  | otherwise = cloneArray arr offset len

toArraySlice :: Vector a -> (Array a, Int, Int)
toArraySlice (Vector offset len arr) = (arr, offset, len)


unsafeFromArraySlice ::
     Array a -- ^ Immutable boxed array.
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Vector a
unsafeFromArraySlice arr offset len = Vector offset len arr


unsafeFreeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
unsafeFreeze = G.unsafeFreeze

freeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
freeze = G.freeze

unsafeThaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
unsafeThaw = G.unsafeThaw

thaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
thaw = G.thaw

unsafeCopy :: PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
unsafeCopy = G.unsafeCopy

copy :: PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
copy = G.copy




module Data.HashMap.Internal
    (
      HashMap(..)
    , Leaf(..)

    , empty
    , singleton

    , null
    , size
    , member
    , lookup
    , (!?)
    , findWithDefault
    , lookupDefault
    , (!)
    , insert
    , insertWith
    , unsafeInsert
    , delete
    , adjust
    , update
    , alter
    , alterF
    , isSubmapOf
    , isSubmapOfBy

    , union
    , unionWith
    , unionWithKey
    , unions

    , compose

    , map
    , mapWithKey
    , traverseWithKey
    , mapKeys

    , difference
    , differenceWith
    , intersection
    , intersectionWith
    , intersectionWithKey
    , intersectionWithKey#

    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

    , mapMaybe
    , mapMaybeWithKey
    , filter
    , filterWithKey

    , keys
    , elems

    , toList
    , fromList
    , fromListWith
    , fromListWithKey

    , Hash
    , Bitmap
    , Shift
    , bitmapIndexedOrFull
    , collision
    , hash
    , mask
    , index
    , bitsPerSubkey
    , maxChildren
    , isLeafOrCollision
    , fullBitmap
    , subkeyMask
    , nextShift
    , sparseIndex
    , two
    , unionArrayBy
    , update32
    , update32M
    , update32With'
    , updateOrConcatWithKey
    , filterMapAux
    , equalKeys
    , equalKeys1
    , lookupRecordCollision
    , LookupRes(..)
    , lookupResToMaybe
    , insert'
    , delete'
    , lookup'
    , insertNewKey
    , insertKeyExists
    , deleteKeyExists
    , insertModifying
    , ptrEq
    , adjust#
    ) where

import Control.Applicative        (Const (..))
import Control.DeepSeq            (NFData (..), NFData1 (..), NFData2 (..))
import Control.Monad.ST           (ST, runST)
import Data.Bifoldable            (Bifoldable (..))
import Data.Bits                  (complement, countTrailingZeros, popCount,
                                   shiftL, unsafeShiftL, unsafeShiftR, (.&.),
                                   (.|.))
import Data.Coerce                (coerce)
import Data.Data                  (Constr, Data (..), DataType)
import Data.Functor.Classes       (Eq1 (..), Eq2 (..), Ord1 (..), Ord2 (..),
                                   Read1 (..), Show1 (..), Show2 (..))
import Data.Functor.Identity      (Identity (..))
import Data.Hashable              (Hashable)
import Data.Hashable.Lifted       (Hashable1, Hashable2)
import Data.HashMap.Internal.List (isPermutationBy, unorderedCompare)
import Data.Semigroup             (Semigroup (..), stimesIdempotentMonoid)
import GHC.Exts                   (Int (..), Int#, TYPE, (==#))
import GHC.Stack                  (HasCallStack)
import Prelude                    hiding (Foldable(..), filter, lookup, map,
                                   pred)
import Text.Read                  hiding (step)

import qualified Data.Data                   as Data
import qualified Data.Foldable               as Foldable
import qualified Data.Functor.Classes        as FC
import qualified Data.Hashable               as H
import qualified Data.Hashable.Lifted        as H
import qualified Data.HashMap.Internal.Array as A
import qualified Data.List                   as List
import qualified GHC.Exts                    as Exts
import qualified Language.Haskell.TH.Syntax  as TH

hash :: H.Hashable a => a -> Hash
hash = fromIntegral . H.hash

data Leaf k v = L !k v
  deriving (Eq)

instance (NFData k, NFData v) => NFData (Leaf k v) where
    rnf (L k v) = rnf k `seq` rnf v

instance (TH.Lift k, TH.Lift v) => TH.Lift (Leaf k v) where
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (L k v) = [|| L k $! v ||]
#else
  lift (L k v) = [| L k $! v |]
#endif

instance NFData k => NFData1 (Leaf k) where
    liftRnf = liftRnf2 rnf

instance NFData2 Leaf where
    liftRnf2 rnf1 rnf2 (L k v) = rnf1 k `seq` rnf2 v

data HashMap k v
    = Empty
    | BitmapIndexed !Bitmap !(A.Array (HashMap k v))
    | Leaf !Hash !(Leaf k v)
    | Full !(A.Array (HashMap k v))
    | Collision !Hash !(A.Array (Leaf k v))

type role HashMap nominal representational

deriving instance (TH.Lift k, TH.Lift v) => TH.Lift (HashMap k v)

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf Empty                 = ()
    rnf (BitmapIndexed _ ary) = rnf ary
    rnf (Leaf _ l)            = rnf l
    rnf (Full ary)            = rnf ary
    rnf (Collision _ ary)     = rnf ary

instance NFData k => NFData1 (HashMap k) where
    liftRnf = liftRnf2 rnf

instance NFData2 HashMap where
    liftRnf2 _ _ Empty                       = ()
    liftRnf2 rnf1 rnf2 (BitmapIndexed _ ary) = liftRnf (liftRnf2 rnf1 rnf2) ary
    liftRnf2 rnf1 rnf2 (Leaf _ l)            = liftRnf2 rnf1 rnf2 l
    liftRnf2 rnf1 rnf2 (Full ary)            = liftRnf (liftRnf2 rnf1 rnf2) ary
    liftRnf2 rnf1 rnf2 (Collision _ ary)     = liftRnf (liftRnf2 rnf1 rnf2) ary

instance Functor (HashMap k) where
    fmap = map

instance Foldable.Foldable (HashMap k) where
    foldMap f = foldMapWithKey (\ _k v -> f v)
    foldr = foldr
    foldl = foldl
    foldr' = foldr'
    foldl' = foldl'
    null = null
    length = size

instance Bifoldable HashMap where
    bifoldMap f g = foldMapWithKey (\ k v -> f k `mappend` g v)
    bifoldr f g = foldrWithKey (\ k v acc -> k `f` (v `g` acc))
    bifoldl f g = foldlWithKey (\ acc k v -> (acc `f` k) `g` v)

instance (Eq k, Hashable k) => Semigroup (HashMap k v) where
  (<>) = union
  stimes = stimesIdempotentMonoid

instance (Eq k, Hashable k) => Monoid (HashMap k v) where
  mempty = empty
  mappend = (<>)

instance (Data k, Data v, Eq k, Hashable k) => Data (HashMap k v) where
    gfoldl f z m   = z fromList `f` toList m
    toConstr _     = fromListConstr
    gunfold k z c  = case Data.constrIndex c of
        1 -> k (z fromList)
        _ -> error "gunfold"
    dataTypeOf _   = hashMapDataType
    dataCast1 f    = Data.gcast1 f
    dataCast2 f    = Data.gcast2 f

fromListConstr :: Constr
fromListConstr = Data.mkConstr hashMapDataType "fromList" [] Data.Prefix

hashMapDataType :: DataType
hashMapDataType = Data.mkDataType "Data.HashMap.Internal.HashMap" [fromListConstr]

type Hash   = Word

type Bitmap = Word

type Shift  = Int

instance Show2 HashMap where
    liftShowsPrec2 spk slk spv slv d m =
        FC.showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
      where
        sp = liftShowsPrec2 spk slk spv slv
        sl = liftShowList2 spk slk spv slv

instance Show k => Show1 (HashMap k) where
    liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Eq k, Hashable k, Read k) => Read1 (HashMap k) where
    liftReadsPrec rp rl = FC.readsData $
        FC.readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Eq k, Hashable k, Read k, Read e) => Read (HashMap k e) where
    readPrec = parens $ prec 10 $ do
      Ident "fromList" <- lexP
      fromList <$> readPrec

    readListPrec = readListPrecDefault

instance (Show k, Show v) => Show (HashMap k v) where
    showsPrec d m = showParen (d > 10) $
      showString "fromList " . shows (toList m)

instance Traversable (HashMap k) where
    traverse f = traverseWithKey (const f)

instance Eq2 HashMap where
    liftEq2 = equal2

instance Eq k => Eq1 (HashMap k) where
    liftEq = equal1

instance (Eq k, Eq v) => Eq (HashMap k v) where
    (==) = equal1 (==)

equal1 :: Eq k
       => (v -> v' -> Bool)
       -> HashMap k v -> HashMap k v' -> Bool
equal1 eq = go
  where
    go Empty Empty = True
    go (BitmapIndexed bm1 ary1) (BitmapIndexed bm2 ary2)
      = bm1 == bm2 && A.sameArray1 go ary1 ary2
    go (Leaf h1 l1) (Leaf h2 l2) = h1 == h2 && leafEq l1 l2
    go (Full ary1) (Full ary2) = A.sameArray1 go ary1 ary2
    go (Collision h1 ary1) (Collision h2 ary2)
      = h1 == h2 && isPermutationBy leafEq (A.toList ary1) (A.toList ary2)
    go _ _ = False

    leafEq (L k1 v1) (L k2 v2) = k1 == k2 && eq v1 v2

equal2 :: (k -> k' -> Bool) -> (v -> v' -> Bool)
      -> HashMap k v -> HashMap k' v' -> Bool
equal2 eqk eqv t1 t2 = go (leavesAndCollisions t1 []) (leavesAndCollisions t2 [])
  where

    go (Leaf k1 l1 : tl1) (Leaf k2 l2 : tl2)
      | k1 == k2 &&
        leafEq l1 l2
      = go tl1 tl2
    go (Collision h1 ary1 : tl1) (Collision h2 ary2 : tl2)
      | h1 == h2 &&
        A.length ary1 == A.length ary2 &&
        isPermutationBy leafEq (A.toList ary1) (A.toList ary2)
      = go tl1 tl2
    go [] [] = True
    go _  _  = False

    leafEq (L k v) (L k' v') = eqk k k' && eqv v v'

instance Ord2 HashMap where
    liftCompare2 = cmp

instance Ord k => Ord1 (HashMap k) where
    liftCompare = cmp compare

instance (Ord k, Ord v) => Ord (HashMap k v) where
    compare = cmp compare compare

cmp :: (k -> k' -> Ordering) -> (v -> v' -> Ordering)
    -> HashMap k v -> HashMap k' v' -> Ordering
cmp cmpk cmpv t1 t2 = go (leavesAndCollisions t1 []) (leavesAndCollisions t2 [])
  where
    go (Leaf k1 l1 : tl1) (Leaf k2 l2 : tl2)
      = compare k1 k2 `mappend`
        leafCompare l1 l2 `mappend`
        go tl1 tl2
    go (Collision h1 ary1 : tl1) (Collision h2 ary2 : tl2)
      = compare h1 h2 `mappend`
        compare (A.length ary1) (A.length ary2) `mappend`
        unorderedCompare leafCompare (A.toList ary1) (A.toList ary2) `mappend`
        go tl1 tl2
    go (Leaf _ _ : _) (Collision _ _ : _) = LT
    go (Collision _ _ : _) (Leaf _ _ : _) = GT
    go [] [] = EQ
    go [] _  = LT
    go _  [] = GT
    go _ _ = error "cmp: Should never happen, leavesAndCollisions includes non Leaf / Collision"

    leafCompare (L k v) (L k' v') = cmpk k k' `mappend` cmpv v v'

equalKeys1 :: (k -> k' -> Bool) -> HashMap k v -> HashMap k' v' -> Bool
equalKeys1 eq t1 t2 = go (leavesAndCollisions t1 []) (leavesAndCollisions t2 [])
  where
    go (Leaf k1 l1 : tl1) (Leaf k2 l2 : tl2)
      | k1 == k2 && leafEq l1 l2
      = go tl1 tl2
    go (Collision h1 ary1 : tl1) (Collision h2 ary2 : tl2)
      | h1 == h2 && A.length ary1 == A.length ary2 &&
        isPermutationBy leafEq (A.toList ary1) (A.toList ary2)
      = go tl1 tl2
    go [] [] = True
    go _  _  = False

    leafEq (L k _) (L k' _) = eq k k'

equalKeys :: Eq k => HashMap k v -> HashMap k v' -> Bool
equalKeys = go
  where
    go :: Eq k => HashMap k v -> HashMap k v' -> Bool
    go Empty Empty = True
    go (BitmapIndexed bm1 ary1) (BitmapIndexed bm2 ary2)
      = bm1 == bm2 && A.sameArray1 go ary1 ary2
    go (Leaf h1 l1) (Leaf h2 l2) = h1 == h2 && leafEq l1 l2
    go (Full ary1) (Full ary2) = A.sameArray1 go ary1 ary2
    go (Collision h1 ary1) (Collision h2 ary2)
      = h1 == h2 && isPermutationBy leafEq (A.toList ary1) (A.toList ary2)
    go _ _ = False

    leafEq (L k1 _) (L k2 _) = k1 == k2

instance Hashable2 HashMap where
    liftHashWithSalt2 hk hv salt hm = go salt (leavesAndCollisions hm [])
      where
        go s [] = s
        go s (Leaf _ l : tl)
          = s `hashLeafWithSalt` l `go` tl
        go s (Collision h a : tl)
          = (s `H.hashWithSalt` h) `hashCollisionWithSalt` a `go` tl
        go s (_ : tl) = s `go` tl

        hashLeafWithSalt s (L k v) = (s `hk` k) `hv` v

        hashCollisionWithSalt s
          = List.foldl' H.hashWithSalt s . arrayHashesSorted s

        arrayHashesSorted s = List.sort . List.map (hashLeafWithSalt s) . A.toList

instance (Hashable k) => Hashable1 (HashMap k) where
    liftHashWithSalt = H.liftHashWithSalt2 H.hashWithSalt

instance (Hashable k, Hashable v) => Hashable (HashMap k v) where
    hashWithSalt salt hm = go salt hm
      where
        go :: Int -> HashMap k v -> Int
        go s Empty = s
        go s (BitmapIndexed _ a) = A.foldl' go s a
        go s (Leaf h (L _ v))
          = s `H.hashWithSalt` h `H.hashWithSalt` v
        go s (Full a) = A.foldl' go s a
        go s (Collision h a)
          = (s `H.hashWithSalt` h) `hashCollisionWithSalt` a

        hashLeafWithSalt :: Int -> Leaf k v -> Int
        hashLeafWithSalt s (L k v) = s `H.hashWithSalt` k `H.hashWithSalt` v

        hashCollisionWithSalt :: Int -> A.Array (Leaf k v) -> Int
        hashCollisionWithSalt s
          = List.foldl' H.hashWithSalt s . arrayHashesSorted s

        arrayHashesSorted :: Int -> A.Array (Leaf k v) -> [Int]
        arrayHashesSorted s = List.sort . List.map (hashLeafWithSalt s) . A.toList

leavesAndCollisions :: HashMap k v -> [HashMap k v] -> [HashMap k v]
leavesAndCollisions (BitmapIndexed _ ary) a = A.foldr leavesAndCollisions a ary
leavesAndCollisions (Full ary)            a = A.foldr leavesAndCollisions a ary
leavesAndCollisions l@(Leaf _ _)          a = l : a
leavesAndCollisions c@(Collision _ _)     a = c : a
leavesAndCollisions Empty                 a = a

isLeafOrCollision :: HashMap k v -> Bool
isLeafOrCollision (Leaf _ _)      = True
isLeafOrCollision (Collision _ _) = True
isLeafOrCollision _               = False


empty :: HashMap k v
empty = Empty

singleton :: (Hashable k) => k -> v -> HashMap k v
singleton k v = Leaf (hash k) (L k v)


null :: HashMap k v -> Bool
null Empty = True
null _   = False

size :: HashMap k v -> Int
size t = go t 0
  where
    go Empty                !n = n
    go (Leaf _ _)            n = n + 1
    go (BitmapIndexed _ ary) n = A.foldl' (flip go) n ary
    go (Full ary)            n = A.foldl' (flip go) n ary
    go (Collision _ ary)     n = n + A.length ary

member :: (Eq k, Hashable k) => k -> HashMap k a -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _  -> True

lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
lookup k m = case lookup# k m of
  (# (# #) | #) -> Nothing
  (# | a #) -> Just a

lookup# :: (Eq k, Hashable k) => k -> HashMap k v -> (# (# #) | v #)
lookup# k m = lookupCont (\_ -> (# (# #) | #)) (\v _i -> (# | v #)) (hash k) k 0 m

lookup' :: Eq k => Hash -> k -> HashMap k v -> Maybe v
lookup' h k m = case lookupRecordCollision# h k m of
  (# (# #) | #) -> Nothing
  (# | (# a, _i #) #) -> Just a

data LookupRes a = Absent | Present a !Int

lookupResToMaybe :: LookupRes a -> Maybe a
lookupResToMaybe Absent        = Nothing
lookupResToMaybe (Present x _) = Just x

lookupRecordCollision :: Eq k => Hash -> k -> HashMap k v -> LookupRes v
lookupRecordCollision h k m = case lookupRecordCollision# h k m of
  (# (# #) | #) -> Absent
  (# | (# a, i #) #) -> Present a (I# i) -- GHC will eliminate the I#

lookupRecordCollision# :: Eq k => Hash -> k -> HashMap k v -> (# (# #) | (# v, Int# #) #)
lookupRecordCollision# h k m =
    lookupCont (\_ -> (# (# #) | #)) (\v (I# i) -> (# | (# v, i #) #)) h k 0 m

lookupCont ::
  forall rep (r :: TYPE rep) k v.
     Eq k
  => ((# #) -> r)    -- Absent continuation
  -> (v -> Int -> r) -- Present continuation
  -> Hash -- The hash of the key
  -> k
  -> Int -- The offset of the subkey in the hash.
  -> HashMap k v -> r
lookupCont absent present !h0 !k0 !s0 !m0 = go h0 k0 s0 m0
  where
    go :: Eq k => Hash -> k -> Int -> HashMap k v -> r
    go !_ !_ !_ Empty = absent (# #)
    go h k _ (Leaf hx (L kx x))
        | h == hx && k == kx = present x (-1)
        | otherwise          = absent (# #)
    go h k s (BitmapIndexed b v)
        | b .&. m == 0 = absent (# #)
        | otherwise    =
            go h k (nextShift s) (A.index v (sparseIndex b m))
      where m = mask h s
    go h k s (Full v) =
      go h k (nextShift s) (A.index v (index h s))
    go h k _ (Collision hx v)
        | h == hx   = lookupInArrayCont absent present k v
        | otherwise = absent (# #)

(!?) :: (Eq k, Hashable k) => HashMap k v -> k -> Maybe v
(!?) m k = lookup k m


findWithDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> HashMap k v -> v
findWithDefault def k t = case lookup k t of
    Just v -> v
    _      -> def


lookupDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> HashMap k v -> v
lookupDefault = findWithDefault

(!) :: (Eq k, Hashable k, HasCallStack) => HashMap k v -> k -> v
(!) m k = case lookup k m of
    Just v  -> v
    Nothing -> error "Data.HashMap.Internal.(!): key not found"

infixl 9 !

collision :: Hash -> Leaf k v -> Leaf k v -> HashMap k v
collision h !e1 !e2 =
    let v = A.run $ do mary <- A.new 2 e1
                       A.write mary 1 e2
                       return mary
    in Collision h v

bitmapIndexedOrFull :: Bitmap -> A.Array (HashMap k v) -> HashMap k v
bitmapIndexedOrFull b !ary
    | b == fullBitmap = Full ary
    | otherwise         = BitmapIndexed b ary

insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k v m = insert' (hash k) k v m

insert' :: Eq k => Hash -> k -> v -> HashMap k v -> HashMap k v
insert' h0 k0 v0 m0 = go h0 k0 v0 0 m0
  where
    go !h !k x !_ Empty = Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then if x `ptrEq` y
                         then t
                         else Leaf h (L k x)
                    else collision h l (L k x)
        | otherwise = runST (two s h k x hy t)
    go h k x s t@(BitmapIndexed b ary)
        | b .&. m == 0 =
            let !ary' = A.insert ary i $! Leaf h (L k x)
            in bitmapIndexedOrFull (b .|. m) ary'
        | otherwise =
            let !st  = A.index ary i
                !st' = go h k x (nextShift s) st
            in if st' `ptrEq` st
               then t
               else BitmapIndexed b (A.update ary i st')
      where m = mask h s
            i = sparseIndex b m
    go h k x s t@(Full ary) =
        let !st  = A.index ary i
            !st' = go h k x (nextShift s) st
        in if st' `ptrEq` st
            then t
            else Full (update32 ary i st')
      where i = index h s
    go h k x s t@(Collision hy v)
        | h == hy   = Collision h (updateOrSnocWith (\a _ -> (# a #)) k x v)
        | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)

insertNewKey :: Hash -> k -> v -> HashMap k v -> HashMap k v
insertNewKey !h0 !k0 x0 !m0 = go h0 k0 x0 0 m0
  where
    go !h !k x !_ Empty = Leaf h (L k x)
    go h k x s t@(Leaf hy l)
      | hy == h = collision h l (L k x)
      | otherwise = runST (two s h k x hy t)
    go h k x s (BitmapIndexed b ary)
        | b .&. m == 0 =
            let !ary' = A.insert ary i $! Leaf h (L k x)
            in bitmapIndexedOrFull (b .|. m) ary'
        | otherwise =
            let !st  = A.index ary i
                !st' = go h k x (nextShift s) st
            in BitmapIndexed b (A.update ary i st')
      where m = mask h s
            i = sparseIndex b m
    go h k x s (Full ary) =
        let !st  = A.index ary i
            !st' = go h k x (nextShift s) st
        in Full (update32 ary i st')
      where i = index h s
    go h k x s t@(Collision hy v)
        | h == hy   = Collision h (A.snoc v (L k x))
        | otherwise =
            go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)


insertKeyExists :: Int -> Hash -> k -> v -> HashMap k v -> HashMap k v
insertKeyExists !collPos0 !h0 !k0 x0 !m0 = go collPos0 h0 k0 x0 m0
  where
    go !_collPos !_shiftedHash !k x (Leaf h _kx)
        = Leaf h (L k x)
    go collPos shiftedHash k x (BitmapIndexed b ary) =
        let !st  = A.index ary i
            !st' = go collPos (shiftHash shiftedHash) k x st
        in BitmapIndexed b (A.update ary i st')
      where m = mask' shiftedHash
            i = sparseIndex b m
    go collPos shiftedHash k x (Full ary) =
        let !st  = A.index ary i
            !st' = go collPos (shiftHash shiftedHash) k x st
        in Full (update32 ary i st')
      where i = index' shiftedHash
    go collPos _shiftedHash k x (Collision h v)
        | collPos >= 0 = Collision h (setAtPosition collPos k x v)
        | otherwise = Empty -- error "Internal error: go {collPos negative}"
    go _ _ _ _ Empty = Empty -- error "Internal error: go Empty"

    index' :: Hash -> Int
    index' w = fromIntegral $ w .&. subkeyMask

    mask' :: Word -> Bitmap
    mask' w = 1 `unsafeShiftL` index' w

    shiftHash h = h `unsafeShiftR` bitsPerSubkey


setAtPosition :: Int -> k -> v -> A.Array (Leaf k v) -> A.Array (Leaf k v)
setAtPosition i k x ary = A.update ary i (L k x)


unsafeInsert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
unsafeInsert k0 v0 m0 = runST (go h0 k0 v0 0 m0)
  where
    h0 = hash k0
    go !h !k x !_ Empty = return $! Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then if x `ptrEq` y
                         then return t
                         else return $! Leaf h (L k x)
                    else return $! collision h l (L k x)
        | otherwise = two s h k x hy t
    go h k x s t@(BitmapIndexed b ary)
        | b .&. m == 0 = do
            ary' <- A.insertM ary i $! Leaf h (L k x)
            return $! bitmapIndexedOrFull (b .|. m) ary'
        | otherwise = do
            st <- A.indexM ary i
            st' <- go h k x (nextShift s) st
            A.unsafeUpdateM ary i st'
            return t
      where m = mask h s
            i = sparseIndex b m
    go h k x s t@(Full ary) = do
        st <- A.indexM ary i
        st' <- go h k x (nextShift s) st
        A.unsafeUpdateM ary i st'
        return t
      where i = index h s
    go h k x s t@(Collision hy v)
        | h == hy   = return $! Collision h (updateOrSnocWith (\a _ -> (# a #)) k x v)
        | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)

two :: Shift -> Hash -> k -> v -> Hash -> HashMap k v -> ST s (HashMap k v)
two = go
  where
    go s h1 k1 v1 h2 t2
        | bp1 == bp2 = do
            st <- go (nextShift s) h1 k1 v1 h2 t2
            ary <- A.singletonM st
            return $ BitmapIndexed bp1 ary
        | otherwise  = do
            mary <- A.new 2 $! Leaf h1 (L k1 v1)
            A.write mary idx2 t2
            ary <- A.unsafeFreeze mary
            return $ BitmapIndexed (bp1 .|. bp2) ary
      where
        bp1  = mask h1 s
        bp2  = mask h2 s
        !(I# i1) = index h1 s
        !(I# i2) = index h2 s
        idx2 = I# (i1 Exts.<# i2)

insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
            -> HashMap k v
insertWith f k new m = insertModifying new (\old -> (# f new old #)) k m

insertModifying :: (Eq k, Hashable k) => v -> (v -> (# v #)) -> k -> HashMap k v
            -> HashMap k v
insertModifying x f k0 m0 = go h0 k0 0 m0
  where
    !h0 = hash k0
    go !h !k !_ Empty = Leaf h (L k x)
    go h k s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then case f y of
                      (# v' #) | ptrEq y v' -> t
                               | otherwise -> Leaf h (L k v')
                    else collision h l (L k x)
        | otherwise = runST (two s h k x hy t)
    go h k s t@(BitmapIndexed b ary)
        | b .&. m == 0 =
            let ary' = A.insert ary i $! Leaf h (L k x)
            in bitmapIndexedOrFull (b .|. m) ary'
        | otherwise =
            let !st   = A.index ary i
                !st'  = go h k (nextShift s) st
                ary'  = A.update ary i $! st'
            in if ptrEq st st'
               then t
               else BitmapIndexed b ary'
      where m = mask h s
            i = sparseIndex b m
    go h k s t@(Full ary) =
        let !st   = A.index ary i
            !st'  = go h k (nextShift s) st
            ary' = update32 ary i $! st'
        in if ptrEq st st'
           then t
           else Full ary'
      where i = index h s
    go h k s t@(Collision hy v)
        | h == hy   =
            let !v' = insertModifyingArr x f k v
            in if A.unsafeSameArray v v'
               then t
               else Collision h v'
        | otherwise = go h k s $ BitmapIndexed (mask hy s) (A.singleton t)

insertModifyingArr :: Eq k => v -> (v -> (# v #)) -> k -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
insertModifyingArr x f k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n = A.snoc ary $ L k x
        | otherwise = case A.index ary i of
            (L kx y) | k == kx   -> case f y of
                                      (# y' #) -> if ptrEq y y'
                                                  then ary
                                                  else A.update ary i (L k y')
                     | otherwise -> go k ary (i+1) n

unsafeInsertWith :: forall k v. (Eq k, Hashable k)
                 => (v -> v -> v) -> k -> v -> HashMap k v
                 -> HashMap k v
unsafeInsertWith f k0 v0 m0 = unsafeInsertWithKey (\_ a b -> (# f a b #)) k0 v0 m0

unsafeInsertWithKey :: forall k v. (Eq k, Hashable k)
                 => (k -> v -> v -> (# v #)) -> k -> v -> HashMap k v
                 -> HashMap k v
unsafeInsertWithKey f k0 v0 m0 = runST (go h0 k0 v0 0 m0)
  where
    h0 = hash k0
    go :: Hash -> k -> v -> Shift -> HashMap k v -> ST s (HashMap k v)
    go !h !k x !_ Empty = return $! Leaf h (L k x)
    go h k x s t@(Leaf hy l@(L ky y))
        | hy == h = if ky == k
                    then case f k x y of
                        (# v #) -> return $! Leaf h (L k v)
                    else return $! collision h l (L k x)
        | otherwise = two s h k x hy t
    go h k x s t@(BitmapIndexed b ary)
        | b .&. m == 0 = do
            ary' <- A.insertM ary i $! Leaf h (L k x)
            return $! bitmapIndexedOrFull (b .|. m) ary'
        | otherwise = do
            st <- A.indexM ary i
            st' <- go h k x (nextShift s) st
            A.unsafeUpdateM ary i st'
            return t
      where m = mask h s
            i = sparseIndex b m
    go h k x s t@(Full ary) = do
        st <- A.indexM ary i
        st' <- go h k x (nextShift s) st
        A.unsafeUpdateM ary i st'
        return t
      where i = index h s
    go h k x s t@(Collision hy v)
        | h == hy   = return $! Collision h (updateOrSnocWithKey f k x v)
        | otherwise = go h k x s $ BitmapIndexed (mask hy s) (A.singleton t)

delete :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
delete k m = delete' (hash k) k m

delete' :: Eq k => Hash -> k -> HashMap k v -> HashMap k v
delete' h0 k0 m0 = go h0 k0 0 m0
  where
    go !_ !_ !_ Empty = Empty
    go h k _ t@(Leaf hy (L ky _))
        | hy == h && ky == k = Empty
        | otherwise          = t
    go h k s t@(BitmapIndexed b ary)
        | b .&. m == 0 = t
        | otherwise =
            let !st = A.index ary i
                !st' = go h k (nextShift s) st
            in if st' `ptrEq` st
                then t
                else case st' of
                Empty | A.length ary == 1 -> Empty
                      | A.length ary == 2 ->
                          case (i, A.index ary 0, A.index ary 1) of
                          (0, _, l) | isLeafOrCollision l -> l
                          (1, l, _) | isLeafOrCollision l -> l
                          _                               -> bIndexed
                      | otherwise -> bIndexed
                    where
                      bIndexed = BitmapIndexed (b .&. complement m) (A.delete ary i)
                l | isLeafOrCollision l && A.length ary == 1 -> l
                _ -> BitmapIndexed b (A.update ary i st')
      where m = mask h s
            i = sparseIndex b m
    go h k s t@(Full ary) =
        let !st   = A.index ary i
            !st' = go h k (nextShift s) st
        in if st' `ptrEq` st
            then t
            else case st' of
            Empty ->
                let ary' = A.delete ary i
                    bm   = fullBitmap .&. complement (1 `unsafeShiftL` i)
                in BitmapIndexed bm ary'
            _ -> Full (A.update ary i st')
      where i = index h s
    go h k _ t@(Collision hy v)
        | h == hy = case indexOf k v of
            Just i
                | A.length v == 2 ->
                    if i == 0
                    then Leaf h (A.index v 1)
                    else Leaf h (A.index v 0)
                | otherwise -> Collision h (A.delete v i)
            Nothing -> t
        | otherwise = t

deleteKeyExists :: Int -> Hash -> k -> HashMap k v -> HashMap k v
deleteKeyExists !collPos0 !h0 !k0 !m0 = go collPos0 h0 k0 m0
  where
    go :: Int -> Word -> k -> HashMap k v -> HashMap k v
    go !_collPos !_shiftedHash !_k (Leaf _ _) = Empty
    go collPos shiftedHash k (BitmapIndexed b ary) =
            let !st = A.index ary i
                !st' = go collPos (shiftHash shiftedHash) k st
            in case st' of
                Empty | A.length ary == 1 -> Empty
                      | A.length ary == 2 ->
                          case (i, A.index ary 0, A.index ary 1) of
                          (0, _, l) | isLeafOrCollision l -> l
                          (1, l, _) | isLeafOrCollision l -> l
                          _                               -> bIndexed
                      | otherwise -> bIndexed
                    where
                      bIndexed = BitmapIndexed (b .&. complement m) (A.delete ary i)
                l | isLeafOrCollision l && A.length ary == 1 -> l
                _ -> BitmapIndexed b (A.update ary i st')
      where m = mask' shiftedHash
            i = sparseIndex b m
    go collPos shiftedHash k (Full ary) =
        let !st   = A.index ary i
            !st' = go collPos (shiftHash shiftedHash) k st
        in case st' of
            Empty ->
                let ary' = A.delete ary i
                    bm   = fullBitmap .&. complement (1 `unsafeShiftL` i)
                in BitmapIndexed bm ary'
            _ -> Full (A.update ary i st')
      where i = index' shiftedHash
    go collPos _shiftedHash _k (Collision h v)
      | A.length v == 2
      = if collPos == 0
        then Leaf h (A.index v 1)
        else Leaf h (A.index v 0)
      | otherwise = Collision h (A.delete v collPos)
    go !_ !_ !_ Empty = Empty -- error "Internal error: deleteKeyExists empty"

    index' :: Hash -> Int
    index' w = fromIntegral $ w .&. subkeyMask

    mask' :: Word -> Bitmap
    mask' w = 1 `unsafeShiftL` index' w

    shiftHash h = h `unsafeShiftR` bitsPerSubkey


adjust :: (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
adjust f k m = adjust# (\v -> (# f v #)) k m

adjust# :: (Eq k, Hashable k) => (v -> (# v #)) -> k -> HashMap k v -> HashMap k v
adjust# f k0 m0 = go h0 k0 0 m0
  where
    h0 = hash k0
    go !_ !_ !_ Empty = Empty
    go h k _ t@(Leaf hy (L ky y))
        | hy == h && ky == k = case f y of
            (# y' #) | ptrEq y y' -> t
                     | otherwise -> Leaf h (L k y')
        | otherwise          = t
    go h k s t@(BitmapIndexed b ary)
        | b .&. m == 0 = t
        | otherwise = let !st   = A.index ary i
                          !st'  = go h k (nextShift s) st
                          ary' = A.update ary i $! st'
                      in if ptrEq st st'
                         then t
                         else BitmapIndexed b ary'
      where m = mask h s
            i = sparseIndex b m
    go h k s t@(Full ary) =
        let i    = index h s
            !st   = A.index ary i
            !st'  = go h k (nextShift s) st
            ary' = update32 ary i $! st'
        in if ptrEq st st'
           then t
           else Full ary'
    go h k _ t@(Collision hy v)
        | h == hy   = let !v' = updateWith# f k v
                      in if A.unsafeSameArray v v'
                         then t
                         else Collision h v'
        | otherwise = t

update :: (Eq k, Hashable k) => (a -> Maybe a) -> k -> HashMap k a -> HashMap k a
update f = alter (>>= f)


alter :: (Eq k, Hashable k) => (Maybe v -> Maybe v) -> k -> HashMap k v -> HashMap k v
alter f k m =
    let !h = hash k
        !lookupRes = lookupRecordCollision h k m
    in case f (lookupResToMaybe lookupRes) of
        Nothing -> case lookupRes of
            Absent            -> m
            Present _ collPos -> deleteKeyExists collPos h k m
        Just v' -> case lookupRes of
            Absent            -> insertNewKey h k v' m
            Present v collPos ->
                if v `ptrEq` v'
                    then m
                    else insertKeyExists collPos h k v' m

alterF :: (Functor f, Eq k, Hashable k)
       => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
alterF f = \ !k !m ->
  let
    !h = hash k
    mv = lookup' h k m
  in (<$> f mv) $ \case
    Nothing -> maybe m (const (delete' h k m)) mv
    Just v' -> insert' h k v' m


test_bottom :: a
test_bottom = error "Data.HashMap.alterF internal error: hit test_bottom"

bogus# :: (# #) -> (# a #)
bogus# _ = error "Data.HashMap.alterF internal error: hit bogus#"


"alterFWeird" forall f. alterF f =
   alterFWeird (f Nothing) (f (Just test_bottom)) f

"alterFconstant" forall (f :: Maybe a -> Identity (Maybe a)) x.
  alterFWeird x x f = \ !k !m ->
    Identity (case runIdentity x of {Nothing -> delete k m; Just a -> insert k a m})

"alterFinsertWith" [1] forall (f :: Maybe a -> Identity (Maybe a)) x y.
  alterFWeird (coerce (Just x)) (coerce (Just y)) f =
    coerce (insertModifying x (\mold -> case runIdentity (f (Just mold)) of
                                            Nothing -> bogus# (# #)
                                            Just new -> (# new #)))

"alterFadjust" forall (f :: Maybe a -> Identity (Maybe a)) _y.
  alterFWeird (coerce Nothing) (coerce (Just _y)) f =
    coerce (adjust# (\x -> case runIdentity (f (Just x)) of
                               Just x' -> (# x' #)
                               Nothing -> bogus# (# #)))

"alterFlookup" forall _ign1 _ign2 (f :: Maybe a -> Const r (Maybe a)).
  alterFWeird _ign1 _ign2 f = \ !k !m -> Const (getConst (f (lookup k m)))

alterFWeird
       :: (Functor f, Eq k, Hashable k)
       => f (Maybe v)
       -> f (Maybe v)
       -> (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
alterFWeird _ _ f = alterFEager f

alterFEager :: (Functor f, Eq k, Hashable k)
       => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
alterFEager f !k m = (<$> f mv) $ \case

    Nothing -> case lookupRes of

      Absent -> m

      Present _ collPos -> deleteKeyExists collPos h k m

    Just v' -> case lookupRes of

      Absent -> insertNewKey h k v' m

      Present v collPos ->
        if v `ptrEq` v'
        then m
        else insertKeyExists collPos h k v' m

  where !h = hash k
        !lookupRes = lookupRecordCollision h k m
        !mv = lookupResToMaybe lookupRes

isSubmapOf :: (Eq k, Hashable k, Eq v) => HashMap k v -> HashMap k v -> Bool
isSubmapOf = Exts.inline isSubmapOfBy (==)

isSubmapOfBy :: (Eq k, Hashable k) => (v1 -> v2 -> Bool) -> HashMap k v1 -> HashMap k v2 -> Bool
isSubmapOfBy comp !m1 !m2 = go 0 m1 m2
  where
    go _ Empty _ = True

    go _ _ Empty = False

    go s (Leaf h1 (L k1 v1)) t2 = lookupCont (\_ -> False) (\v2 _ -> comp v1 v2) h1 k1 s t2

    go _ (Collision h1 ls1) (Collision h2 ls2) =
      h1 == h2 && subsetArray comp ls1 ls2

    go s t1@(Collision h1 _) (BitmapIndexed b ls2)
        | b .&. m == 0 = False
        | otherwise    =
            go (nextShift s) t1 (A.index ls2 (sparseIndex b m))
      where m = mask h1 s

    go s t1@(Collision h1 _) (Full ls2) =
      go (nextShift s) t1 (A.index ls2 (index h1 s))

    go s (BitmapIndexed b1 ls1) (BitmapIndexed b2 ls2) =
      submapBitmapIndexed (go (nextShift s)) b1 ls1 b2 ls2
    go s (BitmapIndexed b1 ls1) (Full ls2) =
      submapBitmapIndexed (go (nextShift s)) b1 ls1 fullBitmap ls2
    go s (Full ls1) (Full ls2) =
      submapBitmapIndexed (go (nextShift s)) fullBitmap ls1 fullBitmap ls2

    go _ (Collision {}) (Leaf {}) = False
    go _ (BitmapIndexed {}) (Leaf {}) = False
    go _ (Full {}) (Leaf {}) = False
    go _ (BitmapIndexed {}) (Collision {}) = False
    go _ (Full {}) (Collision {}) = False
    go _ (Full {}) (BitmapIndexed {}) = False

submapBitmapIndexed :: (HashMap k v1 -> HashMap k v2 -> Bool) -> Bitmap -> A.Array (HashMap k v1) -> Bitmap -> A.Array (HashMap k v2) -> Bool
submapBitmapIndexed comp !b1 !ary1 !b2 !ary2 = subsetBitmaps && go 0 0 (b1Orb2 .&. negate b1Orb2)
  where
    go :: Int -> Int -> Bitmap -> Bool
    go !i !j !m
      | m > b1Orb2 = True

      | b1Andb2 .&. m /= 0 = comp (A.index ary1 i) (A.index ary2 j) &&
                             go (i+1) (j+1) (m `unsafeShiftL` 1)

      | b2 .&. m /= 0 = go i (j+1) (m `unsafeShiftL` 1)

      | otherwise = go i j (m `unsafeShiftL` 1)

    b1Andb2 = b1 .&. b2
    b1Orb2  = b1 .|. b2
    subsetBitmaps = b1Orb2 == b2


union :: Eq k => HashMap k v -> HashMap k v -> HashMap k v
union = unionWith const

unionWith :: Eq k => (v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWith f = unionWithKey (const f)

unionWithKey :: Eq k => (k -> v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWithKey f = go 0
  where
    go !_ t1 Empty = t1
    go _ Empty t2 = t2
    go s t1@(Leaf h1 l1@(L k1 v1)) t2@(Leaf h2 l2@(L k2 v2))
        | h1 == h2  = if k1 == k2
                      then Leaf h1 (L k1 (f k1 v1 v2))
                      else collision h1 l1 l2
        | otherwise = goDifferentHash s h1 h2 t1 t2
    go s t1@(Leaf h1 (L k1 v1)) t2@(Collision h2 ls2)
        | h1 == h2  = Collision h1 (updateOrSnocWithKey (\k a b -> (# f k a b #)) k1 v1 ls2)
        | otherwise = goDifferentHash s h1 h2 t1 t2
    go s t1@(Collision h1 ls1) t2@(Leaf h2 (L k2 v2))
        | h1 == h2  = Collision h1 (updateOrSnocWithKey (\k a b -> (# f k b a #)) k2 v2 ls1)
        | otherwise = goDifferentHash s h1 h2 t1 t2
    go s t1@(Collision h1 ls1) t2@(Collision h2 ls2)
        | h1 == h2  = Collision h1 (updateOrConcatWithKey (\k a b -> (# f k a b #)) ls1 ls2)
        | otherwise = goDifferentHash s h1 h2 t1 t2
    go s (BitmapIndexed b1 ary1) (BitmapIndexed b2 ary2) =
        let b'   = b1 .|. b2
            ary' = unionArrayBy (go (nextShift s)) b1 b2 ary1 ary2
        in bitmapIndexedOrFull b' ary'
    go s (BitmapIndexed b1 ary1) (Full ary2) =
        let ary' = unionArrayBy (go (nextShift s)) b1 fullBitmap ary1 ary2
        in Full ary'
    go s (Full ary1) (BitmapIndexed b2 ary2) =
        let ary' = unionArrayBy (go (nextShift s)) fullBitmap b2 ary1 ary2
        in Full ary'
    go s (Full ary1) (Full ary2) =
        let ary' = unionArrayBy (go (nextShift s)) fullBitmap fullBitmap
                   ary1 ary2
        in Full ary'
    go s (BitmapIndexed b1 ary1) t2
        | b1 .&. m2 == 0 = let ary' = A.insert ary1 i t2
                               b'   = b1 .|. m2
                           in bitmapIndexedOrFull b' ary'
        | otherwise      = let ary' = A.updateWith' ary1 i $ \st1 ->
                                   go (nextShift s) st1 t2
                           in BitmapIndexed b1 ary'
        where
          h2 = leafHashCode t2
          m2 = mask h2 s
          i = sparseIndex b1 m2
    go s t1 (BitmapIndexed b2 ary2)
        | b2 .&. m1 == 0 = let ary' = A.insert ary2 i $! t1
                               b'   = b2 .|. m1
                           in bitmapIndexedOrFull b' ary'
        | otherwise      = let ary' = A.updateWith' ary2 i $ \st2 ->
                                   go (nextShift s) t1 st2
                           in BitmapIndexed b2 ary'
      where
        h1 = leafHashCode t1
        m1 = mask h1 s
        i = sparseIndex b2 m1
    go s (Full ary1) t2 =
        let h2   = leafHashCode t2
            i    = index h2 s
            ary' = update32With' ary1 i $ \st1 -> go (nextShift s) st1 t2
        in Full ary'
    go s t1 (Full ary2) =
        let h1   = leafHashCode t1
            i    = index h1 s
            ary' = update32With' ary2 i $ \st2 -> go (nextShift s) t1 st2
        in Full ary'

    leafHashCode (Leaf h _) = h
    leafHashCode (Collision h _) = h
    leafHashCode _ = error "leafHashCode"

    goDifferentHash s h1 h2 t1 t2
        | m1 == m2  = BitmapIndexed m1 (A.singleton $! goDifferentHash (nextShift s) h1 h2 t1 t2)
        | m1 <  m2  = BitmapIndexed (m1 .|. m2) (A.pair t1 t2)
        | otherwise = BitmapIndexed (m1 .|. m2) (A.pair t2 t1)
      where
        m1 = mask h1 s
        m2 = mask h2 s

unionArrayBy :: (a -> a -> a) -> Bitmap -> Bitmap -> A.Array a -> A.Array a
             -> A.Array a
unionArrayBy f !b1 !b2 !ary1 !ary2 = A.run $ do
    let bCombined = b1 .|. b2
    mary <- A.new_ (popCount bCombined)
    let go !i !i1 !i2 !b
            | b == 0 = return ()
            | testBit (b1 .&. b2) = do
                x1 <- A.indexM ary1 i1
                x2 <- A.indexM ary2 i2
                A.write mary i $! f x1 x2
                go (i+1) (i1+1) (i2+1) b'
            | testBit b1 = do
                A.write mary i =<< A.indexM ary1 i1
                go (i+1) (i1+1) i2 b'
            | otherwise = do
                A.write mary i =<< A.indexM ary2 i2
                go (i+1) i1 (i2+1) b'
          where
            m = 1 `unsafeShiftL` countTrailingZeros b
            testBit x = x .&. m /= 0
            b' = b .&. complement m
    go 0 0 0 bCombined
    return mary


unions :: Eq k => [HashMap k v] -> HashMap k v
unions = List.foldl' union empty



compose :: (Eq b, Hashable b) => HashMap b c -> HashMap a b -> HashMap a c
compose bc !ab
  | null bc = empty
  | otherwise = mapMaybe (bc !?) ab


mapWithKey :: (k -> v1 -> v2) -> HashMap k v1 -> HashMap k v2
mapWithKey f = go
  where
    go Empty = Empty
    go (Leaf h (L k v)) = Leaf h $ L k (f k v)
    go (BitmapIndexed b ary) = BitmapIndexed b $ A.map go ary
    go (Full ary) = Full $ A.map go ary
    go (Collision h ary) = Collision h $
                           A.map' (\ (L k v) -> L k (f k v)) ary

map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = mapWithKey (const f)

traverseWithKey
  :: Applicative f
  => (k -> v1 -> f v2)
  -> HashMap k v1 -> f (HashMap k v2)
traverseWithKey f = go
  where
    go Empty                 = pure Empty
    go (Leaf h (L k v))      = Leaf h . L k <$> f k v
    go (BitmapIndexed b ary) = BitmapIndexed b <$> A.traverse go ary
    go (Full ary)            = Full <$> A.traverse go ary
    go (Collision h ary)     =
        Collision h <$> A.traverse' (\ (L k v) -> L k <$> f k v) ary

mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
mapKeys f = fromList . foldrWithKey (\k x xs -> (f k, x) : xs) []


difference :: (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
difference a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> unsafeInsert k v m
                 _       -> m

differenceWith :: (Eq k, Hashable k) => (v -> w -> Maybe v) -> HashMap k v -> HashMap k w -> HashMap k v
differenceWith f a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> unsafeInsert k v m
                 Just w  -> maybe m (\y -> unsafeInsert k y m) (f v w)

intersection :: Eq k => HashMap k v -> HashMap k w -> HashMap k v
intersection = Exts.inline intersectionWith const

intersectionWith :: Eq k => (v1 -> v2 -> v3) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWith f = Exts.inline intersectionWithKey $ const f

intersectionWithKey :: Eq k => (k -> v1 -> v2 -> v3) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWithKey f = intersectionWithKey# $ \k v1 v2 -> (# f k v1 v2 #)

intersectionWithKey# :: Eq k => (k -> v1 -> v2 -> (# v3 #)) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWithKey# f = go 0
  where
    go !_ _ Empty = Empty
    go _ Empty _ = Empty
    go s (Leaf h1 (L k1 v1)) t2 =
      lookupCont
        (\_ -> Empty)
        (\v _ -> case f k1 v1 v of (# v' #) -> Leaf h1 $ L k1 v')
        h1 k1 s t2
    go s t1 (Leaf h2 (L k2 v2)) =
      lookupCont
        (\_ -> Empty)
        (\v _ -> case f k2 v v2 of (# v' #) -> Leaf h2 $ L k2 v')
        h2 k2 s t1
    go _ (Collision h1 ls1) (Collision h2 ls2) = intersectionCollisions f h1 h2 ls1 ls2
    go s (BitmapIndexed b1 ary1) (BitmapIndexed b2 ary2) =
      intersectionArrayBy (go (nextShift s)) b1 b2 ary1 ary2
    go s (BitmapIndexed b1 ary1) (Full ary2) =
      intersectionArrayBy (go (nextShift s)) b1 fullBitmap ary1 ary2
    go s (Full ary1) (BitmapIndexed b2 ary2) =
      intersectionArrayBy (go (nextShift s)) fullBitmap b2 ary1 ary2
    go s (Full ary1) (Full ary2) =
      intersectionArrayBy (go (nextShift s)) fullBitmap fullBitmap ary1 ary2
    go s (BitmapIndexed b1 ary1) t2@(Collision h2 _ls2)
      | b1 .&. m2 == 0 = Empty
      | otherwise = go (nextShift s) (A.index ary1 i) t2
      where
        m2 = mask h2 s
        i = sparseIndex b1 m2
    go s t1@(Collision h1 _ls1) (BitmapIndexed b2 ary2)
      | b2 .&. m1 == 0 = Empty
      | otherwise = go (nextShift s) t1 (A.index ary2 i)
      where
        m1 = mask h1 s
        i = sparseIndex b2 m1
    go s (Full ary1) t2@(Collision h2 _ls2) = go (nextShift s) (A.index ary1 i) t2
      where
        i = index h2 s
    go s t1@(Collision h1 _ls1) (Full ary2) = go (nextShift s) t1 (A.index ary2 i)
      where
        i = index h1 s

intersectionArrayBy ::
  ( HashMap k v1 ->
    HashMap k v2 ->
    HashMap k v3
  ) ->
  Bitmap ->
  Bitmap ->
  A.Array (HashMap k v1) ->
  A.Array (HashMap k v2) ->
  HashMap k v3
intersectionArrayBy f !b1 !b2 !ary1 !ary2
  | b1 .&. b2 == 0 = Empty
  | otherwise = runST $ do
    mary <- A.new_ $ popCount bIntersect
    let go !i !i1 !i2 !b !bFinal
          | b == 0 = pure (i, bFinal)
          | testBit $ b1 .&. b2 = do
            x1 <- A.indexM ary1 i1
            x2 <- A.indexM ary2 i2
            case f x1 x2 of
              Empty -> go i (i1 + 1) (i2 + 1) b' (bFinal .&. complement m)
              _ -> do
                A.write mary i $! f x1 x2
                go (i + 1) (i1 + 1) (i2 + 1) b' bFinal
          | testBit b1 = go i (i1 + 1) i2 b' bFinal
          | otherwise = go i i1 (i2 + 1) b' bFinal
          where
            m = 1 `unsafeShiftL` countTrailingZeros b
            testBit x = x .&. m /= 0
            b' = b .&. complement m
    (len, bFinal) <- go 0 0 0 bCombined bIntersect
    case len of
      0 -> pure Empty
      1 -> do
        l <- A.read mary 0
        if isLeafOrCollision l
          then pure l
          else BitmapIndexed bFinal <$> (A.unsafeFreeze =<< A.shrink mary 1)
      _ -> bitmapIndexedOrFull bFinal <$> (A.unsafeFreeze =<< A.shrink mary len)
  where
    bCombined = b1 .|. b2
    bIntersect = b1 .&. b2

intersectionCollisions :: Eq k => (k -> v1 -> v2 -> (# v3 #)) -> Hash -> Hash -> A.Array (Leaf k v1) -> A.Array (Leaf k v2) -> HashMap k v3
intersectionCollisions f h1 h2 ary1 ary2
  | h1 == h2 = runST $ do
    mary2 <- A.thaw ary2 0 $ A.length ary2
    mary <- A.new_ $ min (A.length ary1) (A.length ary2)
    let go i j
          | i >= A.length ary1 || j >= A.lengthM mary2 = pure j
          | otherwise = do
            L k1 v1 <- A.indexM ary1 i
            searchSwap k1 j mary2 >>= \case
              Just (L _k2 v2) -> do
                let !(# v3 #) = f k1 v1 v2
                A.write mary j $ L k1 v3
                go (i + 1) (j + 1)
              Nothing -> do
                go (i + 1) j
    len <- go 0 0
    case len of
      0 -> pure Empty
      1 -> Leaf h1 <$> A.read mary 0
      _ -> Collision h1 <$> (A.unsafeFreeze =<< A.shrink mary len)
  | otherwise = Empty

searchSwap :: Eq k => k -> Int -> A.MArray s (Leaf k v) -> ST s (Maybe (Leaf k v))
searchSwap toFind start = go start toFind start
  where
    go i0 k i mary
      | i >= A.lengthM mary = pure Nothing
      | otherwise = do
        l@(L k' _v) <- A.read mary i
        if k == k'
          then do
            A.write mary i =<< A.read mary i0
            pure $ Just l
          else go i0 k (i + 1) mary


foldl' :: (a -> v -> a) -> a -> HashMap k v -> a
foldl' f = foldlWithKey' (\ z _ v -> f z v)

foldr' :: (v -> a -> a) -> a -> HashMap k v -> a
foldr' f = foldrWithKey' (\ _ v z -> f v z)

foldlWithKey' :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey' f = go
  where
    go !z Empty                = z
    go z (Leaf _ (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = A.foldl' go z ary
    go z (Full ary)            = A.foldl' go z ary
    go z (Collision _ ary)     = A.foldl' (\ z' (L k v) -> f z' k v) z ary

foldrWithKey' :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey' f = flip go
  where
    go Empty z                 = z
    go (Leaf _ (L k v)) !z     = f k v z
    go (BitmapIndexed _ ary) !z = A.foldr' go z ary
    go (Full ary) !z           = A.foldr' go z ary
    go (Collision _ ary) !z    = A.foldr' (\ (L k v) z' -> f k v z') z ary

foldr :: (v -> a -> a) -> a -> HashMap k v -> a
foldr f = foldrWithKey (const f)

foldl :: (a -> v -> a) -> a -> HashMap k v -> a
foldl f = foldlWithKey (\a _k v -> f a v)

foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey f = flip go
  where
    go Empty z                 = z
    go (Leaf _ (L k v)) z      = f k v z
    go (BitmapIndexed _ ary) z = A.foldr go z ary
    go (Full ary) z            = A.foldr go z ary
    go (Collision _ ary) z     = A.foldr (\ (L k v) z' -> f k v z') z ary

foldlWithKey :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey f = go
  where
    go z Empty                 = z
    go z (Leaf _ (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = A.foldl go z ary
    go z (Full ary)            = A.foldl go z ary
    go z (Collision _ ary)     = A.foldl (\ z' (L k v) -> f z' k v) z ary

foldMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldMapWithKey f = go
  where
    go Empty = mempty
    go (Leaf _ (L k v)) = f k v
    go (BitmapIndexed _ ary) = A.foldMap go ary
    go (Full ary) = A.foldMap go ary
    go (Collision _ ary) = A.foldMap (\ (L k v) -> f k v) ary


mapMaybeWithKey :: (k -> v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybeWithKey f = filterMapAux onLeaf onColl
  where onLeaf (Leaf h (L k v)) | Just v' <- f k v = Just (Leaf h (L k v'))
        onLeaf _ = Nothing

        onColl (L k v) | Just v' <- f k v = Just (L k v')
                       | otherwise = Nothing

mapMaybe :: (v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybe f = mapMaybeWithKey (const f)

filterWithKey :: forall k v. (k -> v -> Bool) -> HashMap k v -> HashMap k v
filterWithKey pred = filterMapAux onLeaf onColl
  where onLeaf t@(Leaf _ (L k v)) | pred k v = Just t
        onLeaf _ = Nothing

        onColl el@(L k v) | pred k v = Just el
        onColl _ = Nothing


filterMapAux :: forall k v1 v2
              . (HashMap k v1 -> Maybe (HashMap k v2))
             -> (Leaf k v1 -> Maybe (Leaf k v2))
             -> HashMap k v1
             -> HashMap k v2
filterMapAux onLeaf onColl = go
  where
    go Empty = Empty
    go t@Leaf{}
        | Just t' <- onLeaf t = t'
        | otherwise = Empty
    go (BitmapIndexed b ary) = filterA ary b
    go (Full ary) = filterA ary fullBitmap
    go (Collision h ary) = filterC ary h

    filterA ary0 b0 =
        let !n = A.length ary0
        in runST $ do
            mary <- A.new_ n
            step ary0 mary b0 0 0 1 n
      where
        step :: A.Array (HashMap k v1) -> A.MArray s (HashMap k v2)
             -> Bitmap -> Int -> Int -> Bitmap -> Int
             -> ST s (HashMap k v2)
        step !ary !mary !b i !j !bi n
            | i >= n = case j of
                0 -> return Empty
                1 -> do
                    ch <- A.read mary 0
                    case ch of
                      t | isLeafOrCollision t -> return t
                      _                       -> BitmapIndexed b <$> (A.unsafeFreeze =<< A.shrink mary 1)
                _ -> do
                    ary2 <- A.unsafeFreeze =<< A.shrink mary j
                    return $! if j == maxChildren
                              then Full ary2
                              else BitmapIndexed b ary2
            | bi .&. b == 0 = step ary mary b i j (bi `unsafeShiftL` 1) n
            | otherwise = case go (A.index ary i) of
                Empty -> step ary mary (b .&. complement bi) (i+1) j
                         (bi `unsafeShiftL` 1) n
                t     -> do A.write mary j t
                            step ary mary b (i+1) (j+1) (bi `unsafeShiftL` 1) n

    filterC ary0 h =
        let !n = A.length ary0
        in runST $ do
            mary <- A.new_ n
            step ary0 mary 0 0 n
      where
        step :: A.Array (Leaf k v1) -> A.MArray s (Leaf k v2)
             -> Int -> Int -> Int
             -> ST s (HashMap k v2)
        step !ary !mary i !j n
            | i >= n    = case j of
                0 -> return Empty
                1 -> do l <- A.read mary 0
                        return $! Leaf h l
                _ | i == j -> do ary2 <- A.unsafeFreeze mary
                                 return $! Collision h ary2
                  | otherwise -> do ary2 <- A.unsafeFreeze =<< A.shrink mary j
                                    return $! Collision h ary2
            | Just el <- onColl $! A.index ary i
                = A.write mary j el >> step ary mary (i+1) (j+1) n
            | otherwise = step ary mary (i+1) j n

filter :: (v -> Bool) -> HashMap k v -> HashMap k v
filter p = filterWithKey (\_ v -> p v)



keys :: HashMap k v -> [k]
keys = List.map fst . toList

elems :: HashMap k v -> [v]
elems = List.map snd . toList


toList :: HashMap k v -> [(k, v)]
toList t = Exts.build (\ c z -> foldrWithKey (curry c) z t)

fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = List.foldl' (\ m (k, v) -> unsafeInsert k v m) empty

fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWith f = List.foldl' (\ m (k, v) -> unsafeInsertWith f k v m) empty

fromListWithKey :: (Eq k, Hashable k) => (k -> v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWithKey f = List.foldl' (\ m (k, v) -> unsafeInsertWithKey (\k' a b -> (# f k' a b #)) k v m) empty


lookupInArrayCont ::
  forall rep (r :: TYPE rep) k v.
  Eq k => ((# #) -> r) -> (v -> Int -> r) -> k -> A.Array (Leaf k v) -> r
lookupInArrayCont absent present k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go :: Eq k => k -> A.Array (Leaf k v) -> Int -> Int -> r
    go !k !ary !i !n
        | i >= n    = absent (# #)
        | otherwise = case A.index ary i of
            (L kx v)
                | k == kx   -> present v i
                | otherwise -> go k ary (i+1) n

indexOf :: Eq k => k -> A.Array (Leaf k v) -> Maybe Int
indexOf k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n    = Nothing
        | otherwise = case A.index ary i of
            (L kx _)
                | k == kx   -> Just i
                | otherwise -> go k ary (i+1) n

updateWith# :: Eq k => (v -> (# v #)) -> k -> A.Array (Leaf k v) -> A.Array (Leaf k v)
updateWith# f k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n    = ary
        | otherwise = case A.index ary i of
            (L kx y) | k == kx -> case f y of
                          (# y' #)
                             | ptrEq y y' -> ary
                             | otherwise -> A.update ary i (L k y')
                     | otherwise -> go k ary (i+1) n

updateOrSnocWith :: Eq k => (v -> v -> (# v #)) -> k -> v -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
updateOrSnocWith f = updateOrSnocWithKey (const f)

updateOrSnocWithKey :: Eq k => (k -> v -> v -> (# v #)) -> k -> v -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
updateOrSnocWithKey f k0 v0 ary0 = go k0 v0 ary0 0 (A.length ary0)
  where
    go !k v !ary !i !n
        | i >= n = A.snoc ary $ L k v
        | L kx y <- A.index ary i
        , k == kx
        , (# v2 #) <- f k v y
            = A.update ary i (L k v2)
        | otherwise
            = go k v ary (i+1) n

updateOrConcatWithKey :: Eq k => (k -> v -> v -> (# v #)) -> A.Array (Leaf k v) -> A.Array (Leaf k v) -> A.Array (Leaf k v)
updateOrConcatWithKey f ary1 ary2 = A.run $ do

    let indices = A.map' (\(L k _) -> indexOf k ary1) ary2
    let nOnly2 = A.foldl' (\n -> maybe (n+1) (const n)) 0 indices
    let n1 = A.length ary1
    let n2 = A.length ary2
    mary <- A.new_ (n1 + nOnly2)
    A.copy ary1 0 mary 0 n1
    let go !iEnd !i2
          | i2 >= n2 = return ()
          | otherwise = case A.index indices i2 of
               Just i1 -> do -- key occurs in both arrays, store combination in position i1
                             L k v1 <- A.indexM ary1 i1
                             L _ v2 <- A.indexM ary2 i2
                             case f k v1 v2 of (# v3 #) -> A.write mary i1 (L k v3)
                             go iEnd (i2+1)
               Nothing -> do -- key is only in ary2, append to end
                             A.write mary iEnd =<< A.indexM ary2 i2
                             go (iEnd+1) (i2+1)
    go n1 0
    return mary

subsetArray :: Eq k => (v1 -> v2 -> Bool) -> A.Array (Leaf k v1) -> A.Array (Leaf k v2) -> Bool
subsetArray cmpV ary1 ary2 = A.length ary1 <= A.length ary2 && A.all inAry2 ary1
  where
    inAry2 (L k1 v1) = lookupInArrayCont (\_ -> False) (\v2 _ -> cmpV v1 v2) k1 ary2


update32 :: A.Array e -> Int -> e -> A.Array e
update32 ary idx b = runST (update32M ary idx b)

update32M :: A.Array e -> Int -> e -> ST s (A.Array e)
update32M ary idx b = do
    mary <- clone ary
    A.write mary idx b
    A.unsafeFreeze mary

update32With' :: A.Array e -> Int -> (e -> e) -> A.Array e
update32With' ary idx f
  | (# x #) <- A.index# ary idx
  = update32 ary idx $! f x

clone :: A.Array e -> ST s (A.MArray s e)
clone ary =
    A.thaw ary 0 (2^bitsPerSubkey)



bitsPerSubkey :: Int
bitsPerSubkey = 5

maxChildren :: Int
maxChildren = 1 `unsafeShiftL` bitsPerSubkey

subkeyMask :: Word
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

index :: Hash -> Shift -> Int
index w s = fromIntegral $ unsafeShiftR w s .&. subkeyMask

mask :: Hash -> Shift -> Bitmap
mask w s = 1 `unsafeShiftL` index w s

sparseIndex
    :: Bitmap
    -> Bitmap
    -> Int
sparseIndex b m = popCount (b .&. (m - 1))

fullBitmap :: Bitmap
fullBitmap = complement (complement 0 `shiftL` maxChildren)

nextShift :: Shift -> Shift
nextShift s = s + bitsPerSubkey


ptrEq :: a -> a -> Bool
ptrEq x y = Exts.isTrue# (Exts.reallyUnsafePtrEquality# x y ==# 1#)

instance (Eq k, Hashable k) => Exts.IsList (HashMap k v) where
    type Item (HashMap k v) = (k, v)
    fromList = fromList
    toList   = toList

#if __GLASGOW_HASKELL__ >= 702
#endif
#if __GLASGOW_HASKELL__ >= 710 && __GLASGOW_HASKELL__ < 802
#endif

module Control.Monad.Trans.Reader (
    Reader,
    reader,
    runReader,
    mapReader,
    withReader,
    ReaderT(..),
    mapReaderT,
    withReaderT,
    ask,
    local,
    asks,
    liftCallCC,
    liftCatch,
    ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant
#endif
import Data.Functor.Identity

import Control.Applicative
import Control.Monad
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix
#if !(MIN_VERSION_base(4,6,0))
import Control.Monad.Instances ()  -- deprecated from base-4.6
#endif
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
#if (MIN_VERSION_base(4,2,0)) && !(MIN_VERSION_base(4,8,0))
import Data.Functor ((<$))
#endif
#if __GLASGOW_HASKELL__ >= 704
import GHC.Generics
#endif

type Reader r = ReaderT r Identity

reader :: (Monad m) => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)

runReader
    :: Reader r a       -- ^ A @Reader@ to run.
    -> r                -- ^ An initial environment.
    -> a
runReader m = runIdentity . runReaderT m

mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity)

withReader
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> Reader r a       -- ^ Computation to run in the modified environment.
    -> Reader r' a
withReader = withReaderT

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
#if __GLASGOW_HASKELL__ >= 710
    deriving (Generic, Generic1)
#elif __GLASGOW_HASKELL__ >= 704
    deriving (Generic)
#endif

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

withReaderT
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

instance (Functor m) => Functor (ReaderT r m) where
    fmap f  = mapReaderT (fmap f)
#if MIN_VERSION_base(4,2,0)
    x <$ v = mapReaderT (x <$) v
#endif

instance (Applicative m) => Applicative (ReaderT r m) where
    pure    = liftReaderT . pure
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r
#if MIN_VERSION_base(4,2,0)
    u *> v = ReaderT $ \ r -> runReaderT u r *> runReaderT v r
    u <* v = ReaderT $ \ r -> runReaderT u r <* runReaderT v r
#endif
#if MIN_VERSION_base(4,10,0)
    liftA2 f x y = ReaderT $ \ r -> liftA2 f (runReaderT x r) (runReaderT y r)
#endif

instance (Alternative m) => Alternative (ReaderT r m) where
    empty   = liftReaderT empty
    m <|> n = ReaderT $ \ r -> runReaderT m r <|> runReaderT n r

instance (Monad m) => Monad (ReaderT r m) where
#if !(MIN_VERSION_base(4,8,0))
    return   = lift . return
#endif
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
#if MIN_VERSION_base(4,8,0)
    (>>) = (*>)
#else
    m >> k = ReaderT $ \ r -> runReaderT m r >> runReaderT k r
#endif
#if !(MIN_VERSION_base(4,13,0))
    fail msg = lift (fail msg)
#endif

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (ReaderT r m) where
    fail msg = lift (Fail.fail msg)
#endif

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    mzero       = lift mzero
    m `mplus` n = ReaderT $ \ r -> runReaderT m r `mplus` runReaderT n r

instance (MonadFix m) => MonadFix (ReaderT r m) where
    mfix f = ReaderT $ \ r -> mfix $ \ a -> runReaderT (f a) r

instance MonadTrans (ReaderT r) where
    lift   = liftReaderT

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (ReaderT r m) where
    mzipWith f (ReaderT m) (ReaderT n) = ReaderT $ \ a ->
        mzipWith f (m a) (n a)
#endif

#if MIN_VERSION_base(4,12,0)
instance Contravariant m => Contravariant (ReaderT r m) where
    contramap f = ReaderT . fmap (contramap f) . runReaderT
#endif

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

ask :: (Monad m) => ReaderT r m r
ask = ReaderT return

local
    :: (r -> r)         -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r m a
local = withReaderT

asks :: (Monad m)
    => (r -> a)         -- ^ The selector function to apply to the environment.
    -> ReaderT r m a
asks f = ReaderT (return . f)

liftCallCC :: CallCC m a b -> CallCC (ReaderT r m) a b
liftCallCC callCC f = ReaderT $ \ r ->
    callCC $ \ c ->
    runReaderT (f (ReaderT . const . c)) r

liftCatch :: Catch e m a -> Catch e (ReaderT r m) a
liftCatch f m h =
    ReaderT $ \ r -> f (runReaderT m r) (\ e -> runReaderT (h e) r)

#if __GLASGOW_HASKELL__ >= 702
#endif
#if __GLASGOW_HASKELL__ >= 710 && __GLASGOW_HASKELL__ < 802
#endif

module Control.Monad.Trans.Maybe (
    MaybeT(..),
    mapMaybeT,
    hoistMaybe,
    maybeToExceptT,
    exceptToMaybeT,
    liftCallCC,
    liftCatch,
    liftListen,
    liftPass,
  ) where

import Control.Monad.IO.Class
import Control.Monad.Signatures
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except (ExceptT(..))
import Data.Functor.Classes
#if MIN_VERSION_base(4,12,0)
import Data.Functor.Contravariant
#endif

import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus), liftM)
#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif
import Control.Monad.Fix (MonadFix(mfix))
#if MIN_VERSION_base(4,4,0)
import Control.Monad.Zip (MonadZip(mzipWith))
#endif
import Data.Maybe (fromMaybe)
#if !(MIN_VERSION_base(4,8,0)) || defined(__MHS__)
import Data.Foldable (Foldable(foldMap))
import Data.Traversable (Traversable(traverse))
#endif
#if __GLASGOW_HASKELL__ >= 704
import GHC.Generics
#endif

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
#if __GLASGOW_HASKELL__ >= 710
    deriving (Generic, Generic1)
#elif __GLASGOW_HASKELL__ >= 704
    deriving (Generic)
#endif

instance (Eq1 m) => Eq1 (MaybeT m) where
    liftEq eq (MaybeT x) (MaybeT y) = liftEq (liftEq eq) x y

instance (Ord1 m) => Ord1 (MaybeT m) where
    liftCompare comp (MaybeT x) (MaybeT y) = liftCompare (liftCompare comp) x y

instance (Read1 m) => Read1 (MaybeT m) where
    liftReadsPrec rp rl = readsData $
        readsUnaryWith (liftReadsPrec rp' rl') "MaybeT" MaybeT
      where
        rp' = liftReadsPrec rp rl
        rl' = liftReadList rp rl

instance (Show1 m) => Show1 (MaybeT m) where
    liftShowsPrec sp sl d (MaybeT m) =
        showsUnaryWith (liftShowsPrec sp' sl') "MaybeT" d m
      where
        sp' = liftShowsPrec sp sl
        sl' = liftShowList sp sl

instance (Eq1 m, Eq a) => Eq (MaybeT m a) where (==) = eq1
instance (Ord1 m, Ord a) => Ord (MaybeT m a) where compare = compare1
instance (Read1 m, Read a) => Read (MaybeT m a) where readsPrec = readsPrec1
instance (Show1 m, Show a) => Show (MaybeT m a) where showsPrec = showsPrec1

mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

maybeToExceptT :: (Functor m) => e -> MaybeT m a -> ExceptT e m a
maybeToExceptT e (MaybeT m) = ExceptT $ fmap (maybe (Left e) Right) m

exceptToMaybeT :: (Functor m) => ExceptT e m a -> MaybeT m a
exceptToMaybeT (ExceptT m) = MaybeT $ fmap (either (const Nothing) Just) m

instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))

instance (Foldable f) => Foldable (MaybeT f) where
    foldMap f (MaybeT a) = foldMap (foldMap f) a

instance (Traversable f) => Traversable (MaybeT f) where
    traverse f (MaybeT a) = MaybeT <$> traverse (traverse f) a

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = MaybeT . return . Just
    mf <*> mx = MaybeT $ do
        mb_f <- runMaybeT mf
        case mb_f of
            Nothing -> return Nothing
            Just f  -> do
                mb_x <- runMaybeT mx
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))
    m *> k = m >>= \_ -> k

instance (Functor m, Monad m) => Alternative (MaybeT m) where
    empty = MaybeT (return Nothing)
    x <|> y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v

instance (Monad m) => Monad (MaybeT m) where
#if !(MIN_VERSION_base(4,8,0))
    return = MaybeT . return . Just
#endif
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)
#if !(MIN_VERSION_base(4,13,0))
    fail _ = MaybeT (return Nothing)
#endif

#if MIN_VERSION_base(4,9,0)
instance (Monad m) => Fail.MonadFail (MaybeT m) where
    fail _ = MaybeT (return Nothing)
#endif

instance (Monad m) => MonadPlus (MaybeT m) where
    mzero = MaybeT (return Nothing)
    mplus x y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v

instance (MonadFix m) => MonadFix (MaybeT m) where
    mfix f = MaybeT (mfix (runMaybeT . f . fromMaybe bomb))
      where bomb = error "mfix (MaybeT): inner computation returned Nothing"

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (MaybeT m) where
    mzipWith f (MaybeT a) (MaybeT b) = MaybeT $ mzipWith (liftA2 f) a b
#endif

#if MIN_VERSION_base(4,12,0)
instance Contravariant m => Contravariant (MaybeT m) where
    contramap f = MaybeT . contramap (fmap f) . runMaybeT
#endif

liftCallCC :: CallCC m (Maybe a) (Maybe b) -> CallCC (MaybeT m) a b
liftCallCC callCC f =
    MaybeT $ callCC $ \ c -> runMaybeT (f (MaybeT . c . Just))

liftCatch :: Catch e m (Maybe a) -> Catch e (MaybeT m) a
liftCatch f m h = MaybeT $ f (runMaybeT m) (runMaybeT . h)

liftListen :: (Monad m) => Listen w m (Maybe a) -> Listen w (MaybeT m) a
liftListen listen = mapMaybeT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a

liftPass :: (Monad m) => Pass w m (Maybe a) -> Pass w (MaybeT m) a
liftPass pass = mapMaybeT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Nothing     -> (Nothing, id)
        Just (v, f) -> (Just v, f)



module Data.Time.Format.Parse (
    parseTimeM,
    parseTimeMultipleM,
    parseTimeOrError,
    readSTime,
    readPTime,
    ParseTime (),

    module Data.Time.Format.Locale,
) where

import Control.Monad.Fail
import Data.Char
import Data.Proxy
import Data.Time.Calendar.Days
import Data.Time.Clock.Internal.UTCTime
import Data.Time.Clock.Internal.UniversalTime
import Data.Time.Format.Locale
import Data.Time.Format.Parse.Class
import Data.Time.Format.Parse.Instances ()
import Data.Time.LocalTime.Internal.LocalTime
import Data.Time.LocalTime.Internal.TimeOfDay
import Data.Time.LocalTime.Internal.TimeZone
import Data.Time.LocalTime.Internal.ZonedTime
import Data.Traversable
import Text.ParserCombinators.ReadP hiding (char, string)
import Prelude hiding (fail)

parseTimeM ::
    (MonadFail m, ParseTime t) =>
    Bool ->
    TimeLocale ->
    String ->
    String ->
    m t
parseTimeM acceptWS l fmt s = parseTimeMultipleM acceptWS l [(fmt, s)]

parseTimeMultipleM' ::
    (MonadFail m, ParseTime t) =>
    Proxy t ->
    Bool ->
    TimeLocale ->
    [(String, String)] ->
    m t
parseTimeMultipleM' pt acceptWS l fmts = do
    specss <- for fmts $ \(fmt, s) -> parseTimeSpecifiersM pt acceptWS l fmt s
    case buildTime l $ mconcat specss of
        Just t -> return t
        Nothing -> fail "parseTimeM: cannot construct"

parseTimeMultipleM ::
    (MonadFail m, ParseTime t) =>
    Bool ->
    TimeLocale ->
    [(String, String)] ->
    m t
parseTimeMultipleM = parseTimeMultipleM' Proxy

parseTimeOrError ::
    ParseTime t =>
    Bool ->
    TimeLocale ->
    String ->
    String ->
    t
parseTimeOrError acceptWS l fmt s =
    case parseTimeM acceptWS l fmt s of
        [t] -> t
        [] -> error $ "parseTimeOrError: no parse of " ++ show s
        _ -> error $ "parseTimeOrError: multiple parses of " ++ show s

parseTimeSpecifiersM ::
    (MonadFail m, ParseTime t) =>
    Proxy t ->
    Bool ->
    TimeLocale ->
    String ->
    String ->
    m [(Char, String)]
parseTimeSpecifiersM pt acceptWS l fmt s =
    case parseTimeSpecifiers pt acceptWS l fmt s of
        [t] -> return t
        [] -> fail $ "parseTimeM: no parse of " ++ show s
        _ -> fail $ "parseTimeM: multiple parses of " ++ show s

parseTimeSpecifiers ::
    ParseTime t =>
    Proxy t ->
    Bool ->
    TimeLocale ->
    String ->
    String ->
    [[(Char, String)]]
parseTimeSpecifiers pt False l fmt s = [t | (t, "") <- readP_to_S (readPSpecifiers pt False l fmt) s]
parseTimeSpecifiers pt True l fmt s = [t | (t, r) <- readP_to_S (readPSpecifiers pt True l fmt) s, all isSpace r]

readSTime ::
    ParseTime t =>
    Bool ->
    TimeLocale ->
    String ->
    ReadS t
readSTime acceptWS l f = readP_to_S $ readPTime acceptWS l f

readPSpecifiers ::
    ParseTime t =>
    Proxy t ->
    Bool ->
    TimeLocale ->
    String ->
    ReadP [(Char, String)]
readPSpecifiers pt False l f = parseSpecifiers pt l f
readPSpecifiers pt True l f = (skipSpaces >> parseSpecifiers pt l f) <++ parseSpecifiers pt l f

readPTime' ::
    ParseTime t =>
    Proxy t ->
    Bool ->
    TimeLocale ->
    String ->
    ReadP t
readPTime' pt ws l f = do
    pairs <- readPSpecifiers pt ws l f
    case buildTime l pairs of
        Just t -> return t
        Nothing -> pfail

readPTime ::
    ParseTime t =>
    Bool ->
    TimeLocale ->
    String ->
    ReadP t
readPTime = readPTime' Proxy


instance Read Day where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d"

instance Read TimeOfDay where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%H:%M:%S%Q"

instance Read LocalTime where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

instance Read TimeZone where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Z"

instance Read ZonedTime where
    readsPrec n = readParen False $ \s -> [(ZonedTime t z, r2) | (t, r1) <- readsPrec n s, (z, r2) <- readsPrec n r1]

(<||) :: [a] -> [a] -> [a]
[] <|| b = b
a <|| _ = a

instance Read UTCTime where
    readsPrec n s = do
        (lt, s') <- readsPrec n s
        (tz, s'') <- readsPrec n s' <|| pure (utc, s')
        return (localTimeToUTC tz lt, s'')

instance Read UniversalTime where
    readsPrec n s = [(localTimeToUT1 0 t, r) | (t, r) <- readsPrec n s]


module Data.Time.LocalTime.Internal.TimeOfDay (
    TimeOfDay (..),
    midnight,
    midday,
    makeTimeOfDayValid,
    timeToDaysAndTimeOfDay,
    daysAndTimeOfDayToTime,
    utcToLocalTimeOfDay,
    localToUTCTimeOfDay,
    timeToTimeOfDay,
    pastMidnight,
    timeOfDayToTime,
    sinceMidnight,
    dayFractionToTimeOfDay,
    timeOfDayToDayFraction,
) where

import Control.DeepSeq
import Data.Data
import Data.Fixed
import Data.Time.Calendar.Private
import Data.Time.Clock.Internal.DiffTime
import Data.Time.Clock.Internal.NominalDiffTime
import Data.Time.LocalTime.Internal.TimeZone
import GHC.Generics

data TimeOfDay = TimeOfDay
    { todHour :: Int
    , todMin :: Int
    , todSec :: Pico
    }
    deriving (Eq, Ord, Data, Typeable, Generic)

instance NFData TimeOfDay where
    rnf (TimeOfDay h m s) = rnf h `seq` rnf m `seq` rnf s `seq` ()

midnight :: TimeOfDay
midnight = TimeOfDay 0 0 0

midday :: TimeOfDay
midday = TimeOfDay 12 0 0

instance Show TimeOfDay where
    show (TimeOfDay h m s) = (show2 h) ++ ":" ++ (show2 m) ++ ":" ++ (show2Fixed s)

makeTimeOfDayValid :: Int -> Int -> Pico -> Maybe TimeOfDay
makeTimeOfDayValid h m s = do
    _ <- clipValid 0 23 h
    _ <- clipValid 0 59 m
    _ <- clipValid 0 60.999999999999 s
    return (TimeOfDay h m s)

timeToDaysAndTimeOfDay :: NominalDiffTime -> (Integer, TimeOfDay)
timeToDaysAndTimeOfDay dt =
    let
        s = realToFrac dt
        (m, ms) = divMod' s 60
        (h, hm) = divMod' m 60
        (d, dh) = divMod' h 24
    in
        (d, TimeOfDay dh hm ms)

daysAndTimeOfDayToTime :: Integer -> TimeOfDay -> NominalDiffTime
daysAndTimeOfDayToTime d (TimeOfDay dh hm ms) =
    (+) (realToFrac ms) $ (*) 60 $ (+) (realToFrac hm) $ (*) 60 $ (+) (realToFrac dh) $ (*) 24 $ realToFrac d

utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Integer, TimeOfDay)
utcToLocalTimeOfDay zone (TimeOfDay h m s) = (fromIntegral (div h' 24), TimeOfDay (mod h' 24) (mod m' 60) s)
  where
    m' = m + timeZoneMinutes zone
    h' = h + (div m' 60)

localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Integer, TimeOfDay)
localToUTCTimeOfDay zone = utcToLocalTimeOfDay (minutesToTimeZone (negate (timeZoneMinutes zone)))

posixDayLength :: DiffTime
posixDayLength = fromInteger 86400

timeToTimeOfDay :: DiffTime -> TimeOfDay
timeToTimeOfDay dt
    | dt >= posixDayLength = TimeOfDay 23 59 (60 + (realToFrac (dt - posixDayLength)))
timeToTimeOfDay dt = TimeOfDay (fromInteger h) (fromInteger m) s
  where
    s' = realToFrac dt
    s = mod' s' 60
    m' = div' s' 60
    m = mod' m' 60
    h = div' m' 60

pastMidnight :: DiffTime -> TimeOfDay
pastMidnight = timeToTimeOfDay

timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime (TimeOfDay h m s) = ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (realToFrac s)

sinceMidnight :: TimeOfDay -> DiffTime
sinceMidnight = timeOfDayToTime

dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay df = timeToTimeOfDay (realToFrac (df * 86400))

timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction tod = realToFrac (timeOfDayToTime tod) / realToFrac posixDayLength

instance Eq Text where
    Text arrA offA lenA == Text arrB offB lenB
        | lenA == lenB = A.equal arrA offA arrB offB lenA
        | otherwise    = False

instance Ord Text where
    compare = compareText

instance Read Text where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

instance Semigroup Text where
    (<>) = append

instance Monoid Text where
    mempty  = empty
    mappend = (<>)
    mconcat = concat

instance IsString Text where
    fromString = pack

instance Exts.IsList Text where
    type Item Text = Char
    fromList       = pack
    toList         = unpack

instance NFData Text where rnf !_ = ()

instance Binary Text where
    put t = do
      put (lengthWord8 t)
      putBuilder (encodeUtf8Builder t)
    get   = do
      bs <- get
      case decodeUtf8' bs of
        P.Left exn -> P.fail (P.show exn)
        P.Right a -> P.return a


instance Data Text where
  gfoldl f z txt = z pack `f` (unpack txt)
  toConstr _ = packConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z pack)
    _ -> P.error "gunfold"
  dataTypeOf _ = textDataType

instance TH.Lift Text where
#if MIN_VERSION_template_haskell(2,16,0)
  lift txt = do
    let (ptr, len) = unsafePerformIO $ asForeignPtr txt
    case len of
        0 -> TH.varE 'empty
        _ ->
          let
            bytesQ = TH.litE . TH.bytesPrimL $ TH.mkBytes ptr 0 (P.fromIntegral len)
            lenQ = liftInt (P.fromIntegral len)
            liftInt n = (TH.appE (TH.conE 'Exts.I#) (TH.litE (TH.IntPrimL n)))
          in TH.varE 'unpackCStringLen# `TH.appE` bytesQ `TH.appE` lenQ
#else
  lift = TH.appE (TH.varE 'pack) . TH.stringE . unpack
#endif
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

#if MIN_VERSION_template_haskell(2,16,0)
unpackCStringLen# :: Exts.Addr# -> Int -> Text
unpackCStringLen# addr# l = Text ba 0 l
  where
    ba = runST $ do
      marr <- A.new l
      A.copyFromPointer marr 0 (Exts.Ptr addr#) l
      A.unsafeFreeze marr
#endif

instance PrintfArg Text where
  formatArg txt = formatString $ unpack txt

packConstr :: Constr
packConstr = mkConstr textDataType "pack" [] Prefix

textDataType :: DataType
textDataType = mkDataType "Data.Text.Text" [packConstr]

compareText :: Text -> Text -> Ordering
compareText (Text arrA offA lenA) (Text arrB offB lenB) =
    A.compare arrA offA arrB offB (min lenA lenB) <> compare lenA lenB


cons :: Char -> Text -> Text
cons c = unstream . S.cons (safe c) . stream

infixr 5 `cons`

snoc :: Text -> Char -> Text
snoc t c = unstream (S.snoc (stream t) (safe c))

head :: HasCallStack => Text -> Char
head t = S.head (stream t)

uncons :: Text -> Maybe (Char, Text)
uncons t@(Text arr off len)
    | len <= 0  = Nothing
    | otherwise = Just $ let !(Iter c d) = iter t 0
                         in (c, text arr (off+d) (len-d))

last :: HasCallStack => Text -> Char
last t@(Text _ _ len)
    | null t = emptyError "last"
    | otherwise = let Iter c _ = reverseIter t (len - 1) in c

tail :: HasCallStack => Text -> Text
tail t@(Text arr off len)
    | null t = emptyError "tail"
    | otherwise = text arr (off+d) (len-d)
    where d = iter_ t 0

init :: HasCallStack => Text -> Text
init t@(Text arr off len)
    | null t = emptyError "init"
    | otherwise = text arr off (len + reverseIter_ t (len - 1))

unsnoc :: Text -> Maybe (Text, Char)
unsnoc t@(Text arr off len)
    | null t = Nothing
    | otherwise = Just (text arr off (len + d), c)
        where
            Iter c d = reverseIter t (len - 1)

null :: Text -> Bool
null (Text _arr _off len) =
#if defined(ASSERTS)
    assert (len >= 0) $
#endif
    len <= 0

 "TEXT null/empty -> True" null empty = True

pattern Empty :: Text
pattern Empty <- (null -> True) where
  Empty = empty

pattern (:<) :: Char -> Text -> Text
pattern x :< xs <- (uncons -> Just (x, xs)) where
  (:<) = cons
infixr 5 :<

pattern (:>) :: Text -> Char -> Text
pattern xs :> x <- (unsnoc -> Just (xs, x)) where
  (:>) = snoc
infixl 5 :>

isSingleton :: Text -> Bool
isSingleton = S.isSingleton . stream

length ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Int
length = P.negate . measureOff P.maxBound

"TEXT length/filter -> S.length/S.filter" forall p t.
    length (filter p t) = S.length (S.filter p (stream t))
"TEXT length/unstream -> S.length" forall t.
    length (unstream t) = S.length t
"TEXT length/pack -> P.length" forall t.
    length (pack t) = P.length t
"TEXT length/map -> length" forall f t.
    length (map f t) = length t
"TEXT length/zipWith -> length" forall f t1 t2.
    length (zipWith f t1 t2) = min (length t1) (length t2)
"TEXT length/replicate -> n" forall n t.
    length (replicate n t) = mul (max 0 n) (length t)
"TEXT length/cons -> length+1" forall c t.
    length (cons c t) = 1 + length t
"TEXT length/intersperse -> 2*length-1" forall c t.
    length (intersperse c t) = max 0 (mul 2 (length t) - 1)
"TEXT length/intercalate -> n*length" forall s ts.
    length (intercalate s ts) = let lenS = length s in max 0 (P.sum (P.map (\t -> length t + lenS) ts) - lenS)
"TEXT length/empty -> 0"
    length empty = 0

compareLength :: Text -> Int -> Ordering
compareLength t c = S.compareLengthI (stream t) c

"TEXT compareN/length -> compareLength" [~1] forall t n.
    compare (length t) n = compareLength t n

"TEXT ==N/length -> compareLength/==EQ" [~1] forall t n.
    eqInt (length t) n = compareLength t n == EQ

"TEXT /=N/length -> compareLength//=EQ" [~1] forall t n.
    neInt (length t) n = compareLength t n /= EQ

"TEXT <N/length -> compareLength/==LT" [~1] forall t n.
    ltInt (length t) n = compareLength t n == LT

"TEXT <=N/length -> compareLength//=GT" [~1] forall t n.
    leInt (length t) n = compareLength t n /= GT

"TEXT >N/length -> compareLength/==GT" [~1] forall t n.
    gtInt (length t) n = compareLength t n == GT

"TEXT >=N/length -> compareLength//=LT" [~1] forall t n.
    geInt (length t) n = compareLength t n /= LT

map :: (Char -> Char) -> Text -> Text
map f = \t -> if null t then empty else mapNonEmpty f t

"TEXT map/map -> map" forall f g t.
    map f (map g t) = map (f . safe . g) t

intercalate :: Text -> [Text] -> Text
intercalate t = concat . L.intersperse t

intersperse :: Char -> Text -> Text
intersperse c t@(Text src o l) = if null t then empty else runST $ do
    let !cLen = utf8Length c
        dstLen = l + length t P.* cLen

    dst <- A.new dstLen

    let writeSep = case cLen of
          1 -> \dstOff ->
            A.unsafeWrite dst dstOff (ord8 c)
          2 -> let (c0, c1) = ord2 c in \dstOff -> do
            A.unsafeWrite dst dstOff c0
            A.unsafeWrite dst (dstOff + 1) c1
          3 -> let (c0, c1, c2) = ord3 c in \dstOff -> do
            A.unsafeWrite dst dstOff c0
            A.unsafeWrite dst (dstOff + 1) c1
            A.unsafeWrite dst (dstOff + 2) c2
          _ -> let (c0, c1, c2, c3) = ord4 c in \dstOff -> do
            A.unsafeWrite dst dstOff c0
            A.unsafeWrite dst (dstOff + 1) c1
            A.unsafeWrite dst (dstOff + 2) c2
            A.unsafeWrite dst (dstOff + 3) c3
    let go !srcOff !dstOff = if srcOff >= o + l then return () else do
          let m0 = A.unsafeIndex src srcOff
              m1 = A.unsafeIndex src (srcOff + 1)
              m2 = A.unsafeIndex src (srcOff + 2)
              m3 = A.unsafeIndex src (srcOff + 3)
              !d = utf8LengthByLeader m0
          case d of
            1 -> do
              A.unsafeWrite dst dstOff m0
              writeSep (dstOff + 1)
              go (srcOff + 1) (dstOff + 1 + cLen)
            2 -> do
              A.unsafeWrite dst dstOff m0
              A.unsafeWrite dst (dstOff + 1) m1
              writeSep (dstOff + 2)
              go (srcOff + 2) (dstOff + 2 + cLen)
            3 -> do
              A.unsafeWrite dst dstOff m0
              A.unsafeWrite dst (dstOff + 1) m1
              A.unsafeWrite dst (dstOff + 2) m2
              writeSep (dstOff + 3)
              go (srcOff + 3) (dstOff + 3 + cLen)
            _ -> do
              A.unsafeWrite dst dstOff m0
              A.unsafeWrite dst (dstOff + 1) m1
              A.unsafeWrite dst (dstOff + 2) m2
              A.unsafeWrite dst (dstOff + 3) m3
              writeSep (dstOff + 4)
              go (srcOff + 4) (dstOff + 4 + cLen)

    go o 0
    arr <- A.unsafeFreeze dst
    return (Text arr 0 (dstLen - cLen))

replace :: HasCallStack
        => Text
        -> Text
        -> Text
        -> Text
replace needle@(Text _      _      neeLen)
               (Text repArr repOff repLen)
      haystack@(Text hayArr hayOff hayLen)
  | neeLen == 0 = emptyError "replace"
  | len == 0 = empty -- if also haystack is empty, we can't just return 'haystack' as worker/wrapper might duplicate it
  | L.null ixs  = haystack
  | otherwise   = Text (A.run x) 0 len
  where
    ixs = indices needle haystack
    len = hayLen - (neeLen - repLen) `mul` L.length ixs
    x :: ST s (A.MArray s)
    x = do
      marr <- A.new len
      let loop (i:is) o d = do
            let d0 = d + i - o
                d1 = d0 + repLen
            A.copyI (i - o) marr d  hayArr (hayOff+o)
            A.copyI repLen  marr d0 repArr repOff
            loop is (i + neeLen) d1
          loop []     o d = A.copyI (len - d) marr d hayArr (hayOff+o)
      loop ixs 0 0
      return marr



toCaseFold :: Text -> Text
toCaseFold = \t ->
    if null t then empty
    else toCaseFoldNonEmpty t

toLower :: Text -> Text
toLower = \t ->
  if null t then empty
  else toLowerNonEmpty t

toUpper :: Text -> Text
toUpper = \t ->
  if null t then empty
  else toUpperNonEmpty t

toTitle :: Text -> Text
toTitle = \t ->
  if null t then empty
  else toTitleNonEmpty t

justifyLeft :: Int -> Char -> Text -> Text
justifyLeft k c t
    | len >= k  = t
    | otherwise = t `append` replicateChar (k-len) c
  where len = length t

justifyRight :: Int -> Char -> Text -> Text
justifyRight k c t
    | len >= k  = t
    | otherwise = replicateChar (k-len) c `append` t
  where len = length t

center :: Int -> Char -> Text -> Text
center k c t
    | len >= k  = t
    | otherwise = replicateChar l c `append` t `append` replicateChar r c
  where len = length t
        d   = k - len
        r   = d `quot` 2
        l   = d - r

transpose :: [Text] -> [Text]
transpose ts = P.map pack (L.transpose (P.map unpack ts))


foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z t = S.foldl f z (stream t)

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z t = S.foldl' f z (stream t)

foldl1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1 f t = S.foldl1 f (stream t)

foldl1' :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1' f t = S.foldl1' f (stream t)

foldlM' :: Monad m => (a -> Char -> m a) -> a -> Text -> m a
foldlM' f z t = S.foldlM' f z (stream t)

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z t = S.foldr f z (stream t)

foldr1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldr1 f t = S.foldr1 f (stream t)

foldr' :: (Char -> a -> a) -> a -> Text -> a
foldr' f z t = S.foldl' (P.flip f) z (reverseStream t)


concat :: [Text] -> Text
concat ts = case ts of
    [] -> empty
    [t] -> t
    _ | len == 0 -> empty
      | otherwise -> Text (A.run go) 0 len
  where
    len = sumP "concat" $ L.map lengthWord8 ts
    go :: ST s (A.MArray s)
    go = do
      arr <- A.new len
      let step i (Text a o l) = A.copyI l arr i a o >> return (i + l)
      foldM step 0 ts >> return arr

concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . foldr ((:) . f) []

any :: (Char -> Bool) -> Text -> Bool
any p t = S.any p (stream t)

all :: (Char -> Bool) -> Text -> Bool
all p t = S.all p (stream t)

maximum :: HasCallStack => Text -> Char
maximum t = S.maximum (stream t)

minimum :: HasCallStack => Text -> Char
minimum t = S.minimum (stream t)

scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z t = unstream (S.scanl g z (stream t))
    where g a b = safe (f a b)

scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 f t | null t    = empty
           | otherwise = scanl f (unsafeHead t) (unsafeTail t)

scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr f z = S.reverse . S.reverseScanr g z . reverseStream
    where g a b = safe (f a b)

scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 f t | null t    = empty
           | otherwise = scanr f (last t) (init t)

mapAccumL :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumL f z0 = go
  where
    go (Text src o l) = runST $ do
      marr <- A.new (l + 4)
      outer marr (l + 4) o 0 z0
      where
        outer :: forall s. A.MArray s -> Int -> Int -> Int -> a -> ST s (a, Text)
        outer !dst !dstLen = inner
          where
            inner !srcOff !dstOff !z
              | srcOff >= l + o = do
                A.shrinkM dst dstOff
                arr <- A.unsafeFreeze dst
                return (z, Text arr 0 dstOff)
              | dstOff + 4 > dstLen = do
                let !dstLen' = dstLen + (l + o) - srcOff + 4
                dst' <- A.resizeM dst dstLen'
                outer dst' dstLen' srcOff dstOff z
              | otherwise = do
                let !(Iter c d) = iterArray src srcOff
                    (z', c') = f z c
                d' <- unsafeWrite dst dstOff (safe c')
                inner (srcOff + d) (dstOff + d') z'

mapAccumR :: forall a. (a -> Char -> (a, Char)) -> a -> Text -> (a, Text)
mapAccumR f z0 = go
  where
    go (Text src o l) = runST $ do
      marr <- A.new (l + 4)
      outer marr (l + o - 1) (l + 4 - 1) z0
      where
        outer :: forall s. A.MArray s -> Int -> Int -> a -> ST s (a, Text)
        outer !dst = inner
          where
            inner !srcOff !dstOff !z
              | srcOff < o = do
                dstLen <- A.getSizeofMArray dst
                arr <- A.unsafeFreeze dst
                return (z, Text arr (dstOff + 1) (dstLen - dstOff - 1))
              | dstOff < 3 = do
                dstLen <- A.getSizeofMArray dst
                let !dstLen' = dstLen + (srcOff - o) + 4
                dst' <- A.new dstLen'
                A.copyM dst' (dstLen' - dstLen) dst 0 dstLen
                outer dst' srcOff (dstOff + dstLen' - dstLen) z
              | otherwise = do
                let !(Iter c d) = reverseIterArray src (srcOff)
                    (z', c') = f z c
                    c'' = safe c'
                    !d' = utf8Length c''
                    dstOff' = dstOff - d'
                _ <- unsafeWrite dst (dstOff' + 1) c''
                inner (srcOff + d) dstOff' z'


replicate :: Int -> Text -> Text
replicate n t@(Text a o l)
    | n <= 0 || l <= 0       = empty
    | n == 1                 = t
    | isSingleton t          = replicateChar n (unsafeHead t)
    | otherwise              = runST $ do
        let totalLen = n `mul` l
        marr <- A.new totalLen
        A.copyI l marr 0 a o
        A.tile marr l
        arr  <- A.unsafeFreeze marr
        return $ Text arr 0 totalLen

"TEXT replicate/singleton -> replicateChar" [~1] forall n c.
    replicate n (singleton c) = replicateChar n c

replicateChar :: Int -> Char -> Text
replicateChar !len !c'
  | len <= 0  = empty
  | Char.isAscii c = runST $ do
    marr <- A.newFilled len (Char.ord c)
    arr  <- A.unsafeFreeze marr
    return $ Text arr 0 len
  | otherwise = runST $ do
    let cLen = utf8Length c
        totalLen = cLen P.* len
    marr <- A.new totalLen
    _ <- unsafeWrite marr 0 c
    A.tile marr cLen
    arr  <- A.unsafeFreeze marr
    return $ Text arr 0 totalLen
  where
    c = safe c'

unfoldr     :: (a -> Maybe (Char,a)) -> a -> Text
unfoldr f s = unstream (S.unfoldr (firstf safe . f) s)

unfoldrN     :: Int -> (a -> Maybe (Char,a)) -> a -> Text
unfoldrN n f s = unstream (S.unfoldrN n (firstf safe . f) s)


take :: Int -> Text -> Text
take n t@(Text arr off len)
    | n <= 0    = empty
    | n >= len || m >= len || m < 0  = t
    | otherwise = Text arr off m
  where
    m = measureOff n t

measureOff :: Int -> Text -> Int
measureOff !n (Text (A.ByteArray arr) off len) = if len == 0 then 0 else
  cSsizeToInt $
    measure_off arr (intToCSize off) (intToCSize len) (intToCSize n)

takeEnd :: Int -> Text -> Text
takeEnd n t@(Text arr off len)
    | n <= 0    = empty
    | n >= len  = t
    | otherwise = text arr (off+i) (len-i)
  where i = iterNEnd n t

iterNEnd :: Int -> Text -> Int
iterNEnd n t@(Text _arr _off len) = loop (len-1) n
  where loop i !m
          | m <= 0    = i+1
          | i <= 0    = 0
          | otherwise = loop (i+d) (m-1)
          where d = reverseIter_ t i

drop :: Int -> Text -> Text
drop n t@(Text arr off len)
    | n <= 0    = t
    | n >= len || m >= len || m < 0 = empty
    | otherwise = Text arr (off+m) (len-m)
  where m = measureOff n t

dropEnd :: Int -> Text -> Text
dropEnd n t@(Text arr off len)
    | n <= 0    = t
    | n >= len  = empty
    | otherwise = text arr off (iterNEnd n t)

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t@(Text arr off len) = loop 0
  where loop !i | i >= len    = t
                | p c         = loop (i+d)
                | otherwise   = text arr off i
            where Iter c d    = iter t i

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p t@(Text arr off len) = loop (len-1) len
  where loop !i !l | l <= 0    = t
                   | p c       = loop (i+d) (l+d)
                   | otherwise = text arr (off+l) (len-l)
            where Iter c d     = reverseIter t i

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t@(Text arr off len) = loop 0 0
  where loop !i !l | l >= len  = empty
                   | p c       = loop (i+d) (l+d)
                   | otherwise = Text arr (off+i) (len-l)
            where Iter c d     = iter t i

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p t@(Text arr off len) = loop (len-1) len
  where loop !i !l | l <= 0    = empty
                   | p c       = loop (i+d) (l+d)
                   | otherwise = Text arr off l
            where Iter c d     = reverseIter t i

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p

stripStart :: Text -> Text
stripStart = dropWhile Char.isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd Char.isSpace

strip :: Text -> Text
strip = dropAround Char.isSpace

splitAt :: Int -> Text -> (Text, Text)
splitAt n t@(Text arr off len)
    | n <= 0    = (empty, t)
    | n >= len || m >= len || m < 0  = (t, empty)
    | otherwise = (Text arr off m, Text arr (off+m) (len-m))
  where
    m = measureOff n t

span :: (Char -> Bool) -> Text -> (Text, Text)
span p t = case span_ p t of
             (# hd,tl #) -> (hd,tl)

break :: (Char -> Bool) -> Text -> (Text, Text)
break p = span (not . p)

spanM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanM p t@(Text arr off len) = go 0
  where
    go !i | i < len = case iterArray arr (off+i) of
        Iter c l -> do
            continue <- p c
            if continue then go (i+l)
            else pure (text arr off i, text arr (off+i) (len-i))
    go _ = pure (t, empty)

spanEndM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanEndM p t@(Text arr off len) = go (len-1)
  where
    go !i | 0 <= i = case reverseIterArray arr (off+i) of
        Iter c l -> do
            continue <- p c
            if continue then go (i+l)
            else pure (text arr off (i+1), text arr (off+i+1) (len-i-1))
    go _ = pure (empty, t)

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy p = loop
  where
    loop t@(Text arr off len)
        | null t    = []
        | otherwise = text arr off n : loop (text arr (off+n) (len-n))
        where Iter c d = iter t 0
              n     = d + findAIndexOrEnd (not . p c) (Text arr (off+d) (len-d))

findAIndexOrEnd :: (Char -> Bool) -> Text -> Int
findAIndexOrEnd q t@(Text _arr _off len) = go 0
    where go !i | i >= len || q c       = i
                | otherwise             = go (i+d)
                where Iter c d          = iter t i

group :: Text -> [Text]
group = groupBy (==)

inits :: Text -> [Text]
inits = (NonEmptyList.toList $!) . initsNE

initsNE :: Text -> NonEmptyList.NonEmpty Text
initsNE t = empty NonEmptyList.:| case t of
  Text arr off len ->
    let loop i
          | i >= len = []
          | otherwise = let !j = i + iter_ t i in Text arr off j : loop j
    in loop 0

tails :: Text -> [Text]
tails = (NonEmptyList.toList $!) . tailsNE

tailsNE :: Text -> NonEmptyList.NonEmpty Text
tailsNE t
  | null t = empty NonEmptyList.:| []
  | otherwise = t NonEmptyList.:| tails (unsafeTail t)


splitOn :: HasCallStack
        => Text
        -> Text
        -> [Text]
splitOn pat@(Text _ _ l) src@(Text arr off len)
    | l <= 0          = emptyError "splitOn"
    | isSingleton pat = split (== unsafeHead pat) src
    | otherwise       = go 0 (indices pat src)
  where
    go !s (x:xs) =  text arr (s+off) (x-s) : go (x+l) xs
    go  s _      = [text arr (s+off) (len-s)]

"TEXT splitOn/singleton -> split/==" [~1] forall c t.
    splitOn (singleton c) t = split (==c) t

split :: (Char -> Bool) -> Text -> [Text]
split p t
    | null t = [empty]
    | otherwise = loop t
    where loop s | null s'   = [l]
                 | otherwise = l : loop (unsafeTail s')
              where (# l, s' #) = span_ (not . p) s

chunksOf :: Int -> Text -> [Text]
chunksOf k = go
  where
    go t = case splitAt k t of
             (a,b) | null a    -> []
                   | otherwise -> a : go b



elem :: Char -> Text -> Bool
elem c t = S.any (== c) (stream t)

find :: (Char -> Bool) -> Text -> Maybe Char
find p t = S.findBy p (stream t)

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p t = (filter p t, filter (not . p) t)

filter :: (Char -> Bool) -> Text -> Text
filter p = filter_ text p

"TEXT filter/filter -> filter" forall p q t.
    filter p (filter q t) = filter (\c -> q c && p c) t

breakOn :: HasCallStack => Text -> Text -> (Text, Text)
breakOn pat src@(Text arr off len)
    | null pat  = emptyError "breakOn"
    | otherwise = case indices pat src of
                    []    -> (src, empty)
                    (x:_) -> (text arr off x, text arr (off+x) (len-x))

breakOnEnd :: HasCallStack => Text -> Text -> (Text, Text)
breakOnEnd pat src = (reverse b, reverse a)
    where (a,b) = breakOn (reverse pat) (reverse src)

breakOnAll :: HasCallStack
           => Text              -- ^ @needle@ to search for
           -> Text              -- ^ @haystack@ in which to search
           -> [(Text, Text)]
breakOnAll pat src@(Text arr off slen)
    | null pat  = emptyError "breakOnAll"
    | otherwise = L.map step (indices pat src)
  where
    step       x = (chunk 0 x, chunk x (slen-x))
    chunk !n !l  = text arr (n+off) l



index :: HasCallStack => Text -> Int -> Char
index t n = S.index (stream t) n

findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p t = S.findIndex p (stream t)

count :: HasCallStack => Text -> Text -> Int
count pat
    | null pat        = emptyError "count"
    | isSingleton pat = countChar (unsafeHead pat)
    | otherwise       = L.length . indices pat

"TEXT count/singleton -> countChar" [~1] forall c t.
    count (singleton c) t = countChar c t

countChar :: Char -> Text -> Int
countChar c t = S.countChar c (stream t)


zip :: Text -> Text -> [(Char,Char)]
zip a b = S.unstreamList $ S.zipWith (,) (stream a) (stream b)

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f t1 t2 = unstream (S.zipWith g (stream t1) (stream t2))
    where g a b = safe (f a b)

words :: Text -> [Text]
words (Text arr off len) = loop 0 0
  where
    loop !start !n
        | n >= len = if start == n
                     then []
                     else [Text arr (start + off) (n - start)]
        | isAsciiSpace w0 =
            if start == n
            then loop (n + 1) (n + 1)
            else Text arr (start + off) (n - start) : loop (n + 1) (n + 1)
        | w0 < 0x80 = loop start (n + 1)
        | w0 == 0xC2, w1 == 0xA0 =
            if start == n
            then loop (n + 2) (n + 2)
            else Text arr (start + off) (n - start) : loop (n + 2) (n + 2)
        | w0 < 0xE0 = loop start (n + 2)
        |  w0 == 0xE1 && w1 == 0x9A && w2 == 0x80
        || w0 == 0xE2 && (w1 == 0x80 && Char.isSpace (chr3 w0 w1 w2) || w1 == 0x81 && w2 == 0x9F)
        || w0 == 0xE3 && w1 == 0x80 && w2 == 0x80 =
            if start == n
            then loop (n + 3) (n + 3)
            else Text arr (start + off) (n - start) : loop (n + 3) (n + 3)
        | otherwise = loop start (n + utf8LengthByLeader w0)
        where
            w0 = A.unsafeIndex arr (off + n)
            w1 = A.unsafeIndex arr (off + n + 1)
            w2 = A.unsafeIndex arr (off + n + 2)

isAsciiSpace :: Word8 -> Bool
isAsciiSpace w = w .&. 0x50 == 0 && w < 0x80 && (w == 0x20 || w - 0x09 < 5)

lines :: Text -> [Text]
lines (Text arr@(A.ByteArray arr#) off len) = go off
  where
    go !n
      | n >= len + off = []
      | delta < 0 = [Text arr n (len + off - n)]
      | otherwise = Text arr n delta : go (n + delta + 1)
      where
        delta = memchr arr# n (len + off - n) 0x0A

unlines :: [Text] -> Text
unlines = concat . L.foldr (\t acc -> t : singleton '\n' : acc) []

unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')

isPrefixOf :: Text -> Text -> Bool
isPrefixOf a@(Text _ _ alen) b@(Text _ _ blen) =
    alen <= blen && S.isPrefixOf (stream a) (stream b)

isSuffixOf :: Text -> Text -> Bool
isSuffixOf a@(Text _aarr _aoff alen) b@(Text barr boff blen) =
    d >= 0 && a == b'
  where d              = blen - alen
        b' | d == 0    = b
           | otherwise = Text barr (boff+d) alen

isInfixOf ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Text -> Bool
isInfixOf needle haystack
    | null needle        = True
    | isSingleton needle = S.elem (unsafeHead needle) . S.stream $ haystack
    | otherwise          = not . L.null . indices needle $ haystack


stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p@(Text _arr _off plen) t@(Text arr off len)
    | p `isPrefixOf` t = Just $! text arr (off+plen) (len-plen)
    | otherwise        = Nothing

commonPrefixes :: Text -> Text -> Maybe (Text, Text, Text)
commonPrefixes !t0@(Text arr0 off0 len0) !t1@(Text arr1 off1 len1)
  | len0 == 0 = Nothing
  | len1 == 0 = Nothing
  | otherwise = go 0 0
  where
    go !i !j
      | i == len0 = Just (t0, empty, text arr1 (off1 + i) (len1 - i))
      | i == len1 = Just (t1, text arr0 (off0 + i) (len0 - i), empty)
      | a == b = go (i + 1) k
      | k > 0 = Just (Text arr0 off0 k,
                      Text arr0 (off0 + k) (len0 - k),
                      Text arr1 (off1 + k) (len1 - k))
      | otherwise = Nothing
      where
        a = A.unsafeIndex arr0 (off0 + i)
        b = A.unsafeIndex arr1 (off1 + i)
        isLeader = word8ToInt8 a >= -64
        k = if isLeader then i else j

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix p@(Text _arr _off plen) t@(Text arr off len)
    | p `isSuffixOf` t = Just $! text arr off (len-plen)
    | otherwise        = Nothing

sumP :: String -> [Int] -> Int
sumP fun = L.foldl' add 0
  where add a x
            | ax >= 0   = ax
            | otherwise = overflowError fun
          where ax = a + x

emptyError :: HasCallStack => String -> a
emptyError fun = P.error $ "Data.Text." ++ fun ++ ": empty input"

overflowError :: HasCallStack => String -> a
overflowError fun = P.error $ "Data.Text." ++ fun ++ ": size overflow"

show :: Show a => a -> Text
show = pack . P.show

copy :: Text -> Text
copy t@(Text arr off len)
  | null t = empty
  | otherwise = Text (A.run go) 0 len
  where
    go :: ST s (A.MArray s)
    go = do
      marr <- A.new len
      A.copyI len marr 0 arr off
      return marr

ord8 :: Char -> Word8
ord8 = P.fromIntegral . Char.ord

intToCSize :: Int -> CSize
intToCSize = P.fromIntegral

cSsizeToInt :: CSsize -> Int
cSsizeToInt = P.fromIntegral

word8ToInt8 :: Word8 -> Int8
word8ToInt8 = P.fromIntegral




module Data.Text.Encoding
    (

      decodeLatin1
    , decodeASCIIPrefix
    , decodeUtf8Lenient
    , decodeUtf8'
    , decodeASCII'

    , decodeUtf8With
    , decodeUtf16LEWith
    , decodeUtf16BEWith
    , decodeUtf32LEWith
    , decodeUtf32BEWith

    , streamDecodeUtf8With
    , Decoding(..)

    , decodeUtf8Chunk
    , decodeUtf8More
    , Utf8State
    , startUtf8State
    , StrictBuilder
    , StrictTextBuilder
    , strictBuilderToText
    , textToStrictBuilder

    , decodeASCII
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    , streamDecodeUtf8

    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE

    , encodeUtf8Builder
    , encodeUtf8BuilderEscaped

    , validateUtf8Chunk
    , validateUtf8More
    ) where

import Control.Exception (evaluate, try)
import Data.Word (Word8)
import GHC.Exts (byteArrayContents#, unsafeCoerce#)
import GHC.ForeignPtr (ForeignPtr(..), ForeignPtrContents(PlainPtr))
import Data.ByteString (ByteString)
#if defined(PURE_HASKELL)
import Control.Monad.ST.Unsafe (unsafeSTToIO)
import Data.ByteString.Char8 (unpack)
import Data.Text.Internal (pack)
import Foreign.Ptr (minusPtr, plusPtr)
import Foreign.Storable (poke)
#else
import Control.Monad.ST (runST)
import Control.Monad.ST.Unsafe (unsafeIOToST, unsafeSTToIO)
import Data.Bits (shiftR, (.&.))
import Data.Text.Internal.ByteStringCompat (withBS)
import Data.Text.Internal.Unsafe (unsafeWithForeignPtr)
import Foreign.C.Types (CSize(..))
import Foreign.Ptr (Ptr, minusPtr, plusPtr)
import Foreign.Storable (poke, peekByteOff)
#endif
import Data.Text.Encoding.Error (OnDecodeError, UnicodeException, strictDecode, lenientDecode)
import Data.Text.Internal (Text(..), empty)
import Data.Text.Internal.Encoding
import Data.Text.Internal.IsAscii (asciiPrefixLength)
import Data.Text.Unsafe (unsafeDupablePerformIO)
import Data.Text.Show ()
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Internal as B hiding (empty, append)
import qualified Data.ByteString.Builder.Prim as BP
import qualified Data.ByteString.Builder.Prim.Internal as BP
import qualified Data.ByteString.Short.Internal as SBS
import qualified Data.Text.Array as A
import qualified Data.Text.Internal.Encoding.Fusion as E
import qualified Data.Text.Internal.Fusion as F
#if defined(ASSERTS)
import GHC.Stack (HasCallStack)
#endif





decodeASCIIPrefix :: ByteString -> (Text, ByteString)
decodeASCIIPrefix bs = if B.null bs
  then (empty, B.empty)
  else
    let len = asciiPrefixLength bs
        prefix =
          let !(SBS.SBS arr) = SBS.toShort (B.take len bs) in
          Text (A.ByteArray arr) 0 len
        suffix = B.drop len bs in
    (prefix, suffix)

decodeASCII' :: ByteString -> Maybe Text
decodeASCII' bs =
  let (prefix, suffix) = decodeASCIIPrefix bs in
  if B.null suffix then Just prefix else Nothing

decodeASCII :: ByteString -> Text
decodeASCII bs =
  let (prefix, suffix) = decodeASCIIPrefix bs in
  case B.uncons suffix of
    Nothing -> prefix
    Just (word, _) ->
      let !errPos = B.length bs - B.length suffix in
      error $ "decodeASCII: detected non-ASCII codepoint " ++ show word ++ " at position " ++ show errPos

decodeLatin1 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Text
#if defined(PURE_HASKELL)
decodeLatin1 bs = pack (Data.ByteString.Char8.unpack bs)
#else
decodeLatin1 bs = withBS bs $ \fp len -> runST $ do
  dst <- A.new (2 * len)
  let inner srcOff dstOff = if srcOff >= len then return dstOff else do
        asciiPrefixLen <- fmap fromIntegral $ unsafeIOToST $ unsafeWithForeignPtr fp $ \src ->
          c_is_ascii (src `plusPtr` srcOff) (src `plusPtr` len)
        if asciiPrefixLen == 0
        then do
          byte <- unsafeIOToST $ unsafeWithForeignPtr fp $ \src -> peekByteOff src srcOff
          A.unsafeWrite dst dstOff (0xC0 + (byte `shiftR` 6))
          A.unsafeWrite dst (dstOff + 1) (0x80 + (byte .&. 0x3F))
          inner (srcOff + 1) (dstOff + 2)
        else do
          unsafeIOToST $ unsafeWithForeignPtr fp $ \src ->
            unsafeSTToIO $ A.copyFromPointer dst dstOff (src `plusPtr` srcOff) asciiPrefixLen
          inner (srcOff + asciiPrefixLen) (dstOff + asciiPrefixLen)
  actualLen <- inner 0 0
  dst' <- A.resizeM dst actualLen
  arr <- A.unsafeFreeze dst'
  return $ Text arr 0 actualLen
#endif

#if !defined(PURE_HASKELL)
foreign import ccall unsafe "_hs_text_is_ascii" c_is_ascii
    :: Ptr Word8 -> Ptr Word8 -> IO CSize
#endif


data Decoding = Some !Text !ByteString (ByteString -> Decoding)

instance Show Decoding where
    showsPrec d (Some t bs _) = showParen (d > prec) $
                                showString "Some " . showsPrec prec' t .
                                showChar ' ' . showsPrec prec' bs .
                                showString " _"
      where prec = 10; prec' = prec + 1

streamDecodeUtf8 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Decoding
streamDecodeUtf8 = streamDecodeUtf8With strictDecode

streamDecodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Decoding
streamDecodeUtf8With onErr = loop startUtf8State
  where
    loop s chunk =
      let (builder, undecoded, s') = decodeUtf8With2 onErr invalidUtf8Msg s chunk
      in Some (strictBuilderToText builder) undecoded (loop s')

decodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Text
decodeUtf8With onErr = decodeUtf8With1 onErr invalidUtf8Msg

invalidUtf8Msg :: String
invalidUtf8Msg = "Data.Text.Encoding: Invalid UTF-8 stream"

decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With strictDecode

decodeUtf8' ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Either UnicodeException Text
decodeUtf8' = unsafeDupablePerformIO . try . evaluate . decodeUtf8With strictDecode

decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

encodeUtf8Builder :: Text -> B.Builder
encodeUtf8Builder =
    \txt -> B.builder (step txt)
  where
    step txt@(Text arr off len) !k br@(B.BufferRange op ope)
      | op' <= ope = do
          unsafeSTToIO $ A.copyToPointer arr off op len
          k (B.BufferRange op' ope)
      | otherwise = textCopyStep txt k br
      where
        op' = op `plusPtr` len

textCopyStep :: Text -> B.BuildStep a -> B.BuildStep a
textCopyStep (Text arr off len) k =
    go off (off + len)
  where
    go !ip !ipe (B.BufferRange op ope)
      | inpRemaining <= outRemaining = do
          unsafeSTToIO $ A.copyToPointer arr ip op inpRemaining
          let !br = B.BufferRange (op `plusPtr` inpRemaining) ope
          k br
      | otherwise = do
          unsafeSTToIO $ A.copyToPointer arr ip op outRemaining
          let !ip' = ip + outRemaining
          return $ B.bufferFull 1 ope (go ip' ipe)
      where
        outRemaining = ope `minusPtr` op
        inpRemaining = ipe - ip

encodeUtf8BuilderEscaped :: BP.BoundedPrim Word8 -> Text -> B.Builder
encodeUtf8BuilderEscaped be =
    \txt -> B.builder (mkBuildstep txt)
  where
    bound = max 4 $ BP.sizeBound be

    mkBuildstep (Text arr off len) !k =
        outerLoop off
      where
        iend = off + len

        outerLoop !i0 !br@(B.BufferRange op0 ope)
          | i0 >= iend       = k br
          | outRemaining > 0 = goPartial (i0 + min outRemaining inpRemaining)
          | otherwise        = return $ B.bufferFull bound op0 (outerLoop i0)
          where
            outRemaining = (ope `minusPtr` op0) `quot` bound
            inpRemaining = iend - i0

            goPartial !iendTmp = go i0 op0
              where
                go !i !op
                  | i < iendTmp = do
                    let w = A.unsafeIndex arr i
                    if w < 0x80
                      then BP.runB be w op >>= go (i + 1)
                      else poke op w >> go (i + 1) (op `plusPtr` 1)
                  | otherwise = outerLoop i (B.BufferRange op ope)

encodeUtf8 :: Text -> ByteString
encodeUtf8 (Text arr off len)
  | len == 0  = B.empty
  | otherwise = unsafeDupablePerformIO $ do
    marr@(A.MutableByteArray mba) <- unsafeSTToIO $ A.newPinned len
    unsafeSTToIO $ A.copyI len marr 0 arr off
    let fp = ForeignPtr (byteArrayContents# (unsafeCoerce# mba))
                        (PlainPtr mba)
    pure $ B.fromForeignPtr fp 0 len

decodeUtf16LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16LEWith onErr bs = F.unstream (E.streamUtf16LE onErr bs)

decodeUtf16LE :: ByteString -> Text
decodeUtf16LE = decodeUtf16LEWith strictDecode

decodeUtf16BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16BEWith onErr bs = F.unstream (E.streamUtf16BE onErr bs)

decodeUtf16BE :: ByteString -> Text
decodeUtf16BE = decodeUtf16BEWith strictDecode

encodeUtf16LE :: Text -> ByteString
encodeUtf16LE txt = E.unstream (E.restreamUtf16LE (F.stream txt))

encodeUtf16BE :: Text -> ByteString
encodeUtf16BE txt = E.unstream (E.restreamUtf16BE (F.stream txt))

decodeUtf32LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32LEWith onErr bs = F.unstream (E.streamUtf32LE onErr bs)

decodeUtf32LE :: ByteString -> Text
decodeUtf32LE = decodeUtf32LEWith strictDecode

decodeUtf32BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32BEWith onErr bs = F.unstream (E.streamUtf32BE onErr bs)

decodeUtf32BE :: ByteString -> Text
decodeUtf32BE = decodeUtf32BEWith strictDecode

encodeUtf32LE :: Text -> ByteString
encodeUtf32LE txt = E.unstream (E.restreamUtf32LE (F.stream txt))

encodeUtf32BE :: Text -> ByteString
encodeUtf32BE txt = E.unstream (E.restreamUtf32BE (F.stream txt))



module Data.Text.IO
    (
      readFile
    , writeFile
    , appendFile
    , hGetContents
    , hGetChunk
    , hGetLine
    , hPutStr
    , hPutStrLn
    , interact
    , getContents
    , getLine
    , putStr
    , putStrLn
    ) where

import Data.Text (Text)
import Prelude hiding (appendFile, getContents, getLine, interact,
                       putStr, putStrLn, readFile, writeFile)
import System.IO (Handle, IOMode(..), openFile, stdin, stdout,
                  withFile)
import qualified Control.Exception as E
import Control.Monad (liftM2, when)
import Data.IORef (readIORef)
import qualified Data.Text as T
import Data.Text.Internal.IO (hGetLineWith, readChunk, hPutStr, hPutStrLn)
import GHC.IO.Buffer (CharBuffer, isEmptyBuffer)
import GHC.IO.Exception (IOException(ioe_type), IOErrorType(InappropriateType))
import GHC.IO.Handle.Internals (augmentIOError, hClose_help, wantReadableHandle)
import GHC.IO.Handle.Types (BufferMode(..), Handle__(..), HandleType(..))
import System.IO (hGetBuffering, hFileSize, hSetBuffering, hTell)
import System.IO.Error (isEOFError)

readFile :: FilePath -> IO Text
readFile name = openFile name ReadMode >>= hGetContents

writeFile :: FilePath -> Text -> IO ()
writeFile p = withFile p WriteMode . flip hPutStr

appendFile :: FilePath -> Text -> IO ()
appendFile p = withFile p AppendMode . flip hPutStr

catchError :: String -> Handle -> Handle__ -> IOError -> IO (Text, Bool)
catchError caller h Handle__{..} err
    | isEOFError err = do
        buf <- readIORef haCharBuffer
        return $ if isEmptyBuffer buf
                 then (T.empty, True)
                 else (T.singleton '\r', True)
    | otherwise = E.throwIO (augmentIOError err caller h)

readChunkEof :: Handle__ -> CharBuffer -> IO (Text, Bool)
readChunkEof hh buf = do t <- readChunk hh buf
                         return (t, False)

hGetChunk :: Handle -> IO Text
hGetChunk h = wantReadableHandle "hGetChunk" h readSingleChunk
 where
  readSingleChunk hh@Handle__{..} = do
    buf <- readIORef haCharBuffer
    (t, _) <- readChunkEof hh buf `E.catch` catchError "hGetChunk" h hh
    return (hh, t)

hGetContents :: Handle -> IO Text
hGetContents h = do
  chooseGoodBuffering h
  wantReadableHandle "hGetContents" h readAll
 where
  readAll hh@Handle__{..} = do
    let readChunks = do
          buf <- readIORef haCharBuffer
          (t, eof) <- readChunkEof hh buf
                         `E.catch` catchError "hGetContents" h hh
          if eof
            then return [t]
            else (t:) `fmap` readChunks
    ts <- readChunks
    (hh', _) <- hClose_help hh
    return (hh'{haType=ClosedHandle}, T.concat ts)

chooseGoodBuffering :: Handle -> IO ()
chooseGoodBuffering h = do
  bufMode <- hGetBuffering h
  case bufMode of
    BlockBuffering Nothing -> do
      d <- E.catch (liftM2 (-) (hFileSize h) (hTell h)) $ \(e::IOException) ->
           if ioe_type e == InappropriateType
           then return 16384 -- faster than the 2KB default
           else E.throwIO e
      when (d > 0) . hSetBuffering h . BlockBuffering . Just . fromInteger $ d
    _ -> return ()

hGetLine :: Handle -> IO Text
hGetLine = hGetLineWith T.concat

interact :: (Text -> Text) -> IO ()
interact f = putStr . f =<< getContents

getContents :: IO Text
getContents = hGetContents stdin

getLine :: IO Text
getLine = hGetLine stdin

putStr :: Text -> IO ()
putStr = hPutStr stdout

putStrLn :: Text -> IO ()
putStrLn = hPutStrLn stdout



module Data.Text.Lazy
    (


      Text
    , LazyText

    , pack
    , unpack
    , singleton
    , empty
    , fromChunks
    , toChunks
    , toStrict
    , fromStrict
    , foldrChunks
    , foldlChunks

    , pattern Empty
    , pattern (:<)
    , pattern (:>)

    , cons
    , snoc
    , append
    , uncons
    , unsnoc
    , head
    , last
    , tail
    , init
    , null
    , length
    , compareLength

    , map
    , intercalate
    , intersperse
    , transpose
    , reverse
    , replace

    , toCaseFold
    , toLower
    , toUpper
    , toTitle

    , justifyLeft
    , justifyRight
    , center

    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1
    , foldlM'

    , concat
    , concatMap
    , any
    , all
    , maximum
    , minimum
    , isAscii


    , scanl
    , scanl1
    , scanr
    , scanr1

    , mapAccumL
    , mapAccumR

    , repeat
    , replicate
    , cycle
    , iterate
    , unfoldr
    , unfoldrN


    , take
    , takeEnd
    , drop
    , dropEnd
    , takeWhile
    , takeWhileEnd
    , dropWhile
    , dropWhileEnd
    , dropAround
    , strip
    , stripStart
    , stripEnd
    , splitAt
    , span
    , spanM
    , spanEndM
    , breakOn
    , breakOnEnd
    , break
    , group
    , groupBy
    , inits
    , initsNE
    , tails
    , tailsNE

    , splitOn
    , split
    , chunksOf

    , lines
    , words
    , unlines
    , unwords

    , isPrefixOf
    , isSuffixOf
    , isInfixOf

    , stripPrefix
    , stripSuffix
    , commonPrefixes

    , filter
    , find
    , elem
    , breakOnAll
    , partition


    , index
    , count

    , zip
    , zipWith

    , show

    ) where

import Prelude (Char, Bool(..), Maybe(..), String,
                Eq, (==), Ord(..), Ordering(..), Read(..), Show(showsPrec),
                Monad(..), pure, (<$>),
                (&&), (+), (-), (.), ($), (++),
                error, flip, fmap, fromIntegral, not, otherwise, quot)
import qualified Prelude as P
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Data.Bits (finiteBitSize)
import Data.Int (Int64)
import qualified Data.List as L hiding (head, tail)
import Data.Char (isSpace)
import Data.Data (Data(gfoldl, toConstr, gunfold, dataTypeOf), constrIndex,
                  Constr, mkConstr, DataType, mkDataType, Fixity(Prefix))
import Data.Binary (Binary(get, put))
import Data.Binary.Put (putBuilder)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Monoid (Monoid(..))
import Data.Semigroup (Semigroup(..))
import Data.String (IsString(..))
import qualified Data.Text as T
import qualified Data.Text.Array as A
import qualified Data.Text.Internal as T
import qualified Data.Text.Internal.Fusion.Common as S
import qualified Data.Text.Unsafe as T
import qualified Data.Text.Internal.Lazy.Fusion as S
import Data.Text.Internal.Fusion.Types (PairS(..))
import Data.Text.Internal.Lazy.Fusion (stream, unstream)
import Data.Text.Internal.Lazy (Text(..), chunk, empty, foldlChunks,
                                foldrChunks, smallChunkSize, defaultChunkSize, equal, LazyText)
import Data.Text.Internal (firstf, safe, text)
import Data.Text.Internal.Reverse (reverseNonEmpty)
import Data.Text.Internal.Transformation (mapNonEmpty, toCaseFoldNonEmpty, toLowerNonEmpty, toUpperNonEmpty, filter_)
import Data.Text.Lazy.Encoding (decodeUtf8', encodeUtf8Builder)
import Data.Text.Internal.Lazy.Search (indices)
import qualified GHC.CString as GHC
import qualified GHC.Exts as Exts
import GHC.Prim (Addr#)
import GHC.Stack (HasCallStack)
import qualified Language.Haskell.TH.Lib as TH
import qualified Language.Haskell.TH.Syntax as TH
import Text.Printf (PrintfArg, formatArg, formatString)




instance Eq Text where
    (==) = equal

instance Ord Text where
    compare = compareText

compareText :: Text -> Text -> Ordering
compareText Empty Empty = EQ
compareText Empty _     = LT
compareText _     Empty = GT
compareText (Chunk (T.Text arrA offA lenA) as) (Chunk (T.Text arrB offB lenB) bs) =
  A.compare arrA offA arrB offB (min lenA lenB) <> case lenA `compare` lenB of
    LT -> compareText as (Chunk (T.Text arrB (offB + lenA) (lenB - lenA)) bs)
    EQ -> compareText as bs
    GT -> compareText (Chunk (T.Text arrA (offA + lenB) (lenA - lenB)) as) bs

instance Show Text where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read Text where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

instance Semigroup Text where
    (<>) = append

instance Monoid Text where
    mempty  = empty
    mappend = (<>)
    mconcat = concat

instance IsString Text where
    fromString = pack

instance Exts.IsList Text where
    type Item Text = Char
    fromList       = pack
    toList         = unpack

instance NFData Text where
    rnf Empty        = ()
    rnf (Chunk _ ts) = rnf ts

instance Binary Text where
    put t = do
      put (foldlChunks (\n c -> n + T.lengthWord8 c) 0 t)
      putBuilder (encodeUtf8Builder t)
    get   = do
      bs <- get
      case decodeUtf8' bs of
        P.Left exn -> P.fail (P.show exn)
        P.Right a -> P.return a

instance Data Text where
  gfoldl f z txt = z pack `f` (unpack txt)
  toConstr _     = packConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z pack)
    _ -> error "Data.Text.Lazy.Text.gunfold"
  dataTypeOf _   = textDataType

instance TH.Lift Text where
  lift = TH.appE (TH.varE 'fromStrict) . TH.lift . toStrict
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

instance PrintfArg Text where
  formatArg txt = formatString $ unpack txt

packConstr :: Constr
packConstr = mkConstr textDataType "pack" [] Prefix

textDataType :: DataType
textDataType = mkDataType "Data.Text.Lazy.Text" [packConstr]

pack ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  String -> Text
pack = unstream . S.streamList . L.map safe

unpack ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> String
unpack t = S.unstreamList (stream t)

unpackCString# :: Addr# -> Text
unpackCString# addr# = unstream (S.streamCString# addr#)

    unstream (S.streamList (L.map safe (GHC.unpackCString# a)))

    unstream (S.streamList (L.map safe (GHC.unpackCStringUtf8# a)))

    unstream (S.streamList (L.map safe []))

    unstream (S.streamList (L.map safe [a]))

singleton :: Char -> Text
singleton c = Chunk (T.singleton c) Empty

fromChunks :: [T.Text] -> Text
fromChunks cs = L.foldr chunk Empty cs

toChunks :: Text -> [T.Text]
toChunks cs = foldrChunks (:) [] cs

toStrict :: LazyText -> T.StrictText
toStrict t = T.concat (toChunks t)

fromStrict :: T.StrictText -> LazyText
fromStrict t = chunk t Empty


cons :: Char -> Text -> Text
cons c t = Chunk (T.singleton c) t

infixr 5 `cons`

snoc :: Text -> Char -> Text
snoc t c = foldrChunks Chunk (singleton c) t

append :: Text -> Text -> Text
append xs ys = foldrChunks Chunk ys xs

uncons :: Text -> Maybe (Char, Text)
uncons Empty        = Nothing
uncons (Chunk t ts) = Just (T.unsafeHead t, ts')
  where ts' | T.compareLength t 1 == EQ = ts
            | otherwise                 = Chunk (T.unsafeTail t) ts

head :: HasCallStack => Text -> Char
head t = S.head (stream t)

tail :: HasCallStack => Text -> Text
tail (Chunk t ts) = chunk (T.tail t) ts
tail Empty        = emptyError "tail"

init :: HasCallStack => Text -> Text
init (Chunk t0 ts0) = go t0 ts0
    where go t (Chunk t' ts) = Chunk t (go t' ts)
          go t Empty         = chunk (T.init t) Empty
init Empty = emptyError "init"

unsnoc :: Text -> Maybe (Text, Char)
unsnoc Empty          = Nothing
unsnoc ts@(Chunk _ _) = Just (init ts, last ts)

null :: Text -> Bool
null Empty = True
null _     = False

pattern (:<) :: Char -> Text -> Text
pattern x :< xs <- (uncons -> Just (x, xs)) where
  (:<) = cons
infixr 5 :<

pattern (:>) :: Text -> Char -> Text
pattern xs :> x <- (unsnoc -> Just (xs, x)) where
  (:>) = snoc
infixl 5 :>

isSingleton :: Text -> Bool
isSingleton = S.isSingleton . stream

last :: HasCallStack => Text -> Char
last Empty        = emptyError "last"
last (Chunk t ts) = go t ts
    where go _ (Chunk t' ts') = go t' ts'
          go t' Empty         = T.last t'

length :: Text -> Int64
length = foldlChunks go 0
    where
        go :: Int64 -> T.Text -> Int64
        go l t = l + intToInt64 (T.length t)

"TEXT length/map -> length" forall f t.
    length (map f t) = length t
"TEXT length/zipWith -> length" forall f t1 t2.
    length (zipWith f t1 t2) = min (length t1) (length t2)
"TEXT length/replicate -> n" forall n t.
    length (replicate n t) = max 0 n P.* length t
"TEXT length/cons -> length+1" forall c t.
    length (cons c t) = 1 + length t
"TEXT length/intersperse -> 2*length-1" forall c t.
    length (intersperse c t) = max 0 (2 P.* length t - 1)
"TEXT length/intercalate -> n*length" forall s ts.
    length (intercalate s ts) = let lenS = length s in max 0 (P.sum (P.map (\t -> length t + lenS) ts) - lenS)

compareLength :: Text -> Int64 -> Ordering
compareLength t c = S.compareLengthI (stream t) c


map :: (Char -> Char) -> Text -> Text
map f = foldrChunks (Chunk . mapNonEmpty f) Empty

"TEXT map/map -> map" forall f g t.
    map f (map g t) = map (f . safe . g) t

intercalate :: Text -> [Text] -> Text
intercalate t = concat . L.intersperse t

intersperse :: Char -> Text -> Text
intersperse c t = unstream (S.intersperse (safe c) (stream t))

justifyLeft :: Int64 -> Char -> Text -> Text
justifyLeft k c t
    | len >= k  = t
    | otherwise = t `append` replicateChunk (k-len) (T.singleton c)
  where len = length t

justifyRight :: Int64 -> Char -> Text -> Text
justifyRight k c t
    | len >= k  = t
    | otherwise = replicateChunk (k-len) (T.singleton c) `append` t
  where len = length t

center :: Int64 -> Char -> Text -> Text
center k c t
    | len >= k  = t
    | otherwise = replicateChunk l (T.singleton c) `append` t `append` replicateChunk r (T.singleton c)
  where len = length t
        d   = k - len
        r   = d `quot` 2
        l   = d - r

transpose :: [Text] -> [Text]
transpose ts = L.map (\ss -> Chunk (T.pack ss) Empty)
                     (L.transpose (L.map unpack ts))

reverse ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Text
reverse = rev Empty
  where rev a Empty        = a
        rev a (Chunk t ts) = rev (Chunk (reverseNonEmpty t) a) ts

replace :: HasCallStack
        => Text
        -> Text
        -> Text
        -> Text
replace s d = intercalate d . splitOn s



toCaseFold :: Text -> Text
toCaseFold = foldrChunks (\chnk acc -> Chunk (toCaseFoldNonEmpty chnk) acc) Empty

toLower :: Text -> Text
toLower = foldrChunks (\chnk acc -> Chunk (toLowerNonEmpty chnk) acc) Empty

toUpper :: Text -> Text
toUpper = foldrChunks (\chnk acc -> Chunk (toUpperNonEmpty chnk) acc) Empty


toTitle :: Text -> Text
toTitle = foldrChunks (\chnk acc -> Chunk (T.toTitle chnk) acc) Empty

foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z t = S.foldl f z (stream t)

foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z t = S.foldl' f z (stream t)

foldl1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1 f t = S.foldl1 f (stream t)

foldl1' :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1' f t = S.foldl1' f (stream t)

foldlM' :: Monad m => (a -> Char -> m a) -> a -> Text -> m a
foldlM' f z t = S.foldlM' f z (stream t)

foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z t = S.foldr f z (stream t)

foldr1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldr1 f t = S.foldr1 f (stream t)

concat :: [Text] -> Text
concat []                    = Empty
concat (Empty : css)         = concat css
concat (Chunk c Empty : css) = Chunk c (concat css)
concat (Chunk c cs : css)    = Chunk c (concat (cs : css))

concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . foldr ((:) . f) []

any :: (Char -> Bool) -> Text -> Bool
any p t = S.any p (stream t)

all :: (Char -> Bool) -> Text -> Bool
all p t = S.all p (stream t)

maximum :: HasCallStack => Text -> Char
maximum t = S.maximum (stream t)

minimum :: HasCallStack => Text -> Char
minimum t = S.minimum (stream t)

isAscii :: Text -> Bool
isAscii = foldrChunks (\chnk acc -> T.isAscii chnk && acc) True

scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z t = unstream (S.scanl g z (stream t))
    where g a b = safe (f a b)

scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 f t0 = case uncons t0 of
                Nothing -> empty
                Just (t,ts) -> scanl f t ts

scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr f v = reverse . scanl g v . reverse
    where g a b = safe (f b a)

scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 f t | null t    = empty
           | otherwise = scanr f (last t) (init t)

mapAccumL :: (a -> Char -> (a,Char)) -> a -> Text -> (a, Text)
mapAccumL f = go
  where
    go z (Chunk c cs)    = (z'', Chunk c' cs')
        where (z',  c')  = T.mapAccumL f z c
              (z'', cs') = go z' cs
    go z Empty           = (z, Empty)

mapAccumR :: (a -> Char -> (a,Char)) -> a -> Text -> (a, Text)
mapAccumR f = go
  where
    go z (Chunk c cs)   = (z'', Chunk c' cs')
        where (z'', c') = T.mapAccumR f z' c
              (z', cs') = go z cs
    go z Empty          = (z, Empty)

repeat :: Char -> Text
repeat c = let t = Chunk (T.replicate smallChunkSize (T.singleton c)) t
            in t

replicate :: Int64 -> Text -> Text
replicate n
  | n <= 0 = P.const Empty
  | otherwise = \case
    Empty -> Empty
    Chunk t Empty -> replicateChunk n t
    t -> concat (rep n)
      where
        rep 0 = []
        rep i = t : rep (i - 1)

replicateChunk :: Int64 -> T.Text -> Text
replicateChunk !n !t@(T.Text _ _ len)
  | n <= 0 = Empty
  | otherwise = Chunk headChunk $ P.foldr Chunk Empty (L.genericReplicate q normalChunk)
  where
    perChunk = defaultChunkSize `quot` len
    normalChunk = T.replicate perChunk t
    (q, r) = n `P.quotRem` intToInt64 perChunk
    headChunk = T.replicate (int64ToInt r) t

cycle :: HasCallStack => Text -> Text
cycle Empty = emptyError "cycle"
cycle t     = let t' = foldrChunks Chunk t' t
               in t'

iterate :: (Char -> Char) -> Char -> Text
iterate f c = let t c' = Chunk (T.singleton c') (t (f c'))
               in t c

unfoldr :: (a -> Maybe (Char,a)) -> a -> Text
unfoldr f s = unstream (S.unfoldr (firstf safe . f) s)

unfoldrN :: Int64 -> (a -> Maybe (Char,a)) -> a -> Text
unfoldrN n f s = unstream (S.unfoldrN n (firstf safe . f) s)

take :: Int64 -> Text -> Text
take i _ | i <= 0 = Empty
take i t0         = take' i t0
  where
    take' :: Int64 -> Text -> Text
    take' 0 _            = Empty
    take' _ Empty        = Empty
    take' n (Chunk t@(T.Text arr off _) ts)
        | finiteBitSize (0 :: P.Int) == 64, m <- T.measureOff (int64ToInt n) t =
          if m >= 0
          then fromStrict (T.Text arr off m)
          else Chunk t (take' (n + intToInt64 m) ts)

        | n < l     = Chunk (T.take (int64ToInt n) t) Empty
        | otherwise = Chunk t (take' (n - l) ts)
        where l = intToInt64 (T.length t)

takeEnd :: Int64 -> Text -> Text
takeEnd n t0
    | n <= 0    = empty
    | otherwise = takeChunk n empty . L.reverse . toChunks $ t0
  where
    takeChunk :: Int64 -> Text -> [T.Text] -> Text
    takeChunk _ acc [] = acc
    takeChunk i acc (t:ts)
      | i <= l    = chunk (T.takeEnd (int64ToInt i) t) acc
      | otherwise = takeChunk (i-l) (Chunk t acc) ts
      where l = intToInt64 (T.length t)

drop :: Int64 -> Text -> Text
drop i t0
    | i <= 0    = t0
    | otherwise = drop' i t0
  where
    drop' :: Int64 -> Text -> Text
    drop' 0 ts           = ts
    drop' _ Empty        = Empty
    drop' n (Chunk t@(T.Text arr off len) ts)
        | finiteBitSize (0 :: P.Int) == 64, m <- T.measureOff (int64ToInt n) t =
          if m >= 0
          then chunk (T.Text arr (off + m) (len - m)) ts
          else drop' (n + intToInt64 m) ts

        | n < l     = Chunk (T.drop (int64ToInt n) t) ts
        | otherwise = drop' (n - l) ts
        where l   = intToInt64 (T.length t)

dropEnd :: Int64 -> Text -> Text
dropEnd n t0
    | n <= 0    = t0
    | otherwise = dropChunk n . L.reverse . toChunks $ t0
  where
    dropChunk :: Int64 -> [T.Text] -> Text
    dropChunk _ [] = empty
    dropChunk m (t:ts)
      | m >= l    = dropChunk (m-l) ts
      | otherwise = fromChunks . L.reverse $
                    T.dropEnd (int64ToInt m) t : ts
      where l = intToInt64 (T.length t)

dropWords :: Int64 -> Text -> Text
dropWords i t0
    | i <= 0    = t0
    | otherwise = drop' i t0
  where
    drop' :: Int64 -> Text -> Text
    drop' 0 ts           = ts
    drop' _ Empty        = Empty
    drop' n (Chunk (T.Text arr off len) ts)
        | n < len'  = chunk (text arr (off+n') (len-n')) ts
        | otherwise = drop' (n - len') ts
        where len'  = intToInt64 len
              n'    = int64ToInt n

takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t0 = takeWhile' t0
  where takeWhile' Empty        = Empty
        takeWhile' (Chunk t ts) =
          case T.findIndex (not . p) t of
            Just n | n > 0     -> Chunk (T.take n t) Empty
                   | otherwise -> Empty
            Nothing            -> Chunk t (takeWhile' ts)

takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p = takeChunk empty . L.reverse . toChunks
  where takeChunk acc []     = acc
        takeChunk acc (t:ts)
          | T.lengthWord8 t' < T.lengthWord8 t
                             = chunk t' acc
          | otherwise        = takeChunk (Chunk t' acc) ts
          where t' = T.takeWhileEnd p t

dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t0 = dropWhile' t0
  where dropWhile' Empty        = Empty
        dropWhile' (Chunk t ts) =
          case T.findIndex (not . p) t of
            Just n  -> Chunk (T.drop n t) ts
            Nothing -> dropWhile' ts

dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p = go
  where go Empty = Empty
        go (Chunk t Empty) = if T.null t'
                             then Empty
                             else Chunk t' Empty
            where t' = T.dropWhileEnd p t
        go (Chunk t ts) = case go ts of
                            Empty -> go (Chunk t Empty)
                            ts' -> Chunk t ts'

dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p

stripStart :: Text -> Text
stripStart = dropWhile isSpace

stripEnd :: Text -> Text
stripEnd = dropWhileEnd isSpace

strip :: Text -> Text
strip = dropAround isSpace

splitAt :: Int64 -> Text -> (Text, Text)
splitAt = loop
  where
    loop :: Int64 -> Text -> (Text, Text)
    loop !_ Empty     = (empty, empty)
    loop n t | n <= 0 = (empty, t)
    loop n (Chunk t ts)
         | n < len   = let (t',t'') = T.splitAt (int64ToInt n) t
                       in (Chunk t' Empty, Chunk t'' ts)
         | otherwise = let (ts',ts'') = loop (n - len) ts
                       in (Chunk t ts', ts'')
         where len = intToInt64 (T.length t)

splitAtWord :: Int64 -> Text -> PairS Text Text
splitAtWord !_ Empty = empty :*: empty
splitAtWord x (Chunk c@(T.Text arr off len) cs)
    | y >= len  = let h :*: t = splitAtWord (x-intToInt64 len) cs
                  in  Chunk c h :*: t
    | otherwise = chunk (text arr off y) empty :*:
                  chunk (text arr (off+y) (len-y)) cs
    where y = int64ToInt x

breakOn :: HasCallStack => Text -> Text -> (Text, Text)
breakOn pat src
    | null pat  = emptyError "breakOn"
    | otherwise = case indices pat src of
                    []    -> (src, empty)
                    (x:_) -> let h :*: t = splitAtWord x src
                             in  (h, t)

breakOnEnd :: HasCallStack => Text -> Text -> (Text, Text)
breakOnEnd pat src = let (a,b) = breakOn (reverse pat) (reverse src)
                   in  (reverse b, reverse a)

breakOnAll :: HasCallStack
           => Text              -- ^ @needle@ to search for
           -> Text              -- ^ @haystack@ in which to search
           -> [(Text, Text)]
breakOnAll pat src
    | null pat  = emptyError "breakOnAll"
    | otherwise = go 0 empty src (indices pat src)
  where
    go !n p s (x:xs) = let h :*: t = splitAtWord (x-n) s
                           h'      = append p h
                       in (h',t) : go x h' t xs
    go _  _ _ _      = []

break :: (Char -> Bool) -> Text -> (Text, Text)
break p t0 = break' t0
  where break' Empty          = (empty, empty)
        break' c@(Chunk t ts) =
          case T.findIndex p t of
            Nothing      -> let (ts', ts'') = break' ts
                            in (Chunk t ts', ts'')
            Just n | n == 0    -> (Empty, c)
                   | otherwise -> let (a,b) = T.splitAt n t
                                  in (Chunk a Empty, Chunk b ts)

span :: (Char -> Bool) -> Text -> (Text, Text)
span p = break (not . p)

spanM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanM p t0 = go t0
  where
    go Empty = pure (empty, empty)
    go (Chunk t ts) = do
        (t1, t2) <- T.spanM p t
        if T.null t2 then first (chunk t) <$> go ts
        else pure (chunk t1 empty, Chunk t2 ts)

spanEndM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanEndM p t0 = go t0
  where
    go Empty = pure (empty, empty)
    go (Chunk t ts) = do
        (t3, t4) <- go ts
        if null t3 then (\(t1, t2) -> (chunk t1 empty, chunk t2 ts)) <$> T.spanEndM p t
        else pure (Chunk t t3, t4)

group :: Text -> [Text]
group =  groupBy (==)

groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy _  Empty        = []
groupBy eq (Chunk t ts) = cons x ys : groupBy eq zs
                          where (ys,zs) = span (eq x) xs
                                x  = T.unsafeHead t
                                xs = chunk (T.unsafeTail t) ts

inits :: Text -> [Text]
inits = (NE.toList P.$!) . initsNE

initsNE :: Text -> NonEmpty Text
initsNE ts0 = Empty NE.:| inits' 0 ts0
  where
    inits' :: Int64  -- Number of previous chunks i
           -> Text   -- The remainder after dropping i chunks from ts0
           -> [Text] -- Prefixes longer than the first i chunks of ts0.
    inits' !i (Chunk t ts) = L.map (takeChunks i ts0) (NE.tail (T.initsNE t))
                          ++ inits' (i + 1) ts
    inits' _ Empty         = []

takeChunks :: Int64 -> Text -> T.Text -> Text
takeChunks !i (Chunk t ts) lastChunk | i > 0 = Chunk t (takeChunks (i - 1) ts lastChunk)
takeChunks _ _ lastChunk = Chunk lastChunk Empty

tails :: Text -> [Text]
tails = (NE.toList P.$!) . tailsNE

tailsNE :: Text -> NonEmpty Text
tailsNE Empty = Empty :| []
tailsNE ts@(Chunk t ts')
  | T.length t == 1 = ts :| tails ts'
  | otherwise       = ts :| tails (Chunk (T.unsafeTail t) ts')


splitOn :: HasCallStack
        => Text
        -> Text
        -> [Text]
splitOn pat src
    | null pat        = emptyError "splitOn"
    | isSingleton pat = split (== head pat) src
    | otherwise       = go 0 (indices pat src) src
  where
    go  _ []     cs = [cs]
    go !i (x:xs) cs = let h :*: t = splitAtWord (x-i) cs
                      in  h : go (x+l) xs (dropWords l t)
    l = foldlChunks (\a (T.Text _ _ b) -> a + intToInt64 b) 0 pat

"LAZY TEXT splitOn/singleton -> split/==" [~1] forall c t.
    splitOn (singleton c) t = split (==c) t

split :: (Char -> Bool) -> Text -> [Text]
split _ Empty = [Empty]
split p (Chunk t0 ts0) = comb [] (T.split p t0) ts0
  where comb acc (s:[]) Empty        = revChunks (s:acc) : []
        comb acc (s:[]) (Chunk t ts) = comb (s:acc) (T.split p t) ts
        comb acc (s:ss) ts           = revChunks (s:acc) : comb [] ss ts
        comb _   []     _            = impossibleError "split"

chunksOf :: Int64 -> Text -> [Text]
chunksOf k = go
  where
    go t = case splitAt k t of
             (a,b) | null a    -> []
                   | otherwise -> a : go b

lines :: Text -> [Text]
lines Empty = []
lines t = NE.toList $ go t
  where
    go :: Text -> NonEmpty Text
    go Empty = Empty :| []
    go (Chunk x xs)
      | hasNlEnd x = NE.fromList $ P.map fromStrict (T.lines x) ++ lines xs
      | otherwise = case unsnocList (T.lines x) of
      Nothing -> impossibleError "lines"
      Just (ls, l) -> P.foldr (NE.cons . fromStrict) (prependToHead l (go xs)) ls

prependToHead :: T.Text -> NonEmpty Text -> NonEmpty Text
prependToHead l ~(x :| xs) = chunk l x :| xs -- Lazy pattern is crucial!

unsnocList :: [a] -> Maybe ([a], a)
unsnocList [] = Nothing
unsnocList (x : xs) = Just $ go x xs
  where
    go y [] = ([], y)
    go y (z : zs) = first (y :) (go z zs)

hasNlEnd :: T.Text -> Bool
hasNlEnd (T.Text arr off len) = A.unsafeIndex arr (off + len - 1) == 0x0A

words :: Text -> [Text]
words = L.filter (not . null) . split isSpace

unlines :: [Text] -> Text
unlines = concat . L.foldr (\t acc -> t : singleton '\n' : acc) []

unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')

isPrefixOf :: Text -> Text -> Bool
isPrefixOf Empty _  = True
isPrefixOf _ Empty  = False
isPrefixOf (Chunk x xs) (Chunk y ys)
    | lx == ly  = x == y  && isPrefixOf xs ys
    | lx <  ly  = x == yh && isPrefixOf xs (Chunk yt ys)
    | otherwise = xh == y && isPrefixOf (Chunk xt xs) ys
  where (xh,xt) = T.splitAt ly x
        (yh,yt) = T.splitAt lx y
        lx = T.length x
        ly = T.length y

isSuffixOf :: Text -> Text -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y

isInfixOf :: Text -> Text -> Bool
isInfixOf needle haystack
    | null needle        = True
    | isSingleton needle = S.elem (head needle) . S.stream $ haystack
    | otherwise          = not . L.null . indices needle $ haystack


stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p t
    | null p    = Just t
    | otherwise = case commonPrefixes p t of
                    Just (_,c,r) | null c -> Just r
                    _                     -> Nothing

commonPrefixes :: Text -> Text -> Maybe (Text,Text,Text)
commonPrefixes Empty _ = Nothing
commonPrefixes _ Empty = Nothing
commonPrefixes a0 b0   = Just (go a0 b0 [])
  where
    go t0@(Chunk x xs) t1@(Chunk y ys) ps
        = case T.commonPrefixes x y of
            Just (p,a,b)
              | T.null a  -> go xs (chunk b ys) (p:ps)
              | T.null b  -> go (chunk a xs) ys (p:ps)
              | otherwise -> (fromChunks (L.reverse (p:ps)),chunk a xs, chunk b ys)
            Nothing       -> (fromChunks (L.reverse ps),t0,t1)
    go t0 t1 ps = (fromChunks (L.reverse ps),t0,t1)

stripSuffix :: Text -> Text -> Maybe Text
stripSuffix p t = reverse `fmap` stripPrefix (reverse p) (reverse t)

filter :: (Char -> Bool) -> Text -> Text
filter p = foldrChunks (chunk . filter_ T.Text p) Empty

"TEXT filter/filter -> filter" forall p q t.
    filter p (filter q t) = filter (\c -> q c && p c) t

find :: (Char -> Bool) -> Text -> Maybe Char
find p t = S.findBy p (stream t)

elem :: Char -> Text -> Bool
elem c t = S.any (== c) (stream t)

partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p t = (filter p t, filter (not . p) t)

index :: HasCallStack => Text -> Int64 -> Char
index t n = S.index (stream t) n

count :: HasCallStack => Text -> Text -> Int64
count pat
    | null pat        = emptyError "count"
    | otherwise       = go 0  . indices pat
  where go !n []     = n
        go !n (_:xs) = go (n+1) xs

"LAZY TEXT count/singleton -> countChar" [~1] forall c t.
    count (singleton c) t = countChar c t

countChar :: Char -> Text -> Int64
countChar c t = S.countChar c (stream t)

zip :: Text -> Text -> [(Char,Char)]
zip a b = S.unstreamList $ S.zipWith (,) (stream a) (stream b)

zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f t1 t2 = unstream (S.zipWith g (stream t1) (stream t2))
    where g a b = safe (f a b)

show :: Show a => a -> Text
show = pack . P.show

revChunks :: [T.Text] -> Text
revChunks = L.foldl' (flip chunk) Empty

emptyError :: HasCallStack => String -> a
emptyError fun = P.error ("Data.Text.Lazy." ++ fun ++ ": empty input")

impossibleError :: HasCallStack => String -> a
impossibleError fun = P.error ("Data.Text.Lazy." ++ fun ++ ": impossible case")

intToInt64 :: Exts.Int -> Int64
intToInt64 = fromIntegral

int64ToInt :: Int64 -> Exts.Int
int64ToInt = fromIntegral

module Network.Socket (
    withSocketsDo,

    getAddrInfo,

    HostName,
    ServiceName,
    AddrInfo (..),
    defaultHints,

    AddrInfoFlag (..),
    addrInfoFlagImplemented,

    connect,
    bind,
    listen,
    accept,

    close,
    close',
    gracefulClose,
    shutdown,
    ShutdownCmd (..),

    SocketOption (
        SockOpt,
        UnsupportedSocketOption,
        Debug,
        ReuseAddr,
        SoDomain,
        Type,
        SoProtocol,
        SoError,
        DontRoute,
        Broadcast,
        SendBuffer,
        RecvBuffer,
        KeepAlive,
        OOBInline,
        TimeToLive,
        MaxSegment,
        NoDelay,
        Cork,
        Linger,
        ReusePort,
        RecvLowWater,
        SendLowWater,
        RecvTimeOut,
        SendTimeOut,
        UseLoopBack,
        UserTimeout,
        IPv6Only,
        RecvIPv4TTL,
        RecvIPv4TOS,
        RecvIPv4PktInfo,
        RecvIPv6HopLimit,
        RecvIPv6TClass,
        RecvIPv6PktInfo
    ),
    isSupportedSocketOption,
    whenSupported,
    getSocketOption,
    setSocketOption,
    StructLinger (..),
    SocketTimeout (..),
    getSockOpt,
    setSockOpt,
    SockOptValue (..),
    setSockOptValue,

    Socket,
    socket,
    openSocket,
    withFdSocket,
    unsafeFdSocket,
    touchSocket,
    socketToFd,
    fdSocket,
    mkSocket,
    socketToHandle,

    SocketType (
        GeneralSocketType,
        UnsupportedSocketType,
        NoSocketType,
        Stream,
        Datagram,
        Raw,
        RDM,
        SeqPacket
    ),
    isSupportedSocketType,
    getSocketType,

    Family (
        GeneralFamily,
        UnsupportedFamily,
        AF_UNSPEC,
        AF_UNIX,
        AF_INET,
        AF_INET6,
        AF_IMPLINK,
        AF_PUP,
        AF_CHAOS,
        AF_NS,
        AF_NBS,
        AF_ECMA,
        AF_DATAKIT,
        AF_CCITT,
        AF_SNA,
        AF_DECnet,
        AF_DLI,
        AF_LAT,
        AF_HYLINK,
        AF_APPLETALK,
        AF_ROUTE,
        AF_NETBIOS,
        AF_NIT,
        AF_802,
        AF_ISO,
        AF_OSI,
        AF_NETMAN,
        AF_X25,
        AF_AX25,
        AF_OSINET,
        AF_GOSSIP,
        AF_IPX,
        Pseudo_AF_XTP,
        AF_CTF,
        AF_WAN,
        AF_SDL,
        AF_NETWARE,
        AF_NDD,
        AF_INTF,
        AF_COIP,
        AF_CNT,
        Pseudo_AF_RTIP,
        Pseudo_AF_PIP,
        AF_SIP,
        AF_ISDN,
        Pseudo_AF_KEY,
        AF_NATM,
        AF_ARP,
        Pseudo_AF_HDRCMPLT,
        AF_ENCAP,
        AF_LINK,
        AF_RAW,
        AF_RIF,
        AF_NETROM,
        AF_BRIDGE,
        AF_ATMPVC,
        AF_ROSE,
        AF_NETBEUI,
        AF_SECURITY,
        AF_PACKET,
        AF_ASH,
        AF_ECONET,
        AF_ATMSVC,
        AF_IRDA,
        AF_PPPOX,
        AF_WANPIPE,
        AF_BLUETOOTH,
        AF_CAN
    ),
    isSupportedFamily,
    packFamily,
    unpackFamily,

    ProtocolNumber,
    defaultProtocol,

    SockAddr (..),
    isSupportedSockAddr,
    getPeerName,
    getSocketName,

    HostAddress,
    hostAddressToTuple,
    tupleToHostAddress,

    HostAddress6,
    hostAddress6ToTuple,
    tupleToHostAddress6,

    FlowInfo,

    ScopeID,
    ifNameToIndex,
    ifIndexToName,

    PortNumber,
    defaultPort,
    socketPortSafe,
    socketPort,

    isUnixDomainSocketAvailable,
    socketPair,
    sendFd,
    recvFd,
    getPeerCredential,

    getNameInfo,
    NameInfoFlag (..),


    setCloseOnExecIfNeeded,
    getCloseOnExec,
    setNonBlockIfNeeded,
    getNonBlock,

    sendBuf,
    recvBuf,
    sendBufTo,
    recvBufFrom,

    sendBufMsg,
    recvBufMsg,
    MsgFlag (
        MSG_OOB,
        MSG_DONTROUTE,
        MSG_PEEK,
        MSG_EOR,
        MSG_TRUNC,
        MSG_CTRUNC,
        MSG_WAITALL
    ),

    Cmsg (..),
    CmsgId (
        CmsgId,
        CmsgIdIPv4TTL,
        CmsgIdIPv6HopLimit,
        CmsgIdIPv4TOS,
        CmsgIdIPv6TClass,
        CmsgIdIPv4PktInfo,
        CmsgIdIPv6PktInfo,
        CmsgIdFds,
        UnsupportedCmsgId
    ),

    lookupCmsg,
    filterCmsg,

    ControlMessage (..),
    IPv4TTL (..),
    IPv6HopLimit (..),
    IPv4TOS (..),
    IPv6TClass (..),
    IPv4PktInfo (..),
    IPv6PktInfo (..),

    maxListenQueue,

    waitReadSocketSTM,
    waitAndCancelReadSocketSTM,
    waitWriteSocketSTM,
    waitAndCancelWriteSocketSTM,
) where

import Network.Socket.Buffer hiding (
    recvBufFrom,
    recvBufMsg,
    sendBufMsg,
    sendBufTo,
 )
import Network.Socket.Cbits
import Network.Socket.Fcntl
import Network.Socket.Flag
import Network.Socket.Handle
import Network.Socket.If
import Network.Socket.Info
import Network.Socket.Internal
import Network.Socket.Name hiding (getPeerName, getSocketName)
import Network.Socket.Options
import Network.Socket.STM
import Network.Socket.Shutdown
import Network.Socket.SockAddr
import Network.Socket.Syscall hiding (accept, bind, connect)
import Network.Socket.Types
import Network.Socket.Unix
#if !defined(mingw32_HOST_OS)
import Network.Socket.Posix.Cmsg
#else
import Network.Socket.Win32.Cmsg
#endif

module Network.Socket.ByteString (
    send,
    sendAll,
    sendTo,
    sendAllTo,

    sendMany,
    sendManyTo,
    sendManyWithFds,

    recv,
    recvFrom,

    sendMsg,
    recvMsg,
) where

import Data.ByteString (ByteString)

import Network.Socket.ByteString.IO hiding (recvFrom, sendAllTo, sendTo)
import qualified Network.Socket.ByteString.IO as G
import Network.Socket.Types




sendTo :: Socket -> ByteString -> SockAddr -> IO Int
sendTo = G.sendTo

sendAllTo :: Socket -> ByteString -> SockAddr -> IO ()
sendAllTo = G.sendAllTo

recvFrom :: Socket -> Int -> IO (ByteString, SockAddr)
recvFrom = G.recvFrom


module Network.Socket.ByteString.Lazy (
    send,
    sendAll,
    sendWithFds,

    getContents,
    recv,
) where

import Data.ByteString.Lazy.Internal (
    ByteString (..),
    defaultChunkSize,
 )
import Network.Socket (ShutdownCmd (..), shutdown)
import System.IO.Error (catchIOError)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.Posix.Types (Fd (..))
import Prelude hiding (getContents)

#if defined(mingw32_HOST_OS)
import Network.Socket.ByteString.Lazy.Windows  (send, sendAll)
#else
import Network.Socket.ByteString.Lazy.Posix    (send, sendAll)
#endif

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Network.Socket.ByteString as N
import Network.Socket.Imports
import Network.Socket.Types

sendWithFds
    :: Socket
    -> ByteString
    -> [Fd]
    -> IO ()
sendWithFds s lbs fds = N.sendManyWithFds s (L.toChunks lbs) fds


getContents
    :: Socket
    -> IO ByteString
getContents s = loop
  where
    loop = unsafeInterleaveIO $ do
        sbs <- N.recv s defaultChunkSize
        if S.null sbs
            then do
                shutdown s ShutdownReceive `catchIOError` const (return ())
                return Empty
            else Chunk sbs <$> loop

recv
    :: Socket
    -> Int64
    -> IO ByteString
recv s nbytes = chunk <$> N.recv s (fromIntegral nbytes)
  where
    chunk k
        | S.null k = Empty
        | otherwise = Chunk k Empty

#if __GLASGOW_HASKELL__ >= 704
#endif
Module      :  System.FilePath
Copyright   :  (c) Neil Mitchell 2005-2014
License     :  BSD3

Maintainer  :  ndmitchell@gmail.com
Stability   :  stable
Portability :  portable

A library for 'FilePath' manipulations, using Posix or Windows filepaths
depending on the platform.

Both "System.FilePath.Posix" and "System.FilePath.Windows" provide the
same interface.

Given the example 'FilePath': @\/directory\/file.ext@

We can use the following functions to extract pieces.

* 'takeFileName' gives @\"file.ext\"@

* 'takeDirectory' gives @\"\/directory\"@

* 'takeExtension' gives @\".ext\"@

* 'dropExtension' gives @\"\/directory\/file\"@

* 'takeBaseName' gives @\"file\"@

And we could have built an equivalent path with the following expressions:

* @\"\/directory\" '</>' \"file.ext\"@.

* @\"\/directory\/file" '<.>' \"ext\"@.

* @\"\/directory\/file.txt" '-<.>' \"ext\"@.

Each function in this module is documented with several examples,
which are also used as tests.

Here are a few examples of using the @filepath@ functions together:

/Example 1:/ Find the possible locations of a Haskell module @Test@ imported from module @Main@:

@['replaceFileName' path_to_main \"Test\" '<.>' ext | ext <- [\"hs\",\"lhs\"] ]@

/Example 2:/ Download a file from @url@ and save it to disk:

@do let file = 'makeValid' url
  System.Directory.createDirectoryIfMissing True ('takeDirectory' file)@

/Example 3:/ Compile a Haskell file, putting the @.hi@ file under @interface@:

@'takeDirectory' file '</>' \"interface\" '</>' ('takeFileName' file '-<.>' \"hi\")@

References:
[1] <http://msdn.microsoft.com/en-us/library/windows/desktop/aa365247.aspx Naming Files, Paths and Namespaces> (Microsoft MSDN)


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module System.FilePath(
    FilePath,
    pathSeparator, pathSeparators, isPathSeparator,
    searchPathSeparator, isSearchPathSeparator,
    extSeparator, isExtSeparator,

    splitSearchPath, getSearchPath,

    splitExtension,
    takeExtension, replaceExtension, (-<.>), dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, takeExtensions, replaceExtensions, isExtensionOf,
    stripExtension,

    splitFileName,
    takeFileName, replaceFileName, dropFileName,
    takeBaseName, replaceBaseName,
    takeDirectory, replaceDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,

    splitDrive, joinDrive,
    takeDrive, hasDrive, dropDrive, isDrive,

    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,

    normalise, equalFilePath,
    makeRelative,
    isRelative, isAbsolute,
    isValid, makeValid
) where
import System.FilePath.Windows
#else
module System.FilePath(
    FilePath,
    pathSeparator, pathSeparators, isPathSeparator,
    searchPathSeparator, isSearchPathSeparator,
    extSeparator, isExtSeparator,

    splitSearchPath, getSearchPath,

    splitExtension,
    takeExtension, replaceExtension, (-<.>), dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, takeExtensions, replaceExtensions, isExtensionOf,
    stripExtension,

    splitFileName,
    takeFileName, replaceFileName, dropFileName,
    takeBaseName, replaceBaseName,
    takeDirectory, replaceDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,

    splitDrive, joinDrive,
    takeDrive, hasDrive, dropDrive, isDrive,

    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,

    normalise, equalFilePath,
    makeRelative,
    isRelative, isAbsolute,
    isValid, makeValid
) where
import System.FilePath.Posix
#endif


module System.OsPath.Internal where

import {-# SOURCE #-} System.OsPath
    ( isValid )
import System.OsPath.Types
import qualified System.OsString.Internal as OS

import Control.Monad.Catch
    ( MonadThrow )
import Data.ByteString
    ( ByteString )
import Language.Haskell.TH.Quote
    ( QuasiQuoter (..) )
import Language.Haskell.TH.Syntax
    ( Lift (..), lift )
import GHC.IO.Encoding.Failure ( CodingFailureMode(..) )

import System.OsString.Internal.Types
import System.OsPath.Encoding
import Control.Monad (when)
import System.IO
    ( TextEncoding )

#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
import qualified System.OsPath.Windows as PF
import GHC.IO.Encoding.UTF16 ( mkUTF16le )
#else
import qualified System.OsPath.Posix as PF
import GHC.IO.Encoding.UTF8 ( mkUTF8 )
#endif
import GHC.Stack (HasCallStack)



encodeUtf :: MonadThrow m => FilePath -> m OsPath
encodeUtf = OS.encodeUtf

unsafeEncodeUtf :: HasCallStack => String -> OsString
unsafeEncodeUtf = OS.unsafeEncodeUtf

encodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding (wide char)
           -> FilePath
           -> Either EncodingException OsPath
encodeWith = OS.encodeWith

encodeFS :: FilePath -> IO OsPath
encodeFS = OS.encodeFS


decodeUtf :: MonadThrow m => OsPath -> m FilePath
decodeUtf = OS.decodeUtf

decodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding
           -> OsPath
           -> Either EncodingException FilePath
decodeWith = OS.decodeWith

decodeFS :: OsPath -> IO FilePath
decodeFS = OS.decodeFS


fromBytes :: MonadThrow m
          => ByteString
          -> m OsPath
fromBytes = OS.fromBytes



osp :: QuasiQuoter
osp = QuasiQuoter
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
  { quoteExp = \s -> do
      osp' <- either (fail . show) (pure . OsString) . PF.encodeWith (mkUTF16le ErrorOnCodingFailure) $ s
      when (not $ isValid osp') $ fail ("filepath not valid: " ++ show osp')
      lift osp'
  , quotePat = \s -> do
      osp' <- either (fail . show) (pure . OsString) . PF.encodeWith (mkUTF16le ErrorOnCodingFailure) $ s
      when (not $ isValid osp') $ fail ("filepath not valid: " ++ show osp')
      [p|((==) osp' -> True)|]
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a declaration)"
  }
#else
  { quoteExp = \s -> do
      osp' <- either (fail . show) (pure . OsString) . PF.encodeWith (mkUTF8 ErrorOnCodingFailure) $ s
      when (not $ isValid osp') $ fail ("filepath not valid: " ++ show osp')
      lift osp'
  , quotePat = \s -> do
      osp' <- either (fail . show) (pure . OsString) . PF.encodeWith (mkUTF8 ErrorOnCodingFailure) $ s
      when (not $ isValid osp') $ fail ("filepath not valid: " ++ show osp')
      [p|((==) osp' -> True)|]
  , quoteType = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a type)"
  , quoteDec  = \_ ->
      fail "illegal QuasiQuote (allowed as expression or pattern only, used as a declaration)"
  }
#endif


unpack :: OsPath -> [OsChar]
unpack = OS.unpack


pack :: [OsChar] -> OsPath
pack = OS.pack



module System.Directory
   (

      createDirectory
    , createDirectoryIfMissing
    , removeDirectory
    , removeDirectoryRecursive
    , removePathForcibly
    , renameDirectory
    , listDirectory
    , getDirectoryContents
    , getCurrentDirectory
    , setCurrentDirectory
    , withCurrentDirectory

    , getHomeDirectory
    , XdgDirectory(..)
    , getXdgDirectory
    , XdgDirectoryList(..)
    , getXdgDirectoryList
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    , removeFile
    , renameFile
    , renamePath
    , copyFile
    , copyFileWithMetadata
    , getFileSize

    , canonicalizePath
    , makeAbsolute
    , makeRelativeToCurrentDirectory

    , doesPathExist
    , doesFileExist
    , doesDirectoryExist

    , findExecutable
    , findExecutables
    , findExecutablesInDirectories
    , findFile
    , findFiles
    , findFileWith
    , findFilesWith
    , exeExtension

    , createFileLink
    , createDirectoryLink
    , removeDirectoryLink
    , pathIsSymbolicLink
    , getSymbolicLinkTarget



    , Permissions
    , emptyPermissions
    , readable
    , writable
    , executable
    , searchable
    , setOwnerReadable
    , setOwnerWritable
    , setOwnerExecutable
    , setOwnerSearchable

    , getPermissions
    , setPermissions
    , copyPermissions


    , getAccessTime
    , getModificationTime
    , setAccessTime
    , setModificationTime

    , isSymbolicLink

   ) where
import Prelude ()
import System.Directory.Internal
import System.Directory.Internal.Prelude
import Data.Time (UTCTime)
import System.OsPath (decodeFS, encodeFS)
import qualified System.Directory.OsPath as D

A directory contains a series of entries, each of which is a named
reference to a file system object (file, directory etc.).  Some
entries may be hidden, inaccessible, or have some administrative
function (e.g. @.@ or @..@ under
<http://www.opengroup.org/onlinepubs/009695399 POSIX>), but in
this standard all such entries are considered to form part of the
directory contents. Entries in sub-directories are not, however,
considered to form part of the directory contents.

Each file system object is referenced by a /path/.  There is
normally at least one absolute path to each file system object.  In
some operating systems, it may also be possible to have paths which
are relative to the current directory.

Unless otherwise documented:

* 'IO' operations in this package may throw any 'IOError'.  No other types of
  exceptions shall be thrown.

* The list of possible 'IOErrorType's in the API documentation is not
  exhaustive.  The full list may vary by platform and/or evolve over time.




directory offers a limited (and quirky) interface for reading and setting file
and directory permissions; see 'getPermissions' and 'setPermissions' for a
discussion of their limitations.  Because permissions are very difficult to
implement portably across different platforms, users who wish to do more
sophisticated things with permissions are advised to use other,
platform-specific libraries instead.  For example, if you are only interested
in permissions on POSIX-like platforms,
<https://hackage.haskell.org/package/unix/docs/System-Posix-Files.html unix>
offers much more flexibility.

 The 'Permissions' type is used to record whether certain operations are
 permissible on a file\/directory. 'getPermissions' and 'setPermissions'
 get and set these permissions, respectively. Permissions apply both to
 files and directories. For directories, the executable field will be
 'False', and for files the searchable field will be 'False'. Note that
 directories may be searchable without being readable, if permission has
 been given to use them as part of a path, but not to examine the
 directory contents.

Note that to change some, but not all permissions, a construct on the following lines must be used.

>  makeReadable f = do
>     p <- getPermissions f
>     setPermissions f (p {readable = True})


emptyPermissions :: Permissions
emptyPermissions = Permissions {
                       readable   = False,
                       writable   = False,
                       executable = False,
                       searchable = False
                   }

setOwnerReadable :: Bool -> Permissions -> Permissions
setOwnerReadable b p = p { readable = b }

setOwnerWritable :: Bool -> Permissions -> Permissions
setOwnerWritable b p = p { writable = b }

setOwnerExecutable :: Bool -> Permissions -> Permissions
setOwnerExecutable b p = p { executable = b }

setOwnerSearchable :: Bool -> Permissions -> Permissions
setOwnerSearchable b p = p { searchable = b }

getPermissions :: FilePath -> IO Permissions
getPermissions = encodeFS >=> D.getPermissions

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions path p = encodeFS path >>= (`D.setPermissions` p)

copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions src dst = do
  src' <- encodeFS src
  dst' <- encodeFS dst
  D.copyPermissions src' dst'



initially empty, or as near to empty as the operating system
allows.

The operation may fail with:

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES]@

* 'isAlreadyExistsError'
The operand refers to a directory that already exists.
@ [EEXIST]@

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
There is no path to the directory.
@[ENOENT, ENOTDIR]@

* 'System.IO.isFullError'
Insufficient resources (virtual memory, process file descriptors,
physical disk space, etc.) are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* @InappropriateType@
The path refers to an existing non-directory object.
@[EEXIST]@


createDirectory :: FilePath -> IO ()
createDirectory = encodeFS >=> D.createDirectory

createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
                         -> FilePath -- ^ The path to the directory you want to make
                         -> IO ()
createDirectoryIfMissing cp = encodeFS >=> D.createDirectoryIfMissing cp


implementation may specify additional constraints which must be
satisfied before a directory can be removed (e.g. the directory has to
be empty, or may not be in use by other processes).  It is not legal
for an implementation to partially remove a directory unless the
entire directory is removed. A conformant implementation need not
support directory removal in all situations (e.g. removal of the root
directory).

The operation may fail with:

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
The operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
The directory does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@

* @UnsupportedOperation@
The implementation does not support removal in this situation.
@[EINVAL]@

* @InappropriateType@
The operand refers to an existing non-directory object.
@[ENOTDIR]@


removeDirectory :: FilePath -> IO ()
removeDirectory = encodeFS >=> D.removeDirectory

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = encodeFS >=> D.removeDirectoryRecursive

removePathForcibly :: FilePath -> IO ()
removePathForcibly = encodeFS >=> D.removePathForcibly

/file/, where /file/ is not itself a directory. The
implementation may specify additional constraints which must be
satisfied before a file can be removed (e.g. the file may not be in
use by other processes).

The operation may fail with:

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
The operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
The file does not exist.
@[ENOENT, ENOTDIR]@

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY]@

* @InappropriateType@
The operand refers to an existing directory.
@[EPERM, EINVAL]@


removeFile :: FilePath -> IO ()
removeFile = encodeFS >=> D.removeFile

directory from /old/ to /new/.  If the /new/ directory
already exists, it is atomically replaced by the /old/ directory.
If the /new/ directory is neither the /old/ directory nor an
alias of the /old/ directory, it is removed as if by
'removeDirectory'.  A conformant implementation need not support
renaming directories in all situations (e.g. renaming to an existing
directory, or across different physical devices), but the constraints
must be documented.

On Win32 platforms, @renameDirectory@ fails if the /new/ directory already
exists.

The operation may fail with:

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
Either operand is not a valid directory name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
The original directory does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'System.IO.isFullError'
Insufficient resources are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY, ENOTEMPTY, EEXIST]@

* @UnsupportedOperation@
The implementation does not support renaming in this situation.
@[EINVAL, EXDEV]@

* @InappropriateType@
Either path refers to an existing non-directory object.
@[ENOTDIR, EISDIR]@


renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory opath npath = do
  opath' <- encodeFS opath
  npath' <- encodeFS npath
  D.renameDirectory opath' npath'

object from /old/ to /new/.  If the /new/ object already exists, it is
replaced by the /old/ object.  Neither path may refer to an existing
directory.

A conformant implementation need not support renaming files in all situations
(e.g. renaming across different physical devices), but the constraints must be
documented. On Windows, this does not support renaming across different physical
devices; if you are looking to do so, consider using 'copyFileWithMetadata' and
'removeFile'.

On Windows, this calls @MoveFileEx@ with @MOVEFILE_REPLACE_EXISTING@ set,
which is not guaranteed to be atomic
(<https://github.com/haskell/directory/issues/109>).

On other platforms, this operation is atomic.

The operation may fail with:

* @HardwareFault@
A physical I\/O error has occurred.
@[EIO]@

* @InvalidArgument@
Either operand is not a valid file name.
@[ENAMETOOLONG, ELOOP]@

* 'isDoesNotExistError'
The original file does not exist, or there is no path to the target.
@[ENOENT, ENOTDIR]@

* 'isPermissionError'
The process has insufficient privileges to perform the operation.
@[EROFS, EACCES, EPERM]@

* 'System.IO.isFullError'
Insufficient resources are available to perform the operation.
@[EDQUOT, ENOSPC, ENOMEM, EMLINK]@

* @UnsatisfiedConstraints@
Implementation-dependent constraints are not satisfied.
@[EBUSY]@

* @UnsupportedOperation@
The implementation does not support renaming in this situation.
@[EXDEV]@

* @InappropriateType@
Either path refers to an existing directory.
@[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@


renameFile :: FilePath -> FilePath -> IO ()
renameFile opath npath = do
  opath' <- encodeFS opath
  npath' <- encodeFS npath
  D.renameFile opath' npath'

renamePath :: FilePath                  -- ^ Old path
           -> FilePath                  -- ^ New path
           -> IO ()
renamePath opath npath = do
  opath' <- encodeFS opath
  npath' <- encodeFS npath
  D.renamePath opath' npath'

copyFile :: FilePath                    -- ^ Source filename
         -> FilePath                    -- ^ Destination filename
         -> IO ()
copyFile fromFPath toFPath = do
  fromFPath' <- encodeFS fromFPath
  toFPath' <- encodeFS toFPath
  D.copyFile fromFPath' toFPath'

copyFileWithMetadata :: FilePath        -- ^ Source file
                     -> FilePath        -- ^ Destination file
                     -> IO ()
copyFileWithMetadata src dst = do
  src' <- encodeFS src
  dst' <- encodeFS dst
  D.copyFileWithMetadata src' dst'


canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = encodeFS >=> D.canonicalizePath >=> decodeFS

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = encodeFS >=> D.makeAbsolute >=> decodeFS

makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory =
  encodeFS >=> D.makeRelativeToCurrentDirectory >=> decodeFS

findExecutable :: String -> IO (Maybe FilePath)
findExecutable = encodeFS >=> D.findExecutable >=> (`for` decodeFS)

findExecutables :: String -> IO [FilePath]
findExecutables = encodeFS >=> D.findExecutables >=> (`for` decodeFS)

findExecutablesInDirectories :: [FilePath] -> String -> IO [FilePath]
findExecutablesInDirectories path binary = do
  path' <- for path encodeFS
  binary' <- encodeFS binary
  D.findExecutablesInDirectories path' binary'
    >>= (`for` decodeFS)

findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile = findFileWith (\ _ -> pure True)

findFiles :: [FilePath] -> String -> IO [FilePath]
findFiles = findFilesWith (\ _ -> pure True)

findFileWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO (Maybe FilePath)
findFileWith f ds name = do
  ds' <- for ds encodeFS
  name' <- encodeFS name
  D.findFileWith (decodeFS >=> f) ds' name'
    >>= (`for` decodeFS)

findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]
findFilesWith f ds name = do
  ds' <- for ds encodeFS
  name' <- encodeFS name
  res <- D.findFilesWith (decodeFS >=> f) ds' name'
  for res decodeFS

exeExtension :: String
exeExtension = so D.exeExtension

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents = encodeFS >=> D.getDirectoryContents >=> (`for` decodeFS)

listDirectory :: FilePath -> IO [FilePath]
listDirectory = encodeFS >=> D.listDirectory >=> (`for` decodeFS)

getCurrentDirectory :: IO FilePath
getCurrentDirectory = D.getCurrentDirectory >>= decodeFS

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = encodeFS >=> D.setCurrentDirectory

withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  encodeFS dir >>= (`D.withCurrentDirectory` action)

getFileSize :: FilePath -> IO Integer
getFileSize = encodeFS >=> D.getFileSize

doesPathExist :: FilePath -> IO Bool
doesPathExist = encodeFS >=> D.doesPathExist

exists and is either a directory or a symbolic link to a directory,
and 'False' otherwise.

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = encodeFS >=> D.doesDirectoryExist

if the argument file exists and is not a directory, and 'False' otherwise.

doesFileExist :: FilePath -> IO Bool
doesFileExist = encodeFS >=> D.doesFileExist


createFileLink
  :: FilePath                           -- ^ path to the target file
  -> FilePath                           -- ^ path of the link to be created
  -> IO ()
createFileLink target link = do
  target' <- encodeFS target
  link' <- encodeFS link
  D.createFileLink target' link'


createDirectoryLink
  :: FilePath                           -- ^ path to the target directory
  -> FilePath                           -- ^ path of the link to be created
  -> IO ()
createDirectoryLink target link = do
  target' <- encodeFS target
  link' <- encodeFS link
  D.createDirectoryLink target' link'

removeDirectoryLink :: FilePath -> IO ()
removeDirectoryLink = encodeFS >=> D.removeDirectoryLink

pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink = encodeFS >=> D.pathIsSymbolicLink

isSymbolicLink :: FilePath -> IO Bool
isSymbolicLink = pathIsSymbolicLink

getSymbolicLinkTarget :: FilePath -> IO FilePath
getSymbolicLinkTarget = encodeFS >=> D.getSymbolicLinkTarget >=> decodeFS

getAccessTime :: FilePath -> IO UTCTime
getAccessTime = encodeFS >=> D.getAccessTime

getModificationTime :: FilePath -> IO UTCTime
getModificationTime = encodeFS >=> D.getModificationTime

setAccessTime :: FilePath -> UTCTime -> IO ()
setAccessTime path atime = encodeFS path >>= (`D.setAccessTime` atime)

setModificationTime :: FilePath -> UTCTime -> IO ()
setModificationTime path mtime =
  encodeFS path >>= (`D.setModificationTime` mtime)


The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getXdgDirectory' or
'getAppUserDataDirectory' instead.

On Unix, 'getHomeDirectory' behaves as follows:

* Returns $HOME env variable if set (including to an empty string).
* Otherwise uses home directory returned by `getpwuid_r` using the UID of the current proccesses user. This basically reads the /etc/passwd file. An empty home directory field is considered valid.

On Windows, the system is queried for a suitable path; a typical path might be @C:\/Users\//\<user\>/@.

The operation may fail with:

* @UnsupportedOperation@
The operating system has no notion of home directory.

* 'isDoesNotExistError'
The home directory for the current user does not exist, or
cannot be found.
getHomeDirectory :: IO FilePath
getHomeDirectory = D.getHomeDirectory >>= decodeFS

getXdgDirectory :: XdgDirectory         -- ^ which special directory
                -> FilePath             -- ^ a relative path that is appended
                -> IO FilePath
getXdgDirectory xdgDir = encodeFS >=> D.getXdgDirectory xdgDir >=> decodeFS

getXdgDirectoryList :: XdgDirectoryList -- ^ which special directory list
                    -> IO [FilePath]
getXdgDirectoryList = D.getXdgDirectoryList >=> (`for` decodeFS)

getAppUserDataDirectory :: FilePath     -- ^ a relative path that is appended
                        -> IO FilePath
getAppUserDataDirectory = encodeFS >=> D.getAppUserDataDirectory >=>decodeFS


The directory returned is expected to be writable by the current user,
but note that it isn't generally considered good practice to store
application-specific data here; use 'getXdgDirectory' or
'getAppUserDataDirectory' instead.

On Unix, 'getUserDocumentsDirectory' returns the value of the @HOME@
environment variable.  On Windows, the system is queried for a
suitable path; a typical path might be @C:\/Users\//\<user\>/\/Documents@.

The operation may fail with:

* @UnsupportedOperation@
The operating system has no notion of document directory.

* 'isDoesNotExistError'
The document directory for the current user does not exist, or
cannot be found.
getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = D.getUserDocumentsDirectory >>= decodeFS

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = D.getTemporaryDirectory >>= decodeFS

module Data.Set (
              Set          -- instance Eq,Ord,Show,Read,Data

            , empty
            , singleton
            , fromList
            , fromAscList
            , fromDescList
            , fromDistinctAscList
            , fromDistinctDescList
            , powerSet

            , insert

            , delete


            , alterF

            , member
            , notMember
            , lookupLT
            , lookupGT
            , lookupLE
            , lookupGE
            , S.null
            , size
            , isSubsetOf
            , isProperSubsetOf
            , disjoint

            , union
            , unions
            , difference
            , (\\)
            , intersection
            , intersections
            , symmetricDifference
            , cartesianProduct
            , disjointUnion
            , Intersection(..)

            , S.filter
            , takeWhileAntitone
            , dropWhileAntitone
            , spanAntitone
            , partition
            , split
            , splitMember
            , splitRoot

            , lookupIndex
            , findIndex
            , elemAt
            , deleteAt
            , S.take
            , S.drop
            , S.splitAt

            , S.map
            , mapMonotonic

            , S.foldr
            , S.foldl
            , S.foldr'
            , S.foldl'
            , fold

            , lookupMin
            , lookupMax
            , findMin
            , findMax
            , deleteMin
            , deleteMax
            , deleteFindMin
            , deleteFindMax
            , maxView
            , minView


            , elems
            , toList
            , toAscList
            , toDescList

            , showTree
            , showTreeWith
            , valid
            ) where

import Data.Set.Internal as S

#if __GLASGOW_HASKELL__
#endif

#include "containers.h"


module Data.Tree(

      Tree(..)
    , Forest
    , PostOrder(..)

    , unfoldTree
    , unfoldForest
    , unfoldTreeM
    , unfoldForestM
    , unfoldTreeM_BF
    , unfoldForestM_BF

    , foldTree
    , flatten
    , levels
    , leaves
    , edges
    , pathsToRoot
    , pathsFromRoot

    , drawTree
    , drawForest

    ) where

import Utils.Containers.Internal.Prelude as Prelude
import Prelude ()
import Data.Bits ((.&.))
import Data.Foldable (toList)
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Traversable (foldMapDefault)
import Control.Monad (liftM)
import Control.Monad.Fix (MonadFix (..), fix)
import Data.Sequence (Seq, empty, singleton, (<|), (|>), fromList,
            ViewL(..), ViewR(..), viewl, viewr)
import Control.DeepSeq (NFData(rnf),NFData1(liftRnf))

#ifdef __GLASGOW_HASKELL__
import Data.Data (Data)
import GHC.Generics (Generic, Generic1)
import qualified GHC.Exts
import Language.Haskell.TH.Syntax (Lift)
import Language.Haskell.TH ()
#endif

import Control.Monad.Zip (MonadZip (..))

#ifdef __GLASGOW_HASKELL__
import Data.Coerce (coerce)
#endif
import Data.Functor.Classes

#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif

#if MIN_VERSION_base(4,18,0)
import qualified Data.Foldable1 as Foldable1
#endif

data Tree a = Node {
        rootLabel :: a,         -- ^ label value
        subForest :: [Tree a]   -- ^ zero or more child trees
    }
#ifdef __GLASGOW_HASKELL__
  deriving ( Eq
           , Ord -- ^ @since 0.6.5
           , Read
           , Show
           , Data
           , Generic  -- ^ @since 0.5.8
           , Generic1 -- ^ @since 0.5.8
           , Lift -- ^ @since 0.6.6
           )
#else
  deriving (Eq, Ord, Read, Show)
#endif

type Forest a = [Tree a]

instance Eq1 Tree where
  liftEq eq = leq
    where
      leq (Node a fr) (Node a' fr') = eq a a' && liftEq leq fr fr'

instance Ord1 Tree where
  liftCompare cmp = lcomp
    where
      lcomp (Node a fr) (Node a' fr') = cmp a a' <> liftCompare lcomp fr fr'

instance Show1 Tree where
  liftShowsPrec shw shwl p (Node a fr) = showParen (p > 10) $
        showString "Node {rootLabel = " . shw 0 a . showString ", " .
          showString "subForest = " . liftShowList shw shwl fr .
          showString "}"

instance Read1 Tree where
  liftReadsPrec rd rdl p = readParen (p > 10) $
    \s -> do
      ("Node", s1) <- lex s
      ("{", s2) <- lex s1
      ("rootLabel", s3) <- lex s2
      ("=", s4) <- lex s3
      (a, s5) <- rd 0 s4
      (",", s6) <- lex s5
      ("subForest", s7) <- lex s6
      ("=", s8) <- lex s7
      (fr, s9) <- liftReadList rd rdl s8
      ("}", s10) <- lex s9
      pure (Node a fr, s10)

instance Functor Tree where
    fmap = fmapTree
    x <$ Node _ ts = Node x (map (x <$) ts)

fmapTree :: (a -> b) -> Tree a -> Tree b
fmapTree f (Node x ts) = Node (f x) (map (fmapTree f) ts)

#ifdef __GLASGOW_HASKELL__
"fmapTree/coerce" fmapTree coerce = coerce
#endif

instance Applicative Tree where
    pure x = Node x []
    Node f tfs <*> tx@(Node x txs) =
        Node (f x) (map (f <$>) txs ++ map (<*> tx) tfs)
    liftA2 f (Node x txs) ty@(Node y tys) =
        Node (f x y) (map (f x <$>) tys ++ map (\tx -> liftA2 f tx ty) txs)
    Node x txs <* ty@(Node _ tys) =
        Node x (map (x <$) tys ++ map (<* ty) txs)
    Node _ txs *> ty@(Node y tys) =
        Node y (tys ++ map (*> ty) txs)

instance Monad Tree where
    Node x ts >>= f = case f x of
        Node x' ts' -> Node x' (ts' ++ map (>>= f) ts)

instance MonadFix Tree where
  mfix = mfixTree

mfixTree :: (a -> Tree a) -> Tree a
mfixTree f
  | Node a children <- fix (f . rootLabel)
  = Node a (zipWith (\i _ -> mfixTree ((!! i) . subForest . f))
                    [0..] children)

instance Traversable Tree where
  traverse f = go
    where go (Node x ts) = liftA2 Node (f x) (traverse go ts)


instance Foldable Tree where
    fold = foldMap id

    foldMap = foldMapDefault

    foldr f z = \t -> go t z  -- Use a lambda to allow inlining with two arguments
      where
        go (Node x ts) = f x . foldr (\t k -> go t . k) id ts

    foldl' f = go
      where go !z (Node x ts) = foldl' go (f z x) ts

    foldr1 = foldrMap1 id

    foldl1 = foldlMap1 id

    null _ = False

    elem = any . (==)

    maximum = foldlMap1' id max

    minimum = foldlMap1' id min

    sum = foldlMap1' id (+)

    product = foldlMap1' id (*)

#if MIN_VERSION_base(4,18,0)

instance Foldable1.Foldable1 Tree where
  foldMap1 f = go
    where
      go (Node x []) = f x
      go (Node x (t : ts)) =
        f x <> Foldable1.foldrMap1 go (\t' z -> go t' <> z) (t :| ts)

  foldMap1' f = foldlMap1' f (\z x -> z <> f x)

  toNonEmpty (Node x ts) = x :| concatMap toList ts

  maximum = Foldable.maximum

  minimum = Foldable.minimum

  foldrMap1 = foldrMap1

  foldlMap1' = foldlMap1'

  foldlMap1 = foldlMap1
#endif

foldrMap1 :: (a -> b) -> (a -> b -> b) -> Tree a -> b
foldrMap1 f g = go
  where
    go (Node x [])     = f x
    go (Node x (t:ts)) = g x (foldrMap1NE go (\t' z -> foldr g z t') t ts)

foldrMap1NE :: (a -> b) -> (a -> b -> b) -> a -> [a] -> b
foldrMap1NE f g = go
  where
    go x []      = f x
    go x (x':xs) = g x (go x' xs)

foldlMap1' :: (a -> b) -> (b -> a -> b) -> Tree a -> b
foldlMap1' f g =  -- Use a lambda to allow inlining with two arguments
  \(Node x ts) -> foldl' (foldl' g) (f x) ts

foldlMap1 :: (a -> b) -> (b -> a -> b) -> Tree a -> b
foldlMap1 f g =  -- Use a lambda to allow inlining with two arguments
  \(Node x ts) -> foldl (foldl g) (f x) ts

instance NFData a => NFData (Tree a) where
    rnf (Node x ts) = rnf x `seq` rnf ts

instance NFData1 Tree where
    liftRnf rnfx = go
      where
      go (Node x ts) = rnfx x `seq` liftRnf go ts

instance MonadZip Tree where
  mzipWith f (Node a as) (Node b bs)
    = Node (f a b) (mzipWith (mzipWith f) as bs)

  munzip (Node (a, b) ts) = (Node a as, Node b bs)
    where (as, bs) = munzip (map munzip ts)

drawTree :: Tree String -> String
drawTree  = unlines . draw

drawForest :: [Tree String] -> String
drawForest  = unlines . map drawTree

draw :: Tree String -> [String]
draw (Node x ts0) = lines x ++ drawSubTrees ts0
  where
    drawSubTrees [] = []
    drawSubTrees [t] =
        "|" : shift "`- " "   " (draw t)
    drawSubTrees (t:ts) =
        "|" : shift "+- " "|  " (draw t) ++ drawSubTrees ts

    shift first other = zipWith (++) (first : repeat other)

flatten :: Tree a -> [a]
flatten = toList

levels :: Tree a -> [[a]]
levels t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

unfoldForest :: (b -> (a, [b])) -> [b] -> [Tree a]
unfoldForest f = map (unfoldTree f)

unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM f b = do
    (a, bs) <- f b
    ts <- unfoldForestM f bs
    return (Node a ts)

unfoldForestM :: Monad m => (b -> m (a, [b])) -> [b] -> m ([Tree a])
unfoldForestM f = Prelude.mapM (unfoldTreeM f)

unfoldTreeM_BF :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM_BF f b = liftM getElement $ unfoldForestQ f (singleton b)
  where
    getElement xs = case viewl xs of
        x :< _ -> x
        EmptyL -> error "unfoldTreeM_BF"

unfoldForestM_BF :: Monad m => (b -> m (a, [b])) -> [b] -> m ([Tree a])
unfoldForestM_BF f = liftM toList . unfoldForestQ f . fromList

unfoldForestQ :: Monad m => (b -> m (a, [b])) -> Seq b -> m (Seq (Tree a))
unfoldForestQ f aQ = case viewl aQ of
    EmptyL -> return empty
    a :< aQ' -> do
        (b, as) <- f a
        tQ <- unfoldForestQ f (Prelude.foldl (|>) aQ' as)
        let (tQ', ts) = splitOnto [] as tQ
        return (Node b ts <| tQ')
  where
    splitOnto :: [a'] -> [b'] -> Seq a' -> (Seq a', [a'])
    splitOnto as [] q = (q, as)
    splitOnto as (_:bs) q = case viewr q of
        q' :> a -> splitOnto (a:as) bs q'
        EmptyR -> error "unfoldForestQ"

leaves :: Tree a -> [a]
#ifdef __GLASGOW_HASKELL__
leaves t = GHC.Exts.build $ \cons nil ->
  let go (Node x []) z = cons x z
      go (Node _ ts) z = foldr go z ts
  in go t nil
#else
leaves t =
  let go (Node x []) z = x:z
      go (Node _ ts) z = foldr go z ts
  in go t []
#endif

edges :: Tree a -> [(a, a)]
#ifdef __GLASGOW_HASKELL__
edges (Node x0 ts0) = GHC.Exts.build $ \cons nil ->
  let go p = foldr (\(Node x ts) z -> cons (p, x) (go x z ts))
  in go x0 nil ts0
#else
edges (Node x0 ts0) =
  let go p = foldr (\(Node x ts) z -> (p, x) : go x z ts)
  in go x0 [] ts0
#endif

pathsToRoot :: Tree a -> Tree (NonEmpty a)
pathsToRoot = go []
  where
    go ps (Node x ts) = Node (x :| ps) (map (go (x:ps)) ts)


pathsFromRoot :: Tree a -> Tree (NonEmpty a)
pathsFromRoot (Node x0 ts0) = Node (x0 :| []) (map (go (singletonBQ x0)) ts0)
  where
    go !q (Node x ts) = Node (toNonEmptyBQ q') (map (go q') ts)
      where
        !q' = snocBQ q x

data BQ a = BQ
  a -- head
  [a] -- front
  ![a] -- rear (reversed)

singletonBQ :: a -> BQ a
singletonBQ x = BQ x 0 [] []

snocBQ :: BQ a -> a -> BQ a
snocBQ (BQ x0 n f r) x
  | doReverse = BQ x0 (n+1) (f ++ reverse (x:r)) []
  | otherwise = BQ x0 (n+1) f (x:r)
  where
    doReverse = (n+2) .&. (n+1) == 0

toNonEmptyBQ :: BQ a -> NonEmpty a
toNonEmptyBQ (BQ x0 _ f r) = case r of
  [] -> x0 :| f -- optimization, no need to rebuild f
  _ -> x0 :| (f ++ reverse r)

newtype PostOrder a = PostOrder { unPostOrder :: Tree a }
#ifdef __GLASGOW_HASKELL__
  deriving (Eq, Ord, Read, Show, Data, Generic, Generic1, Lift)
#else
  deriving (Eq, Ord, Read, Show)
#endif

instance Functor PostOrder where
#ifdef __GLASGOW_HASKELL__
  fmap = (coerce :: ((a -> b) -> Tree a -> Tree b)
                 -> (a -> b) -> PostOrder a -> PostOrder b)
         fmapTree
  (<$) = (coerce :: (b -> Tree a -> Tree b)
                 -> b -> PostOrder a -> PostOrder b)
         (<$)
#else
  fmap f = PostOrder . fmapTree f . unPostOrder
  (<$) x = PostOrder . (x <$) . unPostOrder
#endif

instance Foldable PostOrder where
    fold = foldMap id

    foldMap = foldMapDefault

    foldr f z0 = \(PostOrder t) -> go t z0  -- Use a lambda to inline with two arguments
      where
        go (Node x ts) z = foldr go (f x z) ts

    foldl' f z0 = \(PostOrder t) -> go z0 t  -- Use a lambda to inline with two arguments
      where
        go !z (Node x ts) =
          let !z' = foldl' go z ts
          in f z' x

    foldr1 = foldrMap1PostOrder id

    foldl1 = foldlMap1PostOrder id

    null _ = False

    elem = any . (==)

    maximum = foldlMap1'PostOrder id max

    minimum = foldlMap1'PostOrder id min

    sum = foldlMap1'PostOrder id (+)

    product = foldlMap1'PostOrder id (*)

instance Traversable PostOrder where
  traverse f = \(PostOrder t) -> PostOrder <$> go t
    where
      go (Node x ts) = liftA2 (flip Node) (traverse go ts) (f x)

#if MIN_VERSION_base(4,18,0)
instance Foldable1.Foldable1 PostOrder where
  foldMap1 f = \(PostOrder t) -> go t  -- Use a lambda to inline with one argument
    where
      go (Node x []) = f x
      go (Node x (t:ts)) =
        Foldable1.foldrMap1 go (\t' z' -> go t' <> z') (t :| ts) <> f x

  foldMap1' f = foldlMap1'PostOrder f (\z x -> z <> f x)

  toNonEmpty (PostOrder t0) = go t0 []
    where
      go (Node x []) z = x :| z
      go (Node x (t:ts)) z =
        go t (foldr (\t' z' -> foldr (:) z' (PostOrder t')) (x:z) ts)

  maximum = Foldable.maximum

  minimum = Foldable.minimum

  foldrMap1 = foldrMap1PostOrder

  foldlMap1' = foldlMap1'PostOrder

  foldlMap1 = foldlMap1PostOrder
#endif

foldrMap1PostOrder :: (a -> b) -> (a -> b -> b) -> PostOrder a -> b
foldrMap1PostOrder f g = \(PostOrder (Node x ts)) ->
  foldr (\t z -> foldr g z (PostOrder t)) (f x) ts

foldlMap1PostOrder :: (a -> b) -> (b -> a -> b) -> PostOrder a -> b
foldlMap1PostOrder f g = \(PostOrder t) -> go t
  where
    go (Node x []) = f x
    go (Node x (t:ts)) =
      g (foldl (\z t' -> foldl g z (PostOrder t')) (go t) ts) x

foldlMap1'PostOrder :: (a -> b) -> (b -> a -> b) -> PostOrder a -> b
foldlMap1'PostOrder f g = \(PostOrder t) -> go t
  where
    go (Node x []) = f x
    go (Node x (t:ts)) =
      let !z' = foldl' (\z t' -> foldl' g z (PostOrder t')) (go t) ts
      in g z' x





#include "containers.h"
#if __GLASGOW_HASKELL__
#endif
#ifdef DEFINE_PATTERN_SYNONYMS
#endif
#ifdef USE_ST_MONAD
#endif


module Data.Graph (

      Graph
    , Bounds
    , Edge
    , Vertex
    , Table

    , graphFromEdges
    , graphFromEdges'
    , buildG

    , vertices
    , edges
    , outdegree
    , indegree

    , transposeG

    , dfs
    , dff
    , topSort
    , reverseTopSort
    , components
    , scc
    , bcc
    , reachable
    , path


    , SCC(..
#ifdef DEFINE_PATTERN_SYNONYMS
      , CyclicSCC
#endif
      )

    , stronglyConnComp
    , stronglyConnCompR

    , flattenSCC
    , flattenSCC1
    , flattenSCCs

    , module Data.Tree

    ) where

import Utils.Containers.Internal.Prelude
import Prelude ()
#if USE_ST_MONAD
import Control.Monad.ST
import Data.Array.ST.Safe (newArray, readArray, writeArray)
# if USE_UNBOXED_ARRAYS
import Data.Array.ST.Safe (STUArray)
# else
import Data.Array.ST.Safe (STArray)
# endif
#else
import Data.IntSet (IntSet)
import qualified Data.IntSet as Set
#endif
import Data.Tree (Tree(Node), Forest)

import Data.Foldable as F
#if MIN_VERSION_base(4,18,0)
import qualified Data.Foldable1 as F1
#endif
import Control.DeepSeq (NFData(rnf),NFData1(liftRnf))
import Data.Maybe
import Data.Array
#if USE_UNBOXED_ARRAYS
import qualified Data.Array.Unboxed as UA
import Data.Array.Unboxed ( UArray )
#else
import qualified Data.Array as UA
#endif
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Functor.Classes
#if !MIN_VERSION_base(4,11,0)
import Data.Semigroup (Semigroup (..))
#endif
#ifdef __GLASGOW_HASKELL__
import GHC.Generics (Generic, Generic1)
import Data.Data (Data)
import Language.Haskell.TH.Syntax (Lift(..))
import Language.Haskell.TH ()
#endif

default ()


data SCC vertex
  = AcyclicSCC vertex
  | NECyclicSCC {-# UNPACK #-} !(NonEmpty vertex)
  deriving ( Eq   -- ^ @since 0.5.9
           , Show -- ^ @since 0.5.9
           , Read -- ^ @since 0.5.9
           )

#ifdef DEFINE_PATTERN_SYNONYMS
pattern CyclicSCC :: [vertex] -> SCC vertex
pattern CyclicSCC xs <- NECyclicSCC (NE.toList -> xs) where
  CyclicSCC [] = error "CyclicSCC: an argument cannot be an empty list"
  CyclicSCC (x : xs) = NECyclicSCC (x :| xs)

#endif

#ifdef __GLASGOW_HASKELL__
deriving instance Data vertex => Data (SCC vertex)

deriving instance Generic1 SCC

deriving instance Generic (SCC vertex)

#if MIN_VERSION_template_haskell(2,15,0)
deriving instance Lift vertex => Lift (SCC vertex)
#else
instance Lift vertex => Lift (SCC vertex) where
  lift (AcyclicSCC v) = [| AcyclicSCC v |]
  lift (NECyclicSCC (v :| vs)) = [| NECyclicSCC (v :| vs) |]
#endif

#endif

instance Eq1 SCC where
  liftEq eq (AcyclicSCC v1) (AcyclicSCC v2) = eq v1 v2
  liftEq eq (NECyclicSCC vs1) (NECyclicSCC vs2) = liftEq eq vs1 vs2
  liftEq _ _ _ = False
instance Show1 SCC where
  liftShowsPrec sp _sl d (AcyclicSCC v) = showsUnaryWith sp "AcyclicSCC" d v
  liftShowsPrec sp sl d (NECyclicSCC vs) = showsUnaryWith (liftShowsPrec sp sl) "NECyclicSCC" d vs
instance Read1 SCC where
  liftReadsPrec rp rl = readsData $
    readsUnaryWith rp "AcyclicSCC" AcyclicSCC <>
    readsUnaryWith (liftReadsPrec rp rl) "NECyclicSCC" NECyclicSCC
#ifdef __GLASGOW_HASKELL__
    <> readsUnaryWith (const rl) "CyclicSCC" CyclicSCC
#endif

instance F.Foldable SCC where
  foldr c n (AcyclicSCC v) = c v n
  foldr c n (NECyclicSCC vs) = foldr c n vs

  toList = flattenSCC

#if MIN_VERSION_base(4,18,0)
instance F1.Foldable1 SCC where
  foldMap1 f (AcyclicSCC v) = f v
  foldMap1 f (NECyclicSCC vs) = F1.foldMap1 f vs

  toNonEmpty = flattenSCC1

#endif

instance Traversable SCC where
  traverse f (AcyclicSCC vertex) = AcyclicSCC <$> f vertex
  traverse f (NECyclicSCC (x :| xs)) =
    liftA2 (\x' xs' -> NECyclicSCC (x' :| xs')) (f x) (traverse f xs)

instance NFData a => NFData (SCC a) where
    rnf (AcyclicSCC v) = rnf v
    rnf (NECyclicSCC vs) = rnf vs

instance NFData1 SCC where
    liftRnf rnfx (AcyclicSCC v)   = rnfx v
    liftRnf rnfx (NECyclicSCC vs) = liftRnf rnfx vs

instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    fmap f (NECyclicSCC (x :| xs)) = NECyclicSCC (f x :| map f xs)

flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

flattenSCC :: SCC vertex -> [vertex]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (NECyclicSCC (v :| vs)) = v : vs

flattenSCC1 :: SCC vertex -> NonEmpty vertex
flattenSCC1 (AcyclicSCC v) = v :| []
flattenSCC1 (NECyclicSCC vs) = vs

stronglyConnComp
        :: Ord key
        => [(node, key, [key])]
        -> [SCC node]

stronglyConnComp edges0
  = map get_node (stronglyConnCompR edges0)
  where
    get_node (AcyclicSCC (n, _, _)) = AcyclicSCC n
    get_node (NECyclicSCC ((n0, _, _) :| triples)) =
      NECyclicSCC (n0 :| [n | (n, _, _) <- triples])

stronglyConnCompR
        :: Ord key
        => [(node, key, [key])]
        -> [SCC (node, key, [key])]     -- ^ Reverse topologically sorted

stronglyConnCompR [] = []  -- added to avoid creating empty array in graphFromEdges -- SOF
stronglyConnCompR edges0
  = map decode forest
  where
    (graph, vertex_fn,_) = graphFromEdges edges0
    forest             = scc graph

    decode (Node v []) | mentions_itself v = NECyclicSCC (vertex_fn v :| [])
                       | otherwise         = AcyclicSCC (vertex_fn v)
    decode (Node v ts) = NECyclicSCC (vertex_fn v :| foldr dec [] ts)

    dec (Node v ts) vs = vertex_fn v : foldr dec vs ts
    mentions_itself v = v `elem` (graph ! v)


type Vertex  = Int
type Table a = Array Vertex a
type Graph   = Array Vertex [Vertex]
type Bounds  = (Vertex, Vertex)
type Edge    = (Vertex, Vertex)

#if !USE_UNBOXED_ARRAYS
type UArray i a = Array i a
#endif

vertices :: Graph -> [Vertex]
vertices  = indices

edges    :: Graph -> [Edge]
edges g   = [ (v, w) | v <- vertices g, w <- g!v ]

buildG :: Bounds -> [Edge] -> Graph
buildG = accumArray (flip (:)) []

transposeG  :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]

outdegree :: Graph -> Array Vertex Int
outdegree  = fmap length

indegree :: Graph -> Array Vertex Int
indegree g = accumArray (+) 0 (bounds g) [(v, 1) | (_, outs) <- assocs g, v <- outs]

graphFromEdges'
        :: Ord key
        => [(node, key, [key])]
        -> (Graph, Vertex -> (node, key, [key]))
graphFromEdges' x = (a,b) where
    (a,b,_) = graphFromEdges x

graphFromEdges
        :: Ord key
        => [(node, key, [key])]
        -> (Graph, Vertex -> (node, key, [key]), key -> Maybe Vertex)
graphFromEdges edges0
  = (graph, \v -> vertex_map ! v, key_vertex)
  where
    max_v           = length edges0 - 1
    bounds0         = (0,max_v) :: (Vertex, Vertex)
    sorted_edges    = L.sortBy lt edges0
    edges1          = zipWith (,) [0..] sorted_edges

    graph           = array bounds0 [(,) v (mapMaybe key_vertex ks) | (,) v (_,    _, ks) <- edges1]
    key_map         = array bounds0 [(,) v k                       | (,) v (_,    k, _ ) <- edges1]
    vertex_map      = array bounds0 edges1

    (_,k1,_) `lt` (_,k2,_) = k1 `compare` k2

    key_vertex k   = findVertex 0 max_v
                   where
                     findVertex a b | a > b
                              = Nothing
                     findVertex a b = case compare k (key_map ! mid) of
                                   LT -> findVertex a (mid-1)
                                   EQ -> Just mid
                                   GT -> findVertex (mid+1) b
                              where
                                mid = a + (b - a) `div` 2


dff          :: Graph -> [Tree Vertex]
dff g         = dfs g (vertices g)


dfs :: Graph -> [Vertex] -> [Tree Vertex]
dfs !g vs0 = run (bounds g) $ \contains include ->
  let
    go [] = pure []
    go (v:vs) = do
      visited <- contains v
      if visited
      then go vs
      else do
        include v
        as <- go (g!v)
        bs <- go vs
        pure $ Node v as : bs
  in go vs0

#if USE_ST_MONAD


newArrayBool
  :: Bounds
#if USE_UNBOXED_ARRAYS
  -> ST s (STUArray s Vertex Bool)
#else
  -> ST s (STArray s Vertex Bool)
#endif
newArrayBool bnds = newArray bnds False

run
  :: Bounds
  -> (forall s. (Vertex -> ST s Bool) -> (Vertex -> ST s ()) -> ST s a)
  -> a
run bnds f = runST $ do
  m <- newArrayBool bnds
  f (readArray m) (\v -> writeArray m v True)

#else /* !USE_ST_MONAD */


newtype SetM a = SetM { runSetM :: IntSet -> (a, IntSet) }

instance Monad SetM where
    SetM v >>= f = SetM $ \s -> case v s of (x, s') -> runSetM (f x) s'

instance Functor SetM where
    f `fmap` SetM v = SetM $ \s -> case v s of (x, s') -> (f x, s')

instance Applicative SetM where
    pure x = SetM $ \s -> (x, s)
    SetM f <*> SetM v = SetM $ \s -> case f s of (k, s') -> case v s' of (x, s'') -> (k x, s'')

run :: Bounds -> ((Vertex -> SetM Bool) -> (Vertex -> SetM ()) -> SetM a) -> a
run _ f = fst (runSetM (f contains include) Set.empty)
  where
    contains v = SetM $ \m -> (Set.member v m, m)
    include v = SetM $ \m -> ((), Set.insert v m)

#endif /* !USE_ST_MONAD */



preorder' :: Tree a -> [a] -> [a]
preorder' (Node a ts) = (a :) . preorderF' ts

preorderF' :: [Tree a] -> [a] -> [a]
preorderF' ts = foldr (.) id $ map preorder' ts

preorderF :: [Tree a] -> [a]
preorderF ts = preorderF' ts []

tabulate        :: Bounds -> [Vertex] -> UArray Vertex Int
tabulate bnds vs = UA.array bnds (zipWith (flip (,)) [1..] vs)

preArr          :: Bounds -> [Tree Vertex] -> UArray Vertex Int
preArr bnds      = tabulate bnds . preorderF


postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF   :: [Tree a] -> [a] -> [a]
postorderF ts = foldr (.) id $ map postorder ts

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g) []

topSort      :: Graph -> [Vertex]
topSort       = reverse . postOrd

reverseTopSort :: Graph -> [Vertex]
reverseTopSort = postOrd


components   :: Graph -> [Tree Vertex]
components    = dff . undirected

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)



scc  :: Graph -> [Tree Vertex]
scc g = dfs g (reverse (postOrd (transposeG g)))


XXX unused code

tree              :: Bounds -> Forest Vertex -> Graph
tree bnds ts       = buildG bnds (concat (map flat ts))
 where flat (Node v ts') = [ (v, w) | Node w _us <- ts' ]
                        ++ concat (map flat ts')

back              :: Graph -> Table Int -> Graph
back g post        = mapT select g
 where select v ws = [ w | w <- ws, post!v < post!w ]

cross             :: Graph -> Table Int -> Table Int -> Graph
cross g pre post   = mapT select g
 where select v ws = [ w | w <- ws, post!v > post!w, pre!v > pre!w ]

forward           :: Graph -> Graph -> Table Int -> Graph
forward g tree' pre = mapT select g
 where select v ws = [ w | w <- ws, pre!v < pre!w ] \\ tree' ! v

mapT    :: (Vertex -> a -> b) -> Array Vertex a -> Array Vertex b
mapT f t = array (bounds t) [ (,) v (f v (t!v)) | v <- indices t ]


reachable :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

path :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)


bcc :: Graph -> [Tree [Vertex]]
bcc g = concatMap bicomps forest
  where

    forest = dff g

    dnum = preArr (bounds g) forest

    bicomps :: Tree Vertex -> [Tree [Vertex]]
    bicomps (Node v tws) =
      [Node (v : curw []) (donew []) | (_, curw, donew) <- map collect tws]

    collect :: Tree Vertex
            -> (Int, [Vertex] -> [Vertex], [Tree [Vertex]] -> [Tree [Vertex]])
    collect (Node v tws) = (lowv, (v:) . curv, donev)
      where
        dv = dnum UA.! v
        accf (lowv', curv', donev') tw
          | loww < dv  -- w's component extends through v
            = (lowv'', curv' . curw, donev' . donew)
          | otherwise  -- w's component ends with v as an articulation point
            = (lowv'', curv', donev' . (Node (v : curw []) (donew []) :))
          where
            (loww, curw, donew) = collect tw
            !lowv'' = min lowv' loww
        !lowv0 = F.foldl' min dv [dnum UA.! w | w <- g!v]
        !(lowv, curv, donev) = F.foldl' accf (lowv0, id, id) tws






module Data.ByteString (

        ByteString,
        StrictByteString,


        empty,
        singleton,
        pack,
        unpack,
        fromStrict,
        toStrict,
        fromFilePath,
        toFilePath,

        cons,
        snoc,
        append,
        head,
        uncons,
        unsnoc,
        last,
        tail,
        init,
        null,
        length,

        map,
        reverse,
        intersperse,
        intercalate,
        transpose,

        foldl,
        foldl',
        foldl1,
        foldl1',

        foldr,
        foldr',
        foldr1,
        foldr1',

        concat,
        concatMap,
        any,
        all,
        maximum,
        minimum,

        scanl,
        scanl1,
        scanr,
        scanr1,

        mapAccumL,
        mapAccumR,

        replicate,
        unfoldr,
        unfoldrN,


        take,
        takeEnd,
        drop,
        dropEnd,
        splitAt,
        takeWhile,
        takeWhileEnd,
        dropWhile,
        dropWhileEnd,
        span,
        spanEnd,
        break,
        breakEnd,
        group,
        groupBy,
        inits,
        tails,
        initsNE,
        tailsNE,
        stripPrefix,
        stripSuffix,

        split,
        splitWith,

        isPrefixOf,
        isSuffixOf,
        isInfixOf,

        isValidUtf8,

        breakSubstring,


        elem,
        notElem,

        find,
        filter,
        partition,

        index,
        indexMaybe,
        (!?),
        elemIndex,
        elemIndices,
        elemIndexEnd,
        findIndex,
        findIndices,
        findIndexEnd,
        count,

        zip,
        zipWith,
        packZipWith,
        unzip,

        sort,

        copy,

        packCString,
        packCStringLen,

        useAsCString,
        useAsCStringLen,


        getLine,
        getContents,
        putStr,
        interact,

        readFile,
        writeFile,
        appendFile,

        hGetLine,
        hGetContents,
        hGet,
        hGetSome,
        hGetNonBlocking,
        hPut,
        hPutNonBlocking,
        hPutStr,
  ) where

import qualified Prelude as P
import Prelude hiding           (reverse,head,tail,last,init,Foldable(..)
                                ,map,lines,unlines
                                ,concat,any,take,drop,splitAt,takeWhile
                                ,dropWhile,span,break,filter
                                ,all,concatMap
                                ,scanl,scanl1,scanr,scanr1
                                ,readFile,writeFile,appendFile,replicate
                                ,getContents,getLine,putStr,putStrLn,interact
                                ,zip,zipWith,unzip,notElem
                                )

import Data.Bits                (finiteBitSize, shiftL, (.|.), (.&.))

import Data.ByteString.Internal.Type
import Data.ByteString.Lazy.Internal (fromStrict, toStrict)
import Data.ByteString.Unsafe

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty(..))

import Data.Word                (Word8)

import Control.Exception        (IOException, catch, finally, assert, throwIO)
import Control.Monad            (when)

import Foreign.C.String         (CString, CStringLen)
import Foreign.ForeignPtr       (ForeignPtr, touchForeignPtr)
import Foreign.ForeignPtr.Unsafe(unsafeForeignPtrToPtr)
import Foreign.Marshal.Alloc    (allocaBytes)
import Foreign.Marshal.Array    (allocaArray)
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable         (Storable(..))

import System.IO                (stdin,stdout,hClose,hFileSize
                                ,hGetBuf,hPutBuf,hGetBufNonBlocking
                                ,hPutBufNonBlocking,withBinaryFile
                                ,IOMode(..),hGetBufSome)
import System.IO.Error          (mkIOError, illegalOperationErrorType)

import Data.IORef
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types
import GHC.IO.Buffer
import GHC.IO.BufferedIO as Buffered
import GHC.IO.Encoding          (getFileSystemEncoding)
import GHC.Foreign              (newCStringLen, peekCStringLen)
import GHC.Stack.Types          (HasCallStack)
import Data.Char                (ord)

import GHC.Base                 (build)
import GHC.Word hiding (Word8)


singleton :: Word8 -> ByteString
singleton c = unsafeTake 1 $ unsafeDrop (fromIntegral c) allBytes

allBytes :: ByteString
allBytes = unsafePackLenLiteral 0x100
  "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x3e\x3f\x40\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4a\x4b\x4c\x4d\x4e\x4f\x50\x51\x52\x53\x54\x55\x56\x57\x58\x59\x5a\x5b\x5c\x5d\x5e\x5f\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6a\x6b\x6c\x6d\x6e\x6f\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7a\x7b\x7c\x7d\x7e\x7f\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff"#

pack :: [Word8] -> ByteString
pack = packBytes

unpack :: ByteString -> [Word8]
unpack bs = build (unpackFoldr bs)

unpackFoldr :: ByteString -> (Word8 -> a -> a) -> a -> a
unpackFoldr bs k z = foldr k z bs

"ByteString unpack-list" [1]  forall bs .
    unpackFoldr bs (:) [] = unpackBytes bs

fromFilePath :: FilePath -> IO ByteString
fromFilePath path = do
    enc <- getFileSystemEncoding
    newCStringLen enc path >>= unsafePackMallocCStringLen

toFilePath :: ByteString -> IO FilePath
toFilePath path = do
    enc <- getFileSystemEncoding
    useAsCStringLen path (peekCStringLen enc)


null :: ByteString -> Bool
null (BS _ l) = assert (l >= 0) $ l <= 0

length :: ByteString -> Int
length (BS _ l) = assert (l >= 0) l


infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

cons :: Word8 -> ByteString -> ByteString
cons c (BS x len) = unsafeCreateFp (checkedAdd "cons" len 1) $ \p -> do
        pokeFp p c
        memcpyFp (p `plusForeignPtr` 1) x len

snoc :: ByteString -> Word8 -> ByteString
snoc (BS x len) c = unsafeCreateFp (checkedAdd "snoc" len 1) $ \p -> do
        memcpyFp p x len
        pokeFp (p `plusForeignPtr` len) c

head :: HasCallStack => ByteString -> Word8
head (BS x l)
    | l <= 0    = errorEmptyList "head"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> peek p

tail :: HasCallStack => ByteString -> ByteString
tail (BS p l)
    | l <= 0    = errorEmptyList "tail"
    | otherwise = BS (plusForeignPtr p 1) (l-1)

uncons :: ByteString -> Maybe (Word8, ByteString)
uncons (BS x l)
    | l <= 0    = Nothing
    | otherwise = Just (accursedUnutterablePerformIO $ unsafeWithForeignPtr x
                                                     $ \p -> peek p,
                        BS (plusForeignPtr x 1) (l-1))

last :: HasCallStack => ByteString -> Word8
last ps@(BS x l)
    | null ps   = errorEmptyList "last"
    | otherwise = accursedUnutterablePerformIO $
                    unsafeWithForeignPtr x $ \p -> peekByteOff p (l-1)

init :: HasCallStack => ByteString -> ByteString
init ps@(BS p l)
    | null ps   = errorEmptyList "init"
    | otherwise = BS p (l-1)

unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc (BS x l)
    | l <= 0    = Nothing
    | otherwise = Just (BS x (l-1),
                        accursedUnutterablePerformIO $
                          unsafeWithForeignPtr x $ \p -> peekByteOff p (l-1))

append :: ByteString -> ByteString -> ByteString
append = mappend


map :: (Word8 -> Word8) -> ByteString -> ByteString
map f (BS srcPtr len) = unsafeCreateFp len $ \dstPtr -> m srcPtr dstPtr
  where
    m !p1 !p2 = map_ 0
      where
      map_ :: Int -> IO ()
      map_ !n
         | n >= len = return ()
         | otherwise = do
              x <- peekFpByteOff p1 n
              pokeFpByteOff p2 n (f x)
              map_ (n+1)

reverse :: ByteString -> ByteString
reverse (BS x l) = unsafeCreateFp l $ \fp ->
  unsafeWithForeignPtr fp $ \p ->
    unsafeWithForeignPtr x  $ \f ->
      c_reverse p f (fromIntegral l)

intersperse :: Word8 -> ByteString -> ByteString
intersperse c ps@(BS x l)
    | length ps < 2  = ps
    | otherwise      = unsafeCreateFp (2*l-1) $ \fp ->
      unsafeWithForeignPtr fp $ \p ->
        unsafeWithForeignPtr x $ \f ->
          c_intersperse p f (fromIntegral l) c

transpose :: [ByteString] -> [ByteString]
transpose = P.map pack . List.transpose . P.map unpack


foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f z = \(BS fp len) ->
  let
    end = unsafeForeignPtrToPtr fp `plusPtr` (-1)
    go !p | p == end  = z
          | otherwise = let !x = accursedUnutterablePerformIO $ do
                                   x' <- peek p
                                   touchForeignPtr fp
                                   return x'
                        in f (go (p `plusPtr` (-1))) x

  in
    go (end `plusPtr` len)

Note [fold inlining]:

GHC will only inline a function marked INLINE
if it is fully saturated (meaning the number of
arguments provided at the call site is at least
equal to the number of lhs arguments).

foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f v = \(BS fp len) ->
  let
    g ptr = go v ptr
      where
        end  = ptr `plusForeignPtr` len
        go !z !p | p == end  = return z
                 | otherwise = do x <- peekFp p
                                  go (f z x) (p `plusForeignPtr` 1)
  in
    accursedUnutterablePerformIO $ g fp

foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k z = \(BS fp len) ->
  let
    ptr = unsafeForeignPtrToPtr fp
    end = ptr `plusPtr` len
    go !p | p == end  = z
          | otherwise = let !x = accursedUnutterablePerformIO $ do
                                   x' <- peek p
                                   touchForeignPtr fp
                                   return x'
                         in k x (go (p `plusPtr` 1))
  in
    go ptr

foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' k v = \(BS fp len) ->
  let
    g ptr = go v (end `plusForeignPtr` len)
      where
        end = ptr `plusForeignPtr` (-1)
        go !z !p | p == end  = return z
                 | otherwise = do x <- peekFp p
                                  go (k x z) (p `plusForeignPtr` (-1))
  in
    accursedUnutterablePerformIO $ g fp


foldl1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 f ps = case uncons ps of
  Nothing     -> errorEmptyList "foldl1"
  Just (h, t) -> foldl f h t

foldl1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' f ps = case uncons ps of
  Nothing     -> errorEmptyList "foldl1'"
  Just (h, t) -> foldl' f h t

foldr1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 f ps = case unsnoc ps of
  Nothing -> errorEmptyList "foldr1"
  Just (b, c) -> foldr f c b

foldr1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' f ps = case unsnoc ps of
  Nothing -> errorEmptyList "foldr1'"
  Just (b, c) -> foldr' f c b


concat :: [ByteString] -> ByteString
concat = mconcat

concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f = concat . foldr ((:) . f) []


any :: (Word8 -> Bool) -> ByteString -> Bool
any _ (BS _ 0)   = False
any f (BS x len) = accursedUnutterablePerformIO $ g x
  where
    g ptr = go ptr
      where
        end = ptr `plusForeignPtr` len
        go !p | p == end  = return False
              | otherwise = do c <- peekFp p
                               if f c then return True
                                      else go (p `plusForeignPtr` 1)

"ByteString specialise any (x ==)" forall x.
    any (x `eqWord8`) = anyByte x
"ByteString specialise any (== x)" forall x.
    any (`eqWord8` x) = anyByte x

anyByte :: Word8 -> ByteString -> Bool
anyByte c (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    q <- memchr p c (fromIntegral l)
    return $! q /= nullPtr

all :: (Word8 -> Bool) -> ByteString -> Bool
all _ (BS _ 0)   = True
all f (BS x len) = accursedUnutterablePerformIO $ g x
  where
    g ptr = go ptr
      where
        end = ptr `plusForeignPtr` len
        go !p | p == end  = return True  -- end of list
              | otherwise = do c <- peekFp p
                               if f c
                                  then go (p `plusForeignPtr` 1)
                                  else return False

"ByteString specialise all (x /=)" forall x.
    all (x `neWord8`) = not . anyByte x
"ByteString specialise all (/= x)" forall x.
    all (`neWord8` x) = not . anyByte x


maximum :: HasCallStack => ByteString -> Word8
maximum xs@(BS x l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
                      c_maximum p (fromIntegral l)

minimum :: HasCallStack => ByteString -> Word8
minimum xs@(BS x l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
                      c_minimum p (fromIntegral l)


mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f acc = \(BS a len) -> unsafeDupablePerformIO $ do
    gp   <- mallocByteString len
    let
      go src dst = mapAccumL_ acc 0
        where
          mapAccumL_ !s !n
             | n >= len = return s
             | otherwise = do
                  x <- peekFpByteOff src n
                  let (s', y) = f s x
                  pokeFpByteOff dst n y
                  mapAccumL_ s' (n+1)
    acc' <- go a gp
    return (acc', BS gp len)

mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f acc = \(BS a len) -> unsafeDupablePerformIO $ do
    gp   <- mallocByteString len
    let
      go src dst = mapAccumR_ acc (len-1)
        where
          mapAccumR_ !s (-1) = return s
          mapAccumR_ !s !n   = do
              x  <- peekFpByteOff src n
              let (s', y) = f s x
              pokeFpByteOff dst n y
              mapAccumR_ s' (n-1)
    acc' <- go a gp
    return (acc', BS gp len)


scanl
    :: (Word8 -> Word8 -> Word8)
    -> Word8
    -> ByteString
    -> ByteString
scanl f v = \(BS a len) -> unsafeCreateFp (checkedAdd "scanl" len 1) $ \q -> do
        pokeFp q v
        let
          go src dst = scanl_ v 0
            where
              scanl_ !z !n
                  | n >= len  = return ()
                  | otherwise = do
                      x <- peekFpByteOff src n
                      let z' = f z x
                      pokeFpByteOff dst n z'
                      scanl_ z' (n+1)
        go a (q `plusForeignPtr` 1)

scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 f ps = case uncons ps of
  Nothing     -> empty
  Just (h, t) -> scanl f h t

scanr
    :: (Word8 -> Word8 -> Word8)
    -> Word8
    -> ByteString
    -> ByteString
scanr f v = \(BS a len) -> unsafeCreateFp (checkedAdd "scanr" len 1) $ \b -> do
        pokeFpByteOff b len v
        let
          go p q = scanr_ v (len-1)
            where
              scanr_ !z !n
                  | n < 0     = return ()
                  | otherwise = do
                      x <- peekFpByteOff p n
                      let z' = f x z
                      pokeFpByteOff q n z'
                      scanr_ z' (n-1)
        go a b

scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 f ps = case unsnoc ps of
  Nothing -> empty
  Just (b, c) -> scanr f c b


replicate :: Int -> Word8 -> ByteString
replicate w c
    | w <= 0    = empty
    | otherwise = unsafeCreateFp w $ \fptr ->
        unsafeWithForeignPtr fptr $ \ptr ->
                      fillBytes ptr c w

unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f = concat . unfoldChunk 32 64
  where unfoldChunk n n' x =
          case unfoldrN n f x of
            (s, Nothing) -> [s]
            (s, Just x') -> s : unfoldChunk n' (n+n') x'

unfoldrN :: Int -> (a -> Maybe (Word8, a)) -> a -> (ByteString, Maybe a)
unfoldrN i f x0
    | i < 0     = (empty, Just x0)
    | otherwise = unsafeDupablePerformIO $ createFpAndTrim' i $ \p -> go p x0 0
  where
    go !p !x !n = go' x n
      where
        go' !x' !n'
          | n' == i    = return (0, n', Just x')
          | otherwise = case f x' of
                          Nothing      -> return (0, n', Nothing)
                          Just (w,x'') -> do pokeFpByteOff p n' w
                                             go' x'' (n'+1)


take :: Int -> ByteString -> ByteString
take n ps@(BS x l)
    | n <= 0    = empty
    | n >= l    = ps
    | otherwise = BS x n

takeEnd :: Int -> ByteString -> ByteString
takeEnd n ps@(BS x len)
  | n >= len  = ps
  | n <= 0    = empty
  | otherwise = BS (plusForeignPtr x (len - n)) n

drop  :: Int -> ByteString -> ByteString
drop n ps@(BS x l)
    | n <= 0    = ps
    | n >= l    = empty
    | otherwise = BS (plusForeignPtr x n) (l-n)

dropEnd :: Int -> ByteString -> ByteString
dropEnd n ps@(BS x len)
    | n <= 0    = ps
    | n >= len  = empty
    | otherwise = BS x (len - n)

splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt n ps@(BS x l)
    | n <= 0    = (empty, ps)
    | n >= l    = (ps, empty)
    | otherwise = (BS x n, BS (plusForeignPtr x n) (l-n))

takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f ps = unsafeTake (findIndexOrLength (not . f) ps) ps

"ByteString specialise takeWhile (x /=)" forall x.
    takeWhile (x `neWord8`) = fst . breakByte x
"ByteString specialise takeWhile (/= x)" forall x.
    takeWhile (`neWord8` x) = fst . breakByte x
"ByteString specialise takeWhile (x ==)" forall x.
    takeWhile (x `eqWord8`) = fst . spanByte x
"ByteString specialise takeWhile (== x)" forall x.
    takeWhile (`eqWord8` x) = fst . spanByte x

takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd f ps = unsafeDrop (findFromEndUntil (not . f) ps) ps

dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f ps = unsafeDrop (findIndexOrLength (not . f) ps) ps

"ByteString specialise dropWhile (x /=)" forall x.
    dropWhile (x `neWord8`) = snd . breakByte x
"ByteString specialise dropWhile (/= x)" forall x.
    dropWhile (`neWord8` x) = snd . breakByte x
"ByteString specialise dropWhile (x ==)" forall x.
    dropWhile (x `eqWord8`) = snd . spanByte x
"ByteString specialise dropWhile (== x)" forall x.
    dropWhile (`eqWord8` x) = snd . spanByte x

dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd f ps = unsafeTake (findFromEndUntil (not . f) ps) ps

break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break p ps = case findIndexOrLength p ps of n -> (unsafeTake n ps, unsafeDrop n ps)

"ByteString specialise break (x ==)" forall x.
    break (x `eqWord8`) = breakByte x
"ByteString specialise break (== x)" forall x.
    break (`eqWord8` x) = breakByte x


breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c p = case elemIndex c p of
    Nothing -> (p,empty)
    Just n  -> (unsafeTake n p, unsafeDrop n p)

breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd  p ps = splitAt (findFromEndUntil p ps) ps

span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)

spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
spanByte c ps@(BS x l) =
    accursedUnutterablePerformIO $  unsafeWithForeignPtr x g
  where
    g p = go 0
      where
        go !i | i >= l    = return (ps, empty)
              | otherwise = do c' <- peekByteOff p i
                               if c /= c'
                                   then return (unsafeTake i ps, unsafeDrop i ps)
                                   else go (i+1)

"ByteString specialise span (x ==)" forall x.
    span (x `eqWord8`) = spanByte x
"ByteString specialise span (== x)" forall x.
    span (`eqWord8` x) = spanByte x

spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd  p ps = splitAt (findFromEndUntil (not.p) ps) ps

splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith _ (BS _  0) = []
splitWith predicate (BS fp len) = splitWith0 0 len fp
  where splitWith0 !off' !len' !fp' =
          accursedUnutterablePerformIO $
              splitLoop fp 0 off' len' fp'

        splitLoop :: ForeignPtr Word8
                  -> Int -> Int -> Int
                  -> ForeignPtr Word8
                  -> IO [ByteString]
        splitLoop p idx2 off' len' fp' = go idx2
          where
            go idx'
                | idx' >= len'  = return [BS (plusForeignPtr fp' off') idx']
                | otherwise = do
                    w <- peekFpByteOff p (off'+idx')
                    if predicate w
                       then return (BS (plusForeignPtr fp' off') idx' :
                                  splitWith0 (off'+idx'+1) (len'-idx'-1) fp')
                       else go (idx'+1)

split :: Word8 -> ByteString -> [ByteString]
split _ (BS _ 0) = []
split w (BS x l) = loop 0
    where
        loop !n =
            let q = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
                      memchr (p `plusPtr` n)
                             w (fromIntegral (l-n))
            in if q == nullPtr
                then [BS (plusForeignPtr x n) (l-n)]
                else let i = q `minusPtr` unsafeForeignPtrToPtr x
                      in BS (plusForeignPtr x n) (i-n) : loop (i+1)



group :: ByteString -> [ByteString]
group xs = case uncons xs of
  Nothing     -> []
  Just (h, _) -> ys : group zs
    where
        (ys, zs) = spanByte h xs

groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy k xs = case uncons xs of
  Nothing     -> []
  Just (h, t) -> unsafeTake n xs : groupBy k (unsafeDrop n xs)
    where
        n = 1 + findIndexOrLength (not . k h) t

intercalate :: ByteString -> [ByteString] -> ByteString
intercalate _ [] = mempty
intercalate _ [x] = x -- This branch exists for laziness, not speed
intercalate (BS sepPtr sepLen) (BS hPtr hLen : t) =
  unsafeCreateFp totalLen $ \dstPtr0 -> do
      memcpyFp dstPtr0 hPtr hLen
      let go _ [] = pure ()
          go dstPtr (BS chunkPtr chunkLen : chunks) = do
            memcpyFp dstPtr sepPtr sepLen
            let destPtr' = dstPtr `plusForeignPtr` sepLen
            memcpyFp destPtr' chunkPtr chunkLen
            go (destPtr' `plusForeignPtr` chunkLen) chunks
      go (dstPtr0 `plusForeignPtr` hLen) t
  where
  totalLen = List.foldl' (\acc chunk -> acc +! sepLen +! length chunk) hLen t
  (+!) = checkedAdd "intercalate"


index :: HasCallStack => ByteString -> Int -> Word8
index ps n
    | n < 0          = moduleError "index" ("negative index: " ++ show n)
    | n >= length ps = moduleError "index" ("index too large: " ++ show n
                                         ++ ", length = " ++ show (length ps))
    | otherwise      = ps `unsafeIndex` n

indexMaybe :: ByteString -> Int -> Maybe Word8
indexMaybe ps n
    | n < 0          = Nothing
    | n >= length ps = Nothing
    | otherwise      = Just $! ps `unsafeIndex` n

(!?) :: ByteString -> Int -> Maybe Word8
(!?) = indexMaybe

elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex c (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    q <- memchr p c (fromIntegral l)
    return $! if q == nullPtr then Nothing else Just $! q `minusPtr` p

elemIndexEnd :: Word8 -> ByteString -> Maybe Int
elemIndexEnd = findIndexEnd . (==)

elemIndices :: Word8 -> ByteString -> [Int]
elemIndices w (BS x l) = loop 0
    where
        loop !n = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
            q <- memchr (p `plusPtr` n) w (fromIntegral (l - n))
            if q == nullPtr
                then return []
                else let !i = q `minusPtr` p
                      in return $ i : loop (i + 1)

count :: Word8 -> ByteString -> Int
count w (BS x m) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
    fromIntegral <$> c_count p (fromIntegral m) w

findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndex k (BS x l) = accursedUnutterablePerformIO $ g x
  where
    g !ptr = go 0
      where
        go !n | n >= l    = return Nothing
              | otherwise = do w <- peekFp $ ptr `plusForeignPtr` n
                               if k w
                                 then return (Just n)
                                 else go (n+1)

findIndexEnd :: (Word8 -> Bool) -> ByteString -> Maybe Int
findIndexEnd k (BS x l) = accursedUnutterablePerformIO $ g x
  where
    g !ptr = go (l-1)
      where
        go !n | n < 0     = return Nothing
              | otherwise = do w <- peekFpByteOff ptr n
                               if k w
                                 then return (Just n)
                                 else go (n-1)

findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
findIndices p = loop 0
   where
     loop !n !qs = case findIndex p qs of
                     Just !i ->
                        let !j = n+i
                         in j : loop (j+1) (unsafeDrop (i+1) qs)
                     Nothing -> []


"ByteString specialise findIndex (x ==)" forall x. findIndex (x`eqWord8`) = elemIndex x
"ByteString specialise findIndex (== x)" forall x. findIndex (`eqWord8`x) = elemIndex x
"ByteString specialise findIndices (x ==)" forall x. findIndices (x`eqWord8`) = elemIndices x
"ByteString specialise findIndices (== x)" forall x. findIndices (`eqWord8`x) = elemIndices x


elem :: Word8 -> ByteString -> Bool
elem c ps = case elemIndex c ps of Nothing -> False ; _ -> True

notElem :: Word8 -> ByteString -> Bool
notElem c ps = not (c `elem` ps)

filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter k = \ps@(BS pIn l) ->
  if null ps
    then ps
    else
      unsafeDupablePerformIO $ createFpAndTrim l $ \pOut -> do
        let
          go' pf pt = go pf pt
            where
              end = pf `plusForeignPtr` l
              go !f !t | f == end  = return t
                       | otherwise = do
                           w <- peekFp f
                           if k w
                             then pokeFp t w
                               >> go (f `plusForeignPtr` 1) (t `plusForeignPtr` 1)
                             else go (f `plusForeignPtr` 1) t
        t <- go' pIn pOut
        return $! t `minusForeignPtr` pOut -- actual length

filterByte :: Word8 -> ByteString -> ByteString
filterByte w ps = replicate (count w ps) w

"ByteString specialise filter (== x)" forall x.
    filter ((==) x) = filterByte x
"ByteString specialise filter (== x)" forall x.
    filter (== x) = filterByte x

find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f p = case findIndex f p of
                    Just n -> Just (p `unsafeIndex` n)
                    _      -> Nothing

partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition f s = unsafeDupablePerformIO $
    do        p <- mallocByteString len
              let end = p `plusForeignPtr` (len - 1)
              mid <- sep 0 p end
              rev mid end
              let i = mid `minusForeignPtr` p
              return (BS p i,
                      BS (p `plusForeignPtr` i) (len - i))
  where
    len  = length s
    incr = (`plusForeignPtr` 1)
    decr = (`plusForeignPtr` (-1))

    sep !i !p1 !p2
       | i == len  = return p1
       | f w       = do pokeFp p1 w
                        sep (i + 1) (incr p1) p2
       | otherwise = do pokeFp p2 w
                        sep (i + 1) p1 (decr p2)
      where
        w = s `unsafeIndex` i

    rev !p1 !p2 -- fixme: surely there are faster ways to do this
      | p1 >= p2  = return ()
      | otherwise = do a <- peekFp p1
                       b <- peekFp p2
                       pokeFp p1 b
                       pokeFp p2 a
                       rev (incr p1) (decr p2)


isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf (BS x1 l1) (BS x2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x1 $ \p1 ->
        unsafeWithForeignPtr x2 $ \p2 -> do
            i <- memcmp p1 p2 (fromIntegral l1)
            return $! i == 0

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix bs1@(BS _ l1) bs2
   | bs1 `isPrefixOf` bs2 = Just (unsafeDrop l1 bs2)
   | otherwise = Nothing

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf (BS x1 l1) (BS x2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x1 $ \p1 ->
        unsafeWithForeignPtr x2 $ \p2 -> do
            i <- memcmp p1 (p2 `plusPtr` (l2 - l1)) (fromIntegral l1)
            return $! i == 0

stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix bs1@(BS _ l1) bs2@(BS _ l2)
   | bs1 `isSuffixOf` bs2 = Just (unsafeTake (l2 - l1) bs2)
   | otherwise = Nothing

isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf p s = null p || not (null $ snd $ breakSubstring p s)

isValidUtf8 :: ByteString -> Bool
isValidUtf8 (BS ptr len) = accursedUnutterablePerformIO $ unsafeWithForeignPtr ptr $ \p -> do
  i <- if len < 1000000
     then cIsValidUtf8 p (fromIntegral len)
     else cIsValidUtf8Safe p (fromIntegral len)
  pure $ i /= 0

breakSubstring :: ByteString -- ^ String to search for
               -> ByteString -- ^ String to search in
               -> (ByteString,ByteString) -- ^ Head and tail of string broken at substring
breakSubstring pat =
  case lp of
    0 -> (empty,)
    1 -> breakByte (unsafeHead pat)
    _ -> if lp * 8 <= finiteBitSize (0 :: Word)
             then shift
             else karpRabin
  where
    unsafeSplitAt i s = (unsafeTake i s, unsafeDrop i s)
    lp                = length pat
    karpRabin :: ByteString -> (ByteString, ByteString)
    karpRabin src
        | length src < lp = (src,empty)
        | otherwise = search (rollingHash $ unsafeTake lp src) lp
      where
        k           = 2891336453 :: Word32
        rollingHash = foldl' (\h b -> h * k + fromIntegral b) 0
        hp          = rollingHash pat
        m           = k ^ lp
        get = fromIntegral . unsafeIndex src
        search !hs !i
            | hp == hs && pat == unsafeTake lp b = u
            | length src <= i                    = (src,empty) -- not found
            | otherwise                          = search hs' (i + 1)
          where
            u@(_, b) = unsafeSplitAt (i - lp) src
            hs' = hs * k +
                  get i -
                  m * get (i - lp)

    shift :: ByteString -> (ByteString, ByteString)
    shift !src
        | length src < lp = (src,empty)
        | otherwise       = search (intoWord $ unsafeTake lp src) lp
      where
        intoWord :: ByteString -> Word
        intoWord = foldl' (\w b -> (w `shiftL` 8) .|. fromIntegral b) 0
        wp   = intoWord pat
        mask = (1 `shiftL` (8 * lp)) - 1
        search !w !i
            | w == wp         = unsafeSplitAt (i - lp) src
            | length src <= i = (src, empty)
            | otherwise       = search w' (i + 1)
          where
            b  = fromIntegral (unsafeIndex src i)
            w' = mask .&. ((w `shiftL` 8) .|. b)


zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip ps qs = case uncons ps of
  Nothing         -> []
  Just (psH, psT) -> case uncons qs of
    Nothing         -> []
    Just (qsH, qsT) -> (psH, qsH) : zip psT qsT

zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith f ps qs = case uncons ps of
  Nothing         -> []
  Just (psH, psT) -> case uncons qs of
    Nothing         -> []
    Just (qsH, qsT) -> f psH qsH : zipWith f psT qsT

packZipWith :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
packZipWith f (BS a l) (BS b m) = unsafeDupablePerformIO $
    createFp len $ go a b
  where
    go p1 p2 = zipWith_ 0
      where
        zipWith_ :: Int -> ForeignPtr Word8 -> IO ()
        zipWith_ !n !r
           | n >= len = return ()
           | otherwise = do
                x <- peekFpByteOff p1 n
                y <- peekFpByteOff p2 n
                pokeFpByteOff r n (f x y)
                zipWith_ (n+1) r

    len = min l m

unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))


inits :: ByteString -> [ByteString]
inits bs = NE.toList $! initsNE bs

initsNE :: ByteString -> NonEmpty ByteString
initsNE (BS x len) = empty :| [BS x n | n <- [1..len]]

tails :: ByteString -> [ByteString]
tails bs = NE.toList $! tailsNE bs

tailsNE :: ByteString -> NonEmpty ByteString
tailsNE p | null p    = empty :| []
          | otherwise = p :| tails (unsafeTail p)


Note [Avoid NonEmpty combinators]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

As of base-4.18, most of the NonEmpty API is surprisingly lazy.
Using it without forcing the arguments yourself is just begging GHC
to make your code waste time allocating useless selector thunks.
This may change in the future. See also this CLC issue:
  https://github.com/haskell/core-libraries-committee/issues/107
But until then, "refactor" with care!

(Even for uses of NonEmpty near lazy ByteStrings, we don't want
the extra laziness of the NonEmpty API.)




sort :: ByteString -> ByteString
sort (BS input l)
  | l <= 20 = unsafeCreateFp l $ \destFP -> do
    memcpyFp destFP input l
    unsafeWithForeignPtr destFP $ \dest -> c_sort dest (fromIntegral l)
  | otherwise = unsafeCreateFp l $ \p -> allocaArray 256 $ \arr -> do

    fillBytes (castPtr arr) 0 (256 * sizeOf (undefined :: Int))
    unsafeWithForeignPtr input (\x -> countOccurrences arr x l)

    let go 256 !_   = return ()
        go i   !ptr = do n <- peekElemOff arr i
                         when (n /= 0) $
                           fillBytes ptr (fromIntegral @Int @Word8 i) n
                         go (i + 1) (ptr `plusPtr` fromIntegral n)
    unsafeWithForeignPtr p (go 0)
  where
    countOccurrences :: Ptr Int -> Ptr Word8 -> Int -> IO ()
    countOccurrences !counts !str !len = go 0
     where
        go !i | i == len    = return ()
              | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                               x <- peekElemOff counts k
                               pokeElemOff counts k (x + 1)
                               go (i + 1)



useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString (BS fp l) action =
  allocaBytes (l+1) $ \buf -> do
    unsafeWithForeignPtr fp $ \p -> copyBytes buf p l
    pokeByteOff buf l (0::Word8)
    action (castPtr buf)

useAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen p@(BS _ l) f = useAsCString p $ \cstr -> f (cstr,l)


packCString :: CString -> IO ByteString
packCString cstr = do
    len <- c_strlen cstr
    packCStringLen (cstr, fromIntegral len)

packCStringLen :: CStringLen -> IO ByteString
packCStringLen (cstr, len) | len >= 0 = createFp len $ \fp ->
    unsafeWithForeignPtr fp $ \p -> copyBytes p (castPtr cstr) len
packCStringLen (_, len) =
    moduleErrorIO "packCStringLen" ("negative length: " ++ show len)


copy :: ByteString -> ByteString
copy (BS x l) = unsafeCreateFp l $ \p -> memcpyFp p x l


getLine :: IO ByteString
getLine = hGetLine stdin

     "Deprecated since @bytestring-0.12@. Use 'Data.ByteString.Char8.getLine' instead. (Functions that rely on ASCII encodings belong in \"Data.ByteString.Char8\")"

hGetLine :: Handle -> IO ByteString
hGetLine h =
  wantReadableHandle_ "Data.ByteString.hGetLine" h $
    \ h_@Handle__{haByteBuffer} -> do
      flushCharReadBuffer h_
      buf <- readIORef haByteBuffer
      if isEmptyBuffer buf
         then fill h_ buf 0 []
         else haveBuf h_ buf 0 []
 where

  fill h_@Handle__{haByteBuffer,haDevice} buf !len xss = do
    (r,buf') <- Buffered.fillReadBuffer haDevice buf
    if r == 0
       then do writeIORef haByteBuffer buf{ bufR=0, bufL=0 }
               if len > 0
                  then mkBigPS len xss
                  else ioe_EOF
       else haveBuf h_ buf' len xss

  haveBuf h_@Handle__{haByteBuffer}
          buf@Buffer{ bufRaw=raw, bufR=w, bufL=r }
          len xss =
    do
        off <- findEOL r w raw
        let new_len = len + off - r
        xs <- mkPS raw r off

        if off /= w
            then do if w == off + 1
                            then writeIORef haByteBuffer buf{ bufL=0, bufR=0 }
                            else writeIORef haByteBuffer buf{ bufL = off + 1 }
                    mkBigPS new_len (xs:xss)
            else fill h_ buf{ bufL=0, bufR=0 } new_len (xs:xss)

  findEOL r w raw
        | r == w = return w
        | otherwise =  do
            c <- readWord8Buf raw r
            if c == fromIntegral (ord '\n')
                then return r -- NB. not r+1: don't include the '\n'
                else findEOL (r+1) w raw

     "Deprecated since @bytestring-0.12@. Use 'Data.ByteString.Char8.hGetLine' instead. (Functions that rely on ASCII encodings belong in \"Data.ByteString.Char8\")"

mkPS :: RawBuffer Word8 -> Int -> Int -> IO ByteString
mkPS buf start end =
 createFp len $ \fp -> memcpyFp fp (buf `plusForeignPtr` start) len
 where
   len = end - start

mkBigPS :: Int -> [ByteString] -> IO ByteString
mkBigPS _ [ps] = return ps
mkBigPS _ pss = return $! concat (P.reverse pss)


hPut :: Handle -> ByteString -> IO ()
hPut _ (BS _  0) = return ()
hPut h (BS ps l) = unsafeWithForeignPtr ps $ \p-> hPutBuf h p l

hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking h bs@(BS ps l) = do
  bytesWritten <- unsafeWithForeignPtr ps $ \p-> hPutBufNonBlocking h p l
  return $! drop bytesWritten bs

hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

putStr :: ByteString -> IO ()
putStr = hPut stdout


hGet :: Handle -> Int -> IO ByteString
hGet h i
    | i >  0    = createFpAndTrim i $ \fp ->
        unsafeWithForeignPtr fp $ \p -> hGetBuf h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGet" i

hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking h i
    | i >  0    = createFpAndTrim i $ \fp ->
        unsafeWithForeignPtr fp $ \p -> hGetBufNonBlocking h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGetNonBlocking" i

hGetSome :: Handle -> Int -> IO ByteString
hGetSome hh i
    | i >  0    = createFpAndTrim i $ \fp ->
        unsafeWithForeignPtr fp $ \p -> hGetBufSome hh p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize hh "hGetSome" i

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []


hGetContents :: Handle -> IO ByteString
hGetContents hnd = do
    bs <- hGetContentsSizeHint hnd 1024 2048
            `finally` hClose hnd
    if length bs < 900
      then return $! copy bs
      else return bs

hGetContentsSizeHint :: Handle
                     -> Int -- ^ first read size
                     -> Int -- ^ initial buffer size increment
                     -> IO ByteString
hGetContentsSizeHint hnd =
    readChunks []
  where
    readChunks chunks sz sz' = do
      fp        <- mallocByteString sz
      readcount <- unsafeWithForeignPtr fp $ \buf -> hGetBuf hnd buf sz
      let chunk = BS fp readcount
      if readcount < sz && sz > 0
        then return $! concat (P.reverse (chunk : chunks))
        else readChunks (chunk : chunks) sz' ((sz+sz') `min` 32752)

getContents :: IO ByteString
getContents = hGetContents stdin

interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents

readFile :: FilePath -> IO ByteString
readFile f =
    withBinaryFile f ReadMode $ \h -> do
      filesz <- catch (hFileSize h) useZeroIfNotRegularFile
      let readsz = (fromIntegral filesz `max` 0) + 1
      hGetContentsSizeHint h readsz (readsz `max` 255)
  where
    useZeroIfNotRegularFile :: IOException -> IO Integer
    useZeroIfNotRegularFile _ = return 0

modifyFile :: IOMode -> FilePath -> ByteString -> IO ()
modifyFile mode f txt = withBinaryFile f mode (`hPut` txt)

writeFile :: FilePath -> ByteString -> IO ()
writeFile = modifyFile WriteMode

appendFile :: FilePath -> ByteString -> IO ()
appendFile = modifyFile AppendMode


errorEmptyList :: HasCallStack => String -> a
errorEmptyList fun = moduleError fun "empty ByteString"

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)

moduleErrorIO :: HasCallStack => String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.ByteString." ++ fun ++ ':':' ':msg

findFromEndUntil :: (Word8 -> Bool) -> ByteString -> Int
findFromEndUntil f ps@(BS _ l) = case unsnoc ps of
  Nothing     -> 0
  Just (b, c) ->
    if f c
      then l
      else findFromEndUntil f b


module Data.ByteString.Internal (

        ByteString
        ( BS
        , PS -- backwards compatibility shim
        ),

        StrictByteString,

        findIndexOrLength,

        packBytes, packUptoLenBytes, unsafePackLenBytes,
        packChars, packUptoLenChars, unsafePackLenChars,
        unpackBytes, unpackAppendBytesLazy, unpackAppendBytesStrict,
        unpackChars, unpackAppendCharsLazy, unpackAppendCharsStrict,
        unsafePackAddress, unsafePackLenAddress,
        unsafePackLiteral, unsafePackLenLiteral,

        empty,
        create,
        createUptoN,
        createUptoN',
        createAndTrim,
        createAndTrim',
        unsafeCreate,
        unsafeCreateUptoN,
        unsafeCreateUptoN',
        mallocByteString,

        mkDeferredByteString,
        fromForeignPtr,
        toForeignPtr,
        fromForeignPtr0,
        toForeignPtr0,

        nullForeignPtr,
        deferForeignPtrAvailability,
        SizeOverflowException,
        overflowError,
        checkedAdd,
        checkedMultiply,

        c_strlen,
        c_free_finalizer,

        memchr,
        memcmp,
        memcpy,
        memset,

        c_reverse,
        c_intersperse,
        c_maximum,
        c_minimum,
        c_count,
        c_sort,

        w2c, c2w, isSpaceWord8, isSpaceChar8,

        accursedUnutterablePerformIO,

        plusForeignPtr,
        unsafeWithForeignPtr
  ) where

import Data.ByteString.Internal.Type




module Data.ByteString.Lazy (

        ByteString,
        LazyByteString,

        empty,
        singleton,
        pack,
        unpack,
        fromStrict,
        toStrict,
        fromChunks,
        toChunks,
        foldrChunks,
        foldlChunks,

        cons,
        cons',
        snoc,
        append,
        head,
        uncons,
        unsnoc,
        last,
        tail,
        init,
        null,
        length,

        map,
        reverse,
        intersperse,
        intercalate,
        transpose,

        foldl,
        foldl',
        foldl1,
        foldl1',
        foldr,
        foldr',
        foldr1,
        foldr1',

        concat,
        concatMap,
        any,
        all,
        maximum,
        minimum,
        compareLength,

        scanl,
        scanl1,
        scanr,
        scanr1,

        mapAccumL,
        mapAccumR,

        repeat,
        replicate,
        cycle,
        iterate,

        unfoldr,


        take,
        takeEnd,
        drop,
        dropEnd,
        splitAt,
        takeWhile,
        takeWhileEnd,
        dropWhile,
        dropWhileEnd,
        span,
        spanEnd,
        break,
        breakEnd,
        group,
        groupBy,
        inits,
        tails,
        initsNE,
        tailsNE,
        stripPrefix,
        stripSuffix,

        split,
        splitWith,

        isPrefixOf,
        isSuffixOf,



        elem,
        notElem,

        find,
        filter,
        partition,

        index,
        indexMaybe,
        (!?),
        elemIndex,
        elemIndexEnd,
        elemIndices,
        findIndex,
        findIndexEnd,
        findIndices,
        count,

        zip,
        zipWith,
        packZipWith,
        unzip,


        copy,


        getContents,
        putStr,
        interact,

        readFile,
        writeFile,
        appendFile,

        hGetContents,
        hGet,
        hGetNonBlocking,
        hPut,
        hPutNonBlocking,
        hPutStr,

  ) where

import Prelude hiding
    (reverse,head,tail,last,init,Foldable(..),map,lines,unlines
    ,concat,any,take,drop,splitAt,takeWhile,dropWhile,span,break,filter
    ,all,concatMap,scanl, scanl1, scanr, scanr1
    ,repeat, cycle, interact, iterate,readFile,writeFile,appendFile,replicate
    ,getContents,getLine,putStr,putStrLn ,zip,zipWith,unzip,notElem)

import qualified Data.List              as List
import qualified Data.List.NonEmpty     as NE
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Bifunctor         as BF
import qualified Data.ByteString        as P  (ByteString) -- type name only
import qualified Data.ByteString        as S  -- S for strict (hmm...)
import qualified Data.ByteString.Internal.Type as S
import qualified Data.ByteString.Unsafe as S
import Data.ByteString.Lazy.Internal

import Control.Exception        (assert)
import Control.Monad            (mplus)
import Data.Word                (Word8)
import Data.Int                 (Int64)
import GHC.Stack.Types          (HasCallStack)
import System.IO                (Handle,openBinaryFile,stdin,stdout,withBinaryFile,IOMode(..)
                                ,hClose)
import System.IO.Error          (mkIOError, illegalOperationErrorType)
import System.IO.Unsafe

import Foreign.Ptr
import Foreign.Storable



empty :: ByteString
empty = Empty

singleton :: Word8 -> ByteString
singleton w = Chunk (S.singleton w) Empty

pack :: [Word8] -> ByteString
pack = packBytes

unpack :: ByteString -> [Word8]
unpack = unpackBytes

fromChunks :: [S.StrictByteString] -> LazyByteString
fromChunks = List.foldr chunk Empty

toChunks :: LazyByteString -> [S.StrictByteString]
toChunks = foldrChunks (:) []


packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = LPS $ L.map (P.packWith k) (chunk defaultChunkSize str)

unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith k (LPS ss) = L.concatMap (S.unpackWith k) ss


null :: ByteString -> Bool
null Empty = True
null _     = False

length :: ByteString -> Int64
length = foldlChunks (\n c -> n + fromIntegral (S.length c)) 0

infixr 5 `cons`, `cons'` --same as list (:)
infixl 5 `snoc`

cons :: Word8 -> ByteString -> ByteString
cons c = Chunk (S.singleton c)

cons' :: Word8 -> ByteString -> ByteString
cons' w (Chunk c cs) | S.length c < 16 = Chunk (S.cons w c) cs
cons' w cs                             = Chunk (S.singleton w) cs

snoc :: ByteString -> Word8 -> ByteString
snoc cs w = foldrChunks Chunk (singleton w) cs

head :: HasCallStack => ByteString -> Word8
head Empty       = errorEmptyList "head"
head (Chunk c _) = S.unsafeHead c

uncons :: ByteString -> Maybe (Word8, ByteString)
uncons Empty = Nothing
uncons (Chunk c cs) = case S.length c of
  1 -> Just (S.unsafeHead c, cs)
  _ -> Just (S.unsafeHead c, Chunk (S.unsafeTail c) cs)

tail :: HasCallStack => ByteString -> ByteString
tail Empty          = errorEmptyList "tail"
tail (Chunk c cs)
  | S.length c == 1 = cs
  | otherwise       = Chunk (S.unsafeTail c) cs

last :: HasCallStack => ByteString -> Word8
last Empty          = errorEmptyList "last"
last (Chunk c0 cs0) = go c0 cs0
  where go c Empty        = S.unsafeLast c
        go _ (Chunk c cs) = go c cs

init :: HasCallStack => ByteString -> ByteString
init Empty          = errorEmptyList "init"
init (Chunk c0 cs0) = go c0 cs0
  where go c Empty | S.length c == 1 = Empty
                   | otherwise       = Chunk (S.unsafeInit c) Empty
        go c (Chunk c' cs)           = Chunk c (go c' cs)

unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc Empty        = Nothing
unsnoc (Chunk c cs) = Just (init (Chunk c cs), last (Chunk c cs))

append :: ByteString -> ByteString -> ByteString
append = mappend


map :: (Word8 -> Word8) -> ByteString -> ByteString
map f = go
    where
        go Empty        = Empty
        go (Chunk x xs) = Chunk y ys
            where
                y  = S.map f x
                ys = go xs

reverse :: ByteString -> ByteString
reverse = rev Empty
  where rev a Empty        = a
        rev a (Chunk c cs) = rev (Chunk (S.reverse c) a) cs

intersperse :: Word8 -> ByteString -> ByteString
intersperse _ Empty        = Empty
intersperse w (Chunk c cs) = Chunk (S.intersperse w c)
                                   (foldrChunks (Chunk . intersperse') Empty cs)
  where intersperse' :: P.ByteString -> P.ByteString
        intersperse' (S.BS fp l) =
          S.unsafeCreateFp (2*l) $ \fp' ->
            S.unsafeWithForeignPtr fp' $ \p' ->
              S.unsafeWithForeignPtr fp $ \p -> do
                poke p' w
                S.c_intersperse (p' `plusPtr` 1) p (fromIntegral l) w

transpose :: [ByteString] -> [ByteString]
transpose css = List.map (\ss -> Chunk (S.pack ss) Empty)
                      (List.transpose (List.map unpack css))


foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f = go
  where go a Empty        = a
        go a (Chunk c cs) = go (S.foldl f a c) cs

foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f = go
  where go !a Empty        = a
        go !a (Chunk c cs) = go (S.foldl' f a c) cs

foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k = foldrChunks (flip (S.foldr k))

foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' f a = go
  where
    go Empty = a
    go (Chunk c cs) = S.foldr' f (foldr' f a cs) c

foldl1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 _ Empty        = errorEmptyList "foldl1"
foldl1 f (Chunk c cs) = go (S.unsafeHead c) (S.unsafeTail c) cs
  where
    go v x xs = let v' = S.foldl f v x
      in case xs of
      Empty -> v'
      Chunk x' xs' -> go v' x' xs'

foldl1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' _ Empty        = errorEmptyList "foldl1'"
foldl1' f (Chunk c cs) = go (S.unsafeHead c) (S.unsafeTail c) cs
  where
    go !v x xs = let v' = S.foldl' f v x
      in case xs of
      Empty -> v'
      Chunk x' xs' -> go v' x' xs'

foldr1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 _ Empty          = errorEmptyList "foldr1"
foldr1 f (Chunk c0 cs0) = go c0 cs0
  where go c Empty         = S.foldr1 f c
        go c (Chunk c' cs) = S.foldr  f (go c' cs) c

foldr1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' _ Empty          = errorEmptyList "foldr1'"
foldr1' f (Chunk c0 cs0) = go c0 cs0
  where go c Empty         = S.foldr1' f c
        go c (Chunk c' cs) = S.foldr'  f (go c' cs) c


concat :: [ByteString] -> ByteString
concat = mconcat

concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap _ Empty        = Empty
concatMap f (Chunk c0 cs0) = to c0 cs0
  where
    go :: ByteString -> P.ByteString -> ByteString -> ByteString
    go Empty        c' cs' = to c' cs'
    go (Chunk c cs) c' cs' = Chunk c (go cs c' cs')

    to :: P.ByteString -> ByteString -> ByteString
    to c cs | S.null c  = case cs of
        Empty          -> Empty
        (Chunk c' cs') -> to c' cs'
            | otherwise = go (f (S.unsafeHead c)) (S.unsafeTail c) cs

any :: (Word8 -> Bool) -> ByteString -> Bool
any f = foldrChunks (\c rest -> S.any f c || rest) False

all :: (Word8 -> Bool) -> ByteString -> Bool
all f = foldrChunks (\c rest -> S.all f c && rest) True

maximum :: HasCallStack => ByteString -> Word8
maximum Empty        = errorEmptyList "maximum"
maximum (Chunk c cs) = foldlChunks (\n c' -> n `max` S.maximum c')
                                   (S.maximum c) cs

minimum :: HasCallStack => ByteString -> Word8
minimum Empty        = errorEmptyList "minimum"
minimum (Chunk c cs) = foldlChunks (\n c' -> n `min` S.minimum c')
                                     (S.minimum c) cs

compareLength :: ByteString -> Int64 -> Ordering
compareLength _ toCmp | toCmp < 0 = GT
compareLength Empty toCmp         = compare 0 toCmp
compareLength (Chunk c cs) toCmp  = compareLength cs (toCmp - fromIntegral (S.length c))

"ByteString.Lazy length/compareN -> compareLength" [~1] forall t n.
  compare (length t) n = compareLength t n
"ByteString.Lazy compareN/length -> compareLength" [~1] forall t n.
  compare n (length t) = compare EQ $ compareLength t n
"ByteString.Lazy length/==N -> compareLength/==EQ" [~1] forall t n.
   length t == n = compareLength t n == EQ
"ByteString.Lazy N==/length -> compareLength/==EQ" [~1] forall t n.
   n == length t = compareLength t n == EQ
"ByteString.Lazy length//=N -> compareLength//=EQ" [~1] forall t n.
   length t /= n = compareLength t n /= EQ
"ByteString.Lazy N/=/length -> compareLength//=EQ" [~1] forall t n.
   n /= length t = compareLength t n /= EQ
"ByteString.Lazy length/<N -> compareLength/==LT" [~1] forall t n.
   length t < n = compareLength t n == LT
"ByteString.Lazy >N/length -> compareLength/==LT" [~1] forall t n.
   n > length t = compareLength t n == LT
"ByteString.Lazy length/<=N -> compareLength//=GT" [~1] forall t n.
   length t <= n = compareLength t n /= GT
"ByteString.Lazy <=N/length -> compareLength//=GT" [~1] forall t n.
   n >= length t = compareLength t n /= GT
"ByteString.Lazy length/>N -> compareLength/==GT" [~1] forall t n.
   length t > n = compareLength t n == GT
"ByteString.Lazy <N/length -> compareLength/==GT" [~1] forall t n.
   n < length t = compareLength t n == GT
"ByteString.Lazy length/>=N -> compareLength//=LT" [~1] forall t n.
   length t >= n = compareLength t n /= LT
"ByteString.Lazy >=N/length -> compareLength//=LT" [~1] forall t n.
   n <= length t = compareLength t n /= LT

mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = go
  where
    go s Empty        = (s, Empty)
    go s (Chunk c cs) = (s'', Chunk c' cs')
        where (s',  c')  = S.mapAccumL f s c
              (s'', cs') = go s' cs

mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = go
  where
    go s Empty        = (s, Empty)
    go s (Chunk c cs) = (s'', Chunk c' cs')
        where (s'', c') = S.mapAccumR f s' c
              (s', cs') = go s cs


scanl
    :: (Word8 -> Word8 -> Word8)
    -> Word8
    -> ByteString
    -> ByteString
scanl function = fmap (uncurry (flip snoc)) . mapAccumL (\x y -> (function x y, x))

scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 function byteStream = case uncons byteStream of
  Nothing -> Empty
  Just (firstByte, remainingBytes) -> scanl function firstByte remainingBytes

scanr
    :: (Word8 -> Word8 -> Word8)
    -> Word8
    -> ByteString
    -> ByteString
scanr function = fmap (uncurry cons) . mapAccumR (\x y -> (function y x, x))

scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 function byteStream = case unsnoc byteStream of
  Nothing -> Empty
  Just (initialBytes, lastByte) -> scanr function lastByte initialBytes


iterate :: (Word8 -> Word8) -> Word8 -> ByteString
iterate f = unfoldr (\x -> case f x of !x' -> Just (x', x'))

repeat :: Word8 -> ByteString
repeat w = cs where cs = Chunk (S.replicate smallChunkSize w) cs

replicate :: Int64 -> Word8 -> ByteString
replicate n w
    | n <= 0             = Empty
    | n < fromIntegral smallChunkSize = Chunk (S.replicate (fromIntegral n) w) Empty
    | r == 0             = cs -- preserve invariant
    | otherwise          = Chunk (S.unsafeTake (fromIntegral r) c) cs
 where
    c      = S.replicate smallChunkSize w
    cs     = nChunks q
    (q, r) = quotRem n (fromIntegral smallChunkSize)
    nChunks 0 = Empty
    nChunks m = Chunk c (nChunks (m-1))

cycle :: HasCallStack => ByteString -> ByteString
cycle Empty = errorEmptyList "cycle"
cycle cs    = cs' where cs' = foldrChunks Chunk cs' cs

unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f = unfoldChunk 32
  where unfoldChunk n x =
          case S.unfoldrN n f x of
            (c, Nothing)
              | S.null c  -> Empty
              | otherwise -> Chunk c Empty
            (c, Just x')  -> Chunk c (unfoldChunk (n*2) x')


take :: Int64 -> ByteString -> ByteString
take i _ | i <= 0 = Empty
take i cs0         = take' i cs0
  where take' 0 _            = Empty
        take' _ Empty        = Empty
        take' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then Chunk (S.take (fromIntegral n) c) Empty
            else Chunk c (take' (n - fromIntegral (S.length c)) cs)

takeEnd :: Int64 -> ByteString -> ByteString
takeEnd i bs
  | i <= 0 = Empty
  | otherwise = splitAtEndFold (\_ res -> res) id i bs

splitAtEndFold
  :: forall result
  .  (S.StrictByteString -> result -> result)
  -> (ByteString -> result)
  -> Int64
  -> ByteString -- ^ Input ByteString
  -> result
splitAtEndFold step end len bs0 = assert (len > 0) $ case bs0 of
  Empty -> end Empty
  Chunk c t -> goR len c t t
 where
  goR :: Int64 -> S.StrictByteString -> ByteString -> ByteString -> result
  goR !undershoot nextOutput@(S.BS noFp noLen) toSplit toScan =
      assert (undershoot > 0) $
      case toScan of
    Empty
      | undershoot >= intToInt64 noLen
        -> end (Chunk nextOutput toSplit)
      | undershootW <- fromIntegral @Int64 @Int undershoot
      , splitIndex <- noLen - undershootW
      , beforeSplit <- S.BS noFp splitIndex
      , afterSplit <- S.BS (noFp `S.plusForeignPtr` splitIndex) undershootW
        -> step beforeSplit $ end (Chunk afterSplit toSplit)

    Chunk (S.BS _ cLen) newBsR
      | cLen64 <- intToInt64 cLen
      , undershoot > cLen64
        -> goR (undershoot - cLen64) nextOutput toSplit newBsR
      | undershootW <- fromIntegral @Int64 @Int undershoot
        -> step nextOutput $ goL (cLen - undershootW) toSplit newBsR

  goL :: Int -> ByteString -> ByteString -> result
  goL !overshoot toSplit toScan =
      assert (overshoot >= 0) $
      case toSplit of
    Empty -> splitAtEndFoldInvariantFailed
    Chunk c@(S.BS _ cLen) newBsL
      | overshoot >= cLen
        -> step c $ goL (overshoot - cLen) newBsL toScan
      | otherwise
        -> goR (intToInt64 $ cLen - overshoot) c newBsL toScan

splitAtEndFoldInvariantFailed :: a
splitAtEndFoldInvariantFailed =
  moduleError "splitAtEndFold"
              "internal error: toSplit not longer than toScan"

drop  :: Int64 -> ByteString -> ByteString
drop i p | i <= 0 = p
drop i cs0 = drop' i cs0
  where drop' 0 cs           = cs
        drop' _ Empty        = Empty
        drop' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then Chunk (S.drop (fromIntegral n) c) cs
            else drop' (n - fromIntegral (S.length c)) cs

dropEnd :: Int64 -> ByteString -> ByteString
dropEnd i p
  | i <= 0 = p
  | otherwise = splitAtEndFold Chunk (const Empty) i p

splitAt :: Int64 -> ByteString -> (ByteString, ByteString)
splitAt i cs0 | i <= 0 = (Empty, cs0)
splitAt i cs0 = splitAt' i cs0
  where splitAt' 0 cs           = (Empty, cs)
        splitAt' _ Empty        = (Empty, Empty)
        splitAt' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then (Chunk (S.take (fromIntegral n) c) Empty
                 ,Chunk (S.drop (fromIntegral n) c) cs)
            else let (cs', cs'') = splitAt' (n - fromIntegral (S.length c)) cs
                   in (Chunk c cs', cs'')


takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f = takeWhile'
  where takeWhile' Empty        = Empty
        takeWhile' (Chunk c cs) =
          case S.findIndexOrLength (not . f) c of
            0                  -> Empty
            n | n < S.length c -> Chunk (S.take n c) Empty
              | otherwise      -> Chunk c (takeWhile' cs)

takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd f = takeWhileEnd'
  where takeWhileEnd' Empty = Empty
        takeWhileEnd' cs    =
            snd $ foldrChunks takeTuple (True,Empty) cs
        takeTuple _ (False, bs) = (False,bs)
        takeTuple c (True,bs)   =
           case S.takeWhileEnd f c of
                c' | S.length c' == S.length c -> (True, Chunk c bs)
                   | otherwise                 -> (False, fromStrict c' `append` bs)

dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f = dropWhile'
  where dropWhile' Empty        = Empty
        dropWhile' (Chunk c cs) =
          case S.findIndexOrLength (not . f) c of
            n | n < S.length c -> Chunk (S.drop n c) cs
              | otherwise      -> dropWhile' cs

dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd f = go []
  where go acc (Chunk c cs)
            | f (S.last c) = go (c : acc) cs
            | otherwise    = List.foldl (flip Chunk) (go [] cs) (c : acc)
        go acc Empty       = dropEndBytes acc
        dropEndBytes []         = Empty
        dropEndBytes (x : xs)   =
            case S.dropWhileEnd f x of
                 x' | S.null x' -> dropEndBytes xs
                    | otherwise -> List.foldl' (flip Chunk) Empty (x' : xs)

break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break f = break'
  where break' Empty        = (Empty, Empty)
        break' (Chunk c cs) =
          case S.findIndexOrLength f c of
            0                  -> (Empty, Chunk c cs)
            n | n < S.length c -> (Chunk (S.take n c) Empty
                                  ,Chunk (S.drop n c) cs)
              | otherwise      -> let (cs', cs'') = break' cs
                                   in (Chunk c cs', cs'')


breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd  f = go []
  where go acc (Chunk c cs)
            | f (S.last c) = List.foldl (flip $ BF.first . Chunk) (go [] cs) (c : acc)
            | otherwise = go (c : acc) cs
        go acc Empty = dropEndBytes acc
        dropEndBytes [] = (Empty, Empty)
        dropEndBytes (x : xs) =
            case S.breakEnd f x of
                 (x', x'') | S.null x' -> let (y, y') = dropEndBytes xs
                                           in (y, y' `append` fromStrict x)
                           | otherwise ->
                                List.foldl' (flip $ BF.first . Chunk) (fromStrict x', fromStrict x'') xs



breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c (LPS ps) = case (breakByte' ps) of (a,b) -> (LPS a, LPS b)
  where breakByte' []     = ([], [])
        breakByte' (x:xs) =
          case P.elemIndex c x of
            Just 0  -> ([], x : xs)
            Just n  -> (P.take n x : [], P.drop n x : xs)
            Nothing -> let (xs', xs'') = breakByte' xs
                        in (x : xs', xs'')

spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
spanByte c (LPS ps) = case (spanByte' ps) of (a,b) -> (LPS a, LPS b)
  where spanByte' []     = ([], [])
        spanByte' (x:xs) =
          case P.spanByte c x of
            (x', x'') | P.null x'  -> ([], x : xs)
                      | P.null x'' -> let (xs', xs'') = spanByte' xs
                                       in (x : xs', xs'')
                      | otherwise  -> (x' : [], x'' : xs)

span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)

spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd p = breakEnd (not . p)

splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith _ Empty          = []
splitWith p (Chunk c0 cs0) = comb [] (S.splitWith p c0) cs0

  where comb :: [P.ByteString] -> [P.ByteString] -> ByteString -> [ByteString]
        comb acc [s] Empty        = [revChunks (s:acc)]
        comb acc [s] (Chunk c cs) = comb (s:acc) (S.splitWith p c) cs
        comb acc (s:ss) cs        = revChunks (s:acc) : comb [] ss cs
        comb _ [] _ = error "Strict splitWith returned [] for nonempty input"

split :: Word8 -> ByteString -> [ByteString]
split _ Empty     = []
split w (Chunk c0 cs0) = comb [] (S.split w c0) cs0

  where comb :: [P.ByteString] -> [P.ByteString] -> ByteString -> [ByteString]
        comb acc [s] Empty        = [revChunks (s:acc)]
        comb acc [s] (Chunk c cs) = comb (s:acc) (S.split w c) cs
        comb acc (s:ss) cs        = revChunks (s:acc) : comb [] ss cs
        comb _ [] _ = error "Strict split returned [] for nonempty input"

group :: ByteString -> [ByteString]
group = go
  where
    go Empty        = []
    go (Chunk c cs)
      | S.length c == 1  = to [c] (S.unsafeHead c) cs
      | otherwise        = to [S.unsafeTake 1 c] (S.unsafeHead c) (Chunk (S.unsafeTail c) cs)

    to acc !_ Empty        = [revNonEmptyChunks acc]
    to acc !w (Chunk c cs) =
      case S.findIndexOrLength (/= w) c of
        0                    -> revNonEmptyChunks acc
                              : go (Chunk c cs)
        n | n == S.length c  -> to (S.unsafeTake n c : acc) w cs
          | otherwise        -> revNonEmptyChunks (S.unsafeTake n c : acc)
                              : go (Chunk (S.unsafeDrop n c) cs)

groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy k = go
  where
    go Empty        = []
    go (Chunk c cs)
      | S.length c == 1  = to [c] (S.unsafeHead c) cs
      | otherwise        = to [S.unsafeTake 1 c] (S.unsafeHead c) (Chunk (S.unsafeTail c) cs)

    to acc !_ Empty        = [revNonEmptyChunks acc]
    to acc !w (Chunk c cs) =
      case S.findIndexOrLength (not . k w) c of
        0                    -> revNonEmptyChunks acc
                              : go (Chunk c cs)
        n | n == S.length c  -> to (S.unsafeTake n c : acc) w cs
          | otherwise        -> revNonEmptyChunks (S.unsafeTake n c : acc)
                              : go (Chunk (S.unsafeDrop n c) cs)

intercalate :: ByteString -> [ByteString] -> ByteString
intercalate s = concat . List.intersperse s


index :: HasCallStack => ByteString -> Int64 -> Word8
index _  i | i < 0  = moduleError "index" ("negative index: " ++ show i)
index cs0 i         = index' cs0 i
  where index' Empty     n = moduleError "index" ("index too large: " ++ show n)
        index' (Chunk c cs) n
          | n >= fromIntegral (S.length c) =
              index' cs (n - fromIntegral (S.length c))
          | otherwise       = S.unsafeIndex c (fromIntegral n)

indexMaybe :: ByteString -> Int64 -> Maybe Word8
indexMaybe _ i | i < 0 = Nothing
indexMaybe cs0 i       = index' cs0 i
  where index' Empty _ = Nothing
        index' (Chunk c cs) n
          | n >= fromIntegral (S.length c) =
              index' cs (n - fromIntegral (S.length c))
          | otherwise       = Just $! S.unsafeIndex c (fromIntegral n)

(!?) :: ByteString -> Int64 -> Maybe Word8
(!?) = indexMaybe

elemIndex :: Word8 -> ByteString -> Maybe Int64
elemIndex w = elemIndex' 0
  where elemIndex' _ Empty        = Nothing
        elemIndex' n (Chunk c cs) =
          case S.elemIndex w c of
            Nothing -> elemIndex' (n + fromIntegral (S.length c)) cs
            Just i  -> Just (n + fromIntegral i)

elemIndexEnd :: Word8 -> ByteString -> Maybe Int64
elemIndexEnd = findIndexEnd . (==)

elemIndices :: Word8 -> ByteString -> [Int64]
elemIndices w = elemIndices' 0
  where elemIndices' _ Empty        = []
        elemIndices' n (Chunk c cs) = List.map ((+n).fromIntegral) (S.elemIndices w c)
                             ++ elemIndices' (n + fromIntegral (S.length c)) cs

count :: Word8 -> ByteString -> Int64
count w = foldlChunks (\n c -> n + fromIntegral (S.count w c)) 0

findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndex k = findIndex' 0
  where findIndex' _ Empty        = Nothing
        findIndex' n (Chunk c cs) =
          case S.findIndex k c of
            Nothing -> findIndex' (n + fromIntegral (S.length c)) cs
            Just i  -> Just (n + fromIntegral i)

findIndexEnd :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndexEnd k = findIndexEnd' 0
  where
    findIndexEnd' _ Empty = Nothing
    findIndexEnd' n (Chunk c cs) =
      let !n' = n + S.length c
          !i  = fromIntegral . (n +) <$> S.findIndexEnd k c
      in findIndexEnd' n' cs `mplus` i

find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f = find'
  where find' Empty        = Nothing
        find' (Chunk c cs) = case S.find f c of
            Nothing -> find' cs
            Just w  -> Just w

findIndices :: (Word8 -> Bool) -> ByteString -> [Int64]
findIndices k = findIndices' 0
  where findIndices' _ Empty        = []
        findIndices' n (Chunk c cs) = List.map ((+n).fromIntegral) (S.findIndices k c)
                             ++ findIndices' (n + fromIntegral (S.length c)) cs


elem :: Word8 -> ByteString -> Bool
elem w cs = case elemIndex w cs of Nothing -> False ; _ -> True

notElem :: Word8 -> ByteString -> Bool
notElem w cs = not (w `elem` cs)

filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter p = go
    where
        go Empty        = Empty
        go (Chunk x xs) = chunk (S.filter p x) (go xs)

filterByte :: Word8 -> ByteString -> ByteString
filterByte w ps = replicate (count w ps) w

"ByteString specialise filter (== x)" forall x.
  filter ((==) x) = filterByte x

"ByteString specialise filter (== x)" forall x.
 filter (== x) = filterByte x

filterNotByte :: Word8 -> ByteString -> ByteString
filterNotByte w (LPS xs) = LPS (filterMap (P.filterNotByte w) xs)

partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition _ Empty = (Empty, Empty)
partition p (Chunk x xs) = (chunk t ts, chunk f fs)
  where
    (t,   f) = S.partition p x
    (ts, fs) = partition   p xs


isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf Empty _  = True
isPrefixOf _ Empty  = False
isPrefixOf (Chunk x xs) (Chunk y ys)
    | S.length x == S.length y = x == y  && isPrefixOf xs ys
    | S.length x <  S.length y = x == yh && isPrefixOf xs (Chunk yt ys)
    | otherwise                = xh == y && isPrefixOf (Chunk xt xs) ys
  where (xh,xt) = S.splitAt (S.length y) x
        (yh,yt) = S.splitAt (S.length x) y

stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix Empty bs  = Just bs
stripPrefix _ Empty  = Nothing
stripPrefix (Chunk x xs) (Chunk y ys)
    | S.length x == S.length y = if x == y then stripPrefix xs ys else Nothing
    | S.length x <  S.length y = do yt <- S.stripPrefix x y
                                    stripPrefix xs (Chunk yt ys)
    | otherwise                = do xt <- S.stripPrefix y x
                                    stripPrefix (Chunk xt xs) ys

isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y

stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix x y = reverse <$> stripPrefix (reverse x) (reverse y)


zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip = zipWith (,)

zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith _ Empty     _  = []
zipWith _ _      Empty = []
zipWith f (Chunk a as) (Chunk b bs) = go a as b bs
  where
    go !x xs !y ys = let
      !xHead = S.unsafeHead x
      !yHead = S.unsafeHead y
      in f xHead yHead : to (S.unsafeTail x) xs (S.unsafeTail y) ys

    to !x xs !y ys
      | Chunk x' xs' <- chunk x xs
      , Chunk y' ys' <- chunk y ys
      = go x' xs' y' ys'
      | otherwise = []

packZipWith :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
packZipWith _ Empty _ = Empty
packZipWith _ _ Empty = Empty
packZipWith f (Chunk a@(S.BS _ al) as) (Chunk b@(S.BS _ bl) bs) = Chunk (S.packZipWith f a b) $
    case compare al bl of
        LT -> packZipWith f as $ Chunk (S.drop al b) bs
        EQ -> packZipWith f as bs
        GT -> packZipWith f (Chunk (S.drop bl a) as) bs

unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
unzip ls = (pack (List.map fst ls), pack (List.map snd ls))


inits :: ByteString -> [ByteString]
inits bs = NE.toList $! initsNE bs

initsNE :: ByteString -> NonEmpty ByteString
initsNE = (Empty :|) . inits' id
  where
    inits' :: (ByteString -> ByteString) -> ByteString -> [ByteString]
    inits' _ Empty = []
    inits' f (Chunk c@(S.BS x len) cs)
      = [f (S.BS x n `Chunk` Empty) | n <- [1..len]]
      ++ inits' (f . Chunk c) cs

tails :: ByteString -> [ByteString]
tails bs = NE.toList $! tailsNE bs

tailsNE :: ByteString -> NonEmpty ByteString
tailsNE bs = case uncons bs of
  Nothing -> Empty :| []
  Just (_, tl) -> bs :| tails tl



copy :: ByteString -> ByteString
copy = foldrChunks (Chunk . S.copy) Empty




hGetContentsN :: Int -> Handle -> IO ByteString
hGetContentsN k h = lazyRead -- TODO close on exceptions
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetSome h k -- only blocks if there is no data available
        if S.null c
          then hClose h >> return Empty
          else Chunk c <$> lazyRead

hGetN :: Int -> Handle -> Int -> IO ByteString
hGetN k h n | n > 0 = readChunks n
  where
    readChunks !i = do
        c <- S.hGet h (min k i)
        case S.length c of
            0 -> return Empty
            m -> do cs <- readChunks (i - m)
                    return (Chunk c cs)

hGetN _ _ 0 = return Empty
hGetN _ h n = illegalBufferSize h "hGet" n

hGetNonBlockingN :: Int -> Handle -> Int -> IO ByteString
hGetNonBlockingN k h n | n > 0= readChunks n
  where
    readChunks !i = do
        c <- S.hGetNonBlocking h (min k i)
        case S.length c of
            0 -> return Empty
            m -> do cs <- readChunks (i - m)
                    return (Chunk c cs)

hGetNonBlockingN _ _ 0 = return Empty
hGetNonBlockingN _ h n = illegalBufferSize h "hGetNonBlocking" n

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []

hGetContents :: Handle -> IO ByteString
hGetContents = hGetContentsN defaultChunkSize

hGet :: Handle -> Int -> IO ByteString
hGet = hGetN defaultChunkSize

hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking = hGetNonBlockingN defaultChunkSize

readFile :: FilePath -> IO ByteString
readFile f = openBinaryFile f ReadMode >>= hGetContents

modifyFile :: IOMode -> FilePath -> ByteString -> IO ()
modifyFile mode f txt = withBinaryFile f mode (`hPut` txt)

writeFile :: FilePath -> ByteString -> IO ()
writeFile = modifyFile WriteMode

appendFile :: FilePath -> ByteString -> IO ()
appendFile = modifyFile AppendMode

getContents :: IO ByteString
getContents = hGetContents stdin

hPut :: Handle -> ByteString -> IO ()
hPut h = foldrChunks (\c rest -> S.hPut h c >> rest) (return ())

hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking _ Empty           = return Empty
hPutNonBlocking h bs@(Chunk c cs) = do
  c' <- S.hPutNonBlocking h c
  case S.length c' of
    l' | l' == S.length c -> hPutNonBlocking h cs
    0                     -> return bs
    _                     -> return (Chunk c' cs)

hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

putStr :: ByteString -> IO ()
putStr = hPut stdout

interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents


errorEmptyList :: HasCallStack => String -> a
errorEmptyList fun = moduleError fun "empty ByteString"

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error ("Data.ByteString.Lazy." ++ fun ++ ':':' ':msg)


revNonEmptyChunks :: [P.ByteString] -> ByteString
revNonEmptyChunks = List.foldl' (flip Chunk) Empty

revChunks :: [P.ByteString] -> ByteString
revChunks = List.foldl' (flip chunk) Empty

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral @Int @Int64


module Data.Binary (

      Binary(..)

    , GBinaryGet(..)
    , GBinaryPut(..)

    , Get
    , Put

    , putWord8
    , getWord8

    , encode                    -- :: Binary a => a -> ByteString
    , decode                    -- :: Binary a => ByteString -> a
    , decodeOrFail

    , encodeFile                -- :: Binary a => FilePath -> a -> IO ()
    , decodeFile                -- :: Binary a => FilePath -> IO a
    , decodeFileOrFail

    , module Data.Word -- useful

    ) where

import Data.Word

import Data.Binary.Class
import Data.Binary.Put
import Data.Binary.Get
import Data.Binary.Generic ()

import qualified Data.ByteString as B ( hGet, length )
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L ( defaultChunkSize )
import System.IO ( withBinaryFile, IOMode(ReadMode) )




encode :: Binary a => a -> ByteString
encode = runPut . put

decode :: Binary a => ByteString -> a
decode = runGet get

decodeOrFail :: Binary a => L.ByteString
             -> Either (L.ByteString, ByteOffset, String)
                       (L.ByteString, ByteOffset, a)
decodeOrFail = runGetOrFail get



encodeFile :: Binary a => FilePath -> a -> IO ()
encodeFile f v = L.writeFile f (encode v)

decodeFile :: Binary a => FilePath -> IO a
decodeFile f = do
  result <- decodeFileOrFail f
  case result of
    Right x -> return x
    Left (_,str) -> error str

decodeFileOrFail :: Binary a => FilePath -> IO (Either (ByteOffset, String) a)
decodeFileOrFail f =
  withBinaryFile f ReadMode $ \h -> do
    feed (runGetIncremental get) h
  where -- TODO: put in Data.Binary.Get and name pushFromHandle?
    feed (Done _ _ x) _ = return (Right x)
    feed (Fail _ pos str) _ = return (Left (pos, str))
    feed (Partial k) h = do
      chunk <- B.hGet h L.defaultChunkSize
      case B.length chunk of
        0 -> feed (k Nothing) h
        _ -> feed (k (Just chunk)) h




module Control.Monad
    (-- *  Functor and monad classes
     Functor(..),
     Monad((>>=), (>>), return),
     MonadFail(fail),
     MonadPlus(mzero, mplus),
     mapM,
     mapM_,
     forM,
     forM_,
     sequence,
     sequence_,
     (=<<),
     (>=>),
     (<=<),
     forever,
     void,
     join,
     msum,
     mfilter,
     filterM,
     mapAndUnzipM,
     zipWithM,
     zipWithM_,
     foldM,
     foldM_,
     replicateM,
     replicateM_,
     guard,
     when,
     unless,
     liftM,
     liftM2,
     liftM3,
     liftM4,
     liftM5,
     ap,
     (<$!>)
     ) where

import GHC.Internal.Control.Monad


The functions in this module use the following naming conventions:

* A postfix \'@M@\' always stands for a function in the Kleisli category:
  The monad type constructor @m@ is added to function results
  (modulo currying) and nowhere else.  So, for example,

> filter  ::              (a ->   Bool) -> [a] ->   [a]
> filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

* A postfix \'@_@\' changes the result type from @(m a)@ to @(m ())@.
  Thus, for example:

> sequence  :: Monad m => [m a] -> m [a]
> sequence_ :: Monad m => [m a] -> m ()

* A prefix \'@m@\' generalizes an existing function to a monadic form.
  Thus, for example:

> filter  ::                (a -> Bool) -> [a] -> [a]
> mfilter :: MonadPlus m => (a -> Bool) -> m a -> m a


           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
           , RankNTypes


module Control.Concurrent (



        ThreadId,
        myThreadId,

        forkIO,
        forkFinally,
        forkIOWithUnmask,
        killThread,
        throwTo,

        forkOn,
        forkOnWithUnmask,
        getNumCapabilities,
        setNumCapabilities,
        threadCapability,


        yield,



        threadDelay,
        threadWaitRead,
        threadWaitWrite,
        threadWaitReadSTM,
        threadWaitWriteSTM,


        module GHC.Internal.Control.Concurrent.MVar,
        module Control.Concurrent.Chan,
        module Control.Concurrent.QSem,
        module Control.Concurrent.QSemN,

        rtsSupportsBoundThreads,
        forkOS,
        forkOSWithUnmask,
        isCurrentThreadBound,
        runInBoundThread,
        runInUnboundThread,

        mkWeakThreadId,











    ) where

import Prelude
import GHC.Internal.Control.Exception.Base as Exception

import GHC.Internal.Conc.Bound
import GHC.Conc hiding (threadWaitRead, threadWaitWrite,
                        threadWaitReadSTM, threadWaitWriteSTM)

import GHC.Internal.System.Posix.Types ( Fd )

#if defined(mingw32_HOST_OS)
import GHC.Internal.Foreign.C.Error
import GHC.Internal.Foreign.C.Types
import GHC.Internal.System.IO
import GHC.Internal.Data.Functor ( void )
import GHC.Internal.Int ( Int64 )
#else
import qualified GHC.Internal.Conc.IO as Conc
#endif

import GHC.Internal.Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Concurrent.QSemN

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then


   #boundthreads#

threadWaitRead :: Fd -> IO ()
threadWaitRead fd
#if defined(mingw32_HOST_OS)
  | threaded  = withThread (waitFd fd False)
  | otherwise = case fd of
                  0 -> do _ <- hWaitForInput stdin (-1)
                          return ()
                  _ -> errorWithoutStackTrace "threadWaitRead requires -threaded on Windows, or use GHC.System.IO.hWaitForInput"
#else
  = Conc.threadWaitRead fd
#endif

threadWaitWrite :: Fd -> IO ()
threadWaitWrite fd
#if defined(mingw32_HOST_OS)
  | threaded  = withThread (waitFd fd True)
  | otherwise = errorWithoutStackTrace "threadWaitWrite requires -threaded on Windows"
#else
  = Conc.threadWaitWrite fd
#endif

threadWaitReadSTM :: Fd -> IO (STM (), IO ())
threadWaitReadSTM fd
#if defined(mingw32_HOST_OS)
  | threaded = do v <- newTVarIO Nothing
                  mask_ $ void $ forkIO $ do result <- try (waitFd fd False)
                                             atomically (writeTVar v $ Just result)
                  let waitAction = do result <- readTVar v
                                      case result of
                                        Nothing         -> retry
                                        Just (Right ()) -> return ()
                                        Just (Left e)   -> throwSTM (e :: IOException)
                  let killAction = return ()
                  return (waitAction, killAction)
  | otherwise = errorWithoutStackTrace "threadWaitReadSTM requires -threaded on Windows"
#else
  = Conc.threadWaitReadSTM fd
#endif

threadWaitWriteSTM :: Fd -> IO (STM (), IO ())
threadWaitWriteSTM fd
#if defined(mingw32_HOST_OS)
  | threaded = do v <- newTVarIO Nothing
                  mask_ $ void $ forkIO $ do result <- try (waitFd fd True)
                                             atomically (writeTVar v $ Just result)
                  let waitAction = do result <- readTVar v
                                      case result of
                                        Nothing         -> retry
                                        Just (Right ()) -> return ()
                                        Just (Left e)   -> throwSTM (e :: IOException)
                  let killAction = return ()
                  return (waitAction, killAction)
  | otherwise = errorWithoutStackTrace "threadWaitWriteSTM requires -threaded on Windows"
#else
  = Conc.threadWaitWriteSTM fd
#endif

#if defined(mingw32_HOST_OS)
foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool

withThread :: IO a -> IO a
withThread io = do
  m <- newEmptyMVar
  _ <- mask_ $ forkIO $ try io >>= putMVar m
  x <- takeMVar m
  case x of
    Right a -> return a
    Left e  -> throwIO (e :: IOException)

waitFd :: Fd -> Bool -> IO ()
waitFd fd write = do
   throwErrnoIfMinus1_ "fdReady" $
        fdReady (fromIntegral fd) (if write then 1 else 0) (-1) 0

foreign import ccall safe "fdReady"
  fdReady :: CInt -> CBool -> Int64 -> CBool -> IO CInt
#endif



      #osthreads# In GHC, threads created by 'forkIO' are lightweight threads, and
      are managed entirely by the GHC runtime.  Typically Haskell
      threads are an order of magnitude or two more efficient (in
      terms of both time and space) than operating system threads.

      The downside of having lightweight threads is that only one can
      run at a time, so if one thread blocks in a foreign call, for
      example, the other threads cannot continue.  The GHC runtime
      works around this by making use of full OS threads where
      necessary.  When the program is built with the @-threaded@
      option (to link against the multithreaded version of the
      runtime), a thread making a @safe@ foreign call will not block
      the other threads in the system; another OS thread will take
      over running Haskell threads until the original call returns.
      The runtime maintains a pool of these /worker/ threads so that
      multiple Haskell threads can be involved in external calls
      simultaneously.

      The "System.IO" module manages multiplexing in its own way.  On
      Windows systems it uses @safe@ foreign calls to ensure that
      threads doing I\/O operations don't block the whole runtime,
      whereas on Unix systems all the currently blocked I\/O requests
      are managed by a single thread (the /IO manager thread/) using
      a mechanism such as @epoll@ or @kqueue@, depending on what is
      provided by the host operating system.

      The runtime will run a Haskell thread using any of the available
      worker OS threads.  If you need control over which particular OS
      thread is used to run a given Haskell thread, perhaps because
      you need to call a foreign library that uses OS-thread-local
      state, then you need bound threads (see "Control.Concurrent#boundthreads").

      If you don't use the @-threaded@ option, then the runtime does
      not make use of multiple OS threads.  Foreign calls will block
      all other running Haskell threads until the call returns.  The
      "System.IO" module still does multiplexing, so there can be multiple
      threads doing I\/O, and this is handled internally by the runtime using
      @select@.


      In a standalone GHC program, only the main thread is
      required to terminate in order for the process to terminate.
      Thus all other forked threads will simply terminate at the same
      time as the main thread (the terminology for this kind of
      behaviour is \"daemonic threads\").

      If you want the program to wait for child threads to
      finish before exiting, you need to program this yourself.  A
      simple mechanism is to have each child thread write to an
      'MVar' when it completes, and have the main
      thread wait on all the 'MVar's before
      exiting:

>   myForkIO :: IO () -> IO (MVar ())
>   myForkIO io = do
>     mvar <- newEmptyMVar
>     forkFinally io (\_ -> putMVar mvar ())
>     return mvar

      Note that we use 'forkFinally' to make sure that the
      'MVar' is written to even if the thread dies or
      is killed for some reason.

      A better method is to keep a global list of all child
      threads which we should wait for at the end of the program:

>    children :: MVar [MVar ()]
>    children = unsafePerformIO (newMVar [])
>
>    waitForChildren :: IO ()
>    waitForChildren = do
>      cs <- takeMVar children
>      case cs of
>        []   -> return ()
>        m:ms -> do
>           putMVar children ms
>           takeMVar m
>           waitForChildren
>
>    forkChild :: IO () -> IO ThreadId
>    forkChild io = do
>        mvar <- newEmptyMVar
>        childs <- takeMVar children
>        putMVar children (mvar:childs)
>        forkFinally io (\_ -> putMVar mvar ())
>
>     main =
>       later waitForChildren $
>       ...

      The main thread principle also applies to calls to Haskell from
      outside, using @foreign export@.  When the @foreign export@ed
      function is invoked, it starts a new main thread, and it returns
      when this main thread terminates.  If the call causes new
      threads to be forked, they may remain in the system after the
      @foreign export@ed function has returned.


      GHC implements pre-emptive multitasking: the execution of
      threads are interleaved in a random fashion.  More specifically,
      a thread may be pre-empted whenever it allocates some memory,
      which unfortunately means that tight loops which do no
      allocation tend to lock out other threads (this only seems to
      happen with pathological benchmark-style code, however).

      The rescheduling timer runs on a 20ms granularity by
      default, but this may be altered using the
      @-i\<n\>@ RTS option.  After a rescheduling
      \"tick\" the running thread is pre-empted as soon as
      possible.

      One final note: the
      @aaaa@ @bbbb@ example may not
      work too well on GHC (see Scheduling, above), due
      to the locking on a 'System.IO.Handle'.  Only one thread
      may hold the lock on a 'System.IO.Handle' at any one
      time, so if a reschedule happens while a thread is holding the
      lock, the other thread won't be able to run.  The upshot is that
      the switch from @aaaa@ to
      @bbbbb@ happens infrequently.  It can be
      improved by lowering the reschedule tick period.  We also have a
      patch that causes a reschedule whenever a thread waiting on a
      lock is woken up, but haven't found it to be useful for anything
      other than this example :-)


GHC attempts to detect when threads are deadlocked using the garbage
collector.  A thread that is not reachable (cannot be found by
following pointers from live objects) must be deadlocked, and in this
case the thread is sent an exception.  The exception is either
'BlockedIndefinitelyOnMVar', 'BlockedIndefinitelyOnSTM',
'NonTermination', or 'Deadlock', depending on the way in which the
thread is deadlocked.

Note that this feature is intended for debugging, and should not be
relied on for the correct operation of your program.  There is no
guarantee that the garbage collector will be accurate enough to detect
your deadlock, and no guarantee that the garbage collector will run in
a timely enough manner.  Basically, the same caveats as for finalizers
apply to deadlock detection.

There is a subtle interaction between deadlock detection and
finalizers (as created by 'GHC.Foreign.Concurrent.newForeignPtr' or the
functions in "System.Mem.Weak"): if a thread is blocked waiting for a
finalizer to run, then the thread will be considered deadlocked and
sent an exception.  So preferably don't do this, but if you have no
alternative then it is possible to prevent the thread from being
considered deadlocked by making a 'StablePtr' pointing to it.  Don't
forget to release the 'StablePtr' later with 'freeStablePtr'.



module Control.Applicative (
    Applicative(..),
    Alternative(..),
    Const(..), WrappedMonad(..), WrappedArrow(..), ZipList(..),
    (<$>), (<$), (<**>),
    liftA, liftA3,
    optional,
    asum,
    ) where

import GHC.Internal.Control.Category hiding ((.), id)
import GHC.Internal.Control.Arrow
import GHC.Internal.Data.Maybe
import GHC.Internal.Data.Tuple
import GHC.Internal.Data.Foldable (asum)
import GHC.Internal.Data.Functor ((<$>))
import GHC.Internal.Data.Functor.Const (Const(..))
import GHC.Internal.Data.Typeable (Typeable)
import GHC.Internal.Data.Data (Data)

import GHC.Internal.Base
import GHC.Internal.Functor.ZipList (ZipList(..))
import GHC.Generics


newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }
                         deriving ( Generic  -- ^ @since 4.7.0.0
                                  , Generic1 -- ^ @since 4.7.0.0
                                  , Monad    -- ^ @since 4.7.0.0
                                  )

instance Monad m => Functor (WrappedMonad m) where
    fmap f (WrapMonad v) = WrapMonad (liftM f v)

instance Monad m => Applicative (WrappedMonad m) where
    pure = WrapMonad . pure
    WrapMonad f <*> WrapMonad v = WrapMonad (f `ap` v)
    liftA2 f (WrapMonad x) (WrapMonad y) = WrapMonad (liftM2 f x y)

instance MonadPlus m => Alternative (WrappedMonad m) where
    empty = WrapMonad mzero
    WrapMonad u <|> WrapMonad v = WrapMonad (u `mplus` v)

deriving instance (Typeable (m :: Type -> Type), Typeable a, Data (m a))
         => Data (WrappedMonad m a)

newtype WrappedArrow a b c = WrapArrow { unwrapArrow :: a b c }
                           deriving ( Generic  -- ^ @since 4.7.0.0
                                    , Generic1 -- ^ @since 4.7.0.0
                                    )

instance Arrow a => Functor (WrappedArrow a b) where
    fmap f (WrapArrow a) = WrapArrow (a >>> arr f)

instance Arrow a => Applicative (WrappedArrow a b) where
    pure x = WrapArrow (arr (const x))
    liftA2 f (WrapArrow u) (WrapArrow v) =
      WrapArrow (u &&& v >>> arr (uncurry f))

instance (ArrowZero a, ArrowPlus a) => Alternative (WrappedArrow a b) where
    empty = WrapArrow zeroArrow
    WrapArrow u <|> WrapArrow v = WrapArrow (u <+> v)

deriving instance (Typeable (a :: Type -> Type -> Type), Typeable b, Typeable c,
                   Data (a b c))
         => Data (WrappedArrow a b c)



optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing


#include "lens-common.h"
module Control.Lens.Fold
  (
    Fold
  , IndexedFold

  , (^..)
  , (^?)
  , (^?!)
  , pre, ipre
  , preview, previews, ipreview, ipreviews
  , preuse, preuses, ipreuse, ipreuses

  , has, hasn't

  , folding, ifolding
  , foldring, ifoldring
  , folded
  , folded64
  , unfolded
  , iterated
  , filtered
  , filteredBy
  , backwards
  , repeated
  , replicated
  , cycled
  , takingWhile
  , droppingWhile
  , worded, lined

  , foldMapOf, foldOf
  , foldrOf, foldlOf
  , toListOf, toNonEmptyOf
  , altOf
  , anyOf, allOf, noneOf
  , andOf, orOf
  , productOf, sumOf
  , traverseOf_, forOf_, sequenceAOf_
  , traverse1Of_, for1Of_, sequence1Of_
  , mapMOf_, forMOf_, sequenceOf_
  , asumOf, msumOf
  , concatMapOf, concatOf
  , elemOf, notElemOf
  , lengthOf
  , nullOf, notNullOf
  , firstOf, first1Of, lastOf, last1Of
  , maximumOf, maximum1Of, minimumOf, minimum1Of
  , maximumByOf, minimumByOf
  , findOf
  , findMOf
  , foldrOf', foldlOf'
  , foldr1Of, foldl1Of
  , foldr1Of', foldl1Of'
  , foldrMOf, foldlMOf
  , lookupOf

  , (^@..)
  , (^@?)
  , (^@?!)

  , ifoldMapOf
  , ifoldrOf
  , ifoldlOf
  , ianyOf
  , iallOf
  , inoneOf
  , itraverseOf_
  , iforOf_
  , imapMOf_
  , iforMOf_
  , iconcatMapOf
  , ifindOf
  , ifindMOf
  , ifoldrOf'
  , ifoldlOf'
  , ifoldrMOf
  , ifoldlMOf
  , itoListOf
  , elemIndexOf
  , elemIndicesOf
  , findIndexOf
  , findIndicesOf

  , ifiltered
  , itakingWhile
  , idroppingWhile

  , Leftmost
  , Rightmost
  , Traversed
  , Sequenced

  , foldBy
  , foldByOf
  , foldMapBy
  , foldMapByOf
  ) where

import Prelude ()

import Control.Applicative.Backwards
import Control.Comonad
import Control.Lens.Getter
import Control.Lens.Internal.Fold
import Control.Lens.Internal.Getter
import Control.Lens.Internal.Indexed
import Control.Lens.Internal.Magma
import Control.Lens.Internal.Prelude
import Control.Lens.Type
import Control.Monad as Monad
import Control.Monad.Reader
import Control.Monad.State
import Data.CallStack
import Data.Functor.Apply hiding ((<.))
import Data.Int (Int64)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid (First (..), All (..), Alt (..), Any (..))
import Data.Reflection

import qualified Data.Semigroup as Semi


infixl 8 ^.., ^?, ^?!, ^@.., ^@?, ^@?!


folding :: Foldable f => (s -> f a) -> Fold s a
folding sfa agb = phantom . traverse_ agb . sfa

ifolding :: (Foldable f, Indexable i p, Contravariant g, Applicative g) => (s -> f (i, a)) -> Over p g s t a b
ifolding sfa f = phantom . traverse_ (phantom . uncurry (indexed f)) . sfa

foldring :: (Contravariant f, Applicative f) => ((a -> f a -> f a) -> f a -> s -> f a) -> LensLike f s t a b
foldring fr f = phantom . fr (\a fa -> f a *> fa) noEffect

ifoldring :: (Indexable i p, Contravariant f, Applicative f) => ((i -> a -> f a -> f a) -> f a -> s -> f a) -> Over p f s t a b
ifoldring ifr f = phantom . ifr (\i a fa -> indexed f i a *> fa) noEffect

folded :: Foldable f => IndexedFold Int (f a) a
folded = conjoined (foldring foldr) (ifoldring ifoldr)

ifoldr :: Foldable f => (Int -> a -> b -> b) -> b -> f a -> b
ifoldr f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0

folded64 :: Foldable f => IndexedFold Int64 (f a) a
folded64 = conjoined (foldring foldr) (ifoldring ifoldr64)

ifoldr64 :: Foldable f => (Int64 -> a -> b -> b) -> b -> f a -> b
ifoldr64 f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0

repeated :: Apply f => LensLike' f a a
repeated f a = as where as = f a .> as

replicated :: Int -> Fold a a
replicated n0 f a = go n0 where
  m = f a
  go 0 = noEffect
  go n = m *> go (n - 1)

cycled :: Apply f => LensLike f s t a b -> LensLike f s t a b
cycled l f a = as where as = l f a .> as

unfolded :: (b -> Maybe (a, b)) -> Fold b a
unfolded f g = go where
  go b = case f b of
    Just (a, b') -> g a *> go b'
    Nothing      -> noEffect

iterated :: Apply f => (a -> a) -> LensLike' f a a
iterated f g = go where
  go a = g a .> go (f a)

filtered :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a
filtered p = dimap (\x -> if p x then Right x else Left x) (either pure id) . right'

filteredBy :: (Indexable i p, Applicative f) => Getting (First i) a i -> p a (f a) -> a -> f a
filteredBy p f val = case val ^? p of
  Nothing -> pure val
  Just witness -> indexed f witness val

takingWhile :: (Conjoined p, Applicative f) => (a -> Bool) -> Over p (TakingWhile p f a a) s t a a -> Over p f s t a a
takingWhile p l pafb = fmap runMagma . traverse (cosieve pafb) . runTakingWhile . l flag where
  flag = cotabulate $ \wa -> let a = extract wa; r = p a in TakingWhile r a $ \pr ->
    if pr && r then Magma () wa else MagmaPure a

droppingWhile :: (Conjoined p, Profunctor q, Applicative f)
              => (a -> Bool)
              -> Optical p q (Compose (State Bool) f) s t a a
              -> Optical p q f s t a a
droppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = cotabulate $ \wa -> Compose $ state $ \b -> let
      a = extract wa
      b' = b && p a
    in (if b' then pure a else cosieve f wa, b')

worded :: Applicative f => IndexedLensLike' Int f String String
worded f = fmap unwords . conjoined traverse (indexing traverse) f . words

lined :: Applicative f => IndexedLensLike' Int f String String
lined f = fmap (intercalate "\n") . conjoined traverse (indexing traverse) f . lines


foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf = coerce

foldOf :: Getting a s a -> s -> a
foldOf l = getConst #. l Const

foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)

foldlOf :: Getting (Dual (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf l f z = (flip appEndo z .# getDual) `rmap` foldMapOf l (Dual #. Endo #. flip f)


toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []

toNonEmptyOf :: Getting (NonEmptyDList a) s a -> s -> NonEmpty a
toNonEmptyOf l = flip getNonEmptyDList [] . foldMapOf l (NonEmptyDList #. (:|))

altOf :: Applicative f => Getting (Alt f a) s a -> s -> f a
altOf l = getAlt #. views l (Alt #. pure)

(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s

andOf :: Getting All s Bool -> s -> Bool
andOf l = getAll #. foldMapOf l All

orOf :: Getting Any s Bool -> s -> Bool
orOf l = getAny #. foldMapOf l Any

anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf l f = getAny #. foldMapOf l (Any #. f)

allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
allOf l f = getAll #. foldMapOf l (All #. f)

noneOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
noneOf l f = not . anyOf l f

productOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a
productOf l = foldlOf' l (*) 1

sumOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a
sumOf l = foldlOf' l (+) 0

traverseOf_ :: Functor f => Getting (Traversed r f) s a -> (a -> f r) -> s -> f ()
traverseOf_ l f = void . getTraversed #. foldMapOf l (Traversed #. f)

forOf_ :: Functor f => Getting (Traversed r f) s a -> s -> (a -> f r) -> f ()
forOf_ = flip . traverseOf_

sequenceAOf_ :: Functor f => Getting (Traversed a f) s (f a) -> s -> f ()
sequenceAOf_ l = void . getTraversed #. foldMapOf l Traversed

traverse1Of_ :: Functor f => Getting (TraversedF r f) s a -> (a -> f r) -> s -> f ()
traverse1Of_ l f = void . getTraversedF #. foldMapOf l (TraversedF #. f)

for1Of_ :: Functor f => Getting (TraversedF r f) s a -> s -> (a -> f r) -> f ()
for1Of_ = flip . traverse1Of_

sequence1Of_ :: Functor f => Getting (TraversedF a f) s (f a) -> s -> f ()
sequence1Of_ l = void . getTraversedF #. foldMapOf l TraversedF

mapMOf_ :: Monad m => Getting (Sequenced r m) s a -> (a -> m r) -> s -> m ()
mapMOf_ l f = liftM skip . getSequenced #. foldMapOf l (Sequenced #. f)

forMOf_ :: Monad m => Getting (Sequenced r m) s a -> s -> (a -> m r) -> m ()
forMOf_ = flip . mapMOf_

sequenceOf_ :: Monad m => Getting (Sequenced a m) s (m a) -> s -> m ()
sequenceOf_ l = liftM skip . getSequenced #. foldMapOf l Sequenced

asumOf :: Alternative f => Getting (Endo (f a)) s (f a) -> s -> f a
asumOf l = foldrOf l (<|>) empty

msumOf :: MonadPlus m => Getting (Endo (m a)) s (m a) -> s -> m a
msumOf l = foldrOf l mplus mzero

elemOf :: Eq a => Getting Any s a -> a -> s -> Bool
elemOf l = anyOf l . (==)

notElemOf :: Eq a => Getting All s a -> a -> s -> Bool
notElemOf l = allOf l . (/=)

concatMapOf :: Getting [r] s a -> (a -> [r]) -> s -> [r]
concatMapOf = coerce

concatOf :: Getting [r] s [r] -> s -> [r]
concatOf l = getConst #. l Const


lengthOf :: Getting (Endo (Endo Int)) s a -> s -> Int
lengthOf l = foldlOf' l (\a _ -> a + 1) 0

(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)

(^?!) :: HasCallStack => s -> Getting (Endo a) s a -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s

firstOf :: Getting (Leftmost a) s a -> s -> Maybe a
firstOf l = getLeftmost . foldMapOf l LLeaf

first1Of :: Getting (Semi.First a) s a -> s -> a
first1Of l = Semi.getFirst . foldMapOf l Semi.First

lastOf :: Getting (Rightmost a) s a -> s -> Maybe a
lastOf l = getRightmost . foldMapOf l RLeaf

last1Of :: Getting (Semi.Last a) s a -> s -> a
last1Of l = Semi.getLast . foldMapOf l Semi.Last

nullOf :: Getting All s a -> s -> Bool
nullOf = hasn't

notNullOf :: Getting Any s a -> s -> Bool
notNullOf = has

maximumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe a
maximumOf l = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! max x y

maximum1Of :: Ord a => Getting (Semi.Max a) s a -> s -> a
maximum1Of l = Semi.getMax . foldMapOf l Semi.Max

minimumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe a
minimumOf l = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! min x y

minimum1Of :: Ord a => Getting (Semi.Min a) s a -> s -> a
minimum1Of l = Semi.getMin . foldMapOf l Semi.Min

maximumByOf :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> Maybe a
maximumByOf l cmp = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! if cmp x y == GT then x else y

minimumByOf :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> Maybe a
minimumByOf l cmp = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! if cmp x y == GT then y else x

findOf :: Getting (Endo (Maybe a)) s a -> (a -> Bool) -> s -> Maybe a
findOf l f = foldrOf l (\a y -> if f a then Just a else y) Nothing

findMOf :: Monad m => Getting (Endo (m (Maybe a))) s a -> (a -> m Bool) -> s -> m (Maybe a)
findMOf l f = foldrOf l (\a y -> f a >>= \r -> if r then return (Just a) else y) $ return Nothing

lookupOf :: Eq k => Getting (Endo (Maybe v)) s (k,v) -> k -> s -> Maybe v
lookupOf l k = foldrOf l (\(k',v) next -> if k == k' then Just v else next) Nothing

foldr1Of :: HasCallStack => Getting (Endo (Maybe a)) s a -> (a -> a -> a) -> s -> a
foldr1Of l f = fromMaybe (error "foldr1Of: empty structure")
             . foldrOf l mf Nothing where
  mf x my = Just $ case my of
    Nothing -> x
    Just y -> f x y

foldl1Of :: HasCallStack => Getting (Dual (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
foldl1Of l f = fromMaybe (error "foldl1Of: empty structure") . foldlOf l mf Nothing where
  mf mx y = Just $ case mx of
    Nothing -> y
    Just x  -> f x y

foldrOf' :: Getting (Dual (Endo (Endo r))) s a -> (a -> r -> r) -> r -> s -> r
foldrOf' l f z0 = \xs -> foldlOf l f' (Endo id) xs `appEndo` z0
  where f' (Endo k) x = Endo $ \ z -> k $! f x z

foldlOf' :: Getting (Endo (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf' l f z0 = \xs -> foldrOf l f' (Endo id) xs `appEndo` z0
  where f' x (Endo k) = Endo $ \z -> k $! f z x

foldr1Of' :: HasCallStack => Getting (Dual (Endo (Endo (Maybe a)))) s a -> (a -> a -> a) -> s -> a
foldr1Of' l f = fromMaybe (error "foldr1Of': empty structure") . foldrOf' l mf Nothing where
  mf x Nothing = Just $! x
  mf x (Just y) = Just $! f x y

foldl1Of' :: HasCallStack => Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
foldl1Of' l f = fromMaybe (error "foldl1Of': empty structure") . foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! f x y

foldrMOf :: Monad m
         => Getting (Dual (Endo (r -> m r))) s a
         -> (a -> r -> m r) -> r -> s -> m r
foldrMOf l f z0 = \xs -> foldlOf l f' return xs z0
  where f' k x z = f x z >>= k

foldlMOf :: Monad m
         => Getting (Endo (r -> m r)) s a
         -> (r -> a -> m r) -> r -> s -> m r
foldlMOf l f z0 = \xs -> foldrOf l f' return xs z0
  where f' x k z = f z x >>= k



has :: Getting Any s a -> s -> Bool
has l = getAny #. foldMapOf l (\_ -> Any True)



hasn't :: Getting All s a -> s -> Bool
hasn't l = getAll #. foldMapOf l (\_ -> All False)


pre :: Getting (First a) s a -> IndexPreservingGetter s (Maybe a)
pre l = dimap (getFirst . getConst #. l (Const #. First #. Just)) phantom

ipre :: IndexedGetting i (First (i, a)) s a -> IndexPreservingGetter s (Maybe (i, a))
ipre l = dimap (getFirst . getConst #. l (Indexed $ \i a -> Const (First (Just (i, a))))) phantom


preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = asks (getFirst #. foldMapOf l (First #. Just))

ipreview :: MonadReader s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreview l = asks (getFirst #. ifoldMapOf l (\i a -> First (Just (i, a))))


previews :: MonadReader s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
previews l f = asks (getFirst . foldMapOf l (First #. Just . f))

ipreviews :: MonadReader s m => IndexedGetting i (First r) s a -> (i -> a -> r) -> m (Maybe r)
ipreviews l f = asks (getFirst . ifoldMapOf l (\i -> First #. Just . f i))


preuse :: MonadState s m => Getting (First a) s a -> m (Maybe a)
preuse l = gets (preview l)

ipreuse :: MonadState s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreuse l = gets (ipreview l)

preuses :: MonadState s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
preuses l f = gets (previews l f)

ipreuses :: MonadState s m => IndexedGetting i (First r) s a -> (i -> a -> r) -> m (Maybe r)
ipreuses l f = gets (ipreviews l f)



backwards :: (Profunctor p, Profunctor q) => Optical p q (Backwards f) s t a b -> Optical p q f s t a b
backwards l f = forwards #. l (Backwards #. f)


ifoldMapOf :: IndexedGetting i m s a -> (i -> a -> m) -> s -> m
ifoldMapOf = coerce

ifoldrOf :: IndexedGetting i (Endo r) s a -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf l f z = flip appEndo z . getConst #. l (Const #. Endo #. Indexed f)

ifoldlOf :: IndexedGetting i (Dual (Endo r)) s a -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf l f z = (flip appEndo z .# getDual) `rmap` ifoldMapOf l (\i -> Dual #. Endo #. flip (f i))

ianyOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
ianyOf = coerce

iallOf :: IndexedGetting i All s a -> (i -> a -> Bool) -> s -> Bool
iallOf = coerce

inoneOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
inoneOf l f = not . ianyOf l f

itraverseOf_ :: Functor f => IndexedGetting i (Traversed r f) s a -> (i -> a -> f r) -> s -> f ()
itraverseOf_ l f = void . getTraversed #. getConst #. l (Const #. Traversed #. Indexed f)

iforOf_ :: Functor f => IndexedGetting i (Traversed r f) s a -> s -> (i -> a -> f r) -> f ()
iforOf_ = flip . itraverseOf_

imapMOf_ :: Monad m => IndexedGetting i (Sequenced r m) s a -> (i -> a -> m r) -> s -> m ()
imapMOf_ l f = liftM skip . getSequenced #. getConst #. l (Const #. Sequenced #. Indexed f)

iforMOf_ :: Monad m => IndexedGetting i (Sequenced r m) s a -> s -> (i -> a -> m r) -> m ()
iforMOf_ = flip . imapMOf_

iconcatMapOf :: IndexedGetting i [r] s a -> (i -> a -> [r]) -> s -> [r]
iconcatMapOf = ifoldMapOf

ifindOf :: IndexedGetting i (Endo (Maybe a)) s a -> (i -> a -> Bool) -> s -> Maybe a
ifindOf l f = ifoldrOf l (\i a y -> if f i a then Just a else y) Nothing

ifindMOf :: Monad m => IndexedGetting i (Endo (m (Maybe a))) s a -> (i -> a -> m Bool) -> s -> m (Maybe a)
ifindMOf l f = ifoldrOf l (\i a y -> f i a >>= \r -> if r then return (Just a) else y) $ return Nothing

ifoldrOf' :: IndexedGetting i (Dual (Endo (r -> r))) s a -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf' l f z0 xs = ifoldlOf l f' id xs z0
  where f' i k x z = k $! f i x z

ifoldlOf' :: IndexedGetting i (Endo (r -> r)) s a -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' l f z0 xs = ifoldrOf l f' id xs z0
  where f' i x k z = k $! f i z x

ifoldrMOf :: Monad m => IndexedGetting i (Dual (Endo (r -> m r))) s a -> (i -> a -> r -> m r) -> r -> s -> m r
ifoldrMOf l f z0 xs = ifoldlOf l f' return xs z0
  where f' i k x z = f i x z >>= k

ifoldlMOf :: Monad m => IndexedGetting i (Endo (r -> m r)) s a -> (i -> r -> a -> m r) -> r -> s -> m r
ifoldlMOf l f z0 xs = ifoldrOf l f' return xs z0
  where f' i x k z = f i z x >>= k

itoListOf :: IndexedGetting i (Endo [(i,a)]) s a -> s -> [(i,a)]
itoListOf l = ifoldrOf l (\i a -> ((i,a):)) []


(^@..) :: s -> IndexedGetting i (Endo [(i,a)]) s a -> [(i,a)]
s ^@.. l = ifoldrOf l (\i a -> ((i,a):)) [] s

(^@?) :: s -> IndexedGetting i (Endo (Maybe (i, a))) s a -> Maybe (i, a)
s ^@? l = ifoldrOf l (\i x _ -> Just (i,x)) Nothing s

(^@?!) :: HasCallStack => s -> IndexedGetting i (Endo (i, a)) s a -> (i, a)
s ^@?! l = ifoldrOf l (\i x _ -> (i,x)) (error "(^@?!): empty Fold") s

elemIndexOf :: Eq a => IndexedGetting i (First i) s a -> a -> s -> Maybe i
elemIndexOf l a = findIndexOf l (a ==)

elemIndicesOf :: Eq a => IndexedGetting i (Endo [i]) s a -> a -> s -> [i]
elemIndicesOf l a = findIndicesOf l (a ==)

findIndexOf :: IndexedGetting i (First i) s a -> (a -> Bool) -> s -> Maybe i
findIndexOf l p = preview (l . filtered p . asIndex)

findIndicesOf :: IndexedGetting i (Endo [i]) s a -> (a -> Bool) -> s -> [i]
findIndicesOf l p = toListOf (l . filtered p . asIndex)


ifiltered :: (Indexable i p, Applicative f) => (i -> a -> Bool) -> Optical' p (Indexed i) f a a
ifiltered p f = Indexed $ \i a -> if p i a then indexed f i a else pure a

itakingWhile :: (Indexable i p, Profunctor q, Contravariant f, Applicative f)
         => (i -> a -> Bool)
         -> Optical' (Indexed i) q (Const (Endo (f s))) s a
         -> Optical' p q f s a
itakingWhile p l f = (flip appEndo noEffect .# getConst) `rmap` l g where
  g = Indexed $ \i a -> Const . Endo $ if p i a then (indexed f i a *>) else const noEffect

idroppingWhile :: (Indexable i p, Profunctor q, Applicative f)
              => (i -> a -> Bool)
              -> Optical (Indexed i) q (Compose (State Bool) f) s t a a
              -> Optical p q f s t a a
idroppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = Indexed $ \ i a -> Compose $ state $ \b -> let
      b' = b && p i a
    in (if b' then pure a else indexed f i a, b')


skip :: a -> ()
skip _ = ()


foldByOf :: Fold s a -> (a -> a -> a) -> a -> s -> a
foldByOf l f z = reifyMonoid f z (foldMapOf l ReflectedMonoid)

foldMapByOf :: Fold s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r
foldMapByOf l f z g = reifyMonoid f z (foldMapOf l (ReflectedMonoid #. g))