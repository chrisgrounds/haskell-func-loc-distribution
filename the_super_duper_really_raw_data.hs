{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Convertion to and from @aeson@ 'A.Value'.
-- 
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

-------------------------------------------------------------------------------
-- Decoding: strict bytestring
-------------------------------------------------------------------------------

-- | Efficiently deserialize a JSON value from a strict 'B.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
decodeStrict :: (A.FromJSON a) => BS.ByteString -> Maybe a
decodeStrict bs = unResult (toResultValue (bsToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> Just x
        | otherwise   -> Nothing
    A.IError _ _      -> Nothing

-- | Like 'decodeStrict' but returns an error message when decoding fails.
eitherDecodeStrict :: (A.FromJSON a) => BS.ByteString -> Either String a
eitherDecodeStrict bs = unResult (toResultValue (bsToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> Right x
        | otherwise   -> Left "Trailing garbage"
    A.IError path msg -> Left $ formatError path msg

-- | Like 'decodeStrict' but throws an 'AesonException' when decoding fails.
throwDecodeStrict :: forall a m. (A.FromJSON a, MonadThrow m) => BS.ByteString -> m a
throwDecodeStrict bs = unResult (toResultValue (bsToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | bsSpace bs' -> pure x
        | otherwise   -> throwM $ AesonException "Trailing garbage"
    A.IError path msg -> throwM $ AesonException $ formatError path msg

-------------------------------------------------------------------------------
-- Decoding: lazy bytestring
-------------------------------------------------------------------------------

-- | Efficiently deserialize a JSON value from a lazy 'L.ByteString'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
decode :: (A.FromJSON a) => LBS.ByteString -> Maybe a
decode bs = unResult (toResultValue (lbsToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Just x
        | otherwise    -> Nothing
    A.IError _ _       -> Nothing

-- | Like 'decode' but returns an error message when decoding fails.
eitherDecode :: (A.FromJSON a) => LBS.ByteString -> Either String a
eitherDecode bs = unResult (toResultValue (lbsToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs' -> Right x
        | otherwise    -> Left "Trailing garbage"
    A.IError path msg  -> Left $ formatError path msg

-- | Like 'decode' but throws an 'AesonException' when decoding fails.
--
-- 'throwDecode' is in @aeson@ since 2.1.2.0, but this variant is added later.
throwDecode :: forall a m. (A.FromJSON a, MonadThrow m) => LBS.ByteString -> m a
throwDecode bs = unResult (toResultValue (lbsToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | lbsSpace bs'  -> pure x
        | otherwise    -> throwM $ AesonException "Trailing garbage"
    A.IError path msg  -> throwM $ AesonException $ formatError path msg

-------------------------------------------------------------------------------
-- Decoding: strict text
-------------------------------------------------------------------------------

-- | Efficiently deserialize a JSON value from a strict 'T.Text'.
-- If this fails due to incomplete or invalid input, 'Nothing' is
-- returned.
--
-- @since 2.2.1.0
decodeStrictText :: (A.FromJSON a) => T.Text -> Maybe a
decodeStrictText bs = unResult (toResultValue (textToTokens bs)) (\_ -> Nothing) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | textSpace bs' -> Just x
        | otherwise     -> Nothing
    A.IError _ _        -> Nothing

-- | Like 'decodeStrictText' but returns an error message when decoding fails.
--
-- @since 2.2.1.0
eitherDecodeStrictText :: (A.FromJSON a) => T.Text -> Either String a
eitherDecodeStrictText bs = unResult (toResultValue (textToTokens bs)) Left $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | textSpace bs' -> Right x
        | otherwise     -> Left "Trailing garbage"
    A.IError path msg   -> Left $ formatError path msg

-- | Like 'decodeStrictText' but throws an 'AesonException' when decoding fails.
--
-- @since 2.2.1.0
throwDecodeStrictText :: forall a m. (A.FromJSON a, MonadThrow m) => T.Text -> m a
throwDecodeStrictText bs = unResult (toResultValue (textToTokens bs)) (throwM . AesonException) $ \v bs' -> case A.ifromJSON v of
    A.ISuccess x
        | textSpace bs' -> pure x
        | otherwise     -> throwM $ AesonException "Trailing garbage"
    A.IError path msg   -> throwM $ AesonException $ formatError path msg

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -O2 #-}
-- | Parser from strict 'ByteString' to 'Tokens'.
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

-- | Lex (and parse) strict 'ByteString' into 'Tokens' stream.
--
-- @since 2.1.2.0
--
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

    -- Array
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

    -- Record
    goR :: Parser TkRecord k
    goR (skipSpace -> bs) k = case BS.uncons bs of
        Nothing                       -> tkErrEOF "record key literal or }"
        Just (W8.DOUBLE_QUOTE,  !bs1) -> goRK bs1 k           -- "
        Just (W8.RIGHT_CURLY, !bs1)   -> TkRecordEnd (k bs1)  -- }
        Just _                        -> tkErrBS bs "record key literal or }"

    -- after record pair, expecting ," or }
    goR1 :: Parser TkRecord k
    goR1 (skipSpace -> bs) k = case BS.uncons bs of
        Nothing                           -> tkErr "Unexpected end-of-input, expecting , or }"
        Just (W8.COMMA, !bs1) -> case BS.uncons (skipSpace bs1) of
            Nothing                      -> tkErrEOF "key literal"
            Just (W8.DOUBLE_QUOTE, !bs2) -> goRK bs2 k
            Just _                       -> tkErrBS bs "key literal"
        Just (W8.RIGHT_CURLY, !bs1)       -> TkRecordEnd (k bs1)
        _                                 -> tkErr $ "Unexpected " ++ showBeginning bs ++ ", expecting , or }"

    -- key of record (after double quote)
    goRK :: Parser TkRecord k
    goRK bs1 k = scanStringLiteral (\t bs -> goRK' t bs k) tkErr bs1

    -- after key of a record, expecting :
    goRK' :: Text -> Parser TkRecord k
    goRK' t (skipSpace -> bs) k = case BS.uncons bs of
        Nothing               -> tkErrEOF ":"
        Just (W8.COLON, !bs3) -> TkPair (Key.fromText t) $ goT bs3 $ \bs4 -> goR1 bs4 k
        Just _                -> tkErrBS bs ":"

stripPrefix :: ByteString -> Int -> ByteString -> Maybe ByteString
stripPrefix pfx n bs | BS.isPrefixOf pfx bs = Just (BS.Unsafe.unsafeDrop n bs)
                     | otherwise            = Nothing
{-# INLINE stripPrefix #-}

type Parser tk k = ByteString -> (ByteString -> k) -> tk k String

showBeginning :: ByteString -> String
showBeginning = show . BS.take 30

-- | Strip leading (ASCII) space
skipSpace :: ByteString -> ByteString
skipSpace = BS.dropWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}

tkErrEOF :: AsError t =>String ->  t k String
tkErrEOF expected = tkErr $
    "Unexpected end-of-input, expecting " ++ expected
{-# INLINE tkErrEOF #-}

tkErrBS :: AsError t => ByteString -> String ->  t k String
tkErrBS bs expected = tkErr $
    "Unexpected " ++ showBeginning bs ++ ", expecting " ++ expected
{-# INLINE tkErrBS #-}

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

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

    -- in goEsc and goSlash we don't need to check for control characters as unescapeText verifies that.
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

-------------------------------------------------------------------------------
-- Number
-------------------------------------------------------------------------------

--
-- number   := integer fraction exponent
-- integer  := 0 | [1-9][0-9]* | -0 | -[1-9][0-9]*
-- fraction := "" | . [0-9]+
-- exponent := "" | E sign [0-9]+ | e sign [0-9]+
-- sign     := "" | - | +
--
-- This scanner doesn't recognize the leading minus sign, we recognize only integer := 0 | [1-9][0-9]*,
-- as the minus sign is recognized by outer scanner already.
--
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

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -O2 #-}
-- | Parser from lazy 'ByteString' to 'Tokens'.
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

-- | Lex (and parse) lazy 'ByteString' into 'Tokens' stream.
--
-- @since 2.1.2.0
--
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

    -- Array
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

    -- Record
    goR :: Parser TkRecord k
    goR (skipSpace -> bs) k = case LBS.uncons bs of
        Nothing                       -> tkErrEOF "record key literal or }"
        Just (W8.DOUBLE_QUOTE,  !bs1) -> goRK bs1 k           -- "
        Just (W8.RIGHT_CURLY, !bs1)   -> TkRecordEnd (k bs1)  -- }
        Just _                        -> tkErrBS bs "record key literal or }"

    -- after record pair, expecting ," or }
    goR1 :: Parser TkRecord k
    goR1 (skipSpace -> bs) k = case LBS.uncons bs of
        Nothing                           -> tkErr "Unexpected end-of-input, expecting , or }"
        Just (W8.COMMA, !bs1) -> case LBS.uncons (skipSpace bs1) of
            Nothing                      -> tkErrEOF "key literal"
            Just (W8.DOUBLE_QUOTE, !bs2) -> goRK bs2 k
            Just _                       -> tkErrBS bs "key literal"
        Just (W8.RIGHT_CURLY, !bs1)       -> TkRecordEnd (k bs1)
        _                                 -> tkErr $ "Unexpected " ++ showBeginning bs ++ ", expecting , or }"

    -- key of record (after double quote)
    goRK :: Parser TkRecord k
    goRK bs1 k = scanStringLiteral (\t bs -> goRK' t bs k) tkErr bs1

    -- after key of a record, expecting :
    goRK' :: Text -> Parser TkRecord k
    goRK' t (skipSpace -> bs) k = case LBS.uncons bs of
        Nothing               -> tkErrEOF ":"
        Just (W8.COLON, !bs3) -> TkPair (Key.fromText t) $ goT bs3 $ \bs4 -> goR1 bs4 k
        Just _                -> tkErrBS bs ":"

stripPrefix :: ByteString -> Int -> ByteString -> Maybe ByteString
stripPrefix pfx n bs | LBS.isPrefixOf pfx bs = Just (LBS.drop (fromIntegral n) bs)
                     | otherwise             = Nothing
{-# INLINE stripPrefix #-}

type Parser tk k = ByteString -> (ByteString -> k) -> tk k String

showBeginning :: ByteString -> String
showBeginning = show . LBS.take 30

-- | Strip leading (ASCII) space
skipSpace :: ByteString -> ByteString
skipSpace = LBS.dropWhile $ \w -> w == 0x20 || w == 0x0a || w == 0x0d || w == 0x09
{-# INLINE skipSpace #-}

tkErrEOF :: AsError t =>String ->  t k String
tkErrEOF expected = tkErr $
    "Unexpected end-of-input, expecting " ++ expected
{-# INLINE tkErrEOF #-}

tkErrBS :: AsError t => ByteString -> String ->  t k String
tkErrBS bs expected = tkErr $
    "Unexpected " ++ showBeginning bs ++ ", expecting " ++ expected
{-# INLINE tkErrBS #-}

lbsTake :: Int -> ByteString -> BS.ByteString
lbsTake n bs = LBS.toStrict (LBS.take (fromIntegral n) bs)

lbsDrop :: Int -> ByteString -> ByteString
lbsDrop n = LBS.drop (fromIntegral n)

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

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

    -- in goEsc and goSlash we don't need to check for control characters as unescapeText verifies that.
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

-------------------------------------------------------------------------------
-- Number
-------------------------------------------------------------------------------

--
-- number   := integer fraction exponent
-- integer  := 0 | [1-9][0-9]* | -0 | -[1-9][0-9]*
-- fraction := "" | . [0-9]+
-- exponent := "" | E sign [0-9]+ | e sign [0-9]+
-- sign     := "" | - | +
--
-- This scanner doesn't recognize the leading minus sign, we recognize only integer := 0 | [1-9][0-9]*,
-- as the minus sign is recognized by outer scanner already.
--
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

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
{-# OPTIONS_GHC -O2 #-}
-- | Parser from strict 'Text' to 'Tokens'.
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


-- | Lex (and parse) strict 'ByteString' into 'Tokens' stream.
--
-- @since 2.2.1.0
--
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
    -- Array
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

    -- Record
    goR :: Parser TkRecord k
    goR (skipSpace -> bs) k = case unconsPoint bs of
        Nothing                       -> tkErrEOF "record key literal or }"
        Just (W.DOUBLE_QUOTE,  !bs1) -> goRK bs1 k           -- "
        Just (W.RIGHT_CURLY, !bs1)   -> TkRecordEnd (k bs1)  -- }
        Just _                        -> tkErrBS bs "record key literal or }"

    -- after record pair, expecting ," or }
    goR1 :: Parser TkRecord k
    goR1 (skipSpace -> bs) k = case unconsPoint bs of
        Nothing                           -> tkErr "Unexpected end-of-input, expecting , or }"
        Just (W.COMMA, !bs1) -> case unconsPoint (skipSpace bs1) of
            Nothing                      -> tkErrEOF "key literal"
            Just (W.DOUBLE_QUOTE, !bs2) -> goRK bs2 k
            Just _                       -> tkErrBS bs "key literal"
        Just (W.RIGHT_CURLY, !bs1)       -> TkRecordEnd (k bs1)
        _                                 -> tkErr $ "Unexpected " ++ showBeginning bs ++ ", expecting , or }"

    -- key of record (after double quote)
    goRK :: Parser TkRecord k
    goRK bs1 k = scanStringLiteral (\t bs -> goRK' t bs k) tkErr bs1

    -- after key of a record, expecting :
    goRK' :: Text -> Parser TkRecord k
    goRK' t (skipSpace -> bs) k = case T.uncons bs of
        Nothing          -> tkErrEOF ":"
        Just (':', !bs3) -> TkPair (Key.fromText t) $ goT bs3 $ \bs4 -> goR1 bs4 k
        Just _           -> tkErrBS bs ":"

stripPrefix :: Text -> Int -> Text -> Maybe Text
stripPrefix pfx _ bs = T.stripPrefix pfx bs
{-# INLINE stripPrefix #-}

type Parser tk k = Text -> (Text -> k) -> tk k String

showBeginning :: Text -> String
showBeginning = show . T.take 30

-- | Strip leading (ASCII) space
skipSpace :: Text -> Text
skipSpace = T.dropWhile $ \w -> w == '\x20' || w == '\x0a' || w == '\x0d' || w == '\x09'
{-# INLINE skipSpace #-}

tkErrEOF :: AsError t => String ->  t k String
tkErrEOF expected = tkErr $
    "Unexpected end-of-input, expecting " ++ expected
{-# INLINE tkErrEOF #-}

tkErrBS :: AsError t => Text -> String ->  t k String
tkErrBS bs expected = tkErr $
    "Unexpected " ++ showBeginning bs ++ ", expecting " ++ expected
{-# INLINE tkErrBS #-}

-------------------------------------------------------------------------------
-- Text
-------------------------------------------------------------------------------

scanStringLiteral
    :: forall r. (Text -> Text -> r)
    -> (String -> r)
    -> Text
    -> r
scanStringLiteral ok err bs0 = go 0 bs0 where
    -- the length is counted in bytes.
    go :: Int -> Text -> r
    go !n !bs = case unconsPoint bs of
        Nothing          -> errEnd
        Just (34, _)     -> ok (unsafeTakePoints n bs0) (unsafeDropPoints (n + 1) bs0)
        Just (92, bs')   -> goSlash (n + 1) bs'
        Just (w8, bs')
            | w8 < 0x20  -> errCC
            -- we don't need to check for @>= 0x80@ chars, as text is valid unicode.
            | otherwise  -> go (n + 1) bs'

    -- in goEsc and goSlash we don't need to check for control characters as unescapeText verifies that.
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

-------------------------------------------------------------------------------
-- Number
-------------------------------------------------------------------------------

--
-- number   := integer fraction exponent
-- integer  := 0 | [1-9][0-9]* | -0 | -[1-9][0-9]*
-- fraction := "" | . [0-9]+
-- exponent := "" | E sign [0-9]+ | e sign [0-9]+
-- sign     := "" | - | +
--
-- This scanner doesn't recognize the leading minus sign, we recognize only integer := 0 | [1-9][0-9]*,
-- as the minus sign is recognized by outer scanner already.
--
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

-------------------------------------------------------------------------------
-- Unsafe primitives
-------------------------------------------------------------------------------

{-# INLINE unconsPoint #-}
-- Uncons a primitive unit of storage from text.
-- The left-over 'Text' value may be invalid.
unconsPoint :: Text -> Maybe (Point, Text)
unconsPoint (Text arr off len)
    | len <= 0  = Nothing
    | otherwise = Just (w8, Text arr (off + 1) (len - 1))
  where
    w8 = A.unsafeIndex arr off

unsafeTakePoints :: Int -> Text -> Text
unsafeTakePoints n (Text arr off _len) = Text arr off n
{-# INLINE unsafeTakePoints #-}

unsafeDropPoints :: Int -> Text -> Text
unsafeDropPoints n (Text arr off len) = Text arr (off + n) (len - n)
{-# INLINE unsafeDropPoints #-}

-- | Token definitions.
module Data.Aeson.Decoding.Tokens (
    -- * Types
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

-- | A well-formed JSON token stream.
--
-- /Note/: 'Lit' exists to make 'Tokens' have only 6 constructors.
-- This may or may not have impact on performance.
--
-- @since 2.1.2.0
--
data Tokens k e
    = TkLit !Lit k
    | TkText !Text k
    | TkNumber !Number k
    | TkArrayOpen (TkArray k e)
    | TkRecordOpen (TkRecord k e)
    | TkErr e
  deriving (Eq, Show)

-- | Literals. @null@, @true@, @false@.
data Lit = LitNull | LitTrue | LitFalse
  deriving (Eq, Show)

-- | Numbers
--
-- We preserve whether the number was integral, decimal or in scientific form.
data Number
    = NumInteger !Integer  -- ^ e.g. @123@
    | NumDecimal !Scientific  -- ^ e.g. @123.456@
    | NumScientific !Scientific -- ^ e.g. @123e456@, @123e-456@ or @123.456E-967@
  deriving (Eq, Show)

-- | Array tokens.
data TkArray k e
    = TkItem (Tokens (TkArray k e) e)
    | TkArrayEnd k
    | TkArrayErr e
  deriving (Eq, Show)

-- | Record tokens.
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

{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Aeson.Encoding.Internal
    (
    -- * Encoding
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
    -- * Predicates
    , nullEncoding
    -- * Encoding constructors
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
    -- ** Decimal numbers
    , int8, int16, int32, int64, int
    , word8, word16, word32, word64, word
    , integer, float, double, scientific
    -- ** Decimal numbers as Text
    , int8Text, int16Text, int32Text, int64Text, intText
    , word8Text, word16Text, word32Text, word64Text, wordText
    , integerText, floatText, doubleText, scientificText
    -- ** Time
    , day
    , month
    , quarter
    , localTime
    , utcTime
    , timeOfDay
    , zonedTime
    -- ** value
    , value
    -- ** JSON tokens
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

-- | An encoding of a JSON value.
--
-- @tag@ represents which kind of JSON the Encoding is encoding to,
-- we reuse 'Text' and 'Value' as tags here.
newtype Encoding' tag = Encoding {
      fromEncoding :: Builder
      -- ^ Acquire the underlying bytestring builder.
    } deriving (Typeable)

-- | Often used synonym for 'Encoding''.
type Encoding = Encoding' Value

-- | Make Encoding from Builder.
--
-- Use with care! You have to make sure that the passed Builder
-- is a valid JSON Encoding!
unsafeToEncoding :: Builder -> Encoding' a
unsafeToEncoding = Encoding

encodingToLazyByteString :: Encoding' a -> BSL.ByteString
encodingToLazyByteString = toLazyByteString . fromEncoding
{-# INLINE encodingToLazyByteString #-}

retagEncoding :: Encoding' a -> Encoding' b
retagEncoding = Encoding . fromEncoding

-------------------------------------------------------------------------------
-- Encoding instances
-------------------------------------------------------------------------------

instance Show (Encoding' a) where
    show (Encoding e) = show (toLazyByteString e)

instance Eq (Encoding' a) where
    Encoding a == Encoding b = toLazyByteString a == toLazyByteString b

instance Ord (Encoding' a) where
    compare (Encoding a) (Encoding b) =
      compare (toLazyByteString a) (toLazyByteString b)

-- | @since 2.2.0.0
instance IsString (Encoding' a) where
  fromString = string

-- | A series of values that, when encoded, should be separated by
-- commas. Since 0.11.0.0, the '.=' operator is overloaded to create
-- either @(Text, Value)@ or 'Series'. You can use Series when
-- encoding directly to a bytestring builder as in the following
-- example:
--
-- > toEncoding (Person name age) = pairs ("name" .= name <> "age" .= age)
data Series = Empty
            | Value (Encoding' Series)
            deriving (Typeable)

pair :: Key -> Encoding -> Series
pair name val = pair' (key name) val
{-# INLINE pair #-}

pairStr :: String -> Encoding -> Series
pairStr name val = pair' (string name) val
{-# INLINE pairStr #-}

pair' :: Encoding' Key -> Encoding -> Series
pair' name val = Value $ retagEncoding $ retagEncoding name >< colon >< val

-- | A variant of a 'pair' where key is already encoded
-- including the quotes and colon.
--
-- @
-- 'pair' "foo" v = 'unsafePair' "\\"foo\\":" v
-- @
--
-- @since 2.0.3.0
--
unsafePairSBS :: ShortByteString -> Encoding -> Series
unsafePairSBS k v = Value $ retagEncoding $ Encoding (B.shortByteString k) >< v
{-# INLINE unsafePairSBS #-}

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

-- | Encode a series of key/value pairs, separated by commas.
pairs :: Series -> Encoding
pairs (Value v) = openCurly >< retagEncoding v >< closeCurly
pairs Empty     = emptyObject_
{-# INLINE pairs #-}

list :: (a -> Encoding) -> [a] -> Encoding
list _  []     = emptyArray_
list to' (x:xs) = openBracket >< to' x >< commas xs >< closeBracket
  where
    commas = foldr (\v vs -> comma >< to' v >< vs) empty
{-# INLINE list #-}

-- | Encode as JSON object
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
{-# INLINE dict #-}

-- | Type tag for tuples contents, see 'tuple'.
data InArray

infixr 6 >*<
-- | See 'tuple'.
(>*<) :: Encoding' a -> Encoding' b -> Encoding' InArray
a >*< b = retagEncoding a >< comma >< retagEncoding b
{-# INLINE (>*<) #-}

empty :: Encoding' a
empty = Encoding mempty

econcat :: [Encoding' a] -> Encoding' a
econcat = foldr (><) empty

infixr 6 ><
(><) :: Encoding' a -> Encoding' a -> Encoding' a
Encoding a >< Encoding b = Encoding (a <> b)
{-# INLINE (><) #-}

-- | Encode as a tuple.
--
-- @
-- toEncoding (X a b c) = tuple $
--     toEncoding a >*<
--     toEncoding b >*<
--     toEncoding c
tuple :: Encoding' InArray -> Encoding
tuple b = retagEncoding $ openBracket >< b >< closeBracket
{-# INLINE tuple #-}

key :: Key -> Encoding' a
key = text . Key.toText

text :: Text -> Encoding' a
text = Encoding . EB.text

lazyText :: LT.Text -> Encoding' a
lazyText t = Encoding $
    B.char7 '"' <>
    LT.foldrChunks (\x xs -> EB.unquoted x <> xs) (B.char7 '"') t

-- | @since 2.0.2.0
shortText :: ST.ShortText -> Encoding' a
shortText t = Encoding $
    B.char7 '"' <>
    -- TODO: if we can determine whether all characters are >=0x20 && <0x80
    -- we could use underlying ShortByteString directly.
    EB.unquoted (ST.toText t) <> B.char7 '"'

string :: String -> Encoding' a
string = Encoding . EB.string

-------------------------------------------------------------------------------
-- chars
-------------------------------------------------------------------------------

comma, colon, openBracket, closeBracket, openCurly, closeCurly :: Encoding' a
comma        = Encoding $ char7 ','
colon        = Encoding $ char7 ':'
openBracket  = Encoding $ char7 '['
closeBracket = Encoding $ char7 ']'
openCurly    = Encoding $ char7 '{'
closeCurly   = Encoding $ char7 '}'

-------------------------------------------------------------------------------
-- Decimal numbers
-------------------------------------------------------------------------------

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

-- |
--
-- >>> double 42
-- "42.0"
--
-- >>> double (0/0)
-- "null"
--
-- >>> double (1/0)
-- "\"+inf\""
--
-- >>> double (-23/0)
-- "\"-inf\""
--
double :: Double -> Encoding
double = realFloatToEncoding $ Encoding . B.doubleDec

scientific :: Scientific -> Encoding
scientific = Encoding . EB.scientific

realFloatToEncoding :: RealFloat a => (a -> Encoding) -> a -> Encoding
realFloatToEncoding e d
    | isNaN d      = null_
    | isInfinite d = if d > 0 then Encoding "\"+inf\"" else Encoding "\"-inf\""
    | otherwise    = e d
{-# INLINE realFloatToEncoding #-}

-------------------------------------------------------------------------------
-- Decimal numbers as Text
-------------------------------------------------------------------------------

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

-- |
--
-- >>> doubleText 42
-- "\"42.0\""
--
-- >>> doubleText (0/0)
-- "\"NaN\""
--
-- >>> doubleText (1/0)
-- "\"+inf\""
--
-- >>> doubleText (-23/0)
-- "\"-inf\""
--
doubleText :: Double -> Encoding' a
doubleText d
    | isInfinite d = if d > 0 then Encoding "\"+inf\"" else Encoding "\"-inf\""
    | otherwise = Encoding . EB.quote . B.doubleDec $ d

scientificText :: Scientific -> Encoding' a
scientificText = Encoding . EB.quote . EB.scientific

-------------------------------------------------------------------------------
-- time
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- Value
-------------------------------------------------------------------------------

value :: Value -> Encoding
value = Encoding . EB.encodeToBuilder

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- |
-- Strong type for JSON keys.
--
-- @since 2.0.0.0

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

-- | @'coercing r1 r2'@ will evaluate to @r1@ if 'Key' is 'Coercible' to  'Text',
-- and to @r2@ otherwise.
--
-- Using 'coercing' we can make more efficient implementations
-- when 'Key' is backed up by 'Text' without exposing internals.
--
coercionToText :: Maybe (Coercion Key Text)
coercionToText = Just Coercion
{-# INLINE coercionToText #-}

-- | @since 2.0.2.0
toShortText :: Key -> ST.ShortText
toShortText = ST.fromText . unKey

-- | @since 2.0.2.0
fromShortText :: ST.ShortText -> Key
fromShortText = Key . ST.toText

-------------------------------------------------------------------------------
-- instances
-------------------------------------------------------------------------------

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

-- | @since 2.0.3.0
instance QC.Arbitrary Key where
    arbitrary = fromString <$> QC.arbitrary
    shrink k  = fromString <$> QC.shrink (toString k)

-- | @since 2.0.3.0
instance QC.CoArbitrary Key where
    coarbitrary = QC.coarbitrary . toString

-- | @since 2.0.3.0
instance QC.Function Key where
    function = QC.functionMap toString fromString

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- An abstract interface for maps from JSON keys to values.
--
-- @since 2.0.0.0

module Data.Aeson.KeyMap (
    -- * Map Type
    KeyMap,

    -- * Query
    null,
    lookup,
    (!?),
    size,
    member,

    -- * Construction
    empty,
    singleton,

    -- ** Insertion
    insert,
    insertWith,

    -- * Deletion
    delete,

    -- * Update
    alterF,

    -- * Combine
    difference,
    union,
    unionWith,
    unionWithKey,
    intersection,
    intersectionWith,
    intersectionWithKey,
    alignWith,
    alignWithKey,

    -- * Lists
    fromList,
    fromListWith,
    toList,
    toAscList,
    elems,

    -- * Maps
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

    -- * Traversal
    -- ** Map
    map,
    mapWithKey,
    mapKeyVal,
    traverse,
    traverseWithKey,

    -- * Folds
    foldr,
    foldr',
    foldl,
    foldl',
    foldMapWithKey,
    foldrWithKey,

    -- * Conversions
    keys,

    -- * Filter
    filter,
    filterWithKey,
    mapMaybe,
    mapMaybeWithKey,

    -- * Key Type
    Key,
) where

-- Import stuff from Prelude explicitly
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

-------------------------------------------------------------------------------
-- Map
-------------------------------------------------------------------------------

-- | A map from JSON key type 'Key' to 'v'.
newtype KeyMap v = KeyMap { unKeyMap :: Map Key v }
  deriving (Eq, Ord, Typeable, Data, Functor)


-- | Construct an empty map.
empty :: KeyMap v
empty = KeyMap M.empty

-- | Is the map empty?
null :: KeyMap v -> Bool
null = M.null . unKeyMap

-- | Return the number of key-value mappings in this map.
size :: KeyMap v -> Int
size = M.size . unKeyMap

-- | Construct a map with a single element.
singleton :: Key -> v -> KeyMap v
singleton k v = KeyMap (M.singleton k v)

-- | Is the key a member of the map?
member :: Key -> KeyMap a -> Bool
member t (KeyMap m) = M.member t m

-- | Remove the mapping for the specified key from this map if present.
delete :: Key -> KeyMap v -> KeyMap v
delete k (KeyMap m) = KeyMap (M.delete k m)

-- | 'alterF' can be used to insert, delete, or update a value in a map.
alterF :: Functor f => (Maybe v -> f (Maybe v)) -> Key -> KeyMap v -> f (KeyMap v)
alterF f k = fmap KeyMap . M.alterF f k . unKeyMap

-- | Return the value to which the specified key is mapped,
-- or Nothing if this map contains no mapping for the key.
lookup :: Key -> KeyMap v -> Maybe v
lookup t tm = M.lookup t (unKeyMap tm)

-- | Associate the specified value with the specified key
-- in this map. If this map previously contained a mapping
-- for the key, the old value is replaced.
insert :: Key -> v -> KeyMap v -> KeyMap v
insert k v tm = KeyMap (M.insert k v (unKeyMap tm))

-- | Insert with a function combining new and old values, taken in that order.
--
-- @since 2.1.1.0
insertWith :: (a -> a -> a) -> Key -> a -> KeyMap a -> KeyMap a
insertWith f k v m = KeyMap (M.insertWith f k v (unKeyMap m))

-- | Map a function over all values in the map.
map :: (a -> b) -> KeyMap a -> KeyMap b
map = fmap

-- | Map a function over all values in the map.
--
-- @since 2.1.0.0
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

-- | Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (Key -> v -> a -> a) -> a -> KeyMap v -> a
foldrWithKey f a = M.foldrWithKey f a . unKeyMap

-- | Perform an Applicative action for each key-value pair
-- in a 'KeyMap' and produce a 'KeyMap' of all the results.
traverse :: Applicative f => (v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverse f = fmap KeyMap . T.traverse f . unKeyMap

-- | Perform an Applicative action for each key-value pair
-- in a 'KeyMap' and produce a 'KeyMap' of all the results.
traverseWithKey :: Applicative f => (Key -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseWithKey f = fmap KeyMap . M.traverseWithKey f  . unKeyMap

-- | Construct a map from a list of elements. Uses the
-- provided function, f, to merge duplicate entries with
-- (f newVal oldVal).
fromListWith :: (v -> v -> v) ->  [(Key, v)] -> KeyMap v
fromListWith op = KeyMap . M.fromListWith op

-- |  Construct a map with the supplied mappings. If the
-- list contains duplicate mappings, the later mappings take
-- precedence.
--
-- >>> fromList [("a", 'x'), ("a", 'y')]
-- fromList [("a",'y')]
--
fromList :: [(Key, v)] -> KeyMap v
fromList = KeyMap . M.fromList

-- | Return a list of this map's keys and elements.
--
-- The order is not stable. Use 'toAscList' for stable ordering.
toList :: KeyMap v -> [(Key, v)]
toList = M.toList . unKeyMap

-- | Return a list of this map' elements.
--
-- @since 2.0.3.0
elems :: KeyMap v -> [v]
elems = M.elems . unKeyMap

-- | Return a list of this map's elements in ascending order
-- based of the textual key.
toAscList :: KeyMap v -> [(Key, v)]
toAscList = M.toAscList . unKeyMap

-- | Difference of two maps. Return elements of the first
-- map not existing in the second.
difference :: KeyMap v -> KeyMap v' -> KeyMap v
difference tm1 tm2 = KeyMap (M.difference (unKeyMap tm1) (unKeyMap tm2))

-- | The (left-biased) union of two maps. It prefers the first map when duplicate
-- keys are encountered, i.e. ('union' == 'unionWith' 'const').
union :: KeyMap v -> KeyMap v -> KeyMap v
union (KeyMap x) (KeyMap y) = KeyMap (M.union x y)

-- | The union with a combining function.
unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith f (KeyMap x) (KeyMap y) = KeyMap (M.unionWith f x y)

-- | The union with a combining function.
unionWithKey :: (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey f (KeyMap x) (KeyMap y) = KeyMap (M.unionWithKey f x y)

-- | The (left-biased) intersection of two maps (based on keys).
intersection :: KeyMap a -> KeyMap b -> KeyMap a
intersection (KeyMap x) (KeyMap y) = KeyMap (M.intersection x y)

-- | The intersection with a combining function.
intersectionWith :: (a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWith f (KeyMap x) (KeyMap y) = KeyMap (M.intersectionWith f x y)

-- | The intersection with a combining function.
intersectionWithKey :: (Key -> a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWithKey f (KeyMap x) (KeyMap y) = KeyMap (M.intersectionWithKey f x y)

-- | Return a list of this map's keys.
keys :: KeyMap v -> [Key]
keys = M.keys . unKeyMap

-- | Convert a 'KeyMap' to a 'HashMap'.
toHashMap :: KeyMap v -> HashMap Key v
toHashMap = H.fromList . toList

-- | Convert a 'HashMap' to a 'KeyMap'.
fromHashMap :: HashMap Key v -> KeyMap v
fromHashMap = fromList . H.toList

-- | Convert a 'KeyMap' to a 'Map'.
toMap :: KeyMap v -> Map Key v
toMap = unKeyMap

-- | Convert a 'Map' to a 'KeyMap'.
fromMap :: Map Key v -> KeyMap v
fromMap = KeyMap

coercionToHashMap :: Maybe (Coercion (HashMap Key v) (KeyMap v))
coercionToHashMap = Nothing
{-# INLINE coercionToHashMap #-}

coercionToMap :: Maybe (Coercion (Map Key v) (KeyMap v))
coercionToMap = Just Coercion
{-# INLINE coercionToMap #-}

-- | Transform the keys and values of a 'KeyMap'.
mapKeyVal :: (Key -> Key) -> (v1 -> v2)
          -> KeyMap v1 -> KeyMap v2
mapKeyVal fk kv = foldrWithKey (\k v -> insert (fk k) (kv v)) empty
{-# INLINE mapKeyVal #-}

-- | Filter all keys/values that satisfy some predicate.
filter :: (v -> Bool) -> KeyMap v -> KeyMap v
filter f (KeyMap m) = KeyMap (M.filter f m)

-- | Filter all keys/values that satisfy some predicate.
filterWithKey :: (Key -> v -> Bool) -> KeyMap v -> KeyMap v
filterWithKey f (KeyMap m) = KeyMap (M.filterWithKey f m)

-- | Map values and collect the Just results.
mapMaybe :: (a -> Maybe b) -> KeyMap a -> KeyMap b
mapMaybe f (KeyMap m) = KeyMap (M.mapMaybe f m)

-- | Map values and collect the Just results.
mapMaybeWithKey :: (Key -> v -> Maybe u) -> KeyMap v -> KeyMap u
mapMaybeWithKey f (KeyMap m) = KeyMap (M.mapMaybeWithKey f m)

#else

-------------------------------------------------------------------------------
-- HashMap
-------------------------------------------------------------------------------

import Data.List (sortBy)
import Data.Ord (comparing)
import Prelude (fst)

-- | A map from JSON key type 'Key' to 'v'.
newtype KeyMap v = KeyMap { unKeyMap :: HashMap Key v }
  deriving (Eq, Ord, Typeable, Data, Functor)

-- | Construct an empty map.
empty :: KeyMap v
empty = KeyMap H.empty

-- | Is the map empty?
null :: KeyMap v -> Bool
null = H.null . unKeyMap

-- | Return the number of key-value mappings in this map.
size :: KeyMap v -> Int
size = H.size . unKeyMap

-- | Construct a map with a single element.
singleton :: Key -> v -> KeyMap v
singleton k v = KeyMap (H.singleton k v)

-- | Is the key a member of the map?
member :: Key -> KeyMap a -> Bool
member t (KeyMap m) = H.member t m

-- | Remove the mapping for the specified key from this map if present.
delete :: Key -> KeyMap v -> KeyMap v
delete k (KeyMap m) = KeyMap (H.delete k m)

-- | 'alterF' can be used to insert, delete, or update a value in a map.
alterF :: Functor f => (Maybe v -> f (Maybe v)) -> Key -> KeyMap v -> f (KeyMap v)
alterF f k = fmap KeyMap . H.alterF f k . unKeyMap

-- | Return the value to which the specified key is mapped,
-- or Nothing if this map contains no mapping for the key.
lookup :: Key -> KeyMap v -> Maybe v
lookup t tm = H.lookup t (unKeyMap tm)

-- | Associate the specified value with the specified key
-- in this map. If this map previously contained a mapping
-- for the key, the old value is replaced.
insert :: Key -> v -> KeyMap v -> KeyMap v
insert k v tm = KeyMap (H.insert k v (unKeyMap tm))

-- | Insert with a function combining new and old values, taken in that order.
--
-- @since 2.1.1.0
insertWith :: (a -> a -> a) -> Key -> a -> KeyMap a -> KeyMap a
insertWith f k v m = KeyMap (H.insertWith f k v (unKeyMap m))

-- | Map a function over all values in the map.
map :: (a -> b) -> KeyMap a -> KeyMap b
map = fmap

-- | Map a function over all values in the map.
--
-- @since 2.1.0.0
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

-- | Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (Key -> v -> a -> a) -> a -> KeyMap v -> a
foldrWithKey f a = H.foldrWithKey f a . unKeyMap

-- | Perform an Applicative action for each key-value pair
-- in a 'KeyMap' and produce a 'KeyMap' of all the results.
traverse :: Applicative f => (v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverse f = fmap KeyMap . T.traverse f . unKeyMap

-- | Perform an Applicative action for each key-value pair
-- in a 'KeyMap' and produce a 'KeyMap' of all the results.
traverseWithKey :: Applicative f => (Key -> v1 -> f v2) -> KeyMap v1 -> f (KeyMap v2)
traverseWithKey f = fmap KeyMap . H.traverseWithKey f  . unKeyMap

-- | Construct a map from a list of elements. Uses the
-- provided function, f, to merge duplicate entries with
-- (f newVal oldVal).
fromListWith :: (v -> v -> v) ->  [(Key, v)] -> KeyMap v
fromListWith op = KeyMap . H.fromListWith op

-- |  Construct a map with the supplied mappings. If the
-- list contains duplicate mappings, the later mappings take
-- precedence.
fromList :: [(Key, v)] -> KeyMap v
fromList = KeyMap . H.fromList

-- | Return a list of this map's elements.
--
-- The order is not stable. Use 'toAscList' for stable ordering.
toList :: KeyMap v -> [(Key, v)]
toList = H.toList . unKeyMap

-- | Return a list of this map' elements.
--
-- @since 2.0.3.0
elems :: KeyMap v -> [v]
elems = H.elems . unKeyMap

-- | Return a list of this map's elements in ascending order
-- based of the textual key.
toAscList :: KeyMap v -> [(Key, v)]
toAscList = sortBy (comparing fst) . toList

-- | Difference of two maps. Return elements of the first
-- map not existing in the second.
difference :: KeyMap v -> KeyMap v' -> KeyMap v
difference tm1 tm2 = KeyMap (H.difference (unKeyMap tm1) (unKeyMap tm2))

-- | The (left-biased) union of two maps. It prefers the first map when duplicate
-- keys are encountered, i.e. ('union' == 'unionWith' 'const').
union :: KeyMap v -> KeyMap v -> KeyMap v
union (KeyMap x) (KeyMap y) = KeyMap (H.union x y)

-- | The union with a combining function.
unionWith :: (v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWith f (KeyMap x) (KeyMap y) = KeyMap (H.unionWith f x y)

-- | The union with a combining function.
unionWithKey :: (Key -> v -> v -> v) -> KeyMap v -> KeyMap v -> KeyMap v
unionWithKey f (KeyMap x) (KeyMap y) = KeyMap (H.unionWithKey f x y)

-- | The (left-biased) intersection of two maps (based on keys).
intersection :: KeyMap a -> KeyMap b -> KeyMap a
intersection (KeyMap x) (KeyMap y) = KeyMap (H.intersection x y)

-- | The intersection with a combining function.
intersectionWith :: (a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWith f (KeyMap x) (KeyMap y) = KeyMap (H.intersectionWith f x y)

-- | The intersection with a combining function.
intersectionWithKey :: (Key -> a -> b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
intersectionWithKey f (KeyMap x) (KeyMap y) = KeyMap (H.intersectionWithKey f x y)

-- | Return a list of this map's keys.
keys :: KeyMap v -> [Key]
keys = H.keys . unKeyMap

-- | Convert a 'KeyMap' to a 'HashMap'.
toHashMap :: KeyMap v -> HashMap Key v
toHashMap = unKeyMap

-- | Convert a 'HashMap' to a 'KeyMap'.
fromHashMap :: HashMap Key v -> KeyMap v
fromHashMap = KeyMap

-- | Convert a 'KeyMap' to a 'Map'.
toMap :: KeyMap v -> Map Key v
toMap = M.fromList . toList

-- | Convert a 'Map' to a 'KeyMap'.
fromMap :: Map Key v -> KeyMap v
fromMap = fromList . M.toList

coercionToHashMap :: Maybe (Coercion (HashMap Key v) (KeyMap v))
coercionToHashMap = Just Coercion
{-# INLINE coercionToHashMap #-}

coercionToMap :: Maybe (Coercion (Map Key v) (KeyMap v))
coercionToMap = Nothing
{-# INLINE coercionToMap #-}

-- | Transform the keys and values of a 'KeyMap'.
mapKeyVal :: (Key -> Key) -> (v1 -> v2)
          -> KeyMap v1 -> KeyMap v2
mapKeyVal fk kv = foldrWithKey (\k v -> insert (fk k) (kv v)) empty
{-# INLINE mapKeyVal #-}

-- | Filter all keys/values that satisfy some predicate.
filter :: (v -> Bool) -> KeyMap v -> KeyMap v
filter f (KeyMap m) = KeyMap (H.filter f m)

-- | Filter all keys/values that satisfy some predicate.
filterWithKey :: (Key -> v -> Bool) -> KeyMap v -> KeyMap v
filterWithKey f (KeyMap m) = KeyMap (H.filterWithKey f m)

-- | Map values and collect the Just results.
mapMaybe :: (a -> Maybe b) -> KeyMap a -> KeyMap b
mapMaybe f (KeyMap m) = KeyMap (H.mapMaybe f m)

-- | Map values and collect the Just results.
mapMaybeWithKey :: (Key -> v -> Maybe u) -> KeyMap v -> KeyMap u
mapMaybeWithKey f (KeyMap m) = KeyMap (H.mapMaybeWithKey f m)

#endif

-------------------------------------------------------------------------------
-- combinators using existing abstractions
-------------------------------------------------------------------------------

-- | Return the value to which the specified key is mapped,
-- or Nothing if this map contains no mapping for the key.
--
-- This is a flipped version of 'lookup'.
--
-- @since 2.1.1.0
--
(!?) :: KeyMap v -> Key -> Maybe v
(!?) m k = lookup k m

-- | Generalized union with combining function.
alignWith :: (These a b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
alignWith f (KeyMap x) (KeyMap y) = KeyMap (SA.alignWith f x y)

-- | Generalized union with combining function.
alignWithKey :: (Key -> These a b -> c) -> KeyMap a -> KeyMap b -> KeyMap c
alignWithKey f (KeyMap x) (KeyMap y) = KeyMap (SAI.ialignWith f x y)

-- | Convert a 'KeyMap' to a @'HashMap' 'Text'@.
toHashMapText :: KeyMap v -> HashMap Text  v
toHashMapText = H.fromList . L.map (first Key.toText) . toList

-- | Convert a @'HashMap' 'Text'@to a 'KeyMap'.
fromHashMapText :: HashMap Text v -> KeyMap v
fromHashMapText = fromList . L.map (first Key.fromText) . H.toList

-- | Convert a 'KeyMap' to a @'Map' 'Text'@.
--
-- @since 2.0.2.0
toMapText :: KeyMap v -> Map Text v
toMapText = M.fromList . L.map (first Key.toText) . toList

-- | Convert a @'Map' 'Text'@to a 'KeyMap'.
--
-- @since 2.0.2.0
fromMapText :: Map Text v -> KeyMap v
fromMapText = fromList . L.map (first Key.fromText) . M.toList

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

-- This are defined using concrete combinators above.

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
    {-# INLINE foldMap #-}
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

-- | @since 2.0.2.0
instance GHC.Exts.IsList (KeyMap v) where
    type Item (KeyMap v) = (Key, v)
    fromList = fromList
    toList   = toAscList

-------------------------------------------------------------------------------
-- template-haskell
-------------------------------------------------------------------------------

instance TH.Lift v => TH.Lift (KeyMap v) where
    lift m = [| fromList m' |] where m' = toList m

#if MIN_VERSION_template_haskell(2,17,0)
    liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
    liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-------------------------------------------------------------------------------
-- hashable
-------------------------------------------------------------------------------

instance Hashable v => Hashable (KeyMap v) where
#ifdef USE_ORDEREDMAP
    hashWithSalt salt (KeyMap m) = M.foldlWithKey'
        (\acc k v -> acc `hashWithSalt` k `hashWithSalt` v)
        (hashWithSalt salt (M.size m)) m
#else
    hashWithSalt salt (KeyMap hm) = hashWithSalt salt hm
#endif

-------------------------------------------------------------------------------
-- deepseq
-------------------------------------------------------------------------------

instance NFData v => NFData (KeyMap v) where
    rnf (KeyMap hm) = rnf hm

-------------------------------------------------------------------------------
-- indexed-traversable
-------------------------------------------------------------------------------

instance WI.FunctorWithIndex Key KeyMap where
    imap = mapWithKey

instance WI.FoldableWithIndex Key KeyMap where
    ifoldr   = foldrWithKey

instance WI.TraversableWithIndex Key KeyMap where
    itraverse = traverseWithKey

-------------------------------------------------------------------------------
-- semialign
-------------------------------------------------------------------------------

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

-------------------------------------------------------------------------------
-- witherable
-------------------------------------------------------------------------------

instance W.Filterable KeyMap where
    filter = filter
    mapMaybe = mapMaybe

instance W.Witherable KeyMap where

instance W.FilterableWithIndex Key KeyMap where
    ifilter = filterWithKey
    imapMaybe = mapMaybeWithKey

instance W.WitherableWithIndex Key KeyMap where

-------------------------------------------------------------------------------
-- QuickCheck
-------------------------------------------------------------------------------

-- | @since 2.0.3.0
instance QC.Arbitrary1 KeyMap where
    liftArbitrary a  = fmap fromList (QC.liftArbitrary (QC.liftArbitrary a))
    liftShrink shr m = fmap fromList (QC.liftShrink (QC.liftShrink shr) (toList m))

-- | @since 2.0.3.0
instance QC.Arbitrary v => QC.Arbitrary (KeyMap v) where
    arbitrary = QC.arbitrary1
    shrink    = QC.shrink1

-- | @since 2.0.3.0
instance QC.CoArbitrary v => QC.CoArbitrary (KeyMap v) where
    coarbitrary = QC.coarbitrary . toList

-- | @since 2.0.3.0
instance QC.Function v => QC.Function (KeyMap v) where
    function = QC.functionMap toList fromList

-- | Like @<https://hackage.haskell.org/package/aeson-qq/docs/Data-Aeson-QQ.html Data.Aeson.QQ>@ but without interpolation.
module Data.Aeson.QQ.Simple (aesonQQ) where

import           Data.Aeson
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (Lift (..))

-- | Converts a string representation of a JSON value into 'Data.Aeson.Value' at compile-time.
--
-- @
-- {-\# LANGUAGE QuasiQuotes \#-}
--
-- import Data.Aeson (Value)
-- import Data.Aeson.QQ.Simple
--
-- joe :: 'Value'
-- joe = [aesonQQ|{ "name": \"Joe\", "age": 12 }|]
-- @
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

-- |
-- Module:      Data.Aeson.Types
-- Copyright:   (c) 2011-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Types for working with JSON data.

module Data.Aeson.Types
    (
    -- * Core JSON types
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
    -- * Convenience types and functions
    , DotNetTime(..)
    , typeMismatch
    , unexpected
    -- * Type conversion
    -- ** Parsing
    , Parser
    , Result(..)
    , FromJSON(..)
    , fromJSON
    , parse
    , parseEither
    , parseMaybe
    , parseFail
    -- ** Parser error handling
    , modifyFailure
    , prependFailure
    , parserThrowError
    , parserCatchError
    -- ** Parsing with paths
    , IResult (..)
    , ifromJSON
    , iparse
    , iparseEither
    -- ** Encoding
    , ToJSON(..)
    , KeyValue(..)
    , KeyValueOmit(..)

    -- ** Keys for maps
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

    -- *** Generic keys
    , GToJSONKey()
    , genericToJSONKey
    , GFromJSONKey()
    , genericFromJSONKey

    -- ** Liftings to unary and binary type constructors
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

    -- ** Generic JSON classes
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

    -- * Inspecting @'Value's@
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

    -- * Generic and TH encoding configuration
    , Options

    -- ** Options fields
    -- $optionsFields
    , fieldLabelModifier
    , constructorTagModifier
    , allNullaryToStringTag
    , omitNothingFields
    , allowOmittedFields
    , sumEncoding
    , unwrapUnaryRecords
    , tagSingleConstructors
    , rejectUnknownFields

    -- ** Options utilities
    , SumEncoding(..)
    , camelTo
    , camelTo2
    , defaultOptions
    , defaultTaggedObject

    -- ** Options for object keys
    , JSONKeyOptions
    , keyModifier
    , defaultJSONKeyOptions

    -- * Parsing exceptions
    , AesonException (..)

    -- * Parsing context
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

-- | Encode a 'Foldable' as a JSON array.
foldable :: (Foldable t, ToJSON a) => t a -> Encoding
foldable = toEncoding . toList
{-# INLINE foldable #-}

-- $optionsFields
-- The functions here are in fact record fields of the 'Options' type.

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Data.Aeson.Text
-- Copyright:   (c) 2012-2016 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     BSD3
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Most frequently, you'll probably want to encode straight to UTF-8
-- (the standard JSON encoding) using 'encode'.
--
-- You can use the conversions to 'Builder's when embedding JSON messages as
-- parts of a protocol.

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

-- | Encode a JSON 'Value' to a "Data.Text.Lazy"
--
-- /Note:/ uses 'toEncoding'
encodeToLazyText :: ToJSON a => a -> LT.Text
encodeToLazyText = LT.decodeUtf8 . encodingToLazyByteString . toEncoding

-- | Encode a JSON 'Value' to a "Data.Text" 'Builder', which can be
-- embedded efficiently in a text-based protocol.
--
-- If you are going to immediately encode straight to a
-- 'L.ByteString', it is more efficient to use 'encode' (lazy ByteString)
-- or @'fromEncoding' . 'toEncoding'@ (ByteString.Builder) instead.
--
-- /Note:/ Uses 'toJSON'
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

  {-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

{-|
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
-- Alternatively, one could pass 'DF2 instead
@

Please note that you can derive instances for tuples using the following syntax:

@
-- FromJSON and ToJSON instances for 4-tuples.
$('deriveJSON' 'defaultOptions' ''(,,,))
@

If you derive `ToJSON` for a type that has no constructors, the splice will
require enabling @EmptyCase@ to compile.

-}
module Data.Aeson.TH
    (
      -- * Encoding configuration
      Options(..)
    , SumEncoding(..)
    , defaultOptions
    , defaultTaggedObject

     -- * FromJSON and ToJSON derivation
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

-- We don't have MonadFail Q, so we should use `fail` from real `Prelude`

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

--------------------------------------------------------------------------------
-- Convenience
--------------------------------------------------------------------------------

-- | Generates both 'ToJSON' and 'FromJSON' instance declarations for the given
-- data type or data family instance constructor.
--
-- This is a convenience function which is equivalent to calling both
-- 'deriveToJSON' and 'deriveFromJSON'.
deriveJSON :: Options
           -- ^ Encoding options.
           -> Name
           -- ^ Name of the type for which to generate 'ToJSON' and 'FromJSON'
           -- instances.
           -> Q [Dec]
deriveJSON = deriveJSONBoth deriveToJSON deriveFromJSON

-- | Generates both 'ToJSON1' and 'FromJSON1' instance declarations for the given
-- data type or data family instance constructor.
--
-- This is a convenience function which is equivalent to calling both
-- 'deriveToJSON1' and 'deriveFromJSON1'.
deriveJSON1 :: Options
            -- ^ Encoding options.
            -> Name
            -- ^ Name of the type for which to generate 'ToJSON1' and 'FromJSON1'
            -- instances.
            -> Q [Dec]
deriveJSON1 = deriveJSONBoth deriveToJSON1 deriveFromJSON1

-- | Generates both 'ToJSON2' and 'FromJSON2' instance declarations for the given
-- data type or data family instance constructor.
--
-- This is a convenience function which is equivalent to calling both
-- 'deriveToJSON2' and 'deriveFromJSON2'.
deriveJSON2 :: Options
            -- ^ Encoding options.
            -> Name
            -- ^ Name of the type for which to generate 'ToJSON2' and 'FromJSON2'
            -- instances.
            -> Q [Dec]
deriveJSON2 = deriveJSONBoth deriveToJSON2 deriveFromJSON2

--------------------------------------------------------------------------------
-- ToJSON
--------------------------------------------------------------------------------

{-
TODO: Don't constrain phantom type variables.

data Foo a = Foo Int
instance (ToJSON a) ⇒ ToJSON Foo where ...

The above (ToJSON a) constraint is not necessary and perhaps undesirable.
-}

-- | Generates a 'ToJSON' instance declaration for the given data type or
-- data family instance constructor.
deriveToJSON :: Options
             -- ^ Encoding options.
             -> Name
             -- ^ Name of the type for which to generate a 'ToJSON' instance
             -- declaration.
             -> Q [Dec]
deriveToJSON = deriveToJSONCommon toJSONClass

-- | Generates a 'ToJSON1' instance declaration for the given data type or
-- data family instance constructor.
deriveToJSON1 :: Options
              -- ^ Encoding options.
              -> Name
              -- ^ Name of the type for which to generate a 'ToJSON1' instance
              -- declaration.
              -> Q [Dec]
deriveToJSON1 = deriveToJSONCommon toJSON1Class

-- | Generates a 'ToJSON2' instance declaration for the given data type or
-- data family instance constructor.
deriveToJSON2 :: Options
              -- ^ Encoding options.
              -> Name
              -- ^ Name of the type for which to generate a 'ToJSON2' instance
              -- declaration.
              -> Q [Dec]
deriveToJSON2 = deriveToJSONCommon toJSON2Class

deriveToJSONCommon :: JSONClass
                   -- ^ The ToJSON variant being derived.
                   -> Options
                   -- ^ Encoding options.
                   -> Name
                   -- ^ Name of the type for which to generate an instance.
                   -> Q [Dec]
deriveToJSONCommon = deriveJSONClass [ (ToJSON,     \jc _ -> consToValue Value jc)
                                     , (ToEncoding, \jc _ -> consToValue Encoding jc)
                                     ]

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value'.
mkToJSON :: Options -- ^ Encoding options.
         -> Name -- ^ Name of the type to encode.
         -> Q Exp
mkToJSON = mkToJSONCommon toJSONClass

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value' by using the given encoding
-- function on occurrences of the last type parameter.
mkLiftToJSON :: Options -- ^ Encoding options.
             -> Name -- ^ Name of the type to encode.
             -> Q Exp
mkLiftToJSON = mkToJSONCommon toJSON1Class

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a 'Value' by using the given encoding
-- functions on occurrences of the last two type parameters.
mkLiftToJSON2 :: Options -- ^ Encoding options.
              -> Name -- ^ Name of the type to encode.
              -> Q Exp
mkLiftToJSON2 = mkToJSONCommon toJSON2Class

mkToJSONCommon :: JSONClass -- ^ Which class's method is being derived.
               -> Options -- ^ Encoding options.
               -> Name -- ^ Name of the encoded type.
               -> Q Exp
mkToJSONCommon = mkFunCommon (\jc _ -> consToValue Value jc)

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string.
mkToEncoding :: Options -- ^ Encoding options.
             -> Name -- ^ Name of the type to encode.
             -> Q Exp
mkToEncoding = mkToEncodingCommon toJSONClass

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string by using the given encoding
-- function on occurrences of the last type parameter.
mkLiftToEncoding :: Options -- ^ Encoding options.
                 -> Name -- ^ Name of the type to encode.
                 -> Q Exp
mkLiftToEncoding = mkToEncodingCommon toJSON1Class

-- | Generates a lambda expression which encodes the given data type or
-- data family instance constructor as a JSON string by using the given encoding
-- functions on occurrences of the last two type parameters.
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

-- | Helper function used by both 'deriveToJSON' and 'mkToJSON'. Generates
-- code to generate a 'Value' or 'Encoding' of a number of constructors. All
-- constructors must be from the same type.
consToValue :: ToJSONFun
            -- ^ The method ('toJSON' or 'toEncoding') being derived.
            -> JSONClass
            -- ^ The ToJSON variant being derived.
            -> Options
            -- ^ Encoding options.
            -> [Type]
            -- ^ The types from the data type/data family instance declaration
            -> [ConstructorInfo]
            -- ^ Constructors for which to generate JSON generating code.
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
      -- A single constructor is directly encoded. The constructor itself may be
      -- forgotten.
      [con] | not (tagSingleConstructors opts) -> [argsToValue letInsert target jc tvMap opts False con]
      _ | allNullaryToStringTag opts && all isNullary cons ->
              [ match (conP conName []) (normalB $ conStr target opts conName) []
              | con <- cons
              , let conName = constructorName con
              ]
        | otherwise -> [argsToValue letInsert target jc tvMap opts True con | con <- cons]

-- | Name of the constructor as a quoted 'Value' or 'Encoding'.
conStr :: ToJSONFun -> Options -> Name -> Q Exp
conStr Value opts = appE [|String|] . conTxt opts
conStr Encoding opts = appE [|E.text|] . conTxt opts

-- | Name of the constructor as a quoted 'Text'.
conTxt :: Options -> Name -> Q Exp
conTxt opts = appE [|T.pack|] . stringE . conString opts

-- | Name of the constructor.
conString :: Options -> Name -> String
conString opts = constructorTagModifier opts . nameBase

-- | If constructor is nullary.
isNullary :: ConstructorInfo -> Bool
isNullary ConstructorInfo { constructorVariant = NormalConstructor
                          , constructorFields  = tys } = null tys
isNullary _ = False

-- | Wrap fields of a non-record constructor. See 'sumToValue'.
opaqueSumToValue :: LetInsert -> ToJSONFun -> Options -> Bool -> Bool -> Name -> ExpQ -> ExpQ
opaqueSumToValue letInsert target opts multiCons nullary conName value =
  sumToValue letInsert target opts multiCons nullary conName
    value
    pairs
  where
    pairs contentsFieldName = pairE letInsert target contentsFieldName value

-- | Wrap fields of a record constructor. See 'sumToValue'.
recordSumToValue :: LetInsert -> ToJSONFun -> Options -> Bool -> Bool -> Name -> ExpQ -> ExpQ
recordSumToValue letInsert target opts multiCons nullary conName pairs =
  sumToValue letInsert target opts multiCons nullary conName
    (fromPairsE target pairs)
    (const pairs)

-- | Wrap fields of a constructor.
sumToValue
  :: LetInsert
  -- ^ Let insertion
  -> ToJSONFun
  -- ^ The method being derived.
  -> Options
  -- ^ Deriving options.
  -> Bool
  -- ^ Does this type have multiple constructors.
  -> Bool
  -- ^ Is this constructor nullary.
  -> Name
  -- ^ Constructor name.
  -> ExpQ
  -- ^ Fields of the constructor as a 'Value' or 'Encoding'.
  -> (String -> ExpQ)
  -- ^ Representation of an 'Object' fragment used for the 'TaggedObject'
  -- variant; of type @[(Text,Value)]@ or @[Encoding]@, depending on the method
  -- being derived.
  --
  -- - For non-records, produces a pair @"contentsFieldName":value@,
  --   given a @contentsFieldName@ as an argument. See 'opaqueSumToValue'.
  -- - For records, produces the list of pairs corresponding to fields of the
  --   encoded value (ignores the argument). See 'recordSumToValue'.
  -> ExpQ
sumToValue letInsert target opts multiCons nullary conName value pairs
    | multiCons =
        case sumEncoding opts of
          TwoElemArray ->
            array target [conStr target opts conName, value]
          TaggedObject{tagFieldName, contentsFieldName} ->
            -- TODO: Maybe throw an error in case
            -- tagFieldName overwrites a field in pairs.
            let tag = pairE letInsert target tagFieldName (conStr target opts conName)
                content = pairs contentsFieldName
            in fromPairsE target $
              if nullary then tag else infixApp tag [|(Monoid.<>)|] content
          ObjectWithSingleField ->
            objectE letInsert target [(conString opts conName, value)]
          UntaggedValue | nullary -> conStr target opts conName
          UntaggedValue -> value
    | otherwise = value

-- | Generates code to generate the JSON encoding of a single constructor.
argsToValue :: LetInsert -> ToJSONFun -> JSONClass -> TyVarMap -> Options -> Bool -> ConstructorInfo -> Q Match

-- Polyadic constructors with special case for unary constructors.
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
               -- Single argument is directly converted.
               [e] -> e
               -- Zero and multiple arguments are converted to a JSON array.
               es -> array target es

    match (conP conName $ map varP args)
          (normalB $ opaqueSumToValue letInsert target opts multiCons (null argTys') conName js)
          []

-- Records.
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

-- Infix constructors.
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

-- | Wrap a list of quoted 'Value's in a quoted 'Array' (of type 'Value').
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

-- | Wrap an associative list of keys and quoted values in a quoted 'Object'.
objectE :: LetInsert -> ToJSONFun -> [(String, ExpQ)] -> ExpQ
objectE letInsert target = fromPairsE target . mconcatE . fmap (uncurry (pairE letInsert target))

-- | 'mconcat' a list of fixed length.
--
-- > mconcatE [ [|x|], [|y|], [|z|] ] = [| x <> (y <> z) |]
mconcatE :: [ExpQ] -> ExpQ
mconcatE [] = [|Monoid.mempty|]
mconcatE [x] = x
mconcatE (x : xs) = infixApp x [|(Monoid.<>)|] (mconcatE xs)

fromPairsE :: ToJSONFun -> ExpQ -> ExpQ
fromPairsE _ = ([|fromPairs|] `appE`)

-- | Create (an encoding of) a key-value pair.
--
-- > pairE "k" [|v|] = [| pair "k" v |]
--
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

--------------------------------------------------------------------------------
-- FromJSON
--------------------------------------------------------------------------------

-- | Generates a 'FromJSON' instance declaration for the given data type or
-- data family instance constructor.
deriveFromJSON :: Options
               -- ^ Encoding options.
               -> Name
               -- ^ Name of the type for which to generate a 'FromJSON' instance
               -- declaration.
               -> Q [Dec]
deriveFromJSON = deriveFromJSONCommon fromJSONClass

-- | Generates a 'FromJSON1' instance declaration for the given data type or
-- data family instance constructor.
deriveFromJSON1 :: Options
                -- ^ Encoding options.
                -> Name
                -- ^ Name of the type for which to generate a 'FromJSON1' instance
                -- declaration.
                -> Q [Dec]
deriveFromJSON1 = deriveFromJSONCommon fromJSON1Class

-- | Generates a 'FromJSON2' instance declaration for the given data type or
-- data family instance constructor.
deriveFromJSON2 :: Options
                -- ^ Encoding options.
                -> Name
                -- ^ Name of the type for which to generate a 'FromJSON3' instance
                -- declaration.
                -> Q [Dec]
deriveFromJSON2 = deriveFromJSONCommon fromJSON2Class

deriveFromJSONCommon :: JSONClass
                     -- ^ The FromJSON variant being derived.
                     -> Options
                     -- ^ Encoding options.
                     -> Name
                     -- ^ Name of the type for which to generate an instance.
                     -- declaration.
                     -> Q [Dec]
deriveFromJSONCommon = deriveJSONClass [(ParseJSON, consFromJSON)]

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor.
mkParseJSON :: Options -- ^ Encoding options.
            -> Name -- ^ Name of the encoded type.
            -> Q Exp
mkParseJSON = mkParseJSONCommon fromJSONClass

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor by using the given parsing
-- function on occurrences of the last type parameter.
mkLiftParseJSON :: Options -- ^ Encoding options.
                -> Name -- ^ Name of the encoded type.
                -> Q Exp
mkLiftParseJSON = mkParseJSONCommon fromJSON1Class

-- | Generates a lambda expression which parses the JSON encoding of the given
-- data type or data family instance constructor by using the given parsing
-- functions on occurrences of the last two type parameters.
mkLiftParseJSON2 :: Options -- ^ Encoding options.
                 -> Name -- ^ Name of the encoded type.
                 -> Q Exp
mkLiftParseJSON2 = mkParseJSONCommon fromJSON2Class

mkParseJSONCommon :: JSONClass -- ^ Which class's method is being derived.
                  -> Options -- ^ Encoding options.
                  -> Name -- ^ Name of the encoded type.
                  -> Q Exp
mkParseJSONCommon = mkFunCommon consFromJSON

-- | Helper function used by both 'deriveFromJSON' and 'mkParseJSON'. Generates
-- code to parse the JSON encoding of a number of constructors. All constructors
-- must be from the same type.
consFromJSON :: JSONClass
             -- ^ The FromJSON variant being derived.
             -> Name
             -- ^ Name of the type to which the constructors belong.
             -> Options
             -- ^ Encoding options
             -> [Type]
             -- ^ The types from the data type/data family instance declaration
             -> [ConstructorInfo]
             -- ^ Constructors for which to generate JSON parsing code.
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

-- | Generates code to parse the JSON encoding of a single constructor.
parseArgs :: JSONClass -- ^ The FromJSON variant being derived.
          -> TyVarMap -- ^ Maps the last type variables to their decoding
                      --   function arguments.
          -> Name -- ^ Name of the type to which the constructor belongs.
          -> Options -- ^ Encoding options.
          -> ConstructorInfo -- ^ Constructor for which to generate JSON parsing code.
          -> Either (String, Name) Name -- ^ Left (valFieldName, objName) or
                                        --   Right valName
          -> Q Exp
-- Nullary constructors.
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

-- Unary constructors.
parseArgs jc tvMap _ _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = [argTy] }
  contents = do
    argTy' <- resolveTypeSynonyms argTy
    matchCases contents $ parseUnaryMatches jc tvMap argTy' conName

-- Polyadic constructors.
parseArgs jc tvMap tName _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = NormalConstructor
                  , constructorFields  = argTys }
  contents = do
    argTys' <- mapM resolveTypeSynonyms argTys
    let len = genericLength argTys'
    matchCases contents $ parseProduct jc tvMap argTys' tName conName len

-- Records.
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

-- Infix constructors. Apart from syntax these are the same as
-- polyadic constructors.
parseArgs jc tvMap tName _
  ConstructorInfo { constructorName    = conName
                  , constructorVariant = InfixConstructor
                  , constructorFields  = argTys }
  contents = do
    argTys' <- mapM resolveTypeSynonyms argTys
    matchCases contents $ parseProduct jc tvMap argTys' tName conName 2

-- | Generates code to parse the JSON encoding of an n-ary
-- constructor.
parseProduct :: JSONClass -- ^ The FromJSON variant being derived.
             -> TyVarMap -- ^ Maps the last type variables to their decoding
                         --   function arguments.
             -> [Type] -- ^ The argument types of the constructor.
             -> Name -- ^ Name of the type to which the constructor belongs.
             -> Name -- ^ 'Con'structor name.
             -> Integer -- ^ 'Con'structor arity.
             -> [Q Match]
parseProduct jc tvMap argTys tName conName numArgs =
    [ do arr <- newName "arr"
         -- List of: "parseJSON (arr `V.unsafeIndex` <IX>)"
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

--------------------------------------------------------------------------------
-- Parsing errors
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Shared ToJSON and FromJSON code
--------------------------------------------------------------------------------

-- | Functionality common to 'deriveJSON', 'deriveJSON1', and 'deriveJSON2'.
deriveJSONBoth :: (Options -> Name -> Q [Dec])
               -- ^ Function which derives a flavor of 'ToJSON'.
               -> (Options -> Name -> Q [Dec])
               -- ^ Function which derives a flavor of 'FromJSON'.
               -> Options
               -- ^ Encoding options.
               -> Name
               -- ^ Name of the type for which to generate 'ToJSON' and 'FromJSON'
               -- instances.
               -> Q [Dec]
deriveJSONBoth dtj dfj opts name =
    liftM2 (++) (dtj opts name) (dfj opts name)

-- | Functionality common to @deriveToJSON(1)(2)@ and @deriveFromJSON(1)(2)@.
deriveJSONClass :: [(JSONFun, JSONClass -> Name -> Options -> [Type]
                                        -> [ConstructorInfo] -> Q Exp)]
                -- ^ The class methods and the functions which derive them.
                -> JSONClass
                -- ^ The class for which to generate an instance.
                -> Options
                -- ^ Encoding options.
                -> Name
                -- ^ Name of the type for which to generate a class instance
                -- declaration.
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
            -- ^ The function which derives the expression.
            -> JSONClass
            -- ^ Which class's method is being derived.
            -> Options
            -- ^ Encoding options.
            -> Name
            -- ^ Name of the encoded type.
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
      -- We force buildTypeInstance here since it performs some checks for whether
      -- or not the provided datatype's kind matches the derived method's
      -- typeclass, and produces errors if it can't.
      !_ <- buildTypeInstance parentName jc ctxt instTys variant
      consFun jc parentName opts instTys cons

data FunArg = Omit | Single | Plural deriving (Eq)

dispatchFunByType :: JSONClass
                  -> JSONFun
                  -> Name
                  -> TyVarMap
                  -> FunArg -- Plural if we are using the function argument that works
                            -- on lists (e.g., [a] -> Value). Single is we are using
                            -- the function argument that works on single values
                            -- (e.g., a -> Value). Omit if we use it to check omission
                            -- (e.g. a -> Bool)
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

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

-- For the given Types, generate an instance context and head.
buildTypeInstance :: Name
                  -- ^ The type constructor or data family name
                  -> JSONClass
                  -- ^ The typeclass to derive
                  -> Cxt
                  -- ^ The datatype context
                  -> [Type]
                  -- ^ The types to instantiate the instance with
                  -> DatatypeVariant
                  -- ^ Are we dealing with a data family instance or not
                  -> Q (Cxt, Type)
buildTypeInstance tyConName jc dataCxt varTysOrig variant = do
    -- Make sure to expand through type/kind synonyms! Otherwise, the
    -- eta-reduction check might get tripped up over type variables in a
    -- synonym that are actually dropped.
    -- (See GHC Trac #11416 for a scenario where this actually happened.)
    varTysExp <- mapM resolveTypeSynonyms varTysOrig

    let remainingLength :: Int
        remainingLength = length varTysOrig - arityInt jc

        droppedTysExp :: [Type]
        droppedTysExp = drop remainingLength varTysExp

        droppedStarKindStati :: [StarKindStatus]
        droppedStarKindStati = map canRealizeKindStar droppedTysExp

    -- Check there are enough types to drop and that all of them are either of
    -- kind * or kind k (for some kind variable k). If not, throw an error.
    when (remainingLength < 0 || elem NotKindStar droppedStarKindStati) $
      derivingKindError jc tyConName

    let droppedKindVarNames :: [Name]
        droppedKindVarNames = catKindVarNames droppedStarKindStati

        -- Substitute kind * for any dropped kind variables
        varTysExpSubst :: [Type]
        varTysExpSubst = map (substNamesWithKindStar droppedKindVarNames) varTysExp

        remainingTysExpSubst, droppedTysExpSubst :: [Type]
        (remainingTysExpSubst, droppedTysExpSubst) =
          splitAt remainingLength varTysExpSubst

        -- All of the type variables mentioned in the dropped types
        -- (post-synonym expansion)
        droppedTyVarNames :: [Name]
        droppedTyVarNames = freeVariables droppedTysExpSubst

    -- If any of the dropped types were polykinded, ensure that they are of kind *
    -- after substituting * for the dropped kind variables. If not, throw an error.
    unless (all hasKindStar droppedTysExpSubst) $
      derivingKindError jc tyConName

    let preds    :: [Maybe Pred]
        kvNames  :: [[Name]]
        kvNames' :: [Name]
        -- Derive instance constraints (and any kind variables which are specialized
        -- to * in those constraints)
        (preds, kvNames) = unzip $ map (deriveConstraint jc) remainingTysExpSubst
        kvNames' = concat kvNames

        -- Substitute the kind variables specialized in the constraints with *
        remainingTysExpSubst' :: [Type]
        remainingTysExpSubst' =
          map (substNamesWithKindStar kvNames') remainingTysExpSubst

        -- We now substitute all of the specialized-to-* kind variable names with
        -- *, but in the original types, not the synonym-expanded types. The reason
        -- we do this is a superficial one: we want the derived instance to resemble
        -- the datatype written in source code as closely as possible. For example,
        -- for the following data family instance:
        --
        --   data family Fam a
        --   newtype instance Fam String = Fam String
        --
        -- We'd want to generate the instance:
        --
        --   instance C (Fam String)
        --
        -- Not:
        --
        --   instance C (Fam [Char])
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
        -- See Note [Kind signatures in derived instances] for an explanation
        -- of the isDataFamily check.
        remainingTysOrigSubst' =
          if isDataFamily
             then remainingTysOrigSubst
             else map unSigT remainingTysOrigSubst

        instanceCxt :: Cxt
        instanceCxt = catMaybes preds

        instanceType :: Type
        instanceType = AppT (ConT $ jsonClassName jc)
                     $ applyTyCon tyConName remainingTysOrigSubst'

    -- If the datatype context mentions any of the dropped type variables,
    -- we can't derive an instance, so throw an error.
    when (any (`predMentionsName` droppedTyVarNames) dataCxt) $
      datatypeContextError tyConName instanceType
    -- Also ensure the dropped types can be safely eta-reduced. Otherwise,
    -- throw an error.
    unless (canEtaReduce remainingTysExpSubst' droppedTysExpSubst) $
      etaReductionError instanceType
    return (instanceCxt, instanceType)

-- | Attempt to derive a constraint on a Type. If successful, return
-- Just the constraint and any kind variable names constrained to *.
-- Otherwise, return Nothing and the empty list.
--
-- See Note [Type inference in derived instances] for the heuristics used to
-- come up with constraints.
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

{-
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
-}

checkExistentialContext :: JSONClass -> TyVarMap -> Cxt -> Name
                        -> Q a -> Q a
checkExistentialContext jc tvMap ctxt conName q =
  if (any (`predMentionsName` M.keys tvMap) ctxt
       || M.size tvMap < arityInt jc)
       && not (allowExQuant jc)
     then existentialContextError conName
     else q

{-
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
-}

-- A mapping of type variable Names to their encoding/decoding function Names.
-- For example, in a ToJSON2 declaration, a TyVarMap might look like
--
-- { a ~> (o1, tj1, tjl1)
-- , b ~> (o2, tj2, tjl2) }
--
-- where a and b are the last two type variables of the datatype,
-- o1 and o2 are function argument of types (a -> Bool),
-- tj1 and tjl1 are the function arguments of types (a -> Value)
-- and ([a] -> Value), and tj2 and tjl2 are the function arguments of types (b -> Value) and ([b] -> Value).
type TyVarMap = Map Name (Name, Name, Name)

-- | Returns True if a Type has kind *.
hasKindStar :: Type -> Bool
hasKindStar VarT{}         = True
hasKindStar (SigT _ StarT) = True
hasKindStar _              = False

-- Returns True is a kind is equal to *, or if it is a kind variable.
isStarOrVar :: Kind -> Bool
isStarOrVar StarT  = True
isStarOrVar VarT{} = True
isStarOrVar _      = False

-- Generate a list of fresh names with a common prefix, and numbered suffixes.
newNameList :: String -> Int -> Q [Name]
newNameList prefix len = mapM newName [prefix ++ show n | n <- [1..len]]

-- | @hasKindVarChain n kind@ Checks if @kind@ is of the form
-- k_0 -> k_1 -> ... -> k_(n-1), where k0, k1, ..., and k_(n-1) can be * or
-- kind variables.
hasKindVarChain :: Int -> Type -> Maybe [Name]
hasKindVarChain kindArrows t =
  let uk = uncurryKind (tyKind t)
  in if (NE.length uk - 1 == kindArrows) && all isStarOrVar uk
        then Just (concatMap freeVariables uk)
        else Nothing

-- | If a Type is a SigT, returns its kind signature. Otherwise, return *.
tyKind :: Type -> Kind
tyKind (SigT _ k) = k
tyKind _          = starK

-- | Extract Just the Name from a type variable. If the argument Type is not a
-- type variable, return Nothing.
varTToNameMaybe :: Type -> Maybe Name
varTToNameMaybe (VarT n)   = Just n
varTToNameMaybe (SigT t _) = varTToNameMaybe t
varTToNameMaybe _          = Nothing

-- | Extract the Name from a type variable. If the argument Type is not a
-- type variable, throw an error.
varTToName :: Type -> Name
varTToName = fromMaybe (error "Not a type variable!") . varTToNameMaybe

flatten3 :: [(a,a,a)] -> [a]
flatten3 = foldr (\(a,b,c) xs -> a:b:c:xs) []

triple :: [a] -> [a]
triple = foldr (\x xs -> x:x:x:xs) []

-- | Fully applies a type constructor to its type variables.
applyTyCon :: Name -> [Type] -> Type
applyTyCon = foldl' AppT . ConT

-- | Is the given type a variable?
isTyVar :: Type -> Bool
isTyVar (VarT _)   = True
isTyVar (SigT t _) = isTyVar t
isTyVar _          = False

-- | Detect if a Name in a list of provided Names occurs as an argument to some
-- type family. This makes an effort to exclude /oversaturated/ arguments to
-- type families. For instance, if one declared the following type family:
--
-- @
-- type family F a :: Type -> Type
-- @
--
-- Then in the type @F a b@, we would consider @a@ to be an argument to @F@,
-- but not @b@.
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

-- | Peel off a kind signature from a Type (if it has one).
unSigT :: Type -> Type
unSigT (SigT t _) = t
unSigT t          = t

-- | Are all of the items in a list (which have an ordering) distinct?
--
-- This uses Set (as opposed to nub) for better asymptotic time complexity.
allDistinct :: Ord a => [a] -> Bool
allDistinct = allDistinct' Set.empty
  where
    allDistinct' :: Ord a => Set a -> [a] -> Bool
    allDistinct' uniqs (x:xs)
        | x `Set.member` uniqs = False
        | otherwise            = allDistinct' (Set.insert x uniqs) xs
    allDistinct' _ _           = True

-- | Does the given type mention any of the Names in the list?
mentionsName :: Type -> [Name] -> Bool
mentionsName = go
  where
    go :: Type -> [Name] -> Bool
    go (AppT t1 t2) names = go t1 names || go t2 names
    go (SigT t _k)  names = go t names
                              || go _k names
    go (VarT n)     names = n `elem` names
    go _            _     = False

-- | Does an instance predicate mention any of the Names in the list?
predMentionsName :: Pred -> [Name] -> Bool
predMentionsName = mentionsName

-- | Split an applied type into its individual components. For example, this:
--
-- @
-- Either Int Char
-- @
--
-- would split to this:
--
-- @
-- [Either, Int, Char]
-- @
unapplyTy :: Type -> NonEmpty Type
unapplyTy = NE.reverse . go
  where
    go :: Type -> NonEmpty Type
    go (AppT t1 t2)    = t2 <| go t1
    go (SigT t _)      = go t
    go (ForallT _ _ t) = go t
    go t               = t :| []

-- | Split a type signature by the arrows on its spine. For example, this:
--
-- @
-- forall a b. (a ~ b) => (a -> b) -> Char -> ()
-- @
--
-- would split to this:
--
-- @
-- (a ~ b, [a -> b, Char, ()])
-- @
uncurryTy :: Type -> (Cxt, NonEmpty Type)
uncurryTy (AppT (AppT ArrowT t1) t2) =
  let (ctxt, tys) = uncurryTy t2
  in (ctxt, t1 <| tys)
uncurryTy (SigT t _) = uncurryTy t
uncurryTy (ForallT _ ctxt t) =
  let (ctxt', tys) = uncurryTy t
  in (ctxt ++ ctxt', tys)
uncurryTy t = ([], t :| [])

-- | Like uncurryType, except on a kind level.
uncurryKind :: Kind -> NonEmpty Kind
uncurryKind = snd . uncurryTy

createKindChain :: Int -> Kind
createKindChain = go starK
  where
    go :: Kind -> Int -> Kind
    go k 0 = k
    go k !n = go (AppT (AppT ArrowT StarT) k) (n - 1)

-- | Makes a string literal expression from a constructor's name.
conNameExp :: Options -> ConstructorInfo -> Q Exp
conNameExp opts = litE
                . stringL
                . constructorTagModifier opts
                . nameBase
                . constructorName

-- | Extracts a record field label.
fieldLabel :: Options -- ^ Encoding options
           -> Name
           -> String
fieldLabel opts = fieldLabelModifier opts . nameBase

-- | The name of the outermost 'Value' constructor.
valueConName :: Value -> String
valueConName (Object _) = "Object"
valueConName (Array  _) = "Array"
valueConName (String _) = "String"
valueConName (Number _) = "Number"
valueConName (Bool   _) = "Boolean"
valueConName Null       = "Null"

applyCon :: Name -> Name -> Pred
applyCon con t = AppT (ConT con) (VarT t)

-- | Checks to see if the last types in a data family instance can be safely eta-
-- reduced (i.e., dropped), given the other types. This checks for three conditions:
--
-- (1) All of the dropped types are type variables
-- (2) All of the dropped types are distinct
-- (3) None of the remaining types mention any of the dropped types
canEtaReduce :: [Type] -> [Type] -> Bool
canEtaReduce remaining dropped =
       all isTyVar dropped
    && allDistinct droppedNames -- Make sure not to pass something of type [Type], since Type
                                -- didn't have an Ord instance until template-haskell-2.10.0.0
    && not (any (`mentionsName` droppedNames) remaining)
  where
    droppedNames :: [Name]
    droppedNames = map varTToName dropped

-------------------------------------------------------------------------------
-- Expanding type synonyms
-------------------------------------------------------------------------------

applySubstitutionKind :: Map Name Kind -> Type -> Type
applySubstitutionKind = applySubstitution

substNameWithKind :: Name -> Kind -> Type -> Type
substNameWithKind n k = applySubstitutionKind (M.singleton n k)

substNamesWithKindStar :: [Name] -> Type -> Type
substNamesWithKindStar ns t = foldr' (`substNameWithKind` starK) t ns

-------------------------------------------------------------------------------
-- Error messages
-------------------------------------------------------------------------------

-- | Either the given data type doesn't have enough type variables, or one of
-- the type variables to be eta-reduced cannot realize kind *.
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

-- | One of the last type variables cannot be eta-reduced (see the canEtaReduce
-- function for the criteria it would have to meet).
etaReductionError :: Type -> Q a
etaReductionError instanceType = fail $
    "Cannot eta-reduce to an instance of form \n\tinstance (...) => "
    ++ pprint instanceType

-- | The data type has a DatatypeContext which mentions one of the eta-reduced
-- type variables.
datatypeContextError :: Name -> Type -> Q a
datatypeContextError dataName instanceType = fail
    . showString "Can't make a derived instance of ‘"
    . showString (pprint instanceType)
    . showString "‘:\n\tData type ‘"
    . showString (nameBase dataName)
    . showString "‘ must not have a class context involving the last type argument(s)"
    $ ""

-- | The data type mentions one of the n eta-reduced type variables in a place other
-- than the last nth positions of a data type in a constructor's field.
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

-- | The data type has an existential constraint which mentions one of the
-- eta-reduced type variables.
existentialContextError :: Name -> a
existentialContextError conName = error
  . showString "Constructor ‘"
  . showString (nameBase conName)
  . showString "‘ must be truly polymorphic in the last argument(s) of the data type"
  $ ""

-------------------------------------------------------------------------------
-- Class-specific constants
-------------------------------------------------------------------------------

-- | A representation of the arity of the ToJSON/FromJSON typeclass being derived.
data Arity = Arity0 | Arity1 | Arity2
  deriving (Enum, Eq, Ord)

-- | Whether ToJSON(1)(2) or FromJSON(1)(2) is being derived.
data Direction = To | From

-- | A representation of which typeclass method is being spliced in.
data JSONFun = ToJSON | ToEncoding | ParseJSON

-- | A refinement of JSONFun to [ToJSON, ToEncoding].
data ToJSONFun = Value | Encoding

targetToJSONFun :: ToJSONFun -> JSONFun
targetToJSONFun Value = ToJSON
targetToJSONFun Encoding = ToEncoding

-- | A representation of which typeclass is being derived.
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

-------------------------------------------------------------------------------
-- StarKindStatus
-------------------------------------------------------------------------------

-- | Whether a type is not of kind *, is of kind *, or is a kind variable.
data StarKindStatus = NotKindStar
                    | KindStar
                    | IsKindVar Name
  deriving Eq

-- | Does a Type have kind * or k (for some kind variable k)?
canRealizeKindStar :: Type -> StarKindStatus
canRealizeKindStar t = case t of
    _ | hasKindStar t -> KindStar
    SigT _ (VarT k) -> IsKindVar k
    _ -> NotKindStar

-- | Returns 'Just' the kind variable 'Name' of a 'StarKindStatus' if it exists.
-- Otherwise, returns 'Nothing'.
starKindStatusToName :: StarKindStatus -> Maybe Name
starKindStatusToName (IsKindVar n) = Just n
starKindStatusToName _             = Nothing

-- | Concat together all of the StarKindStatuses that are IsKindVar and extract
-- the kind variables' Names out.
catKindVarNames :: [StarKindStatus] -> [Name]
catKindVarNames = mapMaybe starKindStatusToName

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Data.Vector
-- Copyright   : (c) Roman Leshchinskiy 2008-2010
--                   Alexey Kuleshevich 2020-2022
--                   Aleksey Khudyakov 2020-2022
--                   Andrew Lelechenko 2020-2022
-- License     : BSD-style
--
-- Maintainer  : Haskell Libraries Team <libraries@haskell.org>
-- Stability   : experimental
-- Portability : non-portable
--
-- A library for boxed vectors (that is, polymorphic arrays capable of
-- holding any Haskell value). The vectors come in two flavours:
--
--  * mutable
--
--  * immutable
--
-- They support a rich interface of both list-like operations and bulk
-- array operations.
--
-- For unboxed arrays, use "Data.Vector.Unboxed".

module Data.Vector (
  -- * Boxed vectors
  Vector, MVector,

  -- * Accessors

  -- ** Length information
  length, null,

  -- ** Indexing
  (!), (!?), head, last,
  unsafeIndex, unsafeHead, unsafeLast,

  -- ** Monadic indexing
  indexM, headM, lastM,
  unsafeIndexM, unsafeHeadM, unsafeLastM,

  -- ** Extracting subvectors (slicing)
  slice, init, tail, take, drop, splitAt, uncons, unsnoc,
  unsafeSlice, unsafeInit, unsafeTail, unsafeTake, unsafeDrop,

  -- * Construction

  -- ** Initialisation
  empty, singleton, replicate, generate, iterateN,

  -- ** Monadic initialisation
  replicateM, generateM, iterateNM, create, createT,

  -- ** Unfolding
  unfoldr, unfoldrN, unfoldrExactN,
  unfoldrM, unfoldrNM, unfoldrExactNM,
  constructN, constructrN,

  -- ** Enumeration
  enumFromN, enumFromStepN, enumFromTo, enumFromThenTo,

  -- ** Concatenation
  cons, snoc, (++), concat,

  -- ** Restricting memory usage
  force,

  -- * Modifying vectors

  -- ** Bulk updates
  (//), update, update_,
  unsafeUpd, unsafeUpdate, unsafeUpdate_,

  -- ** Accumulations
  accum, accumulate, accumulate_,
  unsafeAccum, unsafeAccumulate, unsafeAccumulate_,

  -- ** Permutations
  reverse, backpermute, unsafeBackpermute,

  -- ** Safe destructive updates
  modify,

  -- * Elementwise operations

  -- ** Indexing
  indexed,

  -- ** Mapping
  map, imap, concatMap,

  -- ** Monadic mapping
  mapM, imapM, mapM_, imapM_, forM, forM_,
  iforM, iforM_,

  -- ** Zipping
  zipWith, zipWith3, zipWith4, zipWith5, zipWith6,
  izipWith, izipWith3, izipWith4, izipWith5, izipWith6,
  zip, zip3, zip4, zip5, zip6,

  -- ** Monadic zipping
  zipWithM, izipWithM, zipWithM_, izipWithM_,

  -- ** Unzipping
  unzip, unzip3, unzip4, unzip5, unzip6,

  -- * Working with predicates

  -- ** Filtering
  filter, ifilter, filterM, uniq,
  mapMaybe, imapMaybe,
  mapMaybeM, imapMaybeM,
  catMaybes,
  takeWhile, dropWhile,

  -- ** Partitioning
  partition, unstablePartition, partitionWith, span, break, spanR, breakR, groupBy, group,

  -- ** Searching
  elem, notElem, find, findIndex, findIndexR, findIndices, elemIndex, elemIndices,

  -- * Folding
  foldl, foldl1, foldl', foldl1', foldr, foldr1, foldr', foldr1',
  ifoldl, ifoldl', ifoldr, ifoldr',
  foldMap, foldMap',

  -- ** Specialised folds
  all, any, and, or,
  sum, product,
  maximum, maximumBy, maximumOn,
  minimum, minimumBy, minimumOn,
  minIndex, minIndexBy, maxIndex, maxIndexBy,

  -- ** Monadic folds
  foldM, ifoldM, foldM', ifoldM',
  fold1M, fold1M',foldM_, ifoldM_,
  foldM'_, ifoldM'_, fold1M_, fold1M'_,

  -- ** Monadic sequencing
  sequence, sequence_,

  -- * Scans
  prescanl, prescanl',
  postscanl, postscanl',
  scanl, scanl', scanl1, scanl1',
  iscanl, iscanl',
  prescanr, prescanr',
  postscanr, postscanr',
  scanr, scanr', scanr1, scanr1',
  iscanr, iscanr',

  -- ** Comparisons
  eqBy, cmpBy,

  -- * Conversions

  -- ** Lists
  toList, Data.Vector.fromList, Data.Vector.fromListN,

  -- ** Arrays
  toArray, fromArray, toArraySlice, unsafeFromArraySlice,

  -- ** Other vector types
  G.convert,

  -- ** Mutable vectors
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


-- | Boxed vectors, supporting efficient slicing.
data Vector a = Vector {-# UNPACK #-} !Int
                       {-# UNPACK #-} !Int
                       {-# UNPACK #-} !(Array a)
        deriving ( Typeable )

liftRnfV :: (a -> ()) -> Vector a -> ()
liftRnfV elemRnf = foldl' (\_ -> elemRnf) ()

instance NFData a => NFData (Vector a) where
  rnf = liftRnfV rnf
  {-# INLINEABLE rnf #-}

#if MIN_VERSION_deepseq(1,4,3)
-- | @since 0.12.1.0
instance NFData1 Vector where
  liftRnf = liftRnfV
  {-# INLINEABLE liftRnf #-}
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
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector i n marr)
    = Vector i n `liftM` unsafeFreezeArray marr

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector i n arr)
    = MVector i n `liftM` unsafeThawArray arr

  {-# INLINE basicLength #-}
  basicLength (Vector _ n _) = n

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice j n (Vector i _ arr) = Vector (i+j) n arr

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector i _ arr) j = indexArrayM arr (i+j)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector i n dst) (Vector j _ src)
    = copyArray dst i src j n

-- See http://trac.haskell.org/vector/ticket/12
instance Eq a => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (G.stream xs) (G.stream ys)

-- See http://trac.haskell.org/vector/ticket/12
instance Ord a => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (G.stream xs) (G.stream ys)

  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (G.stream xs) (G.stream ys) == LT

  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= GT

  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (G.stream xs) (G.stream ys) == GT

  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (G.stream xs) (G.stream ys) /= LT

instance Eq1 Vector where
  liftEq eq xs ys = Bundle.eqBy eq (G.stream xs) (G.stream ys)

instance Ord1 Vector where
  liftCompare cmp xs ys = Bundle.cmpBy cmp (G.stream xs) (G.stream ys)

instance Semigroup (Vector a) where
  {-# INLINE (<>) #-}
  (<>) = (++)

  {-# INLINE sconcat #-}
  sconcat = G.concatNE

instance Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = empty

  {-# INLINE mappend #-}
  mappend = (<>)

  {-# INLINE mconcat #-}
  mconcat = concat

instance Functor Vector where
  {-# INLINE fmap #-}
  fmap = map

  {-# INLINE (<$) #-}
  (<$) = map . const

instance Monad Vector where
  {-# INLINE return #-}
  return = Applicative.pure

  {-# INLINE (>>=) #-}
  (>>=) = flip concatMap

#if !(MIN_VERSION_base(4,13,0))
  {-# INLINE fail #-}
  fail = Fail.fail -- == \ _str -> empty
#endif

-- | @since 0.12.1.0
instance Fail.MonadFail Vector where
  {-# INLINE fail #-}
  fail _ = empty

instance MonadPlus Vector where
  {-# INLINE mzero #-}
  mzero = empty

  {-# INLINE mplus #-}
  mplus = (++)

instance MonadZip Vector where
  {-# INLINE mzip #-}
  mzip = zip

  {-# INLINE mzipWith #-}
  mzipWith = zipWith

  {-# INLINE munzip #-}
  munzip = unzip

-- | This instance has the same semantics as the one for lists.
--
--  @since 0.12.2.0
instance MonadFix Vector where
  -- We take care to dispose of v0 as soon as possible (see headM docs).
  --
  -- It's perfectly safe to use non-monadic indexing within generate
  -- call since intermediate vector won't be created until result's
  -- value is demanded.
  {-# INLINE mfix #-}
  mfix f
    | null v0 = empty
    -- We take first element of resulting vector from v0 and create
    -- rest using generate. Note that cons should fuse with generate
    | otherwise = runST $ do
        h <- headM v0
        return $ cons h $
          generate (lv0 - 1) $
            \i -> fix (\a -> f a ! (i + 1))
    where
      -- Used to calculate size of resulting vector
      v0 = fix (f . head)
      !lv0 = length v0

instance Applicative.Applicative Vector where
  {-# INLINE pure #-}
  pure = singleton

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Applicative.Alternative Vector where
  {-# INLINE empty #-}
  empty = empty

  {-# INLINE (<|>) #-}
  (<|>) = (++)

instance Foldable.Foldable Vector where
  {-# INLINE foldr #-}
  foldr = foldr

  {-# INLINE foldl #-}
  foldl = foldl

  {-# INLINE foldr1 #-}
  foldr1 = foldr1

  {-# INLINE foldl1 #-}
  foldl1 = foldl1

  {-# INLINE foldr' #-}
  foldr' = foldr'

  {-# INLINE foldl' #-}
  foldl' = foldl'

  {-# INLINE toList #-}
  toList = toList

  {-# INLINE length #-}
  length = length

  {-# INLINE null #-}
  null = null

  {-# INLINE elem #-}
  elem = elem

  {-# INLINE maximum #-}
  maximum = maximum

  {-# INLINE minimum #-}
  minimum = minimum

  {-# INLINE sum #-}
  sum = sum

  {-# INLINE product #-}
  product = product

instance Traversable.Traversable Vector where
  {-# INLINE traverse #-}
  traverse f xs =
      -- Get the length of the vector in /O(1)/ time
      let !n = G.length xs
      -- Use fromListN to be more efficient in construction of resulting vector
      -- Also behaves better with compact regions, preventing runtime exceptions
      in  Data.Vector.fromListN n Applicative.<$> Traversable.traverse f (toList xs)

  {-# INLINE mapM #-}
  mapM = mapM

  {-# INLINE sequence #-}
  sequence = sequence

-- Length information
-- ------------------

-- | /O(1)/ Yield the length of the vector.
length :: Vector a -> Int
{-# INLINE length #-}
length = G.length

-- | /O(1)/ Test whether a vector is empty.
null :: Vector a -> Bool
{-# INLINE null #-}
null = G.null

-- Indexing
-- --------

-- | O(1) Indexing.
(!) :: Vector a -> Int -> a
{-# INLINE (!) #-}
(!) = (G.!)

-- | O(1) Safe indexing.
(!?) :: Vector a -> Int -> Maybe a
{-# INLINE (!?) #-}
(!?) = (G.!?)

-- | /O(1)/ First element.
head :: Vector a -> a
{-# INLINE head #-}
head = G.head

-- | /O(1)/ Last element.
last :: Vector a -> a
{-# INLINE last #-}
last = G.last

-- | /O(1)/ Unsafe indexing without bounds checking.
unsafeIndex :: Vector a -> Int -> a
{-# INLINE unsafeIndex #-}
unsafeIndex = G.unsafeIndex

-- | /O(1)/ First element, without checking if the vector is empty.
unsafeHead :: Vector a -> a
{-# INLINE unsafeHead #-}
unsafeHead = G.unsafeHead

-- | /O(1)/ Last element, without checking if the vector is empty.
unsafeLast :: Vector a -> a
{-# INLINE unsafeLast #-}
unsafeLast = G.unsafeLast

-- Monadic indexing
-- ----------------

-- | /O(1)/ Indexing in a monad.
--
-- The monad allows operations to be strict in the vector when necessary.
-- Suppose vector copying is implemented like this:
--
-- > copy mv v = ... write mv i (v ! i) ...
--
-- For lazy vectors, @v ! i@ would not be evaluated which means that @mv@
-- would unnecessarily retain a reference to @v@ in each element written.
--
-- With 'indexM', copying can be implemented like this instead:
--
-- > copy mv v = ... do
-- >                   x <- indexM v i
-- >                   write mv i x
--
-- Here, no references to @v@ are retained because indexing (but /not/ the
-- element) is evaluated eagerly.
indexM :: Monad m => Vector a -> Int -> m a
{-# INLINE indexM #-}
indexM = G.indexM

-- | /O(1)/ First element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
headM :: Monad m => Vector a -> m a
{-# INLINE headM #-}
headM = G.headM

-- | /O(1)/ Last element of a vector in a monad. See 'indexM' for an
-- explanation of why this is useful.
lastM :: Monad m => Vector a -> m a
{-# INLINE lastM #-}
lastM = G.lastM

-- | /O(1)/ Indexing in a monad, without bounds checks. See 'indexM' for an
-- explanation of why this is useful.
unsafeIndexM :: Monad m => Vector a -> Int -> m a
{-# INLINE unsafeIndexM #-}
unsafeIndexM = G.unsafeIndexM

-- | /O(1)/ First element in a monad, without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeHeadM :: Monad m => Vector a -> m a
{-# INLINE unsafeHeadM #-}
unsafeHeadM = G.unsafeHeadM

-- | /O(1)/ Last element in a monad, without checking for empty vectors.
-- See 'indexM' for an explanation of why this is useful.
unsafeLastM :: Monad m => Vector a -> m a
{-# INLINE unsafeLastM #-}
unsafeLastM = G.unsafeLastM

-- Extracting subvectors (slicing)
-- -------------------------------

-- | /O(1)/ Yield a slice of the vector without copying it. The vector must
-- contain at least @i+n@ elements.
slice :: Int   -- ^ @i@ starting index
                 -> Int   -- ^ @n@ length
                 -> Vector a
                 -> Vector a
{-# INLINE slice #-}
slice = G.slice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty.
init :: Vector a -> Vector a
{-# INLINE init #-}
init = G.init

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty.
tail :: Vector a -> Vector a
{-# INLINE tail #-}
tail = G.tail

-- | /O(1)/ Yield at the first @n@ elements without copying. The vector may
-- contain less than @n@ elements, in which case it is returned unchanged.
take :: Int -> Vector a -> Vector a
{-# INLINE take #-}
take = G.take

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector may
-- contain less than @n@ elements, in which case an empty vector is returned.
drop :: Int -> Vector a -> Vector a
{-# INLINE drop #-}
drop = G.drop

-- | /O(1)/ Yield the first @n@ elements paired with the remainder, without copying.
--
-- Note that @'splitAt' n v@ is equivalent to @('take' n v, 'drop' n v)@,
-- but slightly more efficient.
--
-- @since 0.7.1
splitAt :: Int -> Vector a -> (Vector a, Vector a)
{-# INLINE splitAt #-}
splitAt = G.splitAt

-- | /O(1)/ Yield the 'head' and 'tail' of the vector, or 'Nothing' if
-- the vector is empty.
--
-- @since 0.12.2.0
uncons :: Vector a -> Maybe (a, Vector a)
{-# INLINE uncons #-}
uncons = G.uncons

-- | /O(1)/ Yield the 'last' and 'init' of the vector, or 'Nothing' if
-- the vector is empty.
--
-- @since 0.12.2.0
unsnoc :: Vector a -> Maybe (Vector a, a)
{-# INLINE unsnoc #-}
unsnoc = G.unsnoc

-- | /O(1)/ Yield a slice of the vector without copying. The vector must
-- contain at least @i+n@ elements, but this is not checked.
unsafeSlice :: Int   -- ^ @i@ starting index
                       -> Int   -- ^ @n@ length
                       -> Vector a
                       -> Vector a
{-# INLINE unsafeSlice #-}
unsafeSlice = G.unsafeSlice

-- | /O(1)/ Yield all but the last element without copying. The vector may not
-- be empty, but this is not checked.
unsafeInit :: Vector a -> Vector a
{-# INLINE unsafeInit #-}
unsafeInit = G.unsafeInit

-- | /O(1)/ Yield all but the first element without copying. The vector may not
-- be empty, but this is not checked.
unsafeTail :: Vector a -> Vector a
{-# INLINE unsafeTail #-}
unsafeTail = G.unsafeTail

-- | /O(1)/ Yield the first @n@ elements without copying. The vector must
-- contain at least @n@ elements, but this is not checked.
unsafeTake :: Int -> Vector a -> Vector a
{-# INLINE unsafeTake #-}
unsafeTake = G.unsafeTake

-- | /O(1)/ Yield all but the first @n@ elements without copying. The vector
-- must contain at least @n@ elements, but this is not checked.
unsafeDrop :: Int -> Vector a -> Vector a
{-# INLINE unsafeDrop #-}
unsafeDrop = G.unsafeDrop

-- Initialisation
-- --------------

-- | /O(1)/ The empty vector.
empty :: Vector a
{-# INLINE empty #-}
empty = G.empty

-- | /O(1)/ A vector with exactly one element.
singleton :: a -> Vector a
{-# INLINE singleton #-}
singleton = G.singleton

-- | /O(n)/ A vector of the given length with the same value in each position.
replicate :: Int -> a -> Vector a
{-# INLINE replicate #-}
replicate = G.replicate

-- | /O(n)/ Construct a vector of the given length by applying the function to
-- each index.
generate :: Int -> (Int -> a) -> Vector a
{-# INLINE generate #-}
generate = G.generate

-- | /O(n)/ Apply the function \(\max(n - 1, 0)\) times to an initial value, producing a vector
-- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- is one less function application than the number of elements in the produced vector.
--
-- \( \underbrace{x, f (x), f (f (x)), \ldots}_{\max(0,n)\rm{~elements}} \)
--
-- ===__Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.iterateN 0 undefined undefined :: V.Vector String
-- []
-- >>> V.iterateN 4 (\x -> x <> x) "Hi"
-- ["Hi","HiHi","HiHiHiHi","HiHiHiHiHiHiHiHi"]
--
-- @since 0.7.1
iterateN :: Int -> (a -> a) -> a -> Vector a
{-# INLINE iterateN #-}
iterateN = G.iterateN

-- Unfolding
-- ---------

-- | /O(n)/ Construct a vector by repeatedly applying the generator function
-- to a seed. The generator function yields 'Just' the next element and the
-- new seed or 'Nothing' if there are no more elements.
--
-- > unfoldr (\n -> if n == 0 then Nothing else Just (n,n-1)) 10
-- >  = <10,9,8,7,6,5,4,3,2,1>
unfoldr :: (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldr #-}
unfoldr = G.unfoldr

-- | /O(n)/ Construct a vector with at most @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields 'Just' the
-- next element and the new seed or 'Nothing' if there are no more elements.
--
-- > unfoldrN 3 (\n -> Just (n,n-1)) 10 = <10,9,8>
unfoldrN :: Int -> (b -> Maybe (a, b)) -> b -> Vector a
{-# INLINE unfoldrN #-}
unfoldrN = G.unfoldrN

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly applying
-- the generator function to a seed. The generator function yields the
-- next element and the new seed.
--
-- > unfoldrExactN 3 (\n -> (n,n-1)) 10 = <10,9,8>
--
-- @since 0.12.2.0
unfoldrExactN  :: Int -> (b -> (a, b)) -> b -> Vector a
{-# INLINE unfoldrExactN #-}
unfoldrExactN = G.unfoldrExactN

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
unfoldrM :: (Monad m) => (b -> m (Maybe (a, b))) -> b -> m (Vector a)
{-# INLINE unfoldrM #-}
unfoldrM = G.unfoldrM

-- | /O(n)/ Construct a vector by repeatedly applying the monadic
-- generator function to a seed. The generator function yields 'Just'
-- the next element and the new seed or 'Nothing' if there are no more
-- elements.
unfoldrNM :: (Monad m) => Int -> (b -> m (Maybe (a, b))) -> b -> m (Vector a)
{-# INLINE unfoldrNM #-}
unfoldrNM = G.unfoldrNM

-- | /O(n)/ Construct a vector with exactly @n@ elements by repeatedly
-- applying the monadic generator function to a seed. The generator
-- function yields the next element and the new seed.
--
-- @since 0.12.2.0
unfoldrExactNM :: (Monad m) => Int -> (b -> m (a, b)) -> b -> m (Vector a)
{-# INLINE unfoldrExactNM #-}
unfoldrExactNM = G.unfoldrExactNM

-- | /O(n)/ Construct a vector with @n@ elements by repeatedly applying the
-- generator function to the already constructed part of the vector.
--
-- > constructN 3 f = let a = f <> ; b = f <a> ; c = f <a,b> in <a,b,c>
constructN :: Int -> (Vector a -> a) -> Vector a
{-# INLINE constructN #-}
constructN = G.constructN

-- | /O(n)/ Construct a vector with @n@ elements from right to left by
-- repeatedly applying the generator function to the already constructed part
-- of the vector.
--
-- > constructrN 3 f = let a = f <> ; b = f<a> ; c = f <b,a> in <c,b,a>
constructrN :: Int -> (Vector a -> a) -> Vector a
{-# INLINE constructrN #-}
constructrN = G.constructrN

-- Enumeration
-- -----------

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+1@
-- etc. This operation is usually more efficient than 'enumFromTo'.
--
-- > enumFromN 5 3 = <5,6,7>
enumFromN :: Num a => a -> Int -> Vector a
{-# INLINE enumFromN #-}
enumFromN = G.enumFromN

-- | /O(n)/ Yield a vector of the given length, containing the values @x@, @x+y@,
-- @x+y+y@ etc. This operations is usually more efficient than 'enumFromThenTo'.
--
-- > enumFromStepN 1 2 5 = <1,3,5,7,9>
enumFromStepN :: Num a => a -> a -> Int -> Vector a
{-# INLINE enumFromStepN #-}
enumFromStepN = G.enumFromStepN

-- | /O(n)/ Enumerate values from @x@ to @y@.
--
-- /WARNING:/ This operation can be very inefficient. If possible, use
-- 'enumFromN' instead.
enumFromTo :: Enum a => a -> a -> Vector a
{-# INLINE enumFromTo #-}
enumFromTo = G.enumFromTo

-- | /O(n)/ Enumerate values from @x@ to @y@ with a specific step @z@.
--
-- /WARNING:/ This operation can be very inefficient. If possible, use
-- 'enumFromStepN' instead.
enumFromThenTo :: Enum a => a -> a -> a -> Vector a
{-# INLINE enumFromThenTo #-}
enumFromThenTo = G.enumFromThenTo

-- Concatenation
-- -------------

-- | /O(n)/ Prepend an element.
cons :: a -> Vector a -> Vector a
{-# INLINE cons #-}
cons = G.cons

-- | /O(n)/ Append an element.
snoc :: Vector a -> a -> Vector a
{-# INLINE snoc #-}
snoc = G.snoc

infixr 5 ++
-- | /O(m+n)/ Concatenate two vectors.
(++) :: Vector a -> Vector a -> Vector a
{-# INLINE (++) #-}
(++) = (G.++)

-- | /O(n)/ Concatenate all vectors in the list.
concat :: [Vector a] -> Vector a
{-# INLINE concat #-}
concat = G.concat

-- Monadic initialisation
-- ----------------------

-- | /O(n)/ Execute the monadic action the given number of times and store the
-- results in a vector.
replicateM :: Monad m => Int -> m a -> m (Vector a)
{-# INLINE replicateM #-}
replicateM = G.replicateM

-- | /O(n)/ Construct a vector of the given length by applying the monadic
-- action to each index.
generateM :: Monad m => Int -> (Int -> m a) -> m (Vector a)
{-# INLINE generateM #-}
generateM = G.generateM

-- | /O(n)/ Apply the monadic function \(\max(n - 1, 0)\) times to an initial value, producing a vector
-- of length \(\max(n, 0)\). The 0th element will contain the initial value, which is why there
-- is one less function application than the number of elements in the produced vector.
--
-- For a non-monadic version, see `iterateN`.
--
-- @since 0.12.0.0
iterateNM :: Monad m => Int -> (a -> m a) -> a -> m (Vector a)
{-# INLINE iterateNM #-}
iterateNM = G.iterateNM

-- | Execute the monadic action and freeze the resulting vector.
--
-- @
-- create (do { v \<- new 2; write v 0 \'a\'; write v 1 \'b\'; return v }) = \<'a','b'\>
-- @
create :: (forall s. ST s (MVector s a)) -> Vector a
{-# INLINE create #-}
-- NOTE: eta-expanded due to http://hackage.haskell.org/trac/ghc/ticket/4120
create p = G.create p

-- | Execute the monadic action and freeze the resulting vectors.
createT :: Traversable.Traversable f => (forall s. ST s (f (MVector s a))) -> f (Vector a)
{-# INLINE createT #-}
createT p = G.createT p



-- Restricting memory usage
-- ------------------------

-- | /O(n)/ Yield the argument, but force it not to retain any extra memory,
-- by copying it.
--
-- This is especially useful when dealing with slices. For example:
--
-- > force (slice 0 2 <huge vector>)
--
-- Here, the slice retains a reference to the huge vector. Forcing it creates
-- a copy of just the elements that belong to the slice and allows the huge
-- vector to be garbage collected.
force :: Vector a -> Vector a
{-# INLINE force #-}
force = G.force

-- Bulk updates
-- ------------

-- | /O(m+n)/ For each pair @(i,a)@ from the list of index/value pairs,
-- replace the vector element at position @i@ by @a@.
--
-- > <5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
--
(//) :: Vector a   -- ^ initial vector (of length @m@)
                -> [(Int, a)] -- ^ list of index/value pairs (of length @n@)
                -> Vector a
{-# INLINE (//) #-}
(//) = (G.//)

-- | /O(m+n)/ For each pair @(i,a)@ from the vector of index/value pairs,
-- replace the vector element at position @i@ by @a@.
--
-- > update <5,9,2,7> <(2,1),(0,3),(2,8)> = <3,9,8,7>
--
update :: Vector a        -- ^ initial vector (of length @m@)
       -> Vector (Int, a) -- ^ vector of index/value pairs (of length @n@)
       -> Vector a
{-# INLINE update #-}
update = G.update

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @a@ from the value vector, replace the element of the
-- initial vector at position @i@ by @a@.
--
-- > update_ <5,9,2,7>  <2,0,2> <1,3,8> = <3,9,8,7>
--
-- The function 'update' provides the same functionality and is usually more
-- convenient.
--
-- @
-- update_ xs is ys = 'update' xs ('zip' is ys)
-- @
update_ :: Vector a   -- ^ initial vector (of length @m@)
        -> Vector Int -- ^ index vector (of length @n1@)
        -> Vector a   -- ^ value vector (of length @n2@)
        -> Vector a
{-# INLINE update_ #-}
update_ = G.update_

-- | Same as ('//'), but without bounds checking.
unsafeUpd :: Vector a -> [(Int, a)] -> Vector a
{-# INLINE unsafeUpd #-}
unsafeUpd = G.unsafeUpd

-- | Same as 'update', but without bounds checking.
unsafeUpdate :: Vector a -> Vector (Int, a) -> Vector a
{-# INLINE unsafeUpdate #-}
unsafeUpdate = G.unsafeUpdate

-- | Same as 'update_', but without bounds checking.
unsafeUpdate_ :: Vector a -> Vector Int -> Vector a -> Vector a
{-# INLINE unsafeUpdate_ #-}
unsafeUpdate_ = G.unsafeUpdate_

-- Accumulations
-- -------------

-- | /O(m+n)/ For each pair @(i,b)@ from the list, replace the vector element
-- @a@ at position @i@ by @f a b@.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.accum (+) (V.fromList [1000,2000,3000]) [(2,4),(1,6),(0,3),(1,10)]
-- [1003,2016,3004]
accum :: (a -> b -> a) -- ^ accumulating function @f@
      -> Vector a      -- ^ initial vector (of length @m@)
      -> [(Int,b)]     -- ^ list of index/value pairs (of length @n@)
      -> Vector a
{-# INLINE accum #-}
accum = G.accum

-- | /O(m+n)/ For each pair @(i,b)@ from the vector of pairs, replace the vector
-- element @a@ at position @i@ by @f a b@.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.accumulate (+) (V.fromList [1000,2000,3000]) (V.fromList [(2,4),(1,6),(0,3),(1,10)])
-- [1003,2016,3004]
accumulate :: (a -> b -> a)  -- ^ accumulating function @f@
            -> Vector a       -- ^ initial vector (of length @m@)
            -> Vector (Int,b) -- ^ vector of index/value pairs (of length @n@)
            -> Vector a
{-# INLINE accumulate #-}
accumulate = G.accumulate

-- | /O(m+min(n1,n2))/ For each index @i@ from the index vector and the
-- corresponding value @b@ from the value vector,
-- replace the element of the initial vector at
-- position @i@ by @f a b@.
--
-- > accumulate_ (+) <5,9,2> <2,1,0,1> <4,6,3,7> = <5+3, 9+6+7, 2+4>
--
-- The function 'accumulate' provides the same functionality and is usually more
-- convenient.
--
-- @
-- accumulate_ f as is bs = 'accumulate' f as ('zip' is bs)
-- @
accumulate_ :: (a -> b -> a) -- ^ accumulating function @f@
            -> Vector a      -- ^ initial vector (of length @m@)
            -> Vector Int    -- ^ index vector (of length @n1@)
            -> Vector b      -- ^ value vector (of length @n2@)
            -> Vector a
{-# INLINE accumulate_ #-}
accumulate_ = G.accumulate_

-- | Same as 'accum', but without bounds checking.
unsafeAccum :: (a -> b -> a) -> Vector a -> [(Int,b)] -> Vector a
{-# INLINE unsafeAccum #-}
unsafeAccum = G.unsafeAccum

-- | Same as 'accumulate', but without bounds checking.
unsafeAccumulate :: (a -> b -> a) -> Vector a -> Vector (Int,b) -> Vector a
{-# INLINE unsafeAccumulate #-}
unsafeAccumulate = G.unsafeAccumulate

-- | Same as 'accumulate_', but without bounds checking.
unsafeAccumulate_
  :: (a -> b -> a) -> Vector a -> Vector Int -> Vector b -> Vector a
{-# INLINE unsafeAccumulate_ #-}
unsafeAccumulate_ = G.unsafeAccumulate_

-- Permutations
-- ------------

-- | /O(n)/ Reverse a vector.
reverse :: Vector a -> Vector a
{-# INLINE reverse #-}
reverse = G.reverse

-- | /O(n)/ Yield the vector obtained by replacing each element @i@ of the
-- index vector by @xs'!'i@. This is equivalent to @'map' (xs'!') is@, but is
-- often much more efficient.
--
-- > backpermute <a,b,c,d> <0,3,2,3,1,0> = <a,d,c,d,b,a>
backpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE backpermute #-}
backpermute = G.backpermute

-- | Same as 'backpermute', but without bounds checking.
unsafeBackpermute :: Vector a -> Vector Int -> Vector a
{-# INLINE unsafeBackpermute #-}
unsafeBackpermute = G.unsafeBackpermute

-- Safe destructive updates
-- ------------------------

-- | Apply a destructive operation to a vector. The operation may be
-- performed in place if it is safe to do so and will modify a copy of the
-- vector otherwise (see 'Data.Vector.Generic.New.New' for details).
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> import qualified Data.Vector.Mutable as MV
-- >>> V.modify (\v -> MV.write v 0 'x') $ V.replicate 4 'a'
-- "xaaa"
modify :: (forall s. MVector s a -> ST s ()) -> Vector a -> Vector a
{-# INLINE modify #-}
modify p = G.modify p

-- Indexing
-- --------

-- | /O(n)/ Pair each element in a vector with its index.
indexed :: Vector a -> Vector (Int,a)
{-# INLINE indexed #-}
indexed = G.indexed

-- Mapping
-- -------

-- | /O(n)/ Map a function over a vector.
map :: (a -> b) -> Vector a -> Vector b
{-# INLINE map #-}
map = G.map

-- | /O(n)/ Apply a function to every element of a vector and its index.
imap :: (Int -> a -> b) -> Vector a -> Vector b
{-# INLINE imap #-}
imap = G.imap

-- | Map a function over a vector and concatenate the results.
concatMap :: (a -> Vector b) -> Vector a -> Vector b
{-# INLINE concatMap #-}
concatMap = G.concatMap

-- Monadic mapping
-- ---------------

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results.
mapM :: Monad m => (a -> m b) -> Vector a -> m (Vector b)
{-# INLINE mapM #-}
mapM = G.mapM

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, yielding a vector of results.
imapM :: Monad m => (Int -> a -> m b) -> Vector a -> m (Vector b)
{-# INLINE imapM #-}
imapM = G.imapM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results.
mapM_ :: Monad m => (a -> m b) -> Vector a -> m ()
{-# INLINE mapM_ #-}
mapM_ = G.mapM_

-- | /O(n)/ Apply the monadic action to every element of a vector and its
-- index, ignoring the results.
imapM_ :: Monad m => (Int -> a -> m b) -> Vector a -> m ()
{-# INLINE imapM_ #-}
imapM_ = G.imapM_

-- | /O(n)/ Apply the monadic action to all elements of the vector, yielding a
-- vector of results. Equivalent to @flip 'mapM'@.
forM :: Monad m => Vector a -> (a -> m b) -> m (Vector b)
{-# INLINE forM #-}
forM = G.forM

-- | /O(n)/ Apply the monadic action to all elements of a vector and ignore the
-- results. Equivalent to @flip 'mapM_'@.
forM_ :: Monad m => Vector a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = G.forM_

-- | /O(n)/ Apply the monadic action to all elements of the vector and their indices, yielding a
-- vector of results. Equivalent to @'flip' 'imapM'@.
--
-- @since 0.12.2.0
iforM :: Monad m => Vector a -> (Int -> a -> m b) -> m (Vector b)
{-# INLINE iforM #-}
iforM = G.iforM

-- | /O(n)/ Apply the monadic action to all elements of the vector and their indices
-- and ignore the results. Equivalent to @'flip' 'imapM_'@.
--
-- @since 0.12.2.0
iforM_ :: Monad m => Vector a -> (Int -> a -> m b) -> m ()
{-# INLINE iforM_ #-}
iforM_ = G.iforM_

-- Zipping
-- -------

-- | /O(min(m,n))/ Zip two vectors with the given function.
zipWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE zipWith #-}
zipWith = G.zipWith

-- | Zip three vectors with the given function.
zipWith3 :: (a -> b -> c -> d) -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE zipWith3 #-}
zipWith3 = G.zipWith3

zipWith4 :: (a -> b -> c -> d -> e)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE zipWith4 #-}
zipWith4 = G.zipWith4

zipWith5 :: (a -> b -> c -> d -> e -> f)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f
{-# INLINE zipWith5 #-}
zipWith5 = G.zipWith5

zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
         -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
         -> Vector f -> Vector g
{-# INLINE zipWith6 #-}
zipWith6 = G.zipWith6

-- | /O(min(m,n))/ Zip two vectors with a function that also takes the
-- elements' indices.
izipWith :: (Int -> a -> b -> c) -> Vector a -> Vector b -> Vector c
{-# INLINE izipWith #-}
izipWith = G.izipWith

-- | Zip three vectors and their indices with the given function.
izipWith3 :: (Int -> a -> b -> c -> d)
          -> Vector a -> Vector b -> Vector c -> Vector d
{-# INLINE izipWith3 #-}
izipWith3 = G.izipWith3

izipWith4 :: (Int -> a -> b -> c -> d -> e)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
{-# INLINE izipWith4 #-}
izipWith4 = G.izipWith4

izipWith5 :: (Int -> a -> b -> c -> d -> e -> f)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f
{-# INLINE izipWith5 #-}
izipWith5 = G.izipWith5

izipWith6 :: (Int -> a -> b -> c -> d -> e -> f -> g)
          -> Vector a -> Vector b -> Vector c -> Vector d -> Vector e
          -> Vector f -> Vector g
{-# INLINE izipWith6 #-}
izipWith6 = G.izipWith6

-- | /O(min(m,n))/ Zip two vectors.
zip :: Vector a -> Vector b -> Vector (a, b)
{-# INLINE zip #-}
zip = G.zip

-- | Zip together three vectors into a vector of triples.
zip3 :: Vector a -> Vector b -> Vector c -> Vector (a, b, c)
{-# INLINE zip3 #-}
zip3 = G.zip3

zip4 :: Vector a -> Vector b -> Vector c -> Vector d
     -> Vector (a, b, c, d)
{-# INLINE zip4 #-}
zip4 = G.zip4

zip5 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e
     -> Vector (a, b, c, d, e)
{-# INLINE zip5 #-}
zip5 = G.zip5

zip6 :: Vector a -> Vector b -> Vector c -> Vector d -> Vector e -> Vector f
     -> Vector (a, b, c, d, e, f)
{-# INLINE zip6 #-}
zip6 = G.zip6

-- Unzipping
-- ---------

-- | /O(min(m,n))/ Unzip a vector of pairs.
unzip :: Vector (a, b) -> (Vector a, Vector b)
{-# INLINE unzip #-}
unzip = G.unzip

unzip3 :: Vector (a, b, c) -> (Vector a, Vector b, Vector c)
{-# INLINE unzip3 #-}
unzip3 = G.unzip3

unzip4 :: Vector (a, b, c, d) -> (Vector a, Vector b, Vector c, Vector d)
{-# INLINE unzip4 #-}
unzip4 = G.unzip4

unzip5 :: Vector (a, b, c, d, e)
       -> (Vector a, Vector b, Vector c, Vector d, Vector e)
{-# INLINE unzip5 #-}
unzip5 = G.unzip5

unzip6 :: Vector (a, b, c, d, e, f)
       -> (Vector a, Vector b, Vector c, Vector d, Vector e, Vector f)
{-# INLINE unzip6 #-}
unzip6 = G.unzip6

-- Monadic zipping
-- ---------------

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and yield a
-- vector of results.
zipWithM :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
{-# INLINE zipWithM #-}
zipWithM = G.zipWithM

-- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- the element index and yield a vector of results.
izipWithM :: Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m (Vector c)
{-# INLINE izipWithM #-}
izipWithM = G.izipWithM

-- | /O(min(m,n))/ Zip the two vectors with the monadic action and ignore the
-- results.
zipWithM_ :: Monad m => (a -> b -> m c) -> Vector a -> Vector b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ = G.zipWithM_

-- | /O(min(m,n))/ Zip the two vectors with a monadic action that also takes
-- the element index and ignore the results.
izipWithM_ :: Monad m => (Int -> a -> b -> m c) -> Vector a -> Vector b -> m ()
{-# INLINE izipWithM_ #-}
izipWithM_ = G.izipWithM_

-- Filtering
-- ---------

-- | /O(n)/ Drop all elements that do not satisfy the predicate.
filter :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE filter #-}
filter = G.filter

-- | /O(n)/ Drop all elements that do not satisfy the predicate which is applied to
-- the values and their indices.
ifilter :: (Int -> a -> Bool) -> Vector a -> Vector a
{-# INLINE ifilter #-}
ifilter = G.ifilter

-- | /O(n)/ Drop repeated adjacent elements. The first element in each group is returned.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.uniq $ V.fromList [1,3,3,200,3]
-- [1,3,200,3]
-- >>> import Data.Semigroup
-- >>> V.uniq $ V.fromList [ Arg 1 'a', Arg 1 'b', Arg 1 'c']
-- [Arg 1 'a']
uniq :: (Eq a) => Vector a -> Vector a
{-# INLINE uniq #-}
uniq = G.uniq

-- | /O(n)/ Map the values and collect the 'Just' results.
mapMaybe :: (a -> Maybe b) -> Vector a -> Vector b
{-# INLINE mapMaybe #-}
mapMaybe = G.mapMaybe

-- | /O(n)/ Map the indices/values and collect the 'Just' results.
imapMaybe :: (Int -> a -> Maybe b) -> Vector a -> Vector b
{-# INLINE imapMaybe #-}
imapMaybe = G.imapMaybe

-- | /O(n)/ Return a Vector of all the 'Just' values.
--
-- @since 0.12.2.0
catMaybes :: Vector (Maybe a) -> Vector a
{-# INLINE catMaybes #-}
catMaybes = mapMaybe id

-- | /O(n)/ Drop all elements that do not satisfy the monadic predicate.
filterM :: Monad m => (a -> m Bool) -> Vector a -> m (Vector a)
{-# INLINE filterM #-}
filterM = G.filterM

-- | /O(n)/ Apply the monadic function to each element of the vector and
-- discard elements returning 'Nothing'.
--
-- @since 0.12.2.0
mapMaybeM :: Monad m => (a -> m (Maybe b)) -> Vector a -> m (Vector b)
{-# INLINE mapMaybeM #-}
mapMaybeM = G.mapMaybeM

-- | /O(n)/ Apply the monadic function to each element of the vector and its index.
-- Discard elements returning 'Nothing'.
--
-- @since 0.12.2.0
imapMaybeM :: Monad m => (Int -> a -> m (Maybe b)) -> Vector a -> m (Vector b)
{-# INLINE imapMaybeM #-}
imapMaybeM = G.imapMaybeM

-- | /O(n)/ Yield the longest prefix of elements satisfying the predicate.
-- The current implementation is not copy-free, unless the result vector is
-- fused away.
takeWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE takeWhile #-}
takeWhile = G.takeWhile

-- | /O(n)/ Drop the longest prefix of elements that satisfy the predicate
-- without copying.
dropWhile :: (a -> Bool) -> Vector a -> Vector a
{-# INLINE dropWhile #-}
dropWhile = G.dropWhile

-- Parititioning
-- -------------

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't. The
-- relative order of the elements is preserved at the cost of a sometimes
-- reduced performance compared to 'unstablePartition'.
partition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE partition #-}
partition = G.partition

-- | /O(n)/ Split the vector into two parts, the first one containing the
-- @`Left`@ elements and the second containing the @`Right`@ elements.
-- The relative order of the elements is preserved.
--
-- @since 0.12.1.0
partitionWith :: (a -> Either b c) -> Vector a -> (Vector b, Vector c)
{-# INLINE partitionWith #-}
partitionWith = G.partitionWith

-- | /O(n)/ Split the vector in two parts, the first one containing those
-- elements that satisfy the predicate and the second one those that don't.
-- The order of the elements is not preserved, but the operation is often
-- faster than 'partition'.
unstablePartition :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE unstablePartition #-}
unstablePartition = G.unstablePartition

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
--
-- Does not fuse.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.span (<4) $ V.generate 10 id
-- ([0,1,2,3],[4,5,6,7,8,9])
span :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE span #-}
span = G.span

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- Does not fuse.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.break (>4) $ V.generate 10 id
-- ([0,1,2,3,4],[5,6,7,8,9])
break :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE break #-}
break = G.break

-- | /O(n)/ Split the vector into the longest prefix of elements that satisfy
-- the predicate and the rest without copying.
--
-- Does not fuse.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.spanR (>4) $ V.generate 10 id
-- ([5,6,7,8,9],[0,1,2,3,4])
spanR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE spanR #-}
spanR = G.spanR

-- | /O(n)/ Split the vector into the longest prefix of elements that do not
-- satisfy the predicate and the rest without copying.
--
-- Does not fuse.
--
-- @since NEXT_VERSION
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.breakR (<5) $ V.generate 10 id
-- ([5,6,7,8,9],[0,1,2,3,4])
breakR :: (a -> Bool) -> Vector a -> (Vector a, Vector a)
{-# INLINE breakR #-}
breakR = G.breakR

-- | /O(n)/ Split a vector into a list of slices, using a predicate function.
--
-- The concatenation of this list of slices is equal to the argument vector,
-- and each slice contains only equal elements, as determined by the equality
-- predicate function.
--
-- Does not fuse.
--
-- >>> import qualified Data.Vector as V
-- >>> import           Data.Char (isUpper)
-- >>> V.groupBy (\a b -> isUpper a == isUpper b) (V.fromList "Mississippi River")
-- ["M","ississippi ","R","iver"]
--
-- See also 'Data.List.groupBy', 'group'.
--
-- @since 0.13.0.1
groupBy :: (a -> a -> Bool) -> Vector a -> [Vector a]
{-# INLINE groupBy #-}
groupBy = G.groupBy

-- | /O(n)/ Split a vector into a list of slices of the input vector.
--
-- The concatenation of this list of slices is equal to the argument vector,
-- and each slice contains only equal elements.
--
-- Does not fuse.
--
-- This is the equivalent of 'groupBy (==)'.
--
-- >>> import qualified Data.Vector as V
-- >>> V.group (V.fromList "Mississippi")
-- ["M","i","ss","i","ss","i","pp","i"]
--
-- See also 'Data.List.group'.
--
-- @since 0.13.0.1
group :: Eq a => Vector a -> [Vector a]
{-# INLINE group #-}
group = G.groupBy (==)

-- Searching
-- ---------

infix 4 `elem`
-- | /O(n)/ Check if the vector contains an element.
elem :: Eq a => a -> Vector a -> Bool
{-# INLINE elem #-}
elem = G.elem

infix 4 `notElem`
-- | /O(n)/ Check if the vector does not contain an element (inverse of 'elem').
notElem :: Eq a => a -> Vector a -> Bool
{-# INLINE notElem #-}
notElem = G.notElem

-- | /O(n)/ Yield 'Just' the first element matching the predicate or 'Nothing'
-- if no such element exists.
find :: (a -> Bool) -> Vector a -> Maybe a
{-# INLINE find #-}
find = G.find

-- | /O(n)/ Yield 'Just' the index of the first element matching the predicate
-- or 'Nothing' if no such element exists.
findIndex :: (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndex #-}
findIndex = G.findIndex

-- | /O(n)/ Yield 'Just' the index of the /last/ element matching the predicate
-- or 'Nothing' if no such element exists.
--
-- Does not fuse.
findIndexR :: (a -> Bool) -> Vector a -> Maybe Int
{-# INLINE findIndexR #-}
findIndexR = G.findIndexR

-- | /O(n)/ Yield the indices of elements satisfying the predicate in ascending
-- order.
findIndices :: (a -> Bool) -> Vector a -> Vector Int
{-# INLINE findIndices #-}
findIndices = G.findIndices

-- | /O(n)/ Yield 'Just' the index of the first occurrence of the given element or
-- 'Nothing' if the vector does not contain the element. This is a specialised
-- version of 'findIndex'.
elemIndex :: Eq a => a -> Vector a -> Maybe Int
{-# INLINE elemIndex #-}
elemIndex = G.elemIndex

-- | /O(n)/ Yield the indices of all occurrences of the given element in
-- ascending order. This is a specialised version of 'findIndices'.
elemIndices :: Eq a => a -> Vector a -> Vector Int
{-# INLINE elemIndices #-}
elemIndices = G.elemIndices

-- Folding
-- -------

-- | /O(n)/ Left fold.
foldl :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl #-}
foldl = G.foldl

-- | /O(n)/ Left fold on non-empty vectors.
foldl1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1 #-}
foldl1 = G.foldl1

-- | /O(n)/ Left fold with strict accumulator.
foldl' :: (a -> b -> a) -> a -> Vector b -> a
{-# INLINE foldl' #-}
foldl' = G.foldl'

-- | /O(n)/ Left fold on non-empty vectors with strict accumulator.
foldl1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldl1' #-}
foldl1' = G.foldl1'

-- | /O(n)/ Right fold.
foldr :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr #-}
foldr = G.foldr

-- | /O(n)/ Right fold on non-empty vectors.
foldr1 :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1 #-}
foldr1 = G.foldr1

-- | /O(n)/ Right fold with a strict accumulator.
foldr' :: (a -> b -> b) -> b -> Vector a -> b
{-# INLINE foldr' #-}
foldr' = G.foldr'

-- | /O(n)/ Right fold on non-empty vectors with strict accumulator.
foldr1' :: (a -> a -> a) -> Vector a -> a
{-# INLINE foldr1' #-}
foldr1' = G.foldr1'

-- | /O(n)/ Left fold using a function applied to each element and its index.
ifoldl :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl #-}
ifoldl = G.ifoldl

-- | /O(n)/ Left fold with strict accumulator using a function applied to each element
-- and its index.
ifoldl' :: (a -> Int -> b -> a) -> a -> Vector b -> a
{-# INLINE ifoldl' #-}
ifoldl' = G.ifoldl'

-- | /O(n)/ Right fold using a function applied to each element and its index.
ifoldr :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr #-}
ifoldr = G.ifoldr

-- | /O(n)/ Right fold with strict accumulator using a function applied to each
-- element and its index.
ifoldr' :: (Int -> a -> b -> b) -> b -> Vector a -> b
{-# INLINE ifoldr' #-}
ifoldr' = G.ifoldr'

-- | /O(n)/ Map each element of the structure to a monoid and combine
-- the results. It uses the same implementation as the corresponding method
-- of the 'Foldable' type class. Note that it's implemented in terms of 'foldr'
-- and won't fuse with functions that traverse the vector from left to
-- right ('map', 'generate', etc.).
--
-- @since 0.12.2.0
foldMap :: (Monoid m) => (a -> m) -> Vector a -> m
{-# INLINE foldMap #-}
foldMap = G.foldMap

-- | /O(n)/ Like 'foldMap', but strict in the accumulator. It uses the same
-- implementation as the corresponding method of the 'Foldable' type class.
-- Note that it's implemented in terms of 'foldl'', so it fuses in most
-- contexts.
--
-- @since 0.12.2.0
foldMap' :: (Monoid m) => (a -> m) -> Vector a -> m
{-# INLINE foldMap' #-}
foldMap' = G.foldMap'


-- Specialised folds
-- -----------------

-- | /O(n)/ Check if all elements satisfy the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.all even $ V.fromList [2, 4, 12]
-- True
-- >>> V.all even $ V.fromList [2, 4, 13]
-- False
-- >>> V.all even (V.empty :: V.Vector Int)
-- True
all :: (a -> Bool) -> Vector a -> Bool
{-# INLINE all #-}
all = G.all

-- | /O(n)/ Check if any element satisfies the predicate.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.any even $ V.fromList [1, 3, 7]
-- False
-- >>> V.any even $ V.fromList [3, 2, 13]
-- True
-- >>> V.any even (V.empty :: V.Vector Int)
-- False
any :: (a -> Bool) -> Vector a -> Bool
{-# INLINE any #-}
any = G.any

-- | /O(n)/ Check if all elements are 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.and $ V.fromList [True, False]
-- False
-- >>> V.and V.empty
-- True
and :: Vector Bool -> Bool
{-# INLINE and #-}
and = G.and

-- | /O(n)/ Check if any element is 'True'.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.or $ V.fromList [True, False]
-- True
-- >>> V.or V.empty
-- False
or :: Vector Bool -> Bool
{-# INLINE or #-}
or = G.or

-- | /O(n)/ Compute the sum of the elements.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.sum $ V.fromList [300,20,1]
-- 321
-- >>> V.sum (V.empty :: V.Vector Int)
-- 0
sum :: Num a => Vector a -> a
{-# INLINE sum #-}
sum = G.sum

-- | /O(n)/ Compute the product of the elements.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.product $ V.fromList [1,2,3,4]
-- 24
-- >>> V.product (V.empty :: V.Vector Int)
-- 1
product :: Num a => Vector a -> a
{-# INLINE product #-}
product = G.product

-- | /O(n)/ Yield the maximum element of the vector. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.maximum $ V.fromList [2, 1]
-- 2
-- >>> import Data.Semigroup
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 2 'b']
-- Arg 2 'b'
-- >>> V.maximum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
maximum :: Ord a => Vector a -> a
{-# INLINE maximum #-}
maximum = G.maximum

-- | /O(n)/ Yield the maximum element of the vector according to the
-- given comparison function. The vector may not be empty. In case of
-- a tie, the first occurrence wins. This behavior is different from
-- 'Data.List.maximumBy' which returns the last tie.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.maximumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
maximumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE maximumBy #-}
maximumBy = G.maximumBy

-- | /O(n)/ Yield the maximum element of the vector by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.maximumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (2,'a')
-- >>> V.maximumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
--
-- @since 0.13.0.0
maximumOn :: Ord b => (a -> b) -> Vector a -> a
{-# INLINE maximumOn #-}
maximumOn = G.maximumOn

-- | /O(n)/ Yield the minimum element of the vector. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.minimum $ V.fromList [2, 1]
-- 1
-- >>> import Data.Semigroup
-- >>> V.minimum $ V.fromList [Arg 2 'a', Arg 1 'b']
-- Arg 1 'b'
-- >>> V.minimum $ V.fromList [Arg 1 'a', Arg 1 'b']
-- Arg 1 'a'
minimum :: Ord a => Vector a -> a
{-# INLINE minimum #-}
minimum = G.minimum

-- | /O(n)/ Yield the minimum element of the vector according to the
-- given comparison function. The vector may not be empty. In case of
-- a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.minimumBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
minimumBy :: (a -> a -> Ordering) -> Vector a -> a
{-# INLINE minimumBy #-}
minimumBy = G.minimumBy

-- | /O(n)/ Yield the minimum element of the vector by comparing the results
-- of a key function on each element. In case of a tie, the first occurrence
-- wins. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.minimumOn fst $ V.fromList [(2,'a'), (1,'b')]
-- (1,'b')
-- >>> V.minimumOn fst $ V.fromList [(1,'a'), (1,'b')]
-- (1,'a')
--
-- @since 0.13.0.0
minimumOn :: Ord b => (a -> b) -> Vector a -> a
{-# INLINE minimumOn #-}
minimumOn = G.minimumOn

-- | /O(n)/ Yield the index of the maximum element of the vector. The vector
-- may not be empty.
maxIndex :: Ord a => Vector a -> Int
{-# INLINE maxIndex #-}
maxIndex = G.maxIndex

-- | /O(n)/ Yield the index of the maximum element of the vector
-- according to the given comparison function. The vector may not be
-- empty. In case of a tie, the first occurrence wins.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.maxIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- 0
-- >>> V.maxIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- 0
maxIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE maxIndexBy #-}
maxIndexBy = G.maxIndexBy

-- | /O(n)/ Yield the index of the minimum element of the vector. The vector
-- may not be empty.
minIndex :: Ord a => Vector a -> Int
{-# INLINE minIndex #-}
minIndex = G.minIndex

-- | /O(n)/ Yield the index of the minimum element of the vector according to
-- the given comparison function. The vector may not be empty.
--
-- ==== __Examples__
--
-- >>> import Data.Ord
-- >>> import qualified Data.Vector as V
-- >>> V.minIndexBy (comparing fst) $ V.fromList [(2,'a'), (1,'b')]
-- 1
-- >>> V.minIndexBy (comparing fst) $ V.fromList [(1,'a'), (1,'b')]
-- 0
minIndexBy :: (a -> a -> Ordering) -> Vector a -> Int
{-# INLINE minIndexBy #-}
minIndexBy = G.minIndexBy

-- Monadic folds
-- -------------

-- | /O(n)/ Monadic fold.
foldM :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM #-}
foldM = G.foldM

-- | /O(n)/ Monadic fold using a function applied to each element and its index.
ifoldM :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
{-# INLINE ifoldM #-}
ifoldM = G.ifoldM

-- | /O(n)/ Monadic fold over non-empty vectors.
fold1M :: Monad m => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M #-}
fold1M = G.fold1M

-- | /O(n)/ Monadic fold with strict accumulator.
foldM' :: Monad m => (a -> b -> m a) -> a -> Vector b -> m a
{-# INLINE foldM' #-}
foldM' = G.foldM'

-- | /O(n)/ Monadic fold with strict accumulator using a function applied to each
-- element and its index.
ifoldM' :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m a
{-# INLINE ifoldM' #-}
ifoldM' = G.ifoldM'

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator.
fold1M' :: Monad m => (a -> a -> m a) -> Vector a -> m a
{-# INLINE fold1M' #-}
fold1M' = G.fold1M'

-- | /O(n)/ Monadic fold that discards the result.
foldM_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM_ #-}
foldM_ = G.foldM_

-- | /O(n)/ Monadic fold that discards the result using a function applied to
-- each element and its index.
ifoldM_ :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE ifoldM_ #-}
ifoldM_ = G.ifoldM_

-- | /O(n)/ Monadic fold over non-empty vectors that discards the result.
fold1M_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M_ #-}
fold1M_ = G.fold1M_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result.
foldM'_ :: Monad m => (a -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE foldM'_ #-}
foldM'_ = G.foldM'_

-- | /O(n)/ Monadic fold with strict accumulator that discards the result
-- using a function applied to each element and its index.
ifoldM'_ :: Monad m => (a -> Int -> b -> m a) -> a -> Vector b -> m ()
{-# INLINE ifoldM'_ #-}
ifoldM'_ = G.ifoldM'_

-- | /O(n)/ Monadic fold over non-empty vectors with strict accumulator
-- that discards the result.
fold1M'_ :: Monad m => (a -> a -> m a) -> Vector a -> m ()
{-# INLINE fold1M'_ #-}
fold1M'_ = G.fold1M'_

-- Monadic sequencing
-- ------------------

-- | Evaluate each action and collect the results.
sequence :: Monad m => Vector (m a) -> m (Vector a)
{-# INLINE sequence #-}
sequence = G.sequence

-- | Evaluate each action and discard the results.
sequence_ :: Monad m => Vector (m a) -> m ()
{-# INLINE sequence_ #-}
sequence_ = G.sequence_

-- Scans
-- -----

-- | /O(n)/ Left-to-right prescan.
--
-- @
-- prescanl f z = 'init' . 'scanl' f z
-- @
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.prescanl (+) 0 (V.fromList [1,2,3,4])
-- [0,1,3,6]
prescanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl #-}
prescanl = G.prescanl

-- | /O(n)/ Left-to-right prescan with strict accumulator.
prescanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE prescanl' #-}
prescanl' = G.prescanl'

-- | /O(n)/ Left-to-right postscan.
--
-- @
-- postscanl f z = 'tail' . 'scanl' f z
-- @
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.postscanl (+) 0 (V.fromList [1,2,3,4])
-- [1,3,6,10]
postscanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl #-}
postscanl = G.postscanl

-- | /O(n)/ Left-to-right postscan with strict accumulator.
postscanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE postscanl' #-}
postscanl' = G.postscanl'

-- | /O(n)/ Left-to-right scan.
--
-- > scanl f z <x1,...,xn> = <y1,...,y(n+1)>
-- >   where y1 = z
-- >         yi = f y(i-1) x(i-1)
--
-- ==== __Examples__
--
-- >>> import qualified Data.Vector as V
-- >>> V.scanl (+) 0 (V.fromList [1,2,3,4])
-- [0,1,3,6,10]
scanl :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl #-}
scanl = G.scanl

-- | /O(n)/ Left-to-right scan with strict accumulator.
scanl' :: (a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE scanl' #-}
scanl' = G.scanl'

-- | /O(n)/ Left-to-right scan over a vector with its index.
--
-- @since 0.12.0.0
iscanl :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE iscanl #-}
iscanl = G.iscanl

-- | /O(n)/ Left-to-right scan over a vector (strictly) with its index.
--
-- @since 0.12.0.0
iscanl' :: (Int -> a -> b -> a) -> a -> Vector b -> Vector a
{-# INLINE iscanl' #-}
iscanl' = G.iscanl'

-- | /O(n)/ Initial-value free left-to-right scan over a vector.
--
-- > scanl f <x1,...,xn> = <y1,...,yn>
-- >   where y1 = x1
-- >         yi = f y(i-1) xi
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector as V
-- >>> V.scanl1 min $ V.fromListN 5 [4,2,4,1,3]
-- [4,2,2,1,1]
-- >>> V.scanl1 max $ V.fromListN 5 [1,3,2,5,4]
-- [1,3,3,5,5]
-- >>> V.scanl1 min (V.empty :: V.Vector Int)
-- []
scanl1 :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1 #-}
scanl1 = G.scanl1

-- | /O(n)/ Initial-value free left-to-right scan over a vector with a strict accumulator.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector as V
-- >>> V.scanl1' min $ V.fromListN 5 [4,2,4,1,3]
-- [4,2,2,1,1]
-- >>> V.scanl1' max $ V.fromListN 5 [1,3,2,5,4]
-- [1,3,3,5,5]
-- >>> V.scanl1' min (V.empty :: V.Vector Int)
-- []
scanl1' :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanl1' #-}
scanl1' = G.scanl1'

-- | /O(n)/ Right-to-left prescan.
--
-- @
-- prescanr f z = 'reverse' . 'prescanl' (flip f) z . 'reverse'
-- @
prescanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr #-}
prescanr = G.prescanr

-- | /O(n)/ Right-to-left prescan with strict accumulator.
prescanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE prescanr' #-}
prescanr' = G.prescanr'

-- | /O(n)/ Right-to-left postscan.
postscanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr #-}
postscanr = G.postscanr

-- | /O(n)/ Right-to-left postscan with strict accumulator.
postscanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE postscanr' #-}
postscanr' = G.postscanr'

-- | /O(n)/ Right-to-left scan.
scanr :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr #-}
scanr = G.scanr

-- | /O(n)/ Right-to-left scan with strict accumulator.
scanr' :: (a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE scanr' #-}
scanr' = G.scanr'

-- | /O(n)/ Right-to-left scan over a vector with its index.
--
-- @since 0.12.0.0
iscanr :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE iscanr #-}
iscanr = G.iscanr

-- | /O(n)/ Right-to-left scan over a vector (strictly) with its index.
--
-- @since 0.12.0.0
iscanr' :: (Int -> a -> b -> b) -> b -> Vector a -> Vector b
{-# INLINE iscanr' #-}
iscanr' = G.iscanr'

-- | /O(n)/ Right-to-left, initial-value free scan over a vector.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector as V
-- >>> V.scanr1 min $ V.fromListN 5 [3,1,4,2,4]
-- [1,1,2,2,4]
-- >>> V.scanr1 max $ V.fromListN 5 [4,5,2,3,1]
-- [5,5,3,3,1]
-- >>> V.scanr1 min (V.empty :: V.Vector Int)
-- []
scanr1 :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1 #-}
scanr1 = G.scanr1

-- | /O(n)/ Right-to-left, initial-value free scan over a vector with a strict
-- accumulator.
--
-- Note: Since 0.13, application of this to an empty vector no longer
-- results in an error; instead it produces an empty vector.
--
-- ==== __Examples__
-- >>> import qualified Data.Vector as V
-- >>> V.scanr1' min $ V.fromListN 5 [3,1,4,2,4]
-- [1,1,2,2,4]
-- >>> V.scanr1' max $ V.fromListN 5 [4,5,2,3,1]
-- [5,5,3,3,1]
-- >>> V.scanr1' min (V.empty :: V.Vector Int)
-- []
scanr1' :: (a -> a -> a) -> Vector a -> Vector a
{-# INLINE scanr1' #-}
scanr1' = G.scanr1'

-- Comparisons
-- ------------------------

-- | /O(n)/ Check if two vectors are equal using the supplied equality
-- predicate.
--
-- @since 0.12.2.0
eqBy :: (a -> b -> Bool) -> Vector a -> Vector b -> Bool
{-# INLINE eqBy #-}
eqBy = G.eqBy

-- | /O(n)/ Compare two vectors using the supplied comparison function for
-- vector elements. Comparison works the same as for lists.
--
-- > cmpBy compare == compare
--
-- @since 0.12.2.0
cmpBy :: (a -> b -> Ordering) -> Vector a -> Vector b -> Ordering
cmpBy = G.cmpBy

-- Conversions - Lists
-- ------------------------

-- | /O(n)/ Convert a vector to a list.
toList :: Vector a -> [a]
{-# INLINE toList #-}
toList = G.toList

-- | /O(n)/ Convert a list to a vector. During the operation, the 
-- vector’s capacity will be doubling until the list's contents are 
-- in the vector. Depending on the list’s size, up to half of the vector’s 
-- capacity might be empty. If you’d rather avoid this, you can use 
-- 'fromListN', which will provide the exact space the list requires but will 
-- prevent list fusion, or @'force' . 'fromList'@, which will create the 
-- vector and then copy it without the superfluous space.
--
-- @since 0.3
fromList :: [a] -> Vector a
{-# INLINE fromList #-}
fromList = G.fromList

-- | /O(n)/ Convert the first @n@ elements of a list to a vector. It's
-- expected that the supplied list will be exactly @n@ elements long. As
-- an optimization, this function allocates a buffer for @n@ elements, which
-- could be used for DoS-attacks by exhausting the memory if an attacker controls
-- that parameter.
--
-- @
-- fromListN n xs = 'fromList' ('take' n xs)
-- @
fromListN :: Int -> [a] -> Vector a
{-# INLINE fromListN #-}
fromListN = G.fromListN

-- Conversions - Arrays
-- -----------------------------

-- | /O(1)/ Convert an array to a vector.
--
-- @since 0.12.2.0
fromArray :: Array a -> Vector a
{-# INLINE fromArray #-}
fromArray arr = Vector 0 (sizeofArray arr) arr

-- | /O(n)/ Convert a vector to an array.
--
-- @since 0.12.2.0
toArray :: Vector a -> Array a
{-# INLINE toArray #-}
toArray (Vector offset len arr)
  | offset == 0 && len == sizeofArray arr = arr
  | otherwise = cloneArray arr offset len

-- | /O(1)/ Extract the underlying `Array`, offset where vector starts and the
-- total number of elements in the vector. Below property always holds:
--
-- > let (array, offset, len) = toArraySlice v
-- > v === unsafeFromArraySlice len offset array
--
-- @since 0.13.0.0
toArraySlice :: Vector a -> (Array a, Int, Int)
{-# INLINE toArraySlice #-}
toArraySlice (Vector offset len arr) = (arr, offset, len)


-- | /O(1)/ Convert an array slice to a vector. This function is very unsafe,
-- because constructing an invalid vector can yield almost all other safe
-- functions in this module unsafe. These are equivalent:
--
-- > unsafeFromArraySlice len offset === unsafeTake len . unsafeDrop offset . fromArray
--
-- @since 0.13.0.0
unsafeFromArraySlice ::
     Array a -- ^ Immutable boxed array.
  -> Int -- ^ Offset
  -> Int -- ^ Length
  -> Vector a
{-# INLINE unsafeFromArraySlice #-}
unsafeFromArraySlice arr offset len = Vector offset len arr

-- Conversions - Mutable vectors
-- -----------------------------

-- | /O(1)/ Unsafely convert a mutable vector to an immutable one without
-- copying. The mutable vector may not be used after this operation.
unsafeFreeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = G.unsafeFreeze

-- | /O(n)/ Yield an immutable copy of the mutable vector.
freeze :: PrimMonad m => MVector (PrimState m) a -> m (Vector a)
{-# INLINE freeze #-}
freeze = G.freeze

-- | /O(1)/ Unsafely convert an immutable vector to a mutable one
-- without copying. Note that this is a very dangerous function and
-- generally it's only safe to read from the resulting vector. In this
-- case, the immutable vector could be used safely as well.
--
-- Problems with mutation happen because GHC has a lot of freedom to
-- introduce sharing. As a result mutable vectors produced by
-- @unsafeThaw@ may or may not share the same underlying buffer. For
-- example:
--
-- > foo = do
-- >   let vec = V.generate 10 id
-- >   mvec <- V.unsafeThaw vec
-- >   do_something mvec
--
-- Here GHC could lift @vec@ outside of foo which means that all calls to
-- @do_something@ will use same buffer with possibly disastrous
-- results. Whether such aliasing happens or not depends on the program in
-- question, optimization levels, and GHC flags.
--
-- All in all, attempts to modify a vector produced by @unsafeThaw@ fall out of
-- domain of software engineering and into realm of black magic, dark
-- rituals, and unspeakable horrors. The only advice that could be given
-- is: "Don't attempt to mutate a vector produced by @unsafeThaw@ unless you
-- know how to prevent GHC from aliasing buffers accidentally. We don't."
unsafeThaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw = G.unsafeThaw

-- | /O(n)/ Yield a mutable copy of an immutable vector.
thaw :: PrimMonad m => Vector a -> m (MVector (PrimState m) a)
{-# INLINE thaw #-}
thaw = G.thaw

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length. This is not checked.
unsafeCopy :: PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE unsafeCopy #-}
unsafeCopy = G.unsafeCopy

-- | /O(n)/ Copy an immutable vector into a mutable one. The two vectors must
-- have the same length.
copy :: PrimMonad m => MVector (PrimState m) a -> Vector a -> m ()
{-# INLINE copy #-}
copy = G.copy

-- $setup
-- >>> :set -Wno-type-defaults
-- >>> import Prelude (Char, String, Bool(True, False), min, max, fst, even, undefined, Ord(..))

{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveLift            #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE PatternGuards         #-}
{-# LANGUAGE RoleAnnotations       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeInType            #-}
{-# LANGUAGE UnboxedSums           #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# OPTIONS_GHC -fno-full-laziness -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | = WARNING
--
-- This module is considered __internal__.
--
-- The Package Versioning Policy __does not apply__.
--
-- The contents of this module may change __in any way whatsoever__
-- and __without any warning__ between minor versions of this package.
--
-- Authors importing this module are expected to track development
-- closely.

module Data.HashMap.Internal
    (
      HashMap(..)
    , Leaf(..)

      -- * Construction
    , empty
    , singleton

      -- * Basic interface
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

      -- * Combine
      -- ** Union
    , union
    , unionWith
    , unionWithKey
    , unions

    -- ** Compose
    , compose

      -- * Transformations
    , map
    , mapWithKey
    , traverseWithKey
    , mapKeys

      -- * Difference and intersection
    , difference
    , differenceWith
    , intersection
    , intersectionWith
    , intersectionWithKey
    , intersectionWithKey#

      -- * Folds
    , foldr'
    , foldl'
    , foldrWithKey'
    , foldlWithKey'
    , foldr
    , foldl
    , foldrWithKey
    , foldlWithKey
    , foldMapWithKey

      -- * Filter
    , mapMaybe
    , mapMaybeWithKey
    , filter
    , filterWithKey

      -- * Conversions
    , keys
    , elems

      -- ** Lists
    , toList
    , fromList
    , fromListWith
    , fromListWithKey

      -- ** Internals used by the strict version
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

-- | Convenience function.  Compute a hash value for the given value.
hash :: H.Hashable a => a -> Hash
hash = fromIntegral . H.hash

data Leaf k v = L !k v
  deriving (Eq)

instance (NFData k, NFData v) => NFData (Leaf k v) where
    rnf (L k v) = rnf k `seq` rnf v

-- | @since 0.2.17.0
instance (TH.Lift k, TH.Lift v) => TH.Lift (Leaf k v) where
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped (L k v) = [|| L k $! v ||]
#else
  lift (L k v) = [| L k $! v |]
#endif

-- | @since 0.2.14.0
instance NFData k => NFData1 (Leaf k) where
    liftRnf = liftRnf2 rnf

-- | @since 0.2.14.0
instance NFData2 Leaf where
    liftRnf2 rnf1 rnf2 (L k v) = rnf1 k `seq` rnf2 v

-- | A map from keys to values.  A map cannot contain duplicate keys;
-- each key can map to at most one value.
data HashMap k v
    = Empty
    -- ^ Invariants:
    --
    -- * 'Empty' is not a valid sub-node. It can only appear at the root. (INV1)
    | BitmapIndexed !Bitmap !(A.Array (HashMap k v))
    -- ^ Invariants:
    --
    -- * Only the lower @maxChildren@ bits of the 'Bitmap' may be set. The
    --   remaining upper bits must be 0. (INV2)
    -- * The array of a 'BitmapIndexed' node stores at least 1 and at most
    --   @'maxChildren' - 1@ sub-nodes. (INV3)
    -- * The number of sub-nodes is equal to the number of 1-bits in its
    --   'Bitmap'. (INV4)
    -- * If a 'BitmapIndexed' node has only one sub-node, this sub-node must
    --   be a 'BitmapIndexed' or a 'Full' node. (INV5)
    | Leaf !Hash !(Leaf k v)
    -- ^ Invariants:
    --
    -- * The location of a 'Leaf' or 'Collision' node in the tree must be
    --   compatible with its 'Hash'. (INV6)
    --   (TODO: Document this properly (#425))
    -- * The 'Hash' of a 'Leaf' node must be the 'hash' of its key. (INV7)
    | Full !(A.Array (HashMap k v))
    -- ^ Invariants:
    --
    -- * The array of a 'Full' node stores exactly 'maxChildren' sub-nodes. (INV8)
    | Collision !Hash !(A.Array (Leaf k v))
    -- ^ Invariants:
    --
    -- * The location of a 'Leaf' or 'Collision' node in the tree must be
    --   compatible with its 'Hash'. (INV6)
    --   (TODO: Document this properly (#425))
    -- * The array of a 'Collision' node must contain at least two sub-nodes. (INV9)
    -- * The 'hash' of each key in a 'Collision' node must be the one stored in
    --   the node. (INV7)
    -- * No two keys stored in a 'Collision' can be equal according to their
    --   'Eq' instance. (INV10)

type role HashMap nominal representational

-- | @since 0.2.17.0
deriving instance (TH.Lift k, TH.Lift v) => TH.Lift (HashMap k v)

instance (NFData k, NFData v) => NFData (HashMap k v) where
    rnf Empty                 = ()
    rnf (BitmapIndexed _ ary) = rnf ary
    rnf (Leaf _ l)            = rnf l
    rnf (Full ary)            = rnf ary
    rnf (Collision _ ary)     = rnf ary

-- | @since 0.2.14.0
instance NFData k => NFData1 (HashMap k) where
    liftRnf = liftRnf2 rnf

-- | @since 0.2.14.0
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
    {-# INLINE foldMap #-}
    foldr = foldr
    {-# INLINE foldr #-}
    foldl = foldl
    {-# INLINE foldl #-}
    foldr' = foldr'
    {-# INLINE foldr' #-}
    foldl' = foldl'
    {-# INLINE foldl' #-}
    null = null
    {-# INLINE null #-}
    length = size
    {-# INLINE length #-}

-- | @since 0.2.11
instance Bifoldable HashMap where
    bifoldMap f g = foldMapWithKey (\ k v -> f k `mappend` g v)
    {-# INLINE bifoldMap #-}
    bifoldr f g = foldrWithKey (\ k v acc -> k `f` (v `g` acc))
    {-# INLINE bifoldr #-}
    bifoldl f g = foldlWithKey (\ acc k v -> (acc `f` k) `g` v)
    {-# INLINE bifoldl #-}

-- | '<>' = 'union'
--
-- If a key occurs in both maps, the mapping from the first will be the mapping in the result.
--
-- ==== __Examples__
--
-- >>> fromList [(1,'a'),(2,'b')] <> fromList [(2,'c'),(3,'d')]
-- fromList [(1,'a'),(2,'b'),(3,'d')]
instance (Eq k, Hashable k) => Semigroup (HashMap k v) where
  (<>) = union
  {-# INLINE (<>) #-}
  stimes = stimesIdempotentMonoid
  {-# INLINE stimes #-}

-- | 'mempty' = 'empty'
--
-- 'mappend' = 'union'
--
-- If a key occurs in both maps, the mapping from the first will be the mapping in the result.
--
-- ==== __Examples__
--
-- >>> mappend (fromList [(1,'a'),(2,'b')]) (fromList [(2,'c'),(3,'d')])
-- fromList [(1,'a'),(2,'b'),(3,'d')]
instance (Eq k, Hashable k) => Monoid (HashMap k v) where
  mempty = empty
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

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

-- | This type is used to store the hash of a key, as produced with 'hash'.
type Hash   = Word

-- | A bitmap as contained by a 'BitmapIndexed' node, or a 'fullBitmap'
-- corresponding to a 'Full' node.
--
-- Only the lower 'maxChildren' bits are used. The remaining bits must be zeros.
type Bitmap = Word

-- | 'Shift' values correspond to the level of the tree that we're currently
-- operating at. At the root level the 'Shift' is @0@. For the subsequent
-- levels the 'Shift' values are 'bitsPerSubkey', @2*'bitsPerSubkey'@ etc.
--
-- Valid values are non-negative and less than @bitSize (0 :: Word)@.
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
    {-# INLINABLE traverse #-}

instance Eq2 HashMap where
    liftEq2 = equal2

instance Eq k => Eq1 (HashMap k) where
    liftEq = equal1

-- | Note that, in the presence of hash collisions, equal @HashMap@s may
-- behave differently, i.e. extensionality may be violated:
--
-- >>> data D = A | B deriving (Eq, Show)
-- >>> instance Hashable D where hashWithSalt salt _d = salt
--
-- >>> x = fromList [(A,1), (B,2)]
-- >>> y = fromList [(B,2), (A,1)]
--
-- >>> x == y
-- True
-- >>> toList x
-- [(A,1),(B,2)]
-- >>> toList y
-- [(B,2),(A,1)]
--
-- In general, the lack of extensionality can be observed with any function
-- that depends on the key ordering, such as folds and traversals.
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
    -- If the two trees are the same, then their lists of 'Leaf's and
    -- 'Collision's read from left to right should be the same (modulo the
    -- order of elements in 'Collision').

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

-- | The ordering is total and consistent with the `Eq` instance. However,
-- nothing else about the ordering is specified, and it may change from
-- version to version of either this package or of hashable.
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

-- Same as 'equal2' but doesn't compare the values.
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

-- Same as 'equal1' but doesn't compare the values.
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
        -- go :: Int -> [HashMap k v] -> Int
        go s [] = s
        go s (Leaf _ l : tl)
          = s `hashLeafWithSalt` l `go` tl
        -- For collisions we hashmix hash value
        -- and then array of values' hashes sorted
        go s (Collision h a : tl)
          = (s `H.hashWithSalt` h) `hashCollisionWithSalt` a `go` tl
        go s (_ : tl) = s `go` tl

        -- hashLeafWithSalt :: Int -> Leaf k v -> Int
        hashLeafWithSalt s (L k v) = (s `hk` k) `hv` v

        -- hashCollisionWithSalt :: Int -> A.Array (Leaf k v) -> Int
        hashCollisionWithSalt s
          = List.foldl' H.hashWithSalt s . arrayHashesSorted s

        -- arrayHashesSorted :: Int -> A.Array (Leaf k v) -> [Int]
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
        -- For collisions we hashmix hash value
        -- and then array of values' hashes sorted
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

-- | Helper to get 'Leaf's and 'Collision's as a list.
leavesAndCollisions :: HashMap k v -> [HashMap k v] -> [HashMap k v]
leavesAndCollisions (BitmapIndexed _ ary) a = A.foldr leavesAndCollisions a ary
leavesAndCollisions (Full ary)            a = A.foldr leavesAndCollisions a ary
leavesAndCollisions l@(Leaf _ _)          a = l : a
leavesAndCollisions c@(Collision _ _)     a = c : a
leavesAndCollisions Empty                 a = a

-- | Helper function to detect 'Leaf's and 'Collision's.
isLeafOrCollision :: HashMap k v -> Bool
isLeafOrCollision (Leaf _ _)      = True
isLeafOrCollision (Collision _ _) = True
isLeafOrCollision _               = False

------------------------------------------------------------------------
-- * Construction

-- | \(O(1)\) Construct an empty map.
empty :: HashMap k v
empty = Empty

-- | \(O(1)\) Construct a map with a single element.
singleton :: (Hashable k) => k -> v -> HashMap k v
singleton k v = Leaf (hash k) (L k v)

------------------------------------------------------------------------
-- * Basic interface

-- | \(O(1)\) Return 'True' if this map is empty, 'False' otherwise.
null :: HashMap k v -> Bool
null Empty = True
null _   = False

-- | \(O(n)\) Return the number of key-value mappings in this map.
size :: HashMap k v -> Int
size t = go t 0
  where
    go Empty                !n = n
    go (Leaf _ _)            n = n + 1
    go (BitmapIndexed _ ary) n = A.foldl' (flip go) n ary
    go (Full ary)            n = A.foldl' (flip go) n ary
    go (Collision _ ary)     n = n + A.length ary

-- | \(O(\log n)\) Return 'True' if the specified key is present in the
-- map, 'False' otherwise.
member :: (Eq k, Hashable k) => k -> HashMap k a -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _  -> True
{-# INLINABLE member #-}

-- | \(O(\log n)\) Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
lookup :: (Eq k, Hashable k) => k -> HashMap k v -> Maybe v
-- GHC does not yet perform a worker-wrapper transformation on
-- unboxed sums automatically. That seems likely to happen at some
-- point (possibly as early as GHC 8.6) but for now we do it manually.
lookup k m = case lookup# k m of
  (# (# #) | #) -> Nothing
  (# | a #) -> Just a
{-# INLINE lookup #-}

lookup# :: (Eq k, Hashable k) => k -> HashMap k v -> (# (# #) | v #)
lookup# k m = lookupCont (\_ -> (# (# #) | #)) (\v _i -> (# | v #)) (hash k) k 0 m
{-# INLINABLE lookup# #-}

-- | lookup' is a version of lookup that takes the hash separately.
-- It is used to implement alterF.
lookup' :: Eq k => Hash -> k -> HashMap k v -> Maybe v
-- GHC does not yet perform a worker-wrapper transformation on
-- unboxed sums automatically. That seems likely to happen at some
-- point (possibly as early as GHC 8.6) but for now we do it manually.
-- lookup' would probably prefer to be implemented in terms of its own
-- lookup'#, but it's not important enough and we don't want too much
-- code.
lookup' h k m = case lookupRecordCollision# h k m of
  (# (# #) | #) -> Nothing
  (# | (# a, _i #) #) -> Just a
{-# INLINE lookup' #-}

-- The result of a lookup, keeping track of if a hash collision occurred.
-- If a collision did not occur then it will have the Int value (-1).
data LookupRes a = Absent | Present a !Int

lookupResToMaybe :: LookupRes a -> Maybe a
lookupResToMaybe Absent        = Nothing
lookupResToMaybe (Present x _) = Just x
{-# INLINE lookupResToMaybe #-}

-- Internal helper for lookup. This version takes the precomputed hash so
-- that functions that make multiple calls to lookup and related functions
-- (insert, delete) only need to calculate the hash once.
--
-- It is used by 'alterF' so that hash computation and key comparison only needs
-- to be performed once. With this information you can use the more optimized
-- versions of insert ('insertNewKey', 'insertKeyExists') and delete
-- ('deleteKeyExists')
--
-- Outcomes:
--   Key not in map           => Absent
--   Key in map, no collision => Present v (-1)
--   Key in map, collision    => Present v position
lookupRecordCollision :: Eq k => Hash -> k -> HashMap k v -> LookupRes v
lookupRecordCollision h k m = case lookupRecordCollision# h k m of
  (# (# #) | #) -> Absent
  (# | (# a, i #) #) -> Present a (I# i) -- GHC will eliminate the I#
{-# INLINE lookupRecordCollision #-}

-- Why do we produce an Int# instead of an Int? Unfortunately, GHC is not
-- yet any good at unboxing things *inside* products, let alone sums. That
-- may be changing in GHC 8.6 or so (there is some work in progress), but
-- for now we use Int# explicitly here. We don't need to push the Int#
-- into lookupCont because inlining takes care of that.
lookupRecordCollision# :: Eq k => Hash -> k -> HashMap k v -> (# (# #) | (# v, Int# #) #)
lookupRecordCollision# h k m =
    lookupCont (\_ -> (# (# #) | #)) (\v (I# i) -> (# | (# v, i #) #)) h k 0 m
-- INLINABLE to specialize to the Eq instance.
{-# INLINABLE lookupRecordCollision# #-}

-- A two-continuation version of lookupRecordCollision. This lets us
-- share source code between lookup and lookupRecordCollision without
-- risking any performance degradation.
--
-- The absent continuation has type @((# #) -> r)@ instead of just @r@
-- so we can be representation-polymorphic in the result type. Since
-- this whole thing is always inlined, we don't have to worry about
-- any extra CPS overhead.
--
-- The @Int@ argument is the offset of the subkey in the hash. When looking up
-- keys at the top-level of a hashmap, the offset should be 0. When looking up
-- keys at level @n@ of a hashmap, the offset should be @n * bitsPerSubkey@.
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
{-# INLINE lookupCont #-}

-- | \(O(\log n)\) Return the value to which the specified key is mapped,
-- or 'Nothing' if this map contains no mapping for the key.
--
-- This is a flipped version of 'lookup'.
--
-- @since 0.2.11
(!?) :: (Eq k, Hashable k) => HashMap k v -> k -> Maybe v
(!?) m k = lookup k m
{-# INLINE (!?) #-}


-- | \(O(\log n)\) Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
--
-- @since 0.2.11
findWithDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> HashMap k v -> v
findWithDefault def k t = case lookup k t of
    Just v -> v
    _      -> def
{-# INLINABLE findWithDefault #-}


-- | \(O(\log n)\) Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
--
-- DEPRECATED: lookupDefault is deprecated as of version 0.2.11, replaced
-- by 'findWithDefault'.
lookupDefault :: (Eq k, Hashable k)
              => v          -- ^ Default value to return.
              -> k -> HashMap k v -> v
lookupDefault = findWithDefault
{-# INLINE lookupDefault #-}

-- | \(O(\log n)\) Return the value to which the specified key is mapped.
-- Calls 'error' if this map contains no mapping for the key.
(!) :: (Eq k, Hashable k, HasCallStack) => HashMap k v -> k -> v
(!) m k = case lookup k m of
    Just v  -> v
    Nothing -> error "Data.HashMap.Internal.(!): key not found"
{-# INLINABLE (!) #-}

infixl 9 !

-- | Create a 'Collision' value with two 'Leaf' values.
collision :: Hash -> Leaf k v -> Leaf k v -> HashMap k v
collision h !e1 !e2 =
    let v = A.run $ do mary <- A.new 2 e1
                       A.write mary 1 e2
                       return mary
    in Collision h v
{-# INLINE collision #-}

-- | Create a 'BitmapIndexed' or 'Full' node.
bitmapIndexedOrFull :: Bitmap -> A.Array (HashMap k v) -> HashMap k v
-- The strictness in @ary@ helps achieve a nice code size reduction in
-- @unionWith[Key]@ with GHC 9.2.2. See the Core diffs in
-- https://github.com/haskell-unordered-containers/unordered-containers/pull/376.
bitmapIndexedOrFull b !ary
    | b == fullBitmap = Full ary
    | otherwise         = BitmapIndexed b ary
{-# INLINE bitmapIndexedOrFull #-}

-- | \(O(\log n)\) Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Eq k, Hashable k) => k -> v -> HashMap k v -> HashMap k v
insert k v m = insert' (hash k) k v m
{-# INLINABLE insert #-}

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
{-# INLINABLE insert' #-}

-- Insert optimized for the case when we know the key is not in the map.
--
-- It is only valid to call this when the key does not exist in the map.
--
-- We can skip:
--  - the key equality check on a Leaf
--  - check for its existence in the array for a hash collision
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
{-# NOINLINE insertNewKey #-}


-- Insert optimized for the case when we know the key is in the map.
--
-- It is only valid to call this when the key exists in the map and you know the
-- hash collision position if there was one. This information can be obtained
-- from 'lookupRecordCollision'. If there is no collision, pass (-1) as collPos
-- (first argument).
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

    -- Customized version of 'index' that doesn't require a 'Shift'.
    index' :: Hash -> Int
    index' w = fromIntegral $ w .&. subkeyMask
    {-# INLINE index' #-}

    -- Customized version of 'mask' that doesn't require a 'Shift'.
    mask' :: Word -> Bitmap
    mask' w = 1 `unsafeShiftL` index' w
    {-# INLINE mask' #-}

    shiftHash h = h `unsafeShiftR` bitsPerSubkey
    {-# INLINE shiftHash #-}

{-# NOINLINE insertKeyExists #-}

-- Replace the ith Leaf with Leaf k v.
--
-- This does not check that @i@ is within bounds of the array.
setAtPosition :: Int -> k -> v -> A.Array (Leaf k v) -> A.Array (Leaf k v)
setAtPosition i k x ary = A.update ary i (L k x)
{-# INLINE setAtPosition #-}


-- | In-place update version of insert
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
{-# INLINABLE unsafeInsert #-}

-- | Create a map from two key-value pairs which hashes don't collide. To
-- enhance sharing, the second key-value pair is represented by the hash of its
-- key and a singleton HashMap pairing its key with its value.
--
-- Note: to avoid silly thunks, this function must be strict in the
-- key. See issue #232. We don't need to force the HashMap argument
-- because it's already in WHNF (having just been matched) and we
-- just put it directly in an array.
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
        -- This way of computing idx2 saves us a branch compared to the previous approach:
        --
        -- idx2 | index h1 s < index h2 s = 1
        --      | otherwise               = 0
        --
        -- See https://github.com/haskell-unordered-containers/unordered-containers/issues/75#issuecomment-1128419337
{-# INLINE two #-}

-- | \(O(\log n)\) Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
insertWith :: (Eq k, Hashable k) => (v -> v -> v) -> k -> v -> HashMap k v
            -> HashMap k v
-- We're not going to worry about allocating a function closure
-- to pass to insertModifying. See comments at 'adjust'.
insertWith f k new m = insertModifying new (\old -> (# f new old #)) k m
{-# INLINE insertWith #-}

-- | @insertModifying@ is a lot like insertWith; we use it to implement alterF.
-- It takes a value to insert when the key is absent and a function
-- to apply to calculate a new value when the key is present. Thanks
-- to the unboxed unary tuple, we avoid introducing any unnecessary
-- thunks in the tree.
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
{-# INLINABLE insertModifying #-}

-- Like insertModifying for arrays; used to implement insertModifying
insertModifyingArr :: Eq k => v -> (v -> (# v #)) -> k -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
insertModifyingArr x f k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
          -- Not found, append to the end.
        | i >= n = A.snoc ary $ L k x
        | otherwise = case A.index ary i of
            (L kx y) | k == kx   -> case f y of
                                      (# y' #) -> if ptrEq y y'
                                                  then ary
                                                  else A.update ary i (L k y')
                     | otherwise -> go k ary (i+1) n
{-# INLINE insertModifyingArr #-}

-- | In-place update version of insertWith
unsafeInsertWith :: forall k v. (Eq k, Hashable k)
                 => (v -> v -> v) -> k -> v -> HashMap k v
                 -> HashMap k v
unsafeInsertWith f k0 v0 m0 = unsafeInsertWithKey (\_ a b -> (# f a b #)) k0 v0 m0
{-# INLINABLE unsafeInsertWith #-}

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
{-# INLINABLE unsafeInsertWithKey #-}

-- | \(O(\log n)\) Remove the mapping for the specified key from this map
-- if present.
delete :: (Eq k, Hashable k) => k -> HashMap k v -> HashMap k v
delete k m = delete' (hash k) k m
{-# INLINABLE delete #-}

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
{-# INLINABLE delete' #-}

-- | Delete optimized for the case when we know the key is in the map.
--
-- It is only valid to call this when the key exists in the map and you know the
-- hash collision position if there was one. This information can be obtained
-- from 'lookupRecordCollision'. If there is no collision, pass (-1) as collPos.
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

    -- Customized version of 'index' that doesn't require a 'Shift'.
    index' :: Hash -> Int
    index' w = fromIntegral $ w .&. subkeyMask
    {-# INLINE index' #-}

    -- Customized version of 'mask' that doesn't require a 'Shift'.
    mask' :: Word -> Bitmap
    mask' w = 1 `unsafeShiftL` index' w
    {-# INLINE mask' #-}

    shiftHash h = h `unsafeShiftR` bitsPerSubkey
    {-# INLINE shiftHash #-}

{-# NOINLINE deleteKeyExists #-}

-- | \(O(\log n)\) Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
adjust :: (Eq k, Hashable k) => (v -> v) -> k -> HashMap k v -> HashMap k v
-- This operation really likes to leak memory, so using this
-- indirect implementation shouldn't hurt much. Furthermore, it allows
-- GHC to avoid a leak when the function is lazy. In particular,
--
--     adjust (const x) k m
-- ==> adjust# (\v -> (# const x v #)) k m
-- ==> adjust# (\_ -> (# x #)) k m
adjust f k m = adjust# (\v -> (# f v #)) k m
{-# INLINE adjust #-}

-- | Much like 'adjust', but not inherently leaky.
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
{-# INLINABLE adjust# #-}

-- | \(O(\log n)\)  The expression @('update' f k map)@ updates the value @x@ at @k@
-- (if it is in the map). If @(f x)@ is 'Nothing', the element is deleted.
-- If it is @('Just' y)@, the key @k@ is bound to the new value @y@.
update :: (Eq k, Hashable k) => (a -> Maybe a) -> k -> HashMap k a -> HashMap k a
update f = alter (>>= f)
{-# INLINABLE update #-}


-- | \(O(\log n)\)  The expression @('alter' f k map)@ alters the value @x@ at @k@, or
-- absence thereof.
--
-- 'alter' can be used to insert, delete, or update a value in a map. In short:
--
-- @
-- 'lookup' k ('alter' f k m) = f ('lookup' k m)
-- @
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
{-# INLINABLE alter #-}

-- | \(O(\log n)\)  The expression @('alterF' f k map)@ alters the value @x@ at
-- @k@, or absence thereof.
--
--  'alterF' can be used to insert, delete, or update a value in a map.
--
-- Note: 'alterF' is a flipped version of the 'at' combinator from
-- <https://hackage.haskell.org/package/lens/docs/Control-Lens-At.html#v:at Control.Lens.At>.
--
-- @since 0.2.10
alterF :: (Functor f, Eq k, Hashable k)
       => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
-- We only calculate the hash once, but unless this is rewritten
-- by rules we may test for key equality multiple times.
-- We force the value of the map for consistency with the rewritten
-- version; otherwise someone could tell the difference using a lazy
-- @f@ and a functor that is similar to Const but not actually Const.
alterF f = \ !k !m ->
  let
    !h = hash k
    mv = lookup' h k m
  in (<$> f mv) $ \case
    Nothing -> maybe m (const (delete' h k m)) mv
    Just v' -> insert' h k v' m

-- We unconditionally rewrite alterF in RULES, but we expose an
-- unfolding just in case it's used in some way that prevents the
-- rule from firing.
{-# INLINABLE [0] alterF #-}

-- This is just a bottom value. See the comment on the "alterFWeird"
-- rule.
test_bottom :: a
test_bottom = error "Data.HashMap.alterF internal error: hit test_bottom"

-- We use this as an error result in RULES to ensure we don't get
-- any useless CallStack nonsense.
bogus# :: (# #) -> (# a #)
bogus# _ = error "Data.HashMap.alterF internal error: hit bogus#"

{-# RULES
-- We probe the behavior of @f@ by applying it to Nothing and to
-- Just test_bottom. Based on the results, and how they relate to
-- each other, we choose the best implementation.

"alterFWeird" forall f. alterF f =
   alterFWeird (f Nothing) (f (Just test_bottom)) f

-- This rule covers situations where alterF is used to simply insert or
-- delete in Identity (most likely via Control.Lens.At). We recognize here
-- (through the repeated @x@ on the LHS) that
--
-- @f Nothing = f (Just bottom)@,
--
-- which guarantees that @f@ doesn't care what its argument is, so
-- we don't have to either.
--
-- Why only Identity? A variant of this rule is actually valid regardless of
-- the functor, but for some functors (e.g., []), it can lead to the
-- same keys being compared multiple times, which is bad if they're
-- ugly things like strings. This is unfortunate, since the rule is likely
-- a good idea for almost all realistic uses, but I don't like nasty
-- edge cases.
"alterFconstant" forall (f :: Maybe a -> Identity (Maybe a)) x.
  alterFWeird x x f = \ !k !m ->
    Identity (case runIdentity x of {Nothing -> delete k m; Just a -> insert k a m})

-- This rule handles the case where 'alterF' is used to do 'insertWith'-like
-- things. Whenever possible, GHC will get rid of the Maybe nonsense for us.
-- We delay this rule to stage 1 so alterFconstant has a chance to fire.
"alterFinsertWith" [1] forall (f :: Maybe a -> Identity (Maybe a)) x y.
  alterFWeird (coerce (Just x)) (coerce (Just y)) f =
    coerce (insertModifying x (\mold -> case runIdentity (f (Just mold)) of
                                            Nothing -> bogus# (# #)
                                            Just new -> (# new #)))

-- Handle the case where someone uses 'alterF' instead of 'adjust'. This
-- rule is kind of picky; it will only work if the function doesn't
-- do anything between case matching on the Maybe and producing a result.
"alterFadjust" forall (f :: Maybe a -> Identity (Maybe a)) _y.
  alterFWeird (coerce Nothing) (coerce (Just _y)) f =
    coerce (adjust# (\x -> case runIdentity (f (Just x)) of
                               Just x' -> (# x' #)
                               Nothing -> bogus# (# #)))

-- The simple specialization to Const; in this case we can look up
-- the key without caring what position it's in. This is only a tiny
-- optimization.
"alterFlookup" forall _ign1 _ign2 (f :: Maybe a -> Const r (Maybe a)).
  alterFWeird _ign1 _ign2 f = \ !k !m -> Const (getConst (f (lookup k m)))
 #-}

-- This is a very unsafe version of alterF used for RULES. When calling
-- alterFWeird x y f, the following *must* hold:
--
-- x = f Nothing
-- y = f (Just _|_)
--
-- Failure to abide by these laws will make demons come out of your nose.
alterFWeird
       :: (Functor f, Eq k, Hashable k)
       => f (Maybe v)
       -> f (Maybe v)
       -> (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
alterFWeird _ _ f = alterFEager f
{-# INLINE [0] alterFWeird #-}

-- | This is the default version of alterF that we use in most non-trivial
-- cases. It's called "eager" because it looks up the given key in the map
-- eagerly, whether or not the given function requires that information.
alterFEager :: (Functor f, Eq k, Hashable k)
       => (Maybe v -> f (Maybe v)) -> k -> HashMap k v -> f (HashMap k v)
alterFEager f !k m = (<$> f mv) $ \case

    ------------------------------
    -- Delete the key from the map.
    Nothing -> case lookupRes of

      -- Key did not exist in the map to begin with, no-op
      Absent -> m

      -- Key did exist
      Present _ collPos -> deleteKeyExists collPos h k m

    ------------------------------
    -- Update value
    Just v' -> case lookupRes of

      -- Key did not exist before, insert v' under a new key
      Absent -> insertNewKey h k v' m

      -- Key existed before
      Present v collPos ->
        if v `ptrEq` v'
        -- If the value is identical, no-op
        then m
        -- If the value changed, update the value.
        else insertKeyExists collPos h k v' m

  where !h = hash k
        !lookupRes = lookupRecordCollision h k m
        !mv = lookupResToMaybe lookupRes
{-# INLINABLE alterFEager #-}

-- | \(O(n \log m)\) Inclusion of maps. A map is included in another map if the keys
-- are subsets and the corresponding values are equal:
--
-- > isSubmapOf m1 m2 = keys m1 `isSubsetOf` keys m2 &&
-- >                    and [ v1 == v2 | (k1,v1) <- toList m1; let v2 = m2 ! k1 ]
--
-- ==== __Examples__
--
-- >>> fromList [(1,'a')] `isSubmapOf` fromList [(1,'a'),(2,'b')]
-- True
--
-- >>> fromList [(1,'a'),(2,'b')] `isSubmapOf` fromList [(1,'a')]
-- False
--
-- @since 0.2.12
isSubmapOf :: (Eq k, Hashable k, Eq v) => HashMap k v -> HashMap k v -> Bool
isSubmapOf = Exts.inline isSubmapOfBy (==)
{-# INLINABLE isSubmapOf #-}

-- | \(O(n \log m)\) Inclusion of maps with value comparison. A map is included in
-- another map if the keys are subsets and if the comparison function is true
-- for the corresponding values:
--
-- > isSubmapOfBy cmpV m1 m2 = keys m1 `isSubsetOf` keys m2 &&
-- >                           and [ v1 `cmpV` v2 | (k1,v1) <- toList m1; let v2 = m2 ! k1 ]
--
-- ==== __Examples__
--
-- >>> isSubmapOfBy (<=) (fromList [(1,'a')]) (fromList [(1,'b'),(2,'c')])
-- True
--
-- >>> isSubmapOfBy (<=) (fromList [(1,'b')]) (fromList [(1,'a'),(2,'c')])
-- False
--
-- @since 0.2.12
isSubmapOfBy :: (Eq k, Hashable k) => (v1 -> v2 -> Bool) -> HashMap k v1 -> HashMap k v2 -> Bool
-- For maps without collisions the complexity is O(n*log m), where n is the size
-- of m1 and m the size of m2: the inclusion operation visits every leaf in m1 at least once.
-- For each leaf in m1, it looks up the key in m2.
--
-- The worst case complexity is O(n*m). The worst case is when both hashmaps m1
-- and m2 are collision nodes for the same hash. Since collision nodes are
-- unsorted arrays, it requires for every key in m1 a linear search to to find a
-- matching key in m2, hence O(n*m).
isSubmapOfBy comp !m1 !m2 = go 0 m1 m2
  where
    -- An empty map is always a submap of any other map.
    go _ Empty _ = True

    -- If the second map is empty and the first is not, it cannot be a submap.
    go _ _ Empty = False

    -- If the first map contains only one entry, lookup the key in the second map.
    go s (Leaf h1 (L k1 v1)) t2 = lookupCont (\_ -> False) (\v2 _ -> comp v1 v2) h1 k1 s t2

    -- In this case, we need to check that for each x in ls1, there is a y in
    -- ls2 such that x `comp` y. This is the worst case complexity-wise since it
    -- requires a O(m*n) check.
    go _ (Collision h1 ls1) (Collision h2 ls2) =
      h1 == h2 && subsetArray comp ls1 ls2

    -- In this case, we only need to check the entries in ls2 with the hash h1.
    go s t1@(Collision h1 _) (BitmapIndexed b ls2)
        | b .&. m == 0 = False
        | otherwise    =
            go (nextShift s) t1 (A.index ls2 (sparseIndex b m))
      where m = mask h1 s

    -- Similar to the previous case we need to traverse l2 at the index for the hash h1.
    go s t1@(Collision h1 _) (Full ls2) =
      go (nextShift s) t1 (A.index ls2 (index h1 s))

    -- In cases where the first and second map are BitmapIndexed or Full,
    -- traverse down the tree at the appropriate indices.
    go s (BitmapIndexed b1 ls1) (BitmapIndexed b2 ls2) =
      submapBitmapIndexed (go (nextShift s)) b1 ls1 b2 ls2
    go s (BitmapIndexed b1 ls1) (Full ls2) =
      submapBitmapIndexed (go (nextShift s)) b1 ls1 fullBitmap ls2
    go s (Full ls1) (Full ls2) =
      submapBitmapIndexed (go (nextShift s)) fullBitmap ls1 fullBitmap ls2

    -- Collision and Full nodes always contain at least two entries. Hence it
    -- cannot be a map of a leaf.
    go _ (Collision {}) (Leaf {}) = False
    go _ (BitmapIndexed {}) (Leaf {}) = False
    go _ (Full {}) (Leaf {}) = False
    go _ (BitmapIndexed {}) (Collision {}) = False
    go _ (Full {}) (Collision {}) = False
    go _ (Full {}) (BitmapIndexed {}) = False
{-# INLINABLE isSubmapOfBy #-}

-- | \(O(\min n m))\) Checks if a bitmap indexed node is a submap of another.
submapBitmapIndexed :: (HashMap k v1 -> HashMap k v2 -> Bool) -> Bitmap -> A.Array (HashMap k v1) -> Bitmap -> A.Array (HashMap k v2) -> Bool
submapBitmapIndexed comp !b1 !ary1 !b2 !ary2 = subsetBitmaps && go 0 0 (b1Orb2 .&. negate b1Orb2)
  where
    go :: Int -> Int -> Bitmap -> Bool
    go !i !j !m
      | m > b1Orb2 = True

      -- In case a key is both in ary1 and ary2, check ary1[i] <= ary2[j] and
      -- increment the indices i and j.
      | b1Andb2 .&. m /= 0 = comp (A.index ary1 i) (A.index ary2 j) &&
                             go (i+1) (j+1) (m `unsafeShiftL` 1)

      -- In case a key occurs in ary1, but not ary2, only increment index j.
      | b2 .&. m /= 0 = go i (j+1) (m `unsafeShiftL` 1)

      -- In case a key neither occurs in ary1 nor ary2, continue.
      | otherwise = go i j (m `unsafeShiftL` 1)

    b1Andb2 = b1 .&. b2
    b1Orb2  = b1 .|. b2
    subsetBitmaps = b1Orb2 == b2
{-# INLINABLE submapBitmapIndexed #-}

------------------------------------------------------------------------
-- * Combine

-- | \(O(n+m)\) The union of two maps. If a key occurs in both maps, the
-- mapping from the first will be the mapping in the result.
--
-- ==== __Examples__
--
-- >>> union (fromList [(1,'a'),(2,'b')]) (fromList [(2,'c'),(3,'d')])
-- fromList [(1,'a'),(2,'b'),(3,'d')]
union :: Eq k => HashMap k v -> HashMap k v -> HashMap k v
union = unionWith const
{-# INLINABLE union #-}

-- | \(O(n+m)\) The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the
-- result.
unionWith :: Eq k => (v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWith f = unionWithKey (const f)
{-# INLINE unionWith #-}

-- | \(O(n+m)\) The union of two maps.  If a key occurs in both maps,
-- the provided function (first argument) will be used to compute the
-- result.
unionWithKey :: Eq k => (k -> v -> v -> v) -> HashMap k v -> HashMap k v
          -> HashMap k v
unionWithKey f = go 0
  where
    -- empty vs. anything
    go !_ t1 Empty = t1
    go _ Empty t2 = t2
    -- leaf vs. leaf
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
    -- branch vs. branch
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
    -- leaf vs. branch
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
{-# INLINE unionWithKey #-}

-- | Strict in the result of @f@.
unionArrayBy :: (a -> a -> a) -> Bitmap -> Bitmap -> A.Array a -> A.Array a
             -> A.Array a
-- The manual forcing of @b1@, @b2@, @ary1@ and @ary2@ results in handsome
-- Core size reductions with GHC 9.2.2. See the Core diffs in
-- https://github.com/haskell-unordered-containers/unordered-containers/pull/376.
unionArrayBy f !b1 !b2 !ary1 !ary2 = A.run $ do
    let bCombined = b1 .|. b2
    mary <- A.new_ (popCount bCombined)
    -- iterate over nonzero bits of b1 .|. b2
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
    -- TODO: For the case where b1 .&. b2 == b1, i.e. when one is a
    -- subset of the other, we could use a slightly simpler algorithm,
    -- where we copy one array, and then update.
{-# INLINE unionArrayBy #-}

-- TODO: Figure out the time complexity of 'unions'.

-- | Construct a set containing all elements from a list of sets.
unions :: Eq k => [HashMap k v] -> HashMap k v
unions = List.foldl' union empty
{-# INLINE unions #-}


------------------------------------------------------------------------
-- * Compose

-- | Relate the keys of one map to the values of
-- the other, by using the values of the former as keys for lookups
-- in the latter.
--
-- Complexity: \( O (n * \log(m)) \), where \(m\) is the size of the first argument
--
-- >>> compose (fromList [('a', "A"), ('b', "B")]) (fromList [(1,'a'),(2,'b'),(3,'z')])
-- fromList [(1,"A"),(2,"B")]
--
-- @
-- ('compose' bc ab '!?') = (bc '!?') <=< (ab '!?')
-- @
--
-- @since 0.2.13.0
compose :: (Eq b, Hashable b) => HashMap b c -> HashMap a b -> HashMap a c
compose bc !ab
  | null bc = empty
  | otherwise = mapMaybe (bc !?) ab

------------------------------------------------------------------------
-- * Transformations

-- | \(O(n)\) Transform this map by applying a function to every value.
mapWithKey :: (k -> v1 -> v2) -> HashMap k v1 -> HashMap k v2
mapWithKey f = go
  where
    go Empty = Empty
    go (Leaf h (L k v)) = Leaf h $ L k (f k v)
    go (BitmapIndexed b ary) = BitmapIndexed b $ A.map go ary
    go (Full ary) = Full $ A.map go ary
    -- Why map strictly over collision arrays? Because there's no
    -- point suspending the O(1) work this does for each leaf.
    go (Collision h ary) = Collision h $
                           A.map' (\ (L k v) -> L k (f k v)) ary
{-# INLINE mapWithKey #-}

-- | \(O(n)\) Transform this map by applying a function to every value.
map :: (v1 -> v2) -> HashMap k v1 -> HashMap k v2
map f = mapWithKey (const f)
{-# INLINE map #-}

-- | \(O(n)\) Perform an 'Applicative' action for each key-value pair
-- in a 'HashMap' and produce a 'HashMap' of all the results.
--
-- Note: the order in which the actions occur is unspecified. In particular,
-- when the map contains hash collisions, the order in which the actions
-- associated with the keys involved will depend in an unspecified way on
-- their insertion order.
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
{-# INLINE traverseWithKey #-}

-- | \(O(n)\).
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key. In this case there is no guarantee which of the
-- associated values is chosen for the conflicting key.
--
-- >>> mapKeys (+ 1) (fromList [(5,"a"), (3,"b")])
-- fromList [(4,"b"),(6,"a")]
-- >>> mapKeys (\ _ -> 1) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")])
-- fromList [(1,"c")]
-- >>> mapKeys (\ _ -> 3) (fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")])
-- fromList [(3,"c")]
--
-- @since 0.2.14.0
mapKeys :: (Eq k2, Hashable k2) => (k1 -> k2) -> HashMap k1 v -> HashMap k2 v
mapKeys f = fromList . foldrWithKey (\k x xs -> (f k, x) : xs) []

------------------------------------------------------------------------
-- * Difference and intersection

-- | \(O(n \log m)\) Difference of two maps. Return elements of the first map
-- not existing in the second.
difference :: (Eq k, Hashable k) => HashMap k v -> HashMap k w -> HashMap k v
difference a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> unsafeInsert k v m
                 _       -> m
{-# INLINABLE difference #-}

-- | \(O(n \log m)\) Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
differenceWith :: (Eq k, Hashable k) => (v -> w -> Maybe v) -> HashMap k v -> HashMap k w -> HashMap k v
differenceWith f a b = foldlWithKey' go empty a
  where
    go m k v = case lookup k b of
                 Nothing -> unsafeInsert k v m
                 Just w  -> maybe m (\y -> unsafeInsert k y m) (f v w)
{-# INLINABLE differenceWith #-}

-- | \(O(n \log m)\) Intersection of two maps. Return elements of the first
-- map for keys existing in the second.
intersection :: Eq k => HashMap k v -> HashMap k w -> HashMap k v
intersection = Exts.inline intersectionWith const
{-# INLINABLE intersection #-}

-- | \(O(n \log m)\) Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWith :: Eq k => (v1 -> v2 -> v3) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWith f = Exts.inline intersectionWithKey $ const f
{-# INLINABLE intersectionWith #-}

-- | \(O(n \log m)\) Intersection of two maps. If a key occurs in both maps
-- the provided function is used to combine the values from the two
-- maps.
intersectionWithKey :: Eq k => (k -> v1 -> v2 -> v3) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWithKey f = intersectionWithKey# $ \k v1 v2 -> (# f k v1 v2 #)
{-# INLINABLE intersectionWithKey #-}

intersectionWithKey# :: Eq k => (k -> v1 -> v2 -> (# v3 #)) -> HashMap k v1 -> HashMap k v2 -> HashMap k v3
intersectionWithKey# f = go 0
  where
    -- empty vs. anything
    go !_ _ Empty = Empty
    go _ Empty _ = Empty
    -- leaf vs. anything
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
    -- collision vs. collision
    go _ (Collision h1 ls1) (Collision h2 ls2) = intersectionCollisions f h1 h2 ls1 ls2
    -- branch vs. branch
    go s (BitmapIndexed b1 ary1) (BitmapIndexed b2 ary2) =
      intersectionArrayBy (go (nextShift s)) b1 b2 ary1 ary2
    go s (BitmapIndexed b1 ary1) (Full ary2) =
      intersectionArrayBy (go (nextShift s)) b1 fullBitmap ary1 ary2
    go s (Full ary1) (BitmapIndexed b2 ary2) =
      intersectionArrayBy (go (nextShift s)) fullBitmap b2 ary1 ary2
    go s (Full ary1) (Full ary2) =
      intersectionArrayBy (go (nextShift s)) fullBitmap fullBitmap ary1 ary2
    -- collision vs. branch
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
{-# INLINE intersectionWithKey# #-}

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
    -- iterate over nonzero bits of b1 .|. b2
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
{-# INLINE intersectionArrayBy #-}

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
{-# INLINE intersectionCollisions #-}

-- | Say we have
-- @
-- 1 2 3 4
-- @
-- and we search for @3@. Then we can mutate the array to
-- @
-- undefined 2 1 4
-- @
-- We don't actually need to write undefined, we just have to make sure that the next search starts 1 after the current one.
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
{-# INLINE searchSwap #-}

------------------------------------------------------------------------
-- * Folds

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldl' :: (a -> v -> a) -> a -> HashMap k v -> a
foldl' f = foldlWithKey' (\ z _ v -> f z v)
{-# INLINE foldl' #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldr' :: (v -> a -> a) -> a -> HashMap k v -> a
foldr' f = foldrWithKey' (\ _ v z -> f v z)
{-# INLINE foldr' #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldlWithKey' :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey' f = go
  where
    go !z Empty                = z
    go z (Leaf _ (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = A.foldl' go z ary
    go z (Full ary)            = A.foldl' go z ary
    go z (Collision _ ary)     = A.foldl' (\ z' (L k v) -> f z' k v) z ary
{-# INLINE foldlWithKey' #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).  Each application of the operator
-- is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldrWithKey' :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey' f = flip go
  where
    go Empty z                 = z
    go (Leaf _ (L k v)) !z     = f k v z
    go (BitmapIndexed _ ary) !z = A.foldr' go z ary
    go (Full ary) !z           = A.foldr' go z ary
    go (Collision _ ary) !z    = A.foldr' (\ (L k v) z' -> f k v z') z ary
{-# INLINE foldrWithKey' #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldr :: (v -> a -> a) -> a -> HashMap k v -> a
foldr f = foldrWithKey (const f)
{-# INLINE foldr #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldl :: (a -> v -> a) -> a -> HashMap k v -> a
foldl f = foldlWithKey (\a _k v -> f a v)
{-# INLINE foldl #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- right-identity of the operator).
foldrWithKey :: (k -> v -> a -> a) -> a -> HashMap k v -> a
foldrWithKey f = flip go
  where
    go Empty z                 = z
    go (Leaf _ (L k v)) z      = f k v z
    go (BitmapIndexed _ ary) z = A.foldr go z ary
    go (Full ary) z            = A.foldr go z ary
    go (Collision _ ary) z     = A.foldr (\ (L k v) z' -> f k v z') z ary
{-# INLINE foldrWithKey #-}

-- | \(O(n)\) Reduce this map by applying a binary operator to all
-- elements, using the given starting value (typically the
-- left-identity of the operator).
foldlWithKey :: (a -> k -> v -> a) -> a -> HashMap k v -> a
foldlWithKey f = go
  where
    go z Empty                 = z
    go z (Leaf _ (L k v))      = f z k v
    go z (BitmapIndexed _ ary) = A.foldl go z ary
    go z (Full ary)            = A.foldl go z ary
    go z (Collision _ ary)     = A.foldl (\ z' (L k v) -> f z' k v) z ary
{-# INLINE foldlWithKey #-}

-- | \(O(n)\) Reduce the map by applying a function to each element
-- and combining the results with a monoid operation.
foldMapWithKey :: Monoid m => (k -> v -> m) -> HashMap k v -> m
foldMapWithKey f = go
  where
    go Empty = mempty
    go (Leaf _ (L k v)) = f k v
    go (BitmapIndexed _ ary) = A.foldMap go ary
    go (Full ary) = A.foldMap go ary
    go (Collision _ ary) = A.foldMap (\ (L k v) -> f k v) ary
{-# INLINE foldMapWithKey #-}

------------------------------------------------------------------------
-- * Filter

-- | \(O(n)\) Transform this map by applying a function to every value
--   and retaining only some of them.
mapMaybeWithKey :: (k -> v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybeWithKey f = filterMapAux onLeaf onColl
  where onLeaf (Leaf h (L k v)) | Just v' <- f k v = Just (Leaf h (L k v'))
        onLeaf _ = Nothing

        onColl (L k v) | Just v' <- f k v = Just (L k v')
                       | otherwise = Nothing
{-# INLINE mapMaybeWithKey #-}

-- | \(O(n)\) Transform this map by applying a function to every value
--   and retaining only some of them.
mapMaybe :: (v1 -> Maybe v2) -> HashMap k v1 -> HashMap k v2
mapMaybe f = mapMaybeWithKey (const f)
{-# INLINE mapMaybe #-}

-- | \(O(n)\) Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: forall k v. (k -> v -> Bool) -> HashMap k v -> HashMap k v
filterWithKey pred = filterMapAux onLeaf onColl
  where onLeaf t@(Leaf _ (L k v)) | pred k v = Just t
        onLeaf _ = Nothing

        onColl el@(L k v) | pred k v = Just el
        onColl _ = Nothing
{-# INLINE filterWithKey #-}


-- | Common implementation for 'filterWithKey' and 'mapMaybeWithKey',
--   allowing the former to former to reuse terms.
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
{-# INLINE filterMapAux #-}

-- | \(O(n)\) Filter this map by retaining only elements which values
-- satisfy a predicate.
filter :: (v -> Bool) -> HashMap k v -> HashMap k v
filter p = filterWithKey (\_ v -> p v)
{-# INLINE filter #-}

------------------------------------------------------------------------
-- * Conversions

-- TODO: Improve fusion rules by modelled them after the Prelude ones
-- on lists.

-- | \(O(n)\) Return a list of this map's keys.  The list is produced
-- lazily.
keys :: HashMap k v -> [k]
keys = List.map fst . toList
{-# INLINE keys #-}

-- | \(O(n)\) Return a list of this map's values.  The list is produced
-- lazily.
elems :: HashMap k v -> [v]
elems = List.map snd . toList
{-# INLINE elems #-}

------------------------------------------------------------------------
-- ** Lists

-- | \(O(n)\) Return a list of this map's elements.  The list is
-- produced lazily. The order of its elements is unspecified.
toList :: HashMap k v -> [(k, v)]
toList t = Exts.build (\ c z -> foldrWithKey (curry c) z t)
{-# INLINE toList #-}

-- | \(O(n)\) Construct a map with the supplied mappings.  If the list
-- contains duplicate mappings, the later mappings take precedence.
fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v
fromList = List.foldl' (\ m (k, v) -> unsafeInsert k v m) empty
{-# INLINABLE fromList #-}

-- | \(O(n \log n)\) Construct a map from a list of elements.  Uses
-- the provided function @f@ to merge duplicate entries with
-- @(f newVal oldVal)@.
--
-- === Examples
--
-- Given a list @xs@, create a map with the number of occurrences of each
-- element in @xs@:
--
-- > let xs = ['a', 'b', 'a']
-- > in fromListWith (+) [ (x, 1) | x <- xs ]
-- >
-- > = fromList [('a', 2), ('b', 1)]
--
-- Given a list of key-value pairs @xs :: [(k, v)]@, group all values by their
-- keys and return a @HashMap k [v]@.
--
-- > let xs = [('a', 1), ('b', 2), ('a', 3)]
-- > in fromListWith (++) [ (k, [v]) | (k, v) <- xs ]
-- >
-- > = fromList [('a', [3, 1]), ('b', [2])]
--
-- Note that the lists in the resulting map contain elements in reverse order
-- from their occurrences in the original list.
--
-- More generally, duplicate entries are accumulated as follows;
-- this matters when @f@ is not commutative or not associative.
--
-- > fromListWith f [(k, a), (k, b), (k, c), (k, d)]
-- > = fromList [(k, f d (f c (f b a)))]
fromListWith :: (Eq k, Hashable k) => (v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWith f = List.foldl' (\ m (k, v) -> unsafeInsertWith f k v m) empty
{-# INLINE fromListWith #-}

-- | \(O(n \log n)\) Construct a map from a list of elements.  Uses
-- the provided function to merge duplicate entries.
--
-- === Examples
--
-- Given a list of key-value pairs where the keys are of different flavours, e.g:
--
-- > data Key = Div | Sub
--
-- and the values need to be combined differently when there are duplicates,
-- depending on the key:
--
-- > combine Div = div
-- > combine Sub = (-)
--
-- then @fromListWithKey@ can be used as follows:
--
-- > fromListWithKey combine [(Div, 2), (Div, 6), (Sub, 2), (Sub, 3)]
-- > = fromList [(Div, 3), (Sub, 1)]
--
-- More generally, duplicate entries are accumulated as follows;
--
-- > fromListWith f [(k, a), (k, b), (k, c), (k, d)]
-- > = fromList [(k, f k d (f k c (f k b a)))]
--
-- @since 0.2.11
fromListWithKey :: (Eq k, Hashable k) => (k -> v -> v -> v) -> [(k, v)] -> HashMap k v
fromListWithKey f = List.foldl' (\ m (k, v) -> unsafeInsertWithKey (\k' a b -> (# f k' a b #)) k v m) empty
{-# INLINE fromListWithKey #-}

------------------------------------------------------------------------
-- Array operations

-- | \(O(n)\) Look up the value associated with the given key in an
-- array.
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
{-# INLINE lookupInArrayCont #-}

-- | \(O(n)\) Lookup the value associated with the given key in this
-- array.  Returns 'Nothing' if the key wasn't found.
indexOf :: Eq k => k -> A.Array (Leaf k v) -> Maybe Int
indexOf k0 ary0 = go k0 ary0 0 (A.length ary0)
  where
    go !k !ary !i !n
        | i >= n    = Nothing
        | otherwise = case A.index ary i of
            (L kx _)
                | k == kx   -> Just i
                | otherwise -> go k ary (i+1) n
{-# INLINABLE indexOf #-}

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
{-# INLINABLE updateWith# #-}

updateOrSnocWith :: Eq k => (v -> v -> (# v #)) -> k -> v -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
updateOrSnocWith f = updateOrSnocWithKey (const f)
{-# INLINABLE updateOrSnocWith #-}

updateOrSnocWithKey :: Eq k => (k -> v -> v -> (# v #)) -> k -> v -> A.Array (Leaf k v)
                 -> A.Array (Leaf k v)
updateOrSnocWithKey f k0 v0 ary0 = go k0 v0 ary0 0 (A.length ary0)
  where
    go !k v !ary !i !n
        -- Not found, append to the end.
        | i >= n = A.snoc ary $ L k v
        | L kx y <- A.index ary i
        , k == kx
        , (# v2 #) <- f k v y
            = A.update ary i (L k v2)
        | otherwise
            = go k v ary (i+1) n
{-# INLINABLE updateOrSnocWithKey #-}

updateOrConcatWithKey :: Eq k => (k -> v -> v -> (# v #)) -> A.Array (Leaf k v) -> A.Array (Leaf k v) -> A.Array (Leaf k v)
updateOrConcatWithKey f ary1 ary2 = A.run $ do
    -- TODO: instead of mapping and then folding, should we traverse?
    -- We'll have to be careful to avoid allocating pairs or similar.

    -- first: look up the position of each element of ary2 in ary1
    let indices = A.map' (\(L k _) -> indexOf k ary1) ary2
    -- that tells us how large the overlap is:
    -- count number of Nothing constructors
    let nOnly2 = A.foldl' (\n -> maybe (n+1) (const n)) 0 indices
    let n1 = A.length ary1
    let n2 = A.length ary2
    -- copy over all elements from ary1
    mary <- A.new_ (n1 + nOnly2)
    A.copy ary1 0 mary 0 n1
    -- append or update all elements from ary2
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
{-# INLINABLE updateOrConcatWithKey #-}

-- | \(O(n*m)\) Check if the first array is a subset of the second array.
subsetArray :: Eq k => (v1 -> v2 -> Bool) -> A.Array (Leaf k v1) -> A.Array (Leaf k v2) -> Bool
subsetArray cmpV ary1 ary2 = A.length ary1 <= A.length ary2 && A.all inAry2 ary1
  where
    inAry2 (L k1 v1) = lookupInArrayCont (\_ -> False) (\v2 _ -> cmpV v1 v2) k1 ary2
    {-# INLINE inAry2 #-}

------------------------------------------------------------------------
-- Manually unrolled loops

-- | \(O(n)\) Update the element at the given position in this array.
update32 :: A.Array e -> Int -> e -> A.Array e
update32 ary idx b = runST (update32M ary idx b)
{-# INLINE update32 #-}

-- | \(O(n)\) Update the element at the given position in this array.
update32M :: A.Array e -> Int -> e -> ST s (A.Array e)
update32M ary idx b = do
    mary <- clone ary
    A.write mary idx b
    A.unsafeFreeze mary
{-# INLINE update32M #-}

-- | \(O(n)\) Update the element at the given position in this array, by applying a function to it.
update32With' :: A.Array e -> Int -> (e -> e) -> A.Array e
update32With' ary idx f
  | (# x #) <- A.index# ary idx
  = update32 ary idx $! f x
{-# INLINE update32With' #-}

-- | Unsafely clone an array of (2^bitsPerSubkey) elements.  The length of the input
-- array is not checked.
clone :: A.Array e -> ST s (A.MArray s e)
clone ary =
    A.thaw ary 0 (2^bitsPerSubkey)

------------------------------------------------------------------------
-- Bit twiddling

-- TODO: Name this 'bitsPerLevel'?! What is a "subkey"?
-- https://github.com/haskell-unordered-containers/unordered-containers/issues/425

-- | Number of bits that are inspected at each level of the hash tree.
--
-- This constant is named /t/ in the original /Ideal Hash Trees/ paper.
bitsPerSubkey :: Int
bitsPerSubkey = 5

-- | The size of a 'Full' node, i.e. @2 ^ 'bitsPerSubkey'@.
maxChildren :: Int
maxChildren = 1 `unsafeShiftL` bitsPerSubkey

-- | Bit mask with the lowest 'bitsPerSubkey' bits set, i.e. @0b11111@.
subkeyMask :: Word
subkeyMask = 1 `unsafeShiftL` bitsPerSubkey - 1

-- | Given a 'Hash' and a 'Shift' that indicates the level in the tree, compute
-- the index into a 'Full' node or into the bitmap of a `BitmapIndexed` node.
--
-- >>> index 0b0010_0010 0
-- 0b0000_0010
index :: Hash -> Shift -> Int
index w s = fromIntegral $ unsafeShiftR w s .&. subkeyMask
{-# INLINE index #-}

-- | Given a 'Hash' and a 'Shift' that indicates the level in the tree, compute
-- the bitmap that contains only the 'index' of the hash at this level.
--
-- The result can be used for constructing one-element 'BitmapIndexed' nodes or
-- to check whether a 'BitmapIndexed' node may possibly contain the given 'Hash'.
--
-- >>> mask 0b0010_0010 0
-- 0b0100
mask :: Hash -> Shift -> Bitmap
mask w s = 1 `unsafeShiftL` index w s
{-# INLINE mask #-}

-- | This array index is computed by counting the number of 1-bits below the
-- 'index' represented by the mask.
--
-- >>> sparseIndex 0b0110_0110 0b0010_0000
-- 2
sparseIndex
    :: Bitmap
    -- ^ Bitmap of a 'BitmapIndexed' node
    -> Bitmap
    -- ^ One-bit 'mask' corresponding to the 'index' of a hash
    -> Int
    -- ^ Index into the array of the 'BitmapIndexed' node
sparseIndex b m = popCount (b .&. (m - 1))
{-# INLINE sparseIndex #-}

-- | A bitmap with the 'maxChildren' least significant bits set, i.e.
-- @0xFF_FF_FF_FF@.
fullBitmap :: Bitmap
-- This needs to use 'shiftL' instead of 'unsafeShiftL', to avoid UB.
-- See issue #412.
fullBitmap = complement (complement 0 `shiftL` maxChildren)
{-# INLINE fullBitmap #-}

-- | Increment a 'Shift' for use at the next deeper level.
nextShift :: Shift -> Shift
nextShift s = s + bitsPerSubkey
{-# INLINE nextShift #-}

------------------------------------------------------------------------
-- Pointer equality

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects.)
ptrEq :: a -> a -> Bool
ptrEq x y = Exts.isTrue# (Exts.reallyUnsafePtrEquality# x y ==# 1#)
{-# INLINE ptrEq #-}

------------------------------------------------------------------------
-- IsList instance
instance (Eq k, Hashable k) => Exts.IsList (HashMap k v) where
    type Item (HashMap k v) = (k, v)
    fromList = fromList
    toList   = toList

    {-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
#if __GLASGOW_HASKELL__ >= 710 && __GLASGOW_HASKELL__ < 802
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Reader
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Declaration of the 'ReaderT' monad transformer, which adds a static
-- environment to a given monad.
--
-- If the computation is to modify the stored information, use
-- "Control.Monad.Trans.State" instead.
-----------------------------------------------------------------------------

module Control.Monad.Trans.Reader (
    -- * The Reader monad
    Reader,
    reader,
    runReader,
    mapReader,
    withReader,
    -- * The ReaderT monad transformer
    ReaderT(..),
    mapReaderT,
    withReaderT,
    -- * Reader operations
    ask,
    local,
    asks,
    -- * Lifting other operations
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

-- | The parameterizable reader monad.
--
-- Computations are functions of a shared environment.
--
-- The 'return' function ignores the environment, while @m '>>=' k@
-- passes the inherited environment to both subcomputations:
--
-- <<images/bind-ReaderT.svg>>
--
type Reader r = ReaderT r Identity

-- | Constructor for computations in the reader monad (equivalent to 'asks').
reader :: (Monad m) => (r -> a) -> ReaderT r m a
reader f = ReaderT (return . f)
{-# INLINE reader #-}

-- | Runs a @Reader@ and extracts the final value from it.
-- (The inverse of 'reader'.)
runReader
    :: Reader r a       -- ^ A @Reader@ to run.
    -> r                -- ^ An initial environment.
    -> a
runReader m = runIdentity . runReaderT m
{-# INLINE runReader #-}

-- | Transform the value returned by a @Reader@.
--
-- * @'runReader' ('mapReader' f m) = f . 'runReader' m@
mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity)
{-# INLINE mapReader #-}

-- | Execute a computation in a modified environment
-- (a specialization of 'withReaderT').
--
-- * @'runReader' ('withReader' f m) = 'runReader' m . f@
withReader
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> Reader r a       -- ^ Computation to run in the modified environment.
    -> Reader r' a
withReader = withReaderT
{-# INLINE withReader #-}

-- | The reader monad transformer,
-- which adds a read-only environment to the given monad.
--
-- The 'return' function ignores the environment, while @m '>>=' k@
-- passes the inherited environment to both subcomputations:
--
-- <<images/bind-ReaderT.svg>>
--
newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }
#if __GLASGOW_HASKELL__ >= 710
    deriving (Generic, Generic1)
#elif __GLASGOW_HASKELL__ >= 704
    deriving (Generic)
#endif

-- | Transform the computation inside a @ReaderT@.
--
-- * @'runReaderT' ('mapReaderT' f m) = f . 'runReaderT' m@
mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m
{-# INLINE mapReaderT #-}

-- | Execute a computation in a modified environment
-- (a more general version of 'local').
--
-- * @'runReaderT' ('withReaderT' f m) = 'runReaderT' m . f@
withReaderT
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f
{-# INLINE withReaderT #-}

instance (Functor m) => Functor (ReaderT r m) where
    fmap f  = mapReaderT (fmap f)
    {-# INLINE fmap #-}
#if MIN_VERSION_base(4,2,0)
    x <$ v = mapReaderT (x <$) v
    {-# INLINE (<$) #-}
#endif

instance (Applicative m) => Applicative (ReaderT r m) where
    pure    = liftReaderT . pure
    {-# INLINE pure #-}
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r
    {-# INLINE (<*>) #-}
#if MIN_VERSION_base(4,2,0)
    u *> v = ReaderT $ \ r -> runReaderT u r *> runReaderT v r
    {-# INLINE (*>) #-}
    u <* v = ReaderT $ \ r -> runReaderT u r <* runReaderT v r
    {-# INLINE (<*) #-}
#endif
#if MIN_VERSION_base(4,10,0)
    liftA2 f x y = ReaderT $ \ r -> liftA2 f (runReaderT x r) (runReaderT y r)
    {-# INLINE liftA2 #-}
#endif

instance (Alternative m) => Alternative (ReaderT r m) where
    empty   = liftReaderT empty
    {-# INLINE empty #-}
    m <|> n = ReaderT $ \ r -> runReaderT m r <|> runReaderT n r
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (ReaderT r m) where
#if !(MIN_VERSION_base(4,8,0))
    return   = lift . return
    {-# INLINE return #-}
#endif
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    {-# INLINE (>>=) #-}
#if MIN_VERSION_base(4,8,0)
    (>>) = (*>)
#else
    m >> k = ReaderT $ \ r -> runReaderT m r >> runReaderT k r
#endif
    {-# INLINE (>>) #-}
#if !(MIN_VERSION_base(4,13,0))
    fail msg = lift (fail msg)
    {-# INLINE fail #-}
#endif

#if MIN_VERSION_base(4,9,0)
instance (Fail.MonadFail m) => Fail.MonadFail (ReaderT r m) where
    fail msg = lift (Fail.fail msg)
    {-# INLINE fail #-}
#endif

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    mzero       = lift mzero
    {-# INLINE mzero #-}
    m `mplus` n = ReaderT $ \ r -> runReaderT m r `mplus` runReaderT n r
    {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (ReaderT r m) where
    mfix f = ReaderT $ \ r -> mfix $ \ a -> runReaderT (f a) r
    {-# INLINE mfix #-}

instance MonadTrans (ReaderT r) where
    lift   = liftReaderT
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (ReaderT r m) where
    mzipWith f (ReaderT m) (ReaderT n) = ReaderT $ \ a ->
        mzipWith f (m a) (n a)
    {-# INLINE mzipWith #-}
#endif

#if MIN_VERSION_base(4,12,0)
instance Contravariant m => Contravariant (ReaderT r m) where
    contramap f = ReaderT . fmap (contramap f) . runReaderT
    {-# INLINE contramap #-}
#endif

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)
{-# INLINE liftReaderT #-}

-- | Fetch the value of the environment.
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return
{-# INLINE ask #-}

-- | Execute a computation in a modified environment
-- (a specialization of 'withReaderT').
--
-- * @'runReaderT' ('local' f m) = 'runReaderT' m . f@
local
    :: (r -> r)         -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r m a
local = withReaderT
{-# INLINE local #-}

-- | Retrieve a function of the current environment.
--
-- * @'asks' f = 'liftM' f 'ask'@
asks :: (Monad m)
    => (r -> a)         -- ^ The selector function to apply to the environment.
    -> ReaderT r m a
asks f = ReaderT (return . f)
{-# INLINE asks #-}

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m a b -> CallCC (ReaderT r m) a b
liftCallCC callCC f = ReaderT $ \ r ->
    callCC $ \ c ->
    runReaderT (f (ReaderT . const . c)) r
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m a -> Catch e (ReaderT r m) a
liftCatch f m h =
    ReaderT $ \ r -> f (runReaderT m r) (\ e -> runReaderT (h e) r)
{-# INLINE liftCatch #-}

{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Safe #-}
{-# LANGUAGE DeriveGeneric #-}
#endif
#if __GLASGOW_HASKELL__ >= 710 && __GLASGOW_HASKELL__ < 802
{-# LANGUAGE AutoDeriveTypeable #-}
#endif
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Maybe
-- Copyright   :  (c) 2007 Yitzak Gale, Eric Kidd
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  R.Paterson@city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- The 'MaybeT' monad transformer extends a monad with the ability to exit
-- the computation without returning a value.
--
-- A sequence of actions produces a value only if all the actions in
-- the sequence do.  If one exits, the rest of the sequence is skipped
-- and the composite action exits.
--
-- For a variant allowing a range of exception values, see
-- "Control.Monad.Trans.Except".
-----------------------------------------------------------------------------

module Control.Monad.Trans.Maybe (
    -- * The MaybeT monad transformer
    MaybeT(..),
    mapMaybeT,
    -- * Monad transformations
    hoistMaybe,
    maybeToExceptT,
    exceptToMaybeT,
    -- * Lifting other operations
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

-- | The parameterizable maybe monad, obtained by composing an arbitrary
-- monad with the 'Maybe' monad.
--
-- Computations are actions that may produce a value or exit.
--
-- The 'return' function yields a computation that produces that
-- value, while @>>=@ sequences two subcomputations, exiting if either
-- computation does.
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
#if __GLASGOW_HASKELL__ >= 710
    deriving (Generic, Generic1)
#elif __GLASGOW_HASKELL__ >= 704
    deriving (Generic)
#endif

instance (Eq1 m) => Eq1 (MaybeT m) where
    liftEq eq (MaybeT x) (MaybeT y) = liftEq (liftEq eq) x y
    {-# INLINE liftEq #-}

instance (Ord1 m) => Ord1 (MaybeT m) where
    liftCompare comp (MaybeT x) (MaybeT y) = liftCompare (liftCompare comp) x y
    {-# INLINE liftCompare #-}

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

-- | Transform the computation inside a @MaybeT@.
--
-- * @'runMaybeT' ('mapMaybeT' f m) = f ('runMaybeT' m)@
mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT
{-# INLINE mapMaybeT #-}

-- | Convert a 'Maybe' computation to 'MaybeT'.
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

-- | Convert a 'MaybeT' computation to 'ExceptT', with a default
-- exception value.
maybeToExceptT :: (Functor m) => e -> MaybeT m a -> ExceptT e m a
maybeToExceptT e (MaybeT m) = ExceptT $ fmap (maybe (Left e) Right) m
{-# INLINE maybeToExceptT #-}

-- | Convert a 'ExceptT' computation to 'MaybeT', discarding the
-- value of any exception.
exceptToMaybeT :: (Functor m) => ExceptT e m a -> MaybeT m a
exceptToMaybeT (ExceptT m) = MaybeT $ fmap (either (const Nothing) Just) m
{-# INLINE exceptToMaybeT #-}

instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))
    {-# INLINE fmap #-}

instance (Foldable f) => Foldable (MaybeT f) where
    foldMap f (MaybeT a) = foldMap (foldMap f) a
    {-# INLINE foldMap #-}

instance (Traversable f) => Traversable (MaybeT f) where
    traverse f (MaybeT a) = MaybeT <$> traverse (traverse f) a
    {-# INLINE traverse #-}

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = MaybeT . return . Just
    {-# INLINE pure #-}
    mf <*> mx = MaybeT $ do
        mb_f <- runMaybeT mf
        case mb_f of
            Nothing -> return Nothing
            Just f  -> do
                mb_x <- runMaybeT mx
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

instance (Functor m, Monad m) => Alternative (MaybeT m) where
    empty = MaybeT (return Nothing)
    {-# INLINE empty #-}
    x <|> y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v
    {-# INLINE (<|>) #-}

instance (Monad m) => Monad (MaybeT m) where
#if !(MIN_VERSION_base(4,8,0))
    return = MaybeT . return . Just
    {-# INLINE return #-}
#endif
    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)
    {-# INLINE (>>=) #-}
#if !(MIN_VERSION_base(4,13,0))
    fail _ = MaybeT (return Nothing)
    {-# INLINE fail #-}
#endif

#if MIN_VERSION_base(4,9,0)
instance (Monad m) => Fail.MonadFail (MaybeT m) where
    fail _ = MaybeT (return Nothing)
    {-# INLINE fail #-}
#endif

instance (Monad m) => MonadPlus (MaybeT m) where
    mzero = MaybeT (return Nothing)
    {-# INLINE mzero #-}
    mplus x y = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> runMaybeT y
            Just _  -> return v
    {-# INLINE mplus #-}

instance (MonadFix m) => MonadFix (MaybeT m) where
    mfix f = MaybeT (mfix (runMaybeT . f . fromMaybe bomb))
      where bomb = error "mfix (MaybeT): inner computation returned Nothing"
    {-# INLINE mfix #-}

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just
    {-# INLINE lift #-}

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

#if MIN_VERSION_base(4,4,0)
instance (MonadZip m) => MonadZip (MaybeT m) where
    mzipWith f (MaybeT a) (MaybeT b) = MaybeT $ mzipWith (liftA2 f) a b
    {-# INLINE mzipWith #-}
#endif

#if MIN_VERSION_base(4,12,0)
instance Contravariant m => Contravariant (MaybeT m) where
    contramap f = MaybeT . contramap (fmap f) . runMaybeT
    {-# INLINE contramap #-}
#endif

-- | Lift a @callCC@ operation to the new monad.
liftCallCC :: CallCC m (Maybe a) (Maybe b) -> CallCC (MaybeT m) a b
liftCallCC callCC f =
    MaybeT $ callCC $ \ c -> runMaybeT (f (MaybeT . c . Just))
{-# INLINE liftCallCC #-}

-- | Lift a @catchE@ operation to the new monad.
liftCatch :: Catch e m (Maybe a) -> Catch e (MaybeT m) a
liftCatch f m h = MaybeT $ f (runMaybeT m) (runMaybeT . h)
{-# INLINE liftCatch #-}

-- | Lift a @listen@ operation to the new monad.
liftListen :: (Monad m) => Listen w m (Maybe a) -> Listen w (MaybeT m) a
liftListen listen = mapMaybeT $ \ m -> do
    (a, w) <- listen m
    return $! fmap (\ r -> (r, w)) a
{-# INLINE liftListen #-}

-- | Lift a @pass@ operation to the new monad.
liftPass :: (Monad m) => Pass w m (Maybe a) -> Pass w (MaybeT m) a
liftPass pass = mapMaybeT $ \ m -> pass $ do
    a <- m
    return $! case a of
        Nothing     -> (Nothing, id)
        Just (v, f) -> (Just v, f)
{-# INLINE liftPass #-}

{-# LANGUAGE Safe #-}

{-# OPTIONS -fno-warn-orphans #-}

module Data.Time.Format.Parse (
    -- * UNIX-style parsing
    parseTimeM,
    parseTimeMultipleM,
    parseTimeOrError,
    readSTime,
    readPTime,
    ParseTime (),

    -- * Locale
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

-- | Parses a time value given a format string.
-- Missing information will be derived from 1970-01-01 00:00 UTC (which was a Thursday).
-- Supports the same %-codes as 'formatTime', including @%-@, @%_@ and @%0@ modifiers, however padding widths are not supported.
-- Case is not significant in the input string.
-- Some variations in the input are accepted:
--
-- [@%z@ @%Ez@] accepts any of @±HHMM@ or @±HH:MM@.
--
-- [@%Z@ @%EZ@] accepts any string of letters, or any of the formats accepted by @%z@.
--
-- [@%0Y@] accepts exactly four digits.
--
-- [@%0G@] accepts exactly four digits.
--
-- [@%0C@] accepts exactly two digits.
--
-- [@%0f@] accepts exactly two digits.
--
-- For example, to parse a date in YYYY-MM-DD format, while allowing the month
-- and date to have optional leading zeros (notice the @-@ modifier used for @%m@
-- and @%d@):
--
-- > Prelude Data.Time> parseTimeM True defaultTimeLocale "%Y-%-m-%-d" "2010-3-04" :: Maybe Day
-- > Just 2010-03-04
parseTimeM ::
    (MonadFail m, ParseTime t) =>
    -- | Accept leading and trailing whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Format string.
    String ->
    -- | Input string.
    String ->
    -- | Return the time value, or fail if the input could not be parsed using the given format.
    m t
parseTimeM acceptWS l fmt s = parseTimeMultipleM acceptWS l [(fmt, s)]

-- | Parses a time value given a list of pairs of format and input.
-- Resulting value is constructed from all provided specifiers.
parseTimeMultipleM' ::
    (MonadFail m, ParseTime t) =>
    Proxy t ->
    -- | Accept leading and trailing whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Pairs of (format string, input string).
    [(String, String)] ->
    -- | Return the time value, or fail if the input could not be parsed using the given format.
    m t
parseTimeMultipleM' pt acceptWS l fmts = do
    specss <- for fmts $ \(fmt, s) -> parseTimeSpecifiersM pt acceptWS l fmt s
    case buildTime l $ mconcat specss of
        Just t -> return t
        Nothing -> fail "parseTimeM: cannot construct"

-- | Parses a time value given a list of pairs of format and input.
-- Resulting value is constructed from all provided specifiers.
parseTimeMultipleM ::
    (MonadFail m, ParseTime t) =>
    -- | Accept leading and trailing whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Pairs of (format string, input string).
    [(String, String)] ->
    -- | Return the time value, or fail if the input could not be parsed using the given format.
    m t
parseTimeMultipleM = parseTimeMultipleM' Proxy

-- | Parse a time value given a format string. Fails if the input could
-- not be parsed using the given format. See 'parseTimeM' for details.
parseTimeOrError ::
    ParseTime t =>
    -- | Accept leading and trailing whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Format string.
    String ->
    -- | Input string.
    String ->
    -- | The time value.
    t
parseTimeOrError acceptWS l fmt s =
    case parseTimeM acceptWS l fmt s of
        [t] -> t
        [] -> error $ "parseTimeOrError: no parse of " ++ show s
        _ -> error $ "parseTimeOrError: multiple parses of " ++ show s

parseTimeSpecifiersM ::
    (MonadFail m, ParseTime t) =>
    Proxy t ->
    -- | Accept leading and trailing whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Format string
    String ->
    -- | Input string.
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
    -- | Accept leading and trailing whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Format string
    String ->
    -- | Input string.
    String ->
    [[(Char, String)]]
parseTimeSpecifiers pt False l fmt s = [t | (t, "") <- readP_to_S (readPSpecifiers pt False l fmt) s]
parseTimeSpecifiers pt True l fmt s = [t | (t, r) <- readP_to_S (readPSpecifiers pt True l fmt) s, all isSpace r]

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readSTime ::
    ParseTime t =>
    -- | Accept leading whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Format string
    String ->
    ReadS t
readSTime acceptWS l f = readP_to_S $ readPTime acceptWS l f

readPSpecifiers ::
    ParseTime t =>
    Proxy t ->
    -- | Accept leading whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Format string
    String ->
    ReadP [(Char, String)]
readPSpecifiers pt False l f = parseSpecifiers pt l f
readPSpecifiers pt True l f = (skipSpaces >> parseSpecifiers pt l f) <++ parseSpecifiers pt l f

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readPTime' ::
    ParseTime t =>
    Proxy t ->
    -- | Accept leading whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Format string
    String ->
    ReadP t
readPTime' pt ws l f = do
    pairs <- readPSpecifiers pt ws l f
    case buildTime l pairs of
        Just t -> return t
        Nothing -> pfail

-- | Parse a time value given a format string.  See 'parseTimeM' for details.
readPTime ::
    ParseTime t =>
    -- | Accept leading whitespace?
    Bool ->
    -- | Time locale.
    TimeLocale ->
    -- | Format string
    String ->
    ReadP t
readPTime = readPTime' Proxy

-- * Read instances for time package types

instance Read Day where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d"

instance Read TimeOfDay where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%H:%M:%S%Q"

instance Read LocalTime where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

-- | This only works for @±HHMM@ format,
-- single-letter military time-zones,
-- and these time-zones: \"UTC\", \"UT\", \"GMT\", \"EST\", \"EDT\", \"CST\", \"CDT\", \"MST\", \"MDT\", \"PST\", \"PDT\",
-- per RFC 822 section 5.
instance Read TimeZone where
    readsPrec _ = readParen False $ readSTime True defaultTimeLocale "%Z"

-- | This only works for a 'zonedTimeZone' in @±HHMM@ format,
-- single-letter military time-zones,
-- and these time-zones: \"UTC\", \"UT\", \"GMT\", \"EST\", \"EDT\", \"CST\", \"CDT\", \"MST\", \"MDT\", \"PST\", \"PDT\",
-- per RFC 822 section 5.
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

    {-# LANGUAGE Safe #-}

module Data.Time.LocalTime.Internal.TimeOfDay (
    -- * Time of day
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

-- | Time of day as represented in hour, minute and second (with picoseconds), typically used to express local time of day.
--
-- @TimeOfDay 24 0 0@ is considered invalid for the purposes of 'makeTimeOfDayValid', as well as reading and parsing,
-- but valid for ISO 8601 parsing in "Data.Time.Format.ISO8601".
data TimeOfDay = TimeOfDay
    { todHour :: Int
    -- ^ range 0 - 23
    , todMin :: Int
    -- ^ range 0 - 59
    , todSec :: Pico
    -- ^ Note that 0 <= 'todSec' < 61, accomodating leap seconds.
    -- Any local minute may have a leap second, since leap seconds happen in all zones simultaneously
    }
    deriving (Eq, Ord, Data, Typeable, Generic)

instance NFData TimeOfDay where
    rnf (TimeOfDay h m s) = rnf h `seq` rnf m `seq` rnf s `seq` ()

-- | Hour zero
midnight :: TimeOfDay
midnight = TimeOfDay 0 0 0

-- | Hour twelve
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

-- | Convert a period of time into a count of days and a time of day since midnight.
-- The time of day will never have a leap second.
timeToDaysAndTimeOfDay :: NominalDiffTime -> (Integer, TimeOfDay)
timeToDaysAndTimeOfDay dt =
    let
        s = realToFrac dt
        (m, ms) = divMod' s 60
        (h, hm) = divMod' m 60
        (d, dh) = divMod' h 24
    in
        (d, TimeOfDay dh hm ms)

-- | Convert a count of days and a time of day since midnight into a period of time.
daysAndTimeOfDayToTime :: Integer -> TimeOfDay -> NominalDiffTime
daysAndTimeOfDayToTime d (TimeOfDay dh hm ms) =
    (+) (realToFrac ms) $ (*) 60 $ (+) (realToFrac hm) $ (*) 60 $ (+) (realToFrac dh) $ (*) 24 $ realToFrac d

-- | Convert a time of day in UTC to a time of day in some timezone, together with a day adjustment.
utcToLocalTimeOfDay :: TimeZone -> TimeOfDay -> (Integer, TimeOfDay)
utcToLocalTimeOfDay zone (TimeOfDay h m s) = (fromIntegral (div h' 24), TimeOfDay (mod h' 24) (mod m' 60) s)
  where
    m' = m + timeZoneMinutes zone
    h' = h + (div m' 60)

-- | Convert a time of day in some timezone to a time of day in UTC, together with a day adjustment.
localToUTCTimeOfDay :: TimeZone -> TimeOfDay -> (Integer, TimeOfDay)
localToUTCTimeOfDay zone = utcToLocalTimeOfDay (minutesToTimeZone (negate (timeZoneMinutes zone)))

posixDayLength :: DiffTime
posixDayLength = fromInteger 86400

-- | Get the time of day given a time since midnight.
-- Time more than 24h will be converted to leap-seconds.
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

-- | Same as 'timeToTimeOfDay'.
pastMidnight :: DiffTime -> TimeOfDay
pastMidnight = timeToTimeOfDay

-- | Get the time since midnight for a given time of day.
timeOfDayToTime :: TimeOfDay -> DiffTime
timeOfDayToTime (TimeOfDay h m s) = ((fromIntegral h) * 60 + (fromIntegral m)) * 60 + (realToFrac s)

-- | Same as 'timeOfDayToTime'.
sinceMidnight :: TimeOfDay -> DiffTime
sinceMidnight = timeOfDayToTime

-- | Get the time of day given the fraction of a day since midnight.
dayFractionToTimeOfDay :: Rational -> TimeOfDay
dayFractionToTimeOfDay df = timeToTimeOfDay (realToFrac (df * 86400))

timeOfDayToDayFraction :: TimeOfDay -> Rational
timeOfDayToDayFraction tod = realToFrac (timeOfDayToTime tod) / realToFrac posixDayLength

instance Eq Text where
    Text arrA offA lenA == Text arrB offB lenB
        | lenA == lenB = A.equal arrA offA arrB offB lenA
        | otherwise    = False
    {-# INLINE (==) #-}

instance Ord Text where
    compare = compareText

instance Read Text where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

-- | @since 1.2.2.0
instance Semigroup Text where
    (<>) = append

instance Monoid Text where
    mempty  = empty
    mappend = (<>)
    mconcat = concat

-- | Performs replacement on invalid scalar values:
--
-- >>> :set -XOverloadedStrings
-- >>> "\55555" :: Text
-- "\65533"
instance IsString Text where
    fromString = pack

-- | Performs replacement on invalid scalar values:
--
-- >>> :set -XOverloadedLists
-- >>> ['\55555'] :: Text
-- "\65533"
--
-- @since 1.2.0.0
instance Exts.IsList Text where
    type Item Text = Char
    fromList       = pack
    toList         = unpack

instance NFData Text where rnf !_ = ()

-- | @since 1.2.1.0
instance Binary Text where
    put t = do
      -- This needs to be in sync with the Binary instance for ByteString
      -- in the binary package.
      put (lengthWord8 t)
      putBuilder (encodeUtf8Builder t)
    get   = do
      bs <- get
      case decodeUtf8' bs of
        P.Left exn -> P.fail (P.show exn)
        P.Right a -> P.return a

-- | This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.
--
-- This instance was created by copying the updated behavior of
-- @"Data.Set".@'Data.Set.Set' and @"Data.Map".@'Data.Map.Map'. If you
-- feel a mistake has been made, please feel free to submit
-- improvements.
--
-- The original discussion is archived here:
-- <https://mail.haskell.org/pipermail/haskell-cafe/2010-January/072379.html could we get a Data instance for Data.Text.Text? >
--
-- The followup discussion that changed the behavior of 'Data.Set.Set'
-- and 'Data.Map.Map' is archived here:
-- <https://mail.haskell.org/pipermail/libraries/2012-August/018366.html Proposal: Allow gunfold for Data.Map, ... >

instance Data Text where
  gfoldl f z txt = z pack `f` (unpack txt)
  toConstr _ = packConstr
  gunfold k z c = case constrIndex c of
    1 -> k (z pack)
    _ -> P.error "gunfold"
  dataTypeOf _ = textDataType

-- | @since 1.2.4.0
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
{-# NOINLINE unpackCStringLen# #-} -- set as NOINLINE to avoid generated code bloat
#endif

-- | @since 1.2.2.0
instance PrintfArg Text where
  formatArg txt = formatString $ unpack txt

packConstr :: Constr
packConstr = mkConstr textDataType "pack" [] Prefix

textDataType :: DataType
textDataType = mkDataType "Data.Text.Text" [packConstr]

-- | /O(n)/ Compare two 'Text' values lexicographically.
compareText :: Text -> Text -> Ordering
compareText (Text arrA offA lenA) (Text arrB offB lenB) =
    A.compare arrA offA arrB offB (min lenA lenB) <> compare lenA lenB
-- This is not a mistake: on contrary to UTF-16 (https://github.com/haskell/text/pull/208),
-- lexicographic ordering of UTF-8 encoded strings matches lexicographic ordering
-- of underlying bytearrays, no decoding is needed.

-- -----------------------------------------------------------------------------
-- * Basic functions

-- | /O(n)/ Adds a character to the front of a 'Text'.  This function
-- is more costly than its 'List' counterpart because it requires
-- copying a new array.  Performs replacement on
-- invalid scalar values.
cons :: Char -> Text -> Text
cons c = unstream . S.cons (safe c) . stream
{-# INLINE [1] cons #-}

infixr 5 `cons`

-- | /O(n)/ Adds a character to the end of a 'Text'.  This copies the
-- entire array in the process.
-- Performs replacement on invalid scalar values.
snoc :: Text -> Char -> Text
snoc t c = unstream (S.snoc (stream t) (safe c))
{-# INLINE snoc #-}

-- | /O(1)/ Returns the first character of a 'Text', which must be
-- non-empty. This is a partial function, consider using 'uncons' instead.
head :: HasCallStack => Text -> Char
head t = S.head (stream t)
{-# INLINE head #-}

-- | /O(1)/ Returns the first character and rest of a 'Text', or
-- 'Nothing' if empty.
uncons :: Text -> Maybe (Char, Text)
uncons t@(Text arr off len)
    | len <= 0  = Nothing
    | otherwise = Just $ let !(Iter c d) = iter t 0
                         in (c, text arr (off+d) (len-d))
{-# INLINE [1] uncons #-}

-- | /O(1)/ Returns the last character of a 'Text', which must be
-- non-empty. This is a partial function, consider using 'unsnoc' instead.
last :: HasCallStack => Text -> Char
last t@(Text _ _ len)
    | null t = emptyError "last"
    | otherwise = let Iter c _ = reverseIter t (len - 1) in c
{-# INLINE [1] last #-}

-- | /O(1)/ Returns all characters after the head of a 'Text', which
-- must be non-empty. This is a partial function, consider using 'uncons' instead.
tail :: HasCallStack => Text -> Text
tail t@(Text arr off len)
    | null t = emptyError "tail"
    | otherwise = text arr (off+d) (len-d)
    where d = iter_ t 0
{-# INLINE [1] tail #-}

-- | /O(1)/ Returns all but the last character of a 'Text', which must
-- be non-empty. This is a partial function, consider using 'unsnoc' instead.
init :: HasCallStack => Text -> Text
init t@(Text arr off len)
    | null t = emptyError "init"
    | otherwise = text arr off (len + reverseIter_ t (len - 1))
{-# INLINE [1] init #-}

-- | /O(1)/ Returns all but the last character and the last character of a
-- 'Text', or 'Nothing' if empty.
--
-- @since 1.2.3.0
unsnoc :: Text -> Maybe (Text, Char)
unsnoc t@(Text arr off len)
    | null t = Nothing
    | otherwise = Just (text arr off (len + d), c)
        where
            Iter c d = reverseIter t (len - 1)
{-# INLINE [1] unsnoc #-}

-- | /O(1)/ Tests whether a 'Text' is empty or not.
null :: Text -> Bool
null (Text _arr _off len) =
#if defined(ASSERTS)
    assert (len >= 0) $
#endif
    len <= 0
{-# INLINE [1] null #-}

{-# RULES
 "TEXT null/empty -> True" null empty = True
#-}

-- | Bidirectional pattern synonym for 'empty' and 'null' (both /O(1)/),
-- to be used together with '(:<)' or '(:>)'.
--
-- @since 2.1.2
pattern Empty :: Text
pattern Empty <- (null -> True) where
  Empty = empty

-- | Bidirectional pattern synonym for 'cons' (/O(n)/) and 'uncons' (/O(1)/),
-- to be used together with 'Empty'.
--
-- @since 2.1.2
pattern (:<) :: Char -> Text -> Text
pattern x :< xs <- (uncons -> Just (x, xs)) where
  (:<) = cons
infixr 5 :<
{-# COMPLETE Empty, (:<) #-}

-- | Bidirectional pattern synonym for 'snoc' (/O(n)/) and 'unsnoc' (/O(1)/)
-- to be used together with 'Empty'.
--
-- @since 2.1.2
pattern (:>) :: Text -> Char -> Text
pattern xs :> x <- (unsnoc -> Just (xs, x)) where
  (:>) = snoc
infixl 5 :>
{-# COMPLETE Empty, (:>) #-}

-- | /O(1)/ Tests whether a 'Text' contains exactly one character.
isSingleton :: Text -> Bool
isSingleton = S.isSingleton . stream
{-# INLINE isSingleton #-}

-- | /O(n)/ Returns the number of characters in a 'Text'.
length ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Int
length = P.negate . measureOff P.maxBound
{-# INLINE [1] length #-}
-- length needs to be phased after the compareN/length rules otherwise
-- it may inline before the rules have an opportunity to fire.

{-# RULES
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
  #-}

-- | /O(min(n,c))/ Compare the count of characters in a 'Text' to a number.
--
-- @
-- 'compareLength' t c = 'P.compare' ('length' t) c
-- @
--
-- This function gives the same answer as comparing against the result
-- of 'length', but can short circuit if the count of characters is
-- greater than the number, and hence be more efficient.
compareLength :: Text -> Int -> Ordering
compareLength t c = S.compareLengthI (stream t) c
{-# INLINE [1] compareLength #-}

{-# RULES
"TEXT compareN/length -> compareLength" [~1] forall t n.
    compare (length t) n = compareLength t n
  #-}

{-# RULES
"TEXT ==N/length -> compareLength/==EQ" [~1] forall t n.
    eqInt (length t) n = compareLength t n == EQ
  #-}

{-# RULES
"TEXT /=N/length -> compareLength//=EQ" [~1] forall t n.
    neInt (length t) n = compareLength t n /= EQ
  #-}

{-# RULES
"TEXT <N/length -> compareLength/==LT" [~1] forall t n.
    ltInt (length t) n = compareLength t n == LT
  #-}

{-# RULES
"TEXT <=N/length -> compareLength//=GT" [~1] forall t n.
    leInt (length t) n = compareLength t n /= GT
  #-}

{-# RULES
"TEXT >N/length -> compareLength/==GT" [~1] forall t n.
    gtInt (length t) n = compareLength t n == GT
  #-}

{-# RULES
"TEXT >=N/length -> compareLength//=LT" [~1] forall t n.
    geInt (length t) n = compareLength t n /= LT
  #-}

-- -----------------------------------------------------------------------------
-- * Transformations
-- | /O(n)/ 'map' @f@ @t@ is the 'Text' obtained by applying @f@ to
-- each element of @t@.
--
-- Example:
--
-- >>> let message = pack "I am not angry. Not at all."
-- >>> T.map (\c -> if c == '.' then '!' else c) message
-- "I am not angry! Not at all!"
--
-- Performs replacement on invalid scalar values.
map :: (Char -> Char) -> Text -> Text
map f = \t -> if null t then empty else mapNonEmpty f t
{-# INLINE [1] map #-}

{-# RULES
"TEXT map/map -> map" forall f g t.
    map f (map g t) = map (f . safe . g) t
#-}

-- | /O(n)/ The 'intercalate' function takes a 'Text' and a list of
-- 'Text's and concatenates the list after interspersing the first
-- argument between each element of the list.
--
-- Example:
--
-- >>> T.intercalate "NI!" ["We", "seek", "the", "Holy", "Grail"]
-- "WeNI!seekNI!theNI!HolyNI!Grail"
intercalate :: Text -> [Text] -> Text
intercalate t = concat . L.intersperse t
{-# INLINE [1] intercalate #-}

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text'.
--
-- Example:
--
-- >>> T.intersperse '.' "SHIELD"
-- "S.H.I.E.L.D"
--
-- Performs replacement on invalid scalar values.
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
{-# INLINE [1] intersperse #-}

-- | /O(m+n)/ Replace every non-overlapping occurrence of @needle@ in
-- @haystack@ with @replacement@.
--
-- This function behaves as though it was defined as follows:
--
-- @
-- replace needle replacement haystack =
--   'intercalate' replacement ('splitOn' needle haystack)
-- @
--
-- As this suggests, each occurrence is replaced exactly once.  So if
-- @needle@ occurs in @replacement@, that occurrence will /not/ itself
-- be replaced recursively:
--
-- >>> replace "oo" "foo" "oo"
-- "foo"
--
-- In cases where several instances of @needle@ overlap, only the
-- first one will be replaced:
--
-- >>> replace "ofo" "bar" "ofofo"
-- "barfo"
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
replace :: HasCallStack
        => Text
        -- ^ @needle@ to search for.  If this string is empty, an
        -- error will occur.
        -> Text
        -- ^ @replacement@ to replace @needle@ with.
        -> Text
        -- ^ @haystack@ in which to search.
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

-- ----------------------------------------------------------------------------
-- ** Case conversions (folds)

-- $case
--
-- When case converting 'Text' values, do not use combinators like
-- @map toUpper@ to case convert each character of a string
-- individually, as this gives incorrect results according to the
-- rules of some writing systems.  The whole-string case conversion
-- functions from this module, such as @toUpper@, obey the correct
-- case conversion rules.  As a result, these functions may map one
-- input character to two or three output characters. For examples,
-- see the documentation of each function.
--
-- /Note/: In some languages, case conversion is a locale- and
-- context-dependent operation. The case conversion functions in this
-- module are /not/ locale sensitive. Programs that require locale
-- sensitivity should use appropriate versions of the
-- <http://hackage.haskell.org/package/text-icu-0.6.3.7/docs/Data-Text-ICU.html#g:4 case mapping functions from the text-icu package >.

-- | /O(n)/ Convert a string to folded case.
--
-- This function is mainly useful for performing caseless (also known
-- as case insensitive) string comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @toCaseFold x == toCaseFold y@
--
-- The result string may be longer than the input string, and may
-- differ from applying 'toLower' to the input string.  For instance,
-- the Armenian small ligature \"&#xfb13;\" (men now, U+FB13) is case
-- folded to the sequence \"&#x574;\" (men, U+0574) followed by
-- \"&#x576;\" (now, U+0576), while the Greek \"&#xb5;\" (micro sign,
-- U+00B5) is case folded to \"&#x3bc;\" (small letter mu, U+03BC)
-- instead of itself.
toCaseFold :: Text -> Text
toCaseFold = \t ->
    if null t then empty
    else toCaseFoldNonEmpty t
{-# INLINE toCaseFold #-}

-- | /O(n)/ Convert a string to lower case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, \"&#x130;\" (Latin capital letter I with dot above,
-- U+0130) maps to the sequence \"i\" (Latin small letter i, U+0069)
-- followed by \" &#x307;\" (combining dot above, U+0307).
toLower :: Text -> Text
toLower = \t ->
  if null t then empty
  else toLowerNonEmpty t
{-# INLINE toLower #-}

-- | /O(n)/ Convert a string to upper case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, the German \"&#xdf;\" (eszett, U+00DF) maps to the
-- two-letter sequence \"SS\".
toUpper :: Text -> Text
toUpper = \t ->
  if null t then empty
  else toUpperNonEmpty t
{-# INLINE toUpper #-}

-- | /O(n)/ Convert a string to title case, using simple case
-- conversion.
--
-- The first letter (as determined by 'Data.Char.isLetter')
-- of the input is converted to title case, as is
-- every subsequent letter that immediately follows a non-letter.
-- Every letter that immediately follows another letter is converted
-- to lower case.
--
-- This function is not idempotent.
-- Consider lower-case letter @ŉ@ (U+0149 LATIN SMALL LETTER N PRECEDED BY APOSTROPHE).
-- Then 'T.toTitle' @"ŉ"@ = @"ʼN"@: the first (and the only) letter of the input
-- is converted to title case, becoming two letters.
-- Now @ʼ@ (U+02BC MODIFIER LETTER APOSTROPHE) is a modifier letter
-- and as such is recognised as a letter by 'Data.Char.isLetter',
-- so 'T.toTitle' @"ʼN"@ = @"'n"@.
--
-- The result string may be longer than the input string. For example,
-- the Latin small ligature &#xfb02; (U+FB02) is converted to the
-- sequence Latin capital letter F (U+0046) followed by Latin small
-- letter l (U+006C).
--
-- /Note/: this function does not take language or culture specific
-- rules into account. For instance, in English, different style
-- guides disagree on whether the book name \"The Hill of the Red
-- Fox\" is correctly title cased&#x2014;but this function will
-- capitalize /every/ word.
--
-- @since 1.0.0.0
toTitle :: Text -> Text
toTitle = \t ->
  if null t then empty
  else toTitleNonEmpty t
{-# INLINE toTitle #-}

-- | /O(n)/ Left-justify a string to the given length, using the
-- specified fill character on the right.
-- Performs replacement on invalid scalar values.
--
-- Examples:
--
-- >>> justifyLeft 7 'x' "foo"
-- "fooxxxx"
--
-- >>> justifyLeft 3 'x' "foobar"
-- "foobar"
justifyLeft :: Int -> Char -> Text -> Text
justifyLeft k c t
    | len >= k  = t
    | otherwise = t `append` replicateChar (k-len) c
  where len = length t
{-# INLINE [1] justifyLeft #-}

-- | /O(n)/ Right-justify a string to the given length, using the
-- specified fill character on the left.  Performs replacement on
-- invalid scalar values.
--
-- Examples:
--
-- >>> justifyRight 7 'x' "bar"
-- "xxxxbar"
--
-- >>> justifyRight 3 'x' "foobar"
-- "foobar"
justifyRight :: Int -> Char -> Text -> Text
justifyRight k c t
    | len >= k  = t
    | otherwise = replicateChar (k-len) c `append` t
  where len = length t
{-# INLINE justifyRight #-}

-- | /O(n)/ Center a string to the given length, using the specified
-- fill character on either side.  Performs replacement on invalid
-- scalar values.
--
-- Examples:
--
-- >>> center 8 'x' "HS"
-- "xxxHSxxx"
center :: Int -> Char -> Text -> Text
center k c t
    | len >= k  = t
    | otherwise = replicateChar l c `append` t `append` replicateChar r c
  where len = length t
        d   = k - len
        r   = d `quot` 2
        l   = d - r
{-# INLINE center #-}

-- | /O(n)/ The 'transpose' function transposes the rows and columns
-- of its 'Text' argument.  Note that this function uses 'pack',
-- 'unpack', and the list version of transpose, and is thus not very
-- efficient.
--
-- Examples:
--
-- >>> transpose ["green","orange"]
-- ["go","rr","ea","en","ng","e"]
--
-- >>> transpose ["blue","red"]
-- ["br","le","ud","e"]
transpose :: [Text] -> [Text]
transpose ts = P.map pack (L.transpose (P.map unpack ts))

-- -----------------------------------------------------------------------------
-- * Reducing 'Text's (folds)

-- | /O(n)/ 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z t = S.foldl f z (stream t)
{-# INLINE foldl #-}

-- | /O(n)/ A strict version of 'foldl'.
foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z t = S.foldl' f z (stream t)
{-# INLINE foldl' #-}

-- | /O(n)/ A variant of 'foldl' that has no starting value argument,
-- and thus must be applied to a non-empty 'Text'.
foldl1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1 f t = S.foldl1 f (stream t)
{-# INLINE foldl1 #-}

-- | /O(n)/ A strict version of 'foldl1'.
foldl1' :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1' f t = S.foldl1' f (stream t)
{-# INLINE foldl1' #-}

-- | /O(n)/ A monadic version of 'foldl''.
--
-- @since 2.1.2
foldlM' :: Monad m => (a -> Char -> m a) -> a -> Text -> m a
foldlM' f z t = S.foldlM' f z (stream t)
{-# INLINE foldlM' #-}

-- | /O(n)/ 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from right to left.
--
-- If the binary operator is strict in its second argument, use 'foldr''
-- instead.
--
-- 'foldr' is lazy like 'Data.List.foldr' for lists: evaluation actually
-- traverses the 'Text' from left to right, only as far as it needs to.
--
-- For example, 'head' can be defined with /O(1)/ complexity using 'foldr':
--
-- @
-- head :: Text -> Char
-- head = foldr const (error "head empty")
-- @
--
-- Searches from left to right with short-circuiting behavior can
-- also be defined using 'foldr' (/e.g./, 'any', 'all', 'find', 'elem').
foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z t = S.foldr f z (stream t)
{-# INLINE foldr #-}

-- | /O(n)/ A variant of 'foldr' that has no starting value argument,
-- and thus must be applied to a non-empty 'Text'.
foldr1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldr1 f t = S.foldr1 f (stream t)
{-# INLINE foldr1 #-}

-- | /O(n)/ A strict version of 'foldr'.
--
-- 'foldr'' evaluates as a right-to-left traversal using constant stack space.
--
-- @since 2.0.1
foldr' :: (Char -> a -> a) -> a -> Text -> a
foldr' f z t = S.foldl' (P.flip f) z (reverseStream t)
{-# INLINE foldr' #-}

-- -----------------------------------------------------------------------------
-- ** Special folds

-- | /O(n)/ Concatenate a list of 'Text's.
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

-- | /O(n)/ Map a function over a 'Text' that results in a 'Text', and
-- concatenate the results.
concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . foldr ((:) . f) []
{-# INLINE concatMap #-}

-- | /O(n)/ 'any' @p@ @t@ determines whether any character in the
-- 'Text' @t@ satisfies the predicate @p@.
any :: (Char -> Bool) -> Text -> Bool
any p t = S.any p (stream t)
{-# INLINE any #-}

-- | /O(n)/ 'all' @p@ @t@ determines whether all characters in the
-- 'Text' @t@ satisfy the predicate @p@.
all :: (Char -> Bool) -> Text -> Bool
all p t = S.all p (stream t)
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'Text', which
-- must be non-empty.
maximum :: HasCallStack => Text -> Char
maximum t = S.maximum (stream t)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'Text', which
-- must be non-empty.
minimum :: HasCallStack => Text -> Char
minimum t = S.minimum (stream t)
{-# INLINE minimum #-}

-- -----------------------------------------------------------------------------
-- * Building 'Text's
-- | /O(n)/ 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left.
-- Performs replacement on invalid scalar values.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- __Properties__
--
-- @'head' ('scanl' f z xs) = z@
--
-- @'last' ('scanl' f z xs) = 'foldl' f z xs@
scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z t = unstream (S.scanl g z (stream t))
    where g a b = safe (f a b)
{-# INLINE scanl #-}

-- | /O(n)/ 'scanl1' is a variant of 'scanl' that has no starting
-- value argument. Performs replacement on invalid scalar values.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 f t | null t    = empty
           | otherwise = scanl f (unsafeHead t) (unsafeTail t)
{-# INLINE scanl1 #-}

-- | /O(n)/ 'scanr' is the right-to-left dual of 'scanl'.  Performs
-- replacement on invalid scalar values.
--
-- > scanr f v == reverse . scanl (flip f) v . reverse
scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr f z = S.reverse . S.reverseScanr g z . reverseStream
    where g a b = safe (f a b)
{-# INLINE scanr #-}

-- | /O(n)/ 'scanr1' is a variant of 'scanr' that has no starting
-- value argument. Performs replacement on invalid scalar values.
scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 f t | null t    = empty
           | otherwise = scanr f (last t) (init t)
{-# INLINE scanr1 #-}

-- | /O(n)/ Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'Text', passing an accumulating
-- parameter from left to right, and returns a final 'Text'.  Performs
-- replacement on invalid scalar values.
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
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- a strict 'foldr'; it applies a function to each element of a
-- 'Text', passing an accumulating parameter from right to left, and
-- returning a final value of this accumulator together with the new
-- 'Text'.
-- Performs replacement on invalid scalar values.
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
{-# INLINE mapAccumR #-}

-- -----------------------------------------------------------------------------
-- ** Generating and unfolding 'Text's

-- | /O(n*m)/ 'replicate' @n@ @t@ is a 'Text' consisting of the input
-- @t@ repeated @n@ times.
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
{-# INLINE [1] replicate #-}

{-# RULES
"TEXT replicate/singleton -> replicateChar" [~1] forall n c.
    replicate n (singleton c) = replicateChar n c
  #-}

-- | /O(n)/ 'replicateChar' @n@ @c@ is a 'Text' of length @n@ with @c@ the
-- value of every element.
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
{-# INLINE replicateChar #-}

-- | /O(n)/, where @n@ is the length of the result. The 'unfoldr'
-- function is analogous to the List 'L.unfoldr'. 'unfoldr' builds a
-- 'Text' from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the 'Text', otherwise
-- 'Just' @(a,b)@.  In this case, @a@ is the next 'Char' in the
-- string, and @b@ is the seed value for further production.
-- Performs replacement on invalid scalar values.
unfoldr     :: (a -> Maybe (Char,a)) -> a -> Text
unfoldr f s = unstream (S.unfoldr (firstf safe . f) s)
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a 'Text' from a seed
-- value. However, the length of the result should be limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the maximum length of the result is known and
-- correct, otherwise its performance is similar to 'unfoldr'.
-- Performs replacement on invalid scalar values.
unfoldrN     :: Int -> (a -> Maybe (Char,a)) -> a -> Text
unfoldrN n f s = unstream (S.unfoldrN n (firstf safe . f) s)
{-# INLINE unfoldrN #-}

-- -----------------------------------------------------------------------------
-- * Substrings

-- | /O(n)/ 'take' @n@, applied to a 'Text', returns the prefix of the
-- 'Text' of length @n@, or the 'Text' itself if @n@ is greater than
-- the length of the Text.
take :: Int -> Text -> Text
take n t@(Text arr off len)
    | n <= 0    = empty
    | n >= len || m >= len || m < 0  = t
    | otherwise = Text arr off m
  where
    m = measureOff n t
{-# INLINE [1] take #-}

-- | /O(n)/ If @t@ is long enough to contain @n@ characters, 'measureOff' @n@ @t@
-- returns a non-negative number, measuring their size in 'Word8'. Otherwise,
-- if @t@ is shorter, return a non-positive number, which is a negated total count
-- of 'Char' available in @t@. If @t@ is empty or @n = 0@, return 0.
--
-- This function is used to implement 'take', 'drop', 'splitAt' and 'length'
-- and is useful on its own in streaming and parsing libraries.
--
-- @since 2.0
measureOff :: Int -> Text -> Int
measureOff !n (Text (A.ByteArray arr) off len) = if len == 0 then 0 else
  cSsizeToInt $
    measure_off arr (intToCSize off) (intToCSize len) (intToCSize n)

-- | /O(n)/ 'takeEnd' @n@ @t@ returns the suffix remaining after
-- taking @n@ characters from the end of @t@.
--
-- Examples:
--
-- >>> takeEnd 3 "foobar"
-- "bar"
--
-- @since 1.1.1.0
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

-- | /O(n)/ 'drop' @n@, applied to a 'Text', returns the suffix of the
-- 'Text' after the first @n@ characters, or the empty 'Text' if @n@
-- is greater than the length of the 'Text'.
drop :: Int -> Text -> Text
drop n t@(Text arr off len)
    | n <= 0    = t
    | n >= len || m >= len || m < 0 = empty
    | otherwise = Text arr (off+m) (len-m)
  where m = measureOff n t
{-# INLINE [1] drop #-}

-- | /O(n)/ 'dropEnd' @n@ @t@ returns the prefix remaining after
-- dropping @n@ characters from the end of @t@.
--
-- Examples:
--
-- >>> dropEnd 3 "foobar"
-- "foo"
--
-- @since 1.1.1.0
dropEnd :: Int -> Text -> Text
dropEnd n t@(Text arr off len)
    | n <= 0    = t
    | n >= len  = empty
    | otherwise = text arr off (iterNEnd n t)

-- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a 'Text',
-- returns the longest prefix (possibly empty) of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t@(Text arr off len) = loop 0
  where loop !i | i >= len    = t
                | p c         = loop (i+d)
                | otherwise   = text arr off i
            where Iter c d    = iter t i
{-# INLINE [1] takeWhile #-}

-- | /O(n)/ 'takeWhileEnd', applied to a predicate @p@ and a 'Text',
-- returns the longest suffix (possibly empty) of elements that
-- satisfy @p@.
-- Examples:
--
-- >>> takeWhileEnd (=='o') "foo"
-- "oo"
--
-- @since 1.2.2.0
takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p t@(Text arr off len) = loop (len-1) len
  where loop !i !l | l <= 0    = t
                   | p c       = loop (i+d) (l+d)
                   | otherwise = text arr (off+l) (len-l)
            where Iter c d     = reverseIter t i
{-# INLINE [1] takeWhileEnd #-}

-- | /O(n)/ 'dropWhile' @p@ @t@ returns the suffix remaining after
-- 'takeWhile' @p@ @t@.
dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t@(Text arr off len) = loop 0 0
  where loop !i !l | l >= len  = empty
                   | p c       = loop (i+d) (l+d)
                   | otherwise = Text arr (off+i) (len-l)
            where Iter c d     = iter t i
{-# INLINE [1] dropWhile #-}

-- | /O(n)/ 'dropWhileEnd' @p@ @t@ returns the prefix remaining after
-- dropping characters that satisfy the predicate @p@ from the end of
-- @t@.
--
-- Examples:
--
-- >>> dropWhileEnd (=='.') "foo..."
-- "foo"
dropWhileEnd :: (Char -> Bool) -> Text -> Text
dropWhileEnd p t@(Text arr off len) = loop (len-1) len
  where loop !i !l | l <= 0    = empty
                   | p c       = loop (i+d) (l+d)
                   | otherwise = Text arr off l
            where Iter c d     = reverseIter t i
{-# INLINE [1] dropWhileEnd #-}

-- | /O(n)/ 'dropAround' @p@ @t@ returns the substring remaining after
-- dropping characters that satisfy the predicate @p@ from both the
-- beginning and end of @t@.
dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p
{-# INLINE [1] dropAround #-}

-- | /O(n)/ Remove leading white space from a string.  Equivalent to:
--
-- > dropWhile isSpace
stripStart :: Text -> Text
stripStart = dropWhile Char.isSpace
{-# INLINE stripStart #-}

-- | /O(n)/ Remove trailing white space from a string.  Equivalent to:
--
-- > dropWhileEnd isSpace
stripEnd :: Text -> Text
stripEnd = dropWhileEnd Char.isSpace
{-# INLINE [1] stripEnd #-}

-- | /O(n)/ Remove leading and trailing white space from a string.
-- Equivalent to:
--
-- > dropAround isSpace
strip :: Text -> Text
strip = dropAround Char.isSpace
{-# INLINE [1] strip #-}

-- | /O(n)/ 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
splitAt :: Int -> Text -> (Text, Text)
splitAt n t@(Text arr off len)
    | n <= 0    = (empty, t)
    | n >= len || m >= len || m < 0  = (t, empty)
    | otherwise = (Text arr off m, Text arr (off+m) (len-m))
  where
    m = measureOff n t

-- | /O(n)/ 'span', applied to a predicate @p@ and text @t@, returns
-- a pair whose first element is the longest prefix (possibly empty)
-- of @t@ of elements that satisfy @p@, and whose second is the
-- remainder of the text.
--
-- >>> T.span (=='0') "000AB"
-- ("000","AB")
span :: (Char -> Bool) -> Text -> (Text, Text)
span p t = case span_ p t of
             (# hd,tl #) -> (hd,tl)
{-# INLINE span #-}

-- | /O(n)/ 'break' is like 'span', but the prefix returned is
-- over elements that fail the predicate @p@.
--
-- >>> T.break (=='c') "180cm"
-- ("180","cm")
break :: (Char -> Bool) -> Text -> (Text, Text)
break p = span (not . p)
{-# INLINE break #-}

-- | /O(length of prefix)/ 'spanM', applied to a monadic predicate @p@,
-- a text @t@, returns a pair @(t1, t2)@ where @t1@ is the longest prefix of
-- @t@ whose elements satisfy @p@, and @t2@ is the remainder of the text.
--
-- >>> T.spanM (\c -> state $ \i -> (fromEnum c == i, i+1)) "abcefg" `runState` 97
-- (("abc","efg"),101)
--
-- 'span' is 'spanM' specialized to 'Data.Functor.Identity.Identity':
--
-- @
-- -- for all p :: Char -> Bool
-- 'span' p = 'Data.Functor.Identity.runIdentity' . 'spanM' ('pure' . p)
-- @
--
-- @since 2.0.1
spanM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanM p t@(Text arr off len) = go 0
  where
    go !i | i < len = case iterArray arr (off+i) of
        Iter c l -> do
            continue <- p c
            if continue then go (i+l)
            else pure (text arr off i, text arr (off+i) (len-i))
    go _ = pure (t, empty)
{-# INLINE spanM #-}

-- | /O(length of suffix)/ 'spanEndM', applied to a monadic predicate @p@,
-- a text @t@, returns a pair @(t1, t2)@ where @t2@ is the longest suffix of
-- @t@ whose elements satisfy @p@, and @t1@ is the remainder of the text.
--
-- >>> T.spanEndM (\c -> state $ \i -> (fromEnum c == i, i-1)) "tuvxyz" `runState` 122
-- (("tuv","xyz"),118)
--
-- @
-- 'spanEndM' p . 'reverse' = fmap ('Data.Bifunctor.bimap' 'reverse' 'reverse') . 'spanM' p
-- @
--
-- @since 2.0.1
spanEndM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanEndM p t@(Text arr off len) = go (len-1)
  where
    go !i | 0 <= i = case reverseIterArray arr (off+i) of
        Iter c l -> do
            continue <- p c
            if continue then go (i+l)
            else pure (text arr off (i+1), text arr (off+i+1) (len-i-1))
    go _ = pure (empty, t)
{-# INLINE spanEndM #-}

-- | /O(n)/ Group characters in a string according to a predicate.
groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy p = loop
  where
    loop t@(Text arr off len)
        | null t    = []
        | otherwise = text arr off n : loop (text arr (off+n) (len-n))
        where Iter c d = iter t 0
              n     = d + findAIndexOrEnd (not . p c) (Text arr (off+d) (len-d))

-- | Returns the /array/ index (in units of 'Word8') at which a
-- character may be found.  This is /not/ the same as the logical
-- index returned by e.g. 'findIndex'.
findAIndexOrEnd :: (Char -> Bool) -> Text -> Int
findAIndexOrEnd q t@(Text _arr _off len) = go 0
    where go !i | i >= len || q c       = i
                | otherwise             = go (i+d)
                where Iter c d          = iter t i

-- | /O(n)/ Group characters in a string by equality.
group :: Text -> [Text]
group = groupBy (==)

-- | /O(n)/ Return all initial segments of the given 'Text', shortest
-- first.
inits :: Text -> [Text]
inits = (NonEmptyList.toList $!) . initsNE

-- | /O(n)/ Return all initial segments of the given 'Text', shortest
-- first.
--
-- @since 2.1.2
initsNE :: Text -> NonEmptyList.NonEmpty Text
initsNE t = empty NonEmptyList.:| case t of
  Text arr off len ->
    let loop i
          | i >= len = []
          | otherwise = let !j = i + iter_ t i in Text arr off j : loop j
    in loop 0

-- | /O(n)/ Return all final segments of the given 'Text', longest
-- first.
tails :: Text -> [Text]
tails = (NonEmptyList.toList $!) . tailsNE

-- | /O(n)/ Return all final segments of the given 'Text', longest
-- first.
--
-- @since 2.1.2
tailsNE :: Text -> NonEmptyList.NonEmpty Text
tailsNE t
  | null t = empty NonEmptyList.:| []
  | otherwise = t NonEmptyList.:| tails (unsafeTail t)

-- $split
--
-- Splitting functions in this library do not perform character-wise
-- copies to create substrings; they just construct new 'Text's that
-- are slices of the original.

-- | /O(m+n)/ Break a 'Text' into pieces separated by the first 'Text'
-- argument (which cannot be empty), consuming the delimiter. An empty
-- delimiter is invalid, and will cause an error to be raised.
--
-- Examples:
--
-- >>> splitOn "\r\n" "a\r\nb\r\nd\r\ne"
-- ["a","b","d","e"]
--
-- >>> splitOn "aaa"  "aaaXaaaXaaaXaaa"
-- ["","X","X","X",""]
--
-- >>> splitOn "x"    "x"
-- ["",""]
--
-- and
--
-- > intercalate s . splitOn s         == id
-- > splitOn (singleton c)             == split (==c)
--
-- (Note: the string @s@ to split on above cannot be empty.)
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
splitOn :: HasCallStack
        => Text
        -- ^ String to split on. If this string is empty, an error
        -- will occur.
        -> Text
        -- ^ Input text.
        -> [Text]
splitOn pat@(Text _ _ l) src@(Text arr off len)
    | l <= 0          = emptyError "splitOn"
    | isSingleton pat = split (== unsafeHead pat) src
    | otherwise       = go 0 (indices pat src)
  where
    go !s (x:xs) =  text arr (s+off) (x-s) : go (x+l) xs
    go  s _      = [text arr (s+off) (len-s)]
{-# INLINE [1] splitOn #-}

{-# RULES
"TEXT splitOn/singleton -> split/==" [~1] forall c t.
    splitOn (singleton c) t = split (==c) t
  #-}

-- | /O(n)/ Splits a 'Text' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- >>> split (=='a') "aabbaca"
-- ["","","bb","c",""]
--
-- >>> split (=='a') ""
-- [""]
split :: (Char -> Bool) -> Text -> [Text]
split p t
    | null t = [empty]
    | otherwise = loop t
    where loop s | null s'   = [l]
                 | otherwise = l : loop (unsafeTail s')
              where (# l, s' #) = span_ (not . p) s
{-# INLINE split #-}

-- | /O(n)/ Splits a 'Text' into components of length @k@.  The last
-- element may be shorter than the other chunks, depending on the
-- length of the input. Examples:
--
-- >>> chunksOf 3 "foobarbaz"
-- ["foo","bar","baz"]
--
-- >>> chunksOf 4 "haskell.org"
-- ["hask","ell.","org"]
chunksOf :: Int -> Text -> [Text]
chunksOf k = go
  where
    go t = case splitAt k t of
             (a,b) | null a    -> []
                   | otherwise -> a : go b
{-# INLINE chunksOf #-}

-- ----------------------------------------------------------------------------
-- * Searching

-------------------------------------------------------------------------------
-- ** Searching with a predicate

-- | /O(n)/ The 'elem' function takes a character and a 'Text', and
-- returns 'True' if the element is found in the given 'Text', or
-- 'False' otherwise.
elem :: Char -> Text -> Bool
elem c t = S.any (== c) (stream t)
{-# INLINE elem #-}

-- | /O(n)/ The 'find' function takes a predicate and a 'Text', and
-- returns the first element matching the predicate, or 'Nothing' if
-- there is no such element.
find :: (Char -> Bool) -> Text -> Maybe Char
find p t = S.findBy p (stream t)
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate and a 'Text',
-- and returns the pair of 'Text's with elements which do and do not
-- satisfy the predicate, respectively; i.e.
--
-- > partition p t == (filter p t, filter (not . p) t)
partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p t = (filter p t, filter (not . p) t)
{-# INLINE partition #-}

-- | /O(n)/ 'filter', applied to a predicate and a 'Text',
-- returns a 'Text' containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Text -> Text
filter p = filter_ text p
{-# INLINE [1] filter #-}

{-# RULES
"TEXT filter/filter -> filter" forall p q t.
    filter p (filter q t) = filter (\c -> q c && p c) t
#-}

-- | /O(n+m)/ Find the first instance of @needle@ (which must be
-- non-'null') in @haystack@.  The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
--
-- Examples:
--
-- >>> breakOn "::" "a::b::c"
-- ("a","::b::c")
--
-- >>> breakOn "/" "foobar"
-- ("foobar","")
--
-- Laws:
--
-- > append prefix match == haystack
-- >   where (prefix, match) = breakOn needle haystack
--
-- If you need to break a string by a substring repeatedly (e.g. you
-- want to break on every instance of a substring), use 'breakOnAll'
-- instead, as it has lower startup overhead.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
breakOn :: HasCallStack => Text -> Text -> (Text, Text)
breakOn pat src@(Text arr off len)
    | null pat  = emptyError "breakOn"
    | otherwise = case indices pat src of
                    []    -> (src, empty)
                    (x:_) -> (text arr off x, text arr (off+x) (len-x))
{-# INLINE breakOn #-}

-- | /O(n+m)/ Similar to 'breakOn', but searches from the end of the
-- string.
--
-- The first element of the returned tuple is the prefix of @haystack@
-- up to and including the last match of @needle@.  The second is the
-- remainder of @haystack@, following the match.
--
-- >>> breakOnEnd "::" "a::b::c"
-- ("a::b::","c")
breakOnEnd :: HasCallStack => Text -> Text -> (Text, Text)
breakOnEnd pat src = (reverse b, reverse a)
    where (a,b) = breakOn (reverse pat) (reverse src)
{-# INLINE breakOnEnd #-}

-- | /O(n+m)/ Find all non-overlapping instances of @needle@ in
-- @haystack@.  Each element of the returned list consists of a pair:
--
-- * The entire string prior to the /k/th match (i.e. the prefix)
--
-- * The /k/th match, followed by the remainder of the string
--
-- Examples:
--
-- >>> breakOnAll "::" ""
-- []
--
-- >>> breakOnAll "/" "a/b/c/"
-- [("a","/b/c/"),("a/b","/c/"),("a/b/c","/")]
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- The @needle@ parameter may not be empty.
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
{-# INLINE breakOnAll #-}

-------------------------------------------------------------------------------
-- ** Indexing 'Text's

-- $index
--
-- If you think of a 'Text' value as an array of 'Char' values (which
-- it is not), you run the risk of writing inefficient code.
--
-- An idiom that is common in some languages is to find the numeric
-- offset of a character or substring, then use that number to split
-- or trim the searched string.  With a 'Text' value, this approach
-- would require two /O(n)/ operations: one to perform the search, and
-- one to operate from wherever the search ended.
--
-- For example, suppose you have a string that you want to split on
-- the substring @\"::\"@, such as @\"foo::bar::quux\"@. Instead of
-- searching for the index of @\"::\"@ and taking the substrings
-- before and after that index, you would instead use @breakOnAll \"::\"@.

-- | /O(n)/ 'Text' index (subscript) operator, starting from 0.
index :: HasCallStack => Text -> Int -> Char
index t n = S.index (stream t) n
{-# INLINE index #-}

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'Text'
-- and returns the index of the first element in the 'Text' satisfying
-- the predicate.
findIndex :: (Char -> Bool) -> Text -> Maybe Int
findIndex p t = S.findIndex p (stream t)
{-# INLINE findIndex #-}

-- | /O(n+m)/ The 'count' function returns the number of times the
-- query string appears in the given 'Text'. An empty query string is
-- invalid, and will cause an error to be raised.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
count :: HasCallStack => Text -> Text -> Int
count pat
    | null pat        = emptyError "count"
    | isSingleton pat = countChar (unsafeHead pat)
    | otherwise       = L.length . indices pat
{-# INLINE [1] count #-}

{-# RULES
"TEXT count/singleton -> countChar" [~1] forall c t.
    count (singleton c) t = countChar c t
  #-}

-- | /O(n)/ The 'countChar' function returns the number of times the
-- query element appears in the given 'Text'.
countChar :: Char -> Text -> Int
countChar c t = S.countChar c (stream t)
{-# INLINE countChar #-}

-------------------------------------------------------------------------------
-- * Zipping

-- | /O(n)/ 'zip' takes two 'Text's and returns a list of
-- corresponding pairs of bytes. If one input 'Text' is short,
-- excess elements of the longer 'Text' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: Text -> Text -> [(Char,Char)]
zip a b = S.unstreamList $ S.zipWith (,) (stream a) (stream b)
{-# INLINE zip #-}

-- | /O(n)/ 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
-- Performs replacement on invalid scalar values.
zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f t1 t2 = unstream (S.zipWith g (stream t1) (stream t2))
    where g a b = safe (f a b)
{-# INLINE [1] zipWith #-}

-- | /O(n)/ Breaks a 'Text' up into a list of words, delimited by 'Char's
-- representing white space.
words :: Text -> [Text]
words (Text arr off len) = loop 0 0
  where
    loop !start !n
        | n >= len = if start == n
                     then []
                     else [Text arr (start + off) (n - start)]
        -- Spaces in UTF-8 take either 1 byte for 0x09..0x0D + 0x20
        | isAsciiSpace w0 =
            if start == n
            then loop (n + 1) (n + 1)
            else Text arr (start + off) (n - start) : loop (n + 1) (n + 1)
        | w0 < 0x80 = loop start (n + 1)
        -- or 2 bytes for 0xA0
        | w0 == 0xC2, w1 == 0xA0 =
            if start == n
            then loop (n + 2) (n + 2)
            else Text arr (start + off) (n - start) : loop (n + 2) (n + 2)
        | w0 < 0xE0 = loop start (n + 2)
        -- or 3 bytes for 0x1680 + 0x2000..0x200A + 0x2028..0x2029 + 0x202F + 0x205F + 0x3000
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
{-# INLINE words #-}

-- Adapted from Data.ByteString.Internal.isSpaceWord8
isAsciiSpace :: Word8 -> Bool
isAsciiSpace w = w .&. 0x50 == 0 && w < 0x80 && (w == 0x20 || w - 0x09 < 5)
{-# INLINE isAsciiSpace #-}

-- | /O(n)/ Breaks a 'Text' up into a list of 'Text's at newline characters
-- @'\\n'@ (LF, line feed). The resulting strings do not contain newlines.
--
-- 'lines' __does not__ treat @'\\r'@ (CR, carriage return) as a newline character.
lines :: Text -> [Text]
lines (Text arr@(A.ByteArray arr#) off len) = go off
  where
    go !n
      | n >= len + off = []
      | delta < 0 = [Text arr n (len + off - n)]
      | otherwise = Text arr n delta : go (n + delta + 1)
      where
        delta = memchr arr# n (len + off - n) 0x0A
{-# INLINE lines #-}

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
unlines :: [Text] -> Text
unlines = concat . L.foldr (\t acc -> t : singleton '\n' : acc) []
{-# INLINE unlines #-}

-- | /O(n)/ Joins words using single space characters.
unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- | /O(n)/ The 'isPrefixOf' function takes two 'Text's and returns
-- 'True' if and only if the first is a prefix of the second.
isPrefixOf :: Text -> Text -> Bool
isPrefixOf a@(Text _ _ alen) b@(Text _ _ blen) =
    alen <= blen && S.isPrefixOf (stream a) (stream b)
{-# INLINE [1] isPrefixOf #-}

-- | /O(n)/ The 'isSuffixOf' function takes two 'Text's and returns
-- 'True' if and only if the first is a suffix of the second.
isSuffixOf :: Text -> Text -> Bool
isSuffixOf a@(Text _aarr _aoff alen) b@(Text barr boff blen) =
    d >= 0 && a == b'
  where d              = blen - alen
        b' | d == 0    = b
           | otherwise = Text barr (boff+d) alen
{-# INLINE isSuffixOf #-}

-- | /O(n+m)/ The 'isInfixOf' function takes two 'Text's and returns
-- 'True' if and only if the first is contained, wholly and intact, anywhere
-- within the second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
isInfixOf ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Text -> Bool
isInfixOf needle haystack
    | null needle        = True
    | isSingleton needle = S.elem (unsafeHead needle) . S.stream $ haystack
    | otherwise          = not . L.null . indices needle $ haystack
{-# INLINE [1] isInfixOf #-}

-------------------------------------------------------------------------------
-- * View patterns

-- | /O(n)/ Return the suffix of the second string if its prefix
-- matches the entire first string.
--
-- Examples:
--
-- >>> stripPrefix "foo" "foobar"
-- Just "bar"
--
-- >>> stripPrefix ""    "baz"
-- Just "baz"
--
-- >>> stripPrefix "foo" "quux"
-- Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text as T
-- >
-- > fnordLength :: Text -> Int
-- > fnordLength (stripPrefix "fnord" -> Just suf) = T.length suf
-- > fnordLength _                                 = -1
stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p@(Text _arr _off plen) t@(Text arr off len)
    | p `isPrefixOf` t = Just $! text arr (off+plen) (len-plen)
    | otherwise        = Nothing

-- | /O(n)/ Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match.
--
-- If the strings do not have a common prefix or either one is empty,
-- this function returns 'Nothing'.
--
-- Examples:
--
-- >>> commonPrefixes "foobar" "fooquux"
-- Just ("foo","bar","quux")
--
-- >>> commonPrefixes "veeble" "fetzer"
-- Nothing
--
-- >>> commonPrefixes "" "baz"
-- Nothing
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
{-# INLINE commonPrefixes #-}

-- | /O(n)/ Return the prefix of the second string if its suffix
-- matches the entire first string.
--
-- Examples:
--
-- >>> stripSuffix "bar" "foobar"
-- Just "foo"
--
-- >>> stripSuffix ""    "baz"
-- Just "baz"
--
-- >>> stripSuffix "foo" "quux"
-- Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text as T
-- >
-- > quuxLength :: Text -> Int
-- > quuxLength (stripSuffix "quux" -> Just pre) = T.length pre
-- > quuxLength _                                = -1
stripSuffix :: Text -> Text -> Maybe Text
stripSuffix p@(Text _arr _off plen) t@(Text arr off len)
    | p `isSuffixOf` t = Just $! text arr off (len-plen)
    | otherwise        = Nothing

-- | Add a list of non-negative numbers.  Errors out on overflow.
sumP :: String -> [Int] -> Int
sumP fun = L.foldl' add 0
  where add a x
            | ax >= 0   = ax
            | otherwise = overflowError fun
          where ax = a + x
{-# INLINE sumP #-} -- Use foldl' and inline for fusion.

emptyError :: HasCallStack => String -> a
emptyError fun = P.error $ "Data.Text." ++ fun ++ ": empty input"

overflowError :: HasCallStack => String -> a
overflowError fun = P.error $ "Data.Text." ++ fun ++ ": size overflow"

-- | Convert a value to 'Text'.
--
-- @since 2.1.2
show :: Show a => a -> Text
show = pack . P.show

-- | /O(n)/ Make a distinct copy of the given string, sharing no
-- storage with the original string.
--
-- As an example, suppose you read a large string, of which you need
-- only a small portion.  If you do not use 'copy', the entire original
-- array will be kept alive in memory by the smaller string. Making a
-- copy \"breaks the link\" to the original array, allowing it to be
-- garbage collected if there are no other live references to it.
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

-------------------------------------------------
-- NOTE: the named chunk below used by doctest;
--       verify the doctests via `doctest -fobject-code Data/Text.hs`

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import qualified Data.Text as T

{-# LANGUAGE BangPatterns, CPP, GeneralizedNewtypeDeriving, MagicHash,
    UnliftedFFITypes #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Data.Text.Encoding
-- Copyright   : (c) 2009, 2010, 2011 Bryan O'Sullivan,
--               (c) 2009 Duncan Coutts,
--               (c) 2008, 2009 Tom Harper
--               (c) 2021 Andrew Lelechenko
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : portable
--
-- Functions for converting 'Text' values to and from 'ByteString',
-- using several standard encodings.
--
-- To gain access to a much larger family of encodings, use the
-- <http://hackage.haskell.org/package/text-icu text-icu package>.

module Data.Text.Encoding
    (
    -- * Decoding ByteStrings to Text
    -- $strict

    -- ** Total Functions #total#
    -- $total
      decodeLatin1
    , decodeASCIIPrefix
    , decodeUtf8Lenient
    , decodeUtf8'
    , decodeASCII'

    -- *** Controllable error handling
    , decodeUtf8With
    , decodeUtf16LEWith
    , decodeUtf16BEWith
    , decodeUtf32LEWith
    , decodeUtf32BEWith

    -- *** Stream oriented decoding
    -- $stream
    , streamDecodeUtf8With
    , Decoding(..)

    -- *** Incremental UTF-8 decoding
    -- $incremental
    , decodeUtf8Chunk
    , decodeUtf8More
    , Utf8State
    , startUtf8State
    , StrictBuilder
    , StrictTextBuilder
    , strictBuilderToText
    , textToStrictBuilder

    -- ** Partial Functions
    -- $partial
    , decodeASCII
    , decodeUtf8
    , decodeUtf16LE
    , decodeUtf16BE
    , decodeUtf32LE
    , decodeUtf32BE

    -- *** Stream oriented decoding
    , streamDecodeUtf8

    -- * Encoding Text to ByteStrings
    , encodeUtf8
    , encodeUtf16LE
    , encodeUtf16BE
    , encodeUtf32LE
    , encodeUtf32BE

    -- * Encoding Text using ByteString Builders
    , encodeUtf8Builder
    , encodeUtf8BuilderEscaped

    -- * ByteString validation
    -- $validation
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

-- $validation
-- These functions are for validating 'ByteString's as encoded text.

-- $strict
--
-- All of the single-parameter functions for decoding bytestrings
-- encoded in one of the Unicode Transformation Formats (UTF) operate
-- in a /strict/ mode: each will throw an exception if given invalid
-- input.
--
-- Each function has a variant, whose name is suffixed with -'With',
-- that gives greater control over the handling of decoding errors.
-- For instance, 'decodeUtf8' will throw an exception, but
-- 'decodeUtf8With' allows the programmer to determine what to do on a
-- decoding error.

-- $total
--
-- These functions facilitate total decoding and should be preferred
-- over their partial counterparts.

-- $partial
--
-- These functions are partial and should only be used with great caution
-- (preferably not at all). See "Data.Text.Encoding#g:total" for better
-- solutions.

-- | Decode a 'ByteString' containing ASCII text.
--
-- This is a total function which returns a pair of the longest ASCII prefix
-- as 'Text', and the remaining suffix as 'ByteString'.
--
-- Important note: the pair is lazy. This lets you check for errors by testing
-- whether the second component is empty, without forcing the first component
-- (which does a copy).
-- To drop references to the input bytestring, force the prefix
-- (using 'seq' or @BangPatterns@) and drop references to the suffix.
--
-- === Properties
--
-- - If @(prefix, suffix) = decodeAsciiPrefix s@, then @'encodeUtf8' prefix <> suffix = s@.
-- - Either @suffix@ is empty, or @'B.head' suffix > 127@.
--
-- @since 2.0.2
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
{-# INLINE decodeASCIIPrefix #-}

-- | Decode a 'ByteString' containing 7-bit ASCII encoded text.
--
-- This is a total function which returns either the 'ByteString' converted to a
-- 'Text' containing ASCII text, or 'Nothing'.
--
-- Use 'decodeASCIIPrefix' to retain the longest ASCII prefix for an invalid
-- input instead of discarding it.
--
-- @since 2.0.2
decodeASCII' :: ByteString -> Maybe Text
decodeASCII' bs =
  let (prefix, suffix) = decodeASCIIPrefix bs in
  if B.null suffix then Just prefix else Nothing
{-# INLINE decodeASCII' #-}

-- | Decode a 'ByteString' containing 7-bit ASCII encoded text.
--
-- This is a partial function: it checks that input does not contain
-- anything except ASCII and copies buffer or throws an error otherwise.
decodeASCII :: ByteString -> Text
decodeASCII bs =
  let (prefix, suffix) = decodeASCIIPrefix bs in
  case B.uncons suffix of
    Nothing -> prefix
    Just (word, _) ->
      let !errPos = B.length bs - B.length suffix in
      error $ "decodeASCII: detected non-ASCII codepoint " ++ show word ++ " at position " ++ show errPos

-- | Decode a 'ByteString' containing Latin-1 (aka ISO-8859-1) encoded text.
--
-- 'decodeLatin1' is semantically equivalent to
--  @Data.Text.pack . Data.ByteString.Char8.unpack@
--
-- This is a total function. However, bear in mind that decoding Latin-1 (non-ASCII)
-- characters to UTf-8 requires actual work and is not just buffer copying.
--
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

-- $stream
--
-- The 'streamDecodeUtf8' and 'streamDecodeUtf8With' functions accept
-- a 'ByteString' that represents a possibly incomplete input (e.g. a
-- packet from a network stream) that may not end on a UTF-8 boundary.
--
-- 1. The maximal prefix of 'Text' that could be decoded from the
--    given input.
--
-- 2. The suffix of the 'ByteString' that could not be decoded due to
--    insufficient input.
--
-- 3. A function that accepts another 'ByteString'.  That string will
--    be assumed to directly follow the string that was passed as
--    input to the original function, and it will in turn be decoded.
--
-- To help understand the use of these functions, consider the Unicode
-- string @\"hi &#9731;\"@. If encoded as UTF-8, this becomes @\"hi
-- \\xe2\\x98\\x83\"@; the final @\'&#9731;\'@ is encoded as 3 bytes.
--
-- Now suppose that we receive this encoded string as 3 packets that
-- are split up on untidy boundaries: @[\"hi \\xe2\", \"\\x98\",
-- \"\\x83\"]@. We cannot decode the entire Unicode string until we
-- have received all three packets, but we would like to make progress
-- as we receive each one.
--
-- @
-- ghci> let s0\@('Some' _ _ f0) = 'streamDecodeUtf8' \"hi \\xe2\"
-- ghci> s0
-- 'Some' \"hi \" \"\\xe2\" _
-- @
--
-- We use the continuation @f0@ to decode our second packet.
--
-- @
-- ghci> let s1\@('Some' _ _ f1) = f0 \"\\x98\"
-- ghci> s1
-- 'Some' \"\" \"\\xe2\\x98\"
-- @
--
-- We could not give @f0@ enough input to decode anything, so it
-- returned an empty string. Once we feed our second continuation @f1@
-- the last byte of input, it will make progress.
--
-- @
-- ghci> let s2\@('Some' _ _ f2) = f1 \"\\x83\"
-- ghci> s2
-- 'Some' \"\\x2603\" \"\" _
-- @
--
-- If given invalid input, an exception will be thrown by the function
-- or continuation where it is encountered.

-- | A stream oriented decoding result.
--
-- @since 1.0.0.0
data Decoding = Some !Text !ByteString (ByteString -> Decoding)

instance Show Decoding where
    showsPrec d (Some t bs _) = showParen (d > prec) $
                                showString "Some " . showsPrec prec' t .
                                showChar ' ' . showsPrec prec' bs .
                                showString " _"
      where prec = 10; prec' = prec + 1

-- | Decode, in a stream oriented way, a 'ByteString' containing UTF-8
-- encoded text that is known to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown (either by this function or a continuation) that cannot be
-- caught in pure code.  For more control over the handling of invalid
-- data, use 'streamDecodeUtf8With'.
--
-- @since 1.0.0.0
streamDecodeUtf8 ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Decoding
streamDecodeUtf8 = streamDecodeUtf8With strictDecode

-- | Decode, in a stream oriented way, a lazy 'ByteString' containing UTF-8
-- encoded text.
--
-- @since 1.0.0.0
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

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Surrogate code points in replacement character returned by 'OnDecodeError'
-- will be automatically remapped to the replacement char @U+FFFD@.
decodeUtf8With ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  OnDecodeError -> ByteString -> Text
decodeUtf8With onErr = decodeUtf8With1 onErr invalidUtf8Msg

invalidUtf8Msg :: String
invalidUtf8Msg = "Data.Text.Encoding: Invalid UTF-8 stream"

-- | Decode a 'ByteString' containing UTF-8 encoded text that is known
-- to be valid.
--
-- If the input contains any invalid UTF-8 data, an exception will be
-- thrown that cannot be caught in pure code.  For more control over
-- the handling of invalid data, use 'decodeUtf8'' or
-- 'decodeUtf8With'.
--
-- This is a partial function: it checks that input is a well-formed
-- UTF-8 sequence and copies buffer or throws an error otherwise.
--
decodeUtf8 :: ByteString -> Text
decodeUtf8 = decodeUtf8With strictDecode
{-# INLINE[0] decodeUtf8 #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- If the input contains any invalid UTF-8 data, the relevant
-- exception will be returned, otherwise the decoded text.
decodeUtf8' ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  ByteString -> Either UnicodeException Text
decodeUtf8' = unsafeDupablePerformIO . try . evaluate . decodeUtf8With strictDecode
{-# INLINE decodeUtf8' #-}

-- | Decode a 'ByteString' containing UTF-8 encoded text.
--
-- Any invalid input bytes will be replaced with the Unicode replacement
-- character U+FFFD.
--
-- @since 2.0
decodeUtf8Lenient :: ByteString -> Text
decodeUtf8Lenient = decodeUtf8With lenientDecode

-- | Encode text to a ByteString 'B.Builder' using UTF-8 encoding.
--
-- @since 1.1.0.0
encodeUtf8Builder :: Text -> B.Builder
encodeUtf8Builder =
    -- manual eta-expansion to ensure inlining works as expected
    \txt -> B.builder (step txt)
  where
    step txt@(Text arr off len) !k br@(B.BufferRange op ope)
      -- Ensure that the common case is not recursive and therefore yields
      -- better code.
      | op' <= ope = do
          unsafeSTToIO $ A.copyToPointer arr off op len
          k (B.BufferRange op' ope)
      | otherwise = textCopyStep txt k br
      where
        op' = op `plusPtr` len
{-# INLINE encodeUtf8Builder #-}

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

-- | Encode text using UTF-8 encoding and escape the ASCII characters using
-- a 'BP.BoundedPrim'.
--
-- Use this function is to implement efficient encoders for text-based formats
-- like JSON or HTML.
--
-- @since 1.1.0.0
{-# INLINE encodeUtf8BuilderEscaped #-}
-- TODO: Extend documentation with references to source code in @blaze-html@
-- or @aeson@ that uses this function.
encodeUtf8BuilderEscaped :: BP.BoundedPrim Word8 -> Text -> B.Builder
encodeUtf8BuilderEscaped be =
    -- manual eta-expansion to ensure inlining works as expected
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
          -- TODO: Use a loop with an integrated bound's check if outRemaining
          -- is smaller than 8, as this will save on divisions.
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

-- | Encode text using UTF-8 encoding.
encodeUtf8 :: Text -> ByteString
encodeUtf8 (Text arr off len)
  | len == 0  = B.empty
  -- It would be easier to use Data.ByteString.Short.fromShort and slice later,
  -- but this is undesirable when len is significantly smaller than length arr.
  | otherwise = unsafeDupablePerformIO $ do
    marr@(A.MutableByteArray mba) <- unsafeSTToIO $ A.newPinned len
    unsafeSTToIO $ A.copyI len marr 0 arr off
    let fp = ForeignPtr (byteArrayContents# (unsafeCoerce# mba))
                        (PlainPtr mba)
    pure $ B.fromForeignPtr fp 0 len

-- | Decode text from little endian UTF-16 encoding.
decodeUtf16LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16LEWith onErr bs = F.unstream (E.streamUtf16LE onErr bs)
{-# INLINE decodeUtf16LEWith #-}

-- | Decode text from little endian UTF-16 encoding.
--
-- If the input contains any invalid little endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16LEWith'.
decodeUtf16LE :: ByteString -> Text
decodeUtf16LE = decodeUtf16LEWith strictDecode
{-# INLINE decodeUtf16LE #-}

-- | Decode text from big endian UTF-16 encoding.
decodeUtf16BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf16BEWith onErr bs = F.unstream (E.streamUtf16BE onErr bs)
{-# INLINE decodeUtf16BEWith #-}

-- | Decode text from big endian UTF-16 encoding.
--
-- If the input contains any invalid big endian UTF-16 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf16BEWith'.
decodeUtf16BE :: ByteString -> Text
decodeUtf16BE = decodeUtf16BEWith strictDecode
{-# INLINE decodeUtf16BE #-}

-- | Encode text using little endian UTF-16 encoding.
encodeUtf16LE :: Text -> ByteString
encodeUtf16LE txt = E.unstream (E.restreamUtf16LE (F.stream txt))
{-# INLINE encodeUtf16LE #-}

-- | Encode text using big endian UTF-16 encoding.
encodeUtf16BE :: Text -> ByteString
encodeUtf16BE txt = E.unstream (E.restreamUtf16BE (F.stream txt))
{-# INLINE encodeUtf16BE #-}

-- | Decode text from little endian UTF-32 encoding.
decodeUtf32LEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32LEWith onErr bs = F.unstream (E.streamUtf32LE onErr bs)
{-# INLINE decodeUtf32LEWith #-}

-- | Decode text from little endian UTF-32 encoding.
--
-- If the input contains any invalid little endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32LEWith'.
decodeUtf32LE :: ByteString -> Text
decodeUtf32LE = decodeUtf32LEWith strictDecode
{-# INLINE decodeUtf32LE #-}

-- | Decode text from big endian UTF-32 encoding.
decodeUtf32BEWith :: OnDecodeError -> ByteString -> Text
decodeUtf32BEWith onErr bs = F.unstream (E.streamUtf32BE onErr bs)
{-# INLINE decodeUtf32BEWith #-}

-- | Decode text from big endian UTF-32 encoding.
--
-- If the input contains any invalid big endian UTF-32 data, an
-- exception will be thrown.  For more control over the handling of
-- invalid data, use 'decodeUtf32BEWith'.
decodeUtf32BE :: ByteString -> Text
decodeUtf32BE = decodeUtf32BEWith strictDecode
{-# INLINE decodeUtf32BE #-}

-- | Encode text using little endian UTF-32 encoding.
encodeUtf32LE :: Text -> ByteString
encodeUtf32LE txt = E.unstream (E.restreamUtf32LE (F.stream txt))
{-# INLINE encodeUtf32LE #-}

-- | Encode text using big endian UTF-32 encoding.
encodeUtf32BE :: Text -> ByteString
encodeUtf32BE txt = E.unstream (E.restreamUtf32BE (F.stream txt))
{-# INLINE encodeUtf32BE #-}

-- $incremental
-- The functions 'decodeUtf8Chunk' and 'decodeUtf8More' provide more
-- control for error-handling and streaming.
--
-- - Those functions return an UTF-8 prefix of the given 'ByteString' up to the next error.
--   For example this lets you insert or delete arbitrary text, or do some
--   stateful operations before resuming, such as keeping track of error locations.
--   In contrast, the older stream-oriented interface only lets you substitute
--   a single fixed 'Char' for each invalid byte in 'OnDecodeError'.
-- - That prefix is encoded as a 'StrictBuilder', so you can accumulate chunks
--   before doing the copying work to construct a 'Text', or you can
--   output decoded fragments immediately as a lazy 'Data.Text.Lazy.Text'.
--
-- For even lower-level primitives, see 'validateUtf8Chunk' and 'validateUtf8More'.

{-# LANGUAGE BangPatterns, CPP, RecordWildCards, ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
-- |
-- Module      : Data.Text.IO
-- Copyright   : (c) 2009, 2010 Bryan O'Sullivan,
--               (c) 2009 Simon Marlow
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- Efficient locale-sensitive support for text I\/O.
--
-- The functions in this module obey the runtime system's locale,
-- character set encoding, and line ending conversion settings.
--
-- If you want to do I\/O using the UTF-8 encoding, use @Data.Text.IO.Utf8@,
-- which is faster than this module.
--
-- If you know in advance that you will be working with data that has
-- a specific encoding, and your application is highly
-- performance sensitive, you may find that it is faster to perform
-- I\/O with bytestrings and to encode and decode yourself than to use
-- the functions in this module.

module Data.Text.IO
    (
    -- * File-at-a-time operations
      readFile
    , writeFile
    , appendFile
    -- * Operations on handles
    , hGetContents
    , hGetChunk
    , hGetLine
    , hPutStr
    , hPutStrLn
    -- * Special cases for standard input and output
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

-- | The 'readFile' function reads a file and returns the contents of
-- the file as a string.  The entire file is read strictly, as with
-- 'getContents'.
--
-- Beware that this function (similarly to 'Prelude.readFile') is locale-dependent.
-- Unexpected system locale may cause your application to read corrupted data or
-- throw runtime exceptions about "invalid argument (invalid byte sequence)"
-- or "invalid argument (invalid character)". This is also slow, because GHC
-- first converts an entire input to UTF-32, which is afterwards converted to UTF-8.
--
-- If your data is UTF-8,
-- using 'Data.Text.Encoding.decodeUtf8' '.' 'Data.ByteString.readFile'
-- is a much faster and safer alternative.
readFile :: FilePath -> IO Text
readFile name = openFile name ReadMode >>= hGetContents

-- | Write a string to a file.  The file is truncated to zero length
-- before writing begins.
writeFile :: FilePath -> Text -> IO ()
writeFile p = withFile p WriteMode . flip hPutStr

-- | Write a string to the end of a file.
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

-- | Wrap readChunk and return a value indicating if we're reached the EOF.
-- This is needed because unpack_nl is unable to discern the difference
-- between a buffer with just \r due to EOF or because not enough data was left
-- for decoding. e.g. the final character decoded from the byte buffer was \r.
readChunkEof :: Handle__ -> CharBuffer -> IO (Text, Bool)
readChunkEof hh buf = do t <- readChunk hh buf
                         return (t, False)

-- | /Experimental./ Read a single chunk of strict text from a
-- 'Handle'. The size of the chunk depends on the amount of input
-- currently buffered.
--
-- This function blocks only if there is no data available, and EOF
-- has not yet been reached. Once EOF is reached, this function
-- returns an empty string instead of throwing an exception.
hGetChunk :: Handle -> IO Text
hGetChunk h = wantReadableHandle "hGetChunk" h readSingleChunk
 where
  readSingleChunk hh@Handle__{..} = do
    buf <- readIORef haCharBuffer
    (t, _) <- readChunkEof hh buf `E.catch` catchError "hGetChunk" h hh
    return (hh, t)

-- | Read the remaining contents of a 'Handle' as a string.  The
-- 'Handle' is closed once the contents have been read, or if an
-- exception is thrown.
--
-- Internally, this function reads a chunk at a time from the
-- lower-level buffering abstraction, and concatenates the chunks into
-- a single string once the entire file has been read.
--
-- As a result, it requires approximately twice as much memory as its
-- result to construct its result.  For files more than a half of
-- available RAM in size, this may result in memory exhaustion.
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

-- | Use a more efficient buffer size if we're reading in
-- block-buffered mode with the default buffer size.  When we can
-- determine the size of the handle we're reading, set the buffer size
-- to that, so that we can read the entire file in one chunk.
-- Otherwise, use a buffer size of at least 16KB.
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

-- | Read a single line from a handle.
hGetLine :: Handle -> IO Text
hGetLine = hGetLineWith T.concat

-- | The 'interact' function takes a function of type @Text -> Text@
-- as its argument. The entire input from the standard input device is
-- passed to this function as its argument, and the resulting string
-- is output on the standard output device.
interact :: (Text -> Text) -> IO ()
interact f = putStr . f =<< getContents

-- | Read all user input on 'stdin' as a single string.
getContents :: IO Text
getContents = hGetContents stdin

-- | Read a single line of user input from 'stdin'.
getLine :: IO Text
getLine = hGetLine stdin

-- | Write a string to 'stdout'.
putStr :: Text -> IO ()
putStr = hPutStr stdout

-- | Write a string to 'stdout', followed by a newline.
putStrLn :: Text -> IO ()
putStrLn = hPutStrLn stdout

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE BangPatterns, MagicHash, CPP, TypeFamilies #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Data.Text.Lazy
-- Copyright   : (c) 2009, 2010, 2012 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Portability : GHC
--
-- A time and space-efficient implementation of Unicode text using
-- lists of packed arrays.
--
-- /Note/: Read below the synopsis for important notes on the use of
-- this module.
--
-- The representation used by this module is suitable for high
-- performance use and for streaming large quantities of data.  It
-- provides a means to manipulate a large body of text without
-- requiring that the entire content be resident in memory.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons',
-- have better time complexity than their "Data.Text" equivalents, due
-- to the underlying representation being a list of chunks. For other
-- operations, lazy 'Text's are usually within a few percent of strict
-- ones, but often with better heap usage if used in a streaming
-- fashion. For data larger than available memory, or if you have
-- tight memory constraints, this module will be the only option.
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.Text.Lazy as L

module Data.Text.Lazy
    (
    -- * Fusion
    -- $fusion

    -- * Acceptable data
    -- $replacement

    -- * Types
      Text
    , LazyText

    -- * Creation and elimination
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

    -- * Pattern matching
    , pattern Empty
    , pattern (:<)
    , pattern (:>)

    -- * Basic interface
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

    -- * Transformations
    , map
    , intercalate
    , intersperse
    , transpose
    , reverse
    , replace

    -- ** Case conversion
    -- $case
    , toCaseFold
    , toLower
    , toUpper
    , toTitle

    -- ** Justification
    , justifyLeft
    , justifyRight
    , center

    -- * Folds
    , foldl
    , foldl'
    , foldl1
    , foldl1'
    , foldr
    , foldr1
    , foldlM'

    -- ** Special folds
    , concat
    , concatMap
    , any
    , all
    , maximum
    , minimum
    , isAscii

    -- * Construction

    -- ** Scans
    , scanl
    , scanl1
    , scanr
    , scanr1

    -- ** Accumulating maps
    , mapAccumL
    , mapAccumR

    -- ** Generation and unfolding
    , repeat
    , replicate
    , cycle
    , iterate
    , unfoldr
    , unfoldrN

    -- * Substrings

    -- ** Breaking strings
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

    -- ** Breaking into many substrings
    -- $split
    , splitOn
    , split
    , chunksOf
    -- , breakSubstring

    -- ** Breaking into lines and words
    , lines
    , words
    , unlines
    , unwords

    -- * Predicates
    , isPrefixOf
    , isSuffixOf
    , isInfixOf

    -- ** View patterns
    , stripPrefix
    , stripSuffix
    , commonPrefixes

    -- * Searching
    , filter
    , find
    , elem
    , breakOnAll
    , partition

    -- , findSubstring

    -- * Indexing
    , index
    , count

    -- * Zipping and unzipping
    , zip
    , zipWith

    -- * Showing values
    , show

    -- -* Ordered text
    -- , sort
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

-- $fusion
--
-- Starting from @text-1.3@ fusion is no longer implicit,
-- and pipelines of transformations usually allocate intermediate 'Text' values.
-- Users, who observe significant changes to performances,
-- are encouraged to use fusion framework explicitly, employing
-- "Data.Text.Internal.Fusion" and "Data.Text.Internal.Fusion.Common".

-- $replacement
--
-- A 'Text' value is a sequence of Unicode scalar values, as defined
-- in
-- <http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=35 §3.9, definition D76 of the Unicode 5.2 standard >.
-- As such, a 'Text' cannot contain values in the range U+D800 to
-- U+DFFF inclusive. Haskell implementations admit all Unicode code
-- points
-- (<http://www.unicode.org/versions/Unicode5.2.0/ch03.pdf#page=13 §3.4, definition D10 >)
-- as 'Char' values, including code points from this invalid range.
-- This means that there are some 'Char' values
-- (corresponding to 'Data.Char.Surrogate' category) that are not valid
-- Unicode scalar values, and the functions in this module must handle
-- those cases.
--
-- Within this module, many functions construct a 'Text' from one or
-- more 'Char' values. Those functions will substitute 'Char' values
-- that are not valid Unicode scalar values with the replacement
-- character \"&#xfffd;\" (U+FFFD).  Functions that perform this
-- inspection and replacement are documented with the phrase
-- \"Performs replacement on invalid scalar values\". The functions replace
-- invalid scalar values, instead of dropping them, as a security
-- measure. For details, see
-- <http://unicode.org/reports/tr36/#Deletion_of_Noncharacters Unicode Technical Report 36, §3.5 >.)

-- $setup
-- >>> :set -package transformers
-- >>> import Control.Monad.Trans.State
-- >>> import Data.Text
-- >>> import qualified Data.Text as T
-- >>> :seti -XOverloadedStrings

instance Eq Text where
    (==) = equal
    {-# INLINE (==) #-}

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
-- This is not a mistake: on contrary to UTF-16 (https://github.com/haskell/text/pull/208),
-- lexicographic ordering of UTF-8 encoded strings matches lexicographic ordering
-- of underlying bytearrays, no decoding is needed.

instance Show Text where
    showsPrec p ps r = showsPrec p (unpack ps) r

instance Read Text where
    readsPrec p str = [(pack x,y) | (x,y) <- readsPrec p str]

-- | @since 1.2.2.0
instance Semigroup Text where
    (<>) = append

instance Monoid Text where
    mempty  = empty
    mappend = (<>)
    mconcat = concat

-- | Performs replacement on invalid scalar values:
--
-- >>> :set -XOverloadedStrings
-- >>> "\55555" :: Data.Text.Lazy.Text
-- "\65533"
instance IsString Text where
    fromString = pack

-- | Performs replacement on invalid scalar values:
--
-- >>> :set -XOverloadedLists
-- >>> ['\55555'] :: Data.Text.Lazy.Text
-- "\65533"
--
-- @since 1.2.0.0
instance Exts.IsList Text where
    type Item Text = Char
    fromList       = pack
    toList         = unpack

instance NFData Text where
    rnf Empty        = ()
    rnf (Chunk _ ts) = rnf ts

-- | @since 1.2.1.0
instance Binary Text where
    put t = do
      -- This needs to be in sync with the Binary instance for ByteString
      -- in the binary package.
      put (foldlChunks (\n c -> n + T.lengthWord8 c) 0 t)
      putBuilder (encodeUtf8Builder t)
    get   = do
      bs <- get
      case decodeUtf8' bs of
        P.Left exn -> P.fail (P.show exn)
        P.Right a -> P.return a

-- | This instance preserves data abstraction at the cost of inefficiency.
-- We omit reflection services for the sake of data abstraction.
--
-- This instance was created by copying the updated behavior of
-- @"Data.Text".@'Data.Text.Text'
instance Data Text where
  gfoldl f z txt = z pack `f` (unpack txt)
  toConstr _     = packConstr
  gunfold k z c  = case constrIndex c of
    1 -> k (z pack)
    _ -> error "Data.Text.Lazy.Text.gunfold"
  dataTypeOf _   = textDataType

-- | @since 1.2.4.0
instance TH.Lift Text where
  lift = TH.appE (TH.varE 'fromStrict) . TH.lift . toStrict
#if MIN_VERSION_template_haskell(2,17,0)
  liftTyped = TH.unsafeCodeCoerce . TH.lift
#elif MIN_VERSION_template_haskell(2,16,0)
  liftTyped = TH.unsafeTExpCoerce . TH.lift
#endif

-- | @since 1.2.2.0
instance PrintfArg Text where
  formatArg txt = formatString $ unpack txt

packConstr :: Constr
packConstr = mkConstr textDataType "pack" [] Prefix

textDataType :: DataType
textDataType = mkDataType "Data.Text.Lazy.Text" [packConstr]

-- | /O(n)/ Convert a 'String' into a 'Text'.
--
-- Performs replacement on invalid scalar values, so @'unpack' . 'pack'@ is not 'id':
--
-- >>> Data.Text.Lazy.unpack (Data.Text.Lazy.pack "\55555")
-- "\65533"
pack ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  String -> Text
pack = unstream . S.streamList . L.map safe
{-# INLINE [1] pack #-}

-- | /O(n)/ Convert a 'Text' into a 'String'.
unpack ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> String
unpack t = S.unstreamList (stream t)
{-# INLINE [1] unpack #-}

-- | /O(n)/ Convert a literal string into a Text.
unpackCString# :: Addr# -> Text
unpackCString# addr# = unstream (S.streamCString# addr#)
{-# NOINLINE unpackCString# #-}

{-# RULES "TEXT literal" forall a.
    unstream (S.streamList (L.map safe (GHC.unpackCString# a)))
      = unpackCString# a #-}

{-# RULES "TEXT literal UTF8" forall a.
    unstream (S.streamList (L.map safe (GHC.unpackCStringUtf8# a)))
      = unpackCString# a #-}

{-# RULES "LAZY TEXT empty literal"
    unstream (S.streamList (L.map safe []))
      = Empty #-}

{-# RULES "LAZY TEXT empty literal" forall a.
    unstream (S.streamList (L.map safe [a]))
      = Chunk (T.singleton a) Empty #-}

-- | /O(1)/ Convert a character into a Text.
-- Performs replacement on invalid scalar values.
singleton :: Char -> Text
singleton c = Chunk (T.singleton c) Empty
{-# INLINE [1] singleton #-}

-- | /O(c)/ Convert a list of strict 'T.Text's into a lazy 'Text'.
fromChunks :: [T.Text] -> Text
fromChunks cs = L.foldr chunk Empty cs

-- | /O(n)/ Convert a lazy 'Text' into a list of strict 'T.Text's.
toChunks :: Text -> [T.Text]
toChunks cs = foldrChunks (:) [] cs

-- | /O(n)/ Convert a lazy 'Text' into a strict 'T.Text'.
toStrict :: LazyText -> T.StrictText
toStrict t = T.concat (toChunks t)
{-# INLINE [1] toStrict #-}

-- | /O(c)/ Convert a strict 'T.Text' into a lazy 'Text'.
fromStrict :: T.StrictText -> LazyText
fromStrict t = chunk t Empty
{-# INLINE [1] fromStrict #-}

-- -----------------------------------------------------------------------------
-- * Basic functions

-- | /O(1)/ Adds a character to the front of a 'Text'.
cons :: Char -> Text -> Text
cons c t = Chunk (T.singleton c) t
{-# INLINE [1] cons #-}

infixr 5 `cons`

-- | /O(n)/ Adds a character to the end of a 'Text'.  This copies the
-- entire array in the process.
snoc :: Text -> Char -> Text
snoc t c = foldrChunks Chunk (singleton c) t
{-# INLINE [1] snoc #-}

-- | /O(n\/c)/ Appends one 'Text' to another.
append :: Text -> Text -> Text
append xs ys = foldrChunks Chunk ys xs
{-# INLINE [1] append #-}

-- | /O(1)/ Returns the first character and rest of a 'Text', or
-- 'Nothing' if empty.
uncons :: Text -> Maybe (Char, Text)
uncons Empty        = Nothing
uncons (Chunk t ts) = Just (T.unsafeHead t, ts')
  where ts' | T.compareLength t 1 == EQ = ts
            | otherwise                 = Chunk (T.unsafeTail t) ts
{-# INLINE uncons #-}

-- | /O(1)/ Returns the first character of a 'Text', which must be
-- non-empty. This is a partial function, consider using 'uncons' instead.
head :: HasCallStack => Text -> Char
head t = S.head (stream t)
{-# INLINE head #-}

-- | /O(1)/ Returns all characters after the head of a 'Text', which
-- must be non-empty. This is a partial function, consider using 'uncons' instead.
tail :: HasCallStack => Text -> Text
tail (Chunk t ts) = chunk (T.tail t) ts
tail Empty        = emptyError "tail"
{-# INLINE [1] tail #-}

-- | /O(n\/c)/ Returns all but the last character of a 'Text', which must
-- be non-empty. This is a partial function, consider using 'unsnoc' instead.
init :: HasCallStack => Text -> Text
init (Chunk t0 ts0) = go t0 ts0
    where go t (Chunk t' ts) = Chunk t (go t' ts)
          go t Empty         = chunk (T.init t) Empty
init Empty = emptyError "init"
{-# INLINE [1] init #-}

-- | /O(n\/c)/ Returns the 'init' and 'last' of a 'Text', or 'Nothing' if
-- empty.
--
-- * It is no faster than using 'init' and 'last'.
--
-- @since 1.2.3.0
unsnoc :: Text -> Maybe (Text, Char)
unsnoc Empty          = Nothing
unsnoc ts@(Chunk _ _) = Just (init ts, last ts)
{-# INLINE unsnoc #-}

-- | /O(1)/ Tests whether a 'Text' is empty or not.
null :: Text -> Bool
null Empty = True
null _     = False
{-# INLINE [1] null #-}

-- | Bidirectional pattern synonym for 'cons' (/O(n)/) and 'uncons' (/O(1)/),
-- to be used together with 'Empty'.
--
-- @since 2.1.2
pattern (:<) :: Char -> Text -> Text
pattern x :< xs <- (uncons -> Just (x, xs)) where
  (:<) = cons
infixr 5 :<
{-# COMPLETE Empty, (:<) #-}

-- | Bidirectional pattern synonym for 'snoc' (/O(n)/) and 'unsnoc' (/O(1)/)
-- to be used together with 'Empty'.
--
-- @since 2.1.2
pattern (:>) :: Text -> Char -> Text
pattern xs :> x <- (unsnoc -> Just (xs, x)) where
  (:>) = snoc
infixl 5 :>
{-# COMPLETE Empty, (:>) #-}

-- | /O(1)/ Tests whether a 'Text' contains exactly one character.
isSingleton :: Text -> Bool
isSingleton = S.isSingleton . stream
{-# INLINE isSingleton #-}

-- | /O(n\/c)/ Returns the last character of a 'Text', which must be
-- non-empty. This is a partial function, consider using 'unsnoc' instead.
last :: HasCallStack => Text -> Char
last Empty        = emptyError "last"
last (Chunk t ts) = go t ts
    where go _ (Chunk t' ts') = go t' ts'
          go t' Empty         = T.last t'
{-# INLINE [1] last #-}

-- | /O(n)/ Returns the number of characters in a 'Text'.
length :: Text -> Int64
length = foldlChunks go 0
    where
        go :: Int64 -> T.Text -> Int64
        go l t = l + intToInt64 (T.length t)
{-# INLINE [1] length #-}

{-# RULES
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
  #-}

-- | /O(min(n,c))/ Compare the count of characters in a 'Text' to a number.
--
-- @
-- 'compareLength' t c = 'P.compare' ('length' t) c
-- @
--
-- This function gives the same answer as comparing against the result
-- of 'length', but can short circuit if the count of characters is
-- greater than the number, and hence be more efficient.
compareLength :: Text -> Int64 -> Ordering
compareLength t c = S.compareLengthI (stream t) c
{-# INLINE [1] compareLength #-}

-- We don't apply those otherwise appealing length-to-compareLength
-- rewrite rules here, because they can change the strictness
-- properties of code.

-- | /O(n)/ 'map' @f@ @t@ is the 'Text' obtained by applying @f@ to
-- each element of @t@. Performs replacement on
-- invalid scalar values.
map :: (Char -> Char) -> Text -> Text
map f = foldrChunks (Chunk . mapNonEmpty f) Empty
{-# INLINE [1] map #-}

{-# RULES
"TEXT map/map -> map" forall f g t.
    map f (map g t) = map (f . safe . g) t
#-}

-- | /O(n)/ The 'intercalate' function takes a 'Text' and a list of
-- 'Text's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: Text -> [Text] -> Text
intercalate t = concat . L.intersperse t
{-# INLINE [1] intercalate #-}

-- | /O(n)/ The 'intersperse' function takes a character and places it
-- between the characters of a 'Text'. Performs
-- replacement on invalid scalar values.
intersperse :: Char -> Text -> Text
intersperse c t = unstream (S.intersperse (safe c) (stream t))
{-# INLINE [1] intersperse #-}

-- | /O(n)/ Left-justify a string to the given length, using the
-- specified fill character on the right. Performs
-- replacement on invalid scalar values.
--
-- Examples:
--
-- > justifyLeft 7 'x' "foo"    == "fooxxxx"
-- > justifyLeft 3 'x' "foobar" == "foobar"
justifyLeft :: Int64 -> Char -> Text -> Text
justifyLeft k c t
    | len >= k  = t
    | otherwise = t `append` replicateChunk (k-len) (T.singleton c)
  where len = length t
{-# INLINE [1] justifyLeft #-}

-- | /O(n)/ Right-justify a string to the given length, using the
-- specified fill character on the left.  Performs replacement on
-- invalid scalar values.
--
-- Examples:
--
-- > justifyRight 7 'x' "bar"    == "xxxxbar"
-- > justifyRight 3 'x' "foobar" == "foobar"
justifyRight :: Int64 -> Char -> Text -> Text
justifyRight k c t
    | len >= k  = t
    | otherwise = replicateChunk (k-len) (T.singleton c) `append` t
  where len = length t
{-# INLINE justifyRight #-}

-- | /O(n)/ Center a string to the given length, using the specified
-- fill character on either side.  Performs replacement on invalid
-- scalar values.
--
-- Examples:
--
-- > center 8 'x' "HS" = "xxxHSxxx"
center :: Int64 -> Char -> Text -> Text
center k c t
    | len >= k  = t
    | otherwise = replicateChunk l (T.singleton c) `append` t `append` replicateChunk r (T.singleton c)
  where len = length t
        d   = k - len
        r   = d `quot` 2
        l   = d - r
{-# INLINE center #-}

-- | /O(n)/ The 'transpose' function transposes the rows and columns
-- of its 'Text' argument.  Note that this function uses 'pack',
-- 'unpack', and the list version of transpose, and is thus not very
-- efficient.
transpose :: [Text] -> [Text]
transpose ts = L.map (\ss -> Chunk (T.pack ss) Empty)
                     (L.transpose (L.map unpack ts))
-- TODO: make this fast

-- | /O(n)/ 'reverse' @t@ returns the elements of @t@ in reverse order.
reverse ::
#if defined(ASSERTS)
  HasCallStack =>
#endif
  Text -> Text
reverse = rev Empty
  where rev a Empty        = a
        rev a (Chunk t ts) = rev (Chunk (reverseNonEmpty t) a) ts

-- | /O(m+n)/ Replace every non-overlapping occurrence of @needle@ in
-- @haystack@ with @replacement@.
--
-- This function behaves as though it was defined as follows:
--
-- @
-- replace needle replacement haystack =
--   'intercalate' replacement ('splitOn' needle haystack)
-- @
--
-- As this suggests, each occurrence is replaced exactly once.  So if
-- @needle@ occurs in @replacement@, that occurrence will /not/ itself
-- be replaced recursively:
--
-- > replace "oo" "foo" "oo" == "foo"
--
-- In cases where several instances of @needle@ overlap, only the
-- first one will be replaced:
--
-- > replace "ofo" "bar" "ofofo" == "barfo"
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
replace :: HasCallStack
        => Text
        -- ^ @needle@ to search for.  If this string is empty, an
        -- error will occur.
        -> Text
        -- ^ @replacement@ to replace @needle@ with.
        -> Text
        -- ^ @haystack@ in which to search.
        -> Text
replace s d = intercalate d . splitOn s
{-# INLINE replace #-}

-- ----------------------------------------------------------------------------
-- ** Case conversions (folds)

-- $case
--
-- With Unicode text, it is incorrect to use combinators like @map
-- toUpper@ to case convert each character of a string individually.
-- Instead, use the whole-string case conversion functions from this
-- module.  For correctness in different writing systems, these
-- functions may map one input character to two or three output
-- characters.

-- | /O(n)/ Convert a string to folded case.
--
-- This function is mainly useful for performing caseless (or case
-- insensitive) string comparisons.
--
-- A string @x@ is a caseless match for a string @y@ if and only if:
--
-- @toCaseFold x == toCaseFold y@
--
-- The result string may be longer than the input string, and may
-- differ from applying 'toLower' to the input string.  For instance,
-- the Armenian small ligature men now (U+FB13) is case folded to the
-- bigram men now (U+0574 U+0576), while the micro sign (U+00B5) is
-- case folded to the Greek small letter letter mu (U+03BC) instead of
-- itself.
toCaseFold :: Text -> Text
toCaseFold = foldrChunks (\chnk acc -> Chunk (toCaseFoldNonEmpty chnk) acc) Empty
{-# INLINE toCaseFold #-}

-- | /O(n)/ Convert a string to lower case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, the Latin capital letter I with dot above (U+0130) maps
-- to the sequence Latin small letter i (U+0069) followed by combining
-- dot above (U+0307).
toLower :: Text -> Text
toLower = foldrChunks (\chnk acc -> Chunk (toLowerNonEmpty chnk) acc) Empty
{-# INLINE toLower #-}

-- | /O(n)/ Convert a string to upper case, using simple case
-- conversion.
--
-- The result string may be longer than the input string.  For
-- instance, the German eszett (U+00DF) maps to the two-letter
-- sequence SS.
toUpper :: Text -> Text
toUpper = foldrChunks (\chnk acc -> Chunk (toUpperNonEmpty chnk) acc) Empty
{-# INLINE toUpper #-}


-- | /O(n)/ Convert a string to title case, using simple case
-- conversion.
--
-- The first letter (as determined by 'Data.Char.isLetter')
-- of the input is converted to title case, as is
-- every subsequent letter that immediately follows a non-letter.
-- Every letter that immediately follows another letter is converted
-- to lower case.
--
-- The result string may be longer than the input string. For example,
-- the Latin small ligature &#xfb02; (U+FB02) is converted to the
-- sequence Latin capital letter F (U+0046) followed by Latin small
-- letter l (U+006C).
--
-- This function is not idempotent.
-- Consider lower-case letter @ŉ@ (U+0149 LATIN SMALL LETTER N PRECEDED BY APOSTROPHE).
-- Then 'T.toTitle' @"ŉ"@ = @"ʼN"@: the first (and the only) letter of the input
-- is converted to title case, becoming two letters.
-- Now @ʼ@ (U+02BC MODIFIER LETTER APOSTROPHE) is a modifier letter
-- and as such is recognised as a letter by 'Data.Char.isLetter',
-- so 'T.toTitle' @"ʼN"@ = @"'n"@.
--
-- /Note/: this function does not take language or culture specific
-- rules into account. For instance, in English, different style
-- guides disagree on whether the book name \"The Hill of the Red
-- Fox\" is correctly title cased&#x2014;but this function will
-- capitalize /every/ word.
--
-- @since 1.0.0.0
toTitle :: Text -> Text
toTitle = foldrChunks (\chnk acc -> Chunk (T.toTitle chnk) acc) Empty
{-# INLINE toTitle #-}

-- | /O(n)/ 'foldl', applied to a binary operator, a starting value
-- (typically the left-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from left to right.
foldl :: (a -> Char -> a) -> a -> Text -> a
foldl f z t = S.foldl f z (stream t)
{-# INLINE foldl #-}

-- | /O(n)/ A strict version of 'foldl'.
--
foldl' :: (a -> Char -> a) -> a -> Text -> a
foldl' f z t = S.foldl' f z (stream t)
{-# INLINE foldl' #-}

-- | /O(n)/ A variant of 'foldl' that has no starting value argument,
-- and thus must be applied to a non-empty 'Text'.
foldl1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1 f t = S.foldl1 f (stream t)
{-# INLINE foldl1 #-}

-- | /O(n)/ A strict version of 'foldl1'.
foldl1' :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldl1' f t = S.foldl1' f (stream t)
{-# INLINE foldl1' #-}

-- | /O(n)/ A monadic version of 'foldl''.
--
-- @since 2.1.2
foldlM' :: Monad m => (a -> Char -> m a) -> a -> Text -> m a
foldlM' f z t = S.foldlM' f z (stream t)
{-# INLINE foldlM' #-}

-- | /O(n)/ 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a 'Text',
-- reduces the 'Text' using the binary operator, from right to left.
--
-- 'foldr' is lazy like 'Data.List.foldr' for lists: evaluation actually
-- traverses the 'Text' from left to right, only as far as it needs to.
--
-- For example, 'head' can be defined with /O(1)/ complexity using 'foldr':
--
-- @
-- head :: Text -> Char
-- head = foldr const (error "head empty")
-- @
foldr :: (Char -> a -> a) -> a -> Text -> a
foldr f z t = S.foldr f z (stream t)
{-# INLINE foldr #-}

-- | /O(n)/ A variant of 'foldr' that has no starting value argument,
-- and thus must be applied to a non-empty 'Text'.
foldr1 :: HasCallStack => (Char -> Char -> Char) -> Text -> Char
foldr1 f t = S.foldr1 f (stream t)
{-# INLINE foldr1 #-}

-- | /O(n)/ Concatenate a list of 'Text's.
concat :: [Text] -> Text
concat []                    = Empty
concat (Empty : css)         = concat css
concat (Chunk c Empty : css) = Chunk c (concat css)
concat (Chunk c cs : css)    = Chunk c (concat (cs : css))
{-# INLINE concat #-}

-- | /O(n)/ Map a function over a 'Text' that results in a 'Text', and
-- concatenate the results.
concatMap :: (Char -> Text) -> Text -> Text
concatMap f = concat . foldr ((:) . f) []
{-# INLINE concatMap #-}

-- | /O(n)/ 'any' @p@ @t@ determines whether any character in the
-- 'Text' @t@ satisfies the predicate @p@.
any :: (Char -> Bool) -> Text -> Bool
any p t = S.any p (stream t)
{-# INLINE any #-}

-- | /O(n)/ 'all' @p@ @t@ determines whether all characters in the
-- 'Text' @t@ satisfy the predicate @p@.
all :: (Char -> Bool) -> Text -> Bool
all p t = S.all p (stream t)
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'Text', which
-- must be non-empty.
maximum :: HasCallStack => Text -> Char
maximum t = S.maximum (stream t)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'Text', which
-- must be non-empty.
minimum :: HasCallStack => Text -> Char
minimum t = S.minimum (stream t)
{-# INLINE minimum #-}

-- | \O(n)\ Test whether 'Text' contains only ASCII code-points (i.e. only
--   U+0000 through U+007F).
--
-- This is a more efficient version of @'all' 'Data.Char.isAscii'@.
--
-- >>> isAscii ""
-- True
--
-- >>> isAscii "abc\NUL"
-- True
--
-- >>> isAscii "abcd€"
-- False
--
-- prop> isAscii t == all (< '\x80') t
--
-- @since 2.0.2
isAscii :: Text -> Bool
isAscii = foldrChunks (\chnk acc -> T.isAscii chnk && acc) True

-- | /O(n)/ 'scanl' is similar to 'foldl', but returns a list of
-- successive reduced values from the left.
-- Performs replacement on invalid scalar values.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > last (scanl f z xs) == foldl f z xs.
scanl :: (Char -> Char -> Char) -> Char -> Text -> Text
scanl f z t = unstream (S.scanl g z (stream t))
    where g a b = safe (f a b)
{-# INLINE scanl #-}

-- | /O(n)/ 'scanl1' is a variant of 'scanl' that has no starting
-- value argument.  Performs replacement on invalid scalar values.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Char -> Char -> Char) -> Text -> Text
scanl1 f t0 = case uncons t0 of
                Nothing -> empty
                Just (t,ts) -> scanl f t ts
{-# INLINE scanl1 #-}

-- | /O(n)/ 'scanr' is the right-to-left dual of 'scanl'.  Performs
-- replacement on invalid scalar values.
--
-- > scanr f v == reverse . scanl (flip f) v . reverse
scanr :: (Char -> Char -> Char) -> Char -> Text -> Text
scanr f v = reverse . scanl g v . reverse
    where g a b = safe (f b a)

-- | /O(n)/ 'scanr1' is a variant of 'scanr' that has no starting
-- value argument.  Performs replacement on invalid scalar values.
scanr1 :: (Char -> Char -> Char) -> Text -> Text
scanr1 f t | null t    = empty
           | otherwise = scanr f (last t) (init t)

-- | /O(n)/ Like a combination of 'map' and 'foldl''. Applies a
-- function to each element of a 'Text', passing an accumulating
-- parameter from left to right, and returns a final 'Text'.  Performs
-- replacement on invalid scalar values.
mapAccumL :: (a -> Char -> (a,Char)) -> a -> Text -> (a, Text)
mapAccumL f = go
  where
    go z (Chunk c cs)    = (z'', Chunk c' cs')
        where (z',  c')  = T.mapAccumL f z c
              (z'', cs') = go z' cs
    go z Empty           = (z, Empty)
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- a strict 'foldr'; it applies a function to each element of a
-- 'Text', passing an accumulating parameter from right to left, and
-- returning a final value of this accumulator together with the new
-- 'Text'.  Performs replacement on invalid scalar values.
mapAccumR :: (a -> Char -> (a,Char)) -> a -> Text -> (a, Text)
mapAccumR f = go
  where
    go z (Chunk c cs)   = (z'', Chunk c' cs')
        where (z'', c') = T.mapAccumR f z' c
              (z', cs') = go z cs
    go z Empty          = (z, Empty)
{-# INLINE mapAccumR #-}

-- | @'repeat' x@ is an infinite 'Text', with @x@ the value of every
-- element.
--
-- @since 1.2.0.5
repeat :: Char -> Text
repeat c = let t = Chunk (T.replicate smallChunkSize (T.singleton c)) t
            in t

-- | /O(n*m)/ 'replicate' @n@ @t@ is a 'Text' consisting of the input
-- @t@ repeated @n@ times.
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
{-# INLINE [1] replicate #-}

replicateChunk :: Int64 -> T.Text -> Text
replicateChunk !n !t@(T.Text _ _ len)
  | n <= 0 = Empty
  | otherwise = Chunk headChunk $ P.foldr Chunk Empty (L.genericReplicate q normalChunk)
  where
    perChunk = defaultChunkSize `quot` len
    normalChunk = T.replicate perChunk t
    (q, r) = n `P.quotRem` intToInt64 perChunk
    headChunk = T.replicate (int64ToInt r) t
{-# INLINE replicateChunk #-}

-- | 'cycle' ties a finite, non-empty 'Text' into a circular one, or
-- equivalently, the infinite repetition of the original 'Text'.
--
-- @since 1.2.0.5
cycle :: HasCallStack => Text -> Text
cycle Empty = emptyError "cycle"
cycle t     = let t' = foldrChunks Chunk t' t
               in t'

-- | @'iterate' f x@ returns an infinite 'Text' of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
--
-- @since 1.2.0.5
iterate :: (Char -> Char) -> Char -> Text
iterate f c = let t c' = Chunk (T.singleton c') (t (f c'))
               in t c

-- | /O(n)/, where @n@ is the length of the result. The 'unfoldr'
-- function is analogous to the List 'L.unfoldr'. 'unfoldr' builds a
-- 'Text' from a seed value. The function takes the element and
-- returns 'Nothing' if it is done producing the 'Text', otherwise
-- 'Just' @(a,b)@.  In this case, @a@ is the next 'Char' in the
-- string, and @b@ is the seed value for further production.
-- Performs replacement on invalid scalar values.
unfoldr :: (a -> Maybe (Char,a)) -> a -> Text
unfoldr f s = unstream (S.unfoldr (firstf safe . f) s)
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a 'Text' from a seed
-- value. However, the length of the result should be limited by the
-- first argument to 'unfoldrN'. This function is more efficient than
-- 'unfoldr' when the maximum length of the result is known and
-- correct, otherwise its performance is similar to 'unfoldr'.
-- Performs replacement on invalid scalar values.
unfoldrN :: Int64 -> (a -> Maybe (Char,a)) -> a -> Text
unfoldrN n f s = unstream (S.unfoldrN n (firstf safe . f) s)
{-# INLINE unfoldrN #-}

-- | /O(n)/ 'take' @n@, applied to a 'Text', returns the prefix of the
-- 'Text' of length @n@, or the 'Text' itself if @n@ is greater than
-- the length of the Text.
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
{-# INLINE [1] take #-}

-- | /O(n)/ 'takeEnd' @n@ @t@ returns the suffix remaining after
-- taking @n@ characters from the end of @t@.
--
-- Examples:
--
-- > takeEnd 3 "foobar" == "bar"
--
-- @since 1.1.1.0
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

-- | /O(n)/ 'drop' @n@, applied to a 'Text', returns the suffix of the
-- 'Text' after the first @n@ characters, or the empty 'Text' if @n@
-- is greater than the length of the 'Text'.
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
{-# INLINE [1] drop #-}

-- | /O(n)/ 'dropEnd' @n@ @t@ returns the prefix remaining after
-- dropping @n@ characters from the end of @t@.
--
-- Examples:
--
-- > dropEnd 3 "foobar" == "foo"
--
-- @since 1.1.1.0
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

-- | /O(n)/ 'dropWords' @n@ returns the suffix with @n@ 'Word8'
-- values dropped, or the empty 'Text' if @n@ is greater than the
-- number of 'Word8' values present.
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

-- | /O(n)/ 'takeWhile', applied to a predicate @p@ and a 'Text',
-- returns the longest prefix (possibly empty) of elements that
-- satisfy @p@.
takeWhile :: (Char -> Bool) -> Text -> Text
takeWhile p t0 = takeWhile' t0
  where takeWhile' Empty        = Empty
        takeWhile' (Chunk t ts) =
          case T.findIndex (not . p) t of
            Just n | n > 0     -> Chunk (T.take n t) Empty
                   | otherwise -> Empty
            Nothing            -> Chunk t (takeWhile' ts)
{-# INLINE [1] takeWhile #-}

-- | /O(n)/ 'takeWhileEnd', applied to a predicate @p@ and a 'Text',
-- returns the longest suffix (possibly empty) of elements that
-- satisfy @p@.
-- Examples:
--
-- > takeWhileEnd (=='o') "foo" == "oo"
--
-- @since 1.2.2.0
takeWhileEnd :: (Char -> Bool) -> Text -> Text
takeWhileEnd p = takeChunk empty . L.reverse . toChunks
  where takeChunk acc []     = acc
        takeChunk acc (t:ts)
          | T.lengthWord8 t' < T.lengthWord8 t
                             = chunk t' acc
          | otherwise        = takeChunk (Chunk t' acc) ts
          where t' = T.takeWhileEnd p t
{-# INLINE takeWhileEnd #-}

-- | /O(n)/ 'dropWhile' @p@ @t@ returns the suffix remaining after
-- 'takeWhile' @p@ @t@.
dropWhile :: (Char -> Bool) -> Text -> Text
dropWhile p t0 = dropWhile' t0
  where dropWhile' Empty        = Empty
        dropWhile' (Chunk t ts) =
          case T.findIndex (not . p) t of
            Just n  -> Chunk (T.drop n t) ts
            Nothing -> dropWhile' ts
{-# INLINE [1] dropWhile #-}

-- | /O(n)/ 'dropWhileEnd' @p@ @t@ returns the prefix remaining after
-- dropping characters that satisfy the predicate @p@ from the end of
-- @t@.
--
-- Examples:
--
-- > dropWhileEnd (=='.') "foo..." == "foo"
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
{-# INLINE dropWhileEnd #-}

-- | /O(n)/ 'dropAround' @p@ @t@ returns the substring remaining after
-- dropping characters that satisfy the predicate @p@ from both the
-- beginning and end of @t@.
dropAround :: (Char -> Bool) -> Text -> Text
dropAround p = dropWhile p . dropWhileEnd p
{-# INLINE [1] dropAround #-}

-- | /O(n)/ Remove leading white space from a string.  Equivalent to:
--
-- > dropWhile isSpace
stripStart :: Text -> Text
stripStart = dropWhile isSpace
{-# INLINE stripStart #-}

-- | /O(n)/ Remove trailing white space from a string.  Equivalent to:
--
-- > dropWhileEnd isSpace
stripEnd :: Text -> Text
stripEnd = dropWhileEnd isSpace
{-# INLINE [1] stripEnd #-}

-- | /O(n)/ Remove leading and trailing white space from a string.
-- Equivalent to:
--
-- > dropAround isSpace
strip :: Text -> Text
strip = dropAround isSpace
{-# INLINE [1] strip #-}

-- | /O(n)/ 'splitAt' @n t@ returns a pair whose first element is a
-- prefix of @t@ of length @n@, and whose second is the remainder of
-- the string. It is equivalent to @('take' n t, 'drop' n t)@.
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

-- | /O(n)/ 'splitAtWord' @n t@ returns a strict pair whose first
-- element is a prefix of @t@ whose chunks contain @n@ 'Word8'
-- values, and whose second is the remainder of the string.
splitAtWord :: Int64 -> Text -> PairS Text Text
splitAtWord !_ Empty = empty :*: empty
splitAtWord x (Chunk c@(T.Text arr off len) cs)
    | y >= len  = let h :*: t = splitAtWord (x-intToInt64 len) cs
                  in  Chunk c h :*: t
    | otherwise = chunk (text arr off y) empty :*:
                  chunk (text arr (off+y) (len-y)) cs
    where y = int64ToInt x

-- | /O(n+m)/ Find the first instance of @needle@ (which must be
-- non-'null') in @haystack@.  The first element of the returned tuple
-- is the prefix of @haystack@ before @needle@ is matched.  The second
-- is the remainder of @haystack@, starting with the match.
--
-- Examples:
--
-- > breakOn "::" "a::b::c" ==> ("a", "::b::c")
-- > breakOn "/" "foobar"   ==> ("foobar", "")
--
-- Laws:
--
-- > append prefix match == haystack
-- >   where (prefix, match) = breakOn needle haystack
--
-- If you need to break a string by a substring repeatedly (e.g. you
-- want to break on every instance of a substring), use 'breakOnAll'
-- instead, as it has lower startup overhead.
--
-- This function is strict in its first argument, and lazy in its
-- second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
breakOn :: HasCallStack => Text -> Text -> (Text, Text)
breakOn pat src
    | null pat  = emptyError "breakOn"
    | otherwise = case indices pat src of
                    []    -> (src, empty)
                    (x:_) -> let h :*: t = splitAtWord x src
                             in  (h, t)

-- | /O(n+m)/ Similar to 'breakOn', but searches from the end of the string.
--
-- The first element of the returned tuple is the prefix of @haystack@
-- up to and including the last match of @needle@.  The second is the
-- remainder of @haystack@, following the match.
--
-- > breakOnEnd "::" "a::b::c" ==> ("a::b::", "c")
breakOnEnd :: HasCallStack => Text -> Text -> (Text, Text)
breakOnEnd pat src = let (a,b) = breakOn (reverse pat) (reverse src)
                   in  (reverse b, reverse a)
{-# INLINE breakOnEnd #-}

-- | /O(n+m)/ Find all non-overlapping instances of @needle@ in
-- @haystack@.  Each element of the returned list consists of a pair:
--
-- * The entire string prior to the /k/th match (i.e. the prefix)
--
-- * The /k/th match, followed by the remainder of the string
--
-- Examples:
--
-- > breakOnAll "::" ""
-- > ==> []
-- > breakOnAll "/" "a/b/c/"
-- > ==> [("a", "/b/c/"), ("a/b", "/c/"), ("a/b/c", "/")]
--
-- This function is strict in its first argument, and lazy in its
-- second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
--
-- The @needle@ parameter may not be empty.
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

-- | /O(n)/ 'break' is like 'span', but the prefix returned is over
-- elements that fail the predicate @p@.
--
-- >>> T.break (=='c') "180cm"
-- ("180","cm")
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

-- | /O(n)/ 'span', applied to a predicate @p@ and text @t@, returns
-- a pair whose first element is the longest prefix (possibly empty)
-- of @t@ of elements that satisfy @p@, and whose second is the
-- remainder of the text.
--
-- >>> T.span (=='0') "000AB"
-- ("000","AB")
span :: (Char -> Bool) -> Text -> (Text, Text)
span p = break (not . p)
{-# INLINE span #-}

-- | /O(length of prefix)/ 'spanM', applied to a monadic predicate @p@,
-- a text @t@, returns a pair @(t1, t2)@ where @t1@ is the longest prefix of
-- @t@ whose elements satisfy @p@, and @t2@ is the remainder of the text.
--
-- >>> T.spanM (\c -> state $ \i -> (fromEnum c == i, i+1)) "abcefg" `runState` 97
-- (("abc","efg"),101)
--
-- 'span' is 'spanM' specialized to 'Data.Functor.Identity.Identity':
--
-- @
-- -- for all p :: Char -> Bool
-- 'span' p = 'Data.Functor.Identity.runIdentity' . 'spanM' ('pure' . p)
-- @
--
-- @since 2.0.1
spanM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanM p t0 = go t0
  where
    go Empty = pure (empty, empty)
    go (Chunk t ts) = do
        (t1, t2) <- T.spanM p t
        if T.null t2 then first (chunk t) <$> go ts
        else pure (chunk t1 empty, Chunk t2 ts)
{-# INLINE spanM #-}

-- | /O(length of suffix)/ 'spanEndM', applied to a monadic predicate @p@,
-- a text @t@, returns a pair @(t1, t2)@ where @t2@ is the longest suffix of
-- @t@ whose elements satisfy @p@, and @t1@ is the remainder of the text.
--
-- >>> T.spanEndM (\c -> state $ \i -> (fromEnum c == i, i-1)) "tuvxyz" `runState` 122
-- (("tuv","xyz"),118)
--
-- @
-- 'spanEndM' p . 'reverse' = fmap ('Data.Bifunctor.bimap' 'reverse' 'reverse') . 'spanM' p
-- @
--
-- @since 2.0.1
spanEndM :: Monad m => (Char -> m Bool) -> Text -> m (Text, Text)
spanEndM p t0 = go t0
  where
    go Empty = pure (empty, empty)
    go (Chunk t ts) = do
        (t3, t4) <- go ts
        if null t3 then (\(t1, t2) -> (chunk t1 empty, chunk t2 ts)) <$> T.spanEndM p t
        else pure (Chunk t t3, t4)
{-# INLINE spanEndM #-}

-- | The 'group' function takes a 'Text' and returns a list of 'Text's
-- such that the concatenation of the result is equal to the argument.
-- Moreover, each sublist in the result contains only equal elements.
-- For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test.
group :: Text -> [Text]
group =  groupBy (==)
{-# INLINE group #-}

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Char -> Char -> Bool) -> Text -> [Text]
groupBy _  Empty        = []
groupBy eq (Chunk t ts) = cons x ys : groupBy eq zs
                          where (ys,zs) = span (eq x) xs
                                x  = T.unsafeHead t
                                xs = chunk (T.unsafeTail t) ts

-- | /O(n²)/ Return all initial segments of the given 'Text',
-- shortest first.
inits :: Text -> [Text]
inits = (NE.toList P.$!) . initsNE

-- | /O(n²)/ Return all initial segments of the given 'Text',
-- shortest first.
--
-- @since 2.1.2
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

-- | /O(n)/ Return all final segments of the given 'Text', longest
-- first.
tails :: Text -> [Text]
tails = (NE.toList P.$!) . tailsNE

-- | /O(n)/ Return all final segments of the given 'Text', longest
-- first.
--
-- @since 2.1.2
tailsNE :: Text -> NonEmpty Text
tailsNE Empty = Empty :| []
tailsNE ts@(Chunk t ts')
  | T.length t == 1 = ts :| tails ts'
  | otherwise       = ts :| tails (Chunk (T.unsafeTail t) ts')

-- $split
--
-- Splitting functions in this library do not perform character-wise
-- copies to create substrings; they just construct new 'Text's that
-- are slices of the original.

-- | /O(m+n)/ Break a 'Text' into pieces separated by the first 'Text'
-- argument (which cannot be an empty string), consuming the
-- delimiter. An empty delimiter is invalid, and will cause an error
-- to be raised.
--
-- Examples:
--
-- > splitOn "\r\n" "a\r\nb\r\nd\r\ne" == ["a","b","d","e"]
-- > splitOn "aaa"  "aaaXaaaXaaaXaaa"  == ["","X","X","X",""]
-- > splitOn "x"    "x"                == ["",""]
--
-- and
--
-- > intercalate s . splitOn s         == id
-- > splitOn (singleton c)             == split (==c)
--
-- (Note: the string @s@ to split on above cannot be empty.)
--
-- This function is strict in its first argument, and lazy in its
-- second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
splitOn :: HasCallStack
        => Text
        -- ^ String to split on. If this string is empty, an error
        -- will occur.
        -> Text
        -- ^ Input text.
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
{-# INLINE [1] splitOn #-}

{-# RULES
"LAZY TEXT splitOn/singleton -> split/==" [~1] forall c t.
    splitOn (singleton c) t = split (==c) t
  #-}

-- | /O(n)/ Splits a 'Text' into components delimited by separators,
-- where the predicate returns True for a separator element.  The
-- resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > split (=='a') "aabbaca" == ["","","bb","c",""]
-- > split (=='a') []        == [""]
split :: (Char -> Bool) -> Text -> [Text]
split _ Empty = [Empty]
split p (Chunk t0 ts0) = comb [] (T.split p t0) ts0
  where comb acc (s:[]) Empty        = revChunks (s:acc) : []
        comb acc (s:[]) (Chunk t ts) = comb (s:acc) (T.split p t) ts
        comb acc (s:ss) ts           = revChunks (s:acc) : comb [] ss ts
        comb _   []     _            = impossibleError "split"
{-# INLINE split #-}

-- | /O(n)/ Splits a 'Text' into components of length @k@.  The last
-- element may be shorter than the other chunks, depending on the
-- length of the input. Examples:
--
-- > chunksOf 3 "foobarbaz"   == ["foo","bar","baz"]
-- > chunksOf 4 "haskell.org" == ["hask","ell.","org"]
chunksOf :: Int64 -> Text -> [Text]
chunksOf k = go
  where
    go t = case splitAt k t of
             (a,b) | null a    -> []
                   | otherwise -> a : go b
{-# INLINE chunksOf #-}

-- | /O(n)/ Breaks a 'Text' up into a list of 'Text's at newline characters
-- @'\\n'@ (LF, line feed). The resulting strings do not contain newlines.
--
-- 'lines' __does not__ treat @'\\r'@ (CR, carriage return) as a newline character.
lines :: Text -> [Text]
lines Empty = []
lines t = NE.toList $ go t
  where
    go :: Text -> NonEmpty Text
    go Empty = Empty :| []
    go (Chunk x xs)
      -- x is non-empty, so T.lines x is non-empty as well
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

-- | /O(n)/ Breaks a 'Text' up into a list of words, delimited by 'Char's
-- representing white space.
words :: Text -> [Text]
words = L.filter (not . null) . split isSpace
{-# INLINE words #-}

-- | /O(n)/ Joins lines, after appending a terminating newline to
-- each.
unlines :: [Text] -> Text
unlines = concat . L.foldr (\t acc -> t : singleton '\n' : acc) []
{-# INLINE unlines #-}

-- | /O(n)/ Joins words using single space characters.
unwords :: [Text] -> Text
unwords = intercalate (singleton ' ')
{-# INLINE unwords #-}

-- | /O(n)/ The 'isPrefixOf' function takes two 'Text's and returns
-- 'True' if and only if the first is a prefix of the second.
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

-- | /O(n)/ The 'isSuffixOf' function takes two 'Text's and returns
-- 'True' if and only if the first is a suffix of the second.
isSuffixOf :: Text -> Text -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y
{-# INLINE isSuffixOf #-}
-- TODO: a better implementation

-- | /O(n+m)/ The 'isInfixOf' function takes two 'Text's and returns
-- 'True' if and only if the first is contained, wholly and intact, anywhere
-- within the second.
--
-- This function is strict in its first argument, and lazy in its
-- second.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
isInfixOf :: Text -> Text -> Bool
isInfixOf needle haystack
    | null needle        = True
    | isSingleton needle = S.elem (head needle) . S.stream $ haystack
    | otherwise          = not . L.null . indices needle $ haystack
{-# INLINE [1] isInfixOf #-}

-------------------------------------------------------------------------------
-- * View patterns

-- | /O(n)/ Return the suffix of the second string if its prefix
-- matches the entire first string.
--
-- Examples:
--
-- > stripPrefix "foo" "foobar" == Just "bar"
-- > stripPrefix ""    "baz"    == Just "baz"
-- > stripPrefix "foo" "quux"   == Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text.Lazy as T
-- >
-- > fnordLength :: Text -> Int
-- > fnordLength (stripPrefix "fnord" -> Just suf) = T.length suf
-- > fnordLength _                                 = -1
stripPrefix :: Text -> Text -> Maybe Text
stripPrefix p t
    | null p    = Just t
    | otherwise = case commonPrefixes p t of
                    Just (_,c,r) | null c -> Just r
                    _                     -> Nothing

-- | /O(n)/ Find the longest non-empty common prefix of two strings
-- and return it, along with the suffixes of each string at which they
-- no longer match.
--
-- If the strings do not have a common prefix or either one is empty,
-- this function returns 'Nothing'.
--
-- Examples:
--
-- > commonPrefixes "foobar" "fooquux" == Just ("foo","bar","quux")
-- > commonPrefixes "veeble" "fetzer"  == Nothing
-- > commonPrefixes "" "baz"           == Nothing
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

-- | /O(n)/ Return the prefix of the second string if its suffix
-- matches the entire first string.
--
-- Examples:
--
-- > stripSuffix "bar" "foobar" == Just "foo"
-- > stripSuffix ""    "baz"    == Just "baz"
-- > stripSuffix "foo" "quux"   == Nothing
--
-- This is particularly useful with the @ViewPatterns@ extension to
-- GHC, as follows:
--
-- > {-# LANGUAGE ViewPatterns #-}
-- > import Data.Text.Lazy as T
-- >
-- > quuxLength :: Text -> Int
-- > quuxLength (stripSuffix "quux" -> Just pre) = T.length pre
-- > quuxLength _                                = -1
stripSuffix :: Text -> Text -> Maybe Text
stripSuffix p t = reverse `fmap` stripPrefix (reverse p) (reverse t)

-- | /O(n)/ 'filter', applied to a predicate and a 'Text',
-- returns a 'Text' containing those characters that satisfy the
-- predicate.
filter :: (Char -> Bool) -> Text -> Text
filter p = foldrChunks (chunk . filter_ T.Text p) Empty
{-# INLINE [1] filter #-}

{-# RULES
"TEXT filter/filter -> filter" forall p q t.
    filter p (filter q t) = filter (\c -> q c && p c) t
#-}

-- | /O(n)/ The 'find' function takes a predicate and a 'Text', and
-- returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
find :: (Char -> Bool) -> Text -> Maybe Char
find p t = S.findBy p (stream t)
{-# INLINE find #-}

-- | /O(n)/ The 'elem' function takes a character and a 'Text', and
-- returns 'True' if the element is found in the given 'Text', or
-- 'False' otherwise.
elem :: Char -> Text -> Bool
elem c t = S.any (== c) (stream t)
{-# INLINE elem #-}

-- | /O(n)/ The 'partition' function takes a predicate and a 'Text',
-- and returns the pair of 'Text's with elements which do and do not
-- satisfy the predicate, respectively; i.e.
--
-- > partition p t == (filter p t, filter (not . p) t)
partition :: (Char -> Bool) -> Text -> (Text, Text)
partition p t = (filter p t, filter (not . p) t)
{-# INLINE partition #-}

-- | /O(n)/ 'Text' index (subscript) operator, starting from 0.
index :: HasCallStack => Text -> Int64 -> Char
index t n = S.index (stream t) n
{-# INLINE index #-}

-- | /O(n+m)/ The 'count' function returns the number of times the
-- query string appears in the given 'Text'. An empty query string is
-- invalid, and will cause an error to be raised.
--
-- In (unlikely) bad cases, this function's time complexity degrades
-- towards /O(n*m)/.
count :: HasCallStack => Text -> Text -> Int64
count pat
    | null pat        = emptyError "count"
    | otherwise       = go 0  . indices pat
  where go !n []     = n
        go !n (_:xs) = go (n+1) xs
{-# INLINE [1] count #-}

{-# RULES
"LAZY TEXT count/singleton -> countChar" [~1] forall c t.
    count (singleton c) t = countChar c t
  #-}

-- | /O(n)/ The 'countChar' function returns the number of times the
-- query element appears in the given 'Text'.
countChar :: Char -> Text -> Int64
countChar c t = S.countChar c (stream t)

-- | /O(n)/ 'zip' takes two 'Text's and returns a list of
-- corresponding pairs of bytes. If one input 'Text' is short,
-- excess elements of the longer 'Text' are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: Text -> Text -> [(Char,Char)]
zip a b = S.unstreamList $ S.zipWith (,) (stream a) (stream b)
{-# INLINE [0] zip #-}

-- | /O(n)/ 'zipWith' generalises 'zip' by zipping with the function
-- given as the first argument, instead of a tupling function.
-- Performs replacement on invalid scalar values.
zipWith :: (Char -> Char -> Char) -> Text -> Text -> Text
zipWith f t1 t2 = unstream (S.zipWith g (stream t1) (stream t2))
    where g a b = safe (f a b)
{-# INLINE [0] zipWith #-}

-- | Convert a value to lazy 'Text'.
--
-- @since 2.1.2
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
    -- * Initialisation
    withSocketsDo,

    -- * Address information
    getAddrInfo,

    -- ** Types
    HostName,
    ServiceName,
    AddrInfo (..),
    defaultHints,

    -- ** Flags
    AddrInfoFlag (..),
    addrInfoFlagImplemented,

    -- * Socket operations
    connect,
    bind,
    listen,
    accept,

    -- ** Closing
    close,
    close',
    gracefulClose,
    shutdown,
    ShutdownCmd (..),

    -- * Socket options
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
    -- ** General socket options
    StructLinger (..),
    SocketTimeout (..),
    getSockOpt,
    setSockOpt,
    -- ** Integrated socket options
    SockOptValue (..),
    setSockOptValue,

    -- * Socket
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

    -- ** Types of Socket
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

    -- ** Family
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

    -- ** Protocol number
    ProtocolNumber,
    defaultProtocol,

    -- * Basic socket address type
    SockAddr (..),
    isSupportedSockAddr,
    getPeerName,
    getSocketName,

    -- ** Host address
    HostAddress,
    hostAddressToTuple,
    tupleToHostAddress,

    -- ** Host address6
    HostAddress6,
    hostAddress6ToTuple,
    tupleToHostAddress6,

    -- ** Flow Info
    FlowInfo,

    -- ** Scope ID
    ScopeID,
    ifNameToIndex,
    ifIndexToName,

    -- ** Port number
    PortNumber,
    defaultPort,
    socketPortSafe,
    socketPort,

    -- * UNIX-domain socket
    isUnixDomainSocketAvailable,
    socketPair,
    sendFd,
    recvFd,
    getPeerCredential,

    -- * Name information
    getNameInfo,
    NameInfoFlag (..),

    -- * Low level

    -- ** socket operations
    setCloseOnExecIfNeeded,
    getCloseOnExec,
    setNonBlockIfNeeded,
    getNonBlock,

    -- ** Sending and receiving data
    sendBuf,
    recvBuf,
    sendBufTo,
    recvBufFrom,

    -- ** Advanced IO
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

    -- ** Control message (ancillary data)
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

    -- ** APIs for control message
    lookupCmsg,
    filterCmsg,

    -- ** Class and types for control message
    ControlMessage (..),
    IPv4TTL (..),
    IPv6HopLimit (..),
    IPv4TOS (..),
    IPv6TClass (..),
    IPv4PktInfo (..),
    IPv6PktInfo (..),

    -- * Special constants
    maxListenQueue,

    -- * STM to check read and write
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

-- |
-- Module      : Network.Socket.ByteString
-- Copyright   : (c) Johan Tibell 2007-2010
-- License     : BSD-style
--
-- Maintainer  : johan.tibell@gmail.com
-- Stability   : stable
-- Portability : portable
--
-- This module provides access to the BSD /socket/ interface. For detailed
-- documentation, consult your favorite POSIX socket reference. All functions
-- communicate failures by converting the error number to an
-- 'System.IO.Error.IOError'.
--
-- This module is made to be imported with "Network.Socket" like so:
--
-- > import Network.Socket
-- > import Network.Socket.ByteString
module Network.Socket.ByteString (
    -- * Send data to a socket
    send,
    sendAll,
    sendTo,
    sendAllTo,

    -- ** Vectored I/O
    -- $vectored
    sendMany,
    sendManyTo,
    sendManyWithFds,

    -- * Receive data from a socket
    recv,
    recvFrom,

    -- * Advanced send and recv
    sendMsg,
    recvMsg,
) where

import Data.ByteString (ByteString)

import Network.Socket.ByteString.IO hiding (recvFrom, sendAllTo, sendTo)
import qualified Network.Socket.ByteString.IO as G
import Network.Socket.Types

-- ----------------------------------------------------------------------------

-- ** Vectored I/O

-- $vectored
--
-- Vectored I\/O, also known as scatter\/gather I\/O, allows multiple
-- data segments to be sent using a single system call, without first
-- concatenating the segments.  For example, given a list of
-- @ByteString@s, @xs@,
--
-- > sendMany sock xs
--
-- is equivalent to
--
-- > sendAll sock (concat xs)
--
-- but potentially more efficient.
--
-- Vectored I\/O are often useful when implementing network protocols
-- that, for example, group data into segments consisting of one or
-- more fixed-length headers followed by a variable-length body.

-- | Send data to the socket.  The recipient can be specified
-- explicitly, so the socket need not be in a connected state.
-- Returns the number of bytes sent. Applications are responsible for
-- ensuring that all data has been sent.
sendTo :: Socket -> ByteString -> SockAddr -> IO Int
sendTo = G.sendTo

-- | Send data to the socket. The recipient can be specified
-- explicitly, so the socket need not be in a connected state.  Unlike
-- 'sendTo', this function continues to send data until either all
-- data has been sent or an error occurs.  On error, an exception is
-- raised, and there is no way to determine how much data, if any, was
-- successfully sent.
sendAllTo :: Socket -> ByteString -> SockAddr -> IO ()
sendAllTo = G.sendAllTo

-- | Receive data from the socket.  The socket need not be in a
-- connected state.  Returns @(bytes, address)@ where @bytes@ is a
-- 'ByteString' representing the data received and @address@ is a
-- 'SockAddr' representing the address of the sending socket.
recvFrom :: Socket -> Int -> IO (ByteString, SockAddr)
recvFrom = G.recvFrom

{-# LANGUAGE CPP #-}

-- |
-- Module      : Network.Socket.ByteString.Lazy
-- Copyright   : (c) Bryan O'Sullivan 2009
-- License     : BSD-style
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : POSIX, GHC
--
-- This module provides access to the BSD /socket/ interface.  For detailed
-- documentation, consult your favorite POSIX socket reference. All functions
-- communicate failures by converting the error number to an
-- 'System.IO.Error.IOError'.
--
-- This module is made to be imported with "Network.Socket" like so:
--
-- > import Network.Socket
-- > import Network.Socket.ByteString.Lazy
-- > import Prelude hiding (getContents)
module Network.Socket.ByteString.Lazy (
    -- * Send data to a socket
    send,
    sendAll,
    sendWithFds,

    -- * Receive data from a socket
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

-- | Send data and file descriptors over a UNIX-domain socket in
--   a single system call. This function does not work on Windows.
sendWithFds
    :: Socket
    -- ^ Socket
    -> ByteString
    -- ^ Data to send
    -> [Fd]
    -- ^ File descriptors
    -> IO ()
sendWithFds s lbs fds = N.sendManyWithFds s (L.toChunks lbs) fds

-- -----------------------------------------------------------------------------
-- Receiving

-- | Receive data from the socket.  The socket must be in a connected
-- state.  Data is received on demand, in chunks; each chunk will be
-- sized to reflect the amount of data received by individual 'recv'
-- calls.
--
-- All remaining data from the socket is consumed.  When there is no
-- more data to be received, the receiving side of the socket is shut
-- down.  If there is an error and an exception is thrown, the socket
-- is not shut down.
getContents
    :: Socket
    -- ^ Connected socket
    -> IO ByteString
    -- ^ Data received
getContents s = loop
  where
    loop = unsafeInterleaveIO $ do
        sbs <- N.recv s defaultChunkSize
        if S.null sbs
            then do
                shutdown s ShutdownReceive `catchIOError` const (return ())
                return Empty
            else Chunk sbs <$> loop

-- | Receive data from the socket.  The socket must be in a connected
-- state.  This function may return fewer bytes than specified.  If
-- the received data is longer than the specified length, it may be
-- discarded depending on the type of socket.  This function may block
-- until a message arrives.
--
-- If there is no more data to be received, returns an empty 'ByteString'.
recv
    :: Socket
    -- ^ Connected socket
    -> Int64
    -- ^ Maximum number of bytes to receive
    -> IO ByteString
    -- ^ Data received
recv s nbytes = chunk <$> N.recv s (fromIntegral nbytes)
  where
    chunk k
        | S.null k = Empty
        | otherwise = Chunk k Empty

        {-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif
{- |
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
-}


#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
module System.FilePath(
    -- * Separator predicates
    FilePath,
    pathSeparator, pathSeparators, isPathSeparator,
    searchPathSeparator, isSearchPathSeparator,
    extSeparator, isExtSeparator,

    -- * @$PATH@ methods
    splitSearchPath, getSearchPath,

    -- * Extension functions
    splitExtension,
    takeExtension, replaceExtension, (-<.>), dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, takeExtensions, replaceExtensions, isExtensionOf,
    stripExtension,

    -- * Filename\/directory functions
    splitFileName,
    takeFileName, replaceFileName, dropFileName,
    takeBaseName, replaceBaseName,
    takeDirectory, replaceDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,

    -- * Drive functions
    splitDrive, joinDrive,
    takeDrive, hasDrive, dropDrive, isDrive,

    -- * Trailing slash functions
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,

    -- * File name manipulations
    normalise, equalFilePath,
    makeRelative,
    isRelative, isAbsolute,
    isValid, makeValid
) where
import System.FilePath.Windows
#else
module System.FilePath(
    -- * Separator predicates
    FilePath,
    pathSeparator, pathSeparators, isPathSeparator,
    searchPathSeparator, isSearchPathSeparator,
    extSeparator, isExtSeparator,

    -- * @$PATH@ methods
    splitSearchPath, getSearchPath,

    -- * Extension functions
    splitExtension,
    takeExtension, replaceExtension, (-<.>), dropExtension, addExtension, hasExtension, (<.>),
    splitExtensions, dropExtensions, takeExtensions, replaceExtensions, isExtensionOf,
    stripExtension,

    -- * Filename\/directory functions
    splitFileName,
    takeFileName, replaceFileName, dropFileName,
    takeBaseName, replaceBaseName,
    takeDirectory, replaceDirectory,
    combine, (</>),
    splitPath, joinPath, splitDirectories,

    -- * Drive functions
    splitDrive, joinDrive,
    takeDrive, hasDrive, dropDrive, isDrive,

    -- * Trailing slash functions
    hasTrailingPathSeparator,
    addTrailingPathSeparator,
    dropTrailingPathSeparator,

    -- * File name manipulations
    normalise, equalFilePath,
    makeRelative,
    isRelative, isAbsolute,
    isValid, makeValid
) where
import System.FilePath.Posix
#endif

{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}  -- needed to quote a view pattern

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



-- | Partial unicode friendly encoding.
--
-- On windows this encodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this encodes as UTF8 (strictly), which is a good guess.
--
-- Throws an 'EncodingException' if encoding fails. If the input does not
-- contain surrogate chars, you can use 'unsafeEncodeUtf'.
encodeUtf :: MonadThrow m => FilePath -> m OsPath
encodeUtf = OS.encodeUtf

-- | Unsafe unicode friendly encoding.
--
-- Like 'encodeUtf', except it crashes when the input contains
-- surrogate chars. For sanitized input, this can be useful.
unsafeEncodeUtf :: HasCallStack => String -> OsString
unsafeEncodeUtf = OS.unsafeEncodeUtf

-- | Encode a 'FilePath' with the specified encoding.
--
-- Note: on windows, we expect a "wide char" encoding (e.g. UCS-2 or UTF-16). Anything
-- that works with @Word16@ boundaries. Picking an incompatible encoding may crash
-- filepath operations.
encodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding (wide char)
           -> FilePath
           -> Either EncodingException OsPath
encodeWith = OS.encodeWith

-- | Like 'encodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
encodeFS :: FilePath -> IO OsPath
encodeFS = OS.encodeFS


-- | Partial unicode friendly decoding.
--
-- On windows this decodes as UTF16-LE (strictly), which is a pretty good guess.
-- On unix this decodes as UTF8 (strictly), which is a good guess.
--
-- Throws a 'EncodingException' if decoding fails.
decodeUtf :: MonadThrow m => OsPath -> m FilePath
decodeUtf = OS.decodeUtf

-- | Decode an 'OsPath' with the specified encoding.
decodeWith :: TextEncoding  -- ^ unix text encoding
           -> TextEncoding  -- ^ windows text encoding
           -> OsPath
           -> Either EncodingException FilePath
decodeWith = OS.decodeWith

-- | Like 'decodeUtf', except this mimics the behavior of the base library when doing filesystem
-- operations, which is:
--
-- 1. on unix, uses shady PEP 383 style encoding (based on the current locale,
--    but PEP 383 only works properly on UTF-8 encodings, so good luck)
-- 2. on windows does permissive UTF-16 encoding, where coding errors generate
--    Chars in the surrogate range
--
-- Looking up the locale requires IO. If you're not worried about calls
-- to 'setFileSystemEncoding', then 'unsafePerformIO' may be feasible (make sure
-- to deeply evaluate the result to catch exceptions).
decodeFS :: OsPath -> IO FilePath
decodeFS = OS.decodeFS


-- | Constructs an @OsPath@ from a ByteString.
--
-- On windows, this ensures valid UCS-2LE, on unix it is passed unchanged/unchecked.
--
-- Throws 'EncodingException' on invalid UCS-2LE on windows (although unlikely).
fromBytes :: MonadThrow m
          => ByteString
          -> m OsPath
fromBytes = OS.fromBytes



-- | QuasiQuote an 'OsPath'. This accepts Unicode characters
-- and encodes as UTF-8 on unix and UTF-16LE on windows. Runs 'isValid'
-- on the input. If used as a pattern, requires turning on the @ViewPatterns@
-- extension.
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


-- | Unpack an 'OsPath' to a list of 'OsChar'.
unpack :: OsPath -> [OsChar]
unpack = OS.unpack


-- | Pack a list of 'OsChar' to an 'OsPath'.
--
-- Note that using this in conjunction with 'unsafeFromChar' to
-- convert from @[Char]@ to 'OsPath' is probably not what
-- you want, because it will truncate unicode code points.
pack :: [OsChar] -> OsPath
pack = OS.pack


-----------------------------------------------------------------------------
-- |
-- Module      :  System.Directory
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- System-independent interface to directory manipulation (FilePath API).
--
-----------------------------------------------------------------------------

module System.Directory
   (
    -- $intro

    -- * Actions on directories
      createDirectory
    , createDirectoryIfMissing
    , removeDirectory
    , removeDirectoryRecursive
    , removePathForcibly
    , renameDirectory
    , listDirectory
    , getDirectoryContents
    -- ** Current working directory
    , getCurrentDirectory
    , setCurrentDirectory
    , withCurrentDirectory

    -- * Pre-defined directories
    , getHomeDirectory
    , XdgDirectory(..)
    , getXdgDirectory
    , XdgDirectoryList(..)
    , getXdgDirectoryList
    , getAppUserDataDirectory
    , getUserDocumentsDirectory
    , getTemporaryDirectory

    -- * Actions on files
    , removeFile
    , renameFile
    , renamePath
    , copyFile
    , copyFileWithMetadata
    , getFileSize

    , canonicalizePath
    , makeAbsolute
    , makeRelativeToCurrentDirectory

    -- * Existence tests
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

    -- * Symbolic links
    , createFileLink
    , createDirectoryLink
    , removeDirectoryLink
    , pathIsSymbolicLink
    , getSymbolicLinkTarget

    -- * Permissions

    -- $permissions

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

    -- * Timestamps

    , getAccessTime
    , getModificationTime
    , setAccessTime
    , setModificationTime

    -- * Deprecated
    , isSymbolicLink

   ) where
import Prelude ()
import System.Directory.Internal
import System.Directory.Internal.Prelude
import Data.Time (UTCTime)
import System.OsPath (decodeFS, encodeFS)
import qualified System.Directory.OsPath as D

{- $intro
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

-}

-----------------------------------------------------------------------------
-- Permissions

{- $permissions

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

-}

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

-- | Get the permissions of a file or directory.
--
-- On Windows, the 'writable' permission corresponds to the "read-only"
-- attribute.  The 'executable' permission is set if the file extension is of
-- an executable file type.  The 'readable' permission is always set.
--
-- On POSIX systems, this returns the result of @access@.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to access the
--   permissions, or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
getPermissions :: FilePath -> IO Permissions
getPermissions = encodeFS >=> D.getPermissions

-- | Set the permissions of a file or directory.
--
-- On Windows, this is only capable of changing the 'writable' permission,
-- which corresponds to the "read-only" attribute.  Changing the other
-- permissions has no effect.
--
-- On POSIX systems, this sets the /owner/ permissions.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to set the permissions,
--   or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
setPermissions :: FilePath -> Permissions -> IO ()
setPermissions path p = encodeFS path >>= (`D.setPermissions` p)

-- | Copy the permissions of one file to another.  This reproduces the
-- permissions more accurately than using 'getPermissions' followed by
-- 'setPermissions'.
--
-- On Windows, this copies only the read-only attribute.
--
-- On POSIX systems, this is equivalent to @stat@ followed by @chmod@.
copyPermissions :: FilePath -> FilePath -> IO ()
copyPermissions src dst = do
  src' <- encodeFS src
  dst' <- encodeFS dst
  D.copyPermissions src' dst'


-----------------------------------------------------------------------------
-- Implementation

{- |@'createDirectory' dir@ creates a new directory @dir@ which is
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

-}

createDirectory :: FilePath -> IO ()
createDirectory = encodeFS >=> D.createDirectory

-- | @'createDirectoryIfMissing' parents dir@ creates a new directory
-- @dir@ if it doesn\'t exist. If the first argument is 'True'
-- the function will also create all parent directories if they are missing.
createDirectoryIfMissing :: Bool     -- ^ Create its parents too?
                         -> FilePath -- ^ The path to the directory you want to make
                         -> IO ()
createDirectoryIfMissing cp = encodeFS >=> D.createDirectoryIfMissing cp


{- | @'removeDirectory' dir@ removes an existing directory /dir/.  The
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

-}

removeDirectory :: FilePath -> IO ()
removeDirectory = encodeFS >=> D.removeDirectory

-- | @'removeDirectoryRecursive' dir@ removes an existing directory /dir/
-- together with its contents and subdirectories. Within this directory,
-- symbolic links are removed without affecting their targets.
--
-- On Windows, the operation fails if /dir/ is a directory symbolic link.
--
-- This operation is reported to be flaky on Windows so retry logic may
-- be advisable. See: https://github.com/haskell/directory/pull/108
removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = encodeFS >=> D.removeDirectoryRecursive

-- | Removes a file or directory at /path/ together with its contents and
-- subdirectories. Symbolic links are removed without affecting their
-- targets. If the path does not exist, nothing happens.
--
-- Unlike other removal functions, this function will also attempt to delete
-- files marked as read-only or otherwise made unremovable due to permissions.
-- As a result, if the removal is incomplete, the permissions or attributes on
-- the remaining files may be altered.  If there are hard links in the
-- directory, then permissions on all related hard links may be altered.
--
-- If an entry within the directory vanishes while @removePathForcibly@ is
-- running, it is silently ignored.
--
-- If an exception occurs while removing an entry, @removePathForcibly@ will
-- still try to remove as many entries as it can before failing with an
-- exception.  The first exception that it encountered is re-thrown.
--
-- @since 1.2.7.0
removePathForcibly :: FilePath -> IO ()
removePathForcibly = encodeFS >=> D.removePathForcibly

{- |'removeFile' /file/ removes the directory entry for an existing file
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

-}

removeFile :: FilePath -> IO ()
removeFile = encodeFS >=> D.removeFile

{- |@'renameDirectory' old new@ changes the name of an existing
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

-}

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory opath npath = do
  opath' <- encodeFS opath
  npath' <- encodeFS npath
  D.renameDirectory opath' npath'

{- |@'renameFile' old new@ changes the name of an existing file system
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

-}

renameFile :: FilePath -> FilePath -> IO ()
renameFile opath npath = do
  opath' <- encodeFS opath
  npath' <- encodeFS npath
  D.renameFile opath' npath'

-- | Rename a file or directory.  If the destination path already exists, it
-- is replaced atomically.  The destination path must not point to an existing
-- directory.  A conformant implementation need not support renaming files in
-- all situations (e.g. renaming across different physical devices), but the
-- constraints must be documented.
--
-- The operation may fail with:
--
-- * @HardwareFault@
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * @InvalidArgument@
-- Either operand is not a valid file name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError'
-- The original file does not exist, or there is no path to the target.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError'
-- The process has insufficient privileges to perform the operation.
-- @[EROFS, EACCES, EPERM]@
--
-- * 'System.IO.isFullError'
-- Insufficient resources are available to perform the operation.
-- @[EDQUOT, ENOSPC, ENOMEM, EMLINK]@
--
-- * @UnsatisfiedConstraints@
-- Implementation-dependent constraints are not satisfied.
-- @[EBUSY]@
--
-- * @UnsupportedOperation@
-- The implementation does not support renaming in this situation.
-- @[EXDEV]@
--
-- * @InappropriateType@
-- Either the destination path refers to an existing directory, or one of the
-- parent segments in the destination path is not a directory.
-- @[ENOTDIR, EISDIR, EINVAL, EEXIST, ENOTEMPTY]@
--
-- @since 1.2.7.0
renamePath :: FilePath                  -- ^ Old path
           -> FilePath                  -- ^ New path
           -> IO ()
renamePath opath npath = do
  opath' <- encodeFS opath
  npath' <- encodeFS npath
  D.renamePath opath' npath'

-- | Copy a file with its permissions.  If the destination file already exists,
-- it is replaced atomically.  Neither path may refer to an existing
-- directory.  No exceptions are thrown if the permissions could not be
-- copied.
copyFile :: FilePath                    -- ^ Source filename
         -> FilePath                    -- ^ Destination filename
         -> IO ()
copyFile fromFPath toFPath = do
  fromFPath' <- encodeFS fromFPath
  toFPath' <- encodeFS toFPath
  D.copyFile fromFPath' toFPath'

-- | Copy a file with its associated metadata.  If the destination file
-- already exists, it is overwritten.  There is no guarantee of atomicity in
-- the replacement of the destination file.  Neither path may refer to an
-- existing directory.  If the source and/or destination are symbolic links,
-- the copy is performed on the targets of the links.
--
-- On Windows, it behaves like the Win32 function
-- <https://msdn.microsoft.com/en-us/library/windows/desktop/aa363851.aspx CopyFile>,
-- which copies various kinds of metadata including file attributes and
-- security resource properties.
--
-- On Unix-like systems, permissions, access time, and modification time are
-- preserved.  If possible, the owner and group are also preserved.  Note that
-- the very act of copying can change the access time of the source file,
-- hence the access times of the two files may differ after the operation
-- completes.
--
-- @since 1.2.6.0
copyFileWithMetadata :: FilePath        -- ^ Source file
                     -> FilePath        -- ^ Destination file
                     -> IO ()
copyFileWithMetadata src dst = do
  src' <- encodeFS src
  dst' <- encodeFS dst
  D.copyFileWithMetadata src' dst'


-- | Make a path absolute, normalize the path, and remove as many indirections
-- from it as possible.  Any trailing path separators are discarded via
-- 'dropTrailingPathSeparator'.  Additionally, on Windows the letter case of
-- the path is canonicalized.
--
-- __Note__: This function is a very big hammer.  If you only need an absolute
-- path, 'makeAbsolute' is sufficient for removing dependence on the current
-- working directory.
--
-- Indirections include the two special directories @.@ and @..@, as well as
-- any symbolic links (and junction points on Windows).  The input path need
-- not point to an existing file or directory.  Canonicalization is performed
-- on the longest prefix of the path that points to an existing file or
-- directory.  The remaining portion of the path that does not point to an
-- existing file or directory will still be normalized, but case
-- canonicalization and indirection removal are skipped as they are impossible
-- to do on a nonexistent path.
--
-- Most programs should not worry about the canonicity of a path.  In
-- particular, despite the name, the function does not truly guarantee
-- canonicity of the returned path due to the presence of hard links, mount
-- points, etc.
--
-- If the path points to an existing file or directory, then the output path
-- shall also point to the same file or directory, subject to the condition
-- that the relevant parts of the file system do not change while the function
-- is still running.  In other words, the function is definitively not atomic.
-- The results can be utterly wrong if the portions of the path change while
-- this function is running.
--
-- Since some indirections (symbolic links on all systems, @..@ on non-Windows
-- systems, and junction points on Windows) are dependent on the state of the
-- existing filesystem, the function can only make a conservative attempt by
-- removing such indirections from the longest prefix of the path that still
-- points to an existing file or directory.
--
-- Note that on Windows parent directories @..@ are always fully expanded
-- before the symbolic links, as consistent with the rest of the Windows API
-- (such as @GetFullPathName@).  In contrast, on POSIX systems parent
-- directories @..@ are expanded alongside symbolic links from left to right.
-- To put this more concretely: if @L@ is a symbolic link for @R/P@, then on
-- Windows @L\\..@ refers to @.@, whereas on other operating systems @L/..@
-- refers to @R@.
--
-- Similar to 'System.FilePath.normalise', passing an empty path is equivalent
-- to passing the current directory.
--
-- @canonicalizePath@ can resolve at least 64 indirections in a single path,
-- more than what is supported by most operating systems.  Therefore, it may
-- return the fully resolved path even though the operating system itself
-- would have long given up.
--
-- On Windows XP or earlier systems, junction expansion is not performed due
-- to their lack of @GetFinalPathNameByHandle@.
--
-- /Changes since 1.2.3.0:/ The function has been altered to be more robust
-- and has the same exception behavior as 'makeAbsolute'.
--
-- /Changes since 1.3.0.0:/ The function no longer preserves the trailing path
-- separator.  File symbolic links that appear in the middle of a path are
-- properly dereferenced.  Case canonicalization and symbolic link expansion
-- are now performed on Windows.
--
canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = encodeFS >=> D.canonicalizePath >=> decodeFS

-- | Convert a path into an absolute path.  If the given path is relative, the
-- current directory is prepended and then the combined result is normalized.
-- If the path is already absolute, the path is simply normalized.  The
-- function preserves the presence or absence of the trailing path separator
-- unless the path refers to the root directory @/@.
--
-- If the path is already absolute, the operation never fails.  Otherwise, the
-- operation may fail with the same exceptions as 'getCurrentDirectory'.
--
-- @since 1.2.2.0
--
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = encodeFS >=> D.makeAbsolute >=> decodeFS

-- | Construct a path relative to the current directory, similar to
-- 'makeRelative'.
--
-- The operation may fail with the same exceptions as 'getCurrentDirectory'.
makeRelativeToCurrentDirectory :: FilePath -> IO FilePath
makeRelativeToCurrentDirectory =
  encodeFS >=> D.makeRelativeToCurrentDirectory >=> decodeFS

-- | Given the name or path of an executable file, 'findExecutable' searches
-- for such a file in a list of system-defined locations, which generally
-- includes @PATH@ and possibly more.  The full path to the executable is
-- returned if found.  For example, @(findExecutable \"ghc\")@ would normally
-- give you the path to GHC.
--
-- The path returned by @'findExecutable' name@ corresponds to the program
-- that would be executed by
-- @<http://hackage.haskell.org/package/process/docs/System-Process.html#v:createProcess createProcess>@
-- when passed the same string (as a @RawCommand@, not a @ShellCommand@),
-- provided that @name@ is not a relative path with more than one segment.
--
-- On Windows, 'findExecutable' calls the Win32 function
-- @<https://msdn.microsoft.com/en-us/library/aa365527.aspx SearchPath>@,
-- which may search other places before checking the directories in the @PATH@
-- environment variable.  Where it actually searches depends on registry
-- settings, but notably includes the directory containing the current
-- executable.
--
-- On non-Windows platforms, the behavior is equivalent to 'findFileWith'
-- using the search directories from the @PATH@ environment variable and
-- testing each file for executable permissions.  Details can be found in the
-- documentation of 'findFileWith'.
findExecutable :: String -> IO (Maybe FilePath)
findExecutable = encodeFS >=> D.findExecutable >=> (`for` decodeFS)

-- | Search for executable files in a list of system-defined locations, which
-- generally includes @PATH@ and possibly more.
--
-- On Windows, this /only returns the first occurrence/, if any.  Its behavior
-- is therefore equivalent to 'findExecutable'.
--
-- On non-Windows platforms, the behavior is equivalent to
-- 'findExecutablesInDirectories' using the search directories from the @PATH@
-- environment variable.  Details can be found in the documentation of
-- 'findExecutablesInDirectories'.
--
-- @since 1.2.2.0
findExecutables :: String -> IO [FilePath]
findExecutables = encodeFS >=> D.findExecutables >=> (`for` decodeFS)

-- | Given a name or path, 'findExecutable' appends the 'exeExtension' to the
-- query and searches for executable files in the list of given search
-- directories and returns all occurrences.
--
-- The behavior is equivalent to 'findFileWith' using the given search
-- directories and testing each file for executable permissions.  Details can
-- be found in the documentation of 'findFileWith'.
--
-- Unlike other similarly named functions, 'findExecutablesInDirectories' does
-- not use @SearchPath@ from the Win32 API.  The behavior of this function on
-- Windows is therefore equivalent to those on non-Windows platforms.
--
-- @since 1.2.4.0
findExecutablesInDirectories :: [FilePath] -> String -> IO [FilePath]
findExecutablesInDirectories path binary = do
  path' <- for path encodeFS
  binary' <- encodeFS binary
  D.findExecutablesInDirectories path' binary'
    >>= (`for` decodeFS)

-- | Search through the given list of directories for the given file.
--
-- The behavior is equivalent to 'findFileWith', returning only the first
-- occurrence.  Details can be found in the documentation of 'findFileWith'.
findFile :: [FilePath] -> String -> IO (Maybe FilePath)
findFile = findFileWith (\ _ -> pure True)

-- | Search through the given list of directories for the given file and
-- returns all paths where the given file exists.
--
-- The behavior is equivalent to 'findFilesWith'.  Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 1.2.1.0
findFiles :: [FilePath] -> String -> IO [FilePath]
findFiles = findFilesWith (\ _ -> pure True)

-- | Search through a given list of directories for a file that has the given
-- name and satisfies the given predicate and return the path of the first
-- occurrence.  The directories are checked in a left-to-right order.
--
-- This is essentially a more performant version of 'findFilesWith' that
-- always returns the first result, if any.  Details can be found in the
-- documentation of 'findFilesWith'.
--
-- @since 1.2.6.0
findFileWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO (Maybe FilePath)
findFileWith f ds name = do
  ds' <- for ds encodeFS
  name' <- encodeFS name
  D.findFileWith (decodeFS >=> f) ds' name'
    >>= (`for` decodeFS)

-- | @findFilesWith predicate dirs name@ searches through the list of
-- directories (@dirs@) for files that have the given @name@ and satisfy the
-- given @predicate@ and returns the paths of those files.  The directories
-- are checked in a left-to-right order and the paths are returned in the same
-- order.
--
-- If the @name@ is a relative path, then for every search directory @dir@,
-- the function checks whether @dir '</>' name@ exists and satisfies the
-- predicate.  If so, @dir '</>' name@ is returned as one of the results.  In
-- other words, the returned paths can be either relative or absolute
-- depending on the search directories were used.  If there are no search
-- directories, no results are ever returned.
--
-- If the @name@ is an absolute path, then the function will return a single
-- result if the file exists and satisfies the predicate and no results
-- otherwise.  This is irrespective of what search directories were given.
--
-- @since 1.2.1.0
findFilesWith :: (FilePath -> IO Bool) -> [FilePath] -> String -> IO [FilePath]
findFilesWith f ds name = do
  ds' <- for ds encodeFS
  name' <- encodeFS name
  res <- D.findFilesWith (decodeFS >=> f) ds' name'
  for res decodeFS

-- | Filename extension for executable files (including the dot if any)
--   (usually @\"\"@ on POSIX systems and @\".exe\"@ on Windows or OS\/2).
--
-- @since 1.2.4.0
exeExtension :: String
exeExtension = so D.exeExtension

-- | Similar to 'listDirectory', but always includes the special entries (@.@
-- and @..@).  (This applies to Windows as well.)
--
-- The operation may fail with the same exceptions as 'listDirectory'.
getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents = encodeFS >=> D.getDirectoryContents >=> (`for` decodeFS)

-- | @'listDirectory' dir@ returns a list of /all/ entries in /dir/ without
-- the special entries (@.@ and @..@).
--
-- The operation may fail with:
--
-- * @HardwareFault@
--   A physical I\/O error has occurred.
--   @[EIO]@
--
-- * @InvalidArgument@
--   The operand is not a valid directory name.
--   @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError'
--   The directory does not exist.
--   @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError'
--   The process has insufficient privileges to perform the operation.
--   @[EACCES]@
--
-- * 'System.IO.isFullError'
--   Insufficient resources are available to perform the operation.
--   @[EMFILE, ENFILE]@
--
-- * @InappropriateType@
--   The path refers to an existing non-directory object.
--   @[ENOTDIR]@
--
-- @since 1.2.5.0
--
listDirectory :: FilePath -> IO [FilePath]
listDirectory = encodeFS >=> D.listDirectory >=> (`for` decodeFS)

-- | Obtain the current working directory as an absolute path.
--
-- In a multithreaded program, the current working directory is a global state
-- shared among all threads of the process.  Therefore, when performing
-- filesystem operations from multiple threads, it is highly recommended to
-- use absolute rather than relative paths (see: 'makeAbsolute').
--
-- Note that 'getCurrentDirectory' is not guaranteed to return the same path
-- received by 'setCurrentDirectory'. On POSIX systems, the path returned will
-- always be fully dereferenced (not contain any symbolic links). For more
-- information, refer to the documentation of
-- <https://pubs.opengroup.org/onlinepubs/9699919799/functions/getcwd.html getcwd>.
--
-- The operation may fail with:
--
-- * @HardwareFault@
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * 'isDoesNotExistError'
-- There is no path referring to the working directory.
-- @[EPERM, ENOENT, ESTALE...]@
--
-- * 'isPermissionError'
-- The process has insufficient privileges to perform the operation.
-- @[EACCES]@
--
-- * 'System.IO.isFullError'
-- Insufficient resources are available to perform the operation.
--
-- * @UnsupportedOperation@
-- The operating system has no notion of current working directory.
--
getCurrentDirectory :: IO FilePath
getCurrentDirectory = D.getCurrentDirectory >>= decodeFS

-- | Change the working directory to the given path.
--
-- In a multithreaded program, the current working directory is a global state
-- shared among all threads of the process.  Therefore, when performing
-- filesystem operations from multiple threads, it is highly recommended to
-- use absolute rather than relative paths (see: 'makeAbsolute').
--
-- The operation may fail with:
--
-- * @HardwareFault@
-- A physical I\/O error has occurred.
-- @[EIO]@
--
-- * @InvalidArgument@
-- The operand is not a valid directory name.
-- @[ENAMETOOLONG, ELOOP]@
--
-- * 'isDoesNotExistError'
-- The directory does not exist.
-- @[ENOENT, ENOTDIR]@
--
-- * 'isPermissionError'
-- The process has insufficient privileges to perform the operation.
-- @[EACCES]@
--
-- * @UnsupportedOperation@
-- The operating system has no notion of current working directory, or the
-- working directory cannot be dynamically changed.
--
-- * @InappropriateType@
-- The path refers to an existing non-directory object.
-- @[ENOTDIR]@
--
setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory = encodeFS >=> D.setCurrentDirectory

-- | Run an 'IO' action with the given working directory and restore the
-- original working directory afterwards, even if the given action fails due
-- to an exception.
--
-- The operation may fail with the same exceptions as 'getCurrentDirectory'
-- and 'setCurrentDirectory'.
--
-- @since 1.2.3.0
--
withCurrentDirectory :: FilePath  -- ^ Directory to execute in
                     -> IO a      -- ^ Action to be executed
                     -> IO a
withCurrentDirectory dir action =
  encodeFS dir >>= (`D.withCurrentDirectory` action)

-- | Obtain the size of a file in bytes.
--
-- @since 1.2.7.0
getFileSize :: FilePath -> IO Integer
getFileSize = encodeFS >=> D.getFileSize

-- | Test whether the given path points to an existing filesystem object.  If
-- the user lacks necessary permissions to search the parent directories, this
-- function may return false even if the file does actually exist.
--
-- @since 1.2.7.0
doesPathExist :: FilePath -> IO Bool
doesPathExist = encodeFS >=> D.doesPathExist

{- |The operation 'doesDirectoryExist' returns 'True' if the argument file
exists and is either a directory or a symbolic link to a directory,
and 'False' otherwise.
-}

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist = encodeFS >=> D.doesDirectoryExist

{- |The operation 'doesFileExist' returns 'True'
if the argument file exists and is not a directory, and 'False' otherwise.
-}

doesFileExist :: FilePath -> IO Bool
doesFileExist = encodeFS >=> D.doesFileExist


-- | Create a /file/ symbolic link.  The target path can be either absolute or
-- relative and need not refer to an existing file.  The order of arguments
-- follows the POSIX convention.
--
-- To remove an existing file symbolic link, use 'removeFile'.
--
-- Although the distinction between /file/ symbolic links and /directory/
-- symbolic links does not exist on POSIX systems, on Windows this is an
-- intrinsic property of every symbolic link and cannot be changed without
-- recreating the link.  A file symbolic link that actually points to a
-- directory will fail to dereference and vice versa.  Moreover, creating
-- symbolic links on Windows may require privileges unavailable to users
-- outside the Administrators group.  Portable programs that use symbolic
-- links should take both into consideration.
--
-- On Windows, the function is implemented using @CreateSymbolicLink@.  Since
-- 1.3.3.0, the @SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE@ flag is included
-- if supported by the operating system.  On POSIX, the function uses @symlink@
-- and is therefore atomic.
--
-- Windows-specific errors: This operation may fail with 'permissionErrorType'
-- if the user lacks the privileges to create symbolic links.  It may also
-- fail with 'illegalOperationErrorType' if the file system does not support
-- symbolic links.
--
-- @since 1.3.1.0
createFileLink
  :: FilePath                           -- ^ path to the target file
  -> FilePath                           -- ^ path of the link to be created
  -> IO ()
createFileLink target link = do
  target' <- encodeFS target
  link' <- encodeFS link
  D.createFileLink target' link'


-- | Create a /directory/ symbolic link.  The target path can be either
-- absolute or relative and need not refer to an existing directory.  The
-- order of arguments follows the POSIX convention.
--
-- To remove an existing directory symbolic link, use 'removeDirectoryLink'.
--
-- Although the distinction between /file/ symbolic links and /directory/
-- symbolic links does not exist on POSIX systems, on Windows this is an
-- intrinsic property of every symbolic link and cannot be changed without
-- recreating the link.  A file symbolic link that actually points to a
-- directory will fail to dereference and vice versa.  Moreover, creating
-- symbolic links on Windows may require privileges unavailable to users
-- outside the Administrators group.  Portable programs that use symbolic
-- links should take both into consideration.
--
-- On Windows, the function is implemented using @CreateSymbolicLink@ with
-- @SYMBOLIC_LINK_FLAG_DIRECTORY@.  Since 1.3.3.0, the
-- @SYMBOLIC_LINK_FLAG_ALLOW_UNPRIVILEGED_CREATE@ flag is also included if
-- supported by the operating system.   On POSIX, this is an alias for
-- 'createFileLink' and is therefore atomic.
--
-- Windows-specific errors: This operation may fail with 'permissionErrorType'
-- if the user lacks the privileges to create symbolic links.  It may also
-- fail with 'illegalOperationErrorType' if the file system does not support
-- symbolic links.
--
-- @since 1.3.1.0
createDirectoryLink
  :: FilePath                           -- ^ path to the target directory
  -> FilePath                           -- ^ path of the link to be created
  -> IO ()
createDirectoryLink target link = do
  target' <- encodeFS target
  link' <- encodeFS link
  D.createDirectoryLink target' link'

-- | Remove an existing /directory/ symbolic link.
--
-- On Windows, this is an alias for 'removeDirectory'.  On POSIX systems, this
-- is an alias for 'removeFile'.
--
-- See also: 'removeFile', which can remove an existing /file/ symbolic link.
--
-- @since 1.3.1.0
removeDirectoryLink :: FilePath -> IO ()
removeDirectoryLink = encodeFS >=> D.removeDirectoryLink

-- | Check whether an existing @path@ is a symbolic link.  If @path@ is a
-- regular file or directory, 'False' is returned.  If @path@ does not exist
-- or is otherwise inaccessible, an exception is thrown (see below).
--
-- On Windows, this checks for @FILE_ATTRIBUTE_REPARSE_POINT@.  In addition to
-- symbolic links, the function also returns true on junction points.  On
-- POSIX systems, this checks for @S_IFLNK@.
--
-- The operation may fail with:
--
-- * 'isDoesNotExistError' if the symbolic link does not exist; or
--
-- * 'isPermissionError' if the user is not permitted to read the symbolic
--   link.
--
-- @since 1.3.0.0
pathIsSymbolicLink :: FilePath -> IO Bool
pathIsSymbolicLink = encodeFS >=> D.pathIsSymbolicLink

{-# DEPRECATED isSymbolicLink "Use 'pathIsSymbolicLink' instead" #-}
isSymbolicLink :: FilePath -> IO Bool
isSymbolicLink = pathIsSymbolicLink

-- | Retrieve the target path of either a file or directory symbolic link.
-- The returned path may not be absolute, may not exist, and may not even be a
-- valid path.
--
-- On Windows systems, this calls @DeviceIoControl@ with
-- @FSCTL_GET_REPARSE_POINT@.  In addition to symbolic links, the function
-- also works on junction points.  On POSIX systems, this calls @readlink@.
--
-- Windows-specific errors: This operation may fail with
-- 'illegalOperationErrorType' if the file system does not support symbolic
-- links.
--
-- @since 1.3.1.0
getSymbolicLinkTarget :: FilePath -> IO FilePath
getSymbolicLinkTarget = encodeFS >=> D.getSymbolicLinkTarget >=> decodeFS

-- | Obtain the time at which the file or directory was last accessed.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to read
--   the access time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Caveat for POSIX systems: This function returns a timestamp with sub-second
-- resolution only if this package is compiled against @unix-2.6.0.0@ or later
-- and the underlying filesystem supports them.
--
-- @since 1.2.3.0
--
getAccessTime :: FilePath -> IO UTCTime
getAccessTime = encodeFS >=> D.getAccessTime

-- | Obtain the time at which the file or directory was last modified.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to read
--   the modification time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Caveat for POSIX systems: This function returns a timestamp with sub-second
-- resolution only if this package is compiled against @unix-2.6.0.0@ or later
-- and the underlying filesystem supports them.
--
getModificationTime :: FilePath -> IO UTCTime
getModificationTime = encodeFS >=> D.getModificationTime

-- | Change the time at which the file or directory was last accessed.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to alter the
--   access time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- Some caveats for POSIX systems:
--
-- * Not all systems support @utimensat@, in which case the function can only
--   emulate the behavior by reading the modification time and then setting
--   both the access and modification times together.  On systems where
--   @utimensat@ is supported, the access time is set atomically with
--   nanosecond precision.
--
-- * If compiled against a version of @unix@ prior to @2.7.0.0@, the function
--   would not be able to set timestamps with sub-second resolution.  In this
--   case, there would also be loss of precision in the modification time.
--
-- @since 1.2.3.0
--
setAccessTime :: FilePath -> UTCTime -> IO ()
setAccessTime path atime = encodeFS path >>= (`D.setAccessTime` atime)

-- | Change the time at which the file or directory was last modified.
--
-- The operation may fail with:
--
-- * 'isPermissionError' if the user is not permitted to alter the
--   modification time; or
--
-- * 'isDoesNotExistError' if the file or directory does not exist.
--
-- * 'InvalidArgument' on FAT32 file system if the time is before
--   DOS Epoch (1 January 1980).
--
-- Some caveats for POSIX systems:
--
-- * Not all systems support @utimensat@, in which case the function can only
--   emulate the behavior by reading the access time and then setting both the
--   access and modification times together.  On systems where @utimensat@ is
--   supported, the modification time is set atomically with nanosecond
--   precision.
--
-- * If compiled against a version of @unix@ prior to @2.7.0.0@, the function
--   would not be able to set timestamps with sub-second resolution.  In this
--   case, there would also be loss of precision in the access time.
--
-- @since 1.2.3.0
--
setModificationTime :: FilePath -> UTCTime -> IO ()
setModificationTime path mtime =
  encodeFS path >>= (`D.setModificationTime` mtime)

{- | Returns the current user's home directory.

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
-}
getHomeDirectory :: IO FilePath
getHomeDirectory = D.getHomeDirectory >>= decodeFS

-- | Obtain the paths to special directories for storing user-specific
--   application data, configuration, and cache files, conforming to the
--   <http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html XDG Base Directory Specification>.
--   Compared with 'getAppUserDataDirectory', this function provides a more
--   fine-grained hierarchy as well as greater flexibility for the user.
--
--   On Windows, 'XdgData' and 'XdgConfig' usually map to the same directory
--   unless overridden.
--
--   Refer to the docs of 'XdgDirectory' for more details.
--
--   The second argument is usually the name of the application.  Since it
--   will be integrated into the path, it must consist of valid path
--   characters.  Note: if the second argument is an absolute path, it will
--   just return the second argument.
--
--   Note: The directory may not actually exist, in which case you would need
--   to create it with file mode @700@ (i.e. only accessible by the owner).
--
--   As of 1.3.5.0, the environment variable is ignored if set to a relative
--   path, per revised XDG Base Directory Specification.  See
--   <https://github.com/haskell/directory/issues/100 #100>.
--
--   @since 1.2.3.0
getXdgDirectory :: XdgDirectory         -- ^ which special directory
                -> FilePath             -- ^ a relative path that is appended
                                        --   to the path; if empty, the base
                                        --   path is returned
                -> IO FilePath
getXdgDirectory xdgDir = encodeFS >=> D.getXdgDirectory xdgDir >=> decodeFS

-- | Similar to 'getXdgDirectory' but retrieves the entire list of XDG
-- directories.
--
-- On Windows, 'XdgDataDirs' and 'XdgConfigDirs' usually map to the same list
-- of directories unless overridden.
--
-- Refer to the docs of 'XdgDirectoryList' for more details.
getXdgDirectoryList :: XdgDirectoryList -- ^ which special directory list
                    -> IO [FilePath]
getXdgDirectoryList = D.getXdgDirectoryList >=> (`for` decodeFS)

-- | Obtain the path to a special directory for storing user-specific
--   application data (traditional Unix location).  Newer applications may
--   prefer the the XDG-conformant location provided by 'getXdgDirectory'
--   (<https://github.com/haskell/directory/issues/6#issuecomment-96521020 migration guide>).
--
--   The argument is usually the name of the application.  Since it will be
--   integrated into the path, it must consist of valid path characters.
--
--   * On Unix-like systems, the path is @~\/./\<app\>/@.
--   * On Windows, the path is @%APPDATA%\//\<app\>/@
--     (e.g. @C:\/Users\//\<user\>/\/AppData\/Roaming\//\<app\>/@)
--
--   Note: the directory may not actually exist, in which case you would need
--   to create it.  It is expected that the parent directory exists and is
--   writable.
--
--   The operation may fail with:
--
--   * @UnsupportedOperation@
--     The operating system has no notion of application-specific data
--     directory.
--
--   * 'isDoesNotExistError'
--     The home directory for the current user does not exist, or cannot be
--     found.
--
getAppUserDataDirectory :: FilePath     -- ^ a relative path that is appended
                                        --   to the path
                        -> IO FilePath
getAppUserDataDirectory = encodeFS >=> D.getAppUserDataDirectory >=>decodeFS

{- | Returns the current user's document directory.

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
-}
getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = D.getUserDocumentsDirectory >>= decodeFS

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = D.getTemporaryDirectory >>= decodeFS

module Data.Set (
            -- * Set type
              Set          -- instance Eq,Ord,Show,Read,Data

            -- * Construction
            , empty
            , singleton
            , fromList
            , fromAscList
            , fromDescList
            , fromDistinctAscList
            , fromDistinctDescList
            , powerSet

            -- * Insertion
            , insert

            -- * Deletion
            , delete

            -- * Generalized insertion/deletion

            , alterF

            -- * Query
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

            -- * Combine
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

            -- * Filter
            , S.filter
            , takeWhileAntitone
            , dropWhileAntitone
            , spanAntitone
            , partition
            , split
            , splitMember
            , splitRoot

            -- * Indexed
            , lookupIndex
            , findIndex
            , elemAt
            , deleteAt
            , S.take
            , S.drop
            , S.splitAt

            -- * Map
            , S.map
            , mapMonotonic

            -- * Folds
            , S.foldr
            , S.foldl
            -- ** Strict folds
            , S.foldr'
            , S.foldl'
            -- ** Legacy folds
            , fold

            -- * Min\/Max
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

            -- * Conversion

            -- ** List
            , elems
            , toList
            , toAscList
            , toDescList

            -- * Debugging
            , showTree
            , showTreeWith
            , valid
            ) where

import Data.Set.Internal as S

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE Trustworthy #-}
#endif

#include "containers.h"

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Tree
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = Multi-way Trees and Forests
--
-- The @'Tree' a@ type represents a lazy, possibly infinite, multi-way tree
-- (also known as a /rose tree/).
--
-- The @'Forest' a@ type represents a forest of @'Tree' a@s.
--
-----------------------------------------------------------------------------

module Data.Tree(

    -- * Trees and Forests
      Tree(..)
    , Forest
    , PostOrder(..)

    -- * Construction
    , unfoldTree
    , unfoldForest
    , unfoldTreeM
    , unfoldForestM
    , unfoldTreeM_BF
    , unfoldForestM_BF

    -- * Elimination
    , foldTree
    , flatten
    , levels
    , leaves
    , edges
    , pathsToRoot
    , pathsFromRoot

    -- * Ascii Drawings
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
-- See Note [ Template Haskell Dependencies ]
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

-- | Non-empty, possibly infinite, multi-way trees; also known as /rose trees/.
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

-- | This type synonym exists primarily for historical
-- reasons.
type Forest a = [Tree a]

-- | @since 0.5.9
instance Eq1 Tree where
  liftEq eq = leq
    where
      leq (Node a fr) (Node a' fr') = eq a a' && liftEq leq fr fr'

-- | @since 0.5.9
instance Ord1 Tree where
  liftCompare cmp = lcomp
    where
      lcomp (Node a fr) (Node a' fr') = cmp a a' <> liftCompare lcomp fr fr'

-- | @since 0.5.9
instance Show1 Tree where
  liftShowsPrec shw shwl p (Node a fr) = showParen (p > 10) $
        showString "Node {rootLabel = " . shw 0 a . showString ", " .
          showString "subForest = " . liftShowList shw shwl fr .
          showString "}"

-- | @since 0.5.9
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
{-# NOINLINE [1] fmapTree #-}
{-# RULES
"fmapTree/coerce" fmapTree coerce = coerce
 #-}
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

-- | @since 0.5.11
instance MonadFix Tree where
  mfix = mfixTree

mfixTree :: (a -> Tree a) -> Tree a
mfixTree f
  | Node a children <- fix (f . rootLabel)
  = Node a (zipWith (\i _ -> mfixTree ((!! i) . subForest . f))
                    [0..] children)

-- | Traverses in pre-order.
instance Traversable Tree where
  traverse f = go
    where go (Node x ts) = liftA2 Node (f x) (traverse go ts)
  {-# INLINE traverse #-}

-- | Folds in pre-order.

-- See Note [Implemented Foldable Tree functions]
instance Foldable Tree where
    fold = foldMap id
    {-# INLINABLE fold #-}

    foldMap = foldMapDefault
    {-# INLINE foldMap #-}

    foldr f z = \t -> go t z  -- Use a lambda to allow inlining with two arguments
      where
        go (Node x ts) = f x . foldr (\t k -> go t . k) id ts
        -- This is equivalent to the following simpler definition, but has been found to optimize
        -- better in benchmarks:
        -- go (Node x ts) z' = f x (foldr go z' ts)
    {-# INLINE foldr #-}

    foldl' f = go
      where go !z (Node x ts) = foldl' go (f z x) ts
    {-# INLINE foldl' #-}

    foldr1 = foldrMap1 id

    foldl1 = foldlMap1 id

    null _ = False
    {-# INLINE null #-}

    elem = any . (==)
    {-# INLINABLE elem #-}

    maximum = foldlMap1' id max
    {-# INLINABLE maximum #-}

    minimum = foldlMap1' id min
    {-# INLINABLE minimum #-}

    sum = foldlMap1' id (+)
    {-# INLINABLE sum #-}

    product = foldlMap1' id (*)
    {-# INLINABLE product #-}

#if MIN_VERSION_base(4,18,0)
-- | Folds in pre-order.
--
-- @since 0.6.7

-- See Note [Implemented Foldable1 Tree functions]
instance Foldable1.Foldable1 Tree where
  foldMap1 f = go
    where
      -- We'd like to write
      --
      -- go (Node x (t : ts)) = f x <> Foldable1.foldMap1 go (t :| ts)
      --
      -- but foldMap1 for NonEmpty isn't very good, so we don't. See
      -- https://github.com/haskell/containers/pull/921#issuecomment-1410398618
      go (Node x []) = f x
      go (Node x (t : ts)) =
        f x <> Foldable1.foldrMap1 go (\t' z -> go t' <> z) (t :| ts)
  {-# INLINE foldMap1 #-}

  foldMap1' f = foldlMap1' f (\z x -> z <> f x)
  {-# INLINE foldMap1' #-}

  toNonEmpty (Node x ts) = x :| concatMap toList ts

  maximum = Foldable.maximum
  {-# INLINABLE maximum #-}

  minimum = Foldable.minimum
  {-# INLINABLE minimum #-}

  foldrMap1 = foldrMap1

  foldlMap1' = foldlMap1'

  foldlMap1 = foldlMap1
#endif

foldrMap1 :: (a -> b) -> (a -> b -> b) -> Tree a -> b
foldrMap1 f g = go
  where
    go (Node x [])     = f x
    go (Node x (t:ts)) = g x (foldrMap1NE go (\t' z -> foldr g z t') t ts)
{-# INLINE foldrMap1 #-}

-- This is foldrMap1 for Data.List.NonEmpty, but is not available before
-- base 4.18.
foldrMap1NE :: (a -> b) -> (a -> b -> b) -> a -> [a] -> b
foldrMap1NE f g = go
  where
    go x []      = f x
    go x (x':xs) = g x (go x' xs)
{-# INLINE foldrMap1NE #-}

foldlMap1' :: (a -> b) -> (b -> a -> b) -> Tree a -> b
foldlMap1' f g =  -- Use a lambda to allow inlining with two arguments
  \(Node x ts) -> foldl' (foldl' g) (f x) ts
{-# INLINE foldlMap1' #-}

foldlMap1 :: (a -> b) -> (b -> a -> b) -> Tree a -> b
foldlMap1 f g =  -- Use a lambda to allow inlining with two arguments
  \(Node x ts) -> foldl (foldl g) (f x) ts
{-# INLINE foldlMap1 #-}

instance NFData a => NFData (Tree a) where
    rnf (Node x ts) = rnf x `seq` rnf ts

-- | @since 0.8
instance NFData1 Tree where
    liftRnf rnfx = go
      where
      go (Node x ts) = rnfx x `seq` liftRnf go ts

-- | @since 0.5.10.1
instance MonadZip Tree where
  mzipWith f (Node a as) (Node b bs)
    = Node (f a b) (mzipWith (mzipWith f) as bs)

  munzip (Node (a, b) ts) = (Node a as, Node b bs)
    where (as, bs) = munzip (map munzip ts)

-- | 2-dimensional ASCII drawing of a tree.
--
-- ==== __Examples__
--
-- > putStr $ drawTree $ fmap show (Node 1 [Node 2 [], Node 3 []])
--
-- @
-- 1
-- |
-- +- 2
-- |
-- `- 3
-- @
--
drawTree :: Tree String -> String
drawTree  = unlines . draw

-- | 2-dimensional ASCII drawing of a forest.
--
-- ==== __Examples__
--
-- > putStr $ drawForest $ map (fmap show) [(Node 1 [Node 2 [], Node 3 []]), (Node 10 [Node 20 []])]
--
-- @
-- 1
-- |
-- +- 2
-- |
-- `- 3
--
-- 10
-- |
-- `- 20
-- @
--
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

-- | Returns the elements of a tree in pre-order.
--
-- @flatten == Data.Foldable.'toList'@
--
-- @
--
--   a
--  / \\    => [a,b,c]
-- b   c
-- @
--
-- ==== __Examples__
--
-- > flatten (Node 1 [Node 2 [], Node 3 []]) == [1,2,3]
flatten :: Tree a -> [a]
flatten = toList

-- | Returns the list of nodes at each level of the tree.
--
-- @
--
--   a
--  / \\    => [[a], [b,c]]
-- b   c
-- @
--
-- ==== __Examples__
--
-- > levels (Node 1 [Node 2 [], Node 3 []]) == [[1],[2,3]]
--
levels :: Tree a -> [[a]]
levels t =
    map (map rootLabel) $
        takeWhile (not . null) $
        iterate (concatMap subForest) [t]

-- | Fold a tree into a "summary" value.
--
-- For each node in the tree, apply @f@ to the @rootLabel@ and the result
-- of applying @f@ to each @subForest@.
--
-- This is also known as the catamorphism on trees.
--
-- ==== __Examples__
--
-- Sum the values in a tree:
--
-- > foldTree (\x xs -> sum (x:xs)) (Node 1 [Node 2 [], Node 3 []]) == 6
--
-- Find the maximum value in the tree:
--
-- > foldTree (\x xs -> maximum (x:xs)) (Node 1 [Node 2 [], Node 3 []]) == 3
--
-- Count the number of leaves in the tree:
--
-- > foldTree (\_ xs -> if null xs then 1 else sum xs) (Node 1 [Node 2 [], Node 3 []]) == 2
--
-- Find depth of the tree; i.e. the number of branches from the root of the tree to the furthest leaf:
--
-- > foldTree (\_ xs -> if null xs then 0 else 1 + maximum xs) (Node 1 [Node 2 [], Node 3 []]) == 1
--
-- You can even implement traverse using foldTree:
--
-- > traverse' f = foldTree (\x xs -> liftA2 Node (f x) (sequenceA xs))
--
--
-- @since 0.5.8
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f = go where
    go (Node x ts) = f x (map go ts)

-- | Build a (possibly infinite) tree from a seed value.
--
-- @unfoldTree f b@ constructs a tree by starting with the tree
-- @Node { rootLabel=b, subForest=[] }@ and repeatedly applying @f@ to each
-- 'rootLabel' value in the tree's leaves to generate its 'subForest'.
--
-- For a monadic version, see 'unfoldTreeM' (depth-first) and
-- 'unfoldTreeM_BF' (breadth-first).
--
-- ==== __Examples__
--
-- Construct the tree of @Integer@s where each node has two children:
-- @left = 2*x@ and @right = 2*x + 1@, where @x@ is the 'rootLabel' of the node.
-- Stop when the values exceed 7.
--
-- > let buildNode x = if 2*x + 1 > 7 then (x, []) else (x, [2*x, 2*x+1])
-- > putStr $ drawTree $ fmap show $ unfoldTree buildNode 1
--
-- @
--
-- 1
-- |
-- +- 2
-- |  |
-- |  +- 4
-- |  |
-- |  `- 5
-- |
-- `- 3
--    |
--    +- 6
--    |
--    `- 7
-- @
--
unfoldTree :: (b -> (a, [b])) -> b -> Tree a
unfoldTree f b = let (a, bs) = f b in Node a (unfoldForest f bs)

-- | Build a (possibly infinite) forest from a list of seed values.
--
-- @unfoldForest f seeds@ invokes 'unfoldTree' on each seed value.
--
-- For a monadic version, see 'unfoldForestM' (depth-first) and
-- 'unfoldForestM_BF' (breadth-first).
--
unfoldForest :: (b -> (a, [b])) -> [b] -> [Tree a]
unfoldForest f = map (unfoldTree f)

-- | Monadic tree builder, in depth-first order.
unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM f b = do
    (a, bs) <- f b
    ts <- unfoldForestM f bs
    return (Node a ts)

-- | Monadic forest builder, in depth-first order.
unfoldForestM :: Monad m => (b -> m (a, [b])) -> [b] -> m ([Tree a])
unfoldForestM f = Prelude.mapM (unfoldTreeM f)

-- | Monadic tree builder, in breadth-first order.
--
-- See 'unfoldTree' for more info.
--
-- Implemented using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldTreeM_BF :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)
unfoldTreeM_BF f b = liftM getElement $ unfoldForestQ f (singleton b)
  where
    getElement xs = case viewl xs of
        x :< _ -> x
        EmptyL -> error "unfoldTreeM_BF"

-- | Monadic forest builder, in breadth-first order.
--
-- See 'unfoldForest' for more info.
--
-- Implemented using an algorithm adapted from
-- /Breadth-First Numbering: Lessons from a Small Exercise in Algorithm Design/,
-- by Chris Okasaki, /ICFP'00/.
unfoldForestM_BF :: Monad m => (b -> m (a, [b])) -> [b] -> m ([Tree a])
unfoldForestM_BF f = liftM toList . unfoldForestQ f . fromList

-- Takes a sequence (queue) of seeds and produces a sequence (reversed queue) of
-- trees of the same length.
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

-- | \(O(n)\). The leaves of the tree in left-to-right order.
--
-- A leaf is a node with no children.
--
-- ==== __Examples__
--
-- >>> :{
-- leaves $
--   Node 1
--     [ Node 2
--         [ Node 4 []
--         , Node 5 []
--         ]
--     , Node 3
--         [ Node 6 []
--         ]
--     ]
-- :}
-- [4,5,6]
-- >>> leaves (Node "root" [])
-- ["root"]
--
-- @since 0.8
leaves :: Tree a -> [a]
#ifdef __GLASGOW_HASKELL__
leaves t = GHC.Exts.build $ \cons nil ->
  let go (Node x []) z = cons x z
      go (Node _ ts) z = foldr go z ts
  in go t nil
{-# INLINE leaves #-} -- Inline for list fusion
#else
leaves t =
  let go (Node x []) z = x:z
      go (Node _ ts) z = foldr go z ts
  in go t []
#endif

-- | \(O(n)\). The edges of the tree as parent-child pairs in pre-order.
--
-- A tree with \(n\) nodes has \(n-1\) edges.
--
-- ==== __Examples__
--
-- >>> :{
-- edges $
--   Node 1
--     [ Node 2
--         [ Node 4 []
--         , Node 5 []
--         ]
--     , Node 3
--         [ Node 6 []
--         ]
--     ]
-- :}
-- [(1,2),(2,4),(2,5),(1,3),(3,6)]
-- >>> edges (Node "root" [])
-- []
--
-- @since 0.8
edges :: Tree a -> [(a, a)]
#ifdef __GLASGOW_HASKELL__
edges (Node x0 ts0) = GHC.Exts.build $ \cons nil ->
  let go p = foldr (\(Node x ts) z -> cons (p, x) (go x z ts))
  in go x0 nil ts0
{-# INLINE edges #-} -- Inline for list fusion
#else
edges (Node x0 ts0) =
  let go p = foldr (\(Node x ts) z -> (p, x) : go x z ts)
  in go x0 [] ts0
#endif

-- | \(O(n)\). Labels on the paths from each node to the root.
--
-- ==== __Examples__
--
-- >>> :{
-- pathsToRoot $
--   Node 1
--     [ Node 2 []
--     , Node 3 []
--     ]
-- :}
-- Node {rootLabel = 1 :| [], subForest = [Node {rootLabel = 2 :| [1], subForest = []},Node {rootLabel = 3 :| [1], subForest = []}]}
-- >>> pathsToRoot (Node "root" [])
-- Node {rootLabel = "root" :| [], subForest = []}
--
-- @since 0.8
pathsToRoot :: Tree a -> Tree (NonEmpty a)
pathsToRoot = go []
  where
    go ps (Node x ts) = Node (x :| ps) (map (go (x:ps)) ts)

-- | Labels on the paths from the root to each node.
--
-- If the path orientation is not important, consider using 'pathsToRoot'
-- instead because it is more efficient.
--
-- ==== __Examples__
--
-- >>> :{
-- pathsFromRoot $
--   Node 1
--     [ Node 2 []
--     , Node 3 []
--     ]
-- :}
-- Node {rootLabel = 1 :| [], subForest = [Node {rootLabel = 1 :| [2], subForest = []},Node {rootLabel = 1 :| [3], subForest = []}]}
-- >>> pathsFromRoot (Node "root" [])
-- Node {rootLabel = "root" :| [], subForest = []}
--
-- @since 0.8

-- See Note [pathsFromRoot implementation]
pathsFromRoot :: Tree a -> Tree (NonEmpty a)
pathsFromRoot (Node x0 ts0) = Node (x0 :| []) (map (go (singletonBQ x0)) ts0)
  where
    go !q (Node x ts) = Node (toNonEmptyBQ q') (map (go q') ts)
      where
        !q' = snocBQ q x

-- An implementation of Chris Okasaki's banker's queue.
-- Invariant: length front >= length rear
data BQ a = BQ
  a -- head
  {-# UNPACK #-} !Word -- length front + length rear
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
    -- We reverse whenever the length of r would exceed that of f.
    -- This happens every time n+2 is a power of 2.

toNonEmptyBQ :: BQ a -> NonEmpty a
toNonEmptyBQ (BQ x0 _ f r) = case r of
  [] -> x0 :| f -- optimization, no need to rebuild f
  _ -> x0 :| (f ++ reverse r)

-- | A newtype over 'Tree' that folds and traverses in post-order.
--
-- @since 0.8
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

-- See Note [Implemented Foldable Tree functions]
instance Foldable PostOrder where
    fold = foldMap id
    {-# INLINABLE fold #-}

    foldMap = foldMapDefault
    {-# INLINE foldMap #-}

    foldr f z0 = \(PostOrder t) -> go t z0  -- Use a lambda to inline with two arguments
      where
        go (Node x ts) z = foldr go (f x z) ts
    {-# INLINE foldr #-}

    foldl' f z0 = \(PostOrder t) -> go z0 t  -- Use a lambda to inline with two arguments
      where
        go !z (Node x ts) =
          let !z' = foldl' go z ts
          in f z' x
    {-# INLINE foldl' #-}

    foldr1 = foldrMap1PostOrder id

    foldl1 = foldlMap1PostOrder id

    null _ = False
    {-# INLINE null #-}

    elem = any . (==)
    {-# INLINABLE elem #-}

    maximum = foldlMap1'PostOrder id max
    {-# INLINABLE maximum #-}

    minimum = foldlMap1'PostOrder id min
    {-# INLINABLE minimum #-}

    sum = foldlMap1'PostOrder id (+)
    {-# INLINABLE sum #-}

    product = foldlMap1'PostOrder id (*)
    {-# INLINABLE product #-}

instance Traversable PostOrder where
  traverse f = \(PostOrder t) -> PostOrder <$> go t
    where
      go (Node x ts) = liftA2 (flip Node) (traverse go ts) (f x)
  {-# INLINE traverse #-}

#if MIN_VERSION_base(4,18,0)
-- See Note [Implemented Foldable1 Tree functions]
instance Foldable1.Foldable1 PostOrder where
  foldMap1 f = \(PostOrder t) -> go t  -- Use a lambda to inline with one argument
    where
      go (Node x []) = f x
      go (Node x (t:ts)) =
        Foldable1.foldrMap1 go (\t' z' -> go t' <> z') (t :| ts) <> f x
  {-# INLINE foldMap1 #-}

  foldMap1' f = foldlMap1'PostOrder f (\z x -> z <> f x)
  {-# INLINE foldMap1' #-}

  toNonEmpty (PostOrder t0) = go t0 []
    where
      go (Node x []) z = x :| z
      go (Node x (t:ts)) z =
        go t (foldr (\t' z' -> foldr (:) z' (PostOrder t')) (x:z) ts)

  maximum = Foldable.maximum
  {-# INLINABLE maximum #-}

  minimum = Foldable.minimum
  {-# INLINABLE minimum #-}

  foldrMap1 = foldrMap1PostOrder

  foldlMap1' = foldlMap1'PostOrder

  foldlMap1 = foldlMap1PostOrder
#endif

foldrMap1PostOrder :: (a -> b) -> (a -> b -> b) -> PostOrder a -> b
foldrMap1PostOrder f g = \(PostOrder (Node x ts)) ->
  foldr (\t z -> foldr g z (PostOrder t)) (f x) ts
{-# INLINE foldrMap1PostOrder #-}

foldlMap1PostOrder :: (a -> b) -> (b -> a -> b) -> PostOrder a -> b
foldlMap1PostOrder f g = \(PostOrder t) -> go t
  where
    go (Node x []) = f x
    go (Node x (t:ts)) =
      g (foldl (\z t' -> foldl g z (PostOrder t')) (go t) ts) x
{-# INLINE foldlMap1PostOrder #-}

foldlMap1'PostOrder :: (a -> b) -> (b -> a -> b) -> PostOrder a -> b
foldlMap1'PostOrder f g = \(PostOrder t) -> go t
  where
    go (Node x []) = f x
    go (Node x (t:ts)) =
      let !z' = foldl' (\z t' -> foldl' g z (PostOrder t')) (go t) ts
      in g z' x
{-# INLINE foldlMap1'PostOrder #-}

--------------------------------------------------------------------------------

-- Note [Implemented Foldable Tree functions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Implemented:
--
-- foldMap, foldr, foldl': Basic functions.
-- fold, elem: Implemented same as the default definition, but INLINABLE to
-- allow specialization.
-- foldr1, foldl1, null, maximum, minimum: Implemented more efficiently than
-- defaults since trees are non-empty.
-- sum, product: Implemented as strict left folds. Defaults use the lazy foldMap
-- before base 4.15.1.
--
-- Not implemented:
--
-- foldMap', toList, length: Defaults perform well.
-- foldr', foldl: Unlikely to be used.

-- Note [Implemented Foldable1 Tree functions]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- Implemented:
--
-- foldMap, foldrMap1, foldlMap1': Basic functions
-- foldMap1': Implemented same as the default definition, but INLINABLE to
-- allow specialization.
-- toNonEmpty, foldlMap1: Implemented more efficiently than default.
-- maximum, minimum: Uses Foldable's implementation.
--
-- Not implemented:
--
-- fold1, head: Defaults perform well.
-- foldrMap1': Unlikely to be used.

-- Note [pathsFromRoot implementation]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We use Okasaki's banker's queue for pathsFromRoot because it has some
-- desirable properties when the result is consumed lazily.
--
-- 1. Fully evaluating a node's NonEmpty takes O(d) time, where d is
--    the depth of the node. This is optimal.
-- 2. The elements in the NonEmpty are yielded lazily. Note that the worst case
--    time to yield an element is not O(1), i.e. it is only amortized O(1).
--    More than O(1) work is done when the next element requires forcing (++)
--    suspensions or reversing a rear list. For example, yielding the head has
--    to force O(log d) (++) and so takes O(log d) time.
-- 3. It builds up some beneficial sharing. It is not possible to share the
--    results since the lists have different ends, but we can share some
--    intermediate structures. Consider m sibling nodes at depth d. The front
--    list is shared between them in (front ++ rear1), (front ++ rear2), ...
--    (front + rearm). Forcing a prefix of front in one list can take arbitrary
--    amounts of time per element (total bounded by O(d)), but once it is
--    forced, front is memoized and doing the same for any of the siblings will
--    take O(1) per element.
--
-- Alternatives:
--
-- * Implement it like pathsToRoot and reverse the NonEmptys. This does satisfy
--   point 1 above. On 2 there's a trade-off, it costs a full O(d) to access the
--   head and O(1) per element after that. On 3 it compares poorly because there
--   is no sharing. Accessing the heads of m siblings will take O(dm) compared
--   to the current O(d + m).
-- * Use Okasaki's real-time queues. This would guarantee O(1) per element, but
--   has worse constant-factor overall and does not seem worth the trouble.
--
-- GHC base also uses a banker's queue for Data.List.inits. inits is similar
-- in nature to pathsFromRoot since a list is a tree where each node has one or
-- zero children.

{-# LANGUAGE CPP #-}
#include "containers.h"
{-# LANGUAGE BangPatterns #-}
#if __GLASGOW_HASKELL__
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
#endif
#ifdef DEFINE_PATTERN_SYNONYMS
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
#endif
#ifdef USE_ST_MONAD
{-# LANGUAGE RankNTypes #-}
#endif

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Graph
-- Copyright   :  (c) The University of Glasgow 2002
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Portability :  portable
--
-- = Finite Graphs
--
-- The @'Graph'@ type is an adjacency list representation of a finite, directed
-- graph with vertices of type @Int@.
--
-- The @'SCC'@ type represents a
-- <https://en.wikipedia.org/wiki/Strongly_connected_component strongly-connected component>
-- of a graph.
--
-- == Implementation
--
-- The implementation is based on
--
--   * David King and John Launchbury,
--     \"/Structuring Depth-First Search Algorithms in Haskell/\",
--     Proceedings of the 22nd ACM SIGPLAN-SIGACT Symposium on Principles of
--     Programming Languages, 344-354, 1995,
--     <https://doi.org/10.1145/199448.199530>.
--
-----------------------------------------------------------------------------

module Data.Graph (

    -- * Graphs
      Graph
    , Bounds
    , Edge
    , Vertex
    , Table

    -- ** Graph Construction
    , graphFromEdges
    , graphFromEdges'
    , buildG

    -- ** Graph Properties
    , vertices
    , edges
    , outdegree
    , indegree

    -- ** Graph Transformations
    , transposeG

    -- ** Graph Algorithms
    , dfs
    , dff
    , topSort
    , reverseTopSort
    , components
    , scc
    , bcc
    , reachable
    , path


    -- * Strongly Connected Components
    , SCC(..
#ifdef DEFINE_PATTERN_SYNONYMS
      , CyclicSCC
#endif
      )

    -- ** Construction
    , stronglyConnComp
    , stronglyConnCompR

    -- ** Conversion
    , flattenSCC
    , flattenSCC1
    , flattenSCCs

    -- * Trees
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

-- std interfaces
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
-- See Note [ Template Haskell Dependencies ]
import Language.Haskell.TH ()
#endif

-- Make sure we don't use Integer by mistake.
default ()

-------------------------------------------------------------------------
--                                                                      -
--      Strongly Connected Components
--                                                                      -
-------------------------------------------------------------------------

-- | Strongly connected component.
data SCC vertex
  = AcyclicSCC vertex
  -- ^ A single vertex that is not in any cycle.
  | NECyclicSCC {-# UNPACK #-} !(NonEmpty vertex)
  -- ^ A maximal set of mutually reachable vertices.
  --
  -- @since 0.7
  deriving ( Eq   -- ^ @since 0.5.9
           , Show -- ^ @since 0.5.9
           , Read -- ^ @since 0.5.9
           )

#ifdef DEFINE_PATTERN_SYNONYMS
-- | Partial pattern synonym for backward compatibility with @containers < 0.7@.
pattern CyclicSCC :: [vertex] -> SCC vertex
pattern CyclicSCC xs <- NECyclicSCC (NE.toList -> xs) where
  CyclicSCC [] = error "CyclicSCC: an argument cannot be an empty list"
  CyclicSCC (x : xs) = NECyclicSCC (x :| xs)

{-# COMPLETE AcyclicSCC, CyclicSCC #-}
#endif

#ifdef __GLASGOW_HASKELL__
-- | @since 0.5.9
deriving instance Data vertex => Data (SCC vertex)

-- | @since 0.5.9
deriving instance Generic1 SCC

-- | @since 0.5.9
deriving instance Generic (SCC vertex)

-- There is no instance Lift (NonEmpty v) before template-haskell-2.15.
#if MIN_VERSION_template_haskell(2,15,0)
-- | @since 0.6.6
deriving instance Lift vertex => Lift (SCC vertex)
#else
instance Lift vertex => Lift (SCC vertex) where
  lift (AcyclicSCC v) = [| AcyclicSCC v |]
  lift (NECyclicSCC (v :| vs)) = [| NECyclicSCC (v :| vs) |]
#endif

#endif

-- | @since 0.5.9
instance Eq1 SCC where
  liftEq eq (AcyclicSCC v1) (AcyclicSCC v2) = eq v1 v2
  liftEq eq (NECyclicSCC vs1) (NECyclicSCC vs2) = liftEq eq vs1 vs2
  liftEq _ _ _ = False
-- | @since 0.5.9
instance Show1 SCC where
  liftShowsPrec sp _sl d (AcyclicSCC v) = showsUnaryWith sp "AcyclicSCC" d v
  liftShowsPrec sp sl d (NECyclicSCC vs) = showsUnaryWith (liftShowsPrec sp sl) "NECyclicSCC" d vs
-- | @since 0.5.9
instance Read1 SCC where
  liftReadsPrec rp rl = readsData $
    readsUnaryWith rp "AcyclicSCC" AcyclicSCC <>
    readsUnaryWith (liftReadsPrec rp rl) "NECyclicSCC" NECyclicSCC
#ifdef __GLASGOW_HASKELL__
    <> readsUnaryWith (const rl) "CyclicSCC" CyclicSCC
#endif

-- | @since 0.5.9
instance F.Foldable SCC where
  foldr c n (AcyclicSCC v) = c v n
  foldr c n (NECyclicSCC vs) = foldr c n vs

  toList = flattenSCC

#if MIN_VERSION_base(4,18,0)
-- | @since 0.7
instance F1.Foldable1 SCC where
  foldMap1 f (AcyclicSCC v) = f v
  foldMap1 f (NECyclicSCC vs) = F1.foldMap1 f vs

  toNonEmpty = flattenSCC1

  -- TODO define more methods
#endif

-- | @since 0.5.9
instance Traversable SCC where
  traverse f (AcyclicSCC vertex) = AcyclicSCC <$> f vertex
  -- Avoid traverse from instance Traversable NonEmpty,
  -- it is redundantly lazy.
  traverse f (NECyclicSCC (x :| xs)) =
    liftA2 (\x' xs' -> NECyclicSCC (x' :| xs')) (f x) (traverse f xs)

instance NFData a => NFData (SCC a) where
    rnf (AcyclicSCC v) = rnf v
    rnf (NECyclicSCC vs) = rnf vs

-- | @since 0.8
instance NFData1 SCC where
    liftRnf rnfx (AcyclicSCC v)   = rnfx v
    liftRnf rnfx (NECyclicSCC vs) = liftRnf rnfx vs

-- | @since 0.5.4
instance Functor SCC where
    fmap f (AcyclicSCC v) = AcyclicSCC (f v)
    -- Avoid fmap from instance Functor NonEmpty,
    -- it is redundantly lazy.
    fmap f (NECyclicSCC (x :| xs)) = NECyclicSCC (f x :| map f xs)

-- | The vertices of a list of strongly connected components.
flattenSCCs :: [SCC a] -> [a]
flattenSCCs = concatMap flattenSCC

-- | The vertices of a strongly connected component.
--
-- @flattenSCC = 'Data.List.NonEmpty.toList' . 'flattenSCC1'@.
--
-- This function is retained for backward compatibility,
-- 'flattenSCC1' has the more precise type.
flattenSCC :: SCC vertex -> [vertex]
flattenSCC (AcyclicSCC v) = [v]
flattenSCC (NECyclicSCC (v :| vs)) = v : vs
-- Note: Best to avoid NE.toList, it is too lazy.

-- | The vertices of a strongly connected component.
--
-- @since 0.8
flattenSCC1 :: SCC vertex -> NonEmpty vertex
flattenSCC1 (AcyclicSCC v) = v :| []
flattenSCC1 (NECyclicSCC vs) = vs

-- | \(O((V+E) \log V)\). The strongly connected components of a directed graph,
-- reverse topologically sorted.
--
-- ==== __Examples__
--
-- > stronglyConnComp [("a",0,[1]),("b",1,[2,3]),("c",2,[1]),("d",3,[3])]
-- >   == [CyclicSCC ["d"],CyclicSCC ["b","c"],AcyclicSCC "a"]
stronglyConnComp
        :: Ord key
        => [(node, key, [key])]
                -- ^ The graph: a list of nodes uniquely identified by keys,
                -- with a list of keys of nodes this node has edges to.
                -- The out-list may contain keys that don't correspond to
                -- nodes of the graph; such edges are ignored.
        -> [SCC node]

stronglyConnComp edges0
  = map get_node (stronglyConnCompR edges0)
  where
    get_node (AcyclicSCC (n, _, _)) = AcyclicSCC n
    get_node (NECyclicSCC ((n0, _, _) :| triples)) =
      NECyclicSCC (n0 :| [n | (n, _, _) <- triples])
{-# INLINABLE stronglyConnComp #-}

-- | \(O((V+E) \log V)\). The strongly connected components of a directed graph,
-- reverse topologically sorted.  The function is the same as
-- 'stronglyConnComp', except that all the information about each node retained.
-- This interface is used when you expect to apply 'SCC' to
-- (some of) the result of 'SCC', so you don't want to lose the
-- dependency information.
--
-- ==== __Examples__
--
-- > stronglyConnCompR [("a",0,[1]),("b",1,[2,3]),("c",2,[1]),("d",3,[3])]
-- >  == [CyclicSCC [("d",3,[3])],CyclicSCC [("b",1,[2,3]),("c",2,[1])],AcyclicSCC ("a",0,[1])]
stronglyConnCompR
        :: Ord key
        => [(node, key, [key])]
                -- ^ The graph: a list of nodes uniquely identified by keys,
                -- with a list of keys of nodes this node has edges to.
                -- The out-list may contain keys that don't correspond to
                -- nodes of the graph; such edges are ignored.
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
{-# INLINABLE stronglyConnCompR #-}

-------------------------------------------------------------------------
--                                                                      -
--      Graphs
--                                                                      -
-------------------------------------------------------------------------

-- | Abstract representation of vertices.
type Vertex  = Int
-- | Table indexed by a contiguous set of vertices.
--
-- /Note: This is included for backwards compatibility./
type Table a = Array Vertex a
-- | Adjacency list representation of a graph, mapping each vertex to its
-- list of successors.
type Graph   = Array Vertex [Vertex]
-- | The bounds of an @Array@.
type Bounds  = (Vertex, Vertex)
-- | An edge from the first vertex to the second.
type Edge    = (Vertex, Vertex)

#if !USE_UNBOXED_ARRAYS
type UArray i a = Array i a
#endif

-- | \(O(V)\). Returns the list of vertices in the graph.
--
-- ==== __Examples__
--
-- > vertices (buildG (0,-1) []) == []
--
-- > vertices (buildG (0,2) [(0,1),(1,2)]) == [0,1,2]
vertices :: Graph -> [Vertex]
vertices  = indices
-- See Note [Inline for fusion]
{-# INLINE vertices #-}

-- | \(O(V+E)\). Returns the list of edges in the graph.
--
-- ==== __Examples__
--
-- > edges (buildG (0,-1) []) == []
--
-- > edges (buildG (0,2) [(0,1),(1,2)]) == [(0,1),(1,2)]
edges    :: Graph -> [Edge]
edges g   = [ (v, w) | v <- vertices g, w <- g!v ]
-- See Note [Inline for fusion]
{-# INLINE edges #-}

-- | \(O(V+E)\). Build a graph from a list of edges.
--
-- Warning: This function will cause a runtime exception if a vertex in the edge
-- list is not within the given @Bounds@.
--
-- ==== __Examples__
--
-- > buildG (0,-1) [] == array (0,-1) []
-- > buildG (0,2) [(0,1), (1,2)] == array (0,1) [(0,[1]),(1,[2])]
-- > buildG (0,2) [(0,1), (0,2), (1,2)] == array (0,2) [(0,[2,1]),(1,[2]),(2,[])]
buildG :: Bounds -> [Edge] -> Graph
buildG = accumArray (flip (:)) []
-- See Note [Inline for fusion]
{-# INLINE buildG #-}

-- | \(O(V+E)\). The graph obtained by reversing all edges.
--
-- ==== __Examples__
--
-- > transposeG (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,[]),(1,[0]),(2,[1])]
transposeG  :: Graph -> Graph
transposeG g = buildG (bounds g) (reverseE g)

reverseE    :: Graph -> [Edge]
reverseE g   = [ (w, v) | (v, w) <- edges g ]
-- See Note [Inline for fusion]
{-# INLINE reverseE #-}

-- | \(O(V+E)\). A table of the count of edges from each node.
--
-- ==== __Examples__
--
-- > outdegree (buildG (0,-1) []) == array (0,-1) []
--
-- > outdegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,1),(1,1),(2,0)]
outdegree :: Graph -> Array Vertex Int
-- This is bizarrely lazy. We build an array filled with thunks, instead
-- of actually calculating anything. This is the historical behavior, and I
-- suppose someone *could* be relying on it, but it might be worth finding
-- out. Note that we *can't* be so lazy with indegree.
outdegree  = fmap length

-- | \(O(V+E)\). A table of the count of edges into each node.
--
-- ==== __Examples__
--
-- > indegree (buildG (0,-1) []) == array (0,-1) []
--
-- > indegree (buildG (0,2) [(0,1), (1,2)]) == array (0,2) [(0,0),(1,1),(2,1)]
indegree :: Graph -> Array Vertex Int
indegree g = accumArray (+) 0 (bounds g) [(v, 1) | (_, outs) <- assocs g, v <- outs]

-- | \(O((V+E) \log V)\). Identical to 'graphFromEdges', except that the return
-- value does not include the function which maps keys to vertices. This
-- version of 'graphFromEdges' is for backwards compatibility.
graphFromEdges'
        :: Ord key
        => [(node, key, [key])]
        -> (Graph, Vertex -> (node, key, [key]))
graphFromEdges' x = (a,b) where
    (a,b,_) = graphFromEdges x
{-# INLINABLE graphFromEdges' #-}

-- | \(O((V+E) \log V)\). Build a graph from a list of nodes uniquely identified
-- by keys, with a list of keys of nodes this node should have edges to.
--
-- This function takes an adjacency list representing a graph with vertices of
-- type @key@ labeled by values of type @node@ and produces a @Graph@-based
-- representation of that list. The @Graph@ result represents the /shape/ of the
-- graph, and the functions describe a) how to retrieve the label and adjacent
-- vertices of a given vertex, and b) how to retrieve a vertex given a key.
--
-- @(graph, nodeFromVertex, vertexFromKey) = graphFromEdges edgeList@
--
-- * @graph :: Graph@ is the raw, array based adjacency list for the graph.
-- * @nodeFromVertex :: Vertex -> (node, key, [key])@ returns the node
--   associated with the given 0-based @Int@ vertex; see /warning/ below. This
--   runs in \(O(1)\) time.
-- * @vertexFromKey :: key -> Maybe Vertex@ returns the @Int@ vertex for the
--   key if it exists in the graph, @Nothing@ otherwise. This runs in
--   \(O(\log V)\) time.
--
-- To safely use this API you must either extract the list of vertices directly
-- from the graph or first call @vertexFromKey k@ to check if a vertex
-- corresponds to the key @k@. Once it is known that a vertex exists you can use
-- @nodeFromVertex@ to access the labelled node and adjacent vertices. See below
-- for examples.
--
-- Note: The out-list may contain keys that don't correspond to nodes of the
-- graph; they are ignored.
--
-- Warning: The @nodeFromVertex@ function will cause a runtime exception if the
-- given @Vertex@ does not exist.
--
-- ==== __Examples__
--
-- An empty graph.
--
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges []
-- > graph = array (0,-1) []
--
-- A graph where the out-list references unspecified nodes (@\'c\'@), these are
-- ignored.
--
-- > (graph, _, _) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c'])]
-- > array (0,1) [(0,[1]),(1,[])]
--
--
-- A graph with 3 vertices: ("a") -> ("b") -> ("c")
--
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
-- > graph == array (0,2) [(0,[1]),(1,[2]),(2,[])]
-- > nodeFromVertex 0 == ("a",'a',"b")
-- > vertexFromKey 'a' == Just 0
--
-- Get the label for a given key.
--
-- > let getNodePart (n, _, _) = n
-- > (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [("a", 'a', ['b']), ("b", 'b', ['c']), ("c", 'c', [])]
-- > getNodePart . nodeFromVertex <$> vertexFromKey 'a' == Just "A"
--
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

    -- key_vertex :: key -> Maybe Vertex
    --  returns Nothing for non-interesting vertices
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
{-# INLINABLE graphFromEdges #-}

-------------------------------------------------------------------------
--                                                                      -
--      Depth first search
--                                                                      -
-------------------------------------------------------------------------

-- | \(O(V+E)\). A spanning forest of the graph, obtained from a depth-first
-- search of the graph starting from each vertex in an unspecified order.
dff          :: Graph -> [Tree Vertex]
dff g         = dfs g (vertices g)

-- | \(O(V+E)\). A spanning forest of the part of the graph reachable from the
-- listed vertices, obtained from a depth-first search of the graph starting at
-- each of the listed vertices in order.

-- This function deviates from King and Launchbury's implementation by
-- bundling together the functions generate, prune, and chop for efficiency
-- reasons.
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

-- Use the ST monad if available, for constant-time primitives.

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
{-# INLINE run #-}

#else /* !USE_ST_MONAD */

-- Portable implementation using IntSet.

newtype SetM a = SetM { runSetM :: IntSet -> (a, IntSet) }

instance Monad SetM where
    SetM v >>= f = SetM $ \s -> case v s of (x, s') -> runSetM (f x) s'

instance Functor SetM where
    f `fmap` SetM v = SetM $ \s -> case v s of (x, s') -> (f x, s')
    {-# INLINE fmap #-}

instance Applicative SetM where
    pure x = SetM $ \s -> (x, s)
    {-# INLINE pure #-}
    SetM f <*> SetM v = SetM $ \s -> case f s of (k, s') -> case v s' of (x, s'') -> (k x, s'')
    {-# INLINE (<*>) #-}

run :: Bounds -> ((Vertex -> SetM Bool) -> (Vertex -> SetM ()) -> SetM a) -> a
run _ f = fst (runSetM (f contains include) Set.empty)
  where
    contains v = SetM $ \m -> (Set.member v m, m)
    include v = SetM $ \m -> ((), Set.insert v m)

#endif /* !USE_ST_MONAD */

-------------------------------------------------------------------------
--                                                                      -
--      Algorithms
--                                                                      -
-------------------------------------------------------------------------

------------------------------------------------------------
-- Algorithm 1: depth first search numbering
------------------------------------------------------------

preorder' :: Tree a -> [a] -> [a]
preorder' (Node a ts) = (a :) . preorderF' ts

preorderF' :: [Tree a] -> [a] -> [a]
preorderF' ts = foldr (.) id $ map preorder' ts

preorderF :: [Tree a] -> [a]
preorderF ts = preorderF' ts []

tabulate        :: Bounds -> [Vertex] -> UArray Vertex Int
tabulate bnds vs = UA.array bnds (zipWith (flip (,)) [1..] vs)
-- Why zipWith (flip (,)) instead of just using zip with the
-- arguments in the other order? We want the [1..] to fuse
-- away, and these days that only happens when it's the first
-- list argument.

preArr          :: Bounds -> [Tree Vertex] -> UArray Vertex Int
preArr bnds      = tabulate bnds . preorderF

------------------------------------------------------------
-- Algorithm 2: topological sorting
------------------------------------------------------------

postorder :: Tree a -> [a] -> [a]
postorder (Node a ts) = postorderF ts . (a :)

postorderF   :: [Tree a] -> [a] -> [a]
postorderF ts = foldr (.) id $ map postorder ts

postOrd :: Graph -> [Vertex]
postOrd g = postorderF (dff g) []

-- | \(O(V+E)\). A topological sort of the graph.
-- The order is partially specified by the condition that a vertex /i/
-- precedes /j/ whenever /j/ is reachable from /i/ but not vice versa.
--
-- Note: A topological sort exists only when there are no cycles in the graph.
-- If the graph has cycles, the output of this function will not be a
-- topological sort. In such a case consider using 'scc'.
topSort      :: Graph -> [Vertex]
topSort       = reverse . postOrd

-- | \(O(V+E)\). Reverse ordering of `topSort`.
--
-- See note in 'topSort'.
--
-- @since 0.6.4
reverseTopSort :: Graph -> [Vertex]
reverseTopSort = postOrd

------------------------------------------------------------
-- Algorithm 3: connected components
------------------------------------------------------------

-- | \(O(V+E)\). The connected components of a graph.
-- Two vertices are connected if there is a path between them, traversing
-- edges in either direction.
components   :: Graph -> [Tree Vertex]
components    = dff . undirected

undirected   :: Graph -> Graph
undirected g  = buildG (bounds g) (edges g ++ reverseE g)

-- Algorithm 4: strongly connected components

-- | \(O(V+E)\). The strongly connected components of a graph, in reverse
-- topological order.
--
-- ==== __Examples__
--
-- > scc (buildG (0,3) [(3,1),(1,2),(2,0),(0,1)])
-- >   == [Node {rootLabel = 0, subForest = [Node {rootLabel = 1, subForest = [Node {rootLabel = 2, subForest = []}]}]}
-- >      ,Node {rootLabel = 3, subForest = []}]

scc  :: Graph -> [Tree Vertex]
scc g = dfs g (reverse (postOrd (transposeG g)))

------------------------------------------------------------
-- Algorithm 5: Classifying edges
------------------------------------------------------------

{-
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
-}

------------------------------------------------------------
-- Algorithm 6: Finding reachable vertices
------------------------------------------------------------

-- | \(O(V+E)\). Returns the list of vertices reachable from a given vertex.
--
-- ==== __Examples__
--
-- > reachable (buildG (0,0) []) 0 == [0]
--
-- > reachable (buildG (0,2) [(0,1), (1,2)]) 0 == [0,1,2]
reachable :: Graph -> Vertex -> [Vertex]
reachable g v = preorderF (dfs g [v])

-- | \(O(V+E)\). Returns @True@ if the second vertex reachable from the first.
--
-- ==== __Examples__
--
-- > path (buildG (0,0) []) 0 0 == True
--
-- > path (buildG (0,2) [(0,1), (1,2)]) 0 2 == True
--
-- > path (buildG (0,2) [(0,1), (1,2)]) 2 0 == False
path :: Graph -> Vertex -> Vertex -> Bool
path g v w    = w `elem` (reachable g v)

------------------------------------------------------------
-- Algorithm 7: Biconnected components
------------------------------------------------------------

-- | \(O(V+E)\). The biconnected components of a graph.
-- An undirected graph is biconnected if the deletion of any vertex
-- leaves it connected.
--
-- The input graph is expected to be undirected, i.e. for every edge in the
-- graph the reverse edge is also in the graph. If the graph is not undirected
-- the output is arbitrary.
bcc :: Graph -> [Tree [Vertex]]
bcc g = concatMap bicomps forest
  where
    -- The algorithm here is the same as given by King and Launchbury, which is
    -- an adaptation of Hopcroft and Tarjan's. The implementation, however, has
    -- been modified from King and Launchbury to make it efficient.

    forest = dff g

    -- dnum!v is the index of vertex v in the dfs preorder of vertices
    dnum = preArr (bounds g) forest

    -- Wraps up the component of every child of the root
    bicomps :: Tree Vertex -> [Tree [Vertex]]
    bicomps (Node v tws) =
      [Node (v : curw []) (donew []) | (_, curw, donew) <- map collect tws]

    -- Returns a triple of
    -- * lowpoint of v
    -- * difference list of vertices in v's component
    -- * difference list of trees of components, whose root components are
    --   adjacent to v's component
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

--------------------------------------------------------------------------------

-- Note [Inline for fusion]
-- ~~~~~~~~~~~~~~~~~~~~~~~~
--
-- We inline simple functions that produce or consume lists so that list fusion
-- can fire. transposeG is a function where this is particularly useful; it has
-- two intermediate lists in its definition which get fused away.

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.ByteString
-- Copyright   : (c) The University of Glasgow 2001,
--               (c) David Roundy 2003-2005,
--               (c) Simon Marlow 2005,
--               (c) Bjorn Bringert 2006,
--               (c) Don Stewart 2005-2008,
--               (c) Duncan Coutts 2006-2013
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- A time- and space-efficient implementation of byte vectors using
-- packed Word8 arrays, suitable for high performance use, both in terms
-- of large data quantities and high speed requirements. Byte vectors
-- are encoded as strict 'Word8' arrays of bytes, held in a 'ForeignPtr',
-- and can be passed between C and Haskell with little effort.
--
-- The recomended way to assemble ByteStrings from smaller parts
-- is to use the builder monoid from "Data.ByteString.Builder".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'ForeignPtr' by David Roundy.
-- Rewritten again and extended by Don Stewart and Duncan Coutts.
--

module Data.ByteString (

        -- * Strict @ByteString@
        ByteString,
        StrictByteString,

        -- ** Heap fragmentation
        -- | With GHC, the 'ByteString' representation uses /pinned memory/,
        -- meaning it cannot be moved by GC. While this is ideal for use with
        -- the foreign function interface and is usually efficient, this
        -- representation may lead to issues with heap fragmentation and wasted
        -- space if the program selectively retains a fraction of many small
        -- 'ByteString's, keeping them live in memory over long durations.
        --
        -- While 'ByteString' is indispensable when working with large blobs of
        -- data and especially when interfacing with native C libraries, be sure
        -- to also check the 'Data.ByteString.Short.ShortByteString' type.
        -- As a type backed by /unpinned/ memory, @ShortByteString@ behaves
        -- similarly to @Text@ (from the @text@ package) on the heap, completely
        -- avoids fragmentation issues, and in many use-cases may better suit
        -- your bytestring-storage needs.

        -- * Introducing and eliminating 'ByteString's
        empty,
        singleton,
        pack,
        unpack,
        fromStrict,
        toStrict,
        fromFilePath,
        toFilePath,

        -- * Basic interface
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

        -- * Transforming ByteStrings
        map,
        reverse,
        intersperse,
        intercalate,
        transpose,

        -- * Reducing 'ByteString's (folds)
        foldl,
        foldl',
        foldl1,
        foldl1',

        foldr,
        foldr',
        foldr1,
        foldr1',

        -- ** Special folds
        concat,
        concatMap,
        any,
        all,
        maximum,
        minimum,

        -- * Building ByteStrings
        -- ** Scans
        scanl,
        scanl1,
        scanr,
        scanr1,

        -- ** Accumulating maps
        mapAccumL,
        mapAccumR,

        -- ** Generating and unfolding ByteStrings
        replicate,
        unfoldr,
        unfoldrN,

        -- * Substrings

        -- ** Breaking strings
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

        -- ** Breaking into many substrings
        split,
        splitWith,

        -- * Predicates
        isPrefixOf,
        isSuffixOf,
        isInfixOf,

        -- ** Encoding validation
        isValidUtf8,

        -- ** Search for arbitrary substrings
        breakSubstring,

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,
        notElem,

        -- ** Searching with a predicate
        find,
        filter,
        partition,

        -- * Indexing ByteStrings
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

        -- * Zipping and unzipping ByteStrings
        zip,
        zipWith,
        packZipWith,
        unzip,

        -- * Ordered ByteStrings
        sort,

        -- * Low level conversions
        -- ** Copying ByteStrings
        copy,

        -- ** Packing 'CString's and pointers
        packCString,
        packCStringLen,

        -- ** Using ByteStrings as 'CString's
        useAsCString,
        useAsCStringLen,

        -- * I\/O with 'ByteString's

        -- ** Standard input and output
        getLine,
        getContents,
        putStr,
        interact,

        -- ** Files
        readFile,
        writeFile,
        appendFile,

        -- ** I\/O with Handles
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

-- hGetBuf and hPutBuf not available in yhc or nhc
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

-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ByteString's

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
-- Taking a slice of some static data rather than allocating a new
-- buffer for each call is nice for several reasons. Since it doesn't
-- involve any side effects hidden in a 'GHC.Magic.runRW#' call, it
-- can be simplified to a constructor application. This may enable GHC
-- to perform further optimizations after inlining, and also causes a
-- fresh singleton to take only 4 words of heap space instead of 9.
-- (The buffer object itself would take up 3 words: header, size, and
-- 1 word of content. The ForeignPtrContents object used to keep the
-- buffer alive would need two more.)
singleton c = unsafeTake 1 $ unsafeDrop (fromIntegral c) allBytes
{-# INLINE singleton #-}

-- | A static blob of all possible bytes (0x00 to 0xff) in order
allBytes :: ByteString
allBytes = unsafePackLenLiteral 0x100
  "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0a\x0b\x0c\x0d\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37\x38\x39\x3a\x3b\x3c\x3d\x3e\x3f\x40\x41\x42\x43\x44\x45\x46\x47\x48\x49\x4a\x4b\x4c\x4d\x4e\x4f\x50\x51\x52\x53\x54\x55\x56\x57\x58\x59\x5a\x5b\x5c\x5d\x5e\x5f\x60\x61\x62\x63\x64\x65\x66\x67\x68\x69\x6a\x6b\x6c\x6d\x6e\x6f\x70\x71\x72\x73\x74\x75\x76\x77\x78\x79\x7a\x7b\x7c\x7d\x7e\x7f\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff"#

-- | /O(n)/ Convert a @['Word8']@ into a 'ByteString'.
--
-- For applications with large numbers of string literals, 'pack' can be a
-- bottleneck. In such cases, consider using 'unsafePackAddress' (GHC only).
pack :: [Word8] -> ByteString
pack = packBytes

-- | /O(n)/ Converts a 'ByteString' to a @['Word8']@.
unpack :: ByteString -> [Word8]
unpack bs = build (unpackFoldr bs)
{-# INLINE unpack #-}

--
-- Have unpack fuse with good list consumers
--
unpackFoldr :: ByteString -> (Word8 -> a -> a) -> a -> a
unpackFoldr bs k z = foldr k z bs
{-# INLINE [0] unpackFoldr #-}

{-# RULES
"ByteString unpack-list" [1]  forall bs .
    unpackFoldr bs (:) [] = unpackBytes bs
 #-}

-- | Convert a 'FilePath' to a 'ByteString'.
--
-- The 'FilePath' type is expected to use the file system encoding
-- as reported by 'GHC.IO.Encoding.getFileSystemEncoding'. This
-- encoding allows for round-tripping of arbitrary data on platforms
-- that allow arbitrary bytes in their paths. This conversion
-- function does the same thing that `System.IO.openFile` would
-- do when decoding the 'FilePath'.
--
-- This function is in 'IO' because the file system encoding can be
-- changed. If the encoding can be assumed to be constant in your
-- use case, you may invoke this function via 'unsafePerformIO'.
--
-- @since 0.11.2.0
fromFilePath :: FilePath -> IO ByteString
fromFilePath path = do
    enc <- getFileSystemEncoding
    newCStringLen enc path >>= unsafePackMallocCStringLen

-- | Convert a 'ByteString' to a 'FilePath'.
--
-- This function uses the file system encoding, and resulting 'FilePath's
-- can be safely used with standard IO functions and will reference the
-- correct path in the presence of arbitrary non-UTF-8 encoded paths.
--
-- This function is in 'IO' because the file system encoding can be
-- changed. If the encoding can be assumed to be constant in your
-- use case, you may invoke this function via 'unsafePerformIO'.
--
-- @since 0.11.2.0
toFilePath :: ByteString -> IO FilePath
toFilePath path = do
    enc <- getFileSystemEncoding
    useAsCStringLen path (peekCStringLen enc)

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null (BS _ l) = assert (l >= 0) $ l <= 0
{-# INLINE null #-}

-- ---------------------------------------------------------------------
-- | /O(1)/ 'length' returns the length of a ByteString as an 'Int'.
length :: ByteString -> Int
length (BS _ l) = assert (l >= 0) l
{-# INLINE length #-}

------------------------------------------------------------------------

infixr 5 `cons` --same as list (:)
infixl 5 `snoc`

-- | /O(n)/ 'cons' is analogous to (:) for lists, but of different
-- complexity, as it requires making a copy.
cons :: Word8 -> ByteString -> ByteString
cons c (BS x len) = unsafeCreateFp (checkedAdd "cons" len 1) $ \p -> do
        pokeFp p c
        memcpyFp (p `plusForeignPtr` 1) x len
{-# INLINE cons #-}

-- | /O(n)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc (BS x len) c = unsafeCreateFp (checkedAdd "snoc" len 1) $ \p -> do
        memcpyFp p x len
        pokeFp (p `plusForeignPtr` len) c
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'uncons' instead.
head :: HasCallStack => ByteString -> Word8
head (BS x l)
    | l <= 0    = errorEmptyList "head"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> peek p
{-# INLINE head #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be non-empty.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'uncons' instead.
tail :: HasCallStack => ByteString -> ByteString
tail (BS p l)
    | l <= 0    = errorEmptyList "tail"
    | otherwise = BS (plusForeignPtr p 1) (l-1)
{-# INLINE tail #-}

-- | /O(1)/ Extract the 'head' and 'tail' of a ByteString, returning 'Nothing'
-- if it is empty.
uncons :: ByteString -> Maybe (Word8, ByteString)
uncons (BS x l)
    | l <= 0    = Nothing
    | otherwise = Just (accursedUnutterablePerformIO $ unsafeWithForeignPtr x
                                                     $ \p -> peek p,
                        BS (plusForeignPtr x 1) (l-1))
{-# INLINE uncons #-}

-- | /O(1)/ Extract the last element of a ByteString, which must be finite and non-empty.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'unsnoc' instead.
last :: HasCallStack => ByteString -> Word8
last ps@(BS x l)
    | null ps   = errorEmptyList "last"
    | otherwise = accursedUnutterablePerformIO $
                    unsafeWithForeignPtr x $ \p -> peekByteOff p (l-1)
{-# INLINE last #-}

-- | /O(1)/ Returns all the elements of a 'ByteString' except the last one.
-- An exception will be thrown in the case of an empty ByteString.
--
-- This is a partial function, consider using 'unsnoc' instead.
init :: HasCallStack => ByteString -> ByteString
init ps@(BS p l)
    | null ps   = errorEmptyList "init"
    | otherwise = BS p (l-1)
{-# INLINE init #-}

-- | /O(1)/ Extract the 'init' and 'last' of a ByteString, returning 'Nothing'
-- if it is empty.
unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc (BS x l)
    | l <= 0    = Nothing
    | otherwise = Just (BS x (l-1),
                        accursedUnutterablePerformIO $
                          unsafeWithForeignPtr x $ \p -> peekByteOff p (l-1))
{-# INLINE unsnoc #-}

-- | /O(n)/ Append two ByteStrings
append :: ByteString -> ByteString -> ByteString
append = mappend
{-# INLINE append #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
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
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ efficiently returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
reverse (BS x l) = unsafeCreateFp l $ \fp ->
  unsafeWithForeignPtr fp $ \p ->
    unsafeWithForeignPtr x  $ \f ->
      c_reverse p f (fromIntegral l)

-- | /O(n)/ The 'intersperse' function takes a 'Word8' and a
-- 'ByteString' and \`intersperses\' that byte between the elements of
-- the 'ByteString'.  It is analogous to the intersperse function on
-- Lists.
intersperse :: Word8 -> ByteString -> ByteString
intersperse c ps@(BS x l)
    | length ps < 2  = ps
    | otherwise      = unsafeCreateFp (2*l-1) $ \fp ->
      unsafeWithForeignPtr fp $ \p ->
        unsafeWithForeignPtr x $ \f ->
          c_intersperse p f (fromIntegral l) c

-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose = P.map pack . List.transpose . P.map unpack

-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
--
foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f z = \(BS fp len) ->
  let
    end = unsafeForeignPtrToPtr fp `plusPtr` (-1)
    -- not tail recursive; traverses array right to left
    go !p | p == end  = z
          | otherwise = let !x = accursedUnutterablePerformIO $ do
                                   x' <- peek p
                                   touchForeignPtr fp
                                   return x'
                        in f (go (p `plusPtr` (-1))) x

  in
    go (end `plusPtr` len)
{-# INLINE foldl #-}

{-
Note [fold inlining]:

GHC will only inline a function marked INLINE
if it is fully saturated (meaning the number of
arguments provided at the call site is at least
equal to the number of lhs arguments).

-}
-- | 'foldl'' is like 'foldl', but strict in the accumulator.
--
foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f v = \(BS fp len) ->
          -- see fold inlining
  let
    g ptr = go v ptr
      where
        end  = ptr `plusForeignPtr` len
        -- tail recursive; traverses array left to right
        go !z !p | p == end  = return z
                 | otherwise = do x <- peekFp p
                                  go (f z x) (p `plusForeignPtr` 1)
  in
    accursedUnutterablePerformIO $ g fp
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k z = \(BS fp len) ->
          -- see fold inlining
  let
    ptr = unsafeForeignPtrToPtr fp
    end = ptr `plusPtr` len
    -- not tail recursive; traverses array left to right
    go !p | p == end  = z
          | otherwise = let !x = accursedUnutterablePerformIO $ do
                                   x' <- peek p
                                   touchForeignPtr fp
                                   return x'
                         in k x (go (p `plusPtr` 1))
  in
    go ptr
{-# INLINE foldr #-}

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' k v = \(BS fp len) ->
          -- see fold inlining
  let
    g ptr = go v (end `plusForeignPtr` len)
      where
        end = ptr `plusForeignPtr` (-1)
        -- tail recursive; traverses array right to left
        go !z !p | p == end  = return z
                 | otherwise = do x <- peekFp p
                                  go (k x z) (p `plusForeignPtr` (-1))
  in
    accursedUnutterablePerformIO $ g fp

{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteString's.
-- An exception will be thrown in the case of an empty ByteString.
foldl1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 f ps = case uncons ps of
  Nothing     -> errorEmptyList "foldl1"
  Just (h, t) -> foldl f h t
{-# INLINE foldl1 #-}

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
-- An exception will be thrown in the case of an empty ByteString.
foldl1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' f ps = case uncons ps of
  Nothing     -> errorEmptyList "foldl1'"
  Just (h, t) -> foldl' f h t
{-# INLINE foldl1' #-}

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
-- An exception will be thrown in the case of an empty ByteString.
foldr1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 f ps = case unsnoc ps of
  Nothing -> errorEmptyList "foldr1"
  Just (b, c) -> foldr f c b
{-# INLINE foldr1 #-}

-- | 'foldr1'' is a variant of 'foldr1', but is strict in the
-- accumulator.
foldr1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' f ps = case unsnoc ps of
  Nothing -> errorEmptyList "foldr1'"
  Just (b, c) -> foldr' f c b
{-# INLINE foldr1' #-}

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat = mconcat

-- | Map a function over a 'ByteString' and concatenate the results
concatMap :: (Word8 -> ByteString) -> ByteString -> ByteString
concatMap f = concat . foldr ((:) . f) []

-- foldr (append . f) empty

-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
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
{-# INLINE [1] any #-}

{-# RULES
"ByteString specialise any (x ==)" forall x.
    any (x `eqWord8`) = anyByte x
"ByteString specialise any (== x)" forall x.
    any (`eqWord8` x) = anyByte x
  #-}

-- | Is any element of 'ByteString' equal to c?
anyByte :: Word8 -> ByteString -> Bool
anyByte c (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    q <- memchr p c (fromIntegral l)
    return $! q /= nullPtr
{-# INLINE anyByte #-}

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
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
{-# INLINE [1] all #-}

{-# RULES
"ByteString specialise all (x /=)" forall x.
    all (x `neWord8`) = not . anyByte x
"ByteString specialise all (/= x)" forall x.
    all (`neWord8` x) = not . anyByte x
  #-}

------------------------------------------------------------------------

-- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
-- An exception will be thrown in the case of an empty ByteString.
maximum :: HasCallStack => ByteString -> Word8
maximum xs@(BS x l)
    | null xs   = errorEmptyList "maximum"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
                      c_maximum p (fromIntegral l)
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
-- An exception will be thrown in the case of an empty ByteString.
minimum :: HasCallStack => ByteString -> Word8
minimum xs@(BS x l)
    | null xs   = errorEmptyList "minimum"
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
                      c_minimum p (fromIntegral l)
{-# INLINE minimum #-}

------------------------------------------------------------------------

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f acc = \(BS a len) -> unsafeDupablePerformIO $ do
               -- see fold inlining
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
{-# INLINE mapAccumL #-}

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f acc = \(BS a len) -> unsafeDupablePerformIO $ do
               -- see fold inlining
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
{-# INLINE mapAccumR #-}

-- ---------------------------------------------------------------------
-- Building ByteStrings

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > head (scanl f z xs) == z
-- > last (scanl f z xs) == foldl f z xs
--
scanl
    :: (Word8 -> Word8 -> Word8)
    -- ^ accumulator -> element -> new accumulator
    -> Word8
    -- ^ starting value of accumulator
    -> ByteString
    -- ^ input of length n
    -> ByteString
    -- ^ output of length n+1
scanl f v = \(BS a len) -> unsafeCreateFp (checkedAdd "scanl" len 1) $ \q -> do
         -- see fold inlining
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
{-# INLINE scanl #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 f ps = case uncons ps of
  Nothing     -> empty
  Just (h, t) -> scanl f h t
{-# INLINE scanl1 #-}

-- | 'scanr' is similar to 'foldr', but returns a list of successive
-- reduced values from the right.
--
-- > scanr f z [..., x{n-1}, xn] == [..., x{n-1} `f` (xn `f` z), xn `f` z, z]
--
-- Note that
--
-- > head (scanr f z xs) == foldr f z xs
-- > last (scanr f z xs) == z
--
scanr
    :: (Word8 -> Word8 -> Word8)
    -- ^ element -> accumulator -> new accumulator
    -> Word8
    -- ^ starting value of accumulator
    -> ByteString
    -- ^ input of length n
    -> ByteString
    -- ^ output of length n+1
scanr f v = \(BS a len) -> unsafeCreateFp (checkedAdd "scanr" len 1) $ \b -> do
         -- see fold inlining
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
{-# INLINE scanr #-}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 f ps = case unsnoc ps of
  Nothing -> empty
  Just (b, c) -> scanr f c b
{-# INLINE scanr1 #-}

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | /O(n)/ 'replicate' @n x@ is a ByteString of length @n@ with @x@
-- the value of every element. The following holds:
--
-- > replicate w c = fst (unfoldrN w (\u -> Just (u,u)) c)
replicate :: Int -> Word8 -> ByteString
replicate w c
    | w <= 0    = empty
    | otherwise = unsafeCreateFp w $ \fptr ->
        unsafeWithForeignPtr fptr $ \ptr ->
                      fillBytes ptr c w
{-# INLINE replicate #-}

-- | /O(n)/, where /n/ is the length of the result.  The 'unfoldr'
-- function is analogous to the List \'unfoldr\'.  'unfoldr' builds a
-- ByteString from a seed value.  The function takes the element and
-- returns 'Nothing' if it is done producing the ByteString or returns
-- 'Just' @(a,b)@, in which case, @a@ is the next byte in the string,
-- and @b@ is the seed value for further production.
--
-- Examples:
--
-- >    unfoldr (\x -> if x <= 5 then Just (x, x + 1) else Nothing) 0
-- > == pack [0, 1, 2, 3, 4, 5]
--
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f = concat . unfoldChunk 32 64
  where unfoldChunk n n' x =
          case unfoldrN n f x of
            (s, Nothing) -> [s]
            (s, Just x') -> s : unfoldChunk n' (n+n') x'
{-# INLINE unfoldr #-}

-- | /O(n)/ Like 'unfoldr', 'unfoldrN' builds a ByteString from a seed
-- value.  However, the length of the result is limited by the first
-- argument to 'unfoldrN'.  This function is more efficient than 'unfoldr'
-- when the maximum length of the result is known.
--
-- The following equation relates 'unfoldrN' and 'unfoldr':
--
-- > fst (unfoldrN n f s) == take n (unfoldr f s)
--
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
{-# INLINE unfoldrN #-}

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(1)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int -> ByteString -> ByteString
take n ps@(BS x l)
    | n <= 0    = empty
    | n >= l    = ps
    | otherwise = BS x n
{-# INLINE take #-}

-- | /O(1)/ @'takeEnd' n xs@ is equivalent to @'drop' ('length' xs - n) xs@.
-- Takes @n@ elements from end of bytestring.
--
-- >>> takeEnd 3 "abcdefg"
-- "efg"
-- >>> takeEnd 0 "abcdefg"
-- ""
-- >>> takeEnd 4 "abc"
-- "abc"
--
-- @since 0.11.1.0
takeEnd :: Int -> ByteString -> ByteString
takeEnd n ps@(BS x len)
  | n >= len  = ps
  | n <= 0    = empty
  | otherwise = BS (plusForeignPtr x (len - n)) n
{-# INLINE takeEnd #-}

-- | /O(1)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or 'empty' if @n > 'length' xs@.
drop  :: Int -> ByteString -> ByteString
drop n ps@(BS x l)
    | n <= 0    = ps
    | n >= l    = empty
    | otherwise = BS (plusForeignPtr x n) (l-n)
{-# INLINE drop #-}

-- | /O(1)/ @'dropEnd' n xs@ is equivalent to @'take' ('length' xs - n) xs@.
-- Drops @n@ elements from end of bytestring.
--
-- >>> dropEnd 3 "abcdefg"
-- "abcd"
-- >>> dropEnd 0 "abcdefg"
-- "abcdefg"
-- >>> dropEnd 4 "abc"
-- ""
--
-- @since 0.11.1.0
dropEnd :: Int -> ByteString -> ByteString
dropEnd n ps@(BS x len)
    | n <= 0    = ps
    | n >= len  = empty
    | otherwise = BS x (len - n)
{-# INLINE dropEnd #-}

-- | /O(1)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
splitAt :: Int -> ByteString -> (ByteString, ByteString)
splitAt n ps@(BS x l)
    | n <= 0    = (empty, ps)
    | n >= l    = (ps, empty)
    | otherwise = (BS x n, BS (plusForeignPtr x n) (l-n))
{-# INLINE splitAt #-}

-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f ps = unsafeTake (findIndexOrLength (not . f) ps) ps
{-# INLINE [1] takeWhile #-}

{-# RULES
"ByteString specialise takeWhile (x /=)" forall x.
    takeWhile (x `neWord8`) = fst . breakByte x
"ByteString specialise takeWhile (/= x)" forall x.
    takeWhile (`neWord8` x) = fst . breakByte x
"ByteString specialise takeWhile (x ==)" forall x.
    takeWhile (x `eqWord8`) = fst . spanByte x
"ByteString specialise takeWhile (== x)" forall x.
    takeWhile (`eqWord8` x) = fst . spanByte x
  #-}

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- @since 0.10.12.0
takeWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhileEnd f ps = unsafeDrop (findFromEndUntil (not . f) ps) ps
{-# INLINE takeWhileEnd #-}

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f ps = unsafeDrop (findIndexOrLength (not . f) ps) ps
{-# INLINE [1] dropWhile #-}

{-# RULES
"ByteString specialise dropWhile (x /=)" forall x.
    dropWhile (x `neWord8`) = snd . breakByte x
"ByteString specialise dropWhile (/= x)" forall x.
    dropWhile (`neWord8` x) = snd . breakByte x
"ByteString specialise dropWhile (x ==)" forall x.
    dropWhile (x `eqWord8`) = snd . spanByte x
"ByteString specialise dropWhile (== x)" forall x.
    dropWhile (`eqWord8` x) = snd . spanByte x
  #-}

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- @since 0.10.12.0
dropWhileEnd :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhileEnd f ps = unsafeTake (findFromEndUntil (not . f) ps) ps
{-# INLINE dropWhileEnd #-}

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
--
-- Under GHC, a rewrite rule will transform break (==) into a
-- call to the specialised breakByte:
--
-- > break ((==) x) = breakByte x
-- > break (==x) = breakByte x
--
break :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
break p ps = case findIndexOrLength p ps of n -> (unsafeTake n ps, unsafeDrop n ps)
{-# INLINE [1] break #-}

-- See bytestring #70
{-# RULES
"ByteString specialise break (x ==)" forall x.
    break (x `eqWord8`) = breakByte x
"ByteString specialise break (== x)" forall x.
    break (`eqWord8` x) = breakByte x
  #-}

-- INTERNAL:

-- | 'breakByte' breaks its ByteString argument at the first occurrence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (==99) "abcd" == breakByte 99 "abcd" -- fromEnum 'c' == 99
--
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c p = case elemIndex c p of
    Nothing -> (p,empty)
    Just n  -> (unsafeTake n p, unsafeDrop n p)
{-# INLINE breakByte #-}

-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('dropWhileEnd' (not . p) &&& 'takeWhileEnd' (not . p))@.
--
breakEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
breakEnd  p ps = splitAt (findFromEndUntil p ps) ps

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)
{-# INLINE [1] span #-}

-- | 'spanByte' breaks its ByteString argument at the first
-- occurrence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (==99) "abcd" == spanByte 99 "abcd" -- fromEnum 'c' == 99
--
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
{-# INLINE spanByte #-}

-- See bytestring #70
{-# RULES
"ByteString specialise span (x ==)" forall x.
    span (x `eqWord8`) = spanByte x
"ByteString specialise span (== x)" forall x.
    span (`eqWord8` x) = spanByte x
  #-}

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'spanEnd' @p@ is equivalent to @'breakEnd' (not . p)@ and to @('dropWhileEnd' p &&& 'takeWhileEnd' p)@.
--
-- We have
--
-- > spanEnd (not . isSpace) "x y z" == ("x y ", "z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    ==
-- > let (x, y) = span (not . isSpace) (reverse ps) in (reverse y, reverse x)
--
spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd  p ps = splitAt (findFromEndUntil (not.p) ps) ps

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
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
{-# INLINE splitWith #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split 10  "a\nb\nd\ne" == ["a","b","d","e"]   -- fromEnum '\n' == 10
-- > split 97  "aXaXaXa"    == ["","X","X","X",""] -- fromEnum 'a' == 97
-- > split 120 "x"          == ["",""]             -- fromEnum 'x' == 120
-- > split undefined ""     == []                  -- and not [""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteString's that
-- are slices of the original.
--
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

{-# INLINE split #-}


-- | The 'group' function takes a ByteString and returns a list of
-- ByteStrings such that the concatenation of the result is equal to the
-- argument.  Moreover, each string in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test. It is about 40% faster than
-- /groupBy (==)/
group :: ByteString -> [ByteString]
group xs = case uncons xs of
  Nothing     -> []
  Just (h, _) -> ys : group zs
    where
        (ys, zs) = spanByte h xs

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (Word8 -> Word8 -> Bool) -> ByteString -> [ByteString]
groupBy k xs = case uncons xs of
  Nothing     -> []
  Just (h, t) -> unsafeTake n xs : groupBy k (unsafeDrop n xs)
    where
        n = 1 + findIndexOrLength (not . k h) t

-- | /O(n)/ The 'intercalate' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
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
{-# INLINABLE intercalate #-}

-- ---------------------------------------------------------------------
-- Indexing ByteStrings

-- | /O(1)/ 'ByteString' index (subscript) operator, starting from 0.
--
-- This is a partial function, consider using 'indexMaybe' instead.
index :: HasCallStack => ByteString -> Int -> Word8
index ps n
    | n < 0          = moduleError "index" ("negative index: " ++ show n)
    | n >= length ps = moduleError "index" ("index too large: " ++ show n
                                         ++ ", length = " ++ show (length ps))
    | otherwise      = ps `unsafeIndex` n
{-# INLINE index #-}

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
indexMaybe :: ByteString -> Int -> Maybe Word8
indexMaybe ps n
    | n < 0          = Nothing
    | n >= length ps = Nothing
    | otherwise      = Just $! ps `unsafeIndex` n
{-# INLINE indexMaybe #-}

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
(!?) :: ByteString -> Int -> Maybe Word8
(!?) = indexMaybe
{-# INLINE (!?) #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int
elemIndex c (BS x l) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
    q <- memchr p c (fromIntegral l)
    return $! if q == nullPtr then Nothing else Just $! q `minusPtr` p
{-# INLINE elemIndex #-}

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs = case elemIndex c (reverse xs) of
-- >   Nothing -> Nothing
-- >   Just i  -> Just (length xs - 1 - i)
--
elemIndexEnd :: Word8 -> ByteString -> Maybe Int
elemIndexEnd = findIndexEnd . (==)
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> ByteString -> [Int]
elemIndices w (BS x l) = loop 0
    where
        loop !n = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p -> do
            q <- memchr (p `plusPtr` n) w (fromIntegral (l - n))
            if q == nullPtr
                then return []
                else let !i = q `minusPtr` p
                      in return $ i : loop (i + 1)
{-# INLINE elemIndices #-}

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> ByteString -> Int
count w (BS x m) = accursedUnutterablePerformIO $ unsafeWithForeignPtr x $ \p ->
    fromIntegral <$> c_count p (fromIntegral m) w
{-# INLINE count #-}

-- | /O(n)/ The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
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
{-# INLINE [1] findIndex #-}

-- | /O(n)/ The 'findIndexEnd' function takes a predicate and a 'ByteString' and
-- returns the index of the last element in the ByteString
-- satisfying the predicate.
--
-- @since 0.10.12.0
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
{-# INLINE findIndexEnd #-}

-- | /O(n)/ The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> ByteString -> [Int]
findIndices p = loop 0
   where
     loop !n !qs = case findIndex p qs of
                     Just !i ->
                        let !j = n+i
                         in j : loop (j+1) (unsafeDrop (i+1) qs)
                     Nothing -> []
{-# INLINE [1] findIndices #-}


{-# RULES
"ByteString specialise findIndex (x ==)" forall x. findIndex (x`eqWord8`) = elemIndex x
"ByteString specialise findIndex (== x)" forall x. findIndex (`eqWord8`x) = elemIndex x
"ByteString specialise findIndices (x ==)" forall x. findIndices (x`eqWord8`) = elemIndices x
"ByteString specialise findIndices (== x)" forall x. findIndices (`eqWord8`x) = elemIndices x
  #-}

-- ---------------------------------------------------------------------
-- Searching ByteStrings

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate.
elem :: Word8 -> ByteString -> Bool
elem c ps = case elemIndex c ps of Nothing -> False ; _ -> True
{-# INLINE elem #-}

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Word8 -> ByteString -> Bool
notElem c ps = not (c `elem` ps)
{-# INLINE notElem #-}

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter k = \ps@(BS pIn l) ->
        -- see fold inlining.
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
{-# INLINE filter #-}

{-
--
-- | /O(n)/ A first order equivalent of /filter . (==)/, for the common
-- case of filtering a single byte. It is more efficient to use
-- /filterByte/ in this case.
--
-- > filterByte == filter . (==)
--
-- filterByte is around 10x faster, and uses much less space, than its
-- filter equivalent
--
filterByte :: Word8 -> ByteString -> ByteString
filterByte w ps = replicate (count w ps) w
{-# INLINE filterByte #-}

{-# RULES
"ByteString specialise filter (== x)" forall x.
    filter ((==) x) = filterByte x
"ByteString specialise filter (== x)" forall x.
    filter (== x) = filterByte x
  #-}
-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f p = case findIndex f p of
                    Just n -> Just (p `unsafeIndex` n)
                    _      -> Nothing
{-# INLINE find #-}

-- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- the pair of ByteStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
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

-- --------------------------------------------------------------------
-- Sarching for substrings

-- |/O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- if the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf (BS x1 l1) (BS x2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x1 $ \p1 ->
        unsafeWithForeignPtr x2 $ \p2 -> do
            i <- memcmp p1 p2 (fromIntegral l1)
            return $! i == 0

-- | /O(n)/ The 'stripPrefix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
-- @since 0.10.8.0
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix bs1@(BS _ l1) bs2
   | bs1 `isPrefixOf` bs2 = Just (unsafeDrop l1 bs2)
   | otherwise = Nothing

-- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
-- However, the real implementation uses memcmp to compare the end of the
-- string only, with no reverse required..
isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf (BS x1 l1) (BS x2 l2)
    | l1 == 0   = True
    | l2 < l1   = False
    | otherwise = accursedUnutterablePerformIO $ unsafeWithForeignPtr x1 $ \p1 ->
        unsafeWithForeignPtr x2 $ \p2 -> do
            i <- memcmp p1 (p2 `plusPtr` (l2 - l1)) (fromIntegral l1)
            return $! i == 0

-- | /O(n)/ The 'stripSuffix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix bs1@(BS _ l1) bs2@(BS _ l2)
   | bs1 `isSuffixOf` bs2 = Just (unsafeTake (l2 - l1) bs2)
   | otherwise = Nothing

-- | Check whether one string is a substring of another.
isInfixOf :: ByteString -> ByteString -> Bool
isInfixOf p s = null p || not (null $ snd $ breakSubstring p s)

-- | /O(n)/ Check whether a 'ByteString' represents valid UTF-8.
--
-- @since 0.11.2.0
isValidUtf8 :: ByteString -> Bool
isValidUtf8 (BS ptr len) = accursedUnutterablePerformIO $ unsafeWithForeignPtr ptr $ \p -> do
  -- Use a safe FFI call for large inputs to avoid GC synchronization pauses
  -- in multithreaded contexts.
  -- This specific limit was chosen based on results of a simple benchmark, see:
  -- https://github.com/haskell/bytestring/issues/451#issuecomment-991879338
  -- When changing this function, also consider changing the related function:
  -- Data.ByteString.Short.Internal.isValidUtf8
  i <- if len < 1000000
     then cIsValidUtf8 p (fromIntegral len)
     else cIsValidUtf8Safe p (fromIntegral len)
  pure $ i /= 0

-- | Break a string on a substring, returning a pair of the part of the
-- string prior to the match, and the rest of the string.
--
-- The following relationships hold:
--
-- > break (== c) l == breakSubstring (singleton c) l
--
-- For example, to tokenise a string, dropping delimiters:
--
-- > tokenise x y = h : if null t then [] else tokenise x (drop (length x) t)
-- >     where (h,t) = breakSubstring x y
--
-- To skip to the first occurrence of a string:
--
-- > snd (breakSubstring x y)
--
-- To take the parts of a string before a delimiter:
--
-- > fst (breakSubstring x y)
--
-- Note that calling `breakSubstring x` does some preprocessing work, so
-- you should avoid unnecessarily duplicating breakSubstring calls with the same
-- pattern.
--
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
    {-# INLINE karpRabin #-}

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
    {-# INLINE shift #-}

-- ---------------------------------------------------------------------
-- Zipping

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of bytes. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip ps qs = case uncons ps of
  Nothing         -> []
  Just (psH, psT) -> case uncons qs of
    Nothing         -> []
    Just (qsH, qsT) -> (psH, qsH) : zip psT qsT

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list of
-- corresponding sums.
zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith f ps qs = case uncons ps of
  Nothing         -> []
  Just (psH, psT) -> case uncons qs of
    Nothing         -> []
    Just (qsH, qsT) -> f psH qsH : zipWith f psT qsT
{-# NOINLINE [1] zipWith #-}

-- | A specialised version of `zipWith` for the common case of a
-- simultaneous map over two ByteStrings, to build a 3rd.
--
-- @since 0.11.1.0
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
{-# INLINE packZipWith #-}

-- | /O(n)/ 'unzip' transforms a list of pairs of bytes into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
unzip ls = (pack (P.map fst ls), pack (P.map snd ls))
{-# INLINE unzip #-}

-- ---------------------------------------------------------------------
-- Special lists

-- | /O(n)/ Returns all initial segments of the given 'ByteString', shortest first.
inits :: ByteString -> [ByteString]
-- see Note [Avoid NonEmpty combinators]
inits bs = NE.toList $! initsNE bs

-- | /O(n)/ Returns all initial segments of the given 'ByteString', shortest first.
--
-- @since 0.11.4.0
initsNE :: ByteString -> NonEmpty ByteString
-- see Note [Avoid NonEmpty combinators]
initsNE (BS x len) = empty :| [BS x n | n <- [1..len]]

-- | /O(n)/ Returns all final segments of the given 'ByteString', longest first.
tails :: ByteString -> [ByteString]
-- see Note [Avoid NonEmpty combinators]
tails bs = NE.toList $! tailsNE bs

-- | /O(n)/ Returns all final segments of the given 'ByteString', longest first.
--
-- @since 0.11.4.0
tailsNE :: ByteString -> NonEmpty ByteString
-- see Note [Avoid NonEmpty combinators]
tailsNE p | null p    = empty :| []
          | otherwise = p :| tails (unsafeTail p)

-- less efficent spacewise: tails (BS x l) = [BS (plusForeignPtr x n) (l-n) | n <- [0..l]]

{-
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
-}



-- ---------------------------------------------------------------------
-- ** Ordered 'ByteString's

-- | /O(n)/ Sort a ByteString efficiently, using counting sort.
sort :: ByteString -> ByteString
sort (BS input l)
  -- qsort outperforms counting sort for small arrays
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
    -- Count the number of occurrences of each byte.
    countOccurrences :: Ptr Int -> Ptr Word8 -> Int -> IO ()
    countOccurrences !counts !str !len = go 0
     where
        go !i | i == len    = return ()
              | otherwise = do k <- fromIntegral `fmap` peekElemOff str i
                               x <- peekElemOff counts k
                               pokeElemOff counts k (x + 1)
                               go (i + 1)


-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a
-- null-terminated @CString@.  The @CString@ is a copy and will be freed
-- automatically; it must not be stored or used after the
-- subcomputation finishes.
useAsCString :: ByteString -> (CString -> IO a) -> IO a
useAsCString (BS fp l) action =
  allocaBytes (l+1) $ \buf -> do
    unsafeWithForeignPtr fp $ \p -> copyBytes buf p l
    pokeByteOff buf l (0::Word8)
    action (castPtr buf)

-- | /O(n) construction/ Use a @ByteString@ with a function requiring a 'CStringLen'.
-- As for 'useAsCString' this function makes a copy of the original @ByteString@.
-- It must not be stored or used after the subcomputation finishes.
--
-- Beware that this function is not required to add a terminating @\NUL@ byte at the end of the 'CStringLen' it provides.
-- If you need to construct a pointer to a null-terminated sequence, use 'useAsCString'
-- (and measure length independently if desired).
useAsCStringLen :: ByteString -> (CStringLen -> IO a) -> IO a
useAsCStringLen p@(BS _ l) f = useAsCString p $ \cstr -> f (cstr,l)

------------------------------------------------------------------------

-- | /O(n)./ Construct a new @ByteString@ from a @CString@. The
-- resulting @ByteString@ is an immutable copy of the original
-- @CString@, and is managed on the Haskell heap. The original
-- @CString@ must be null terminated.
packCString :: CString -> IO ByteString
packCString cstr = do
    len <- c_strlen cstr
    packCStringLen (cstr, fromIntegral len)

-- | /O(n)./ Construct a new @ByteString@ from a @CStringLen@. The
-- resulting @ByteString@ is an immutable copy of the original @CStringLen@.
-- The @ByteString@ is a normal Haskell value and will be managed on the
-- Haskell heap.
packCStringLen :: CStringLen -> IO ByteString
packCStringLen (cstr, len) | len >= 0 = createFp len $ \fp ->
    unsafeWithForeignPtr fp $ \p -> copyBytes p (castPtr cstr) len
packCStringLen (_, len) =
    moduleErrorIO "packCStringLen" ("negative length: " ++ show len)

------------------------------------------------------------------------

-- | /O(n)/ Make a copy of the 'ByteString' with its own storage.
-- This is mainly useful to allow the rest of the data pointed
-- to by the 'ByteString' to be garbage collected, for example
-- if a large string has been read in, and only a small part of it
-- is needed in the rest of the program.
--
copy :: ByteString -> ByteString
copy (BS x l) = unsafeCreateFp l $ \p -> memcpyFp p x l

-- ---------------------------------------------------------------------
-- Line IO

-- | Read a line from stdin.
getLine :: IO ByteString
getLine = hGetLine stdin

{-# DEPRECATED getLine
     "Deprecated since @bytestring-0.12@. Use 'Data.ByteString.Char8.getLine' instead. (Functions that rely on ASCII encodings belong in \"Data.ByteString.Char8\")"
  #-}

-- | Read a line from a handle
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

      -- if eol == True, then off is the offset of the '\n'
      -- otherwise off == w and the buffer is now empty.
        if off /= w
            then do if w == off + 1
                            then writeIORef haByteBuffer buf{ bufL=0, bufR=0 }
                            else writeIORef haByteBuffer buf{ bufL = off + 1 }
                    mkBigPS new_len (xs:xss)
            else fill h_ buf{ bufL=0, bufR=0 } new_len (xs:xss)

  -- find the end-of-line character, if there is one
  findEOL r w raw
        | r == w = return w
        | otherwise =  do
            c <- readWord8Buf raw r
            if c == fromIntegral (ord '\n')
                then return r -- NB. not r+1: don't include the '\n'
                else findEOL (r+1) w raw

{-# DEPRECATED hGetLine
     "Deprecated since @bytestring-0.12@. Use 'Data.ByteString.Char8.hGetLine' instead. (Functions that rely on ASCII encodings belong in \"Data.ByteString.Char8\")"
  #-}

mkPS :: RawBuffer Word8 -> Int -> Int -> IO ByteString
mkPS buf start end =
 createFp len $ \fp -> memcpyFp fp (buf `plusForeignPtr` start) len
 where
   len = end - start

mkBigPS :: Int -> [ByteString] -> IO ByteString
mkBigPS _ [ps] = return ps
mkBigPS _ pss = return $! concat (P.reverse pss)

-- ---------------------------------------------------------------------
-- Block IO

-- | Outputs a 'ByteString' to the specified 'Handle'.
hPut :: Handle -> ByteString -> IO ()
hPut _ (BS _  0) = return ()
hPut h (BS ps l) = unsafeWithForeignPtr ps $ \p-> hPutBuf h p l

-- | Similar to 'hPut' except that it will never block. Instead it returns
-- any tail that did not get written. This tail may be 'empty' in the case that
-- the whole string was written, or the whole original string if nothing was
-- written. Partial writes are also possible.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hPut'.
--
hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking h bs@(BS ps l) = do
  bytesWritten <- unsafeWithForeignPtr ps $ \p-> hPutBufNonBlocking h p l
  return $! drop bytesWritten bs

-- | A synonym for 'hPut', for compatibility
hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

-- | Write a ByteString to 'stdout'.
putStr :: ByteString -> IO ()
putStr = hPut stdout

------------------------------------------------------------------------
-- Low level IO

-- | Read a 'ByteString' directly from the specified 'Handle'.  This
-- is far more efficient than reading the characters into a 'String'
-- and then using 'pack'. First argument is the Handle to read from,
-- and the second is the number of bytes to read. It returns the bytes
-- read, up to n, or 'empty' if EOF has been reached.
--
-- 'hGet' is implemented in terms of 'hGetBuf'.
--
-- If the handle is a pipe or socket, and the writing end
-- is closed, 'hGet' will behave as if EOF was reached.
--
hGet :: Handle -> Int -> IO ByteString
hGet h i
    | i >  0    = createFpAndTrim i $ \fp ->
        unsafeWithForeignPtr fp $ \p -> hGetBuf h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGet" i

-- | hGetNonBlocking is similar to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.  If there is no data available to be read, 'hGetNonBlocking'
-- returns 'empty'.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hGet'.
--
hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking h i
    | i >  0    = createFpAndTrim i $ \fp ->
        unsafeWithForeignPtr fp $ \p -> hGetBufNonBlocking h p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize h "hGetNonBlocking" i

-- | Like 'hGet', except that a shorter 'ByteString' may be returned
-- if there are not enough bytes immediately available to satisfy the
-- whole request.  'hGetSome' only blocks if there is no data
-- available, and EOF has not yet been reached.
--
hGetSome :: Handle -> Int -> IO ByteString
hGetSome hh i
    | i >  0    = createFpAndTrim i $ \fp ->
        unsafeWithForeignPtr fp $ \p -> hGetBufSome hh p i
    | i == 0    = return empty
    | otherwise = illegalBufferSize hh "hGetSome" i

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle fn sz =
    ioError (mkIOError illegalOperationErrorType msg (Just handle) Nothing)
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []


-- | Read a handle's entire contents strictly into a 'ByteString'.
--
-- This function reads chunks at a time, increasing the chunk size on each
-- read. The final string is then reallocated to the appropriate size. For
-- files > half of available memory, this may lead to memory exhaustion.
-- Consider using 'readFile' in this case.
--
-- The Handle is closed once the contents have been read,
-- or if an exception is thrown.
--
hGetContents :: Handle -> IO ByteString
hGetContents hnd = do
    bs <- hGetContentsSizeHint hnd 1024 2048
            `finally` hClose hnd
    -- don't waste too much space for small files:
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
      -- We rely on the hGetBuf behaviour (not hGetBufSome) where it reads up
      -- to the size we ask for, or EOF. So short reads indicate EOF.
      if readcount < sz && sz > 0
        then return $! concat (P.reverse (chunk : chunks))
        else readChunks (chunk : chunks) sz' ((sz+sz') `min` 32752)
             -- we grow the buffer sizes, but not too huge
             -- we concatenate in the end anyway

-- | getContents. Read stdin strictly. Equivalent to hGetContents stdin
-- The 'Handle' is closed after the contents have been read.
--
getContents :: IO ByteString
getContents = hGetContents stdin

-- | The interact function takes a function of type @ByteString -> ByteString@
-- as its argument. The entire input from the standard input device is passed
-- to this function as its argument, and the resulting string is output on the
-- standard output device.
--
interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents

-- | Read an entire file strictly into a 'ByteString'.
--
readFile :: FilePath -> IO ByteString
readFile f =
    withBinaryFile f ReadMode $ \h -> do
      -- hFileSize fails if file is not regular file (like
      -- /dev/null). Catch exception and try reading anyway.
      filesz <- catch (hFileSize h) useZeroIfNotRegularFile
      let readsz = (fromIntegral filesz `max` 0) + 1
      hGetContentsSizeHint h readsz (readsz `max` 255)
      -- Our initial size is one bigger than the file size so that in the
      -- typical case we will read the whole file in one go and not have
      -- to allocate any more chunks. We'll still do the right thing if the
      -- file size is 0 or is changed before we do the read.
  where
    useZeroIfNotRegularFile :: IOException -> IO Integer
    useZeroIfNotRegularFile _ = return 0

modifyFile :: IOMode -> FilePath -> ByteString -> IO ()
modifyFile mode f txt = withBinaryFile f mode (`hPut` txt)

-- | Write a 'ByteString' to a file.
writeFile :: FilePath -> ByteString -> IO ()
writeFile = modifyFile WriteMode

-- | Append a 'ByteString' to a file.
appendFile :: FilePath -> ByteString -> IO ()
appendFile = modifyFile AppendMode

-- ---------------------------------------------------------------------
-- Internal utilities

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: HasCallStack => String -> a
errorEmptyList fun = moduleError fun "empty ByteString"
{-# NOINLINE errorEmptyList #-}

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error (moduleErrorMsg fun msg)
{-# NOINLINE moduleError #-}

moduleErrorIO :: HasCallStack => String -> String -> IO a
moduleErrorIO fun msg = throwIO . userError $ moduleErrorMsg fun msg
{-# NOINLINE moduleErrorIO #-}

moduleErrorMsg :: String -> String -> String
moduleErrorMsg fun msg = "Data.ByteString." ++ fun ++ ':':' ':msg

-- Find from the end of the string using predicate
findFromEndUntil :: (Word8 -> Bool) -> ByteString -> Int
findFromEndUntil f ps@(BS _ l) = case unsnoc ps of
  Nothing     -> 0
  Just (b, c) ->
    if f c
      then l
      else findFromEndUntil f b

      {-# OPTIONS_HADDOCK not-home #-}

-- |
-- Module      : Data.ByteString.Internal
-- Copyright   : (c) Don Stewart 2006-2008
--               (c) Duncan Coutts 2006-2012
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : unstable
-- Portability : non-portable
--
-- A module containing semi-public 'ByteString' internals. This exposes the
-- 'ByteString' representation and low level construction functions. As such
-- all the functions in this module are unsafe. The API is also not stable.
--
-- Where possible application should instead use the functions from the normal
-- public interface modules, such as "Data.ByteString.Unsafe". Packages that
-- extend the ByteString system at a low level will need to use this module.
--
module Data.ByteString.Internal (

        -- * The @ByteString@ type and representation
        ByteString
        ( BS
        , PS -- backwards compatibility shim
        ),

        StrictByteString,

        -- * Internal indexing
        findIndexOrLength,

        -- * Conversion with lists: packing and unpacking
        packBytes, packUptoLenBytes, unsafePackLenBytes,
        packChars, packUptoLenChars, unsafePackLenChars,
        unpackBytes, unpackAppendBytesLazy, unpackAppendBytesStrict,
        unpackChars, unpackAppendCharsLazy, unpackAppendCharsStrict,
        unsafePackAddress, unsafePackLenAddress,
        unsafePackLiteral, unsafePackLenLiteral,

        -- * Low level imperative construction
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

        -- * Conversion to and from ForeignPtrs
        mkDeferredByteString,
        fromForeignPtr,
        toForeignPtr,
        fromForeignPtr0,
        toForeignPtr0,

        -- * Utilities
        nullForeignPtr,
        deferForeignPtrAvailability,
        SizeOverflowException,
        overflowError,
        checkedAdd,
        checkedMultiply,

        -- * Standard C Functions
        c_strlen,
        c_free_finalizer,

        memchr,
        memcmp,
        memcpy,
        memset,

        -- * cbits functions
        c_reverse,
        c_intersperse,
        c_maximum,
        c_minimum,
        c_count,
        c_sort,

        -- * Chars
        w2c, c2w, isSpaceWord8, isSpaceChar8,

        -- * Deprecated and unmentionable
        accursedUnutterablePerformIO,

        -- * Exported compatibility shim
        plusForeignPtr,
        unsafeWithForeignPtr
  ) where

import Data.ByteString.Internal.Type

{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_HADDOCK prune #-}

-- |
-- Module      : Data.ByteString.Lazy
-- Copyright   : (c) Don Stewart 2006
--               (c) Duncan Coutts 2006-2011
-- License     : BSD-style
--
-- Maintainer  : dons00@gmail.com, duncan@community.haskell.org
-- Stability   : stable
-- Portability : portable
--
-- A time and space-efficient implementation of lazy byte vectors
-- using lists of packed 'Word8' arrays, suitable for high performance
-- use, both in terms of large data quantities, or high speed
-- requirements. Lazy ByteStrings are encoded as lazy lists of strict chunks
-- of bytes.
--
-- A key feature of lazy ByteStrings is the means to manipulate large or
-- unbounded streams of data without requiring the entire sequence to be
-- resident in memory. To take advantage of this you have to write your
-- functions in a lazy streaming style, e.g. classic pipeline composition. The
-- default I\/O chunk size is 32k, which should be good in most circumstances.
--
-- Some operations, such as 'concat', 'append', 'reverse' and 'cons', have
-- better complexity than their "Data.ByteString" equivalents, due to
-- optimisations resulting from the list spine structure. For other
-- operations lazy ByteStrings are usually within a few percent of
-- strict ones.
--
-- The recomended way to assemble lazy ByteStrings from smaller parts
-- is to use the builder monoid from "Data.ByteString.Builder".
--
-- This module is intended to be imported @qualified@, to avoid name
-- clashes with "Prelude" functions.  eg.
--
-- > import qualified Data.ByteString.Lazy as B
--
-- Original GHC implementation by Bryan O\'Sullivan.
-- Rewritten to use 'Data.Array.Unboxed.UArray' by Simon Marlow.
-- Rewritten to support slices and use 'Foreign.ForeignPtr.ForeignPtr'
-- by David Roundy.
-- Rewritten again and extended by Don Stewart and Duncan Coutts.
-- Lazy variant by Duncan Coutts and Don Stewart.
--

module Data.ByteString.Lazy (

        -- * Lazy @ByteString@
        ByteString,
        LazyByteString,

        -- * Introducing and eliminating 'ByteString's
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

        -- * Basic interface
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

        -- * Transforming ByteStrings
        map,
        reverse,
        intersperse,
        intercalate,
        transpose,

        -- * Reducing 'ByteString's (folds)
        foldl,
        foldl',
        foldl1,
        foldl1',
        foldr,
        foldr',
        foldr1,
        foldr1',

        -- ** Special folds
        concat,
        concatMap,
        any,
        all,
        maximum,
        minimum,
        compareLength,

        -- * Building ByteStrings
        -- ** Scans
        scanl,
        scanl1,
        scanr,
        scanr1,

        -- ** Accumulating maps
        mapAccumL,
        mapAccumR,

        -- ** Infinite ByteStrings
        repeat,
        replicate,
        cycle,
        iterate,

        -- ** Unfolding ByteStrings
        unfoldr,

        -- * Substrings

        -- ** Breaking strings
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

        -- ** Breaking into many substrings
        split,
        splitWith,

        -- * Predicates
        isPrefixOf,
        isSuffixOf,
--        isInfixOf,

        -- ** Search for arbitrary substrings
--        isSubstringOf,

        -- * Searching ByteStrings

        -- ** Searching by equality
        elem,
        notElem,

        -- ** Searching with a predicate
        find,
        filter,
        partition,

        -- * Indexing ByteStrings
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

        -- * Zipping and unzipping ByteStrings
        zip,
        zipWith,
        packZipWith,
        unzip,

        -- * Ordered ByteStrings
--        sort,

        -- * Low level conversions
        -- ** Copying ByteStrings
        copy,
--        defrag,

        -- * I\/O with 'ByteString's
        -- $IOChunk

        -- ** Standard input and output
        getContents,
        putStr,
        interact,

        -- ** Files
        readFile,
        writeFile,
        appendFile,

        -- ** I\/O with Handles
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


-- -----------------------------------------------------------------------------
-- Introducing and eliminating 'ByteString's

-- | /O(1)/ The empty 'ByteString'
empty :: ByteString
empty = Empty
{-# INLINE empty #-}

-- | /O(1)/ Convert a 'Word8' into a 'ByteString'
singleton :: Word8 -> ByteString
singleton w = Chunk (S.singleton w) Empty
{-# INLINE singleton #-}

-- | /O(n)/ Convert a '[Word8]' into a 'ByteString'.
pack :: [Word8] -> ByteString
pack = packBytes

-- | /O(n)/ Converts a 'ByteString' to a '[Word8]'.
unpack :: ByteString -> [Word8]
unpack = unpackBytes

-- | /O(c)/ Convert a list of 'S.StrictByteString' into a 'LazyByteString'
fromChunks :: [S.StrictByteString] -> LazyByteString
fromChunks = List.foldr chunk Empty

-- | /O(c)/ Convert a 'LazyByteString' into a list of 'S.StrictByteString'
toChunks :: LazyByteString -> [S.StrictByteString]
toChunks = foldrChunks (:) []

------------------------------------------------------------------------

{-
-- | /O(n)/ Convert a '[a]' into a 'ByteString' using some
-- conversion function
packWith :: (a -> Word8) -> [a] -> ByteString
packWith k str = LPS $ L.map (P.packWith k) (chunk defaultChunkSize str)
{-# INLINE packWith #-}
{-# SPECIALIZE packWith :: (Char -> Word8) -> [Char] -> ByteString #-}

-- | /O(n)/ Converts a 'ByteString' to a '[a]', using a conversion function.
unpackWith :: (Word8 -> a) -> ByteString -> [a]
unpackWith k (LPS ss) = L.concatMap (S.unpackWith k) ss
{-# INLINE unpackWith #-}
{-# SPECIALIZE unpackWith :: (Word8 -> Char) -> ByteString -> [Char] #-}
-}

-- ---------------------------------------------------------------------
-- Basic interface

-- | /O(1)/ Test whether a ByteString is empty.
null :: ByteString -> Bool
null Empty = True
null _     = False
{-# INLINE null #-}

-- | /O(c)/ 'length' returns the length of a ByteString as an 'Int64'
length :: ByteString -> Int64
length = foldlChunks (\n c -> n + fromIntegral (S.length c)) 0
{-# INLINE [1] length #-}

infixr 5 `cons`, `cons'` --same as list (:)
infixl 5 `snoc`

-- | /O(1)/ 'cons' is analogous to '(Prelude.:)' for lists.
--
cons :: Word8 -> ByteString -> ByteString
cons c = Chunk (S.singleton c)
{-# INLINE cons #-}

-- | /O(1)/ Unlike 'cons', 'cons'' is
-- strict in the ByteString that we are consing onto. More precisely, it forces
-- the head and the first chunk. It does this because, for space efficiency, it
-- may coalesce the new byte onto the first \'chunk\' rather than starting a
-- new \'chunk\'.
--
-- So that means you can't use a lazy recursive contruction like this:
--
-- > let xs = cons' c xs in xs
--
-- You can however use 'cons', as well as 'repeat' and 'cycle', to build
-- infinite lazy ByteStrings.
--
cons' :: Word8 -> ByteString -> ByteString
cons' w (Chunk c cs) | S.length c < 16 = Chunk (S.cons w c) cs
cons' w cs                             = Chunk (S.singleton w) cs
{-# INLINE cons' #-}

-- | /O(n\/c)/ Append a byte to the end of a 'ByteString'
snoc :: ByteString -> Word8 -> ByteString
snoc cs w = foldrChunks Chunk (singleton w) cs
{-# INLINE snoc #-}

-- | /O(1)/ Extract the first element of a ByteString, which must be non-empty.
--
-- This is a partial function, consider using 'uncons' instead.
head :: HasCallStack => ByteString -> Word8
head Empty       = errorEmptyList "head"
head (Chunk c _) = S.unsafeHead c
{-# INLINE head #-}

-- | /O(1)/ Extract the 'head' and 'tail' of a ByteString, returning 'Nothing'
-- if it is empty.
uncons :: ByteString -> Maybe (Word8, ByteString)
uncons Empty = Nothing
uncons (Chunk c cs) = case S.length c of
  -- Don't move this test inside of the Just or (,).
  -- We don't want to allocate a thunk to put inside of the tuple!
  -- And if "let !tl = ... in Just (..., tl)" seems more appealing,
  -- remember that this function must remain lazy in cs.
  1 -> Just (S.unsafeHead c, cs)
  _ -> Just (S.unsafeHead c, Chunk (S.unsafeTail c) cs)
{-# INLINE uncons #-}

-- | /O(1)/ Extract the elements after the head of a ByteString, which must be
-- non-empty.
--
-- This is a partial function, consider using 'uncons' instead.
tail :: HasCallStack => ByteString -> ByteString
tail Empty          = errorEmptyList "tail"
tail (Chunk c cs)
  | S.length c == 1 = cs
  | otherwise       = Chunk (S.unsafeTail c) cs
{-# INLINE tail #-}

-- | /O(n\/c)/ Extract the last element of a ByteString, which must be finite
-- and non-empty.
--
-- This is a partial function, consider using 'unsnoc' instead.
last :: HasCallStack => ByteString -> Word8
last Empty          = errorEmptyList "last"
last (Chunk c0 cs0) = go c0 cs0
  where go c Empty        = S.unsafeLast c
        go _ (Chunk c cs) = go c cs
-- XXX Don't inline this. Something breaks with 6.8.2 (haven't investigated yet)

-- | /O(n\/c)/ Returns all the elements of a 'ByteString' except the last one.
--
-- This is a partial function, consider using 'unsnoc' instead.
init :: HasCallStack => ByteString -> ByteString
init Empty          = errorEmptyList "init"
init (Chunk c0 cs0) = go c0 cs0
  where go c Empty | S.length c == 1 = Empty
                   | otherwise       = Chunk (S.unsafeInit c) Empty
        go c (Chunk c' cs)           = Chunk c (go c' cs)

-- | /O(n\/c)/ Extract the 'init' and 'last' of a ByteString, returning 'Nothing'
-- if it is empty.
--
-- * It is no faster than using 'init' and 'last'
unsnoc :: ByteString -> Maybe (ByteString, Word8)
unsnoc Empty        = Nothing
unsnoc (Chunk c cs) = Just (init (Chunk c cs), last (Chunk c cs))

-- | /O(n\/c)/ Append two ByteStrings
append :: ByteString -> ByteString -> ByteString
append = mappend
{-# INLINE append #-}

-- ---------------------------------------------------------------------
-- Transformations

-- | /O(n)/ 'map' @f xs@ is the ByteString obtained by applying @f@ to each
-- element of @xs@.
map :: (Word8 -> Word8) -> ByteString -> ByteString
map f = go
    where
        go Empty        = Empty
        go (Chunk x xs) = Chunk y ys
            where
                y  = S.map f x
                ys = go xs
{-# INLINE map #-}

-- | /O(n)/ 'reverse' @xs@ returns the elements of @xs@ in reverse order.
reverse :: ByteString -> ByteString
reverse = rev Empty
  where rev a Empty        = a
        rev a (Chunk c cs) = rev (Chunk (S.reverse c) a) cs
{-# INLINE reverse #-}

-- | The 'intersperse' function takes a 'Word8' and a 'ByteString' and
-- \`intersperses\' that byte between the elements of the 'ByteString'.
-- It is analogous to the intersperse function on Lists.
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

-- | The 'transpose' function transposes the rows and columns of its
-- 'ByteString' argument.
transpose :: [ByteString] -> [ByteString]
transpose css = List.map (\ss -> Chunk (S.pack ss) Empty)
                      (List.transpose (List.map unpack css))
--TODO: make this fast

-- ---------------------------------------------------------------------
-- Reducing 'ByteString's

-- | 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a ByteString, reduces the
-- ByteString using the binary operator, from left to right.
foldl :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl f = go
  where go a Empty        = a
        go a (Chunk c cs) = go (S.foldl f a c) cs
{-# INLINE foldl #-}

-- | 'foldl'' is like 'foldl', but strict in the accumulator.
foldl' :: (a -> Word8 -> a) -> a -> ByteString -> a
foldl' f = go
  where go !a Empty        = a
        go !a (Chunk c cs) = go (S.foldl' f a c) cs
{-# INLINE foldl' #-}

-- | 'foldr', applied to a binary operator, a starting value
-- (typically the right-identity of the operator), and a ByteString,
-- reduces the ByteString using the binary operator, from right to left.
foldr :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr k = foldrChunks (flip (S.foldr k))
{-# INLINE foldr #-}

-- | 'foldr'' is like 'foldr', but strict in the accumulator.
--
-- @since 0.11.2.0
foldr' :: (Word8 -> a -> a) -> a -> ByteString -> a
foldr' f a = go
  where
    go Empty = a
    go (Chunk c cs) = S.foldr' f (foldr' f a cs) c
{-# INLINE foldr' #-}

-- | 'foldl1' is a variant of 'foldl' that has no starting value
-- argument, and thus must be applied to non-empty 'ByteString's.
foldl1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1 _ Empty        = errorEmptyList "foldl1"
foldl1 f (Chunk c cs) = go (S.unsafeHead c) (S.unsafeTail c) cs
  where
    go v x xs = let v' = S.foldl f v x
      in case xs of
      Empty -> v'
      Chunk x' xs' -> go v' x' xs'

-- | 'foldl1'' is like 'foldl1', but strict in the accumulator.
foldl1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldl1' _ Empty        = errorEmptyList "foldl1'"
foldl1' f (Chunk c cs) = go (S.unsafeHead c) (S.unsafeTail c) cs
  where
    go !v x xs = let v' = S.foldl' f v x
      in case xs of
      Empty -> v'
      Chunk x' xs' -> go v' x' xs'

-- | 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty 'ByteString's
foldr1 :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1 _ Empty          = errorEmptyList "foldr1"
foldr1 f (Chunk c0 cs0) = go c0 cs0
  where go c Empty         = S.foldr1 f c
        go c (Chunk c' cs) = S.foldr  f (go c' cs) c

-- | 'foldr1'' is like 'foldr1', but strict in the accumulator.
--
-- @since 0.11.2.0
foldr1' :: HasCallStack => (Word8 -> Word8 -> Word8) -> ByteString -> Word8
foldr1' _ Empty          = errorEmptyList "foldr1'"
foldr1' f (Chunk c0 cs0) = go c0 cs0
  where go c Empty         = S.foldr1' f c
        go c (Chunk c' cs) = S.foldr'  f (go c' cs) c

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/ Concatenate a list of ByteStrings.
concat :: [ByteString] -> ByteString
concat = mconcat

-- | Map a function over a 'ByteString' and concatenate the results
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

-- | /O(n)/ Applied to a predicate and a ByteString, 'any' determines if
-- any element of the 'ByteString' satisfies the predicate.
any :: (Word8 -> Bool) -> ByteString -> Bool
any f = foldrChunks (\c rest -> S.any f c || rest) False
{-# INLINE any #-}

-- | /O(n)/ Applied to a predicate and a 'ByteString', 'all' determines
-- if all elements of the 'ByteString' satisfy the predicate.
all :: (Word8 -> Bool) -> ByteString -> Bool
all f = foldrChunks (\c rest -> S.all f c && rest) True
{-# INLINE all #-}

-- | /O(n)/ 'maximum' returns the maximum value from a 'ByteString'
maximum :: HasCallStack => ByteString -> Word8
maximum Empty        = errorEmptyList "maximum"
maximum (Chunk c cs) = foldlChunks (\n c' -> n `max` S.maximum c')
                                   (S.maximum c) cs
{-# INLINE maximum #-}

-- | /O(n)/ 'minimum' returns the minimum value from a 'ByteString'
minimum :: HasCallStack => ByteString -> Word8
minimum Empty        = errorEmptyList "minimum"
minimum (Chunk c cs) = foldlChunks (\n c' -> n `min` S.minimum c')
                                     (S.minimum c) cs
{-# INLINE minimum #-}

-- | /O(c)/ 'compareLength' compares the length of a 'ByteString'
-- to an 'Int64'
--
-- @since 0.11.1.0
compareLength :: ByteString -> Int64 -> Ordering
compareLength _ toCmp | toCmp < 0 = GT
compareLength Empty toCmp         = compare 0 toCmp
compareLength (Chunk c cs) toCmp  = compareLength cs (toCmp - fromIntegral (S.length c))
{-# INLINE compareLength #-}

{-# RULES
"ByteString.Lazy length/compareN -> compareLength" [~1] forall t n.
  compare (length t) n = compareLength t n
"ByteString.Lazy compareN/length -> compareLength" [~1] forall t n.
  -- compare EQ LT = GT and vice versa
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
  #-}

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from left to right, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumL :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumL f = go
  where
    go s Empty        = (s, Empty)
    go s (Chunk c cs) = (s'', Chunk c' cs')
        where (s',  c')  = S.mapAccumL f s c
              (s'', cs') = go s' cs

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a ByteString,
-- passing an accumulating parameter from right to left, and returning a
-- final value of this accumulator together with the new ByteString.
mapAccumR :: (acc -> Word8 -> (acc, Word8)) -> acc -> ByteString -> (acc, ByteString)
mapAccumR f = go
  where
    go s Empty        = (s, Empty)
    go s (Chunk c cs) = (s'', Chunk c' cs')
        where (s'', c') = S.mapAccumR f s' c
              (s', cs') = go s cs

-- ---------------------------------------------------------------------
-- Building ByteStrings

-- | 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left.
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Note that
--
-- > head (scanl f z xs) == z
-- > last (scanl f z xs) == foldl f z xs
--
scanl
    :: (Word8 -> Word8 -> Word8)
    -- ^ accumulator -> element -> new accumulator
    -> Word8
    -- ^ starting value of accumulator
    -> ByteString
    -- ^ input of length n
    -> ByteString
    -- ^ output of length n+1
scanl function = fmap (uncurry (flip snoc)) . mapAccumL (\x y -> (function x y, x))
{-# INLINE scanl #-}

-- | 'scanl1' is a variant of 'scanl' that has no starting value argument.
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
--
-- @since 0.11.2.0
scanl1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanl1 function byteStream = case uncons byteStream of
  Nothing -> Empty
  Just (firstByte, remainingBytes) -> scanl function firstByte remainingBytes

-- | 'scanr' is similar to 'foldr', but returns a list of successive
-- reduced values from the right.
--
-- > scanr f z [..., x{n-1}, xn] == [..., x{n-1} `f` (xn `f` z), xn `f` z, z]
--
-- Note that
--
-- > head (scanr f z xs) == foldr f z xs
-- > last (scanr f z xs) == z
--
-- @since 0.11.2.0
scanr
    :: (Word8 -> Word8 -> Word8)
    -- ^ element -> accumulator -> new accumulator
    -> Word8
    -- ^ starting value of accumulator
    -> ByteString
    -- ^ input of length n
    -> ByteString
    -- ^ output of length n+1
scanr function = fmap (uncurry cons) . mapAccumR (\x y -> (function y x, x))

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
--
-- @since 0.11.2.0
scanr1 :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString
scanr1 function byteStream = case unsnoc byteStream of
  Nothing -> Empty
  Just (initialBytes, lastByte) -> scanr function lastByte initialBytes

-- ---------------------------------------------------------------------
-- Unfolds and replicates

-- | @'iterate' f x@ returns an infinite ByteString of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
--
iterate :: (Word8 -> Word8) -> Word8 -> ByteString
iterate f = unfoldr (\x -> case f x of !x' -> Just (x', x'))

-- | @'repeat' x@ is an infinite ByteString, with @x@ the value of every
-- element.
--
repeat :: Word8 -> ByteString
repeat w = cs where cs = Chunk (S.replicate smallChunkSize w) cs

-- | /O(n)/ @'replicate' n x@ is a ByteString of length @n@ with @x@
-- the value of every element.
--
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

-- | 'cycle' ties a finite ByteString into a circular one, or equivalently,
-- the infinite repetition of the original ByteString.
--
cycle :: HasCallStack => ByteString -> ByteString
cycle Empty = errorEmptyList "cycle"
cycle cs    = cs' where cs' = foldrChunks Chunk cs' cs

-- | /O(n)/ The 'unfoldr' function is analogous to the List \'unfoldr\'.
-- 'unfoldr' builds a ByteString from a seed value.  The function takes
-- the element and returns 'Nothing' if it is done producing the
-- ByteString or returns 'Just' @(a,b)@, in which case, @a@ is a
-- prepending to the ByteString and @b@ is used as the next element in a
-- recursive call.
unfoldr :: (a -> Maybe (Word8, a)) -> a -> ByteString
unfoldr f = unfoldChunk 32
  where unfoldChunk n x =
          case S.unfoldrN n f x of
            (c, Nothing)
              | S.null c  -> Empty
              | otherwise -> Chunk c Empty
            (c, Just x')  -> Chunk c (unfoldChunk (n*2) x')

-- ---------------------------------------------------------------------
-- Substrings

-- | /O(n\/c)/ 'take' @n@, applied to a ByteString @xs@, returns the prefix
-- of @xs@ of length @n@, or @xs@ itself if @n > 'length' xs@.
take :: Int64 -> ByteString -> ByteString
take i _ | i <= 0 = Empty
take i cs0         = take' i cs0
  where take' 0 _            = Empty
        take' _ Empty        = Empty
        take' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then Chunk (S.take (fromIntegral n) c) Empty
            else Chunk c (take' (n - fromIntegral (S.length c)) cs)

-- | /O(c)/ @'takeEnd' n xs@ is equivalent to @'drop' ('length' xs - n) xs@.
-- Takes @n@ elements from end of bytestring.
--
-- >>> takeEnd 3 "abcdefg"
-- "efg"
-- >>> takeEnd 0 "abcdefg"
-- ""
-- >>> takeEnd 4 "abc"
-- "abc"
--
-- @since 0.11.2.0
takeEnd :: Int64 -> ByteString -> ByteString
takeEnd i bs
  | i <= 0 = Empty
  | otherwise = splitAtEndFold (\_ res -> res) id i bs

-- | Helper function for implementing 'takeEnd' and 'dropEnd'
splitAtEndFold
  :: forall result
  .  (S.StrictByteString -> result -> result)
  -- ^ What to do when one chunk of output is ready
  -- (The StrictByteString will not be empty.)
  -> (ByteString -> result)
  -- ^ What to do when the split-point is reached
  -> Int64
  -- ^ Number of bytes to leave at the end (must be strictly positive)
  -> ByteString -- ^ Input ByteString
  -> result
{-# INLINE splitAtEndFold #-}
splitAtEndFold step end len bs0 = assert (len > 0) $ case bs0 of
  Empty -> end Empty
  Chunk c t -> goR len c t t
 where
  -- Idea: Keep two references into the input ByteString:
  --   "toSplit" tracks the current split point,
  --   "toScan"  tracks the yet-unprocessed tail.
  -- When they are closer than "len" bytes apart, process more input.  ("goR")
  -- When they are  at  least  "len" bytes apart, produce more output. ("goL")
  -- We always have that "toScan" is a suffix of "toSplit",
  -- and "toSplit" is a suffix of the original input (bs0).
  goR :: Int64 -> S.StrictByteString -> ByteString -> ByteString -> result
  goR !undershoot nextOutput@(S.BS noFp noLen) toSplit toScan =
      assert (undershoot > 0) $
      -- INVARIANT: length toSplit == length toScan + len - undershoot
      -- (not 'assert'ed because that would break our laziness properties)
      case toScan of
    Empty
      | undershoot >= intToInt64 noLen
        -> end (Chunk nextOutput toSplit)
      | undershootW <- fromIntegral @Int64 @Int undershoot
        -- conversion Int64->Int is OK because 0 < undershoot < noLen
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
      -- INVARIANT: length toSplit == length toScan + len + intToInt64 overshoot
      -- (not 'assert'ed because that would break our laziness properties)
      case toSplit of
    Empty -> splitAtEndFoldInvariantFailed
    Chunk c@(S.BS _ cLen) newBsL
      | overshoot >= cLen
        -> step c $ goL (overshoot - cLen) newBsL toScan
      | otherwise
        -> goR (intToInt64 $ cLen - overshoot) c newBsL toScan

splitAtEndFoldInvariantFailed :: a
-- See Note [Float error calls out of INLINABLE things] in D.B.Internal.Type
splitAtEndFoldInvariantFailed =
  moduleError "splitAtEndFold"
              "internal error: toSplit not longer than toScan"

-- | /O(n\/c)/ 'drop' @n xs@ returns the suffix of @xs@ after the first @n@
-- elements, or 'empty' if @n > 'length' xs@.
drop  :: Int64 -> ByteString -> ByteString
drop i p | i <= 0 = p
drop i cs0 = drop' i cs0
  where drop' 0 cs           = cs
        drop' _ Empty        = Empty
        drop' n (Chunk c cs) =
          if n < fromIntegral (S.length c)
            then Chunk (S.drop (fromIntegral n) c) cs
            else drop' (n - fromIntegral (S.length c)) cs

-- | /O(n)/ @'dropEnd' n xs@ is equivalent to @'take' ('length' xs - n) xs@.
-- Drops @n@ elements from end of bytestring.
--
-- >>> dropEnd 3 "abcdefg"
-- "abcd"
-- >>> dropEnd 0 "abcdefg"
-- "abcdefg"
-- >>> dropEnd 4 "abc"
-- ""
--
-- @since 0.11.2.0
dropEnd :: Int64 -> ByteString -> ByteString
dropEnd i p
  | i <= 0 = p
  | otherwise = splitAtEndFold Chunk (const Empty) i p

-- | /O(n\/c)/ 'splitAt' @n xs@ is equivalent to @('take' n xs, 'drop' n xs)@.
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


-- | Similar to 'Prelude.takeWhile',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate.
takeWhile :: (Word8 -> Bool) -> ByteString -> ByteString
takeWhile f = takeWhile'
  where takeWhile' Empty        = Empty
        takeWhile' (Chunk c cs) =
          case S.findIndexOrLength (not . f) c of
            0                  -> Empty
            n | n < S.length c -> Chunk (S.take n c) Empty
              | otherwise      -> Chunk c (takeWhile' cs)

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate.
--
-- @'takeWhileEnd' p@ is equivalent to @'reverse' . 'takeWhile' p . 'reverse'@.
--
-- >>> {-# LANGUAGE OverloadedLists #-)
-- >>> takeWhileEnd even [1,2,3,4,6]
-- [4,6]
--
-- @since 0.11.2.0
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

-- | Similar to 'Prelude.dropWhile',
-- drops the longest (possibly empty) prefix of elements
-- satisfying the predicate and returns the remainder.
dropWhile :: (Word8 -> Bool) -> ByteString -> ByteString
dropWhile f = dropWhile'
  where dropWhile' Empty        = Empty
        dropWhile' (Chunk c cs) =
          case S.findIndexOrLength (not . f) c of
            n | n < S.length c -> Chunk (S.drop n c) cs
              | otherwise      -> dropWhile' cs

-- | Similar to 'Prelude.dropWhileEnd',
-- drops the longest (possibly empty) suffix of elements
-- satisfying the predicate and returns the remainder.
--
-- @'dropWhileEnd' p@ is equivalent to @'reverse' . 'dropWhile' p . 'reverse'@.
--
-- >>> {-# LANGUAGE OverloadedLists #-)
-- >>> dropWhileEnd even [1,2,3,4,6]
-- [1,2,3]
--
-- @since 0.11.2.0
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

-- | Similar to 'Prelude.break',
-- returns the longest (possibly empty) prefix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'break' @p@ is equivalent to @'span' (not . p)@ and to @('takeWhile' (not . p) &&& 'dropWhile' (not . p))@.
--
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


-- | Returns the longest (possibly empty) suffix of elements which __do not__
-- satisfy the predicate and the remainder of the string.
--
-- 'breakEnd' @p@ is equivalent to @'spanEnd' (not . p)@ and to @('dropWhileEnd' (not . p) &&& 'takeWhileEnd' (not . p))@.
--
-- @since 0.11.2.0
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


--
-- TODO
--
-- Add rules
--

{-
-- | 'breakByte' breaks its ByteString argument at the first occurrence
-- of the specified byte. It is more efficient than 'break' as it is
-- implemented with @memchr(3)@. I.e.
--
-- > break (==99) "abcd" == breakByte 99 "abcd" -- fromEnum 'c' == 99
--
breakByte :: Word8 -> ByteString -> (ByteString, ByteString)
breakByte c (LPS ps) = case (breakByte' ps) of (a,b) -> (LPS a, LPS b)
  where breakByte' []     = ([], [])
        breakByte' (x:xs) =
          case P.elemIndex c x of
            Just 0  -> ([], x : xs)
            Just n  -> (P.take n x : [], P.drop n x : xs)
            Nothing -> let (xs', xs'') = breakByte' xs
                        in (x : xs', xs'')

-- | 'spanByte' breaks its ByteString argument at the first
-- occurrence of a byte other than its argument. It is more efficient
-- than 'span (==)'
--
-- > span  (==99) "abcd" == spanByte 99 "abcd" -- fromEnum 'c' == 99
--
spanByte :: Word8 -> ByteString -> (ByteString, ByteString)
spanByte c (LPS ps) = case (spanByte' ps) of (a,b) -> (LPS a, LPS b)
  where spanByte' []     = ([], [])
        spanByte' (x:xs) =
          case P.spanByte c x of
            (x', x'') | P.null x'  -> ([], x : xs)
                      | P.null x'' -> let (xs', xs'') = spanByte' xs
                                       in (x : xs', xs'')
                      | otherwise  -> (x' : [], x'' : xs)
-}

-- | Similar to 'Prelude.span',
-- returns the longest (possibly empty) prefix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'span' @p@ is equivalent to @'break' (not . p)@ and to @('takeWhile' p &&& 'dropWhile' p)@.
--
span :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
span p = break (not . p)

-- | Returns the longest (possibly empty) suffix of elements
-- satisfying the predicate and the remainder of the string.
--
-- 'spanEnd' @p@ is equivalent to @'breakEnd' (not . p)@ and to @('dropWhileEnd' p &&& 'takeWhileEnd' p)@.
--
-- We have
--
-- > spanEnd (not . isSpace) "x y z" == ("x y ", "z")
--
-- and
--
-- > spanEnd (not . isSpace) ps
-- >    ==
-- > let (x, y) = span (not . isSpace) (reverse ps) in (reverse y, reverse x)
--
-- @since 0.11.2.0
spanEnd :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
spanEnd p = breakEnd (not . p)

-- | /O(n)/ Splits a 'ByteString' into components delimited by
-- separators, where the predicate returns True for a separator element.
-- The resulting components do not contain the separators.  Two adjacent
-- separators result in an empty component in the output.  eg.
--
-- > splitWith (==97) "aabbaca" == ["","","bb","c",""] -- fromEnum 'a' == 97
-- > splitWith undefined ""     == []                  -- and not [""]
--
splitWith :: (Word8 -> Bool) -> ByteString -> [ByteString]
splitWith _ Empty          = []
splitWith p (Chunk c0 cs0) = comb [] (S.splitWith p c0) cs0

  where comb :: [P.ByteString] -> [P.ByteString] -> ByteString -> [ByteString]
        comb acc [s] Empty        = [revChunks (s:acc)]
        comb acc [s] (Chunk c cs) = comb (s:acc) (S.splitWith p c) cs
        comb acc (s:ss) cs        = revChunks (s:acc) : comb [] ss cs
        comb _ [] _ = error "Strict splitWith returned [] for nonempty input"
{-# INLINE splitWith #-}

-- | /O(n)/ Break a 'ByteString' into pieces separated by the byte
-- argument, consuming the delimiter. I.e.
--
-- > split 10  "a\nb\nd\ne" == ["a","b","d","e"]   -- fromEnum '\n' == 10
-- > split 97  "aXaXaXa"    == ["","X","X","X",""] -- fromEnum 'a' == 97
-- > split 120 "x"          == ["",""]             -- fromEnum 'x' == 120
-- > split undefined ""     == []                  -- and not [""]
--
-- and
--
-- > intercalate [c] . split c == id
-- > split == splitWith . (==)
--
-- As for all splitting functions in this library, this function does
-- not copy the substrings, it just constructs new 'ByteString's that
-- are slices of the original.
--
split :: Word8 -> ByteString -> [ByteString]
split _ Empty     = []
split w (Chunk c0 cs0) = comb [] (S.split w c0) cs0

  where comb :: [P.ByteString] -> [P.ByteString] -> ByteString -> [ByteString]
        comb acc [s] Empty        = [revChunks (s:acc)]
        comb acc [s] (Chunk c cs) = comb (s:acc) (S.split w c) cs
        comb acc (s:ss) cs        = revChunks (s:acc) : comb [] ss cs
        comb _ [] _ = error "Strict split returned [] for nonempty input"
{-# INLINE split #-}

-- | The 'group' function takes a ByteString and returns a list of
-- ByteStrings such that the concatenation of the result is equal to the
-- argument.  Moreover, each string in the result contains only equal
-- elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to
-- supply their own equality test.
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

-- | The 'groupBy' function is the non-overloaded version of 'group'.
--
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

-- | /O(n)/ The 'intercalate' function takes a 'ByteString' and a list of
-- 'ByteString's and concatenates the list after interspersing the first
-- argument between each element of the list.
intercalate :: ByteString -> [ByteString] -> ByteString
intercalate s = concat . List.intersperse s

-- ---------------------------------------------------------------------
-- Indexing ByteStrings

-- | /O(c)/ 'ByteString' index (subscript) operator, starting from 0.
--
-- This is a partial function, consider using 'indexMaybe' instead.
index :: HasCallStack => ByteString -> Int64 -> Word8
index _  i | i < 0  = moduleError "index" ("negative index: " ++ show i)
index cs0 i         = index' cs0 i
  where index' Empty     n = moduleError "index" ("index too large: " ++ show n)
        index' (Chunk c cs) n
          | n >= fromIntegral (S.length c) =
              index' cs (n - fromIntegral (S.length c))
          | otherwise       = S.unsafeIndex c (fromIntegral n)

-- | /O(c)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
indexMaybe :: ByteString -> Int64 -> Maybe Word8
indexMaybe _ i | i < 0 = Nothing
indexMaybe cs0 i       = index' cs0 i
  where index' Empty _ = Nothing
        index' (Chunk c cs) n
          | n >= fromIntegral (S.length c) =
              index' cs (n - fromIntegral (S.length c))
          | otherwise       = Just $! S.unsafeIndex c (fromIntegral n)

-- | /O(1)/ 'ByteString' index, starting from 0, that returns 'Just' if:
--
-- > 0 <= n < length bs
--
-- @since 0.11.0.0
(!?) :: ByteString -> Int64 -> Maybe Word8
(!?) = indexMaybe
{-# INLINE (!?) #-}

-- | /O(n)/ The 'elemIndex' function returns the index of the first
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element.
-- This implementation uses memchr(3).
elemIndex :: Word8 -> ByteString -> Maybe Int64
elemIndex w = elemIndex' 0
  where elemIndex' _ Empty        = Nothing
        elemIndex' n (Chunk c cs) =
          case S.elemIndex w c of
            Nothing -> elemIndex' (n + fromIntegral (S.length c)) cs
            Just i  -> Just (n + fromIntegral i)

-- | /O(n)/ The 'elemIndexEnd' function returns the last index of the
-- element in the given 'ByteString' which is equal to the query
-- element, or 'Nothing' if there is no such element. The following
-- holds:
--
-- > elemIndexEnd c xs = case elemIndex c (reverse xs) of
-- >   Nothing -> Nothing
-- >   Just i  -> Just (length xs - 1 - i)
--
-- @since 0.10.6.0
elemIndexEnd :: Word8 -> ByteString -> Maybe Int64
elemIndexEnd = findIndexEnd . (==)
{-# INLINE elemIndexEnd #-}

-- | /O(n)/ The 'elemIndices' function extends 'elemIndex', by returning
-- the indices of all elements equal to the query element, in ascending order.
-- This implementation uses memchr(3).
elemIndices :: Word8 -> ByteString -> [Int64]
elemIndices w = elemIndices' 0
  where elemIndices' _ Empty        = []
        elemIndices' n (Chunk c cs) = List.map ((+n).fromIntegral) (S.elemIndices w c)
                             ++ elemIndices' (n + fromIntegral (S.length c)) cs

-- | count returns the number of times its argument appears in the ByteString
--
-- > count = length . elemIndices
--
-- But more efficiently than using length on the intermediate list.
count :: Word8 -> ByteString -> Int64
count w = foldlChunks (\n c -> n + fromIntegral (S.count w c)) 0

-- | The 'findIndex' function takes a predicate and a 'ByteString' and
-- returns the index of the first element in the ByteString
-- satisfying the predicate.
findIndex :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndex k = findIndex' 0
  where findIndex' _ Empty        = Nothing
        findIndex' n (Chunk c cs) =
          case S.findIndex k c of
            Nothing -> findIndex' (n + fromIntegral (S.length c)) cs
            Just i  -> Just (n + fromIntegral i)
{-# INLINE findIndex #-}

-- | The 'findIndexEnd' function takes a predicate and a 'ByteString' and
-- returns the index of the last element in the ByteString
-- satisfying the predicate.
--
-- @since 0.10.12.0
findIndexEnd :: (Word8 -> Bool) -> ByteString -> Maybe Int64
findIndexEnd k = findIndexEnd' 0
  where
    findIndexEnd' _ Empty = Nothing
    findIndexEnd' n (Chunk c cs) =
      let !n' = n + S.length c
          !i  = fromIntegral . (n +) <$> S.findIndexEnd k c
      in findIndexEnd' n' cs `mplus` i
{-# INLINE findIndexEnd #-}

-- | /O(n)/ The 'find' function takes a predicate and a ByteString,
-- and returns the first element in matching the predicate, or 'Nothing'
-- if there is no such element.
--
-- > find f p = case findIndex f p of Just n -> Just (p ! n) ; _ -> Nothing
--
find :: (Word8 -> Bool) -> ByteString -> Maybe Word8
find f = find'
  where find' Empty        = Nothing
        find' (Chunk c cs) = case S.find f c of
            Nothing -> find' cs
            Just w  -> Just w
{-# INLINE find #-}

-- | The 'findIndices' function extends 'findIndex', by returning the
-- indices of all elements satisfying the predicate, in ascending order.
findIndices :: (Word8 -> Bool) -> ByteString -> [Int64]
findIndices k = findIndices' 0
  where findIndices' _ Empty        = []
        findIndices' n (Chunk c cs) = List.map ((+n).fromIntegral) (S.findIndices k c)
                             ++ findIndices' (n + fromIntegral (S.length c)) cs
{-# INLINE findIndices #-}

-- ---------------------------------------------------------------------
-- Searching ByteStrings

-- | /O(n)/ 'elem' is the 'ByteString' membership predicate.
elem :: Word8 -> ByteString -> Bool
elem w cs = case elemIndex w cs of Nothing -> False ; _ -> True

-- | /O(n)/ 'notElem' is the inverse of 'elem'
notElem :: Word8 -> ByteString -> Bool
notElem w cs = not (w `elem` cs)

-- | /O(n)/ 'filter', applied to a predicate and a ByteString,
-- returns a ByteString containing those characters that satisfy the
-- predicate.
filter :: (Word8 -> Bool) -> ByteString -> ByteString
filter p = go
    where
        go Empty        = Empty
        go (Chunk x xs) = chunk (S.filter p x) (go xs)
{-# INLINE filter #-}

{-
-- | /O(n)/ and /O(n\/c) space/ A first order equivalent of /filter .
-- (==)/, for the common case of filtering a single byte. It is more
-- efficient to use /filterByte/ in this case.
--
-- > filterByte == filter . (==)
--
-- filterByte is around 10x faster, and uses much less space, than its
-- filter equivalent
filterByte :: Word8 -> ByteString -> ByteString
filterByte w ps = replicate (count w ps) w
{-# INLINE filterByte #-}

{-# RULES
"ByteString specialise filter (== x)" forall x.
  filter ((==) x) = filterByte x

"ByteString specialise filter (== x)" forall x.
 filter (== x) = filterByte x
  #-}
-}

{-
-- | /O(n)/ A first order equivalent of /filter . (\/=)/, for the common
-- case of filtering a single byte out of a list. It is more efficient
-- to use /filterNotByte/ in this case.
--
-- > filterNotByte == filter . (/=)
--
-- filterNotByte is around 2x faster than its filter equivalent.
filterNotByte :: Word8 -> ByteString -> ByteString
filterNotByte w (LPS xs) = LPS (filterMap (P.filterNotByte w) xs)
-}

-- | /O(n)/ The 'partition' function takes a predicate a ByteString and returns
-- the pair of ByteStrings with elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p bs == (filter p xs, filter (not . p) xs)
--
partition :: (Word8 -> Bool) -> ByteString -> (ByteString, ByteString)
partition _ Empty = (Empty, Empty)
partition p (Chunk x xs) = (chunk t ts, chunk f fs)
  where
    (t,   f) = S.partition p x
    (ts, fs) = partition   p xs

-- ---------------------------------------------------------------------
-- Searching for substrings

-- | /O(n)/ The 'isPrefixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a prefix of the second.
isPrefixOf :: ByteString -> ByteString -> Bool
isPrefixOf Empty _  = True
isPrefixOf _ Empty  = False
isPrefixOf (Chunk x xs) (Chunk y ys)
    | S.length x == S.length y = x == y  && isPrefixOf xs ys
    | S.length x <  S.length y = x == yh && isPrefixOf xs (Chunk yt ys)
    | otherwise                = xh == y && isPrefixOf (Chunk xt xs) ys
  where (xh,xt) = S.splitAt (S.length y) x
        (yh,yt) = S.splitAt (S.length x) y

-- | /O(n)/ The 'stripPrefix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its prefix, and otherwise
-- 'Nothing'.
--
-- @since 0.10.8.0
stripPrefix :: ByteString -> ByteString -> Maybe ByteString
stripPrefix Empty bs  = Just bs
stripPrefix _ Empty  = Nothing
stripPrefix (Chunk x xs) (Chunk y ys)
    | S.length x == S.length y = if x == y then stripPrefix xs ys else Nothing
    | S.length x <  S.length y = do yt <- S.stripPrefix x y
                                    stripPrefix xs (Chunk yt ys)
    | otherwise                = do xt <- S.stripPrefix y x
                                    stripPrefix (Chunk xt xs) ys

-- | /O(n)/ The 'isSuffixOf' function takes two ByteStrings and returns 'True'
-- iff the first is a suffix of the second.
--
-- The following holds:
--
-- > isSuffixOf x y == reverse x `isPrefixOf` reverse y
--
isSuffixOf :: ByteString -> ByteString -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y
--TODO: a better implementation

-- | /O(n)/ The 'stripSuffix' function takes two ByteStrings and returns 'Just'
-- the remainder of the second iff the first is its suffix, and otherwise
-- 'Nothing'.
stripSuffix :: ByteString -> ByteString -> Maybe ByteString
stripSuffix x y = reverse <$> stripPrefix (reverse x) (reverse y)
--TODO: a better implementation

-- ---------------------------------------------------------------------
-- Zipping

-- | /O(n)/ 'zip' takes two ByteStrings and returns a list of
-- corresponding pairs of bytes. If one input ByteString is short,
-- excess elements of the longer ByteString are discarded. This is
-- equivalent to a pair of 'unpack' operations.
zip :: ByteString -> ByteString -> [(Word8,Word8)]
zip = zipWith (,)

-- | 'zipWith' generalises 'zip' by zipping with the function given as
-- the first argument, instead of a tupling function.  For example,
-- @'zipWith' (+)@ is applied to two ByteStrings to produce the list of
-- corresponding sums.
zipWith :: (Word8 -> Word8 -> a) -> ByteString -> ByteString -> [a]
zipWith _ Empty     _  = []
zipWith _ _      Empty = []
zipWith f (Chunk a as) (Chunk b bs) = go a as b bs
  where
    -- This loop is written in a slightly awkward way but ensures we
    -- don't have to allocate any 'Chunk' objects to pass to a recursive
    -- call.  We have in some sense performed SpecConstr manually.
    go !x xs !y ys = let
      -- Creating a thunk for reading one byte would
      -- be wasteful, so we evaluate these eagerly.
      -- See also #558 for a similar issue with uncons.
      !xHead = S.unsafeHead x
      !yHead = S.unsafeHead y
      in f xHead yHead : to (S.unsafeTail x) xs (S.unsafeTail y) ys

    to !x xs !y ys
      | Chunk x' xs' <- chunk x xs
      , Chunk y' ys' <- chunk y ys
      = go x' xs' y' ys'
      | otherwise = []

-- | A specialised version of `zipWith` for the common case of a
-- simultaneous map over two ByteStrings, to build a 3rd.
--
-- @since 0.11.1.0
packZipWith :: (Word8 -> Word8 -> Word8) -> ByteString -> ByteString -> ByteString
packZipWith _ Empty _ = Empty
packZipWith _ _ Empty = Empty
packZipWith f (Chunk a@(S.BS _ al) as) (Chunk b@(S.BS _ bl) bs) = Chunk (S.packZipWith f a b) $
    case compare al bl of
        LT -> packZipWith f as $ Chunk (S.drop al b) bs
        EQ -> packZipWith f as bs
        GT -> packZipWith f (Chunk (S.drop bl a) as) bs
{-# INLINE packZipWith #-}

-- | /O(n)/ 'unzip' transforms a list of pairs of bytes into a pair of
-- ByteStrings. Note that this performs two 'pack' operations.
unzip :: [(Word8,Word8)] -> (ByteString,ByteString)
unzip ls = (pack (List.map fst ls), pack (List.map snd ls))
{-# INLINE unzip #-}

-- ---------------------------------------------------------------------
-- Special lists

-- | Returns all initial segments of the given 'ByteString', shortest first.
inits :: ByteString -> [ByteString]
-- see Note [Avoid NonEmpty combinators] in Data.ByteString
inits bs = NE.toList $! initsNE bs

-- | Returns all initial segments of the given 'ByteString', shortest first.
--
-- @since 0.11.4.0
initsNE :: ByteString -> NonEmpty ByteString
-- see Note [Avoid NonEmpty combinators] in Data.ByteString
initsNE = (Empty :|) . inits' id
  where
    inits' :: (ByteString -> ByteString) -> ByteString -> [ByteString]
    -- inits' f bs === map f (tail (inits bs))
    inits' _ Empty = []
    inits' f (Chunk c@(S.BS x len) cs)
      = [f (S.BS x n `Chunk` Empty) | n <- [1..len]]
      ++ inits' (f . Chunk c) cs

-- | /O(n)/ Returns all final segments of the given 'ByteString', longest first.
tails :: ByteString -> [ByteString]
-- see Note [Avoid NonEmpty combinators] in Data.ByteString
tails bs = NE.toList $! tailsNE bs

-- | /O(n)/ Returns all final segments of the given 'ByteString', longest first.
--
-- @since 0.11.4.0
tailsNE :: ByteString -> NonEmpty ByteString
-- see Note [Avoid NonEmpty combinators] in Data.ByteString
tailsNE bs = case uncons bs of
  Nothing -> Empty :| []
  Just (_, tl) -> bs :| tails tl


-- ---------------------------------------------------------------------
-- Low level constructors

-- | /O(n)/ Make a copy of the 'ByteString' with its own storage.
--   This is mainly useful to allow the rest of the data pointed
--   to by the 'ByteString' to be garbage collected, for example
--   if a large string has been read in, and only a small part of it
--   is needed in the rest of the program.
copy :: ByteString -> ByteString
copy = foldrChunks (Chunk . S.copy) Empty
--TODO, we could coalese small blocks here
--FIXME: probably not strict enough, if we're doing this to avoid retaining
-- the parent blocks then we'd better copy strictly.

-- ---------------------------------------------------------------------

-- TODO defrag func that concatenates block together that are below a threshold
-- defrag :: ByteString -> ByteString

-- ---------------------------------------------------------------------
-- Lazy ByteString IO
--
-- Rule for when to close: is it expected to read the whole file?
-- If so, close when done.
--

-- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- are read on demand, in at most @k@-sized chunks. It does not block
-- waiting for a whole @k@-sized chunk, so if less than @k@ bytes are
-- available then they will be returned immediately as a smaller chunk.
--
-- The handle is closed on EOF.
--
hGetContentsN :: Int -> Handle -> IO ByteString
hGetContentsN k h = lazyRead -- TODO close on exceptions
  where
    lazyRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetSome h k -- only blocks if there is no data available
        if S.null c
          then hClose h >> return Empty
          else Chunk c <$> lazyRead

-- | Read @n@ bytes into a 'ByteString', directly from the
-- specified 'Handle', in chunks of size @k@.
--
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

-- | hGetNonBlockingN is similar to 'hGetContentsN', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available. Chunks are read on demand, in @k@-sized chunks.
--
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
    --TODO: System.IO uses InvalidArgument here, but it's not exported :-(
    where
      msg = fn ++ ": illegal ByteString size " ++ showsPrec 9 sz []

-- | Read entire handle contents /lazily/ into a 'ByteString'. Chunks
-- are read on demand, using the default chunk size.
--
-- File handles are closed on EOF if all the file is read, or through
-- garbage collection otherwise.
--
hGetContents :: Handle -> IO ByteString
hGetContents = hGetContentsN defaultChunkSize

-- | Read @n@ bytes into a 'ByteString', directly from the specified 'Handle'.
--
hGet :: Handle -> Int -> IO ByteString
hGet = hGetN defaultChunkSize

-- | hGetNonBlocking is similar to 'hGet', except that it will never block
-- waiting for data to become available, instead it returns only whatever data
-- is available.  If there is no data available to be read, 'hGetNonBlocking'
-- returns 'empty'.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hGet'.
--
hGetNonBlocking :: Handle -> Int -> IO ByteString
hGetNonBlocking = hGetNonBlockingN defaultChunkSize

-- | Read an entire file /lazily/ into a 'ByteString'.
--
-- The 'Handle' will be held open until EOF is encountered.
--
-- Note that this function's implementation relies on 'hGetContents'.
-- The reader is advised to read its documentation.
--
readFile :: FilePath -> IO ByteString
readFile f = openBinaryFile f ReadMode >>= hGetContents

modifyFile :: IOMode -> FilePath -> ByteString -> IO ()
modifyFile mode f txt = withBinaryFile f mode (`hPut` txt)

-- | Write a 'ByteString' to a file.
--
writeFile :: FilePath -> ByteString -> IO ()
writeFile = modifyFile WriteMode

-- | Append a 'ByteString' to a file.
--
appendFile :: FilePath -> ByteString -> IO ()
appendFile = modifyFile AppendMode

-- | getContents. Equivalent to hGetContents stdin. Will read /lazily/
--
getContents :: IO ByteString
getContents = hGetContents stdin

-- | Outputs a 'ByteString' to the specified 'Handle'.
--
-- The chunks will be
-- written one at a time. Other threads might write to the 'Handle' in between,
-- and hence 'hPut' alone is not suitable for concurrent writes.
--
hPut :: Handle -> ByteString -> IO ()
hPut h = foldrChunks (\c rest -> S.hPut h c >> rest) (return ())

-- | Similar to 'hPut' except that it will never block. Instead it returns
-- any tail that did not get written. This tail may be 'empty' in the case that
-- the whole string was written, or the whole original string if nothing was
-- written. Partial writes are also possible.
--
-- Note: on Windows and with Haskell implementation other than GHC, this
-- function does not work correctly; it behaves identically to 'hPut'.
--
hPutNonBlocking :: Handle -> ByteString -> IO ByteString
hPutNonBlocking _ Empty           = return Empty
hPutNonBlocking h bs@(Chunk c cs) = do
  c' <- S.hPutNonBlocking h c
  case S.length c' of
    l' | l' == S.length c -> hPutNonBlocking h cs
    0                     -> return bs
    _                     -> return (Chunk c' cs)

-- | A synonym for 'hPut', for compatibility
--
hPutStr :: Handle -> ByteString -> IO ()
hPutStr = hPut

-- | Write a ByteString to 'stdout'.
--
-- The chunks will be
-- written one at a time. Other threads might write to the 'stdout' in between,
-- and hence 'putStr' alone is not suitable for concurrent writes.
--
putStr :: ByteString -> IO ()
putStr = hPut stdout

-- | The interact function takes a function of type @ByteString -> ByteString@
-- as its argument. The entire input from the standard input device is passed
-- to this function as its argument, and the resulting string is output on the
-- standard output device.
--
interact :: (ByteString -> ByteString) -> IO ()
interact transformer = putStr . transformer =<< getContents

-- ---------------------------------------------------------------------
-- Internal utilities

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: HasCallStack => String -> a
errorEmptyList fun = moduleError fun "empty ByteString"
{-# NOINLINE errorEmptyList #-}

moduleError :: HasCallStack => String -> String -> a
moduleError fun msg = error ("Data.ByteString.Lazy." ++ fun ++ ':':' ':msg)
{-# NOINLINE moduleError #-}


-- reverse a list of non-empty chunks into a lazy ByteString
revNonEmptyChunks :: [P.ByteString] -> ByteString
revNonEmptyChunks = List.foldl' (flip Chunk) Empty

-- reverse a list of possibly-empty chunks into a lazy ByteString
revChunks :: [P.ByteString] -> ByteString
revChunks = List.foldl' (flip chunk) Empty

intToInt64 :: Int -> Int64
intToInt64 = fromIntegral @Int @Int64

{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Data.Binary
-- Copyright   : Lennart Kolmodin
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Lennart Kolmodin <kolmodin@gmail.com>
-- Stability   : unstable
-- Portability : portable to Hugs and GHC. Requires the FFI and some flexible instances.
--
-- Binary serialisation of Haskell values to and from lazy 'ByteString's.
-- The Binary library provides methods for encoding Haskell values as
-- streams of bytes directly in memory. The resulting 'ByteString' can
-- then be written to disk, sent over the network, or further processed
-- (for example, compressed with gzip).
--
-- The @binary@ package is notable in that it provides both pure, and
-- high performance serialisation.
--
-- Values encoded using the 'Binary' class are always encoded in network order
-- (big endian) form, and encoded data should be portable across
-- machine endianness, word size, or compiler version. For example,
-- data encoded using the 'Binary' class could be written on any machine,
-- and read back on any another.
--
-- If the specifics of the data format is not important to you, for example,
-- you are more interested in serializing and deserializing values than
-- in which format will be used, it is possible to derive 'Binary'
-- instances using the generic support. See 'GBinaryGet' and
-- 'GBinaryPut'.
--
-- If you have specific requirements about the encoding format, you can use
-- the encoding and decoding primitives directly, see the modules
-- "Data.Binary.Get" and "Data.Binary.Put".
--
-----------------------------------------------------------------------------

module Data.Binary (

    -- * The Binary class
      Binary(..)
    -- ** Example
    -- $example

    -- * Generic support
    -- $generics
    , GBinaryGet(..)
    , GBinaryPut(..)

    -- * The Get and Put monads
    , Get
    , Put

    -- * Useful helpers for writing instances
    , putWord8
    , getWord8

    -- * Binary serialisation
    , encode                    -- :: Binary a => a -> ByteString
    , decode                    -- :: Binary a => ByteString -> a
    , decodeOrFail

    -- * IO functions for serialisation
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

------------------------------------------------------------------------

-- $example
-- To serialise a custom type, an instance of Binary for that type is
-- required. For example, suppose we have a data structure:
--
-- > data Exp = IntE Int
-- >          | OpE  String Exp Exp
-- >    deriving Show
--
-- We can encode values of this type into bytestrings using the
-- following instance, which proceeds by recursively breaking down the
-- structure to serialise:
--
-- > instance Binary Exp where
-- >       put (IntE i)      = do put (0 :: Word8)
-- >                              put i
-- >       put (OpE s e1 e2) = do put (1 :: Word8)
-- >                              put s
-- >                              put e1
-- >                              put e2
-- >
-- >       get = do t <- get :: Get Word8
-- >                case t of
-- >                     0 -> do i <- get
-- >                             return (IntE i)
-- >                     1 -> do s  <- get
-- >                             e1 <- get
-- >                             e2 <- get
-- >                             return (OpE s e1 e2)
--
-- Note how we write an initial tag byte to indicate each variant of the
-- data type.
--
-- We can simplify the writing of 'get' instances using monadic
-- combinators:
--
-- >       get = do tag <- getWord8
-- >                case tag of
-- >                    0 -> liftM  IntE get
-- >                    1 -> liftM3 OpE  get get get
--
-- To serialise this to a bytestring, we use 'encode', which packs the
-- data structure into a binary format, in a lazy bytestring
--
-- > > let e = OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
-- > > let v = encode e
--
-- Where @v@ is a binary encoded data structure. To reconstruct the
-- original data, we use 'decode'
--
-- > > decode v :: Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- The lazy ByteString that results from 'encode' can be written to
-- disk, and read from disk using Data.ByteString.Lazy IO functions,
-- such as hPutStr or writeFile:
--
-- > > writeFile "/tmp/exp.txt" (encode e)
--
-- And read back with:
--
-- > > readFile "/tmp/exp.txt" >>= return . decode :: IO Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- We can also directly serialise a value to and from a Handle, or a file:
--
-- > > v <- decodeFile  "/tmp/exp.txt" :: IO Exp
-- > OpE "*" (IntE 7) (OpE "/" (IntE 4) (IntE 2))
--
-- And write a value to disk
--
-- > > encodeFile "/tmp/a.txt" v
--

------------------------------------------------------------------------
-- Wrappers to run the underlying monad

-- | Encode a value using binary serialisation to a lazy ByteString.
--
encode :: Binary a => a -> ByteString
encode = runPut . put
{-# INLINE encode #-}

-- | Decode a value from a lazy ByteString, reconstructing the original structure.
decode :: Binary a => ByteString -> a
decode = runGet get

-- | Decode a value from a lazy ByteString. Returning 'Left' on failure and
-- 'Right' on success. In both cases the unconsumed input and the number of
-- consumed bytes is returned. In case of failure, a human-readable error
-- message will be returned as well.
--
-- @since 0.7.0.0
decodeOrFail :: Binary a => L.ByteString
             -> Either (L.ByteString, ByteOffset, String)
                       (L.ByteString, ByteOffset, a)
decodeOrFail = runGetOrFail get


------------------------------------------------------------------------
-- Convenience IO operations

-- | Lazily serialise a value to a file.
--
-- This is just a convenience function, it's defined simply as:
--
-- > encodeFile f = B.writeFile f . encode
--
-- So for example if you wanted to compress as well, you could use:
--
-- > B.writeFile f . compress . encode
--
encodeFile :: Binary a => FilePath -> a -> IO ()
encodeFile f v = L.writeFile f (encode v)

-- | Decode a value from a file. In case of errors, 'error' will
-- be called with the error message.
--
-- @since 0.7.0.0
decodeFile :: Binary a => FilePath -> IO a
decodeFile f = do
  result <- decodeFileOrFail f
  case result of
    Right x -> return x
    Left (_,str) -> error str

-- | Decode a value from a file. In case of success, the value will be returned
-- in 'Right'. In case of decoder errors, the error message together with
-- the byte offset will be returned.
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

------------------------------------------------------------------------
-- $generics
--
-- Beginning with GHC 7.2, it is possible to use binary serialization
-- without writing any instance boilerplate code.
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import Data.Binary
-- > import GHC.Generics (Generic)
-- >
-- > data Foo = Foo
-- >          deriving (Generic)
-- >
-- > -- GHC will automatically fill out the instance
-- > instance Binary Foo
--
-- This mechanism makes use of GHC's efficient built-in generics
-- support.

{-# LANGUAGE Safe #-}

-- |
--
-- Module      :  Control.Monad
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'Functor', 'Monad' and 'MonadPlus' classes,
-- with some useful operations on monads.

module Control.Monad
    (-- *  Functor and monad classes
     Functor(..),
     Monad((>>=), (>>), return),
     MonadFail(fail),
     MonadPlus(mzero, mplus),
     -- *  Functions
     -- **  Naming conventions
     -- $naming
     -- **  Basic @Monad@ functions
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
     -- **  Generalisations of list functions
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
     -- **  Conditional execution of monadic expressions
     guard,
     when,
     unless,
     -- **  Monadic lifting operators
     liftM,
     liftM2,
     liftM3,
     liftM4,
     liftM5,
     ap,
     -- **  Strict monadic functions
     (<$!>)
     ) where

import GHC.Internal.Control.Monad

{- $naming

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

-}

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
           , RankNTypes
  #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
-- kludge for the Control.Concurrent.QSem, Control.Concurrent.QSemN
-- and Control.Concurrent.SampleVar imports.

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  non-portable (concurrency)
--
-- A common interface to a collection of useful concurrency
-- abstractions.
--
-----------------------------------------------------------------------------

module Control.Concurrent (
        -- * Concurrent Haskell

        -- $conc_intro

        -- * Basic concurrency operations

        ThreadId,
        myThreadId,

        forkIO,
        forkFinally,
        forkIOWithUnmask,
        killThread,
        throwTo,

        -- ** Threads with affinity
        forkOn,
        forkOnWithUnmask,
        getNumCapabilities,
        setNumCapabilities,
        threadCapability,

        -- * Scheduling

        -- $conc_scheduling
        yield,

        -- ** Blocking

        -- $blocking

        -- ** Waiting
        threadDelay,
        threadWaitRead,
        threadWaitWrite,
        threadWaitReadSTM,
        threadWaitWriteSTM,

        -- * Communication abstractions

        module GHC.Internal.Control.Concurrent.MVar,
        module Control.Concurrent.Chan,
        module Control.Concurrent.QSem,
        module Control.Concurrent.QSemN,

        -- * Bound Threads
        -- $boundthreads
        rtsSupportsBoundThreads,
        forkOS,
        forkOSWithUnmask,
        isCurrentThreadBound,
        runInBoundThread,
        runInUnboundThread,

        -- * Weak references to ThreadIds
        mkWeakThreadId,

        -- * GHC's implementation of concurrency

        -- |This section describes features specific to GHC's
        -- implementation of Concurrent Haskell.

        -- ** Haskell threads and Operating System threads

        -- $osthreads

        -- ** Terminating the program

        -- $termination

        -- ** Pre-emption

        -- $preemption

        -- ** Deadlock

        -- $deadlock

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

{- $conc_intro

The concurrency extension for Haskell is described in the paper
/Concurrent Haskell/
<http://www.haskell.org/ghc/docs/papers/concurrent-haskell.ps.gz>.

Concurrency is \"lightweight\", which means that both thread creation
and context switching overheads are extremely low.  Scheduling of
Haskell threads is done internally in the Haskell runtime system, and
doesn't make use of any operating system-supplied thread packages.

However, if you want to interact with a foreign library that expects your
program to use the operating system-supplied thread package, you can do so
by using 'forkOS' instead of 'forkIO'.

Haskell threads can communicate via 'MVar's, a kind of synchronised
mutable variable (see "Control.Concurrent.MVar").  Several common
concurrency abstractions can be built from 'MVar's, and these are
provided by the "Control.Concurrent" module.
In GHC, threads may also communicate via exceptions.
-}

{- $conc_scheduling

    Scheduling may be either pre-emptive or co-operative,
    depending on the implementation of Concurrent Haskell (see below
    for information related to specific compilers).  In a co-operative
    system, context switches only occur when you use one of the
    primitives defined in this module.  This means that programs such
    as:


>   main = forkIO (write 'a') >> write 'b'
>     where write c = putChar c >> write c

    will print either @aaaaaaaaaaaaaa...@ or @bbbbbbbbbbbb...@,
    instead of some random interleaving of @a@s and @b@s.  In
    practice, cooperative multitasking is sufficient for writing
    simple graphical user interfaces.
-}

{- $blocking
Different Haskell implementations have different characteristics with
regard to which operations block /all/ threads.

Using GHC without the @-threaded@ option, all foreign calls will block
all other Haskell threads in the system, although I\/O operations will
not.  With the @-threaded@ option, only foreign calls with the @unsafe@
attribute will block all other threads.

-}

-- | Fork a thread and call the supplied function when the thread is about
-- to terminate, with an exception or a returned value.  The function is
-- called with asynchronous exceptions masked.
--
-- > forkFinally action and_then =
-- >   mask $ \restore ->
-- >     forkIO $ try (restore action) >>= and_then
--
-- This function is useful for informing the parent when a child
-- terminates, for example.
--
-- @since 4.6.0.0
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then

-- ---------------------------------------------------------------------------
-- Bound Threads

{- $boundthreads
   #boundthreads#

Support for multiple operating system threads and bound threads as described
below is currently only available in the GHC runtime system if you use the
/-threaded/ option when linking.

Other Haskell systems do not currently support multiple operating system threads.

A bound thread is a haskell thread that is /bound/ to an operating system
thread. While the bound thread is still scheduled by the Haskell run-time
system, the operating system thread takes care of all the foreign calls made
by the bound thread.

To a foreign library, the bound thread will look exactly like an ordinary
operating system thread created using OS functions like @pthread_create@
or @CreateThread@.

Bound threads can be created using the 'forkOS' function below. All foreign
exported functions are run in a bound thread (bound to the OS thread that
called the function). Also, the @main@ action of every Haskell program is
run in a bound thread.

Why do we need this? Because if a foreign library is called from a thread
created using 'forkIO', it won't have access to any /thread-local state/ -
state variables that have specific values for each OS thread
(see POSIX's @pthread_key_create@ or Win32's @TlsAlloc@). Therefore, some
libraries (OpenGL, for example) will not work from a thread created using
'forkIO'. They work fine in threads created using 'forkOS' or when called
from @main@ or from a @foreign export@.

In terms of performance, 'forkOS' (aka bound) threads are much more
expensive than 'forkIO' (aka unbound) threads, because a 'forkOS'
thread is tied to a particular OS thread, whereas a 'forkIO' thread
can be run by any OS thread.  Context-switching between a 'forkOS'
thread and a 'forkIO' thread is many times more expensive than between
two 'forkIO' threads.

Note in particular that the main program thread (the thread running
@Main.main@) is always a bound thread, so for good concurrency
performance you should ensure that the main thread is not doing
repeated communication with other threads in the system.  Typically
this means forking subthreads to do the work using 'forkIO', and
waiting for the results in the main thread.

-}

-- ---------------------------------------------------------------------------
-- threadWaitRead/threadWaitWrite

-- | Block the current thread until data is available to read on the
-- given file descriptor (GHC only).
--
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitRead', use
-- 'GHC.Conc.closeFdWith'.
threadWaitRead :: Fd -> IO ()
threadWaitRead fd
#if defined(mingw32_HOST_OS)
  -- we have no IO manager implementing threadWaitRead on Windows.
  -- fdReady does the right thing, but we have to call it in a
  -- separate thread, otherwise threadWaitRead won't be interruptible,
  -- and this only works with -threaded.
  | threaded  = withThread (waitFd fd False)
  | otherwise = case fd of
                  0 -> do _ <- hWaitForInput stdin (-1)
                          return ()
                        -- hWaitForInput does work properly, but we can only
                        -- do this for stdin since we know its FD.
                  _ -> errorWithoutStackTrace "threadWaitRead requires -threaded on Windows, or use GHC.System.IO.hWaitForInput"
#else
  = Conc.threadWaitRead fd
#endif

-- | Block the current thread until data can be written to the
-- given file descriptor (GHC only).
--
-- This will throw an 'IOError' if the file descriptor was closed
-- while this thread was blocked.  To safely close a file descriptor
-- that has been used with 'threadWaitWrite', use
-- 'GHC.Conc.closeFdWith'.
threadWaitWrite :: Fd -> IO ()
threadWaitWrite fd
#if defined(mingw32_HOST_OS)
  | threaded  = withThread (waitFd fd True)
  | otherwise = errorWithoutStackTrace "threadWaitWrite requires -threaded on Windows"
#else
  = Conc.threadWaitWrite fd
#endif

-- | Returns an STM action that can be used to wait for data
-- to read from a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
--
-- @since 4.7.0.0
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

-- | Returns an STM action that can be used to wait until data
-- can be written to a file descriptor. The second returned value
-- is an IO action that can be used to deregister interest
-- in the file descriptor.
--
-- @since 4.7.0.0
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

-- ---------------------------------------------------------------------------
-- More docs

{- $osthreads

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
-}

{- $termination

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
-}

{- $preemption

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
-}

{- $deadlock

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
-}

{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative
-- Copyright   :  Conor McBride and Ross Paterson 2005
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- This module describes a structure intermediate between a functor and
-- a monad (technically, a strong lax monoidal functor).  Compared with
-- monads, this interface lacks the full power of the binding operation
-- '>>=', but
--
-- * it has more instances.
--
-- * it is sufficient for many uses, e.g. context-free parsing, or the
--   'Data.Traversable.Traversable' class.
--
-- * instances can perform analysis of computations before they are
--   executed, and thus produce shared optimizations.
--
-- This interface was introduced for parsers by Niklas R&#xF6;jemo, because
-- it admits more sharing than the monadic interface.  The names here are
-- mostly based on parsing work by Doaitse Swierstra.
--
-- For more details, see
-- <http://www.soi.city.ac.uk/~ross/papers/Applicative.html Applicative Programming with Effects>,
-- by Conor McBride and Ross Paterson.

module Control.Applicative (
    -- * Applicative functors
    Applicative(..),
    -- * Alternatives
    Alternative(..),
    -- * Instances
    Const(..), WrappedMonad(..), WrappedArrow(..), ZipList(..),
    -- * Utility functions
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

-- $setup
-- >>> import Prelude

newtype WrappedMonad m a = WrapMonad { unwrapMonad :: m a }
                         deriving ( Generic  -- ^ @since 4.7.0.0
                                  , Generic1 -- ^ @since 4.7.0.0
                                  , Monad    -- ^ @since 4.7.0.0
                                  )

-- | @since 2.01
instance Monad m => Functor (WrappedMonad m) where
    fmap f (WrapMonad v) = WrapMonad (liftM f v)

-- | @since 2.01
instance Monad m => Applicative (WrappedMonad m) where
    pure = WrapMonad . pure
    WrapMonad f <*> WrapMonad v = WrapMonad (f `ap` v)
    liftA2 f (WrapMonad x) (WrapMonad y) = WrapMonad (liftM2 f x y)

-- | @since 2.01
instance MonadPlus m => Alternative (WrappedMonad m) where
    empty = WrapMonad mzero
    WrapMonad u <|> WrapMonad v = WrapMonad (u `mplus` v)

-- | @since 4.14.0.0
deriving instance (Typeable (m :: Type -> Type), Typeable a, Data (m a))
         => Data (WrappedMonad m a)

newtype WrappedArrow a b c = WrapArrow { unwrapArrow :: a b c }
                           deriving ( Generic  -- ^ @since 4.7.0.0
                                    , Generic1 -- ^ @since 4.7.0.0
                                    )

-- | @since 2.01
instance Arrow a => Functor (WrappedArrow a b) where
    fmap f (WrapArrow a) = WrapArrow (a >>> arr f)

-- | @since 2.01
instance Arrow a => Applicative (WrappedArrow a b) where
    pure x = WrapArrow (arr (const x))
    liftA2 f (WrapArrow u) (WrapArrow v) =
      WrapArrow (u &&& v >>> arr (uncurry f))

-- | @since 2.01
instance (ArrowZero a, ArrowPlus a) => Alternative (WrappedArrow a b) where
    empty = WrapArrow zeroArrow
    WrapArrow u <|> WrapArrow v = WrapArrow (u <+> v)

-- | @since 4.14.0.0
deriving instance (Typeable (a :: Type -> Type -> Type), Typeable b, Typeable c,
                   Data (a b c))
         => Data (WrappedArrow a b c)

-- extra functions

-- | One or none.
--
-- It is useful for modelling any computation that is allowed to fail.
--
-- ==== __Examples__
--
-- Using the 'Alternative' instance of "Control.Monad.Except", the following functions:
--
-- >>> import Control.Monad.Except
--
-- >>> canFail = throwError "it failed" :: Except String Int
-- >>> final = return 42                :: Except String Int
--
-- Can be combined by allowing the first function to fail:
--
-- >>> runExcept $ canFail *> final
-- Left "it failed"
--
-- >>> runExcept $ optional canFail *> final
-- Right 42

optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing

{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE Trustworthy #-}

#include "lens-common.h"
{-# OPTIONS_GHC -Wno-orphans #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Lens.Fold
-- Copyright   :  (C) 2012-16 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  Rank2Types
--
-- A @'Fold' s a@ is a generalization of something 'Foldable'. It allows
-- you to extract multiple results from a container. A 'Foldable' container
-- can be characterized by the behavior of
-- @'Data.Foldable.foldMap' :: ('Foldable' t, 'Monoid' m) => (a -> m) -> t a -> m@.
-- Since we want to be able to work with monomorphic containers, we could
-- generalize this signature to @forall m. 'Monoid' m => (a -> m) -> s -> m@,
-- and then decorate it with 'Const' to obtain
--
-- @type 'Fold' s a = forall m. 'Monoid' m => 'Getting' m s a@
--
-- Every 'Getter' is a valid 'Fold' that simply doesn't use the 'Monoid'
-- it is passed.
--
-- In practice the type we use is slightly more complicated to allow for
-- better error messages and for it to be transformed by certain
-- 'Applicative' transformers.
--
-- Everything you can do with a 'Foldable' container, you can with with a 'Fold' and there are
-- combinators that generalize the usual 'Foldable' operations here.
----------------------------------------------------------------------------
module Control.Lens.Fold
  (
  -- * Folds
    Fold
  , IndexedFold

  -- * Getting Started
  , (^..)
  , (^?)
  , (^?!)
  , pre, ipre
  , preview, previews, ipreview, ipreviews
  , preuse, preuses, ipreuse, ipreuses

  , has, hasn't

  -- ** Building Folds
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

  -- ** Folding
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

  -- * Indexed Folds
  , (^@..)
  , (^@?)
  , (^@?!)

  -- ** Indexed Folding
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

  -- ** Building Indexed Folds
  , ifiltered
  , itakingWhile
  , idroppingWhile

  -- * Internal types
  , Leftmost
  , Rightmost
  , Traversed
  , Sequenced

  -- * Fold with Reified Monoid
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

-- $setup
-- >>> :set -XNoOverloadedStrings
-- >>> import Control.Lens
-- >>> import Control.Lens.Extras (is)
-- >>> import Data.Function
-- >>> import Data.List.Lens
-- >>> import Data.List.NonEmpty (NonEmpty (..))
-- >>> import Debug.SimpleReflect.Expr
-- >>> import Debug.SimpleReflect.Vars as Vars hiding (f,g)
-- >>> import Control.DeepSeq (NFData (..), force)
-- >>> import Control.Exception (evaluate)
-- >>> import Data.Maybe (fromMaybe)
-- >>> import Data.Monoid (Sum (..))
-- >>> import System.Timeout (timeout)
-- >>> import qualified Data.Map as Map
-- >>> let f :: Expr -> Expr; f = Debug.SimpleReflect.Vars.f
-- >>> let g :: Expr -> Expr; g = Debug.SimpleReflect.Vars.g
-- >>> let timingOut :: NFData a => a -> IO a; timingOut = fmap (fromMaybe (error "timeout")) . timeout (5*10^6) . evaluate . force

infixl 8 ^.., ^?, ^?!, ^@.., ^@?, ^@?!

--------------------------
-- Folds
--------------------------

-- | Obtain a 'Fold' by lifting an operation that returns a 'Foldable' result.
--
-- This can be useful to lift operations from @Data.List@ and elsewhere into a 'Fold'.
--
-- >>> [1,2,3,4]^..folding reverse
-- [4,3,2,1]
folding :: Foldable f => (s -> f a) -> Fold s a
folding sfa agb = phantom . traverse_ agb . sfa
{-# INLINE folding #-}

ifolding :: (Foldable f, Indexable i p, Contravariant g, Applicative g) => (s -> f (i, a)) -> Over p g s t a b
ifolding sfa f = phantom . traverse_ (phantom . uncurry (indexed f)) . sfa
{-# INLINE ifolding #-}

-- | Obtain a 'Fold' by lifting 'foldr' like function.
--
-- >>> [1,2,3,4]^..foldring foldr
-- [1,2,3,4]
foldring :: (Contravariant f, Applicative f) => ((a -> f a -> f a) -> f a -> s -> f a) -> LensLike f s t a b
foldring fr f = phantom . fr (\a fa -> f a *> fa) noEffect
{-# INLINE foldring #-}

-- | Obtain 'FoldWithIndex' by lifting 'ifoldr' like function.
ifoldring :: (Indexable i p, Contravariant f, Applicative f) => ((i -> a -> f a -> f a) -> f a -> s -> f a) -> Over p f s t a b
ifoldring ifr f = phantom . ifr (\i a fa -> indexed f i a *> fa) noEffect
{-# INLINE ifoldring #-}

-- | Obtain a 'Fold' from any 'Foldable' indexed by ordinal position.
--
-- >>> Just 3^..folded
-- [3]
--
-- >>> Nothing^..folded
-- []
--
-- >>> [(1,2),(3,4)]^..folded.both
-- [1,2,3,4]
folded :: Foldable f => IndexedFold Int (f a) a
folded = conjoined (foldring foldr) (ifoldring ifoldr)
{-# INLINE folded #-}

ifoldr :: Foldable f => (Int -> a -> b -> b) -> b -> f a -> b
ifoldr f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldr #-}

-- | Obtain a 'Fold' from any 'Foldable' indexed by ordinal position.
folded64 :: Foldable f => IndexedFold Int64 (f a) a
folded64 = conjoined (foldring foldr) (ifoldring ifoldr64)
{-# INLINE folded64 #-}

ifoldr64 :: Foldable f => (Int64 -> a -> b -> b) -> b -> f a -> b
ifoldr64 f z xs = foldr (\ x g i -> i `seq` f i x (g (i+1))) (const z) xs 0
{-# INLINE ifoldr64 #-}

-- | Form a 'Fold1' by repeating the input forever.
--
-- @
-- 'repeat' ≡ 'toListOf' 'repeated'
-- @
--
-- >>> timingOut $ 5^..taking 20 repeated
-- [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
--
-- @
-- 'repeated' :: 'Fold1' a a
-- @
repeated :: Apply f => LensLike' f a a
repeated f a = as where as = f a .> as
{-# INLINE repeated #-}

-- | A 'Fold' that replicates its input @n@ times.
--
-- @
-- 'replicate' n ≡ 'toListOf' ('replicated' n)
-- @
--
-- >>> 5^..replicated 20
-- [5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5]
replicated :: Int -> Fold a a
replicated n0 f a = go n0 where
  m = f a
  go 0 = noEffect
  go n = m *> go (n - 1)
{-# INLINE replicated #-}

-- | Transform a non-empty 'Fold' into a 'Fold1' that loops over its elements over and over.
--
-- >>> timingOut $ [1,2,3]^..taking 7 (cycled traverse)
-- [1,2,3,1,2,3,1]
--
-- @
-- 'cycled' :: 'Fold1' s a -> 'Fold1' s a
-- @
cycled :: Apply f => LensLike f s t a b -> LensLike f s t a b
cycled l f a = as where as = l f a .> as
{-# INLINE cycled #-}

-- | Build a 'Fold' that unfolds its values from a seed.
--
-- @
-- 'Prelude.unfoldr' ≡ 'toListOf' '.' 'unfolded'
-- @
--
-- >>> 10^..unfolded (\b -> if b == 0 then Nothing else Just (b, b-1))
-- [10,9,8,7,6,5,4,3,2,1]
unfolded :: (b -> Maybe (a, b)) -> Fold b a
unfolded f g = go where
  go b = case f b of
    Just (a, b') -> g a *> go b'
    Nothing      -> noEffect
{-# INLINE unfolded #-}

-- | @x '^.' 'iterated' f@ returns an infinite 'Fold1' of repeated applications of @f@ to @x@.
--
-- @
-- 'toListOf' ('iterated' f) a ≡ 'iterate' f a
-- @
--
-- @
-- 'iterated' :: (a -> a) -> 'Fold1' a a
-- @
iterated :: Apply f => (a -> a) -> LensLike' f a a
iterated f g = go where
  go a = g a .> go (f a)
{-# INLINE iterated #-}

-- | Obtain a 'Fold' that can be composed with to filter another 'Lens', 'Iso', 'Getter', 'Fold' (or 'Traversal').
--
-- Note: This is /not/ a legal 'Traversal', unless you are very careful not to invalidate the predicate on the target.
--
-- Note: This is also /not/ a legal 'Prism', unless you are very careful not to inject a value that fails the predicate.
--
-- As a counter example, consider that given @evens = 'filtered' 'even'@ the second 'Traversal' law is violated:
--
-- @
-- 'Control.Lens.Setter.over' evens 'succ' '.' 'Control.Lens.Setter.over' evens 'succ' '/=' 'Control.Lens.Setter.over' evens ('succ' '.' 'succ')
-- @
--
-- So, in order for this to qualify as a legal 'Traversal' you can only use it for actions that preserve the result of the predicate!
--
-- >>> [1..10]^..folded.filtered even
-- [2,4,6,8,10]
--
-- This will preserve an index if it is present.
filtered :: (Choice p, Applicative f) => (a -> Bool) -> Optic' p f a a
filtered p = dimap (\x -> if p x then Right x else Left x) (either pure id) . right'
{-# INLINE filtered #-}

-- | Obtain a potentially empty 'IndexedTraversal' by taking the first element from another,
-- potentially empty `Fold` and using it as an index.
--
-- The resulting optic can be composed with to filter another 'Lens', 'Iso', 'Getter', 'Fold' (or 'Traversal').
--
-- >>> [(Just 2, 3), (Nothing, 4)] & mapped . filteredBy (_1 . _Just) <. _2 %@~ (*) :: [(Maybe Int, Int)]
-- [(Just 2,6),(Nothing,4)]
--
-- @
-- 'filteredBy' :: 'Fold' a i -> 'IndexedTraversal'' i a a
-- @
--
-- Note: As with 'filtered', this is /not/ a legal 'IndexedTraversal', unless you are very careful not to invalidate the predicate on the target!
filteredBy :: (Indexable i p, Applicative f) => Getting (First i) a i -> p a (f a) -> a -> f a
filteredBy p f val = case val ^? p of
  Nothing -> pure val
  Just witness -> indexed f witness val

-- | Obtain a 'Fold' by taking elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- @
-- 'takeWhile' p ≡ 'toListOf' ('takingWhile' p 'folded')
-- @
--
-- >>> timingOut $ toListOf (takingWhile (<=3) folded) [1..]
-- [1,2,3]
--
-- @
-- 'takingWhile' :: (a -> 'Bool') -> 'Fold' s a                         -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Getter' s a                       -> 'Fold' s a
-- 'takingWhile' :: (a -> 'Bool') -> 'Traversal'' s a                   -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Lens'' s a                        -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Prism'' s a                       -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'Iso'' s a                         -> 'Fold' s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a          -> 'IndexedFold' i s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a               -> 'IndexedFold' i s a -- * See note below
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- 'takingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- @
--
-- /Note:/ When applied to a 'Traversal', 'takingWhile' yields something that can be used as if it were a 'Traversal', but
-- which is not a 'Traversal' per the laws, unless you are careful to ensure that you do not invalidate the predicate when
-- writing back through it.
takingWhile :: (Conjoined p, Applicative f) => (a -> Bool) -> Over p (TakingWhile p f a a) s t a a -> Over p f s t a a
takingWhile p l pafb = fmap runMagma . traverse (cosieve pafb) . runTakingWhile . l flag where
  flag = cotabulate $ \wa -> let a = extract wa; r = p a in TakingWhile r a $ \pr ->
    if pr && r then Magma () wa else MagmaPure a
{-# INLINE takingWhile #-}

-- | Obtain a 'Fold' by dropping elements from another 'Fold', 'Lens', 'Iso', 'Getter' or 'Traversal' while a predicate holds.
--
-- @
-- 'dropWhile' p ≡ 'toListOf' ('droppingWhile' p 'folded')
-- @
--
-- >>> toListOf (droppingWhile (<=3) folded) [1..6]
-- [4,5,6]
--
-- >>> toListOf (droppingWhile (<=3) folded) [1,6,1]
-- [6,1]
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'Fold' s a                         -> 'Fold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'Getter' s a                       -> 'Fold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'Traversal'' s a                   -> 'Fold' s a                -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Lens'' s a                        -> 'Fold' s a                -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Prism'' s a                       -> 'Fold' s a                -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'Iso'' s a                         -> 'Fold' s a                -- see notes
-- @
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingTraversal'' s a    -> 'IndexPreservingFold' s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingLens'' s a         -> 'IndexPreservingFold' s a -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingGetter' s a        -> 'IndexPreservingFold' s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexPreservingFold' s a          -> 'IndexPreservingFold' s a
-- @
--
-- @
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedTraversal'' i s a          -> 'IndexedFold' i s a       -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedLens'' i s a               -> 'IndexedFold' i s a       -- see notes
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedGetter' i s a              -> 'IndexedFold' i s a
-- 'droppingWhile' :: (a -> 'Bool') -> 'IndexedFold' i s a                -> 'IndexedFold' i s a
-- @
--
-- Note: Many uses of this combinator will yield something that meets the types, but not the laws of a valid
-- 'Traversal' or 'IndexedTraversal'. The 'Traversal' and 'IndexedTraversal' laws are only satisfied if the
-- new values you assign to the first target also does not pass the predicate! Otherwise subsequent traversals
-- will visit fewer elements and 'Traversal' fusion is not sound.
--
-- So for any traversal @t@ and predicate @p@, @`droppingWhile` p t@ may not be lawful, but
-- @(`Control.Lens.Traversal.dropping` 1 . `droppingWhile` p) t@ is. For example:
--
-- >>> let l  :: Traversal' [Int] Int; l  = droppingWhile (<= 1) traverse
-- >>> let l' :: Traversal' [Int] Int; l' = dropping 1 l
--
-- @l@ is not a lawful setter because @`Control.Lens.Setter.over` l f .
-- `Control.Lens.Setter.over` l g ≢ `Control.Lens.Setter.over` l (f . g)@:
--
-- >>> [1,2,3] & l .~ 0 & l .~ 4
-- [1,0,0]
-- >>> [1,2,3] & l .~ 4
-- [1,4,4]
--
-- @l'@ on the other hand behaves lawfully:
--
-- >>> [1,2,3] & l' .~ 0 & l' .~ 4
-- [1,2,4]
-- >>> [1,2,3] & l' .~ 4
-- [1,2,4]
droppingWhile :: (Conjoined p, Profunctor q, Applicative f)
              => (a -> Bool)
              -> Optical p q (Compose (State Bool) f) s t a a
              -> Optical p q f s t a a
droppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = cotabulate $ \wa -> Compose $ state $ \b -> let
      a = extract wa
      b' = b && p a
    in (if b' then pure a else cosieve f wa, b')
{-# INLINE droppingWhile #-}

-- | A 'Fold' over the individual 'words' of a 'String'.
--
-- @
-- 'worded' :: 'Fold' 'String' 'String'
-- 'worded' :: 'Traversal'' 'String' 'String'
-- @
--
-- @
-- 'worded' :: 'IndexedFold' 'Int' 'String' 'String'
-- 'worded' :: 'IndexedTraversal'' 'Int' 'String' 'String'
-- @
--
-- Note: This function type-checks as a 'Traversal' but it doesn't satisfy the laws. It's only valid to use it
-- when you don't insert any whitespace characters while traversing, and if your original 'String' contains only
-- isolated space characters (and no other characters that count as space, such as non-breaking spaces).
worded :: Applicative f => IndexedLensLike' Int f String String
worded f = fmap unwords . conjoined traverse (indexing traverse) f . words
{-# INLINE worded #-}

-- | A 'Fold' over the individual 'lines' of a 'String'.
--
-- @
-- 'lined' :: 'Fold' 'String' 'String'
-- 'lined' :: 'Traversal'' 'String' 'String'
-- @
--
-- @
-- 'lined' :: 'IndexedFold' 'Int' 'String' 'String'
-- 'lined' :: 'IndexedTraversal'' 'Int' 'String' 'String'
-- @
--
-- Note: This function type-checks as a 'Traversal' but it doesn't satisfy the laws. It's only valid to use it
-- when you don't insert any newline characters while traversing, and if your original 'String' contains only
-- isolated newline characters.
lined :: Applicative f => IndexedLensLike' Int f String String
lined f = fmap (intercalate "\n") . conjoined traverse (indexing traverse) f . lines
{-# INLINE lined #-}

--------------------------
-- Fold/Getter combinators
--------------------------

-- | Map each part of a structure viewed through a 'Lens', 'Getter',
-- 'Fold' or 'Traversal' to a monoid and combine the results.
--
-- >>> foldMapOf (folded . both . _Just) Sum [(Just 21, Just 21)]
-- Sum {getSum = 42}
--
-- @
-- 'Data.Foldable.foldMap' = 'foldMapOf' 'folded'
-- @
--
-- @
-- 'foldMapOf' ≡ 'views'
-- 'ifoldMapOf' l = 'foldMapOf' l '.' 'Indexed'
-- @
--
-- @
-- 'foldMapOf' ::                'Getter' s a      -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r    => 'Fold' s a        -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Semigroup' r => 'Fold1' s a       -> (a -> r) -> s -> r
-- 'foldMapOf' ::                'Lens'' s a       -> (a -> r) -> s -> r
-- 'foldMapOf' ::                'Iso'' s a        -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r    => 'Traversal'' s a  -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Semigroup' r => 'Traversal1'' s a -> (a -> r) -> s -> r
-- 'foldMapOf' :: 'Monoid' r    => 'Prism'' s a      -> (a -> r) -> s -> r
-- @
--
-- @
-- 'foldMapOf' :: 'Getting' r s a -> (a -> r) -> s -> r
-- @
foldMapOf :: Getting r s a -> (a -> r) -> s -> r
foldMapOf = coerce
{-# INLINE foldMapOf #-}

-- | Combine the elements of a structure viewed through a 'Lens', 'Getter',
-- 'Fold' or 'Traversal' using a monoid.
--
-- >>> foldOf (folded.folded) [[Sum 1,Sum 4],[Sum 8, Sum 8],[Sum 21]]
-- Sum {getSum = 42}
--
-- @
-- 'Data.Foldable.fold' = 'foldOf' 'folded'
-- @
--
-- @
-- 'foldOf' ≡ 'view'
-- @
--
-- @
-- 'foldOf' ::             'Getter' s m     -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Fold' s m       -> s -> m
-- 'foldOf' ::             'Lens'' s m      -> s -> m
-- 'foldOf' ::             'Iso'' s m       -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Traversal'' s m -> s -> m
-- 'foldOf' :: 'Monoid' m => 'Prism'' s m     -> s -> m
-- @
foldOf :: Getting a s a -> s -> a
foldOf l = getConst #. l Const
{-# INLINE foldOf #-}

-- | Right-associative fold of parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- @
-- 'Data.Foldable.foldr' ≡ 'foldrOf' 'folded'
-- @
--
-- @
-- 'foldrOf' :: 'Getter' s a     -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Fold' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Lens'' s a      -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Iso'' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Traversal'' s a -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf' :: 'Prism'' s a     -> (a -> r -> r) -> r -> s -> r
-- @
--
-- @
-- 'ifoldrOf' l ≡ 'foldrOf' l '.' 'Indexed'
-- @
--
-- @
-- 'foldrOf' :: 'Getting' ('Endo' r) s a -> (a -> r -> r) -> r -> s -> r
-- @
foldrOf :: Getting (Endo r) s a -> (a -> r -> r) -> r -> s -> r
foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
{-# INLINE foldrOf #-}

-- | Left-associative fold of the parts of a structure that are viewed through a 'Lens', 'Getter', 'Fold' or 'Traversal'.
--
-- @
-- 'Data.Foldable.foldl' ≡ 'foldlOf' 'folded'
-- @
--
-- @
-- 'foldlOf' :: 'Getter' s a     -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Fold' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Lens'' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Iso'' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Traversal'' s a -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf' :: 'Prism'' s a     -> (r -> a -> r) -> r -> s -> r
-- @
foldlOf :: Getting (Dual (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
foldlOf l f z = (flip appEndo z .# getDual) `rmap` foldMapOf l (Dual #. Endo #. flip f)
{-# INLINE foldlOf #-}

-- | Extract a list of the targets of a 'Fold'. See also ('^..').
--
-- @
-- 'Data.Foldable.toList' ≡ 'toListOf' 'folded'
-- ('^..') ≡ 'flip' 'toListOf'
-- @

-- >>> toListOf both ("hello","world")
-- ["hello","world"]
--
-- @
-- 'toListOf' :: 'Getter' s a     -> s -> [a]
-- 'toListOf' :: 'Fold' s a       -> s -> [a]
-- 'toListOf' :: 'Lens'' s a      -> s -> [a]
-- 'toListOf' :: 'Iso'' s a       -> s -> [a]
-- 'toListOf' :: 'Traversal'' s a -> s -> [a]
-- 'toListOf' :: 'Prism'' s a     -> s -> [a]
-- @
toListOf :: Getting (Endo [a]) s a -> s -> [a]
toListOf l = foldrOf l (:) []
{-# INLINE toListOf #-}

-- | Extract a 'NonEmpty' of the targets of 'Fold1'.
--
-- >>> toNonEmptyOf both1 ("hello", "world")
-- "hello" :| ["world"]
--
-- @
-- 'toNonEmptyOf' :: 'Getter' s a      -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Fold1' s a       -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Lens'' s a       -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Iso'' s a        -> s -> NonEmpty a
-- 'toNonEmptyOf' :: 'Traversal1'' s a -> s -> NonEmpty a
-- @
toNonEmptyOf :: Getting (NonEmptyDList a) s a -> s -> NonEmpty a
toNonEmptyOf l = flip getNonEmptyDList [] . foldMapOf l (NonEmptyDList #. (:|))

-- | Calls 'pure' on the target of a 'Lens', 'Getter', or 'Iso'.
--
-- Calls 'pure' on the targets of a 'Traversal', 'Fold', or 'Prism', and
-- combines them with '<|>' (or `empty` if none).  Intuitively, it collects
-- targets into an 'Alternative' until the container fills up or it runs out of
-- targets, whichever comes first.
--
-- Generalizes 'toListOf' and '(^?)'.
--
-- >>> altOf both ("hello", "world") :: [String]
-- ["hello","world"]
-- >>> altOf both ("hello", "world") :: Maybe String
-- Just "hello"
--
-- @
-- 'altOf' :: Applicative f => 'Lens'' s a      -> s -> f a
-- 'altOf' :: Applicative f => 'Getter' s a     -> s -> f a
-- 'altOf' :: Applicative f => 'Iso'' s a       -> s -> f a
--
-- 'altOf' :: Alternative f => 'Traversal'' s a -> s -> f a
-- 'altOf' :: Alternative f => 'Fold' s a       -> s -> f a
-- 'altOf' :: Alternative f => 'Prism'' s a     -> s -> f a
-- @
altOf :: Applicative f => Getting (Alt f a) s a -> s -> f a
altOf l = getAlt #. views l (Alt #. pure)
{-# INLINE altOf #-}

-- | A convenient infix (flipped) version of 'toListOf'.
--
-- >>> [[1,2],[3]]^..id
-- [[[1,2],[3]]]
-- >>> [[1,2],[3]]^..traverse
-- [[1,2],[3]]
-- >>> [[1,2],[3]]^..traverse.traverse
-- [1,2,3]
--
-- >>> (1,2)^..both
-- [1,2]
--
-- @
-- 'Data.Foldable.toList' xs ≡ xs '^..' 'folded'
-- ('^..') ≡ 'flip' 'toListOf'
-- @
--
-- @
-- ('^..') :: s -> 'Getter' s a     -> [a]
-- ('^..') :: s -> 'Fold' s a       -> [a]
-- ('^..') :: s -> 'Lens'' s a      -> [a]
-- ('^..') :: s -> 'Iso'' s a       -> [a]
-- ('^..') :: s -> 'Traversal'' s a -> [a]
-- ('^..') :: s -> 'Prism'' s a     -> [a]
-- @
(^..) :: s -> Getting (Endo [a]) s a -> [a]
s ^.. l = toListOf l s
{-# INLINE (^..) #-}

-- | Returns 'True' if every target of a 'Fold' is 'True'.
--
-- >>> andOf both (True,False)
-- False
-- >>> andOf both (True,True)
-- True
--
-- @
-- 'Data.Foldable.and' ≡ 'andOf' 'folded'
-- @
--
-- @
-- 'andOf' :: 'Getter' s 'Bool'     -> s -> 'Bool'
-- 'andOf' :: 'Fold' s 'Bool'       -> s -> 'Bool'
-- 'andOf' :: 'Lens'' s 'Bool'      -> s -> 'Bool'
-- 'andOf' :: 'Iso'' s 'Bool'       -> s -> 'Bool'
-- 'andOf' :: 'Traversal'' s 'Bool' -> s -> 'Bool'
-- 'andOf' :: 'Prism'' s 'Bool'     -> s -> 'Bool'
-- @
andOf :: Getting All s Bool -> s -> Bool
andOf l = getAll #. foldMapOf l All
{-# INLINE andOf #-}

-- | Returns 'True' if any target of a 'Fold' is 'True'.
--
-- >>> orOf both (True,False)
-- True
-- >>> orOf both (False,False)
-- False
--
-- @
-- 'Data.Foldable.or' ≡ 'orOf' 'folded'
-- @
--
-- @
-- 'orOf' :: 'Getter' s 'Bool'     -> s -> 'Bool'
-- 'orOf' :: 'Fold' s 'Bool'       -> s -> 'Bool'
-- 'orOf' :: 'Lens'' s 'Bool'      -> s -> 'Bool'
-- 'orOf' :: 'Iso'' s 'Bool'       -> s -> 'Bool'
-- 'orOf' :: 'Traversal'' s 'Bool' -> s -> 'Bool'
-- 'orOf' :: 'Prism'' s 'Bool'     -> s -> 'Bool'
-- @
orOf :: Getting Any s Bool -> s -> Bool
orOf l = getAny #. foldMapOf l Any
{-# INLINE orOf #-}

-- | Returns 'True' if any target of a 'Fold' satisfies a predicate.
--
-- >>> anyOf both (=='x') ('x','y')
-- True
-- >>> import Data.Data.Lens
-- >>> anyOf biplate (== "world") (((),2::Int),"hello",("world",11::Int))
-- True
--
-- @
-- 'Data.Foldable.any' ≡ 'anyOf' 'folded'
-- @
--
-- @
-- 'ianyOf' l ≡ 'anyOf' l '.' 'Indexed'
-- @
--
-- @
-- 'anyOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'anyOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
anyOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
anyOf l f = getAny #. foldMapOf l (Any #. f)
{-# INLINE anyOf #-}

-- | Returns 'True' if every target of a 'Fold' satisfies a predicate.
--
-- >>> allOf both (>=3) (4,5)
-- True
-- >>> allOf folded (>=2) [1..10]
-- False
--
-- @
-- 'Data.Foldable.all' ≡ 'allOf' 'folded'
-- @
--
-- @
-- 'iallOf' l = 'allOf' l '.' 'Indexed'
-- @
--
-- @
-- 'allOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'allOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
allOf :: Getting All s a -> (a -> Bool) -> s -> Bool
allOf l f = getAll #. foldMapOf l (All #. f)
{-# INLINE allOf #-}

-- | Returns 'True' only if no targets of a 'Fold' satisfy a predicate.
--
-- >>> noneOf each (is _Nothing) (Just 3, Just 4, Just 5)
-- True
-- >>> noneOf (folded.folded) (<10) [[13,99,20],[3,71,42]]
-- False
--
-- @
-- 'inoneOf' l = 'noneOf' l '.' 'Indexed'
-- @
--
-- @
-- 'noneOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Bool'
-- 'noneOf' :: 'Prism'' s a     -> (a -> 'Bool') -> s -> 'Bool'
-- @
noneOf :: Getting Any s a -> (a -> Bool) -> s -> Bool
noneOf l f = not . anyOf l f
{-# INLINE noneOf #-}

-- | Calculate the 'Product' of every number targeted by a 'Fold'.
--
-- >>> productOf both (4,5)
-- 20
-- >>> productOf folded [1,2,3,4,5]
-- 120
--
-- @
-- 'Data.Foldable.product' ≡ 'productOf' 'folded'
-- @
--
-- This operation may be more strict than you would expect. If you
-- want a lazier version use @'ala' 'Product' '.' 'foldMapOf'@
--
-- @
-- 'productOf' :: 'Num' a => 'Getter' s a     -> s -> a
-- 'productOf' :: 'Num' a => 'Fold' s a       -> s -> a
-- 'productOf' :: 'Num' a => 'Lens'' s a      -> s -> a
-- 'productOf' :: 'Num' a => 'Iso'' s a       -> s -> a
-- 'productOf' :: 'Num' a => 'Traversal'' s a -> s -> a
-- 'productOf' :: 'Num' a => 'Prism'' s a     -> s -> a
-- @
productOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a
productOf l = foldlOf' l (*) 1
{-# INLINE productOf #-}

-- | Calculate the 'Sum' of every number targeted by a 'Fold'.
--
-- >>> sumOf both (5,6)
-- 11
-- >>> sumOf folded [1,2,3,4]
-- 10
-- >>> sumOf (folded.both) [(1,2),(3,4)]
-- 10
-- >>> import Data.Data.Lens
-- >>> sumOf biplate [(1::Int,[]),(2,[(3::Int,4::Int)])] :: Int
-- 10
--
-- @
-- 'Data.Foldable.sum' ≡ 'sumOf' 'folded'
-- @
--
-- This operation may be more strict than you would expect. If you
-- want a lazier version use @'ala' 'Sum' '.' 'foldMapOf'@
--
-- @
-- 'sumOf' '_1' :: 'Num' a => (a, b) -> a
-- 'sumOf' ('folded' '.' 'Control.Lens.Tuple._1') :: ('Foldable' f, 'Num' a) => f (a, b) -> a
-- @
--
-- @
-- 'sumOf' :: 'Num' a => 'Getter' s a     -> s -> a
-- 'sumOf' :: 'Num' a => 'Fold' s a       -> s -> a
-- 'sumOf' :: 'Num' a => 'Lens'' s a      -> s -> a
-- 'sumOf' :: 'Num' a => 'Iso'' s a       -> s -> a
-- 'sumOf' :: 'Num' a => 'Traversal'' s a -> s -> a
-- 'sumOf' :: 'Num' a => 'Prism'' s a     -> s -> a
-- @
sumOf :: Num a => Getting (Endo (Endo a)) s a -> s -> a
sumOf l = foldlOf' l (+) 0
{-# INLINE sumOf #-}

-- | Traverse over all of the targets of a 'Fold' (or 'Getter'), computing an 'Applicative' (or 'Functor')-based answer,
-- but unlike 'Control.Lens.Traversal.traverseOf' do not construct a new structure. 'traverseOf_' generalizes
-- 'Data.Foldable.traverse_' to work over any 'Fold'.
--
-- When passed a 'Getter', 'traverseOf_' can work over any 'Functor', but when passed a 'Fold', 'traverseOf_' requires
-- an 'Applicative'.
--
-- >>> traverseOf_ both putStrLn ("hello","world")
-- hello
-- world
--
-- @
-- 'Data.Foldable.traverse_' ≡ 'traverseOf_' 'folded'
-- @
--
-- @
-- 'traverseOf_' '_2' :: 'Functor' f => (c -> f r) -> (d, c) -> f ()
-- 'traverseOf_' 'Control.Lens.Prism._Left' :: 'Applicative' f => (a -> f b) -> 'Either' a c -> f ()
-- @
--
-- @
-- 'itraverseOf_' l ≡ 'traverseOf_' l '.' 'Indexed'
-- @
--
-- The rather specific signature of 'traverseOf_' allows it to be used as if the signature was any of:
--
-- @
-- 'traverseOf_' :: 'Functor' f     => 'Getter' s a     -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Fold' s a       -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Lens'' s a      -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Functor' f     => 'Iso'' s a       -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Traversal'' s a -> (a -> f r) -> s -> f ()
-- 'traverseOf_' :: 'Applicative' f => 'Prism'' s a     -> (a -> f r) -> s -> f ()
-- @
traverseOf_ :: Functor f => Getting (Traversed r f) s a -> (a -> f r) -> s -> f ()
traverseOf_ l f = void . getTraversed #. foldMapOf l (Traversed #. f)
{-# INLINE traverseOf_ #-}

-- | Traverse over all of the targets of a 'Fold' (or 'Getter'), computing an 'Applicative' (or 'Functor')-based answer,
-- but unlike 'Control.Lens.Traversal.forOf' do not construct a new structure. 'forOf_' generalizes
-- 'Data.Foldable.for_' to work over any 'Fold'.
--
-- When passed a 'Getter', 'forOf_' can work over any 'Functor', but when passed a 'Fold', 'forOf_' requires
-- an 'Applicative'.
--
-- @
-- 'for_' ≡ 'forOf_' 'folded'
-- @
--
-- >>> forOf_ both ("hello","world") putStrLn
-- hello
-- world
--
-- The rather specific signature of 'forOf_' allows it to be used as if the signature was any of:
--
-- @
-- 'iforOf_' l s ≡ 'forOf_' l s '.' 'Indexed'
-- @
--
-- @
-- 'forOf_' :: 'Functor' f     => 'Getter' s a     -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Fold' s a       -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Lens'' s a      -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Functor' f     => 'Iso'' s a       -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Traversal'' s a -> s -> (a -> f r) -> f ()
-- 'forOf_' :: 'Applicative' f => 'Prism'' s a     -> s -> (a -> f r) -> f ()
-- @
forOf_ :: Functor f => Getting (Traversed r f) s a -> s -> (a -> f r) -> f ()
forOf_ = flip . traverseOf_
{-# INLINE forOf_ #-}

-- | Evaluate each action in observed by a 'Fold' on a structure from left to right, ignoring the results.
--
-- @
-- 'sequenceA_' ≡ 'sequenceAOf_' 'folded'
-- @
--
-- >>> sequenceAOf_ both (putStrLn "hello",putStrLn "world")
-- hello
-- world
--
-- @
-- 'sequenceAOf_' :: 'Functor' f     => 'Getter' s (f a)     -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Fold' s (f a)       -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Lens'' s (f a)      -> s -> f ()
-- 'sequenceAOf_' :: 'Functor' f     => 'Iso'' s (f a)       -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Traversal'' s (f a) -> s -> f ()
-- 'sequenceAOf_' :: 'Applicative' f => 'Prism'' s (f a)     -> s -> f ()
-- @
sequenceAOf_ :: Functor f => Getting (Traversed a f) s (f a) -> s -> f ()
sequenceAOf_ l = void . getTraversed #. foldMapOf l Traversed
{-# INLINE sequenceAOf_ #-}

-- | Traverse over all of the targets of a 'Fold1', computing an 'Apply' based answer.
--
-- As long as you have 'Applicative' or 'Functor' effect you are better using 'traverseOf_'.
-- The 'traverse1Of_' is useful only when you have genuine 'Apply' effect.
--
-- >>> traverse1Of_ both1 (\ks -> Map.fromList [ (k, ()) | k <- ks ]) ("abc", "bcd")
-- fromList [('b',()),('c',())]
--
-- @
-- 'traverse1Of_' :: 'Apply' f => 'Fold1' s a -> (a -> f r) -> s -> f ()
-- @
--
-- @since 4.16
traverse1Of_ :: Functor f => Getting (TraversedF r f) s a -> (a -> f r) -> s -> f ()
traverse1Of_ l f = void . getTraversedF #. foldMapOf l (TraversedF #. f)
{-# INLINE traverse1Of_ #-}

-- | See 'forOf_' and 'traverse1Of_'.
--
-- >>> for1Of_ both1 ("abc", "bcd") (\ks -> Map.fromList [ (k, ()) | k <- ks ])
-- fromList [('b',()),('c',())]
--
-- @
-- 'for1Of_' :: 'Apply' f => 'Fold1' s a -> s -> (a -> f r) -> f ()
-- @
--
-- @since 4.16
for1Of_ :: Functor f => Getting (TraversedF r f) s a -> s -> (a -> f r) -> f ()
for1Of_ = flip . traverse1Of_
{-# INLINE for1Of_ #-}

-- | See 'sequenceAOf_' and 'traverse1Of_'.
--
-- @
-- 'sequence1Of_' :: 'Apply' f => 'Fold1' s (f a) -> s -> f ()
-- @
--
-- @since 4.16
sequence1Of_ :: Functor f => Getting (TraversedF a f) s (f a) -> s -> f ()
sequence1Of_ l = void . getTraversedF #. foldMapOf l TraversedF
{-# INLINE sequence1Of_ #-}

-- | Map each target of a 'Fold' on a structure to a monadic action, evaluate these actions from left to right, and ignore the results.
--
-- >>> mapMOf_ both putStrLn ("hello","world")
-- hello
-- world
--
-- @
-- 'Data.Foldable.mapM_' ≡ 'mapMOf_' 'folded'
-- @
--
-- @
-- 'mapMOf_' :: 'Monad' m => 'Getter' s a     -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Fold' s a       -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Lens'' s a      -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Iso'' s a       -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Traversal'' s a -> (a -> m r) -> s -> m ()
-- 'mapMOf_' :: 'Monad' m => 'Prism'' s a     -> (a -> m r) -> s -> m ()
-- @
mapMOf_ :: Monad m => Getting (Sequenced r m) s a -> (a -> m r) -> s -> m ()
mapMOf_ l f = liftM skip . getSequenced #. foldMapOf l (Sequenced #. f)
{-# INLINE mapMOf_ #-}

-- | 'forMOf_' is 'mapMOf_' with two of its arguments flipped.
--
-- >>> forMOf_ both ("hello","world") putStrLn
-- hello
-- world
--
-- @
-- 'Data.Foldable.forM_' ≡ 'forMOf_' 'folded'
-- @
--
-- @
-- 'forMOf_' :: 'Monad' m => 'Getter' s a     -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Fold' s a       -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Lens'' s a      -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Iso'' s a       -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Traversal'' s a -> s -> (a -> m r) -> m ()
-- 'forMOf_' :: 'Monad' m => 'Prism'' s a     -> s -> (a -> m r) -> m ()
-- @
forMOf_ :: Monad m => Getting (Sequenced r m) s a -> s -> (a -> m r) -> m ()
forMOf_ = flip . mapMOf_
{-# INLINE forMOf_ #-}

-- | Evaluate each monadic action referenced by a 'Fold' on the structure from left to right, and ignore the results.
--
-- >>> sequenceOf_ both (putStrLn "hello",putStrLn "world")
-- hello
-- world
--
-- @
-- 'Data.Foldable.sequence_' ≡ 'sequenceOf_' 'folded'
-- @
--
-- @
-- 'sequenceOf_' :: 'Monad' m => 'Getter' s (m a)     -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Fold' s (m a)       -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Lens'' s (m a)      -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Iso'' s (m a)       -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Traversal'' s (m a) -> s -> m ()
-- 'sequenceOf_' :: 'Monad' m => 'Prism'' s (m a)     -> s -> m ()
-- @
sequenceOf_ :: Monad m => Getting (Sequenced a m) s (m a) -> s -> m ()
sequenceOf_ l = liftM skip . getSequenced #. foldMapOf l Sequenced
{-# INLINE sequenceOf_ #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- >>> asumOf both ("hello","world")
-- "helloworld"
--
-- >>> asumOf each (Nothing, Just "hello", Nothing)
-- Just "hello"
--
-- @
-- 'asum' ≡ 'asumOf' 'folded'
-- @
--
-- @
-- 'asumOf' :: 'Alternative' f => 'Getter' s (f a)     -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Fold' s (f a)       -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Lens'' s (f a)      -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Iso'' s (f a)       -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Traversal'' s (f a) -> s -> f a
-- 'asumOf' :: 'Alternative' f => 'Prism'' s (f a)     -> s -> f a
-- @
asumOf :: Alternative f => Getting (Endo (f a)) s (f a) -> s -> f a
asumOf l = foldrOf l (<|>) empty
{-# INLINE asumOf #-}

-- | The sum of a collection of actions, generalizing 'concatOf'.
--
-- >>> msumOf both ("hello","world")
-- "helloworld"
--
-- >>> msumOf each (Nothing, Just "hello", Nothing)
-- Just "hello"
--
-- @
-- 'msum' ≡ 'msumOf' 'folded'
-- @
--
-- @
-- 'msumOf' :: 'MonadPlus' m => 'Getter' s (m a)     -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Fold' s (m a)       -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Lens'' s (m a)      -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Iso'' s (m a)       -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Traversal'' s (m a) -> s -> m a
-- 'msumOf' :: 'MonadPlus' m => 'Prism'' s (m a)     -> s -> m a
-- @
msumOf :: MonadPlus m => Getting (Endo (m a)) s (m a) -> s -> m a
msumOf l = foldrOf l mplus mzero
{-# INLINE msumOf #-}

-- | Does the element occur anywhere within a given 'Fold' of the structure?
--
-- >>> elemOf both "hello" ("hello","world")
-- True
--
-- @
-- 'elem' ≡ 'elemOf' 'folded'
-- @
--
-- @
-- 'elemOf' :: 'Eq' a => 'Getter' s a     -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Fold' s a       -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Lens'' s a      -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Iso'' s a       -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Traversal'' s a -> a -> s -> 'Bool'
-- 'elemOf' :: 'Eq' a => 'Prism'' s a     -> a -> s -> 'Bool'
-- @
elemOf :: Eq a => Getting Any s a -> a -> s -> Bool
elemOf l = anyOf l . (==)
{-# INLINE elemOf #-}

-- | Does the element not occur anywhere within a given 'Fold' of the structure?
--
-- >>> notElemOf each 'd' ('a','b','c')
-- True
--
-- >>> notElemOf each 'a' ('a','b','c')
-- False
--
-- @
-- 'notElem' ≡ 'notElemOf' 'folded'
-- @
--
-- @
-- 'notElemOf' :: 'Eq' a => 'Getter' s a     -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Fold' s a       -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Iso'' s a       -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Lens'' s a      -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Traversal'' s a -> a -> s -> 'Bool'
-- 'notElemOf' :: 'Eq' a => 'Prism'' s a     -> a -> s -> 'Bool'
-- @
notElemOf :: Eq a => Getting All s a -> a -> s -> Bool
notElemOf l = allOf l . (/=)
{-# INLINE notElemOf #-}

-- | Map a function over all the targets of a 'Fold' of a container and concatenate the resulting lists.
--
-- >>> concatMapOf both (\x -> [x, x + 1]) (1,3)
-- [1,2,3,4]
--
-- @
-- 'concatMap' ≡ 'concatMapOf' 'folded'
-- @
--
-- @
-- 'concatMapOf' :: 'Getter' s a     -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Fold' s a       -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Lens'' s a      -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Iso'' s a       -> (a -> [r]) -> s -> [r]
-- 'concatMapOf' :: 'Traversal'' s a -> (a -> [r]) -> s -> [r]
-- @
concatMapOf :: Getting [r] s a -> (a -> [r]) -> s -> [r]
concatMapOf = coerce
{-# INLINE concatMapOf #-}

-- | Concatenate all of the lists targeted by a 'Fold' into a longer list.
--
-- >>> concatOf both ("pan","ama")
-- "panama"
--
-- @
-- 'concat' ≡ 'concatOf' 'folded'
-- 'concatOf' ≡ 'view'
-- @
--
-- @
-- 'concatOf' :: 'Getter' s [r]     -> s -> [r]
-- 'concatOf' :: 'Fold' s [r]       -> s -> [r]
-- 'concatOf' :: 'Iso'' s [r]       -> s -> [r]
-- 'concatOf' :: 'Lens'' s [r]      -> s -> [r]
-- 'concatOf' :: 'Traversal'' s [r] -> s -> [r]
-- @
concatOf :: Getting [r] s [r] -> s -> [r]
concatOf l = getConst #. l Const
{-# INLINE concatOf #-}


-- | Calculate the number of targets there are for a 'Fold' in a given container.
--
-- /Note:/ This can be rather inefficient for large containers and just like 'length',
-- this will not terminate for infinite folds.
--
-- @
-- 'length' ≡ 'lengthOf' 'folded'
-- @
--
-- >>> lengthOf _1 ("hello",())
-- 1
--
-- >>> lengthOf traverse [1..10]
-- 10
--
-- >>> lengthOf (traverse.traverse) [[1,2],[3,4],[5,6]]
-- 6
--
-- @
-- 'lengthOf' ('folded' '.' 'folded') :: ('Foldable' f, 'Foldable' g) => f (g a) -> 'Int'
-- @
--
-- @
-- 'lengthOf' :: 'Getter' s a     -> s -> 'Int'
-- 'lengthOf' :: 'Fold' s a       -> s -> 'Int'
-- 'lengthOf' :: 'Lens'' s a      -> s -> 'Int'
-- 'lengthOf' :: 'Iso'' s a       -> s -> 'Int'
-- 'lengthOf' :: 'Traversal'' s a -> s -> 'Int'
-- @
lengthOf :: Getting (Endo (Endo Int)) s a -> s -> Int
lengthOf l = foldlOf' l (\a _ -> a + 1) 0
{-# INLINE lengthOf #-}

-- | Perform a safe 'head' of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- When using a 'Traversal' as a partial 'Lens', or a 'Fold' as a partial 'Getter' this can be a convenient
-- way to extract the optional value.
--
-- Note: if you get stack overflows due to this, you may want to use 'firstOf' instead, which can deal
-- more gracefully with heavily left-biased trees. This is because '^?' works by using the
-- 'Data.Monoid.First' monoid, which can occasionally cause space leaks.
--
-- >>> Left 4 ^?_Left
-- Just 4
--
-- >>> Right 4 ^?_Left
-- Nothing
--
-- >>> "world" ^? ix 3
-- Just 'l'
--
-- >>> "world" ^? ix 20
-- Nothing
--
-- This operator works as an infix version of 'preview'.
--
-- @
-- ('^?') ≡ 'flip' 'preview'
-- @
--
-- It may be helpful to think of '^?' as having one of the following
-- more specialized types:
--
-- @
-- ('^?') :: s -> 'Getter' s a     -> 'Maybe' a
-- ('^?') :: s -> 'Fold' s a       -> 'Maybe' a
-- ('^?') :: s -> 'Lens'' s a      -> 'Maybe' a
-- ('^?') :: s -> 'Iso'' s a       -> 'Maybe' a
-- ('^?') :: s -> 'Traversal'' s a -> 'Maybe' a
-- @
(^?) :: s -> Getting (First a) s a -> Maybe a
s ^? l = getFirst (foldMapOf l (First #. Just) s)
{-# INLINE (^?) #-}

-- | Perform an *UNSAFE* 'head' of a 'Fold' or 'Traversal' assuming that it is there.
--
-- >>> Left 4 ^?! _Left
-- 4
--
-- >>> "world" ^?! ix 3
-- 'l'
--
-- @
-- ('^?!') :: s -> 'Getter' s a     -> a
-- ('^?!') :: s -> 'Fold' s a       -> a
-- ('^?!') :: s -> 'Lens'' s a      -> a
-- ('^?!') :: s -> 'Iso'' s a       -> a
-- ('^?!') :: s -> 'Traversal'' s a -> a
-- @
(^?!) :: HasCallStack => s -> Getting (Endo a) s a -> a
s ^?! l = foldrOf l const (error "(^?!): empty Fold") s
{-# INLINE (^?!) #-}

-- | Retrieve the 'First' entry of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- The answer is computed in a manner that leaks space less than @'preview'@ or @^?'@
-- and gives you back access to the outermost 'Just' constructor more quickly, but does so
-- in a way that builds an intermediate structure, and thus may have worse
-- constant factors. This also means that it can not be used in any 'Control.Monad.Reader.MonadReader',
-- but must instead have 's' passed as its last argument, unlike 'preview'.
--
-- Note: this could been named `headOf`.
--
-- >>> firstOf traverse [1..10]
-- Just 1
--
-- >>> firstOf both (1,2)
-- Just 1
--
-- >>> firstOf ignored ()
-- Nothing
--
-- @
-- 'firstOf' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'firstOf' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'firstOf' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'firstOf' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'firstOf' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
firstOf :: Getting (Leftmost a) s a -> s -> Maybe a
firstOf l = getLeftmost . foldMapOf l LLeaf
{-# INLINE firstOf #-}

-- | Retrieve the 'Data.Semigroup.First' entry of a 'Fold1' or 'Traversal1' or the result from a 'Getter' or 'Lens'.
--
-- >>> first1Of traverse1 (1 :| [2..10])
-- 1
--
-- >>> first1Of both1 (1,2)
-- 1
--
-- /Note:/ this is different from '^.'.
--
-- >>> first1Of traverse1 ([1,2] :| [[3,4],[5,6]])
-- [1,2]
--
-- >>> ([1,2] :| [[3,4],[5,6]]) ^. traverse1
-- [1,2,3,4,5,6]
--
-- @
-- 'first1Of' :: 'Getter' s a      -> s -> a
-- 'first1Of' :: 'Fold1' s a       -> s -> a
-- 'first1Of' :: 'Lens'' s a       -> s -> a
-- 'first1Of' :: 'Iso'' s a        -> s -> a
-- 'first1Of' :: 'Traversal1'' s a -> s -> a
-- @
first1Of :: Getting (Semi.First a) s a -> s -> a
first1Of l = Semi.getFirst . foldMapOf l Semi.First

-- | Retrieve the 'Last' entry of a 'Fold' or 'Traversal' or retrieve 'Just' the result
-- from a 'Getter' or 'Lens'.
--
-- The answer is computed in a manner that leaks space less than @'ala' 'Last' '.' 'foldMapOf'@
-- and gives you back access to the outermost 'Just' constructor more quickly, but may have worse
-- constant factors.
--
-- >>> lastOf traverse [1..10]
-- Just 10
--
-- >>> lastOf both (1,2)
-- Just 2
--
-- >>> lastOf ignored ()
-- Nothing
--
-- @
-- 'lastOf' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'lastOf' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'lastOf' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'lastOf' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'lastOf' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
lastOf :: Getting (Rightmost a) s a -> s -> Maybe a
lastOf l = getRightmost . foldMapOf l RLeaf
{-# INLINE lastOf #-}

-- | Retrieve the 'Data.Semigroup.Last' entry of a 'Fold1' or 'Traversal1' or retrieve the result
-- from a 'Getter' or 'Lens'.o
--
-- >>> last1Of traverse1 (1 :| [2..10])
-- 10
--
-- >>> last1Of both1 (1,2)
-- 2
--
-- @
-- 'last1Of' :: 'Getter' s a      -> s -> 'Maybe' a
-- 'last1Of' :: 'Fold1' s a       -> s -> 'Maybe' a
-- 'last1Of' :: 'Lens'' s a       -> s -> 'Maybe' a
-- 'last1Of' :: 'Iso'' s a        -> s -> 'Maybe' a
-- 'last1Of' :: 'Traversal1'' s a -> s -> 'Maybe' a
-- @
last1Of :: Getting (Semi.Last a) s a -> s -> a
last1Of l = Semi.getLast . foldMapOf l Semi.Last

-- | Returns 'True' if this 'Fold' or 'Traversal' has no targets in the given container.
--
-- Note: 'nullOf' on a valid 'Iso', 'Lens' or 'Getter' should always return 'False'.
--
-- @
-- 'null' ≡ 'nullOf' 'folded'
-- @
--
-- This may be rather inefficient compared to the 'null' check of many containers.
--
-- >>> nullOf _1 (1,2)
-- False
--
-- >>> nullOf ignored ()
-- True
--
-- >>> nullOf traverse []
-- True
--
-- >>> nullOf (element 20) [1..10]
-- True
--
-- @
-- 'nullOf' ('folded' '.' '_1' '.' 'folded') :: ('Foldable' f, 'Foldable' g) => f (g a, b) -> 'Bool'
-- @
--
-- @
-- 'nullOf' :: 'Getter' s a     -> s -> 'Bool'
-- 'nullOf' :: 'Fold' s a       -> s -> 'Bool'
-- 'nullOf' :: 'Iso'' s a       -> s -> 'Bool'
-- 'nullOf' :: 'Lens'' s a      -> s -> 'Bool'
-- 'nullOf' :: 'Traversal'' s a -> s -> 'Bool'
-- @
nullOf :: Getting All s a -> s -> Bool
nullOf = hasn't
{-# INLINE nullOf #-}

-- | Returns 'True' if this 'Fold' or 'Traversal' has any targets in the given container.
--
-- A more \"conversational\" alias for this combinator is 'has'.
--
-- Note: 'notNullOf' on a valid 'Iso', 'Lens' or 'Getter' should always return 'True'.
--
-- @
-- 'not' '.' 'null' ≡ 'notNullOf' 'folded'
-- @
--
-- This may be rather inefficient compared to the @'not' '.' 'null'@ check of many containers.
--
-- >>> notNullOf _1 (1,2)
-- True
--
-- >>> notNullOf traverse [1..10]
-- True
--
-- >>> notNullOf folded []
-- False
--
-- >>> notNullOf (element 20) [1..10]
-- False
--
-- @
-- 'notNullOf' ('folded' '.' '_1' '.' 'folded') :: ('Foldable' f, 'Foldable' g) => f (g a, b) -> 'Bool'
-- @
--
-- @
-- 'notNullOf' :: 'Getter' s a     -> s -> 'Bool'
-- 'notNullOf' :: 'Fold' s a       -> s -> 'Bool'
-- 'notNullOf' :: 'Iso'' s a       -> s -> 'Bool'
-- 'notNullOf' :: 'Lens'' s a      -> s -> 'Bool'
-- 'notNullOf' :: 'Traversal'' s a -> s -> 'Bool'
-- @
notNullOf :: Getting Any s a -> s -> Bool
notNullOf = has
{-# INLINE notNullOf #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold' or 'Traversal' safely.
--
-- Note: 'maximumOf' on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- >>> maximumOf traverse [1..10]
-- Just 10
--
-- >>> maximumOf traverse []
-- Nothing
--
-- >>> maximumOf (folded.filtered even) [1,4,3,6,7,9,2]
-- Just 6
--
-- @
-- 'maximum' ≡ 'fromMaybe' ('error' \"empty\") '.' 'maximumOf' 'folded'
-- @
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
-- @'rmap' 'getMax' ('foldMapOf' l 'Max')@ has lazier semantics but could leak memory.
--
-- @
-- 'maximumOf' :: 'Ord' a => 'Getter' s a     -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Fold' s a       -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Iso'' s a       -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Lens'' s a      -> s -> 'Maybe' a
-- 'maximumOf' :: 'Ord' a => 'Traversal'' s a -> s -> 'Maybe' a
-- @
maximumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe a
maximumOf l = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! max x y
{-# INLINE maximumOf #-}

-- | Obtain the maximum element targeted by a 'Fold1' or 'Traversal1'.
--
-- >>> maximum1Of traverse1 (1 :| [2..10])
-- 10
--
-- @
-- 'maximum1Of' :: 'Ord' a => 'Getter' s a      -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Fold1' s a       -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Iso'' s a        -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Lens'' s a       -> s -> a
-- 'maximum1Of' :: 'Ord' a => 'Traversal1'' s a -> s -> a
-- @
maximum1Of :: Ord a => Getting (Semi.Max a) s a -> s -> a
maximum1Of l = Semi.getMax . foldMapOf l Semi.Max
{-# INLINE maximum1Of #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold' or 'Traversal' safely.
--
-- Note: 'minimumOf' on a valid 'Iso', 'Lens' or 'Getter' will always return 'Just' a value.
--
-- >>> minimumOf traverse [1..10]
-- Just 1
--
-- >>> minimumOf traverse []
-- Nothing
--
-- >>> minimumOf (folded.filtered even) [1,4,3,6,7,9,2]
-- Just 2
--
-- @
-- 'minimum' ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumOf' 'folded'
-- @
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
-- @'rmap' 'getMin' ('foldMapOf' l 'Min')@ has lazier semantics but could leak memory.
--
--
-- @
-- 'minimumOf' :: 'Ord' a => 'Getter' s a     -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Fold' s a       -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Iso'' s a       -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Lens'' s a      -> s -> 'Maybe' a
-- 'minimumOf' :: 'Ord' a => 'Traversal'' s a -> s -> 'Maybe' a
-- @
minimumOf :: Ord a => Getting (Endo (Endo (Maybe a))) s a -> s -> Maybe a
minimumOf l = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! min x y
{-# INLINE minimumOf #-}

-- | Obtain the minimum element targeted by a 'Fold1' or 'Traversal1'.
--
-- >>> minimum1Of traverse1 (1 :| [2..10])
-- 1
--
-- @
-- 'minimum1Of' :: 'Ord' a => 'Getter' s a      -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Fold1' s a       -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Iso'' s a        -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Lens'' s a       -> s -> a
-- 'minimum1Of' :: 'Ord' a => 'Traversal1'' s a -> s -> a
-- @
minimum1Of :: Ord a => Getting (Semi.Min a) s a -> s -> a
minimum1Of l = Semi.getMin . foldMapOf l Semi.Min
{-# INLINE minimum1Of #-}

-- | Obtain the maximum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso',
-- or 'Getter' according to a user supplied 'Ordering'.
--
-- >>> maximumByOf traverse (compare `on` length) ["mustard","relish","ham"]
-- Just "mustard"
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
--
-- @
-- 'Data.Foldable.maximumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'maximumByOf' 'folded' cmp
-- @
--
-- @
-- 'maximumByOf' :: 'Getter' s a     -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Fold' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Iso'' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Lens'' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'maximumByOf' :: 'Traversal'' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
maximumByOf :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> Maybe a
maximumByOf l cmp = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! if cmp x y == GT then x else y
{-# INLINE maximumByOf #-}

-- | Obtain the minimum element (if any) targeted by a 'Fold', 'Traversal', 'Lens', 'Iso'
-- or 'Getter' according to a user supplied 'Ordering'.
--
-- In the interest of efficiency, This operation has semantics more strict than strictly necessary.
--
-- >>> minimumByOf traverse (compare `on` length) ["mustard","relish","ham"]
-- Just "ham"
--
-- @
-- 'minimumBy' cmp ≡ 'Data.Maybe.fromMaybe' ('error' \"empty\") '.' 'minimumByOf' 'folded' cmp
-- @
--
-- @
-- 'minimumByOf' :: 'Getter' s a     -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Fold' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Iso'' s a       -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Lens'' s a      -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- 'minimumByOf' :: 'Traversal'' s a -> (a -> a -> 'Ordering') -> s -> 'Maybe' a
-- @
minimumByOf :: Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> Ordering) -> s -> Maybe a
minimumByOf l cmp = foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! if cmp x y == GT then y else x
{-# INLINE minimumByOf #-}

-- | The 'findOf' function takes a 'Lens' (or 'Getter', 'Iso', 'Fold', or 'Traversal'),
-- a predicate and a structure and returns the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- >>> findOf each even (1,3,4,6)
-- Just 4
--
-- >>> findOf folded even [1,3,5,7]
-- Nothing
--
-- @
-- 'findOf' :: 'Getter' s a     -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Fold' s a       -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Iso'' s a       -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Lens'' s a      -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' :: 'Traversal'' s a -> (a -> 'Bool') -> s -> 'Maybe' a
-- @
--
-- @
-- 'Data.Foldable.find' ≡ 'findOf' 'folded'
-- 'ifindOf' l ≡ 'findOf' l '.' 'Indexed'
-- @
--
-- A simpler version that didn't permit indexing, would be:
--
-- @
-- 'findOf' :: 'Getting' ('Endo' ('Maybe' a)) s a -> (a -> 'Bool') -> s -> 'Maybe' a
-- 'findOf' l p = 'foldrOf' l (\a y -> if p a then 'Just' a else y) 'Nothing'
-- @
findOf :: Getting (Endo (Maybe a)) s a -> (a -> Bool) -> s -> Maybe a
findOf l f = foldrOf l (\a y -> if f a then Just a else y) Nothing
{-# INLINE findOf #-}

-- | The 'findMOf' function takes a 'Lens' (or 'Getter', 'Iso', 'Fold', or 'Traversal'),
-- a monadic predicate and a structure and returns in the monad the leftmost element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- >>>  findMOf each ( \x -> print ("Checking " ++ show x) >> return (even x)) (1,3,4,6)
-- "Checking 1"
-- "Checking 3"
-- "Checking 4"
-- Just 4
--
-- >>>  findMOf each ( \x -> print ("Checking " ++ show x) >> return (even x)) (1,3,5,7)
-- "Checking 1"
-- "Checking 3"
-- "Checking 5"
-- "Checking 7"
-- Nothing
--
-- @
-- 'findMOf' :: ('Monad' m, 'Getter' s a)     -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Fold' s a)       -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Iso'' s a)       -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Lens'' s a)      -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' :: ('Monad' m, 'Traversal'' s a) -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- @
--
-- @
-- 'findMOf' 'folded' :: (Monad m, Foldable f) => (a -> m Bool) -> f a -> m (Maybe a)
-- 'ifindMOf' l ≡ 'findMOf' l '.' 'Indexed'
-- @
--
-- A simpler version that didn't permit indexing, would be:
--
-- @
-- 'findMOf' :: Monad m => 'Getting' ('Endo' (m ('Maybe' a))) s a -> (a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'findMOf' l p = 'foldrOf' l (\a y -> p a >>= \x -> if x then return ('Just' a) else y) $ return 'Nothing'
-- @
findMOf :: Monad m => Getting (Endo (m (Maybe a))) s a -> (a -> m Bool) -> s -> m (Maybe a)
findMOf l f = foldrOf l (\a y -> f a >>= \r -> if r then return (Just a) else y) $ return Nothing
{-# INLINE findMOf #-}

-- | The 'lookupOf' function takes a 'Fold' (or 'Getter', 'Traversal',
-- 'Lens', 'Iso', etc.), a key, and a structure containing key/value pairs.
-- It returns the first value corresponding to the given key. This function
-- generalizes 'lookup' to work on an arbitrary 'Fold' instead of lists.
--
-- >>> lookupOf folded 4 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'b'
--
-- >>> lookupOf each 2 [(2, 'a'), (4, 'b'), (4, 'c')]
-- Just 'a'
--
-- @
-- 'lookupOf' :: 'Eq' k => 'Fold' s (k,v) -> k -> s -> 'Maybe' v
-- @
lookupOf :: Eq k => Getting (Endo (Maybe v)) s (k,v) -> k -> s -> Maybe v
lookupOf l k = foldrOf l (\(k',v) next -> if k == k' then Just v else next) Nothing
{-# INLINE lookupOf #-}

-- | A variant of 'foldrOf' that has no base case and thus may only be applied
-- to lenses and structures such that the 'Lens' views at least one element of
-- the structure.
--
-- >>> foldr1Of each (+) (1,2,3,4)
-- 10
--
-- @
-- 'foldr1Of' l f ≡ 'Prelude.foldr1' f '.' 'toListOf' l
-- 'Data.Foldable.foldr1' ≡ 'foldr1Of' 'folded'
-- @
--
-- @
-- 'foldr1Of' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldr1Of' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldr1Of :: HasCallStack => Getting (Endo (Maybe a)) s a -> (a -> a -> a) -> s -> a
-- See: NOTE: [Inlining and arity]
foldr1Of l f = fromMaybe (error "foldr1Of: empty structure")
             . foldrOf l mf Nothing where
  mf x my = Just $ case my of
    Nothing -> x
    Just y -> f x y
{-# INLINE foldr1Of #-}

-- | A variant of 'foldlOf' that has no base case and thus may only be applied to lenses and structures such
-- that the 'Lens' views at least one element of the structure.
--
-- >>> foldl1Of each (+) (1,2,3,4)
-- 10
--
-- @
-- 'foldl1Of' l f ≡ 'Prelude.foldl1' f '.' 'toListOf' l
-- 'Data.Foldable.foldl1' ≡ 'foldl1Of' 'folded'
-- @
--
-- @
-- 'foldl1Of' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldl1Of' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldl1Of :: HasCallStack => Getting (Dual (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
-- See: NOTE: [Inlining and arity]
foldl1Of l f = fromMaybe (error "foldl1Of: empty structure") . foldlOf l mf Nothing where
  mf mx y = Just $ case mx of
    Nothing -> y
    Just x  -> f x y
{-# INLINE foldl1Of #-}

-- | Strictly fold right over the elements of a structure.
--
-- @
-- 'Data.Foldable.foldr'' ≡ 'foldrOf'' 'folded'
-- @
--
-- @
-- 'foldrOf'' :: 'Getter' s a     -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Fold' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Iso'' s a       -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Lens'' s a      -> (a -> r -> r) -> r -> s -> r
-- 'foldrOf'' :: 'Traversal'' s a -> (a -> r -> r) -> r -> s -> r
-- @
foldrOf' :: Getting (Dual (Endo (Endo r))) s a -> (a -> r -> r) -> r -> s -> r
-- See: NOTE: [Inlining and arity]
foldrOf' l f z0 = \xs -> foldlOf l f' (Endo id) xs `appEndo` z0
  where f' (Endo k) x = Endo $ \ z -> k $! f x z
{-# INLINE foldrOf' #-}

-- | Fold over the elements of a structure, associating to the left, but strictly.
--
-- @
-- 'Data.Foldable.foldl'' ≡ 'foldlOf'' 'folded'
-- @
--
-- @
-- 'foldlOf'' :: 'Getter' s a     -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Fold' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Iso'' s a       -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Lens'' s a      -> (r -> a -> r) -> r -> s -> r
-- 'foldlOf'' :: 'Traversal'' s a -> (r -> a -> r) -> r -> s -> r
-- @
foldlOf' :: Getting (Endo (Endo r)) s a -> (r -> a -> r) -> r -> s -> r
-- See: NOTE: [Inlining and arity]
foldlOf' l f z0 = \xs -> foldrOf l f' (Endo id) xs `appEndo` z0
  where f' x (Endo k) = Endo $ \z -> k $! f z x
{-# INLINE foldlOf' #-}

-- | A variant of 'foldrOf'' that has no base case and thus may only be applied
-- to folds and structures such that the fold views at least one element of the
-- structure.
--
-- @
-- 'foldr1Of' l f ≡ 'Prelude.foldr1' f '.' 'toListOf' l
-- @
--
-- @
-- 'foldr1Of'' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldr1Of'' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldr1Of' :: HasCallStack => Getting (Dual (Endo (Endo (Maybe a)))) s a -> (a -> a -> a) -> s -> a
-- See: NOTE: [Inlining and arity]
foldr1Of' l f = fromMaybe (error "foldr1Of': empty structure") . foldrOf' l mf Nothing where
  mf x Nothing = Just $! x
  mf x (Just y) = Just $! f x y
{-# INLINE foldr1Of' #-}

-- | A variant of 'foldlOf'' that has no base case and thus may only be applied
-- to folds and structures such that the fold views at least one element of
-- the structure.
--
-- @
-- 'foldl1Of'' l f ≡ 'Data.List.foldl1'' f '.' 'toListOf' l
-- @
--
-- @
-- 'foldl1Of'' :: 'Getter' s a     -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Fold' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Iso'' s a       -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Lens'' s a      -> (a -> a -> a) -> s -> a
-- 'foldl1Of'' :: 'Traversal'' s a -> (a -> a -> a) -> s -> a
-- @
foldl1Of' :: HasCallStack => Getting (Endo (Endo (Maybe a))) s a -> (a -> a -> a) -> s -> a
-- See: NOTE: [Inlining and arity]
foldl1Of' l f = fromMaybe (error "foldl1Of': empty structure") . foldlOf' l mf Nothing where
  mf Nothing y = Just $! y
  mf (Just x) y = Just $! f x y
{-# INLINE foldl1Of' #-}

-- | Monadic fold over the elements of a structure, associating to the right,
-- i.e. from right to left.
--
-- @
-- 'Data.Foldable.foldrM' ≡ 'foldrMOf' 'folded'
-- @
--
-- @
-- 'foldrMOf' :: 'Monad' m => 'Getter' s a     -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Fold' s a       -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Iso'' s a       -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Lens'' s a      -> (a -> r -> m r) -> r -> s -> m r
-- 'foldrMOf' :: 'Monad' m => 'Traversal'' s a -> (a -> r -> m r) -> r -> s -> m r
-- @
foldrMOf :: Monad m
         => Getting (Dual (Endo (r -> m r))) s a
         -> (a -> r -> m r) -> r -> s -> m r
-- See: NOTE: [Inlining and arity]
foldrMOf l f z0 = \xs -> foldlOf l f' return xs z0
  where f' k x z = f x z >>= k
{-# INLINE foldrMOf #-}

-- | Monadic fold over the elements of a structure, associating to the left,
-- i.e. from left to right.
--
-- @
-- 'Data.Foldable.foldlM' ≡ 'foldlMOf' 'folded'
-- @
--
-- @
-- 'foldlMOf' :: 'Monad' m => 'Getter' s a     -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Fold' s a       -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Iso'' s a       -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Lens'' s a      -> (r -> a -> m r) -> r -> s -> m r
-- 'foldlMOf' :: 'Monad' m => 'Traversal'' s a -> (r -> a -> m r) -> r -> s -> m r
-- @
foldlMOf :: Monad m
         => Getting (Endo (r -> m r)) s a
         -> (r -> a -> m r) -> r -> s -> m r
-- See: NOTE: [Inlining and arity]
foldlMOf l f z0 = \xs -> foldrOf l f' return xs z0
  where f' x k z = f z x >>= k
{-# INLINE foldlMOf #-}

-- NOTE: [Inlining and arity]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~
--
-- GHC uses the following inlining heuristic: a function body is inlined if
-- all its arguments on the LHS are applied. So the following two definitions
-- are not equivalent from the inliner's PoV:
--
-- > foldlOf' l f z0 xs = ...
-- > foldlOf' l f z0 = \xs -> ...
--
-- GHC will be less eager to inline the first one and this results in
-- worse code. For example, a simple list summation using `sumOf` will be 8x slower
-- with the first version.


-- | Check to see if this 'Fold' or 'Traversal' matches 1 or more entries.
--
-- >>> has (element 0) []
-- False
--
-- >>> has _Left (Left 12)
-- True
--
-- >>> has _Right (Left 12)
-- False
--
-- This will always return 'True' for a 'Lens' or 'Getter'.
--
-- >>> has _1 ("hello","world")
-- True
--
-- @
-- 'has' :: 'Getter' s a     -> s -> 'Bool'
-- 'has' :: 'Fold' s a       -> s -> 'Bool'
-- 'has' :: 'Iso'' s a       -> s -> 'Bool'
-- 'has' :: 'Lens'' s a      -> s -> 'Bool'
-- 'has' :: 'Traversal'' s a -> s -> 'Bool'
-- @
has :: Getting Any s a -> s -> Bool
has l = getAny #. foldMapOf l (\_ -> Any True)
{-# INLINE has #-}



-- | Check to see if this 'Fold' or 'Traversal' has no matches.
--
-- >>> hasn't _Left (Right 12)
-- True
--
-- >>> hasn't _Left (Left 12)
-- False
hasn't :: Getting All s a -> s -> Bool
hasn't l = getAll #. foldMapOf l (\_ -> All False)
{-# INLINE hasn't #-}

------------------------------------------------------------------------------
-- Pre
------------------------------------------------------------------------------

-- | This converts a 'Fold' to a 'IndexPreservingGetter' that returns the first element, if it
-- exists, as a 'Maybe'.
--
-- @
-- 'pre' :: 'Getter' s a     -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Fold' s a       -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Traversal'' s a -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Lens'' s a      -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Iso'' s a       -> 'IndexPreservingGetter' s ('Maybe' a)
-- 'pre' :: 'Prism'' s a     -> 'IndexPreservingGetter' s ('Maybe' a)
-- @
pre :: Getting (First a) s a -> IndexPreservingGetter s (Maybe a)
pre l = dimap (getFirst . getConst #. l (Const #. First #. Just)) phantom
{-# INLINE pre #-}

-- | This converts an 'IndexedFold' to an 'IndexPreservingGetter' that returns the first index
-- and element, if they exist, as a 'Maybe'.
--
-- @
-- 'ipre' :: 'IndexedGetter' i s a     -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedFold' i s a       -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedTraversal'' i s a -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- 'ipre' :: 'IndexedLens'' i s a      -> 'IndexPreservingGetter' s ('Maybe' (i, a))
-- @
ipre :: IndexedGetting i (First (i, a)) s a -> IndexPreservingGetter s (Maybe (i, a))
ipre l = dimap (getFirst . getConst #. l (Indexed $ \i a -> Const (First (Just (i, a))))) phantom
{-# INLINE ipre #-}

------------------------------------------------------------------------------
-- Preview
------------------------------------------------------------------------------

-- | Retrieve the first value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens'). See also 'firstOf' and '^?', which are similar with
-- some subtle differences (explained below).
--
-- @
-- 'Data.Maybe.listToMaybe' '.' 'toList' ≡ 'preview' 'folded'
-- @
--
-- @
-- 'preview' = 'view' '.' 'pre'
-- @
--
--
-- Unlike '^?', this function uses a
-- 'Control.Monad.Reader.MonadReader' to read the value to be focused in on.
-- This allows one to pass the value as the last argument by using the
-- 'Control.Monad.Reader.MonadReader' instance for @(->) s@
-- However, it may also be used as part of some deeply nested transformer stack.
--
-- 'preview' uses a monoidal value to obtain the result.
-- This means that it generally has good performance, but can occasionally cause space leaks
-- or even stack overflows on some data types.
-- There is another function, 'firstOf', which avoids these issues at the cost of
-- a slight constant performance cost and a little less flexibility.
--
-- It may be helpful to think of 'preview' as having one of the following
-- more specialized types:
--
-- @
-- 'preview' :: 'Getter' s a     -> s -> 'Maybe' a
-- 'preview' :: 'Fold' s a       -> s -> 'Maybe' a
-- 'preview' :: 'Lens'' s a      -> s -> 'Maybe' a
-- 'preview' :: 'Iso'' s a       -> s -> 'Maybe' a
-- 'preview' :: 'Traversal'' s a -> s -> 'Maybe' a
-- @
--
--
-- @
-- 'preview' :: 'MonadReader' s m => 'Getter' s a     -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Fold' s a       -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Lens'' s a      -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Iso'' s a       -> m ('Maybe' a)
-- 'preview' :: 'MonadReader' s m => 'Traversal'' s a -> m ('Maybe' a)
--
-- @
preview :: MonadReader s m => Getting (First a) s a -> m (Maybe a)
preview l = asks (getFirst #. foldMapOf l (First #. Just))
{-# INLINE preview #-}

-- | Retrieve the first index and value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens'). See also ('^@?').
--
-- @
-- 'ipreview' = 'view' '.' 'ipre'
-- @
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.
--
-- @
-- 'ipreview' :: 'IndexedGetter' i s a     -> s -> 'Maybe' (i, a)
-- 'ipreview' :: 'IndexedFold' i s a       -> s -> 'Maybe' (i, a)
-- 'ipreview' :: 'IndexedLens'' i s a      -> s -> 'Maybe' (i, a)
-- 'ipreview' :: 'IndexedTraversal'' i s a -> s -> 'Maybe' (i, a)
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Control.Monad.Monad' transformer stack:
--
-- @
-- 'ipreview' :: 'MonadReader' s m => 'IndexedGetter' s a     -> m ('Maybe' (i, a))
-- 'ipreview' :: 'MonadReader' s m => 'IndexedFold' s a       -> m ('Maybe' (i, a))
-- 'ipreview' :: 'MonadReader' s m => 'IndexedLens'' s a      -> m ('Maybe' (i, a))
-- 'ipreview' :: 'MonadReader' s m => 'IndexedTraversal'' s a -> m ('Maybe' (i, a))
-- @
ipreview :: MonadReader s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreview l = asks (getFirst #. ifoldMapOf l (\i a -> First (Just (i, a))))
{-# INLINE ipreview #-}

-- | Retrieve a function of the first value targeted by a 'Fold' or
-- 'Traversal' (or 'Just' the result from a 'Getter' or 'Lens').
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.

-- @
-- 'previews' = 'views' '.' 'pre'
-- @
--
-- @
-- 'previews' :: 'Getter' s a     -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Fold' s a       -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Lens'' s a      -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Iso'' s a       -> (a -> r) -> s -> 'Maybe' r
-- 'previews' :: 'Traversal'' s a -> (a -> r) -> s -> 'Maybe' r
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Monad' transformer stack:
--
-- @
-- 'previews' :: 'MonadReader' s m => 'Getter' s a     -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Fold' s a       -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Lens'' s a      -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Iso'' s a       -> (a -> r) -> m ('Maybe' r)
-- 'previews' :: 'MonadReader' s m => 'Traversal'' s a -> (a -> r) -> m ('Maybe' r)
-- @
previews :: MonadReader s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
previews l f = asks (getFirst . foldMapOf l (First #. Just . f))
{-# INLINE previews #-}

-- | Retrieve a function of the first index and value targeted by an 'IndexedFold' or
-- 'IndexedTraversal' (or 'Just' the result from an 'IndexedGetter' or 'IndexedLens').
-- See also ('^@?').
--
-- @
-- 'ipreviews' = 'views' '.' 'ipre'
-- @
--
-- This is usually applied in the 'Control.Monad.Reader.Reader'
-- 'Control.Monad.Monad' @(->) s@.
--
-- @
-- 'ipreviews' :: 'IndexedGetter' i s a     -> (i -> a -> r) -> s -> 'Maybe' r
-- 'ipreviews' :: 'IndexedFold' i s a       -> (i -> a -> r) -> s -> 'Maybe' r
-- 'ipreviews' :: 'IndexedLens'' i s a      -> (i -> a -> r) -> s -> 'Maybe' r
-- 'ipreviews' :: 'IndexedTraversal'' i s a -> (i -> a -> r) -> s -> 'Maybe' r
-- @
--
-- However, it may be useful to think of its full generality when working with
-- a 'Control.Monad.Monad' transformer stack:
--
-- @
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedGetter' i s a     -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedFold' i s a       -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedLens'' i s a      -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreviews' :: 'MonadReader' s m => 'IndexedTraversal'' i s a -> (i -> a -> r) -> m ('Maybe' r)
-- @
ipreviews :: MonadReader s m => IndexedGetting i (First r) s a -> (i -> a -> r) -> m (Maybe r)
ipreviews l f = asks (getFirst . ifoldMapOf l (\i -> First #. Just . f i))
{-# INLINE ipreviews #-}

------------------------------------------------------------------------------
-- Preuse
------------------------------------------------------------------------------

-- | Retrieve the first value targeted by a 'Fold' or 'Traversal' (or 'Just' the result
-- from a 'Getter' or 'Lens') into the current state.
--
-- @
-- 'preuse' = 'use' '.' 'pre'
-- @
--
-- @
-- 'preuse' :: 'MonadState' s m => 'Getter' s a     -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Fold' s a       -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Lens'' s a      -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Iso'' s a       -> m ('Maybe' a)
-- 'preuse' :: 'MonadState' s m => 'Traversal'' s a -> m ('Maybe' a)
-- @
preuse :: MonadState s m => Getting (First a) s a -> m (Maybe a)
preuse l = gets (preview l)
{-# INLINE preuse #-}

-- | Retrieve the first index and value targeted by an 'IndexedFold' or 'IndexedTraversal' (or 'Just' the index
-- and result from an 'IndexedGetter' or 'IndexedLens') into the current state.
--
-- @
-- 'ipreuse' = 'use' '.' 'ipre'
-- @
--
-- @
-- 'ipreuse' :: 'MonadState' s m => 'IndexedGetter' i s a     -> m ('Maybe' (i, a))
-- 'ipreuse' :: 'MonadState' s m => 'IndexedFold' i s a       -> m ('Maybe' (i, a))
-- 'ipreuse' :: 'MonadState' s m => 'IndexedLens'' i s a      -> m ('Maybe' (i, a))
-- 'ipreuse' :: 'MonadState' s m => 'IndexedTraversal'' i s a -> m ('Maybe' (i, a))
-- @
ipreuse :: MonadState s m => IndexedGetting i (First (i, a)) s a -> m (Maybe (i, a))
ipreuse l = gets (ipreview l)
{-# INLINE ipreuse #-}

-- | Retrieve a function of the first value targeted by a 'Fold' or
-- 'Traversal' (or 'Just' the result from a 'Getter' or 'Lens') into the current state.
--
-- @
-- 'preuses' = 'uses' '.' 'pre'
-- @
--
-- @
-- 'preuses' :: 'MonadState' s m => 'Getter' s a     -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Fold' s a       -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Lens'' s a      -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Iso'' s a       -> (a -> r) -> m ('Maybe' r)
-- 'preuses' :: 'MonadState' s m => 'Traversal'' s a -> (a -> r) -> m ('Maybe' r)
-- @
preuses :: MonadState s m => Getting (First r) s a -> (a -> r) -> m (Maybe r)
preuses l f = gets (previews l f)
{-# INLINE preuses #-}

-- | Retrieve a function of the first index and value targeted by an 'IndexedFold' or
-- 'IndexedTraversal' (or a function of 'Just' the index and result from an 'IndexedGetter'
-- or 'IndexedLens') into the current state.
--
-- @
-- 'ipreuses' = 'uses' '.' 'ipre'
-- @
--
-- @
-- 'ipreuses' :: 'MonadState' s m => 'IndexedGetter' i s a     -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreuses' :: 'MonadState' s m => 'IndexedFold' i s a       -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreuses' :: 'MonadState' s m => 'IndexedLens'' i s a      -> (i -> a -> r) -> m ('Maybe' r)
-- 'ipreuses' :: 'MonadState' s m => 'IndexedTraversal'' i s a -> (i -> a -> r) -> m ('Maybe' r)
-- @
ipreuses :: MonadState s m => IndexedGetting i (First r) s a -> (i -> a -> r) -> m (Maybe r)
ipreuses l f = gets (ipreviews l f)
{-# INLINE ipreuses #-}

------------------------------------------------------------------------------
-- Profunctors
------------------------------------------------------------------------------


-- | This allows you to 'Control.Traversable.traverse' the elements of a pretty much any 'LensLike' construction in the opposite order.
--
-- This will preserve indexes on 'Indexed' types and will give you the elements of a (finite) 'Fold' or 'Traversal' in the opposite order.
--
-- This has no practical impact on a 'Getter', 'Setter', 'Lens' or 'Iso'.
--
-- /NB:/ To write back through an 'Iso', you want to use 'Control.Lens.Isomorphic.from'.
-- Similarly, to write back through an 'Prism', you want to use 'Control.Lens.Review.re'.
backwards :: (Profunctor p, Profunctor q) => Optical p q (Backwards f) s t a b -> Optical p q f s t a b
backwards l f = forwards #. l (Backwards #. f)
{-# INLINE backwards #-}

------------------------------------------------------------------------------
-- Indexed Folds
------------------------------------------------------------------------------

-- | Fold an 'IndexedFold' or 'IndexedTraversal' by mapping indices and values to an arbitrary 'Monoid' with access
-- to the @i@.
--
-- When you don't need access to the index then 'foldMapOf' is more flexible in what it accepts.
--
-- @
-- 'foldMapOf' l ≡ 'ifoldMapOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldMapOf' ::             'IndexedGetter' i s a     -> (i -> a -> m) -> s -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedFold' i s a       -> (i -> a -> m) -> s -> m
-- 'ifoldMapOf' ::             'IndexedLens'' i s a      -> (i -> a -> m) -> s -> m
-- 'ifoldMapOf' :: 'Monoid' m => 'IndexedTraversal'' i s a -> (i -> a -> m) -> s -> m
-- @
--
ifoldMapOf :: IndexedGetting i m s a -> (i -> a -> m) -> s -> m
ifoldMapOf = coerce
{-# INLINE ifoldMapOf #-}

-- | Right-associative fold of parts of a structure that are viewed through an 'IndexedFold' or 'IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'foldrOf' is more flexible in what it accepts.
--
-- @
-- 'foldrOf' l ≡ 'ifoldrOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldrOf' :: 'IndexedGetter' i s a     -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedFold' i s a       -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf' :: 'IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf :: IndexedGetting i (Endo r) s a -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf l f z = flip appEndo z . getConst #. l (Const #. Endo #. Indexed f)
{-# INLINE ifoldrOf #-}

-- | Left-associative fold of the parts of a structure that are viewed through an 'IndexedFold' or 'IndexedTraversal' with
-- access to the @i@.
--
-- When you don't need access to the index then 'foldlOf' is more flexible in what it accepts.
--
-- @
-- 'foldlOf' l ≡ 'ifoldlOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldlOf' :: 'IndexedGetter' i s a     -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedFold' i s a       -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedLens'' i s a      -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf' :: 'IndexedTraversal'' i s a -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf :: IndexedGetting i (Dual (Endo r)) s a -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf l f z = (flip appEndo z .# getDual) `rmap` ifoldMapOf l (\i -> Dual #. Endo #. flip (f i))
{-# INLINE ifoldlOf #-}

-- | Return whether or not any element viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'anyOf' is more flexible in what it accepts.
--
-- @
-- 'anyOf' l ≡ 'ianyOf' l '.' 'const'
-- @
--
-- @
-- 'ianyOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'ianyOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
ianyOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
ianyOf = coerce
{-# INLINE ianyOf #-}

-- | Return whether or not all elements viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'allOf' is more flexible in what it accepts.
--
-- @
-- 'allOf' l ≡ 'iallOf' l '.' 'const'
-- @
--
-- @
-- 'iallOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'iallOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
iallOf :: IndexedGetting i All s a -> (i -> a -> Bool) -> s -> Bool
iallOf = coerce
{-# INLINE iallOf #-}

-- | Return whether or not none of the elements viewed through an 'IndexedFold' or 'IndexedTraversal'
-- satisfy a predicate, with access to the @i@.
--
-- When you don't need access to the index then 'noneOf' is more flexible in what it accepts.
--
-- @
-- 'noneOf' l ≡ 'inoneOf' l '.' 'const'
-- @
--
-- @
-- 'inoneOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'inoneOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'inoneOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Bool'
-- 'inoneOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Bool'
-- @
inoneOf :: IndexedGetting i Any s a -> (i -> a -> Bool) -> s -> Bool
inoneOf l f = not . ianyOf l f
{-# INLINE inoneOf #-}

-- | Traverse the targets of an 'IndexedFold' or 'IndexedTraversal' with access to the @i@, discarding the results.
--
-- When you don't need access to the index then 'traverseOf_' is more flexible in what it accepts.
--
-- @
-- 'traverseOf_' l ≡ 'Control.Lens.Traversal.itraverseOf' l '.' 'const'
-- @
--
-- @
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedGetter' i s a     -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedFold' i s a       -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Functor' f     => 'IndexedLens'' i s a      -> (i -> a -> f r) -> s -> f ()
-- 'itraverseOf_' :: 'Applicative' f => 'IndexedTraversal'' i s a -> (i -> a -> f r) -> s -> f ()
-- @
itraverseOf_ :: Functor f => IndexedGetting i (Traversed r f) s a -> (i -> a -> f r) -> s -> f ()
itraverseOf_ l f = void . getTraversed #. getConst #. l (Const #. Traversed #. Indexed f)
{-# INLINE itraverseOf_ #-}

-- | Traverse the targets of an 'IndexedFold' or 'IndexedTraversal' with access to the index, discarding the results
-- (with the arguments flipped).
--
-- @
-- 'iforOf_' ≡ 'flip' '.' 'itraverseOf_'
-- @
--
-- When you don't need access to the index then 'forOf_' is more flexible in what it accepts.
--
-- @
-- 'forOf_' l a ≡ 'iforOf_' l a '.' 'const'
-- @
--
-- @
-- 'iforOf_' :: 'Functor' f     => 'IndexedGetter' i s a     -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedFold' i s a       -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Functor' f     => 'IndexedLens'' i s a      -> s -> (i -> a -> f r) -> f ()
-- 'iforOf_' :: 'Applicative' f => 'IndexedTraversal'' i s a -> s -> (i -> a -> f r) -> f ()
-- @
iforOf_ :: Functor f => IndexedGetting i (Traversed r f) s a -> s -> (i -> a -> f r) -> f ()
iforOf_ = flip . itraverseOf_
{-# INLINE iforOf_ #-}

-- | Run monadic actions for each target of an 'IndexedFold' or 'IndexedTraversal' with access to the index,
-- discarding the results.
--
-- When you don't need access to the index then 'mapMOf_' is more flexible in what it accepts.
--
-- @
-- 'mapMOf_' l ≡ 'Control.Lens.Setter.imapMOf' l '.' 'const'
-- @
--
-- @
-- 'imapMOf_' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> m r) -> s -> m ()
-- 'imapMOf_' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> m r) -> s -> m ()
-- @
imapMOf_ :: Monad m => IndexedGetting i (Sequenced r m) s a -> (i -> a -> m r) -> s -> m ()
imapMOf_ l f = liftM skip . getSequenced #. getConst #. l (Const #. Sequenced #. Indexed f)
{-# INLINE imapMOf_ #-}

-- | Run monadic actions for each target of an 'IndexedFold' or 'IndexedTraversal' with access to the index,
-- discarding the results (with the arguments flipped).
--
-- @
-- 'iforMOf_' ≡ 'flip' '.' 'imapMOf_'
-- @
--
-- When you don't need access to the index then 'forMOf_' is more flexible in what it accepts.
--
-- @
-- 'forMOf_' l a ≡ 'Control.Lens.Traversal.iforMOf' l a '.' 'const'
-- @
--
-- @
-- 'iforMOf_' :: 'Monad' m => 'IndexedGetter' i s a     -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedFold' i s a       -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedLens'' i s a      -> s -> (i -> a -> m r) -> m ()
-- 'iforMOf_' :: 'Monad' m => 'IndexedTraversal'' i s a -> s -> (i -> a -> m r) -> m ()
-- @
iforMOf_ :: Monad m => IndexedGetting i (Sequenced r m) s a -> s -> (i -> a -> m r) -> m ()
iforMOf_ = flip . imapMOf_
{-# INLINE iforMOf_ #-}

-- | Concatenate the results of a function of the elements of an 'IndexedFold' or 'IndexedTraversal'
-- with access to the index.
--
-- When you don't need access to the index then 'concatMapOf'  is more flexible in what it accepts.
--
-- @
-- 'concatMapOf' l ≡ 'iconcatMapOf' l '.' 'const'
-- 'iconcatMapOf' ≡ 'ifoldMapOf'
-- @
--
-- @
-- 'iconcatMapOf' :: 'IndexedGetter' i s a     -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedFold' i s a       -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedLens'' i s a      -> (i -> a -> [r]) -> s -> [r]
-- 'iconcatMapOf' :: 'IndexedTraversal'' i s a -> (i -> a -> [r]) -> s -> [r]
-- @
iconcatMapOf :: IndexedGetting i [r] s a -> (i -> a -> [r]) -> s -> [r]
iconcatMapOf = ifoldMapOf
{-# INLINE iconcatMapOf #-}

-- | The 'ifindOf' function takes an 'IndexedFold' or 'IndexedTraversal', a predicate that is also
-- supplied the index, a structure and returns the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'findOf' is more flexible in what it accepts.
--
-- @
-- 'findOf' l ≡ 'ifindOf' l '.' 'const'
-- @
--
-- @
-- 'ifindOf' :: 'IndexedGetter' i s a     -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- 'ifindOf' :: 'IndexedFold' i s a       -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- 'ifindOf' :: 'IndexedLens'' i s a      -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- 'ifindOf' :: 'IndexedTraversal'' i s a -> (i -> a -> 'Bool') -> s -> 'Maybe' a
-- @
ifindOf :: IndexedGetting i (Endo (Maybe a)) s a -> (i -> a -> Bool) -> s -> Maybe a
ifindOf l f = ifoldrOf l (\i a y -> if f i a then Just a else y) Nothing
{-# INLINE ifindOf #-}

-- | The 'ifindMOf' function takes an 'IndexedFold' or 'IndexedTraversal', a monadic predicate that is also
-- supplied the index, a structure and returns in the monad the left-most element of the structure
-- matching the predicate, or 'Nothing' if there is no such element.
--
-- When you don't need access to the index then 'findMOf' is more flexible in what it accepts.
--
-- @
-- 'findMOf' l ≡ 'ifindMOf' l '.' 'const'
-- @
--
-- @
-- 'ifindMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'ifindMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'ifindMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- 'ifindMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> m 'Bool') -> s -> m ('Maybe' a)
-- @
ifindMOf :: Monad m => IndexedGetting i (Endo (m (Maybe a))) s a -> (i -> a -> m Bool) -> s -> m (Maybe a)
ifindMOf l f = ifoldrOf l (\i a y -> f i a >>= \r -> if r then return (Just a) else y) $ return Nothing
{-# INLINE ifindMOf #-}

-- | /Strictly/ fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrOf'' is more flexible in what it accepts.
--
-- @
-- 'foldrOf'' l ≡ 'ifoldrOf'' l '.' 'const'
-- @
--
-- @
-- 'ifoldrOf'' :: 'IndexedGetter' i s a     -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedFold' i s a       -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedLens'' i s a      -> (i -> a -> r -> r) -> r -> s -> r
-- 'ifoldrOf'' :: 'IndexedTraversal'' i s a -> (i -> a -> r -> r) -> r -> s -> r
-- @
ifoldrOf' :: IndexedGetting i (Dual (Endo (r -> r))) s a -> (i -> a -> r -> r) -> r -> s -> r
ifoldrOf' l f z0 xs = ifoldlOf l f' id xs z0
  where f' i k x z = k $! f i x z
{-# INLINE ifoldrOf' #-}

-- | Fold over the elements of a structure with an index, associating to the left, but /strictly/.
--
-- When you don't need access to the index then 'foldlOf'' is more flexible in what it accepts.
--
-- @
-- 'foldlOf'' l ≡ 'ifoldlOf'' l '.' 'const'
-- @
--
-- @
-- 'ifoldlOf'' :: 'IndexedGetter' i s a       -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedFold' i s a         -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedLens'' i s a        -> (i -> r -> a -> r) -> r -> s -> r
-- 'ifoldlOf'' :: 'IndexedTraversal'' i s a   -> (i -> r -> a -> r) -> r -> s -> r
-- @
ifoldlOf' :: IndexedGetting i (Endo (r -> r)) s a -> (i -> r -> a -> r) -> r -> s -> r
ifoldlOf' l f z0 xs = ifoldrOf l f' id xs z0
  where f' i x k z = k $! f i z x
{-# INLINE ifoldlOf' #-}

-- | Monadic fold right over the elements of a structure with an index.
--
-- When you don't need access to the index then 'foldrMOf' is more flexible in what it accepts.
--
-- @
-- 'foldrMOf' l ≡ 'ifoldrMOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> a -> r -> m r) -> r -> s -> m r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> a -> r -> m r) -> r -> s -> m r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> a -> r -> m r) -> r -> s -> m r
-- 'ifoldrMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> a -> r -> m r) -> r -> s -> m r
-- @
ifoldrMOf :: Monad m => IndexedGetting i (Dual (Endo (r -> m r))) s a -> (i -> a -> r -> m r) -> r -> s -> m r
ifoldrMOf l f z0 xs = ifoldlOf l f' return xs z0
  where f' i k x z = f i x z >>= k
{-# INLINE ifoldrMOf #-}

-- | Monadic fold over the elements of a structure with an index, associating to the left.
--
-- When you don't need access to the index then 'foldlMOf' is more flexible in what it accepts.
--
-- @
-- 'foldlMOf' l ≡ 'ifoldlMOf' l '.' 'const'
-- @
--
-- @
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedGetter' i s a     -> (i -> r -> a -> m r) -> r -> s -> m r
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedFold' i s a       -> (i -> r -> a -> m r) -> r -> s -> m r
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedLens'' i s a      -> (i -> r -> a -> m r) -> r -> s -> m r
-- 'ifoldlMOf' :: 'Monad' m => 'IndexedTraversal'' i s a -> (i -> r -> a -> m r) -> r -> s -> m r
-- @
ifoldlMOf :: Monad m => IndexedGetting i (Endo (r -> m r)) s a -> (i -> r -> a -> m r) -> r -> s -> m r
ifoldlMOf l f z0 xs = ifoldrOf l f' return xs z0
  where f' i x k z = f i z x >>= k
{-# INLINE ifoldlMOf #-}

-- | Extract the key-value pairs from a structure.
--
-- When you don't need access to the indices in the result, then 'toListOf' is more flexible in what it accepts.
--
-- @
-- 'toListOf' l ≡ 'map' 'snd' '.' 'itoListOf' l
-- @
--
-- @
-- 'itoListOf' :: 'IndexedGetter' i s a     -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedFold' i s a       -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedLens'' i s a      -> s -> [(i,a)]
-- 'itoListOf' :: 'IndexedTraversal'' i s a -> s -> [(i,a)]
-- @
itoListOf :: IndexedGetting i (Endo [(i,a)]) s a -> s -> [(i,a)]
itoListOf l = ifoldrOf l (\i a -> ((i,a):)) []
{-# INLINE itoListOf #-}

-- | An infix version of 'itoListOf'.

-- @
-- ('^@..') :: s -> 'IndexedGetter' i s a     -> [(i,a)]
-- ('^@..') :: s -> 'IndexedFold' i s a       -> [(i,a)]
-- ('^@..') :: s -> 'IndexedLens'' i s a      -> [(i,a)]
-- ('^@..') :: s -> 'IndexedTraversal'' i s a -> [(i,a)]
-- @
(^@..) :: s -> IndexedGetting i (Endo [(i,a)]) s a -> [(i,a)]
s ^@.. l = ifoldrOf l (\i a -> ((i,a):)) [] s
{-# INLINE (^@..) #-}

-- | Perform a safe 'head' (with index) of an 'IndexedFold' or 'IndexedTraversal' or retrieve 'Just' the index and result
-- from an 'IndexedGetter' or 'IndexedLens'.
--
-- When using a 'IndexedTraversal' as a partial 'IndexedLens', or an 'IndexedFold' as a partial 'IndexedGetter' this can be a convenient
-- way to extract the optional value.
--
-- @
-- ('^@?') :: s -> 'IndexedGetter' i s a     -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedFold' i s a       -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedLens'' i s a      -> 'Maybe' (i, a)
-- ('^@?') :: s -> 'IndexedTraversal'' i s a -> 'Maybe' (i, a)
-- @
(^@?) :: s -> IndexedGetting i (Endo (Maybe (i, a))) s a -> Maybe (i, a)
s ^@? l = ifoldrOf l (\i x _ -> Just (i,x)) Nothing s
{-# INLINE (^@?) #-}

-- | Perform an *UNSAFE* 'head' (with index) of an 'IndexedFold' or 'IndexedTraversal' assuming that it is there.
--
-- @
-- ('^@?!') :: s -> 'IndexedGetter' i s a     -> (i, a)
-- ('^@?!') :: s -> 'IndexedFold' i s a       -> (i, a)
-- ('^@?!') :: s -> 'IndexedLens'' i s a      -> (i, a)
-- ('^@?!') :: s -> 'IndexedTraversal'' i s a -> (i, a)
-- @
(^@?!) :: HasCallStack => s -> IndexedGetting i (Endo (i, a)) s a -> (i, a)
s ^@?! l = ifoldrOf l (\i x _ -> (i,x)) (error "(^@?!): empty Fold") s
{-# INLINE (^@?!) #-}

-- | Retrieve the index of the first value targeted by a 'IndexedFold' or 'IndexedTraversal' which is equal to a given value.
--
-- @
-- 'Data.List.elemIndex' ≡ 'elemIndexOf' 'folded'
-- @
--
-- @
-- 'elemIndexOf' :: 'Eq' a => 'IndexedFold' i s a       -> a -> s -> 'Maybe' i
-- 'elemIndexOf' :: 'Eq' a => 'IndexedTraversal'' i s a -> a -> s -> 'Maybe' i
-- @
elemIndexOf :: Eq a => IndexedGetting i (First i) s a -> a -> s -> Maybe i
elemIndexOf l a = findIndexOf l (a ==)
{-# INLINE elemIndexOf #-}

-- | Retrieve the indices of the values targeted by a 'IndexedFold' or 'IndexedTraversal' which are equal to a given value.
--
-- @
-- 'Data.List.elemIndices' ≡ 'elemIndicesOf' 'folded'
-- @
--
-- @
-- 'elemIndicesOf' :: 'Eq' a => 'IndexedFold' i s a       -> a -> s -> [i]
-- 'elemIndicesOf' :: 'Eq' a => 'IndexedTraversal'' i s a -> a -> s -> [i]
-- @
elemIndicesOf :: Eq a => IndexedGetting i (Endo [i]) s a -> a -> s -> [i]
elemIndicesOf l a = findIndicesOf l (a ==)
{-# INLINE elemIndicesOf #-}

-- | Retrieve the index of the first value targeted by a 'IndexedFold' or 'IndexedTraversal' which satisfies a predicate.
--
-- @
-- 'Data.List.findIndex' ≡ 'findIndexOf' 'folded'
-- @
--
-- @
-- 'findIndexOf' :: 'IndexedFold' i s a       -> (a -> 'Bool') -> s -> 'Maybe' i
-- 'findIndexOf' :: 'IndexedTraversal'' i s a -> (a -> 'Bool') -> s -> 'Maybe' i
-- @
findIndexOf :: IndexedGetting i (First i) s a -> (a -> Bool) -> s -> Maybe i
findIndexOf l p = preview (l . filtered p . asIndex)
{-# INLINE findIndexOf #-}

-- | Retrieve the indices of the values targeted by a 'IndexedFold' or 'IndexedTraversal' which satisfy a predicate.
--
-- @
-- 'Data.List.findIndices' ≡ 'findIndicesOf' 'folded'
-- @
--
-- @
-- 'findIndicesOf' :: 'IndexedFold' i s a       -> (a -> 'Bool') -> s -> [i]
-- 'findIndicesOf' :: 'IndexedTraversal'' i s a -> (a -> 'Bool') -> s -> [i]
-- @
findIndicesOf :: IndexedGetting i (Endo [i]) s a -> (a -> Bool) -> s -> [i]
findIndicesOf l p = toListOf (l . filtered p . asIndex)
{-# INLINE findIndicesOf #-}

-------------------------------------------------------------------------------
-- Converting to Folds
-------------------------------------------------------------------------------

-- | Filter an 'IndexedFold' or 'IndexedGetter', obtaining an 'IndexedFold'.
--
-- >>> [0,0,0,5,5,5]^..traversed.ifiltered (\i a -> i <= a)
-- [0,5,5,5]
--
-- Compose with 'ifiltered' to filter another 'IndexedLens', 'IndexedIso', 'IndexedGetter', 'IndexedFold' (or 'IndexedTraversal') with
-- access to both the value and the index.
--
-- Note: As with 'filtered', this is /not/ a legal 'IndexedTraversal', unless you are very careful not to invalidate the predicate on the target!
ifiltered :: (Indexable i p, Applicative f) => (i -> a -> Bool) -> Optical' p (Indexed i) f a a
ifiltered p f = Indexed $ \i a -> if p i a then indexed f i a else pure a
{-# INLINE ifiltered #-}

-- | Obtain an 'IndexedFold' by taking elements from another
-- 'IndexedFold', 'IndexedLens', 'IndexedGetter' or 'IndexedTraversal' while a predicate holds.
--
-- @
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedTraversal'' i s a    -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedLens'' i s a         -> 'IndexedFold' i s a
-- 'itakingWhile' :: (i -> a -> 'Bool') -> 'IndexedGetter' i s a        -> 'IndexedFold' i s a
-- @
--
-- Note: Applying 'itakingWhile' to an 'IndexedLens' or 'IndexedTraversal' will still allow you to use it as a
-- pseudo-'IndexedTraversal', but if you change the value of any target to one where the predicate returns
-- 'False', then you will break the 'Traversal' laws and 'Traversal' fusion will no longer be sound.
itakingWhile :: (Indexable i p, Profunctor q, Contravariant f, Applicative f)
         => (i -> a -> Bool)
         -> Optical' (Indexed i) q (Const (Endo (f s))) s a
         -> Optical' p q f s a
itakingWhile p l f = (flip appEndo noEffect .# getConst) `rmap` l g where
  g = Indexed $ \i a -> Const . Endo $ if p i a then (indexed f i a *>) else const noEffect
{-# INLINE itakingWhile #-}

-- | Obtain an 'IndexedFold' by dropping elements from another 'IndexedFold', 'IndexedLens', 'IndexedGetter' or 'IndexedTraversal' while a predicate holds.
--
-- @
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedFold' i s a          -> 'IndexedFold' i s a
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedTraversal'' i s a    -> 'IndexedFold' i s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedLens'' i s a         -> 'IndexedFold' i s a -- see notes
-- 'idroppingWhile' :: (i -> a -> 'Bool') -> 'IndexedGetter' i s a        -> 'IndexedFold' i s a
-- @
--
-- Note: As with `droppingWhile` applying 'idroppingWhile' to an 'IndexedLens' or 'IndexedTraversal' will still
-- allow you to use it as a pseudo-'IndexedTraversal', but if you change the value of the first target to one
-- where the predicate returns 'True', then you will break the 'Traversal' laws and 'Traversal' fusion will
-- no longer be sound.
idroppingWhile :: (Indexable i p, Profunctor q, Applicative f)
              => (i -> a -> Bool)
              -> Optical (Indexed i) q (Compose (State Bool) f) s t a a
              -> Optical p q f s t a a
idroppingWhile p l f = (flip evalState True .# getCompose) `rmap` l g where
  g = Indexed $ \ i a -> Compose $ state $ \b -> let
      b' = b && p i a
    in (if b' then pure a else indexed f i a, b')
{-# INLINE idroppingWhile #-}

------------------------------------------------------------------------------
-- Misc.
------------------------------------------------------------------------------

skip :: a -> ()
skip _ = ()
{-# INLINE skip #-}

------------------------------------------------------------------------------
-- Folds with Reified Monoid
------------------------------------------------------------------------------

-- | Fold a value using a specified 'Fold' and 'Monoid' operations.
-- This is like 'foldBy' where the 'Foldable' instance can be
-- manually specified.
--
-- @
-- 'foldByOf' 'folded' ≡ 'foldBy'
-- @
--
-- @
-- 'foldByOf' :: 'Getter' s a     -> (a -> a -> a) -> a -> s -> a
-- 'foldByOf' :: 'Fold' s a       -> (a -> a -> a) -> a -> s -> a
-- 'foldByOf' :: 'Lens'' s a      -> (a -> a -> a) -> a -> s -> a
-- 'foldByOf' :: 'Traversal'' s a -> (a -> a -> a) -> a -> s -> a
-- 'foldByOf' :: 'Iso'' s a       -> (a -> a -> a) -> a -> s -> a
-- @
--
-- >>> foldByOf both (++) [] ("hello","world")
-- "helloworld"
foldByOf :: Fold s a -> (a -> a -> a) -> a -> s -> a
foldByOf l f z = reifyMonoid f z (foldMapOf l ReflectedMonoid)

-- | Fold a value using a specified 'Fold' and 'Monoid' operations.
-- This is like 'foldMapBy' where the 'Foldable' instance can be
-- manually specified.
--
-- @
-- 'foldMapByOf' 'folded' ≡ 'foldMapBy'
-- @
--
-- @
-- 'foldMapByOf' :: 'Getter' s a     -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- 'foldMapByOf' :: 'Fold' s a       -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- 'foldMapByOf' :: 'Traversal'' s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- 'foldMapByOf' :: 'Lens'' s a      -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- 'foldMapByOf' :: 'Iso'' s a       -> (r -> r -> r) -> r -> (a -> r) -> s -> r
-- @
--
-- >>> foldMapByOf both (+) 0 length ("hello","world")
-- 10
foldMapByOf :: Fold s a -> (r -> r -> r) -> r -> (a -> r) -> s -> r
foldMapByOf l f z g = reifyMonoid f z (foldMapOf l (ReflectedMonoid #. g))