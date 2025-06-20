{-# LANGUAGE OverloadedStrings #-}
module Bot.PolinomeAction (polinoeAction) where

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Complex as C
import qualified Numeric (showFFloat)
import Bot.Types
import Data.List (foldl', intercalate)
import System.Random (randomRs, mkStdGen)

-- | Parse "solve 2x^3 + 3x^2 - x + 1" into coefficient list [2, 3, -1, 1]
parsePolynomial :: T.Text -> Maybe [Double]
parsePolynomial txt =
  let cleaned = T.replace "-" "+-" $ T.filter (/= ' ') $ T.toLower txt
      withoutSolve = T.stripPrefix "solve" cleaned
  in case withoutSolve of
       Nothing -> Nothing
       Just body ->
        let terms = filter (not . T.null) $ T.splitOn "+" body             
            coeffsWithPowers = map parseTerm terms
        in if any (\(c, p) -> c == Nothing || p == Nothing) coeffsWithPowers
              then Nothing
              else Just (assembleCoeffs $ map (\(Just c, Just p) -> (p, c)) coeffsWithPowers)

-- | Parse term like "2x^3" -> (Just 2, Just 3), "-x" -> (Just -1, Just 1), "5" -> (Just 5, Just 0)
parseTerm :: T.Text -> (Maybe Double, Maybe Int)
parseTerm term
  | "x^" `T.isInfixOf` term =
      let [c, p] = T.splitOn "x^" term
      in (readMaybeCoeff c, readMaybeInt p)
  | "x" `T.isInfixOf` term =
      let c = T.takeWhile (/= 'x') term
      in (readMaybeCoeff c, Just 1)
  | otherwise = (readMaybeCoeff term, Just 0)

readMaybeCoeff :: T.Text -> Maybe Double
readMaybeCoeff t
  | t == "" = Just 1
  | t == "-" = Just (-1)
  | otherwise = case reads (T.unpack t) of [(x, "")] -> Just x; _ -> Nothing

readMaybeInt :: T.Text -> Maybe Int
readMaybeInt t = case reads (T.unpack t) of [(x, "")] -> Just x; _ -> Nothing

-- | Convert list of (power, coeff) to list of coefficients ordered from highest to 0
assembleCoeffs :: [(Int, Double)] -> [Double]
assembleCoeffs terms =
  let maxPow = maximum (map fst terms)
      coeffMap = foldl' (\acc (p, c) -> acc ++ replicate (p - length acc + 1) 0) [] terms
      updated = foldl' (\acc (p, c) -> take p acc ++ [acc !! p + c] ++ drop (p + 1) acc) coeffMap terms
  in reverse updated  -- Highest degree to lowest

-- | Run Aberth method (placeholder implementation)
aberthSolve :: [Double] -> [C.Complex Double]
aberthSolve coeffs = newtonFallback coeffs 100

-- | Newton-Raphson fallback root finder (for now)
newtonFallback :: [Double] -> Int -> [C.Complex Double]
newtonFallback coeffs maxIter =
  let deg = length coeffs - 1
      deriv = polyDeriv coeffs
      guesses = take deg $ map (\(r, i) -> r C.:+ i) $ zip (randomRs (-1, 1) (mkStdGen 42)) (randomRs (-1, 1) (mkStdGen 17))
  in map (\z0 -> iterate (\z -> z - (polyEval coeffs z / polyEval deriv z)) z0 !! maxIter) guesses

polyEval :: [Double] -> C.Complex Double -> C.Complex Double
polyEval coeffs x = foldl' (\acc c -> acc * x + (c C.:+ 0)) 0 coeffs

polyDeriv :: [Double] -> [Double]
polyDeriv coeffs = zipWith (*) (map fromIntegral [n-1,n-2..0]) (init coeffs)
  where n = length coeffs

-- | Pretty print polynomial like 2x^3 - x + 5
formatPolynomial :: [Double] -> T.Text
formatPolynomial coeffs =
  let deg = length coeffs - 1
      terms = [formatTerm c (deg - i) | (i, c) <- zip [0..] coeffs, abs c > 1e-10]
  in if null terms then "0" else T.intercalate " " terms

formatTerm :: Double -> Int -> T.Text
formatTerm c 0 = T.pack $ showSigned c
formatTerm 1 1 = " + x"
formatTerm (-1) 1 = " - x"
formatTerm c 1 = T.pack (showSigned c) <> "x"
formatTerm 1 p = T.pack $ " + x^" ++ show p
formatTerm (-1) p = T.pack $ " - x^" ++ show p
formatTerm c p = T.pack (showSigned c ++ "x^" ++ show p)

showSigned :: Double -> String
showSigned x
  | x >= 0 = " + " ++ showFFloat' x
  | otherwise = " - " ++ showFFloat' (abs x)

showFFloat' :: Double -> String
showFFloat' = flip (Numeric.showFFloat (Just 4)) ""

-- | Format list of complex roots
formatRootsList :: [C.Complex Double] -> T.Text
formatRootsList roots = T.unlines $ zipWith formatRoot [1..] roots

formatRoot :: Int -> C.Complex Double -> T.Text
formatRoot i r =
  let realPart = Numeric.showFFloat (Just 4) (C.realPart r) ""
      imagPart = Numeric.showFFloat (Just 4) (C.imagPart r) ""
  in T.pack $ "r" ++ show i ++ " = " ++ realPart ++ " + " ++ imagPart ++ "i"

-- | Format roots as product: (x - r1)(x - r2)...
formatRootProduct :: [C.Complex Double] -> T.Text
formatRootProduct roots = T.concat $ map formatFactor roots

formatFactor :: C.Complex Double -> T.Text
formatFactor r =
  let realPart = Numeric.showFFloat (Just 4) (C.realPart r) ""
      imagPart = Numeric.showFFloat (Just 4) (C.imagPart r) ""
  in "(x - (" <> T.pack realPart <> " + " <> T.pack imagPart <> "i))"

-- | Main bot action
polinoeAction :: BotAction GlobalState
polinoeAction = BotAction
  { matchMsg = \_ msg -> "solve" `T.isPrefixOf` T.toLower msg
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) $ do
        let content = messageContent msg
        case parsePolynomial content of
          Nothing -> void $ restCall $ CreateMessage (messageChannelId msg)
                        "Couldn't parse polynomial. Use: solve 2x^3 - 3x + 5"
          Just coeffs -> do
            let pretty = formatPolynomial coeffs
                roots = aberthSolve coeffs
                listFormat = formatRootsList roots
                multFormat = formatRootProduct roots
                fullMsg = "Polynomial: " <> pretty <> "\n\n"
                       <> "Roots:\n" <> listFormat <> "\n"
                       <> "Factorized form:\n" <> multFormat
            void $ restCall $ CreateMessage (messageChannelId msg) fullMsg
      _ -> return ()
  }
