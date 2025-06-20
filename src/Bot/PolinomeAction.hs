{-# LANGUAGE OverloadedStrings #-}
module Bot.PolinomeAction (polinoeAction) where

import Discord
import Discord.Types
import Discord.Requests
import Control.Monad (void, unless)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Complex (Complex((:+)), cis, realPart, imagPart, magnitude)
import Numeric (showFFloat)
import Bot.Types

-- | Parse "solve 2x^7 + 3x^3 - x + 1" into coefficients list
parsePolynomial :: T.Text -> Maybe [Double]
parsePolynomial txt =
  let cleaned = T.replace "-" "+-" $ T.filter (/= ' ') $ T.toLower txt
      withoutSolve = T.stripPrefix "solve" cleaned
  in case withoutSolve of
       Nothing -> Nothing
       Just body ->
         let terms = map T.unpack . filter (not . T.null) $ T.splitOn "+" body
             coeffList = map parseTerm terms
         in if any (== Nothing) coeffList
              then Nothing
              else Just (collapseCoeffs (map (\(Just (c, p)) -> (c, p)) coeffList))

-- | Parse each term like "2x^3", "-x", "5" into (coefficient, power)
parseTerm :: String -> Maybe (Double, Int)
parseTerm term
  | 'x' `elem` term = case span (/= 'x') term of
      ("", xs)      -> Just (1, extractPower xs)
      ("-", xs)     -> Just (-1, extractPower xs)
      (c, xs)       -> do coeff <- readMaybe c
                          return (coeff, extractPower xs)
  | otherwise = do num <- readMaybe term
                   return (num, 0)

extractPower :: String -> Int
extractPower str = case str of
  "x"       -> 1
  "x^"      -> 1
  ('x':'^':p) -> case reads p of
                  [(n, "")] -> n
                  _         -> 1
  _         -> 1

-- | Collapse a list of (coefficient, power) into a full coefficient list
collapseCoeffs :: [(Double, Int)] -> [Double]
collapseCoeffs terms =
  let deg = maximum (map snd terms)
      getCoeff p = sum [c | (c, p') <- terms, p' == p]
  in [getCoeff p | p <- [deg, deg-1..0]]

-- | Fallback readMaybe implementation
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of [(x, "")] -> Just x; _ -> Nothing

-- Polynomial evaluation
evalPoly :: [Double] -> Complex Double -> Complex Double
evalPoly coeffs z = sum $ zipWith (\c i -> (c :+ 0) * z^^i) (reverse coeffs) [0..]

evalPolyDeriv :: [Double] -> Complex Double -> Complex Double
evalPolyDeriv coeffs z =
  let deg = length coeffs - 1
      derivCoeffs = zipWith (*) (map fromIntegral [deg, deg-1 .. 1]) (init coeffs)
  in evalPoly derivCoeffs z

-- Initial guesses based on uniform angle and radius
initialGuesses :: Int -> [Complex Double]
initialGuesses deg =
  let radius = 1.5
      angles = [2 * pi * fromIntegral k / fromIntegral deg | k <- [0..deg-1]]
  in [radius * cis theta | theta <- angles]

-- Aberth iteration
iterateAberth :: [Double] -> [Complex Double] -> Int -> [Complex Double]
iterateAberth _ guesses 0 = guesses
iterateAberth coeffs guesses n =
  let update j zj =
        let fz = evalPoly coeffs zj
            f'z = evalPolyDeriv coeffs zj
            sumTerms = sum [1 / (zj - zk) | (k, zk) <- zip [0..] guesses, k /= j]
        in zj - fz / f'z / (1 - (fz / f'z) * sumTerms)
      newGuesses = zipWith update [0..] guesses
  in iterateAberth coeffs newGuesses (n - 1)

-- Format complex root nicely
formatRoot :: Complex Double -> T.Text
formatRoot z =
  let re = realPart z
      im = imagPart z
      showFFloat' x = T.pack (showFFloat (Just 4) x "")
  in "r = " <> showFFloat' re <> " + " <> showFFloat' im <> "j"

-- Convert to factorized form
formatFactorized :: [Complex Double] -> T.Text
formatFactorized roots =
  T.concat $ map (\z -> "(x - (" <> formatRoot z <> "))") roots

-- Human-readable polynomial display
displayPoly :: [Double] -> T.Text
displayPoly coeffs =
  let deg = length coeffs - 1
      term c p
        | abs c < 1e-8 = ""
        | otherwise =
            let sign = if c >= 0 then " + " else " - "
                coeffText = T.pack $ showFFloat (Just 4) (abs c) ""
                power = case p of
                          0 -> ""
                          1 -> "x"
                          _ -> "x^" <> T.pack (show p)
            in sign <> coeffText <> power
      terms = zipWith term coeffs [deg, deg-1 .. 0]
  in T.strip $ T.concat terms

-- Final solver interface
aberthSolve :: [Double] -> [T.Text]
aberthSolve coeffs =
  let degree = length coeffs - 1
      guesses = initialGuesses degree
      roots = iterateAberth coeffs guesses 50
  in  [ "Polynomial: " <> displayPoly coeffs
      , ""
      , "Roots:" ] ++
      map formatRoot roots ++
      [ ""
      , "Factorized form:"
      , formatFactorized roots
      ]

-- Discord command handler
polinoeAction :: BotAction GlobalState
polinoeAction = BotAction
  { matchMsg = \_ msg -> "solve" `T.isPrefixOf` T.toLower msg
  , runAction = \event _ -> case event of
      MessageCreate msg -> unless (userIsBot (messageAuthor msg)) $ do
        let content = messageContent msg
        case parsePolynomial content of
          Nothing -> void $ restCall $ CreateMessage (messageChannelId msg)
                        "Couldn't parse polynomial. Try: `solve x^3 + 2x - 1`"
          Just coeffs -> do
            let outputLines = aberthSolve coeffs
            void $ restCall $ CreateMessage (messageChannelId msg)
                   (T.intercalate "\n" outputLines)
      _ -> return ()
  }
