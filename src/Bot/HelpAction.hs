{-# LANGUAGE OverloadedStrings #-}
module Bot.HelpAction (helpAction, mathHelpAction) where

import Discord.Types
import Control.Monad (void)
import qualified Data.Text as T
import Bot.Util (sendMessageSafe)

import Bot.Types

helpText :: T.Text
helpText = T.intercalate "\n"
  [ "**Available Commands:**"
  , ""
  , "**ping** - Responds with `Pong!`"
  , "**pong** - Responds with `Ping!`"
  , ""
  , "**roll** `Label(NdK)` - Roll dice with optional labels and nesting."
  , "  Example: `roll attack(2d8 electric(2d6 1d4))`"
  , ""
  , "**calc** `<expression>` - Evaluate a math expression. Supports +, -, *, /, ^, %, and bitwise ops."
  , "  Example: `calc 1 + 2 * (3 + 4)`"
  , ""
  , "**solve** `<polynomial>` - Parses a polynomial and finds its roots."
  , "  Example: `solve x^2 - 1`"
  , ""
  , "**fib** `N` - Returns the Nth Fibonacci number (supports negative indices)."
  , "  Example: `fib 10`"
  , ""
  , "**rebase** `Num BaseA BaseB` - Converts a number from one base to another."
  , "  Example: `rebase 12 10 2` -> `1100`"
  , ""
  , "**collatz** `N` - Shows the 3n+1 sequence and stopping time."
  , "  Example: `collatz 27`"
  , ""
  , "**factorize** `N` - Prime factorization (Pollard rho-Brent, 2s timeout)."
  , "  Example: `factorize 360` -> `360 = 2^3 * 3^2 * 5`"
  , ""
  , "**choose** `A | B | C` - Picks one option uniformly at random."
  , "  Example: `choose pizza | sushi | tacos`"
  , ""
  , "**coinflip** - Flips a coin (heads or tails)."
  , ""
  , "**help -- math** - Longer help with the math behind each command."
  , ""
  , "**Detectors:**"
  , "These aren't commands — they automatically scan every message and respond when they spot a pattern."
  , ""
  , "**Factorial** - Detects `N!` in messages and computes the result."
  , "  Example: `I wonder what 12! is`"
  , ""
  , "**Number properties** - For each integer in a message, lists which of"
  , "  *prime*, *happy*, *perfect*, *abundant*, *narcissistic*, *fibonacci*, *triangular*"
  , "  it satisfies."
  , "  Example: `is 17 a big number, and what about 6?`"
  , ""
  , "**Palindrome** - Detects when a whole message reads the same forwards and backwards."
  , "  Example: `A man a plan a canal Panama`"
  ]

mathHelpText :: T.Text
mathHelpText = T.intercalate "\n"
  [ "**Math behind the commands:**"
  , ""
  , "**roll** - Uniform random integers via `randomRIO`. Independent rolls per die;"
  , "  nested groups sum their children's totals."
  , ""
  , "**calc** - Recursive descent parser → AST → evaluator. Supports `+ - * / ^ %`"
  , "  and bitwise ops, with standard precedence."
  , ""
  , "**solve** - Univariate polynomial in `x`. Closed-form roots up to degree 4"
  , "  (linear / quadratic / Cardano / Ferrari). Roots returned as complex numbers."
  , ""
  , "**fib** - Fast doubling identity:"
  , "  `F(2k) = F(k) * (2*F(k+1) - F(k))`, `F(2k+1) = F(k+1)^2 + F(k)^2`."
  , "  O(log N) `Integer` multiplies. Negative `n` via `F(-n) = (-1)^(n+1) * F(n)`."
  , ""
  , "**rebase** - Interpret digits in base A, then divmod into base B."
  , "  Digits 0-9 then a-z → max base 36."
  , ""
  , "**collatz** - Direct 3n+1 simulation. Stopping time is iterations to reach 1;"
  , "  peak is the largest value seen. (Convergence is conjectural — verified up"
  , "  to ~2^68 in practice.)"
  , ""
  , "**factorize** - Trial-divide by every prime up to 10^4 first (so things like"
  , "  `2^100 * p * q` peel off the 2's even though n is huge). Whatever remains"
  , "  goes through Miller-Rabin; if composite, Pollard rho with Brent's"
  , "  batched-gcd improvement (heuristic O(n^(1/4)) per factor). 2 s internal timeout."
  , ""
  , "**Detectors:**"
  , ""
  , "**Factorial** - Exact value when small; for large N, estimates digit count via"
  , "  Kamenetsky: `digits(N!) ≈ ⌊N * log10(N/e) + log10(2πN)/2⌋ + 1`."
  , ""
  , "**Number properties:**"
  , "  • *prime* — Miller-Rabin (deterministic for small `n`; 5 random witnesses"
  , "    + Fermat base-2 prefilter for large `n`)."
  , "  • *happy* — iterate digit-square-sum; converges to 1 (happy) or hits the"
  , "    cycle 4→16→37→58→89→145→42→20→4 (unhappy)."
  , "  • *perfect* — σ(n) − n == n, with σ via O(√n) divisor-pair scan."
  , "  • *abundant* — σ(n) − n > n."
  , "  • *narcissistic* — n equals the sum of its digits each raised to the digit"
  , "    count. e.g. `153 = 1^3 + 5^3 + 3^3`."
  , "  • *fibonacci* — walk `F_0, F_1, F_2, ...` until we hit n; reported as"
  , "    `fibonacci(k)` where `k` is the index."
  , "  • *triangular* — `8n + 1` is a perfect square (so n = k(k+1)/2 for some k)."
  , ""
  , "**Palindrome** - Strip non-alphanumerics, lowercase, compare to reverse. O(N)."
  ]

helpAction :: BotAction GlobalState
helpAction = BotAction
  { botActionName = "Help"
  , matchMsg = \_ txt -> "command" `T.isPrefixOf` txt
  , runAction = \event _ -> case event of
      MessageCreate msg ->
        void $ sendMessageSafe "Help" (messageChannelId msg) helpText
      _ -> return ()
  , actionMemBudget = Nothing
  }

mathHelpAction :: BotAction GlobalState
mathHelpAction = BotAction
  { botActionName = "Help:Math"
  , matchMsg = \_ txt -> "help -- math" `T.isPrefixOf` T.toLower (T.strip txt)
  , runAction = \event _ -> case event of
      MessageCreate msg ->
        void $ sendMessageSafe "Help:Math" (messageChannelId msg) mathHelpText
      _ -> return ()
  , actionMemBudget = Nothing
  }
