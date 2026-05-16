# docs_llm

Guide for LLMs writing code in this Haskell Discord bot. Read this before
adding a command, detector, property, or math primitive.

## Project layout

```
src/
├── Main.hs                          -- entry point: builds the actions list, dispatches events
├── Lib.hs                           -- (mostly empty)
└── Bot/
    ├── Types.hs                     -- BotAction record, GlobalState, MessagePredicate / MessageAction
    ├── Util.hs                      -- sendMessageSafe (use this for ALL replies — caps at 2000 chars)
    ├── HelpAction.hs                -- helpAction ("command") and mathHelpAction ("help -- math")
    ├── PingPong.hs                  -- pingAction / pongAction
    ├── Dice.hs                      -- "roll …"
    ├── MathExpr.hs                  -- "calc …"
    ├── PolinomeAction.hs            -- "solve …"
    ├── FibAction.hs                 -- "fib N"
    ├── RebaseAction.hs              -- "rebase N from to"
    ├── Collatz.hs                   -- "collatz N"
    ├── Choose.hs                    -- "choose A | B | C", coinflip
    ├── Factorize.hs                 -- "factorize N" (action wrapper)
    ├── Factorization.hs             -- pure Pollard rho-Brent / hybrid factorization
    ├── Primality.hs                 -- pure Miller-Rabin (`isProbablyPrime`)
    ├── FactorialDetector.hs         -- detects "N!" in messages
    ├── PalindromeDetector.hs        -- detects whole-message palindromes (O(N))
    ├── NumberProperties.hs          -- aggregator action; runs every property per number
    └── NumberProperties/
        ├── Types.hs                 -- the Property record
        ├── Internal.hs              -- shared helpers (e.g. properDivisorSum)
        ├── Prime.hs                 -- delegates to Bot.Primality
        ├── Happy.hs
        ├── Perfect.hs
        └── Abundant.hs
```

`package.yaml` is the hpack source for `discordBot.cabal`. Library modules live
under `src/`, source-dir is auto-discovered. The `.cabal` file *does* list every
module — when you add a file, add the module to **both** `library.exposed-modules`
and `executable.other-modules` in `discordBot.cabal`.

## The BotAction pattern

Every command and passive detector is a `BotAction GlobalState`:

```haskell
data BotAction state = BotAction
  { botActionName    :: String                  -- shown in logs / timeout messages
  , matchMsg         :: state -> T.Text -> Bool -- runs against every message
  , runAction        :: Event -> state -> DiscordHandler ()
  , actionMemBudget  :: Maybe Int64             -- per-action allocation override
  }
```

`matchMsg` is **pure** and runs on every incoming message — keep it cheap (a
prefix check, a digit scan, or similar). `runAction` only fires when `matchMsg`
returns True.

`actionMemBudget` is `Nothing` for almost everything (the global 100 MB default
applies). Set it to `Just (N * 1024 * 1024)` if your action legitimately needs
more headroom — `Bot.Factorize.factorizeAction` requests 500 MB because Pollard
rho on a hard semiprime allocates faster than 100 MB / 2 s.

## Dispatch (the most important thing to internalize)

`Main.hs` walks the `actions` list **in order** and runs the **first** action
whose `matchMsg` returns True. There is no fallthrough.

Consequences:

- **Order matters.** Place narrow-trigger commands (`fib `, `roll `, `factorize `)
  before broad detectors. The current order in `Main.hs` is: math help, general
  help, explicit commands, text-based detectors, number-based detectors.
- **If two detectors would both fire on the same message, only one runs.** If
  you need both, **combine them in a single detector**. See `Bot.NumberProperties`:
  every property is a separate file, but one action fires and reports them all.
- A 5-second timeout wraps every action via `runWithTimeoutWithLimit` in
  `Main.hs`. If you need a tighter cap (e.g., factorization at 2 s), nest a
  `System.Timeout.timeout` inside `runAction` — the outer 5 s is a harmless
  fallback.

## Adding a new explicit command

1. Create `src/Bot/Foo.hs`:

   ```haskell
   {-# LANGUAGE OverloadedStrings #-}
   module Bot.Foo (fooAction) where

   import Discord.Types
   import Control.Monad (void, unless)
   import qualified Data.Text as T
   import Bot.Types
   import Bot.Util (sendMessageSafe)

   fooAction :: BotAction GlobalState
   fooAction = BotAction
     { botActionName = "Foo"
     , matchMsg = \_ txt -> "foo " `T.isPrefixOf` T.toLower txt
     , runAction = \event _ -> case event of
         MessageCreate msg -> unless (userIsBot (messageAuthor msg)) $ do
           let arg = T.strip $ T.dropWhile (/= ' ') (messageContent msg)
           void $ sendMessageSafe "Foo" (messageChannelId msg) ("got: " <> arg)
         _ -> return ()
     }
   ```

2. Import + register in `src/Main.hs` (in the `actions` list — pick the right
   group based on what `matchMsg` looks like).
3. Add `Bot.Foo` to **both** `exposed-modules` (library) and `other-modules`
   (executable) in `discordBot.cabal`.
4. Update `Bot.HelpAction.helpText` (one entry) and, if relevant, the
   `mathHelpText` for the math explanation.
5. Update `README.md`.

## Adding a passive detector

Same recipe as a command, but `matchMsg` checks a content pattern (not a prefix)
and `runAction` produces a reply only when the pattern fires.

Place it in the right slot in `Main.hs`'s actions list:
- text-based (palindrome, factorial detector) goes after explicit commands
- number-based (`numberPropertyAction`) goes last, since it'll match almost any
  message containing an integer

## Adding a new number property

Properties are deliberately decoupled from dispatch. Just:

1. Create `src/Bot/NumberProperties/Foo.hs`. For a plain yes/no property, use
   `simpleProperty`:

   ```haskell
   {-# LANGUAGE OverloadedStrings #-}
   module Bot.NumberProperties.Foo (property) where

   import Bot.NumberProperties.Types (Property, simpleProperty)

   isFoo :: Integer -> Bool
   isFoo n = ...

   property :: Property
   property = simpleProperty "foo" (const True) isFoo
   --                       name   applies      check
   ```

   For a property whose label depends on the input (e.g. Fibonacci returns
   `fibonacci(k)` with the index), build the `Property` record directly:

   ```haskell
   {-# LANGUAGE OverloadedStrings #-}
   module Bot.NumberProperties.Foo (property) where

   import qualified Data.Text as T
   import Bot.NumberProperties.Types (Property(..))

   property :: Property
   property = Property
     { propertyName    = "foo"
     , propertyApplies = const True
     , propertyLabel   = \n -> case fooIndex n of
         Just k  -> Just (T.pack ("foo(" ++ show k ++ ")"))
         Nothing -> Nothing
     }
   ```

2. Reuse `Bot.NumberProperties.Internal.{properDivisorSum, isqrt, isPerfectSquare}`
   for divisor- or square-root-based checks.
3. Add `Bot.NumberProperties.Foo` to `discordBot.cabal`.
4. Register in `Bot.NumberProperties.allProperties`.

No `Main.hs` changes. No dispatch changes. The existing `numberPropertyAction`
runs every registered property per detected number and concatenates labels.

### About `propertyApplies`

Each property declares a fast guard saying "I'm tractable for this input."
The aggregator in `Bot.NumberProperties` runs `propertyApplies` first and only
calls `propertyLabel` when the guard returns True. The point: a single slow
property (e.g., O(√n) divisor sum on a 20-digit number) can no longer starve
the cheap properties (Miller-Rabin) by blowing the per-message timeout.

Rules of thumb:

- **`const True`** — Miller-Rabin, digit-iteration, anything that's fast at any
  size. Used by `Prime` and `Happy`.
- **`(<= 10^12)`** — O(√n) checks. √(10¹²) ≈ 10⁶ iterations, sub-ms in Haskell.
  Used by `Perfect` and `Abundant`.
- **A more specific predicate** — anything where the property only makes sense
  for a particular shape of input (e.g., "is Fibonacci": fast lookup by index,
  but you'd want a guard that rules out negative or trivially-small inputs).

`propertyApplies` is meant to be **constant-time-ish**. Don't put a slow check
inside it; that defeats the point. If your test is itself the expensive thing,
either lower the cap on the input size or restructure the algorithm.

## Conventions

- Pragmas at the top of every module: `{-# LANGUAGE OverloadedStrings #-}`.
  Add `{-# LANGUAGE BlockArguments #-}` if you write `do` after `unless` /
  `when` without `$`. Add `{-# LANGUAGE BangPatterns #-}` only when you actually
  need strictness annotations (number-crunching loops).
- **Always declare an explicit export list** (`module Bot.Foo (fooAction) where`).
  `-Wmissing-export-lists` is on.
- **Always send replies via `Bot.Util.sendMessageSafe`** — never `restCall
  (CreateMessage …)` directly. `sendMessageSafe` enforces Discord's 2000-char
  cap and logs the over-length case instead of failing.
- **Filter bot messages**: wrap `runAction` body with
  `unless (userIsBot (messageAuthor msg)) $ …` to avoid feedback loops.
- Use `Data.Text` (`T.Text`), not `String`. `T.pack` / `T.unpack` only at
  boundaries (e.g., reading numbers, `Text.Read.readMaybe`).
- Prefer **pure functions in their own module**, with the bot action being a
  thin Discord wrapper. See `Bot.Factorization` (pure) + `Bot.Factorize` (the
  Discord wrapper). Same split for `Bot.Primality` vs the property module that
  uses it.

## Build

This project is built with **stack**, not cabal:

```
stack build
```

(Plain `cabal build` currently breaks on this machine due to an unrelated
`req` / `tls` dependency-resolver issue — use stack.)

## Reusable utilities

- `Bot.Primality.isProbablyPrime :: Integer -> Bool` — Miller-Rabin with
  small-prime sieve prefilter. Use this instead of writing your own primality
  check.
- `Bot.Factorization.factorize :: Integer -> [(Integer, Int)]` — full prime
  decomposition with multiplicities, sorted by base.
- `Bot.Factorization.pollardBrent :: Integer -> Maybe Integer` — single
  non-trivial-factor extraction. Caller is expected to handle small primes /
  primality first.
- `Bot.NumberProperties.Internal.properDivisorSum :: Integer -> Integer` —
  σ(n) − n in O(√n). Reuse for any divisor-based property.

## Don'ts

- Don't import `Discord.Requests` or `Discord` for sending messages — go through
  `sendMessageSafe`.
- Don't reach for emojis. The codebase is mostly emoji-free; ask before adding any.
- Don't add caches / `IORef` / `unsafePerformIO` unless the workload genuinely
  needs it. Most number-theoretic ops on Discord-scale inputs are microseconds.
- Don't forget to register new modules in `discordBot.cabal` — `stack build`
  won't find them otherwise.
- Don't write a long expensive `matchMsg`. Every message in every channel runs
  every `matchMsg` until one returns True.
- Don't extend the global 5 s timeout. If your action needs to be tighter,
  nest your own `System.Timeout.timeout` inside `runAction`.
