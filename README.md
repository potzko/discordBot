# discordBot

does some math stuff

### To run:
Add a `.env` file with the following content:
DISCORD_TOKEN=<your token>


### Implements:
Responds to the following commands:

- `ping` → `pong`
- `pong` → `ping`
- `fib N` → Returns the Nth Fibonacci number (supports negative indices)
- `rebase Num BaseA BaseB` → Converts `Num` from `BaseA` to `BaseB`, e.g.:
- - rebase 12 10 2 -> 1100
- - rebase 1100 2 10 -> 12
- `calc <math expression>` → Evaluates and returns the result of the expression
- `solve <polynomial>` → Parses and solves for roots.
- - Outputs:
- - The cleaned-up polynomial
- - Its roots (as complex numbers)
- - A factorized form using the roots
- `collatz N` → Shows the 3n+1 sequence, stopping time, and peak value
- `factorize N` → Prime factorization (Pollard rho-Brent, 2 s timeout). E.g.: `factorize 360` → `360 = 2^3 * 3^2 * 5`
- `choose A | B | C` → Picks one option uniformly at random
- `coinflip` → Flips a coin (heads or tails)
- `help -- math` → Longer help that explains the math behind each command (general listing is shown by `command`)
- `roll` Lable(NdK) e.g. : 
- - roll attack(2d8 electric(2d6 1d4)) →
```
Roll: (total = 24)
    attack: (total = 24)
        2d8: [4,6]

        electric: (total = 14)
            2d6: [6,4]
            1d4: [4]
```

### Passive detectors:
These don't need a command — they scan every message and respond if a pattern is found:

- **Factorial** — replies with the value of any `N!` it finds (e.g., `12!`)
- **Number properties** — for every integer in the message, lists which of *prime*,
  *happy*, *perfect*, *abundant* hold. Each property lives in its own file under
  `src/Bot/NumberProperties/` — drop a new file there and register it in `allProperties`
  to add another check.
- **Palindrome** — reports when a whole message (after stripping non-alphanumerics
  and lowercasing) reads the same forwards and backwards

