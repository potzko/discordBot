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
