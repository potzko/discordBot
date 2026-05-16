#!/bin/bash
# Run the bot with a 1 GB process-wide heap cap.
# (Per-action 100 MB allocation cap is enforced inside the binary; see Main.hs.)
set -euo pipefail
exec stack exec discordBot-exe -- +RTS -M1G -RTS
