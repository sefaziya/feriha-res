#!/usr/bin/env bash
# RES — tmux headless runner
set -euo pipefail
PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
SESSION_NAME="${RES_SESSION:-RES}"

if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
  echo "Session '$SESSION_NAME' already exists. Attach: tmux attach -t $SESSION_NAME"
  exit 1
fi

tmux new-session -d -s "$SESSION_NAME" "cd '$PROJECT_DIR' && Rscript 02_Core/run_engine.R"
echo "RES engine started in tmux session: $SESSION_NAME"
echo "Attach: tmux attach -t $SESSION_NAME"
