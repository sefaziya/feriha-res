#!/usr/bin/env bash
# RES Monitor — tmux headless Shiny dashboard
set -euo pipefail
PROJECT_DIR="$(cd "$(dirname "$0")" && pwd)"
SESSION_NAME="${RES_MONITOR_SESSION:-RES_MONITOR}"

if tmux has-session -t "$SESSION_NAME" 2>/dev/null; then
  echo "Session '$SESSION_NAME' already exists. Attach: tmux attach -t $SESSION_NAME"
  exit 1
fi

tmux new-session -d -s "$SESSION_NAME" "cd '$PROJECT_DIR' && Rscript 07_Monitor/run_monitor.R"
echo "RES Monitor started in tmux session: $SESSION_NAME"
echo "Attach: tmux attach -t $SESSION_NAME"
echo "Default URL (with SSH tunnel): http://127.0.0.1:8788"
