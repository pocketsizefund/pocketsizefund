---
argument-hint: [message]
allowed-tools: Bash(*), Git(*), Pulumi(*)
description: Manage infra (up & down)
argument-hint: up | down
model: claude-3-5-haiku-20241022
---

# Task
Interpret the first token of "$ARGUMENTS" as the action (up|down). Default to up.

Then:
- If `up`, run: `flox activate --mask infrastructure up`
- If `down`, run: `flox activate -- mask infrastructure down`
