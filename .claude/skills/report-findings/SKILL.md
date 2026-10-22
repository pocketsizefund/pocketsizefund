---
name: report-findings
description: How to present a research or data result in the oscmcompany/fund repository — a study measurement, a declaration needing decisions, or the progress of a data fetch, thaw or clean. Load before writing any such reply in that repository; not for ordinary coding work there or anywhere else.
---

# Report findings

These rules shape in-conversation replies, and the only files they touch are `glossary.md` and `measurements.md`.
Ad-hoc Markdown notes saved to a study's `scratch/<study>/notes/` stay as they are. Where these rules conflict with
the global length rules, these win, but every line still has to earn its place: enrich, never repeat.

## Shape of a result reply

1. **Header line**, first, inside the opening code block or on its own line:
   `<study> · <task or step> · <what was measured>    verdict: PASS | FAIL | MIXED | PENDING | INFO    needs you: no`
   When a decision is required, write `needs you: yes — <the ask in a few words>`, so the user can triage several
   reports at a glance and open only the ones that ask for something.
2. **One sentence** on what the work set out to find.
3. **A chart** wherever two or more numbers are compared (see below).
4. **Tables** where several items get verdicts, each followed by its legend.
5. **At most one highlighted conclusion line**, `→ <conclusion>`, and never a prose restatement of the table.
6. **Decisions**, when needed: numbered options with the recommended one first and one sentence of why each, then a
   direct question.
7. **A progress bar** when the reply closes a task or a run, or reports a data job (see below).

## Every number is scaled

- Put each number beside its target or its baseline (the null, the trivial arm, the cost breakeven) and a verdict
  word: `0.02 against a breakeven of 0.04 — short`. A bare number is never acceptable.
- Quote skill against a named baseline, and print the population and the undefined share beside it.
- When a procedure is abstract, restate it as the smallest concrete example ("200 names, buy the top 20, short the
  bottom 20, equal dollars").

## Terms are explained every time

- Explain each technical term every time it appears in a new reply, inline in a parenthesis or as a footnote
  (`IC¹` with `¹ IC: …` under the chart or table), until the user asks to cut back.
- Take the explanation and the "good looks like" line from `glossary.md` beside this file, preferring the fund's own
  value from the untracked `measurements.md` when it holds one; for a term not listed, explain it and add it to the
  glossary. When a study measures a new reference value, record it with the date in `measurements.md`, never in the
  glossary, which is public.

## Tables carry legends

Under every table, one line per column that is not self-evident: `column — what it holds; good is …`. Cells carry
their units (`bp`, `%`, `sessions`). Prefer full words in column names over abbreviations.

## Charts

Draw in a fenced code block so the alignment holds. The staple is a horizontal bar chart against a target:

```text
Predictive strength (IC¹), model against what it must clear
  shuffled null  ▏ 0.00            ← what pure noise scores
  model          ██████████░░░░░░░░░░  0.02
  breakeven      ████████████████████  0.04   ← pays its trading cost
¹ IC: how well the forecast ranks tomorrow's winners above the losers;
  0 is noise, 0.04 pays for itself in this example, above 0.10 is rare.
```

- Scale bars to the largest value at 20 characters; `█` for the value, `░` for the remainder up to the target.
- Annotate the reference rows with `←` notes saying what each one means.
- For signed values (net bp per trade), centre an axis at zero:

```text
Net return per trade (bp), after costs                  (illustrative)
                                   loss┼gain
  arm A                     ███████████┤                       -1.40
  arm B            ████████████████████┤                       -2.60
  arm C                                ├███████                +0.90
```

- Across runs or seeds, one row per run in a fixed order; within a run, one row per arm. Other forms (a sparkline
  `▁▂▃▅▇`, a small grid) are fine when the comparison calls for them.

## Progress bars

Use them for countable work: plan tasks, study steps, sessions thawed or fetched, rows cleaned.

```text
Study plan     ▓▓▓░░░░░░  3/9 tasks · next: task 4, the null check
Thaw           ▓▓▓▓▓▓▓░░  412/530 sessions · ~40 min left
```

Include one when a task or run finishes, or when reporting on a long data job, and not in every reply.

## Privacy

These replies are private. Nothing from them (a study's subject, its results, its numbers) goes into commits, pull
requests or fixtures.
