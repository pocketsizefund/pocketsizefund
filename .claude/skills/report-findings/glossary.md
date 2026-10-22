# Glossary

Each term is followed by what it is in plain words, then **good**, a general rule of thumb for what a good value looks
like. The fund's own measured values live in the untracked `measurements.md` beside this file and are quoted ahead of
these. Add terms here as they appear, and record a value a study measures in `measurements.md`, never here.

## Statistics and evaluation

- **Information coefficient (IC)** — the rank correlation, within one session, between what a model forecast for each
  name and what each name then did; averaged over sessions. **Good:** 0 is noise; what pays depends on the cost a
  trade must clear; above 0.10 is rare anywhere.
- **AUC (area under the ROC curve)** — the chance a model scores a random positive case above a random negative one.
  **Good:** 0.5 is a coin; 0.7+ is useful.
- **Basis point (bp)** — one hundredth of a percent; 10bp on $10,000 is $10. **Good:** depends on the cost it must
  clear; a round trip in liquid names costs on the order of 10bp.
- **Standard error (SE)** — how far an estimate would wobble if the experiment were rerun on fresh data.
  **Good:** an effect is worth reading when it is at least 2 SE from zero (or from its baseline).
- **Session-clustered standard error** — a standard error that treats each trading day as one observation, because
  trades on the same day move together. **Good:** this is the honest one; 10,000 trades over 5 sessions is closer to 5
  observations than to 10,000.
- **t-statistic (t)** — an estimate divided by its standard error. **Good:** |t| ≥ 2 clears the usual bar.
- **Effective sample size** — how many independent observations the data is really worth after correlation.
  **Good:** report it beside the row count; the row count flatters.
- **Null / permutation null / shuffled-label null** — the same pipeline run on data where the effect has been
  deliberately destroyed (labels shuffled), so whatever it reports is the machinery's own bias. **Good:** near zero,
  and the real reading must clear it by a margin.
- **Oracle** — an arm that is handed the true answer, giving the ceiling no model can beat. **Good:** shows how much
  room there is; a model near the oracle is suspicious.
- **Trivial baseline / trivial arm** — the simplest thing that could be done instead (do nothing, predict the
  average). **Good:** the model must beat it, and skill is quoted against it.
- **Hash coin** — a deterministic pseudo-random choice derived from a hash (of a name, a session), so a split is random
  but reproducible. **Good:** not a quality measure; it guarantees no one chose the split.
- **Ridge regression** — a linear model with a penalty that shrinks coefficients toward zero to avoid overfitting.
  **Good:** a strong cheap baseline; a fancier model must beat it.
- **Gradient-boosted trees (booster)** — many small decision trees added one at a time, each correcting the last.
  **Good:** beats ridge when the relationship is non-linear; not by default.
- **Rounds / early stopping** — how many trees a booster adds; early stopping halts once validation stops improving.
  **Good:** stopping at very few rounds means there was little to learn.
- **Magnitude calibration (slope)** — whether a forecast's size in bp matches what follows: regress the realised
  return on the forecast; a slope of 1 means "forecast 5bp, get 5bp on average". **Good:** near 1; a slope well under 1
  means the forecast is too large. Fixed by multiplying the forecast by a slope fitted on other data.
- **Calibration** — whether stated probabilities come true at their stated rates (70% forecasts happen 70% of the
  time). **Good:** observed matches forecast within noise in every band.
- **Quantile / decile / bands (0.1, 0.9)** — cut points of a distribution; 0.1 is the value 10% of outcomes fall
  below. A 0.1–0.9 band should contain 80% of outcomes. **Good:** ~10% land below the 0.1 forecast and ~10% above
  the 0.9 one.
- **Median** — the middle value; half above, half below. **Good:** resistant to outliers, unlike the mean.
- **Rank cut** — selecting names by rank (top 10%) rather than by a fixed threshold. **Good:** ranks stay stable as
  levels drift.
- **Folds / walk-forward** — splitting time into blocks and always training on the past and testing on the next
  block. **Good:** the only honest test for a time series; results should hold across folds.
- **Construction / validation / confirm / kept-out sessions** — the data used to build, to tune, to check once, and
  never to touch until the end. **Good:** a result counts only on sessions the model never saw.
- **Split-sample (halves) test** — compare the effect in the first and second half and test whether they differ.
  **Good:** no significant difference; "both halves positive" is not the test.
- **Mutual information (as share of entropy)** — how much knowing one variable reduces uncertainty about another,
  as a fraction of the total uncertainty. **Good:** compare against the permutation null; a few percent is typical.
- **Undefined share** — the fraction of inputs where an estimate could not be computed. **Good:** small and always
  reported; 40% undefined means the estimate describes a different population.
- **Variance across seeds** — how much a result changes when only the random seed changes. **Good:** some variation;
  suspiciously low variance means the data's shape pins the metric.
- **Sharpe ratio** — average return divided by its volatility, annualised. **Good:** above 1 is good, above 2 is
  excellent and rare after costs.
- **Sortino ratio** — Sharpe that penalises only downside volatility. **Good:** read like Sharpe, usually higher.
- **Grid / off-grid evaluation** — scoring a model on the fixed set of decision times it was designed for (grid)
  versus times between them (off-grid). **Good:** off-grid close to grid; a large drop means it learned the grid.

## Market microstructure

- **Spread / half-spread** — the gap between the best price to sell (bid) and to buy (ask); crossing it once costs
  half of it. **Good:** under 2bp is tight, and few names quote that tight.
- **Touch** — the best bid and best ask, the top of the order book. Trading "at the touch" means at those prices.
- **Tick** — the smallest price increment (one cent for most stocks), or one market event (a trade or quote update).
- **Microprice** — the mid-price nudged toward the side with less size queued, a better guess of the next price.
- **Order-flow imbalance** — net buying pressure from changes at the touch: size added on the bid minus the ask.
  **Good:** positive tends to precede up-moves over seconds to minutes.
- **Quote imbalance** — bid size against ask size at the touch, from −1 (all ask) to +1 (all bid).
- **Buy-initiated / sell-initiated** — whether a trade happened because a buyer crossed to the ask (aggressive buy)
  or a seller hit the bid.
- **Sweep / sweep imbalance / signed sweep volume** — an aggressive order that walks through several price levels at
  once; the imbalance is buy sweeps minus sell sweeps, and signed volume weights each by size.
- **Queue** — the line of resting orders at one price; an order fills only after those ahead of it.
- **Resting vs crossing** — resting waits in the book to be filled (earns the spread, may not fill); crossing takes
  the other side immediately (pays the spread, always fills).
- **Fill rate** — the share of resting orders that actually fill. **Good:** depends on the strategy; read with
  adverse selection.
- **Adverse selection** — the orders that do fill are disproportionately the ones you wish had not, because someone
  better informed traded against you.
- **Conservative / optimistic fill bound** — the pessimistic and generous assumptions about whether a simulated
  resting order would have filled. **Good:** a result must survive the conservative bound.
- **Cross-name flow** — order flow in other, related names used to forecast this one.
- **Roll measure** — a spread estimated from how prices bounce between trades. **Good:** a rough check only, never a
  substitute for quoted spreads.
- **Bounce (bid-ask bounce)** — apparent reversal caused by trades alternating between bid and ask, not by any real
  price change. **Good:** a reversal that vanishes when one bar is skipped was bounce.
- **NBBO / SIP** — the national best bid and offer, as published by the official consolidated feed (the SIP).
- **Opening and closing auction** — single-price crosses at the open and close where much of the day's volume trades.

## Portfolio and returns

- **Gross / net** — return before and after trading costs. **Good:** only net pays.
- **Cost breakeven** — the forecast skill (or gross return) at which a strategy exactly covers its costs.
  **Good:** the bar every result must clear, not zero.
- **Net bp per trade / per session** — average profit after costs per trade, or per trading day. **Good:** positive
  under the conservative fill bound and at least 2 SE from zero.
- **Long-short (top/bottom decile) portfolio** — buy the names ranked highest, short those ranked lowest, equal
  dollars each side, so the market's direction cancels.
- **Market-net target** — a name's return minus the market's, so the model predicts the stock and not the index.
- **Volatility-scaled target** — a return divided by the name's typical volatility, so quiet and wild stocks count
  alike.
- **Beta-neutral** — sized so the portfolio's exposure to the market's moves is zero.
- **Cost-gated** — a trade is taken only when its forecast exceeds its expected cost.
- **SEC fee / TAF** — small regulatory charges on sales (SEC fee) and on trades (FINRA's trading activity fee); tiny
  per trade but nonzero, unlike the zero commission.
- **Dollar volume** — shares traded times price; the honest measure of liquidity, unlike share volume.
- **Universe** — the set of names a study is run over; it is part of the answer and journaled beside it.

## Process

- **Declaration** — the study's free choices (windows, thresholds, splits) fixed in writing before any result is
  seen.
- **Returns bar** — the minimum net return a direction must show to be worth pursuing, set in advance.
- **Admission criteria** — four questions a new direction must pass before it is opened: a returns bar, a reachable
  size, an underserved gap, and a capability we hold.
- **Arm** — one variant in a comparison (the model, the null, the trivial baseline), named with its parameters.
- **Kill** — a direction declared refuted, scoped to exactly the conditions under which it failed.
