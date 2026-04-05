# Future Enhancements — Deferred from UI Polish Design (2026-04-05)

Items identified during the module-by-module UI design session that were deferred for good reasons but should not be forgotten. Organized by module, then by a general "data sources" section for items that span multiple modules.

---

## New Data Sources Needed

These unlock enhancements across multiple modules:

- **Event calendar feed** (EIA storage dates, OPEC meetings, FOMC, refinery outage reports, Treasury refunding, payrolls, contract expiry/roll dates) — would feed Term Structure, Volatility, Options Greeks, Scenarios, and Overview
- **CME CVOL / exchange-published implied vol series** — would strengthen Volatility module with official market-implied vol benchmarks
- **Bid-ask / liquidity / open interest data** — would enable liquidity stress scoring in Scenarios, hedging cost estimation in Cross-Hedge, and liquidity watchlist in Overview
- **Weather data** — would enable weather-linked overlays in Seasonality and weather-sensitivity scoring
- **LNG flows, pipeline flows, regional gas storage** — would deepen Fundamentals for gas markets
- **Position / book feed** — would enable portfolio-level Greeks aggregation, net portfolio P&L in Scenarios, and position-aware hedge recommendations

---

## Term Structure

- **Animated curve replay** — show curve evolution over time as an animation. High visual impact for presentations but low daily-use value for trading.
- **Gas-specific seasonal spreads in Term Structure** (Summer 2026 vs Winter 2026/27) — currently housed in Seasonality module. Could add a cross-link or duplicate panel.
- **Crack-linked curve metrics** — show product curves relative to crude curve structure. Currently in Co-Dynamics via spread panels.

---

## Volatility

- **Event vol monitor** — vol behavior around specific events (EIA storage, OPEC, FOMC, expiry, roll). Requires event calendar data.
- **CVOL integration** — CME's cross-asset implied vol index. Requires CME data feed.
- **Jump diagnostics for NG and TTF** — statistical detection of discontinuous price moves. Specialized econometrics (Barndorff-Nielsen bipower variation or similar).
- **Density ridge plots for return distributions** — comparing distributions across commodities or time periods. Vol cone + cross-asset box plots cover the same insight more efficiently, but ridgeline plots are visually compelling for presentations.

---

## Co-Dynamics

- **Lead-lag network graph** — directional network visualization showing which markets lead/lag. High implementation cost; beta dynamics and correlation breaks cover the same trading insight.
- **Spillover / connectedness heatmap (Diebold-Yilmaz)** — requires VAR model infrastructure. The connectedness score KPI approximates this.
- **Dynamic Conditional Correlation (DCC-GARCH)** — time-varying correlation model. Rolling correlation with confidence bands is the practical production approximation.
- **Full crack spread decomposition** — decompose crack spreads into components (crude basis, refining margin, product premium). Partially covered by spread panels.

---

## Cross-Hedge

- **Portfolio hedge optimizer with constraints** — quadratic optimization (minimize variance subject to position limits, liquidity constraints, max notional). Requires optimization infrastructure (quadprog or similar).
- **DCC-GARCH hedge ratios** — time-varying covariance-based hedge ratios. Rolling OLS with confidence bands is the practical approximation.
- **ECM / cointegration hedge ratios** — error correction model-based ratios for structurally linked pairs. OU fit on residuals approximates the same insight.
- **Hedging cost model** — full cost decomposition including margin requirements, bid-ask slippage, roll cost, and financing. Partially approximated via roll yield from term structure data.

---

## Options Greeks

- **Portfolio-level net Greeks** — aggregate Greeks across a book of positions. Requires position/book feed.
- **Event calendar with implied move** — show expected move into key events (EIA, OPEC) derived from straddle pricing. Requires event calendar data.
- **Calendar spread option analytics** — Greeks and payoffs for calendar spread options specifically. Specialized, lower priority.
- **Greeks mapped to tradable events** — connect gamma cliffs and vega concentration to specific upcoming events. Requires event calendar.

---

## Seasonality

- **Weather-linked overlays** — HDD/CDD data overlaid on seasonal price patterns for NG and products. Requires weather data source.
- **Inventory-normalized seasonality** — seasonal patterns adjusted for current inventory levels (e.g., "given current storage, where should price be seasonally?"). Requires deeper EIA integration.
- **Treasury seasonality** — issuance/refunding cycle effects on rates. Weak signal, low priority.

---

## Scenarios

- **Full portfolio P&L under stress** — requires position feed. Currently per-contract.
- **Liquidity stress score** — model how liquidity dries up under stress. Requires bid-ask and volume data.
- **Real-time event-triggered scenarios** — automatically suggest relevant scenarios based on upcoming events. Requires event calendar feed.
- **Copula-based joint simulation** — model non-linear dependence structure between commodities under stress (beyond correlation). Specialized econometrics.

---

## Fundamentals

- **Refinery runs and utilization** — additional EIA series beyond current inventory/storage data.
- **Product supplied / implied demand** — EIA demand proxy data.
- **Import/export flow data** — EIA trade data.
- **Crack spreads and refinery economics panel** — dedicated refinery margin analysis.
- **Macro-factor dashboard** — growth, inflation, dollar, rates indicators beyond Treasury curve.
- **Consensus vs actual surprise scoring** — requires consensus estimate data source.
- **Narrative generation** — auto-generated fundamental commentary ("the move is inventory-driven because...").

---

## Overview

- **Commodity watchlist with liquidity and open interest** — requires exchange data feed.
- **"Top anomalies today" card** — auto-detection of statistical outliers across all modules. Could be built from existing calc layer outputs.
- **Drill-down from overview cards to underlying module** — click a card to navigate to the relevant analytical page. UI enhancement, no data dependency.
- **Auto-generated trader note** — summarize the other eight modules into a morning brief. Could use the calc layer outputs to template this.

---

## Visualization Enhancements (Cross-Module)

- **Interactive GIS maps** — pipeline routes, refinery locations, storage terminals, trade hubs. Requires geospatial dataset. Deferred as not needed for current scope.
- **Sankey diagrams** — commodity flow from production to consumption. Requires flow/balance data. Deferred as not needed for current scope.
- **Animated charts** — time-evolving term structure, PCA dynamics. High visual impact but implementation-heavy.
- **3D wireframe surfaces** — for optimization landscapes and multi-parameter exploration. plotly supports this but interaction can be clunky.

---

## Infrastructure / Platform

- **Real-time data feed** — replace batch snapshot with streaming or polling updates.
- **Multi-user session state** — save per-user filter presets, watchlists, and scenario configurations.
- **Report export** — PDF/HTML report generation from current dashboard state.
- **Alert/notification system** — trigger alerts when z-scores, correlations, or vol metrics cross thresholds.
- **Audit trail** — log which scenarios were run and what parameters were used.
