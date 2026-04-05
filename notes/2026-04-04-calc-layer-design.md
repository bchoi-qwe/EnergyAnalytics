# Calculation Layer Design

**Date:** 2026-04-04
**Status:** Approved
**Scope:** Replace all `ea_mock_*` functions with real calculation layers that read from the canonical snapshot.

## Context

The EnergyAnalytics dashboard serves a senior energy trader who trades across all products (CL, BRN, HO, RB, NG). The dashboard's purpose is to tell the story of market dynamics and co-dynamics through graphs and figures.

The data pipeline and snapshot are complete (8 Feather datasets). The Options Greeks module is the only one wired to real calculations. The remaining 7 modules use `ea_mock_*` fake data.

## Architecture

### File Structure

One calc file per module, plus shared utilities:

```
R/
  calc_utils.R              # shared helpers
  calc_forward_curves.R     # ea_calc_forward_curves(filters)
  calc_fundamentals.R       # ea_calc_fundamentals(filters)
  calc_volatility.R         # ea_calc_volatility(filters)
  calc_codynamics.R         # ea_calc_codynamics(filters)
  calc_seasonality.R        # ea_calc_seasonality(filters)
  calc_hedging.R            # ea_calc_hedging(filters)
  calc_overview.R           # ea_calc_overview(filters)
  calc_scenarios.R          # ea_calc_scenarios(filters, shocks)
  options_calculations.R    # ea_calc_surface_greeks(filters) [already done]
```

Each `ea_calc_*()` function:
- Takes the standardized `filters` reactive value (from `mod_global_filters`)
- Reads from the canonical Feather snapshot
- Returns a named list of tidy dataframes shaped for plotly charts
- Replaces the corresponding `ea_mock_*()` call in the module server

### Data Flow

```
Global Filters (small-r reactive)
    |
    v
ea_calc_*(filters)
    |
    ├── ea_load_filtered_curves(filters)  [calc_utils.R]
    ├── ea_load_dataset(...)              [data_foundation.R]
    ├── RTL::fitOU / garch / sim*         [where applicable]
    ├── slider::slide_*                   [rolling stats]
    └── Rcpp: black76_greeks_vectorised   [options only]
    |
    v
Named list of tidy dataframes -> module server -> plotly charts
```

### Computation Split

- **Rcpp:** Black-76 greeks engine (already done). Future heavy simulation loops only.
- **slider:** Rolling statistics — vol, correlation, beta.
- **PerformanceAnalytics:** Drawdowns, VaR, CVaR, cumulative returns.
- **broom:** Tidy regression output for hedge ratios and betas.
- **moments / tseries:** Distribution testing for vol and scenarios.
- **RTL (verified working):** `fitOU`, `garch`, `rolladjust`, `GBSOption`, `spreadOption`, `simGBM`, `simOU`.
- **RTL (data sourcing, already in snapshot):** `dflong`, `eia2tidy_all`, `expiry_table`.
- **Base R / dplyr:** Everything else.

Log returns are computed directly (`log(price / lag(price))`) — no RTL dependency for this.

## Build Order

### Filters Input Shape

Every `ea_calc_*()` function receives a `filters` list with this structure (from `ea_global_filter_defaults`):

```r
list(
  market_complex = "Cross-Commodity",
  commodities = c("CL", "BRN", "RB"),        # selected markets
  comparison_commodity = "NG",                 # benchmark market
  date_range = c(start_date, end_date),        # observation window
  rolling_window = "63D",                      # lookback for rolling stats
  tenor_bucket = c("Front", "Quarterly"),      # curve granularity
  expiry_range = c(1, 12),                     # contract month range
  regime = c("All Regimes"),                   # vol regime filter
  scenario_preset = "Base Case"                # scenario selection
)
```

### Phase 1: calc_utils.R

Shared helpers every calc file needs:

- **`ea_load_filtered_curves(filters)`** — loads `commodity_curve_long`, filters to selected markets and expiry range.
- **`ea_log_returns(curves_long)`** — computes log returns from long-format price data.
- **`ea_latest_snapshot_date()`** — most recent date in the snapshot.
- **`ea_curve_to_wide(curves_long)`** — pivots long curves to wide format (date x curve_point).

### Phase 2: calc_forward_curves.R

Answers: *What's the curve shape? How has it changed? Where are the spreads?*

| Output | Description | Columns |
|--------|-------------|---------|
| `curve_snapshot` | Current forward curve by tenor for each market | `market`, `curve_point`, `curve_point_num`, `price`, `label` |
| `curve_history` | Lead market curve at multiple historical dates (latest, 1W, 1M, 3M ago) | `snapshot_label`, `curve_point_num`, `price` |
| `curve_change_heatmap` | Daily price changes by contract month over time | `date`, `curve_point_num`, `change` |
| `calendar_spreads` | M1-M2, M1-M3 spread timeseries per market. Uses `RTL::rolladjust` | `date`, `market`, `spread_label`, `value` |
| `curve_shape_metrics` | Contango/backwardation, slope, curvature per market | `market`, `slope`, `curvature`, `structure_label` |

RTL: `rolladjust`. Rcpp: none.

### Phase 3: calc_fundamentals.R

Answers: *What's the physical market saying? Bullish or bearish vs. seasonal norms?*

| Output | Description | Columns |
|--------|-------------|---------|
| `stocks_timeseries` | Inventory levels over time by product | `date`, `product`, `location`, `value`, `unit` |
| `stocks_seasonal` | Current year vs. 5-year range by week-of-year | `week`, `current`, `avg_5yr`, `min_5yr`, `max_5yr`, `product` |
| `stocks_deviation` | Surplus/deficit vs. 5-year average | `date`, `product`, `deviation`, `deviation_pct` |
| `storage_capacity_util` | Storage utilization rates | `date`, `product`, `capacity`, `stocks`, `utilization_pct` |
| `supply_demand_balance` | Net balance by component (if data available) | `date`, `product`, `component`, `value` |

Note: BRN uses placeholder data until a non-EIA source is found. EIA covers CL (crude), HO (distillate), RB (gasoline), NG (Lower48).

RTL: none (snapshot data already built via `eia2tidy_all`). Rcpp: none.

### Phase 4: calc_volatility.R

Answers: *Is vol elevated or compressed? Where's the skew? Term structure normal or inverted?*

| Output | Description | Columns |
|--------|-------------|---------|
| `realized_vol_timeseries` | Rolling realized vol (20d, 60d, 120d) per market front contract | `date`, `market`, `window`, `realized_vol` |
| `garch_vol` | GARCH(1,1) conditional vol for lead market. Uses `RTL::garch` | `date`, `returns`, `garch_vol` |
| `vol_term_structure` | Realized vol by tenor bucket per market | `market`, `tenor`, `realized_vol` |
| `vol_surface_grid` | IV by strike moneyness and tenor | `market`, `curve_point_num`, `moneyness`, `iv` |
| `vol_skew_snapshot` | IV smile at front tenor per market | `market`, `moneyness`, `iv` |
| `realized_vs_implied` | Realized vol vs. ATM implied vol | `date`, `market`, `realized`, `implied` |

Uses `slider` for rolling std dev. RTL: `garch`. Rcpp: existing Black-76 engine for IV.

### Phase 5: calc_codynamics.R

Answers: *How are products co-moving? Are correlations stable? Where are the dislocations?*

| Output | Description | Columns |
|--------|-------------|---------|
| `correlation_matrix` | Rolling correlation across selected markets | `market_x`, `market_y`, `correlation` |
| `correlation_timeseries` | Rolling correlation between each pair over time | `date`, `pair`, `correlation`, `window` |
| `spread_timeseries` | Key intercommodity spreads (CL-BRN, cracks, NG-HTT) | `date`, `spread_label`, `value` |
| `spread_zscore` | Z-score of current spreads vs. rolling history | `spread_label`, `current`, `mean`, `std`, `zscore` |
| `beta_matrix` | Pairwise rolling regression betas | `market_x`, `market_y`, `beta`, `r_squared` |
| `pca_decomposition` | PC weights across the complex | `market`, `PC1`, `PC2`, `PC3` |

Uses `slider` for rolling correlation/beta. `broom` for regression tidying. Rcpp: none.

### Phase 6: calc_seasonality.R

Answers: *Where are we in the seasonal cycle? Tracking or deviating?*

| Output | Description | Columns |
|--------|-------------|---------|
| `seasonal_overlay` | Current year vs. historical years, indexed | `day_of_year`, `year`, `market`, `indexed_value` |
| `seasonal_range` | 5-year min/max/mean band by day-of-year | `day_of_year`, `market`, `avg`, `min`, `max` |
| `seasonal_returns` | Average return by month/week across history | `market`, `period`, `avg_return`, `hit_rate` |
| `mean_reversion` | OU fit on front spread. Uses `RTL::fitOU` | `market`, `theta`, `mu`, `sigma`, `half_life` |
| `seasonal_deviation` | Current year percentile rank in historical distribution | `date`, `market`, `percentile` |

RTL: `fitOU`. Rcpp: none.

### Phase 7: calc_hedging.R

Answers: *Optimal hedge ratios? Are betas stable? Where does effectiveness break down?*

| Output | Description | Columns |
|--------|-------------|---------|
| `hedge_ratios` | OLS beta of each market vs. benchmark | `market`, `beta`, `r_squared`, `std_error` |
| `rolling_beta` | Rolling hedge ratio over time | `date`, `market`, `beta`, `r_squared` |
| `hedge_effectiveness` | Hedged vs. unhedged vol per market | `market`, `unhedged_vol`, `hedged_vol`, `vol_reduction_pct` |
| `residual_timeseries` | Hedge residuals over time | `date`, `market`, `residual` |
| `ou_fit` | OU parameters on hedge residuals. Uses `RTL::fitOU` | `market`, `theta`, `mu`, `sigma`, `half_life` |
| `cross_hedge_matrix` | Pairwise beta/R-squared grid | `market_x`, `market_y`, `beta`, `r_squared` |

Uses `slider` for rolling regressions. `broom` for tidy output. RTL: `fitOU`.

### Phase 8: calc_overview.R

Answers: *Morning briefing — what moved, what matters, what's unusual.*

| Output | Description | Columns |
|--------|-------------|---------|
| `price_snapshot` | Latest price, daily change, % change per market | `market`, `price`, `change`, `change_pct` |
| `relative_performance` | Normalized (indexed to 100) price paths | `date`, `market`, `indexed_value` |
| `curve_structure_summary` | Contango/backwardation, M1-M2, slope per market | `market`, `m1_m2_spread`, `slope`, `structure_label` |
| `correlation_snapshot` | Latest cross-market correlation matrix | `market_x`, `market_y`, `correlation` |
| `spread_monitor` | Key spreads with level, change, z-score | `spread_label`, `level`, `change`, `zscore` |
| `vol_snapshot` | Current realized vol and percentile rank | `market`, `realized_vol_20d`, `vol_percentile` |
| `drawdown_summary` | Current drawdown from peak. Uses `PerformanceAnalytics` | `market`, `drawdown_pct`, `days_from_peak` |

RTL: none. Rcpp: none.

### Phase 9: calc_scenarios.R

Answers: *What happens under stress? Where are the tail risks?*

| Output | Description | Columns |
|--------|-------------|---------|
| `price_simulations` | GBM Monte Carlo paths. Uses `RTL::simGBM` | `t`, `market`, `sim_id`, `price` |
| `ou_simulations` | Mean-reverting spread sims. Uses `RTL::simOU` calibrated from `fitOU` | `t`, `spread_label`, `sim_id`, `value` |
| `stress_scenarios` | Predefined shock scenarios applied to current prices | `scenario_label`, `market`, `shock_pct`, `new_price`, `pnl_impact` |
| `spread_option_pnl` | Spread option payoffs. Uses `RTL::spreadOption` | `spread_level`, `payoff`, `spread_label` |
| `var_summary` | VaR/CVaR at 95th/99th. Uses `PerformanceAnalytics` | `market`, `var_95`, `var_99`, `cvar_95`, `cvar_99` |
| `correlation_stress` | Correlation matrix under normal vs. stress regimes | `market_x`, `market_y`, `corr_normal`, `corr_stress` |

RTL: `simGBM`, `simOU`, `fitOU`, `spreadOption`. Rcpp: none beyond existing engine.

## New Package Dependencies

These need to be added to DESCRIPTION `Imports`:

- `slider` — rolling window computations
- `PerformanceAnalytics` — VaR, drawdowns, risk metrics
- `broom` — tidy regression output
- `moments` — distribution testing (skewness, kurtosis)
- `tseries` — stationarity tests

Already in DESCRIPTION: `dplyr`, `tidyr`, `purrr`, `arrow`, `plotly`, `Rcpp`, `RTL`, `scales`, `stringr`, `tibble`, `lubridate`, `readr`.

## Known Gaps

- **BRN fundamentals:** No Brent-specific EIA inventory data. Using placeholder until an alternative source is identified.
- **Real implied vol data:** `options_surface_long` currently built from a single sample CSV. The parametric vol surface in `ea_generate_vol_surface` serves as fallback.
- **RTL function stability:** Many RTL functions have non-standard argument names or undocumented behavior. All RTL calls verified working as of 2026-04-04, but should be wrapped with error handling.
