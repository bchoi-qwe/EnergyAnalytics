#include <Rcpp.h>
#include <cmath>

// ---------------------------------------------------------------------------
// Black-76 Options Pricing Engine — Central Difference Method
// Vectorised over (F, K, r, T, sigma, is_call) for high-throughput surface
// calculations across entire volatility grids and terminal curves.
//
// All Greeks are computed numerically via central finite differences:
//   1st order:  dV/dx  ≈ [V(x+h) - V(x-h)] / (2h)
//   2nd order:  d²V/dx² ≈ [V(x+h) - 2V(x) + V(x-h)] / h²
//   Cross:      d²V/dxdy ≈ [V(x+h,y+k) - V(x+h,y-k) - V(x-h,y+k) + V(x-h,y-k)] / (4hk)
//   3rd order:  d³V/dx³ ≈ [V(x+2h) - 2V(x+h) + 2V(x-h) - V(x-2h)] / (2h³)
//
// Returns a DataFrame with columns:
//   premium, delta, vega, theta, rho,          (price + 1st order)
//   gamma, vanna, vomma, charm,                (2nd order)
//   speed, zomma, color, ultima                 (3rd order)
// ---------------------------------------------------------------------------

// Black-76 closed-form price (scalar) used as the base for central differences
static double b76_price(double Fi, double Ki, double ri, double Ti,
                        double si, bool call_i) {
  if (Ti <= 0.0 || si <= 0.0 || Fi <= 0.0 || Ki <= 0.0) return 0.0;

  double sqrt_T  = std::sqrt(Ti);
  double sig_sqT = si * sqrt_T;
  double d1      = (std::log(Fi / Ki) + 0.5 * si * si * Ti) / sig_sqT;
  double d2      = d1 - sig_sqT;
  double df      = std::exp(-ri * Ti);
  double Nd1     = R::pnorm(d1, 0.0, 1.0, 1, 0);
  double Nd2     = R::pnorm(d2, 0.0, 1.0, 1, 0);

  if (call_i) {
    return df * (Fi * Nd1 - Ki * Nd2);
  } else {
    return df * (Ki * (1.0 - Nd2) - Fi * (1.0 - Nd1));
  }
}

// [[Rcpp::export]]
Rcpp::DataFrame rolling_pair_stats_cpp(
    Rcpp::NumericVector x,
    Rcpp::NumericVector y,
    int window,
    int min_obs = 10
) {
  int n = x.size();
  if (y.size() != n) {
    Rcpp::stop("x and y must have the same length");
  }
  if (window < 1) {
    Rcpp::stop("window must be >= 1");
  }
  if (min_obs < 1) {
    min_obs = 1;
  }

  Rcpp::NumericVector corr(n, NA_REAL);
  Rcpp::NumericVector beta(n, NA_REAL);
  Rcpp::NumericVector r_squared(n, NA_REAL);
  Rcpp::IntegerVector n_obs(n);

  std::vector<int> valid(n, 0);
  double sx = 0.0;
  double sy = 0.0;
  double sxx = 0.0;
  double syy = 0.0;
  double sxy = 0.0;
  int count = 0;

  for (int i = 0; i < n; ++i) {
    int drop_idx = i - window;
    if (drop_idx >= 0 && valid[drop_idx] == 1) {
      double x_drop = x[drop_idx];
      double y_drop = y[drop_idx];
      sx -= x_drop;
      sy -= y_drop;
      sxx -= x_drop * x_drop;
      syy -= y_drop * y_drop;
      sxy -= x_drop * y_drop;
      count -= 1;
    }

    double xi = x[i];
    double yi = y[i];
    if (R_FINITE(xi) && R_FINITE(yi)) {
      valid[i] = 1;
      sx += xi;
      sy += yi;
      sxx += xi * xi;
      syy += yi * yi;
      sxy += xi * yi;
      count += 1;
    }

    if (i < window - 1) {
      continue;
    }

    n_obs[i] = count;
    if (count < min_obs) {
      continue;
    }

    double count_d = static_cast<double>(count);
    double ssx = sxx - (sx * sx) / count_d;
    double ssy = syy - (sy * sy) / count_d;
    double sxy_c = sxy - (sx * sy) / count_d;

    if (!R_FINITE(ssx) || !R_FINITE(ssy) || !R_FINITE(sxy_c) || ssx <= 0.0 || ssy <= 0.0) {
      continue;
    }

    double corr_i = sxy_c / std::sqrt(ssx * ssy);
    if (!R_FINITE(corr_i)) {
      continue;
    }
    if (corr_i > 1.0) corr_i = 1.0;
    if (corr_i < -1.0) corr_i = -1.0;

    corr[i] = corr_i;
    beta[i] = sxy_c / ssx;
    r_squared[i] = corr_i * corr_i;
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("corr") = corr,
    Rcpp::Named("beta") = beta,
    Rcpp::Named("r_squared") = r_squared,
    Rcpp::Named("n_obs") = n_obs
  );
}

// [[Rcpp::export]]
Rcpp::DataFrame black76_greeks_vectorised(
    Rcpp::NumericVector F,      // Forward / futures price
    Rcpp::NumericVector K,      // Strike price
    Rcpp::NumericVector r,      // Risk-free rate (annualised, decimal)
    Rcpp::NumericVector T,      // Time to expiry (years)
    Rcpp::NumericVector sigma,  // Implied volatility (annualised, decimal)
    Rcpp::LogicalVector is_call // TRUE = call, FALSE = put
) {
  int n = F.size();

  // Output vectors
  Rcpp::NumericVector premium(n);
  Rcpp::NumericVector delta_v(n);
  Rcpp::NumericVector vega_v(n);
  Rcpp::NumericVector theta_v(n);
  Rcpp::NumericVector rho_v(n);
  Rcpp::NumericVector gamma_v(n);
  Rcpp::NumericVector vanna_v(n);
  Rcpp::NumericVector vomma_v(n);
  Rcpp::NumericVector charm_v(n);
  Rcpp::NumericVector speed_v(n);
  Rcpp::NumericVector zomma_v(n);
  Rcpp::NumericVector color_v(n);
  Rcpp::NumericVector ultima_v(n);

  for (int i = 0; i < n; i++) {
    double Fi     = F[i];
    double Ki     = K[i];
    double ri     = r[i];
    double Ti     = T[i];
    double si     = sigma[i];
    bool   call_i = is_call[i];

    // Guard against degenerate inputs
    if (Ti <= 0.0 || si <= 0.0 || Fi <= 0.0 || Ki <= 0.0) {
      premium[i] = delta_v[i] = vega_v[i] = theta_v[i] = rho_v[i] = 0.0;
      gamma_v[i] = vanna_v[i] = vomma_v[i] = charm_v[i] = 0.0;
      speed_v[i] = zomma_v[i] = color_v[i] = ultima_v[i] = 0.0;
      continue;
    }

    // --- Bump sizes ---
    double hF = Fi * 0.01;               // 1% of forward price
    double hs = 0.01;                     // 1 vol point
    double hT = 1.0 / 365.25;            // 1 day
    double hr = 0.0001;                   // 1 basis point

    // Ensure bumps don't push into invalid territory
    double si_up  = si + hs;
    double si_dn  = std::fmax(si - hs, 0.001);
    hs = (si_up - si_dn) / 2.0;  // adjust if clamped

    double si_up2  = si + 2.0 * hs;
    double si_dn2  = std::fmax(si - 2.0 * hs, 0.001);

    double Ti_up  = Ti + hT;
    double Ti_dn  = std::fmax(Ti - hT, 1e-6);
    hT = (Ti_up - Ti_dn) / 2.0;  // adjust if clamped

    double Fi_up  = Fi + hF;
    double Fi_dn  = std::fmax(Fi - hF, 0.01);
    hF = (Fi_up - Fi_dn) / 2.0;  // adjust if clamped

    double Fi_up2  = Fi + 2.0 * hF;
    double Fi_dn2  = std::fmax(Fi - 2.0 * hF, 0.01);

    // --- Base price ---
    double V0 = b76_price(Fi, Ki, ri, Ti, si, call_i);
    premium[i] = V0;

    // --- 1st Order Greeks (central difference) ---

    // Delta: dV/dF
    double V_Fup = b76_price(Fi_up, Ki, ri, Ti, si, call_i);
    double V_Fdn = b76_price(Fi_dn, Ki, ri, Ti, si, call_i);
    delta_v[i] = (V_Fup - V_Fdn) / (2.0 * hF);

    // Vega: dV/dσ
    double V_sup = b76_price(Fi, Ki, ri, Ti, si_up, call_i);
    double V_sdn = b76_price(Fi, Ki, ri, Ti, si_dn, call_i);
    vega_v[i] = (V_sup - V_sdn) / (2.0 * hs);

    // Theta: -dV/dT (time decay; negate so positive = decay cost)
    double V_Tup = b76_price(Fi, Ki, ri, Ti_up, si, call_i);
    double V_Tdn = b76_price(Fi, Ki, ri, Ti_dn, si, call_i);
    theta_v[i] = -(V_Tup - V_Tdn) / (2.0 * hT);

    // Rho: dV/dr
    double V_rup = b76_price(Fi, Ki, ri + hr, Ti, si, call_i);
    double V_rdn = b76_price(Fi, Ki, ri - hr, Ti, si, call_i);
    rho_v[i] = (V_rup - V_rdn) / (2.0 * hr);

    // --- 2nd Order Greeks ---

    // Gamma: d²V/dF²
    gamma_v[i] = (V_Fup - 2.0 * V0 + V_Fdn) / (hF * hF);

    // Vanna: d²V/dFdσ (cross partial)
    double V_Fup_sup = b76_price(Fi_up, Ki, ri, Ti, si_up, call_i);
    double V_Fup_sdn = b76_price(Fi_up, Ki, ri, Ti, si_dn, call_i);
    double V_Fdn_sup = b76_price(Fi_dn, Ki, ri, Ti, si_up, call_i);
    double V_Fdn_sdn = b76_price(Fi_dn, Ki, ri, Ti, si_dn, call_i);
    vanna_v[i] = (V_Fup_sup - V_Fup_sdn - V_Fdn_sup + V_Fdn_sdn) / (4.0 * hF * hs);

    // Vomma / Volga: d²V/dσ²
    vomma_v[i] = (V_sup - 2.0 * V0 + V_sdn) / (hs * hs);

    // Charm: d²V/dFdT = dDelta/dT (negate for convention: -dDelta/dT)
    double V_Fup_Tup = b76_price(Fi_up, Ki, ri, Ti_up, si, call_i);
    double V_Fup_Tdn = b76_price(Fi_up, Ki, ri, Ti_dn, si, call_i);
    double V_Fdn_Tup = b76_price(Fi_dn, Ki, ri, Ti_up, si, call_i);
    double V_Fdn_Tdn = b76_price(Fi_dn, Ki, ri, Ti_dn, si, call_i);
    charm_v[i] = -(V_Fup_Tup - V_Fup_Tdn - V_Fdn_Tup + V_Fdn_Tdn) / (4.0 * hF * hT);

    // --- 3rd Order Greeks ---

    // Speed: d³V/dF³ = dGamma/dF
    double V_F2up = b76_price(Fi_up2, Ki, ri, Ti, si, call_i);
    double V_F2dn = b76_price(Fi_dn2, Ki, ri, Ti, si, call_i);
    speed_v[i] = (V_F2up - 2.0 * V_Fup + 2.0 * V_Fdn - V_F2dn) / (2.0 * hF * hF * hF);

    // Zomma: d²Gamma/dσ = d³V/dF²dσ
    // = [Gamma(σ+h) - Gamma(σ-h)] / (2h)
    // where Gamma(σ) = [V(F+h,σ) - 2V(F,σ) + V(F-h,σ)] / h²
    double gamma_sup = (V_Fup_sup - 2.0 * V_sup + V_Fdn_sup) / (hF * hF);
    double gamma_sdn = (V_Fup_sdn - 2.0 * V_sdn + V_Fdn_sdn) / (hF * hF);
    zomma_v[i] = (gamma_sup - gamma_sdn) / (2.0 * hs);

    // Color: dGamma/dT = d³V/dF²dT (negate for convention)
    // = -[Gamma(T+h) - Gamma(T-h)] / (2h)
    double gamma_Tup = (V_Fup_Tup - 2.0 * V_Tup + V_Fdn_Tup) / (hF * hF);
    double gamma_Tdn = (V_Fup_Tdn - 2.0 * V_Tdn + V_Fdn_Tdn) / (hF * hF);
    color_v[i] = -(gamma_Tup - gamma_Tdn) / (2.0 * hT);

    // Ultima: d³V/dσ³ = dVomma/dσ
    // = [Vomma(σ+h) - Vomma(σ-h)] / (2h)
    // Vomma(σ) = [V(σ+h) - 2V(σ) + V(σ-h)] / h²
    // So we need V at σ-2h, σ-h, σ, σ+h, σ+2h
    double V_s2up = b76_price(Fi, Ki, ri, Ti, si_up2, call_i);
    double V_s2dn = b76_price(Fi, Ki, ri, Ti, si_dn2, call_i);
    double vomma_up = (V_s2up - 2.0 * V_sup + V0) / (hs * hs);
    double vomma_dn = (V0 - 2.0 * V_sdn + V_s2dn) / (hs * hs);
    ultima_v[i] = (vomma_up - vomma_dn) / (2.0 * hs);
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("premium") = premium,
    Rcpp::Named("delta")   = delta_v,
    Rcpp::Named("vega")    = vega_v,
    Rcpp::Named("theta")   = theta_v,
    Rcpp::Named("rho")     = rho_v,
    Rcpp::Named("gamma")   = gamma_v,
    Rcpp::Named("vanna")   = vanna_v,
    Rcpp::Named("vomma")   = vomma_v,
    Rcpp::Named("charm")   = charm_v,
    Rcpp::Named("speed")   = speed_v,
    Rcpp::Named("zomma")   = zomma_v,
    Rcpp::Named("color")   = color_v,
    Rcpp::Named("ultima")  = ultima_v
  );
}
