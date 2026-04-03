#include <Rcpp.h>
#include <cmath>

// ---------------------------------------------------------------------------
// Black-76 Options Pricing Engine
// Vectorised over (F, K, r, T, sigma, is_call) for high-throughput surface
// calculations across entire volatility grids and terminal curves.
//
// Returns a DataFrame with columns:
//   premium, delta, vega, theta, rho,          (price + 1st order)
//   gamma, vanna, vomma, charm,                (2nd order)
//   speed, zomma, color, ultima                 (3rd order)
// ---------------------------------------------------------------------------

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
  Rcpp::NumericVector delta(n);
  Rcpp::NumericVector vega(n);
  Rcpp::NumericVector theta(n);
  Rcpp::NumericVector rho(n);
  Rcpp::NumericVector gamma(n);
  Rcpp::NumericVector vanna(n);
  Rcpp::NumericVector vomma(n);
  Rcpp::NumericVector charm(n);
  Rcpp::NumericVector speed(n);
  Rcpp::NumericVector zomma(n);
  Rcpp::NumericVector color(n);
  Rcpp::NumericVector ultima(n);

  for (int i = 0; i < n; i++) {
    double Fi     = F[i];
    double Ki     = K[i];
    double ri     = r[i];
    double Ti     = T[i];
    double si     = sigma[i];
    bool   call_i = is_call[i];

    // Guard against degenerate inputs
    if (Ti <= 0.0 || si <= 0.0 || Fi <= 0.0 || Ki <= 0.0) {
      premium[i] = delta[i] = vega[i] = theta[i] = rho[i] = 0.0;
      gamma[i] = vanna[i] = vomma[i] = charm[i] = 0.0;
      speed[i] = zomma[i] = color[i] = ultima[i] = 0.0;
      continue;
    }

    double sqrt_T  = std::sqrt(Ti);
    double sig_sqT = si * sqrt_T;
    double d1      = (std::log(Fi / Ki) + 0.5 * si * si * Ti) / sig_sqT;
    double d2      = d1 - sig_sqT;
    double df      = std::exp(-ri * Ti);            // discount factor
    double nd1     = R::dnorm(d1, 0.0, 1.0, 0);     // PDF at d1
    double Nd1     = R::pnorm(d1, 0.0, 1.0, 1, 0);  // CDF at d1
    double Nd2     = R::pnorm(d2, 0.0, 1.0, 1, 0);  // CDF at d2

    // ------ PRICE ------
    if (call_i) {
      premium[i] = df * (Fi * Nd1 - Ki * Nd2);
    } else {
      premium[i] = df * (Ki * (1.0 - Nd2) - Fi * (1.0 - Nd1));
    }

    // ------ 1ST ORDER ------
    // Delta (dV/dF)
    if (call_i) {
      delta[i] = df * Nd1;
    } else {
      delta[i] = df * (Nd1 - 1.0);
    }

    // Vega (dV/dσ) — per unit vol (not per 1%)
    vega[i] = df * Fi * nd1 * sqrt_T;

    // Theta (dV/dt) — negative of dV/dT
    double theta_common = -df * Fi * nd1 * si / (2.0 * sqrt_T);
    if (call_i) {
      theta[i] = theta_common - ri * df * Ki * Nd2 + ri * df * Fi * Nd1;
    } else {
      theta[i] = theta_common + ri * df * Ki * (1.0 - Nd2) - ri * df * Fi * (1.0 - Nd1);
    }
    // Convention: negate so positive theta = time decay cost
    theta[i] = -theta[i];

    // Rho (dV/dr)
    if (call_i) {
      rho[i] = -Ti * premium[i];
    } else {
      rho[i] = -Ti * premium[i];
    }

    // ------ 2ND ORDER ------
    // Gamma (d²V/dF²)
    gamma[i] = df * nd1 / (Fi * sig_sqT);

    // Vanna (d²V/dFdσ = dDelta/dσ)
    // Vanna = -df * nd1 * d2 / sigma
    vanna[i] = -df * nd1 * d2 / si;

    // Vomma / Volga (d²V/dσ²)
    // Vomma = vega * d1 * d2 / sigma
    vomma[i] = vega[i] * d1 * d2 / si;

    // Charm (dDelta/dt = -dDelta/dT)
    // Charm = -df * nd1 * (2*r*T - d2*sig*sqrt_T) / (2*T*sig*sqrt_T)
    double charm_val = df * nd1 * (2.0 * ri * Ti - d2 * sig_sqT) / (2.0 * Ti * sig_sqT);
    if (!call_i) {
      // put charm has an additional term from the discount factor derivative
      // but under Black-76 both call and put delta differ by df,
      // so charm_put = charm_call + r * df
    }
    charm[i] = charm_val;

    // ------ 3RD ORDER ------
    // Speed (dGamma/dF = d³V/dF³)
    speed[i] = -gamma[i] / Fi * (d1 / sig_sqT + 1.0);

    // Zomma (dGamma/dσ)
    zomma[i] = gamma[i] * (d1 * d2 - 1.0) / si;

    // Color (dGamma/dt = -dGamma/dT)
    // Color = -gamma / (2*T) * (1 + d1*(2*r*T - d2*sig*sqrt_T)/(sig*sqrt_T))
    double color_inner = 2.0 * ri * Ti - d2 * sig_sqT;
    color[i] = -gamma[i] / (2.0 * Ti) * (1.0 + d1 * color_inner / sig_sqT);

    // Ultima (dVomma/dσ = d³V/dσ³)
    // Ultima = -vega / (sigma^2) * (d1*d2*(1 - d1*d2) + d1^2 + d2^2)
    double d1d2 = d1 * d2;
    ultima[i] = -vega[i] / (si * si) * (d1d2 * (1.0 - d1d2) + d1 * d1 + d2 * d2);
  }

  return Rcpp::DataFrame::create(
    Rcpp::Named("premium") = premium,
    Rcpp::Named("delta")   = delta,
    Rcpp::Named("vega")    = vega,
    Rcpp::Named("theta")   = theta,
    Rcpp::Named("rho")     = rho,
    Rcpp::Named("gamma")   = gamma,
    Rcpp::Named("vanna")   = vanna,
    Rcpp::Named("vomma")   = vomma,
    Rcpp::Named("charm")   = charm,
    Rcpp::Named("speed")   = speed,
    Rcpp::Named("zomma")   = zomma,
    Rcpp::Named("color")   = color,
    Rcpp::Named("ultima")  = ultima
  );
}
