#include <Rcpp.h>
#include <map>
#include <vector>
#include <cmath>

// ---------------------------------------------------------------------------
//  2-Regime Buffer Indicators (C++ implementation)
// ---------------------------------------------------------------------------

//' @noRd
//' @param q Threshold variable (original scale, not demeaned)
//' @param g1 Lower buffer boundary (rL)
//' @param g2 Upper buffer boundary (rU)
//' @param id Integer panel identifiers (converted in R wrapper)
//' @return Numeric vector: 0 = regime 1, 1 = regime 2
// [[Rcpp::export]]
Rcpp::NumericVector buildBufferIndicators_cpp(Rcpp::NumericVector q,
                                               double g1, double g2,
                                               Rcpp::IntegerVector id) {
  int n = q.size();
  Rcpp::NumericVector d(n, 0.0);

  // Group indices by id using std::map (keys sorted for deterministic order)
  std::map<int, std::vector<int>> groups;
  for (int k = 0; k < n; k++) {
    groups[id[k]].push_back(k);
  }

  // Process each unit
  for (auto& pair : groups) {
    auto& idx = pair.second;
    int ni = static_cast<int>(idx.size());
    for (int t = 0; t < ni; t++) {
      int pos = idx[t];
      double q_it = q[pos];

      if (t == 0) {
        // First observation: nearest-boundary rule
        if (q_it <= g1) {
          d[pos] = 0.0;
        } else if (q_it > g2) {
          d[pos] = 1.0;
        } else {
          double dist_low  = std::abs(q_it - g1);
          double dist_high = std::abs(q_it - g2);
          d[pos] = (dist_high < dist_low) ? 1.0 : 0.0;
        }
      } else {
        double d_prev = d[idx[t - 1]];
        if (q_it > g2) {
          d[pos] = 1.0;
        } else if (q_it < g1) {
          d[pos] = 0.0;
        } else {
          d[pos] = d_prev;
        }
      }
    }
  }

  return d;
}

// ---------------------------------------------------------------------------
//  3-Regime Buffer Indicators (C++ implementation)
// ---------------------------------------------------------------------------

//' @noRd
//' @param q Threshold variable (original scale, not demeaned)
//' @param rL1 Lower boundary of buffer zone 1
//' @param rU1 Upper boundary of buffer zone 1
//' @param rL2 Lower boundary of buffer zone 2 (must be > rU1)
//' @param rU2 Upper boundary of buffer zone 2
//' @param id Integer panel identifiers (converted in R wrapper)
//' @return Integer vector with values 1L, 2L, or 3L
// [[Rcpp::export]]
Rcpp::IntegerVector buildBufferIndicators3_cpp(Rcpp::NumericVector q,
                                                double rL1, double rU1,
                                                double rL2, double rU2,
                                                Rcpp::IntegerVector id) {
  int n = q.size();
  Rcpp::IntegerVector d(n);

  // Group indices by id
  std::map<int, std::vector<int>> groups;
  for (int k = 0; k < n; k++) {
    groups[id[k]].push_back(k);
  }

  // Process each unit
  for (auto& pair : groups) {
    auto& idx = pair.second;
    int ni = static_cast<int>(idx.size());
    for (int t = 0; t < ni; t++) {
      int pos = idx[t];
      double q_it = q[pos];

      if (t == 0) {
        // First observation: nearest-boundary rule
        if (q_it <= rL1) {
          d[pos] = 1;
        } else if (q_it <= rU1) {
          d[pos] = (std::abs(q_it - rL1) < std::abs(q_it - rU1)) ? 1 : 2;
        } else if (q_it <= rL2) {
          d[pos] = 2;
        } else if (q_it <= rU2) {
          d[pos] = (std::abs(q_it - rL2) < std::abs(q_it - rU2)) ? 2 : 3;
        } else {
          d[pos] = 3;
        }
      } else {
        int d_prev = d[idx[t - 1]];
        if (q_it <= rL1) {
          d[pos] = 1;
        } else if (q_it <= rU1) {
          d[pos] = (d_prev <= 1) ? 1 : 2;
        } else if (q_it <= rL2) {
          d[pos] = 2;
        } else if (q_it <= rU2) {
          d[pos] = (d_prev <= 2) ? 2 : 3;
        } else {
          d[pos] = 3;
        }
      }
    }
  }

  return d;
}
