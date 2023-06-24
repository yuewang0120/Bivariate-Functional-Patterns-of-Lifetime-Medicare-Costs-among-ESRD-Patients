#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat kfit(const arma::mat & X, const arma::vec & Y, const arma::vec & t, const arma::vec & s, const arma::vec & teval, const arma::vec & seval, double h) {
    int n = X.n_rows;
    int p = X.n_cols;
    int m = teval.size();
    arma::mat coef(m, p);
    arma::mat xkx(p, p);
    arma::vec kvec(n);
    arma::vec xky(p);
    double temp;
    for (int j = 0; j < m; j++) {
        for (int i = 0; i < n; i++) {
            temp = ((t(i) - teval(j)) * (t(i) - teval(j)) + (s(i) - seval(j)) * (s(i) - seval(j))) / h / h;
            if (temp < 6) {
                kvec(i) = exp(-temp / 2);
            } else {
                kvec(i) = 0;
            }
        }
        for (int k = 0; k < p; k++) {
            for (int l = 0; l <= k; l++) {
                temp = 0;
                for (int i = 0; i < n; i++) {
                    temp += X(i, k) * kvec(i) * X(i, l);
                }
                xkx(k, l) = temp;
                xkx(l, k) = temp;
            }
        }
        if ((int)arma::rank(xkx) == p) {
            for (int k = 0; k < p; k++) {
                temp = 0;
                for (int i = 0; i < n; i++) {
                    temp += X(i, k) * kvec(i) * Y(i);
                }
                xky(k) = temp;
            }
            coef.row(j) = arma::solve(xkx, xky, arma::solve_opts::no_approx + arma::solve_opts::likely_sympd + arma::solve_opts::fast).t();
        } else {
            coef.row(j).fill(arma::datum::nan);
        }
    }
    return coef;
}