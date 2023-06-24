#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat kres(const arma::mat & X, const arma::mat & Y, const arma::vec & t, const arma::vec & s, const arma::vec pos, double h) {
    int n = X.n_rows;
    int m = pos.size();
    int p = X.n_cols;
    int q = Y.n_cols; 
    arma::mat result(m, q);
    arma::mat xkx(p, p);
    arma::vec kvec(n);
    arma::mat xky(p, q);
    double temp;
    // int complete = 0;
    for (int j = 0; j < m; j++) {
        // if (100 * j >= (complete + 1) * n) {
        //     complete++;
        //     Rprintf("%i ", complete);
        // }
        for (int i = 0; i < n; i++) {
            temp = ((t(i) - t(pos(j)-1)) * (t(i) - t(pos(j)-1)) + (s(i) - s(pos(j)-1)) * (s(i) - s(pos(j)-1))) / h / h;
            if (temp < 6) {
                kvec(i) = exp(-temp / 2);
            } else {
                kvec(i) = 0;
            }
        }
        for (int k = 0; k < p; k++) {
            for (int l = 0; l < p; l++) {
                if (k > l) {
                    xkx(k, l) = xkx(l, k);
                } else {
                    temp = 0;
                    for (int i = 0; i < n; i++) {
                        temp += X(i, k) * kvec(i) * X(i, l);
                    }
                    xkx(k, l) = temp;
                }
            }
        }
        for (int k = 0; k < p; k++) {
            for (int l = 0; l < q; l++) {
                temp = 0;
                for (int i = 0; i < n; i++) {
                    temp += X(i, k) * kvec(i) * Y(i, l);
                }
                xky(k, l) = temp;
            }
        }
        if ((int)arma::rank(xkx) == p) {
            result.row(j) = Y.row(pos(j)-1) - X.row(pos(j)-1) * arma::solve(xkx, xky, arma::solve_opts::no_approx + arma::solve_opts::fast + arma::solve_opts::likely_sympd);
        } else {
            result.row(j).fill(arma::datum::nan);
        }
    }
    return result;
}
