#include "RcppArmadillo.h"
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export]]
arma::mat sandwich(const arma::mat & X, const arma::vec & R, const arma::vec & id, const arma::vec & t, const arma::vec & s, const arma::vec & teval, const arma::vec & seval, double h) {
    int n = X.n_rows;
    int p = X.n_cols;
    int m = teval.size();
    arma::mat lower(m, p * (p + 1) / 2);
    arma::mat bun(p, p);
    arma::mat mtemp(p, p);
    arma::mat meat(p, p);
    arma::vec kvec(n);
    double temp, temp2;
    arma::uvec lower_indices = arma::trimatl_ind(size(bun));
    for (int j = 0; j < m; j++) {
        meat.fill(0);
        for (int i = 0; i < n; i++) {
            temp = ((t(i) - teval(j)) * (t(i) - teval(j)) + (s(i) - seval(j)) * (s(i) - seval(j))) / h / h;
            if (temp < 6) {
                kvec(i) = exp(-temp / 2);
            } else {
                kvec(i) = 0;
            }
        }
        for (int k = 0; k < p; k++) {
            for (int l = 0; l < p; l++) {
                temp = 0;
                for (int i = 0; i < n; i++) {
                    temp += X(i, k) * kvec(i) * X(i, l);
                }
                bun(k, l) = temp;
            }
        }
        for (int k = 0; k < p; k++) {
            for (int l = 0; l < p; l++) {
                temp = 0;
                temp2 = 0;
                for (int i = 0; i < n; i++) {
                    if (i > 0 && id(i) != id(i-1)) {
                        meat(k, l) += temp * temp2;
                        temp = 0;
                        temp2 = 0;
                    }
                    temp += X(i, k) * kvec(i) * R(i);
                    temp2 += X(i, l) * kvec(i) * R(i);
                }
                meat(k, l) += temp * temp2;
            }
        }
        if ((int)arma::rank(bun) == p) {
            mtemp = arma::inv_sympd(bun);
            mtemp = mtemp * meat * mtemp;
            lower.row(j) = mtemp(lower_indices).t();
        } else {
            lower.row(j).fill(arma::datum::nan);
        }
    }
    return lower;
}