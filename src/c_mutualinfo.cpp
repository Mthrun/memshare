#include "c_mutualinfo.h"

#include <cmath>
#include <iostream>

using namespace Rcpp;

//author Julian Maerte
double mutualinfo(Rcpp::IntegerMatrix joint, int n) {
  int rows = joint.nrow();
  int cols = joint.ncol();

  double* px = new double[rows]();
  double* py = new double[cols]();

  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
      px[i] += joint(i,j) / (double) n;
      py[j] += joint(i,j) / (double) n;
    }
  }

  double mi = 0;

  for (int i = 0; i < rows; i++) {
    for (int j = 0; j < cols; j++) {
      if (joint(i, j) > 0) {
        double pxy = joint(i, j) / (double) n;
        mi += pxy * log(pxy / (px[i] * py[j]));
      }
    }
  }
  delete[] px;
  delete[] py;

  return mi / log(2);
}

extern "C" SEXP C_mutualinfo(SEXP jointSEXP, SEXP nSEXP) {
    try {
      IntegerMatrix joint(jointSEXP);   // Convert SEXP -> Rcpp::IntegerMatrix
      int n = as<int>(nSEXP);           // Convert SEXP -> int

      double res = mutualinfo(joint, n);
      return wrap(res);                  // Convert double -> SEXP
    } catch (std::exception &e) {
      Rf_error("mutualinfo error: %s", e.what());
    } catch (...) {
      Rf_error("mutualinfo unknown error");
    }
}
