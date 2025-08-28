#include <Rcpp.h>

double mutualinfo(Rcpp::IntegerMatrix joint, int n);

extern "C" SEXP C_mutualinfo(SEXP jointSEXP, SEXP nSEXP);