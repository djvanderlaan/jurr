#include "histw.h"

extern "C" {

  void histw_c(double* h, const double* y, const double* w, const int* n, 
      const double *borders, const int* nborders) {
    histw(h, y, w, *n, borders, *nborders);
  }
  
  void histw2_c(double* h, const double* x, const double* y, const double* w, const int* n, 
      const double *xborders, const int* nxborders, const double *yborders, const int* nyborders) {
    histw2(h, x, y, w, *n, xborders, *nxborders, yborders, *nyborders);
  }

}

