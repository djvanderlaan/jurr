#include "histw.h"

void histw(double* h, const double* y, const double* w, unsigned int n, const double *borders, unsigned int nborders) {
  const double* bp = 0;
  for (unsigned int i = 0; i < n; ++i, ++y, ++w) {
    bp = borders;
    for (unsigned int j = 0; j < nborders; ++j, ++bp) {
      if (*y < *bp) { 
        h[j] += *w;
        break;
      }
    }
    if (*y >= *bp) h[nborders] += *w;
  }
}


void histw2(double* h, const double* x, const double* y, const double* w, unsigned int n, 
    const double *xborders, unsigned int nxborders, const double *yborders, unsigned int nyborders) {
          
  const double* bp = 0;
  unsigned int  ix = 0;
  unsigned int  iy = 0;
  for (unsigned int i = 0; i < n; ++i, ++y, ++x, ++w) {      
    // determine x-bin 
    bp = xborders;
    ix = nxborders;
    for (unsigned int j = 0; j < nxborders; ++j, ++bp) {
      if (*x < *bp) { 
        ix = j;
        break;
      }
    }
    // determine y-bin
    bp = yborders;
    iy = nyborders;
    for (unsigned int j = 0; j < nyborders; ++j, ++bp) {
      if (*y < *bp) { 
        iy = j;
        break;
      }
    }
    // bin
    h[iy*(nxborders+1)+ix] += *w;
  }
}
