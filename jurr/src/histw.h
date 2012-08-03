#ifndef histw_h
#define histw_h

void histw(double* h, const double* y, const double* w, unsigned int n, 
    const double *borders, unsigned int nborders);

void histw2(double* h, const double* x, const double* y, const double* w, 
    unsigned int n, const double *xborders, unsigned int nxborders, 
    const double *yborders, unsigned int nyborders);

#endif 
