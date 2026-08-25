#include <Rcpp.h>
#include <omp.h>
#define sqrt2 1.4142135624
using namespace Rcpp;

// [[Rcpp::plugins(openmp)]]

//' @title Create data frame of raster cells and the travel time between them
//'
//' @description Uses an Integer Matrix of the cells values in a raster
//' representation of a road network to determine the IDs of adjacent matrix or,
//' equivalently, raster cells and calculate the travel cost, quantified in
//' seconds, from the center of the first cell to the center of the second.
//'
//' @details The cell values in m should be integers and represent the road
//' speed limit in miles per hour. Cell values equal to 255 correspond to NODATA
//' (background / no road cell). cellSize is the resolution of the raster used
//' to extract m in native units, meters of feet. This function is called
//' internally by prepareGraph().
//'
//' @param m Integer Matrix
//' @param cellSize double
//' @param linearUnitFactor double
//'
//' @noRd
//'
//' @export
// [[Rcpp::export]]
 DataFrame rcpp_buildNodeDF(IntegerMatrix m, double cellSize, double linearUnitFactor) {

   int nc = m.ncol();
   int nr = m.nrow();
   int r, c;

   int sumNW=0, sumN=0, sumNE=0, sumW=0;
#pragma omp parallel for private(r, c) shared(m) reduction(+:sumNW, sumN, sumNE, sumW)
   for(c=1; c<(nc-1); c++) {
     for(r=1; r<nr; r++) {
       if(m(r,c) != 255) {
         if(m(r-1,c-1) != 255)
           sumNW++;
         if(m(r-1,c) != 255)
           sumN++;
         if(m(r-1,c+1) != 255)
           sumNE++;
         if(m(r,c-1) != 255)
           sumW++;
       }
     }
   }

   int nSegments = sumNW + sumN + sumNE + sumW;

   NumericVector fromNodeRowVec(nSegments);
   NumericVector fromNodeColVec(nSegments);
   NumericVector toNodeRowVec(nSegments);
   NumericVector toNodeColVec(nSegments);
   NumericVector costVec(nSegments);

   int inc=0;

   for(c=1; c<(nc-1); c++) {
     for(r=1; r<nr; r++) {
       if(m(r,c) != 255) {
         if(m(r-1,c-1) != 255) {
           fromNodeRowVec(inc) = 1.0*r;
           fromNodeColVec(inc) = 1.0*c;
           toNodeRowVec(inc)   = 1.0*(r-1);
           toNodeColVec(inc)   = 1.0*(c-1);
           costVec(inc)        = sqrt2;
           inc++;
         }
         if(m(r-1,c) != 255) {
           fromNodeRowVec(inc) = 1.0*r;
           fromNodeColVec(inc) = 1.0*c;
           toNodeRowVec(inc)   = 1.0*(r-1);
           toNodeColVec(inc)   = 1.0*c;
           costVec(inc)        = 1.0;
           inc++;
         }
         if(m(r-1,c+1) != 255) {
           fromNodeRowVec(inc) = 1.0*r;
           fromNodeColVec(inc) = 1.0*c;
           toNodeRowVec(inc)   = 1.0*(r-1);
           toNodeColVec(inc)   = 1.0*(c+1);
           costVec(inc)        = sqrt2;
           inc++;
         }
         if(m(r,c-1) != 255) {
           fromNodeRowVec(inc) = 1.0*r;
           fromNodeColVec(inc) = 1.0*c;
           toNodeRowVec(inc)   = 1.0*r;
           toNodeColVec(inc)   = 1.0*(c-1);
           costVec(inc)        = 1.0;
           inc++;
         }
       }
     }
   }

   double scaleValue = 3600.0 * cellSize * linearUnitFactor * 2.0 / 1609.34;
   int j;

#pragma omp parallel for private(j)
   for(j=0; j<nSegments; j++) {
     costVec(j) = scaleValue * costVec(j) /
       (m(fromNodeRowVec(j), fromNodeColVec(j)) + m(toNodeRowVec(j), toNodeColVec(j)));
   }

   NumericVector terraFromID(nSegments);
   NumericVector terraToID(nSegments);

#pragma omp parallel for private(j)
   for(j=0; j<nSegments; j++) {
     terraFromID(j) = fromNodeRowVec(j) * nc + fromNodeColVec(j) + 1;
     terraToID(j)   = toNodeRowVec(j) * nc + toNodeColVec(j) + 1;
   }

   DataFrame nodeDF = DataFrame::create(Named("from") = terraFromID,
                                        Named("to")   = terraToID,
                                        Named("cost") = costVec);

   return(nodeDF);
 }
