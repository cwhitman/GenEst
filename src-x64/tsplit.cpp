#include <Rcpp.h>
#include <cmath>

using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix calcRateC(NumericMatrix Mtilde, NumericMatrix Aj,
  NumericVector days, NumericMatrix searches){
// days = master search schedule with all search days
// times = vector of bin boundaries for temporal split
  const int nsim = Mtilde.ncol();
  const int x = Mtilde.nrow();
  const int nsearch = days.size() - 1;
  int xi, i, i0, i1;
// di = index into days for carcass xi [so c(1, 0, 1, 1, 0) -> c(0, 2, 3, 0, 0)]
  int di[x][nsearch + 1];
  for (xi = 0; xi < x; xi++){
    i = 0;
    for (int si = 0; si < days.size(); si++){
      if (int(searches(xi, si)) == 1){
        di[xi][i] = si;
        i++;
      }
    }
  }
  NumericMatrix rate(nsim, nsearch);
  double tmprate;
  for (int simi = 0; simi < nsim; simi++){
    for (int xi = 0; xi < x; xi++){
      i0 = int(Aj(xi, simi)) - 1;
      i1 = i0 + 1;
      tmprate = Mtilde(xi, simi)/(days[di[xi][i1]] - days[di[xi][i0]]);
      for (i = di[xi][i0]; i < di[xi][i1]; i++){
        rate(simi, i) += tmprate; 
      }
    }
  }
// need to prorate the rate for the unmonitored parts of the season
  return(rate);
}

// [[Rcpp::export]]
NumericMatrix calcTsplitC(NumericMatrix rate, NumericVector days, NumericVector times){
// rate = array of rates
// days = master search schedule with all search days
// tbins = vector of bin boundaries for temporal split
  const int nsim = rate.nrow();
  const int ntimes = times.size();
  int si;
  NumericMatrix splits(ntimes - 1, nsim);
  for (int simi = 0; simi < nsim; simi++){
    si = 1;
    for (int ti = 1; ti < ntimes; ti++){
      if (days[si] <= times[ti]){
        splits(ti - 1, simi) += rate(simi, si - 1) * (days[si] - times[ti - 1]);
        si++;
      }
      while (si < days.size() && days[si] <= times[ti]){
        splits(ti - 1, simi) += rate(simi, si - 1) * (days[si] - days[si - 1]);
        si++;
      }
      splits(ti - 1, simi) +=  rate(simi, si - 1) * (times[ti] - fmax(days[si - 1], times[ti - 1]));
    }
  }
  return(splits);
}

