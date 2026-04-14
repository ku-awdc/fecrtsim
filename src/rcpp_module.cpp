#include <Rcpp.h>

// [[Rcpp::depends(bayescount)]]

int test()
{
  return 0;
}

RCPP_MODULE(fecrtsim_module){

	using namespace Rcpp;

	function("Rcpp_test", &test);

}
