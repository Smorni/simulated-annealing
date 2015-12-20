#include <RcppArmadilloExtensions/sample.h>
#include <Rcpp.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// [[Rcpp::export]]
List ska(NumericVector objWeight,
         NumericVector objValue,
         float maxWeight, 
         float tau,
         int N) {
  // m .... weight of objects
  // V .... value of objects
  // M .... maximum weight allowed
  // tau .. starting temperature
  // N .... number of iterations
  
  RNGScope scope;
  
  LogicalVector subsetOther;
  LogicalVector subsetTemp;
  
  int nObjs;
  IntegerVector Objs;
  
  nObjs = objWeight.length();
  Objs = seq_len(nObjs);
  
  IntegerVector curObj = IntegerVector::create(RcppArmadillo::sample(Objs, 1, false)[0]);
  float curValue;
  float curWeight;
  
  NumericVector tempVector;
  IntegerVector tempIn;
  IntegerVector tempOut;
  float tempSum;
  
  subsetTemp = in(Objs, curObj);
  
  tempVector = objValue[subsetTemp];
  curValue = std::accumulate(tempVector.begin(), tempVector.end(), 0.0);
  
//  Rcout << curObj << '\n' << curValue << '\n';
  
  tempVector = objWeight[subsetTemp];
  curWeight = std::accumulate(tempVector.begin(), tempVector.end(), 0.0);
  
  IntegerVector bestObj;
  float bestValue;
  float bestWeight;
  
  bestObj = curObj;
  bestValue = curValue;
  bestWeight = curWeight;
  
  IntegerVector tempObj;
  float tempValue;
  float tempWeight;
  
  float testLHS;
  float testRHS;
  
  IntegerVector otherObj;
  
  for (int i = 0; i < N; i++) {
    tempObj = curObj;
    
    subsetTemp = in(Objs, tempObj);
    tempVector = objWeight[subsetTemp];
    tempSum = std::accumulate(tempVector.begin(), tempVector.end(), 0.0);
    
//    Rcout <<'\n' << i << '\n' << tempObj << '\n' << tempSum << '\n';

    while (tempSum <= maxWeight) {
      // adding random objects until over maxWeight
      subsetOther = !in(Objs, tempObj);
      otherObj = Objs[subsetOther];
      
      tempIn = RcppArmadillo::sample(otherObj, 1, false);
      tempObj.push_back(tempIn[0]);
      
      subsetTemp = in(Objs,tempObj);
      tempVector = objWeight[subsetTemp];
      tempSum = std::accumulate(tempVector.begin(), tempVector.end(), 0.0);
    }
    
//    Rcout << tempObj << '\n';

    
    while (tempSum > maxWeight) {
      // removing random objects until under maxWeight
      tempOut = RcppArmadillo::sample(tempObj, 1, false);
      
      subsetTemp = !in(tempObj, tempOut);
      tempObj = tempObj[subsetTemp];
      
      subsetTemp = in(Objs, tempObj);
      tempVector = objWeight[subsetTemp];
      tempSum = std::accumulate(tempVector.begin(), tempVector.end(), 0.0);
    }
    
    // Rcout << tempObj << '\n';
    // calculate the values for this iteration
    tempWeight = tempSum;
    
    subsetTemp = in(Objs, tempObj);
    tempVector = objValue[subsetTemp];
    tempValue = std::accumulate(tempVector.begin(), tempVector.end(), 0.0);
    
    // Rcout << tempValue << '\n' << bestValue << '\n';
    
    if (tempValue >= bestValue) {
      bestObj = tempObj;
      bestValue = tempValue;
      bestWeight = tempWeight;
      
//       curObj = tempObj;
//       curValue = tempValue;
    }
    
    // else {
      // candidate acceptance
      testLHS = R::runif(0,1);
      testRHS = exp((curValue - tempValue) / (tau / i));
      
      if(testLHS < testRHS) {
        curObj = tempObj;
        curValue = tempValue;
      // }
    }
  }
  
  return List::create(_["Objects"] = bestObj,
                      _["Total_value"] = bestValue,
                      _["Total_weight"] = bestWeight);
}

