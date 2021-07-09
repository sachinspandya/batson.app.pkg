#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double probcpp(bool cog, int c, int m, double d){
  if(cog){
    return log(c)+d-log(c*exp(d)+m);
  }else{
    return log(m)-log(c*exp(d)+m);
  }
}

// [[Rcpp::export]]
double Lk_cpp(double d,NumericMatrix x, bool L){
  double lglkr=0;
  for(int i=0; i < x.nrow();++i){
    lglkr = lglkr + probcpp(x(i,3),x(i,1),x(i,2)-x(i,1),d);
  }
  if(L){
    return lglkr;
  }else{
    return exp(lglkr);
  }
}

// [[Rcpp::export]]
double Priorcpp(double theta, double mu, double std) {
  return 1/(sqrt(2*M_PI)*std)*exp(-pow((theta-mu),2)/(2*pow(std,2)));
}

// [[Rcpp::export]]
List make_posterior(NumericMatrix x, int niter, 
                    double theta_start_val, double theta_proposal_sd, double prior_mean, double prior_sd){
  NumericVector theta(niter);
  double current_theta;
  double new_theta;
  double r;
  double thresh;
  double acpt;
  int acptn=0;
  
  theta[0] = theta_start_val;
  
  for(int i = 1; i < niter; ++i){
    
    current_theta = theta[i-1];
    
    new_theta = rnorm(1,current_theta, theta_proposal_sd)[0];
    
    
    r = log(Priorcpp(new_theta,prior_mean,prior_sd))+Lk_cpp(new_theta,x, 1)-log(Priorcpp(current_theta,prior_mean,prior_sd))-
      Lk_cpp(current_theta,x,1);
    
    thresh = log(runif(1)[0]);
    
    if(thresh<r){
      theta[i] = new_theta;
      acptn+=1;
      //std::cout << acptn << std::endl;
    } else{
      theta[i] = current_theta;
    }
    
    
  }
  //std::cout << acptn << std:: endl;
  acpt = static_cast<double>(acptn)/niter;
  //std::cout << acpt << std:: endl;
  return List::create(Named("theta")=theta,Named("accept_rate")=acpt);
}

// [[Rcpp::export]]
double Lk_cpp_p(double d,NumericMatrix x, bool L){
  double lglkr=0;
  for(int i=0; i < x.nrow();++i){
    lglkr = lglkr + probcpp(x(i,3),x(i,1),x(i,2)-x(i,1),d);
  }
  if(L){
    return lglkr;
  }else{
    return exp(lglkr);
  }
}

// [[Rcpp::export]]
double Priorcpp_p(double theta, double mu, double std,NumericMatrix x,double a0) {
  return pow(Lk_cpp_p(theta,x,0),a0)*1/(sqrt(2*M_PI)*std)*exp(-pow((theta-mu),2)/(2*pow(std,2)));
}

// [[Rcpp::export]]
List make_posterior_p(NumericMatrix x,NumericMatrix x_p,double a0, int niter, 
                      double theta_start_val, double theta_proposal_sd, double prior_mean, double prior_sd){
  NumericVector theta(niter);
  double current_theta;
  double new_theta;
  double r;
  double thresh;
  double acpt;
  int acptn=0;
  
  theta[0] = theta_start_val;
  
  for(int i = 1; i < niter; ++i){
    
    current_theta = theta[i-1];
    
    new_theta = rnorm(1,current_theta, theta_proposal_sd)[0];
    
    
    r = log(Priorcpp_p(new_theta,prior_mean,prior_sd,x_p,a0))+Lk_cpp_p(new_theta,x, 1)-log(Priorcpp_p(current_theta,prior_mean,prior_sd,x_p,a0))-
      Lk_cpp_p(current_theta,x,1);
    
    
    thresh = log(runif(1)[0]);
    
    
    
    if(thresh<r){
      theta[i] = new_theta;
      acptn+=1;
      //std::cout << acptn << std::endl;
    } else{
      theta[i] = current_theta;
    }
    
    
  }
  //std::cout << acptn << std:: endl;
  acpt = static_cast<double>(acptn)/niter;
  //std::cout << acpt << std:: endl;
  return List::create(Named("theta")=theta,Named("accept_rate")=acpt);
}

// [[Rcpp::export]]
List make_posterior_prior(NumericMatrix x_p,double a0, int niter, 
                          double theta_start_val, double theta_proposal_sd, double prior_mean, double prior_sd){
  NumericVector theta(niter);
  double current_theta;
  double new_theta;
  double r;
  double thresh;
  double acpt;
  int acptn=0;
  
  theta[0] = theta_start_val;
  
  for(int i = 1; i < niter; ++i){
    
    current_theta = theta[i-1];
    
    new_theta = rnorm(1,current_theta, theta_proposal_sd)[0];
    
    
    r = log(Priorcpp_p(new_theta,prior_mean,prior_sd,x_p,a0))-log(Priorcpp_p(current_theta,prior_mean,prior_sd,x_p,a0));
    
    thresh = log(runif(1)[0]);
    
    
    
    if(thresh<r){
      theta[i] = new_theta;
      acptn+=1;
      //std::cout << acptn << std::endl;
    } else{
      theta[i] = current_theta;
    }
    
    
  }
  //std::cout << acptn << std:: endl;
  acpt = static_cast<double>(acptn)/niter;
  //std::cout << acpt << std:: endl;
  return List::create(Named("theta")=theta,Named("accept_rate")=acpt);
}
