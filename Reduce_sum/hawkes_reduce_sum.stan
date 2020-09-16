functions {
  real partial_sum_1(real[] ll_slice,
                   int start, int end) {
    return sum(log(ll_slice));
  }

  real partial_sum_2(real[] timeD2_slice,
                   int start, int end,
                   real lengthscaleT) {
    return sum(exp(to_vector(timeD2_slice)*lengthscaleT)); 
  }

  vector gaussian(row_vector x, real lengthscale) {
    return(1/(2*pi())*exp(-.5 * rows_dot_self(x)/lengthscale^2)/lengthscale^2);
  }
}

data {
  int<lower=1> n;
  int hour[n];
  matrix[n,2] Space;
  real Time[n]; // NB: these must be sorted from smallest to largest!
  real time_window;
  real space_window;

  vector[n] muST;
  real muSTintegral;
}

transformed data {
  matrix[n,n] timeD;
  matrix[n,n] spaceD;
  real timeD2[n];
  for(i in 1:n) {
    for(j in 1:n) {
      timeD[i,j] = -(Time[i] - Time[j]);
      spaceD[i,j] = distance(Space[i], Space[j]);
    }
    timeD2[i] = -(time_window - Time[i]); 
  }
}

parameters {
  real<lower=0> lengthscaleS;
  real<lower=0> lengthscaleT;
  real<lower=0> a;
  real<lower=0> mu;
  real<lower=0> mu0;
}

transformed parameters {
  real ll[n];
  //real lp;
  ll = to_array_1d(mu0 + muST * mu);
  for(i in 2:n) { 
    //for(j in 1:(i-1)) {
      ll[i] = a * lengthscaleT * lengthscaleT * dot_product(sub_row(exp(timeD),i,1,i-1), head(gaussian(spaceD[i,],lengthscaleS), i-1));
      //ll[i] = ll[i] + (a * lengthscaleT * exp(timeD[i,j] * lengthscaleT) * gaussian(spaceD[i,j],lengthscaleS));
    }
  lp = reduce_sum(partial_sum_1,ll,1) - muSTintegral * mu - mu0 * space_window * time_window 
  + a * (reduce_sum(partial_sum_2,timeD2,1,lengthscaleT) - n);
  //lp = sum(log(ll)) - muSTintegral * mu - mu0 * space_window * time_window + a * (sum(exp(timeD2*lengthscaleT)) - n);
}

model {
  target += lp
  lengthscaleS ~ normal(0,10);
  lengthscaleT ~ normal(0,10);
  a ~  normal(0,10);
  mu0 ~ normal(0,1);
  mu ~ normal(0,1);
}

generated quantities {
  vector[n] background;
  real lengthscale_minutes;
  lengthscale_minutes = 24*60/lengthscaleT;
  background = (mu0 + muST * mu ) ./ to_vector(ll);
}

