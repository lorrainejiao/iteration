iteration
================
Xiaoluo Jiao
11/4/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1] -2.9577822 -0.8204193 -0.8450750 -0.2126382  0.6305993  1.5470064
    ##  [7]  0.5340271 -0.4409059 -0.5336899 -0.6833402  0.1447130  1.1461879
    ## [13]  0.4734176  1.6589100  0.1239669  0.3884132 -0.1968459  0.9038391
    ## [19] -0.2354576 -0.7665597 -0.1502152  1.1967150  0.1363070 -1.4707910
    ## [25]  0.4296177

``` r
z_scores = function(x) {
  
  z = (x - mean(x))/sd(x)
  
  return(z)

}

z_scores(x = x_vec)
```

    ##  [1] -2.9577822 -0.8204193 -0.8450750 -0.2126382  0.6305993  1.5470064
    ##  [7]  0.5340271 -0.4409059 -0.5336899 -0.6833402  0.1447130  1.1461879
    ## [13]  0.4734176  1.6589100  0.1239669  0.3884132 -0.1968459  0.9038391
    ## [19] -0.2354576 -0.7665597 -0.1502152  1.1967150  0.1363070 -1.4707910
    ## [25]  0.4296177

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -1.37021236 -0.45275554  0.37435105  0.41059356  1.93113972  1.66588780
    ##  [7]  0.05542176 -0.97449504 -0.36586245 -1.20785639  0.33409832 -1.52371170
    ## [13]  1.54973325 -0.93247764  0.44898207 -0.23138301 -1.88563022 -0.33305679
    ## [19] -0.49366565  1.11892891  0.05567701 -0.30029410 -0.37731680  0.54253097
    ## [25]  0.95381870 -0.11018520  1.37402821  1.90953305  0.78787172  0.03608992
    ## [31] -1.02957019  0.14941620  0.75699966 -1.38332253 -0.34183687  0.20224268
    ## [37] -0.02439403  0.53060604 -2.17171597  0.32179189

How great is this??

``` r
z_scores = function(x) {
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if(length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x))/sd(x)
  
  return(z)

}
```

``` r
z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is"))
```

    ## Error in z_scores(c("my", "name", "is")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if(length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)

}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.44  3.38

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.1 0.298
