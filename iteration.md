iteration
================
Xiaoluo Jiao
11/4/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1]  1.31796314  0.65362096 -1.35986912  0.18435836  1.05018658  0.23221959
    ##  [7] -0.06251536  0.55556428 -3.30496576 -0.50027726 -0.11190763  1.26901268
    ## [13]  0.67062261 -0.30870418 -0.19595233  1.19427289 -0.55949007 -0.30264122
    ## [19] -0.36931971  0.26643475  0.21380097 -0.42358724 -0.50486015  1.31761147
    ## [25] -0.92157825

``` r
z_scores = function(x) {
  
  z = (x - mean(x))/sd(x)
  
  return(z)

}

z_scores(x = x_vec)
```

    ##  [1]  1.31796314  0.65362096 -1.35986912  0.18435836  1.05018658  0.23221959
    ##  [7] -0.06251536  0.55556428 -3.30496576 -0.50027726 -0.11190763  1.26901268
    ## [13]  0.67062261 -0.30870418 -0.19595233  1.19427289 -0.55949007 -0.30264122
    ## [19] -0.36931971  0.26643475  0.21380097 -0.42358724 -0.50486015  1.31761147
    ## [25] -0.92157825

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -0.55230927 -0.89936283 -1.34436032 -1.30785667 -0.34682927 -2.40855696
    ##  [7]  0.93149295 -0.57397570  1.00198038  0.23045214 -0.46577003 -0.55550844
    ## [13] -0.33811357  1.72032294  0.44127627  1.07243538  1.92863688 -0.56001899
    ## [19] -0.70245847  0.92550740  1.29429263  0.25502119 -1.29975914 -0.21344791
    ## [25] -0.90657578  0.16052647 -1.11791391  0.24182268 -0.42092915  0.92532191
    ## [31]  0.30426315  0.19138125 -0.45759601 -0.02963119  1.53124227  0.12198727
    ## [37] -0.12893150  2.23695585  0.29556136 -1.18057523

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
    ## 1  5.53  3.64

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.314

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x), 
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.39  2.77

Let’s write a function that simulates data, computes the mean and sd

``` r
sim_mean_sd = function(n, mu = 4, sigma = 3) { # default values, can be updated later
  
  # do checks on inputs
  
  sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )
  
  sim_data %>% 
    summarize(
      mean = mean(x), 
      sd = sd(x)
    )
}

sim_mean_sd(30, 4, 3)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.39  3.14

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.38  2.67
