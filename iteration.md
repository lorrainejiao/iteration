iteration
================
Xiaoluo Jiao
11/4/2021

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1] -0.43350892  0.58506859 -0.14248882  0.24407785  0.41272703  0.41648084
    ##  [7] -0.49981671  1.41232318  0.73046579 -0.24687751  1.17607995  0.89985522
    ## [13] -1.44149839  2.07626444 -0.79901978 -0.49893370  0.89557105 -0.64858899
    ## [19] -1.41625939 -1.26147337 -1.00014975  1.13036255 -0.04292257 -1.87853969
    ## [25]  0.33080110

``` r
z_scores = function(x) {
  
  z = (x - mean(x))/sd(x)
  
  return(z)

}

z_scores(x = x_vec)
```

    ##  [1] -0.43350892  0.58506859 -0.14248882  0.24407785  0.41272703  0.41648084
    ##  [7] -0.49981671  1.41232318  0.73046579 -0.24687751  1.17607995  0.89985522
    ## [13] -1.44149839  2.07626444 -0.79901978 -0.49893370  0.89557105 -0.64858899
    ## [19] -1.41625939 -1.26147337 -1.00014975  1.13036255 -0.04292257 -1.87853969
    ## [25]  0.33080110

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1] -0.6944946007  1.2592702269 -0.6301750620 -0.5257185338 -0.9646615279
    ##  [6] -2.2970294371  0.2536797272  0.6831070797 -0.3083426168 -1.1920081392
    ## [11]  1.1423694256 -0.7912404813  0.0873765304 -0.0330571779  0.9946890523
    ## [16] -1.1677200295 -0.1168214290  0.0008314091  0.0799343920 -0.3639282706
    ## [21] -1.0410744552  0.1673317128 -0.0260522778  0.4191776459  0.6624383287
    ## [26] -0.6940050550  1.5320292346  0.5160536481  1.0381629769  1.3214224414
    ## [31] -2.2183924625 -0.4801442197  1.0037529897 -1.5256593467  1.3366027849
    ## [36] -0.3186296234 -0.1191482554 -0.1142669048  2.1235275903  1.0008127098

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
    ## 1  5.72  3.44

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.280

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
    ## 1  1.60  2.76

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
    ## 1  3.43  3.16

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.36  3.21

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Okay but there are a lot of pages of reviews…

Write a function that gets reviews based on page url

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)
  
  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  
  return(reviews)
}

get_page_reviews(url)
```

    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…

``` r
url_2 = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

get_page_reviews(url_2)
```

    ## # A tibble: 10 × 3
    ##    title                                       stars              text          
    ##    <chr>                                       <chr>              <chr>         
    ##  1 Good funny                                  3.0 out of 5 stars "\n  Would re…
    ##  2 Not available w/in 48 hour window           1.0 out of 5 stars "\n  I couldn…
    ##  3 Your mom went to college.                   5.0 out of 5 stars "\n  Classic …
    ##  4 Very funny movie                            5.0 out of 5 stars "\n  I watch …
    ##  5 Watch it twice! Trust me!                   5.0 out of 5 stars "\n  Nothing …
    ##  6 A classic                                   5.0 out of 5 stars "\n  If you d…
    ##  7 Can't say how many times I've seen          5.0 out of 5 stars "\n  Such a g…
    ##  8 I pity the fool who doesn’t own this movie. 5.0 out of 5 stars "\n  I love t…
    ##  9 I don’t know why it’s so popular!           2.0 out of 5 stars "\n  My girlf…
    ## 10 Okay                                        3.0 out of 5 stars "\n  Okay\n"

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
get_page_reviews(urls[1]),
get_page_reviews(urls[2]),
get_page_reviews(urls[3]),
get_page_reviews(urls[4]),
get_page_reviews(urls[5]))
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…
    ## # … with 40 more rows
