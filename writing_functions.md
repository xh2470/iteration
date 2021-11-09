writing\_functions
================
Xueqing Huang
2021/11/4

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.4     v dplyr   1.0.7
    ## v tidyr   1.1.3     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## 载入程辑包：'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.60444380 -1.57609099 -1.82291314  1.33076536  0.45563334 -1.07521407
    ##  [7]  1.92438727 -0.61406526  0.12574037  0.35750895 -0.44810761 -0.15122114
    ## [13]  0.14478888  0.01855205 -1.06457996  0.43155796  0.79828095  0.17241015
    ## [19] -1.97526028  0.42311456  0.57746637  1.59299397  0.47289033 -0.98985627
    ## [25]  0.28677441

``` r
z_scores = function(x) {
  
  z = (x - mean(x)) / sd(x)
  return(z)
  
}

z_scores(x_vec)
```

    ##  [1]  0.60444380 -1.57609099 -1.82291314  1.33076536  0.45563334 -1.07521407
    ##  [7]  1.92438727 -0.61406526  0.12574037  0.35750895 -0.44810761 -0.15122114
    ## [13]  0.14478888  0.01855205 -1.06457996  0.43155796  0.79828095  0.17241015
    ## [19] -1.97526028  0.42311456  0.57746637  1.59299397  0.47289033 -0.98985627
    ## [25]  0.28677441

``` r
y_vec = rnorm(40,mean = 12, sd = .3)
z_scores(y_vec)
```

    ##  [1] -0.85299523  0.26034667  0.54652531  1.44379870  0.06091269 -1.08909811
    ##  [7]  0.63648451  0.45709497  1.84821945 -0.16890248  0.54337578  0.86133888
    ## [13] -0.03383011 -0.27068187 -1.70533133  1.51377755  1.05469044  0.53875450
    ## [19] -0.09190024 -1.91698297 -1.33591672  0.05579991  1.86067088  0.33048184
    ## [25] -0.82117171  0.71118110  0.17565428  0.25445430 -0.57167297  1.00279209
    ## [31] -0.30835767  0.74122310 -1.11239040  0.39015384 -1.77308893 -1.09312622
    ## [37]  0.18583030 -0.07257377 -0.06657928 -2.18896105

How great is this?

``` r
z_scores(3)
```

    ## [1] NA

``` r
z_scores("my name is jeff")
```

    ## Warning in mean.default(x): 参数不是数值也不是逻辑值：回覆NA

    ## Error in x - mean(x): 二进列运算符中有非数值参数

``` r
mtcars
```

    ##                      mpg cyl  disp  hp drat    wt  qsec vs am gear carb
    ## Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4    4
    ## Mazda RX4 Wag       21.0   6 160.0 110 3.90 2.875 17.02  0  1    4    4
    ## Datsun 710          22.8   4 108.0  93 3.85 2.320 18.61  1  1    4    1
    ## Hornet 4 Drive      21.4   6 258.0 110 3.08 3.215 19.44  1  0    3    1
    ## Hornet Sportabout   18.7   8 360.0 175 3.15 3.440 17.02  0  0    3    2
    ## Valiant             18.1   6 225.0 105 2.76 3.460 20.22  1  0    3    1
    ## Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3    4
    ## Merc 240D           24.4   4 146.7  62 3.69 3.190 20.00  1  0    4    2
    ## Merc 230            22.8   4 140.8  95 3.92 3.150 22.90  1  0    4    2
    ## Merc 280            19.2   6 167.6 123 3.92 3.440 18.30  1  0    4    4
    ## Merc 280C           17.8   6 167.6 123 3.92 3.440 18.90  1  0    4    4
    ## Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3    3
    ## Merc 450SL          17.3   8 275.8 180 3.07 3.730 17.60  0  0    3    3
    ## Merc 450SLC         15.2   8 275.8 180 3.07 3.780 18.00  0  0    3    3
    ## Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3    4
    ## Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3    4
    ## Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3    4
    ## Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4    1
    ## Honda Civic         30.4   4  75.7  52 4.93 1.615 18.52  1  1    4    2
    ## Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4    1
    ## Toyota Corona       21.5   4 120.1  97 3.70 2.465 20.01  1  0    3    1
    ## Dodge Challenger    15.5   8 318.0 150 2.76 3.520 16.87  0  0    3    2
    ## AMC Javelin         15.2   8 304.0 150 3.15 3.435 17.30  0  0    3    2
    ## Camaro Z28          13.3   8 350.0 245 3.73 3.840 15.41  0  0    3    4
    ## Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3    2
    ## Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4    1
    ## Porsche 914-2       26.0   4 120.3  91 4.43 2.140 16.70  0  1    5    2
    ## Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5    2
    ## Ford Pantera L      15.8   8 351.0 264 4.22 3.170 14.50  0  1    5    4
    ## Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5    6
    ## Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5    8
    ## Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4    2

``` r
z_scores(mtcars)
```

    ## Warning in mean.default(x): 参数不是数值也不是逻辑值：回覆NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE))
```

    ## [1]  0.5  0.5 -1.5  0.5

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  z = (x - mean(x)) / sd(x)
  
  z
}
```

## Multiple outputs

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(mean = mean_x, 
       sd = sd_x)
}

mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.84  3.06

``` r
mean_and_sd(y_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.9 0.312

## Different sample sizes, means, sds

``` r
sim_data = tibble(
  x = rnorm(30, mean = 2, sd = 3)
)

sim_data %>% 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   1.99      3.44

## write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu, sigma) {
  
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  sim_data %>% 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}

sim_mean_sd(n = 100, mu = 6, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   6.17      3.33

``` r
sim_mean_sd(mu = 6, n = 100, sigma = 3)
```

    ## # A tibble: 1 x 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   5.67      3.01

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim()
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
vec_urls = str_c(url_base, 1:5)

dynamite_reviews = bind_rows(
  read_page_reviews(vec_urls[1]),
  read_page_reviews(vec_urls[2]),
  read_page_reviews(vec_urls[3]),
  read_page_reviews(vec_urls[4]),
  read_page_reviews(vec_urls[5])
)

dynamite_reviews
```

    ## # A tibble: 50 x 3
    ##    title                                                 stars text             
    ##    <chr>                                                 <dbl> <chr>            
    ##  1 Vintage                                                   5 Easy to order. I~
    ##  2 too many commercials                                      1 5 minutes into t~
    ##  3 this film is so good!                                     5 VOTE FOR PEDRO!  
    ##  4 Good movie                                                5 Weird story, goo~
    ##  5 I Just everyone to know this....                          5 VOTE FOR PEDRO !~
    ##  6 the cobweb in his hair during the bike ramp scene lol     5 5 stars for bein~
    ##  7 Best quirky movie ever                                    5 You all know the~
    ##  8 Classic Film                                              5 Had to order thi~
    ##  9 hehehehe                                                  5 goodjobboys      
    ## 10 Painful                                                   1 I think I sneeze~
    ## # ... with 40 more rows

``` r
f = function(x) {
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

``` r
x_vec = rnorm(25, 0, 1)

my_summary = function(x, summ_func) {
  summ_func(x)
}

my_summary(x_vec, sd)
```

    ## [1] 0.9093032
