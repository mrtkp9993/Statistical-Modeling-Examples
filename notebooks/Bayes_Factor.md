Bayes Factors
================
Murat Koptur
25 Ağustos 2018

``` r
library(haven)
library(BayesFactor)
```

    ## Loading required package: coda

    ## Loading required package: Matrix

    ## ************
    ## Welcome to BayesFactor 0.9.12-4.2. If you have questions, please contact Richard Morey (richarddmorey@gmail.com).
    ## 
    ## Type BFManual() to open the manual.
    ## ************

``` r
scents <- read_spss("../data/scents.sav")
head(scents)
```

    ## # A tibble: 6 x 4
    ##    part sex       noscent scent
    ##   <dbl> <chr+lbl>   <dbl> <dbl>
    ## 1     1 1            27.7  30.6
    ## 2     2 2            57.2  43.3
    ## 3     3 1            57.9  53.4
    ## 4     4 1            38    37.4
    ## 5     5 1            57.9  48.6
    ## 6     6 2            32    35.5

``` r
scents$diffs <- scents$noscent - scents$scent
head(scents)
```

    ## # A tibble: 6 x 5
    ##    part sex       noscent scent  diffs
    ##   <dbl> <chr+lbl>   <dbl> <dbl>  <dbl>
    ## 1     1 1            27.7  30.6  -2.9 
    ## 2     2 2            57.2  43.3  13.9 
    ## 3     3 1            57.9  53.4   4.5 
    ## 4     4 1            38    37.4   0.6 
    ## 5     5 1            57.9  48.6   9.30
    ## 6     6 2            32    35.5  -3.5

``` r
bf <- ttestBF(scents$diffs)
bf
```

    ## Bayes factor analysis
    ## --------------
    ## [1] Alt., r=0.707 : 0.2294321 ±0.03%
    ## 
    ## Against denominator:
    ##   Null, mu = 0 
    ## ---
    ## Bayes factor type: BFoneSample, JZS

``` r
sprintf("Bayes factor: %f", exp(bf@bayesFactor$bf))
```

    ## [1] "Bayes factor: 0.229432"
