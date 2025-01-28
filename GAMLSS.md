Generalized Additive Models for Location, Scale, and Shape (GAMLSS)
================
Joshua Edefo
2025-01-28

GAMLSS is a statistical framework that has great flexibility and power.
Some benefits include flexibility in distribution, modelling resulting
into multiple parameters, also works well with non-linear relationships
of variables and handles heterogeneity. However, it also comes with some
disadvantages, such as complexity, interpretability and software
availability

It is used to model the effect of number of seizures episodes (n_epil),
medication adherence (adh), and number of medications taken (n_med) on
quality of life (qol) of epilepsy patients

Libraries

``` r
library(gamlss)
```

    ## Warning: package 'gamlss' was built under R version 4.3.3

    ## Warning: package 'gamlss.data' was built under R version 4.3.3

    ## Warning: package 'gamlss.dist' was built under R version 4.3.3

``` r
library(usethis)
```

    ## Warning: package 'usethis' was built under R version 4.3.2

Data and preparation of data

``` r
data <- read.csv("C:/Users/joe62/OneDrive - Aberystwyth University/Apps/Desktop/Destop Folder/R code/ep.csv")

##Checking the structure
str(data)
```

    ## 'data.frame':    141 obs. of  4 variables:
    ##  $ n_epil: int  2 2 1 1 0 1 0 2 3 0 ...
    ##  $ n_med : int  3 2 1 2 2 2 2 2 3 2 ...
    ##  $ qol   : num  3.91 3.36 2.55 1.91 2.45 ...
    ##  $ adh   : int  1 3 2 5 4 5 3 4 4 7 ...

``` r
summary(data)
```

    ##      n_epil           n_med            qol             adh        
    ##  Min.   : 0.000   Min.   :1.000   Min.   :1.000   Min.   : 1.000  
    ##  1st Qu.: 0.000   1st Qu.:2.000   1st Qu.:1.636   1st Qu.: 4.000  
    ##  Median : 1.000   Median :2.000   Median :2.455   Median : 6.000  
    ##  Mean   : 1.312   Mean   :2.085   Mean   :2.420   Mean   : 5.922  
    ##  3rd Qu.: 2.000   3rd Qu.:2.000   3rd Qu.:3.091   3rd Qu.: 8.000  
    ##  Max.   :11.000   Max.   :4.000   Max.   :4.818   Max.   :18.000

``` r
## Remove observations with missing values

data = na.omit(data)
```

Fitting the GAMLSS model and model summary

``` r
model <- gamlss(
  qol ~ n_epil + adh + n_med,
  data = data,
  family = BCT
)
```

    ## GAMLSS-RS iteration 1: Global Deviance = 297.1069 
    ## GAMLSS-RS iteration 2: Global Deviance = 288.5957 
    ## GAMLSS-RS iteration 3: Global Deviance = 288.042 
    ## GAMLSS-RS iteration 4: Global Deviance = 288.0388 
    ## GAMLSS-RS iteration 5: Global Deviance = 288.0382

``` r
summary(model)
```

    ## Warning in summary.gamlss(model): summary: vcov has failed, option qr is used instead

    ## ******************************************************************
    ## Family:  c("BCT", "Box-Cox t") 
    ## 
    ## Call:  gamlss(formula = qol ~ n_epil + adh + n_med, family = BCT, data = data) 
    ## 
    ## 
    ## Fitting method: RS() 
    ## 
    ## ------------------------------------------------------------------
    ## Mu link function:  identity
    ## Mu Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  2.26117    0.23702   9.540  < 2e-16 ***
    ## n_epil       0.28306    0.04567   6.198 6.28e-09 ***
    ## adh         -0.08966    0.01997  -4.490 1.50e-05 ***
    ## n_med        0.10813    0.09656   1.120    0.265    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Sigma link function:  log
    ## Sigma Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -1.20974    0.05955  -20.32   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## Nu link function:  identity 
    ## Nu Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)
    ## (Intercept)  0.05458    0.21343   0.256    0.799
    ## 
    ## ------------------------------------------------------------------
    ## Tau link function:  log 
    ## Tau Coefficients:
    ##              Estimate Std. Error  t value Pr(>|t|)    
    ## (Intercept) 3.419e+01  8.422e-07 40603585   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## ------------------------------------------------------------------
    ## No. of observations in the fit:  141 
    ## Degrees of Freedom for the fit:  7
    ##       Residual Deg. of Freedom:  134 
    ##                       at cycle:  5 
    ##  
    ## Global Deviance:     288.0382 
    ##             AIC:     302.0382 
    ##             SBC:     322.6795 
    ## ******************************************************************

Summary of results Mu (Location) Coefficients: Intercept: 2.26117
(highly significant, p \< 2e-16). n_epil: 0.28306 (highly significant, p
\< 6.28e-09). adh: -0.08966 (highly significant, p \< 1.50e-05). n_med:
0.10813 (not significant, p = 0.265). Sigma (Scale) Coefficient:
Intercept: -1.20974 (highly significant, p \< 2e-16). Nu (Shape)
Coefficient: Intercept: 0.05458 (not significant, p = 0.799). Tau
(Shape) Coefficient: Intercept: 34.19 (highly significant, p \< 2e-16).

session information

``` r
sessionInfo()
```

    ## R version 4.3.1 (2023-06-16 ucrt)
    ## Platform: x86_64-w64-mingw32/x64 (64-bit)
    ## Running under: Windows 11 x64 (build 22631)
    ## 
    ## Matrix products: default
    ## 
    ## 
    ## locale:
    ## [1] LC_COLLATE=English_United Kingdom.utf8 
    ## [2] LC_CTYPE=English_United Kingdom.utf8   
    ## [3] LC_MONETARY=English_United Kingdom.utf8
    ## [4] LC_NUMERIC=C                           
    ## [5] LC_TIME=English_United Kingdom.utf8    
    ## 
    ## time zone: Europe/London
    ## tzcode source: internal
    ## 
    ## attached base packages:
    ## [1] parallel  splines   stats     graphics  grDevices utils     datasets 
    ## [8] methods   base     
    ## 
    ## other attached packages:
    ## [1] usethis_2.2.2     gamlss_5.4-22     nlme_3.1-162      gamlss.dist_6.1-1
    ## [5] gamlss.data_6.0-6
    ## 
    ## loaded via a namespace (and not attached):
    ##  [1] vctrs_0.6.5       cli_3.6.1         knitr_1.44        rlang_1.1.1      
    ##  [5] xfun_0.40         purrr_1.0.2       glue_1.6.2        htmltools_0.5.8.1
    ##  [9] rmarkdown_2.25    grid_4.3.1        evaluate_0.21     MASS_7.3-60      
    ## [13] fastmap_1.2.0     yaml_2.3.7        lifecycle_1.0.3   compiler_4.3.1   
    ## [17] fs_1.6.3          rstudioapi_0.15.0 lattice_0.21-8    digest_0.6.33    
    ## [21] magrittr_2.0.3    Matrix_1.6-1.1    tools_4.3.1       survival_3.7-0
