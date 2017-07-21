    #Import scale data

    #Load readr
    library(readxl)
    # Read scale data from local, and import into using read_excel to convert into a dataframe
    scale_df <- read_excel("C:/Users/ifeoma/Google Drive/HONOURS/PROJECTS/ESSENTIAL INFO/Skull analysis/Data collection and Tests/Standardizing area technique/Significance.xlsx")

    # Exlpore the contents of scale_df
    dim(scale_df)

    ## [1] 14  3

    colnames(scale_df)

    ## [1] "Distance_cm"  "Reality_mm2"  "Observed_mm2"

    head(scale_df)

    ## # A tibble: 6 x 3
    ##   Distance_cm Reality_mm2 Observed_mm2
    ##         <dbl>       <dbl>        <dbl>
    ## 1         0.0      34.478       34.478
    ## 2         0.5      34.478       33.468
    ## 3         1.0      34.478       32.296
    ## 4         1.5      34.478       32.053
    ## 5         2.0      34.478       31.253
    ## 6         2.5      34.478       31.130

    tail(scale_df)

    ## # A tibble: 6 x 3
    ##   Distance_cm Reality_mm2 Observed_mm2
    ##         <dbl>       <dbl>        <dbl>
    ## 1         4.0      34.478       28.711
    ## 2         4.5      34.478       28.144
    ## 3         5.0      34.478       27.657
    ## 4         5.5      34.478       26.907
    ## 5         6.0      34.478       26.011
    ## 6         6.5      34.478       25.439

    summary(scale_df)

    ##   Distance_cm     Reality_mm2     Observed_mm2  
    ##  Min.   :0.000   Min.   :34.48   Min.   :25.44  
    ##  1st Qu.:1.625   1st Qu.:34.48   1st Qu.:27.78  
    ##  Median :3.250   Median :34.48   Median :29.76  
    ##  Mean   :3.250   Mean   :34.48   Mean   :29.79  
    ##  3rd Qu.:4.875   3rd Qu.:34.48   3rd Qu.:31.85  
    ##  Max.   :6.500   Max.   :34.48   Max.   :34.48

    #change from wide to long format for ggplot2 
    library(dplyr)

    ## Warning: package 'dplyr' was built under R version 3.4.1

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(tidyr)
    sdf <- scale_df %>%
      gather(key = error, value = area_mm2, -Distance_cm)
    #plot 
    library(ggplot2)

    # plot on same grid, each series colored differently -- 
    scaleplot <- ggplot(sdf, aes(Distance_cm, area_mm2)) +
      geom_point(aes(colour = error)) + #add title 
      ggtitle("Scale bar error")

    # save plot 
    ggsave(filename = "scaleplot.pdf", plot = scaleplot) 

    ## Saving 7 x 5 in image

    #call up plot 
    scaleplot

![](Scale_bar_files/figure-markdown_strict/Scale%20bar%20error%20trials-1.png)

     # Wilcoxon signed-rank test (a non-parametric test for paired oridnal data)
        #Assumptions:
          #1. Dependent samples - the two samples need to be dependent observations           of the cases. 
          #2. Independence - The Wilcoxon sign test assumes independence, meaning             that the paired observations are randomly and independently drawn.
          #3. Continuous dependent variable - To account for the fact that in most              cases the dependent variable is binominal distributed, a continuity               correction is applied.
          #4. Ordinal level of measurement - The Wilcoxon sign test needs both                  dependent measurements to be at least of ordinal scale. 

    # H0:  median difference between observed & reality is zero
    # H1: median difference between observed & reality is not equal to zero

    scale_test <- wilcox.test(area_mm2 ~ error,
                data = sdf,
                paired = TRUE,
                exact = NULL, correct = TRUE,
                conf.int = TRUE, conf.level = 0.95)

    ## Warning in wilcox.test.default(x = c(34.478, 33.468, 32.296, 32.053,
    ## 31.253, : cannot compute exact p-value with zeroes

    ## Warning in wilcox.test.default(x = c(34.478, 33.468, 32.296, 32.053,
    ## 31.253, : cannot compute exact confidence interval with zeroes

    #print out test 
    scale_test

    ## 
    ##  Wilcoxon signed rank test with continuity correction
    ## 
    ## data:  area_mm2 by error
    ## V = 0, p-value = 0.001662
    ## alternative hypothesis: true location shift is not equal to 0
    ## 95 percent confidence interval:
    ##  -6.820991 -3.286551
    ## sample estimates:
    ## (pseudo)median 
    ##      -5.022965

    #Definition of a confidence interval:
    #Interval in which the true population mean will fall with a certain probability

    #creat diff
    diff_df <- scale_df %>%
      mutate(O_R = Observed_mm2 - Reality_mm2)

    #Print
    diff_df

    ## # A tibble: 14 x 4
    ##    Distance_cm Reality_mm2 Observed_mm2    O_R
    ##          <dbl>       <dbl>        <dbl>  <dbl>
    ##  1         0.0      34.478       34.478  0.000
    ##  2         0.5      34.478       33.468 -1.010
    ##  3         1.0      34.478       32.296 -2.182
    ##  4         1.5      34.478       32.053 -2.425
    ##  5         2.0      34.478       31.253 -3.225
    ##  6         2.5      34.478       31.130 -3.348
    ##  7         3.0      34.478       30.323 -4.155
    ##  8         3.5      34.478       29.190 -5.288
    ##  9         4.0      34.478       28.711 -5.767
    ## 10         4.5      34.478       28.144 -6.334
    ## 11         5.0      34.478       27.657 -6.821
    ## 12         5.5      34.478       26.907 -7.571
    ## 13         6.0      34.478       26.011 -8.467
    ## 14         6.5      34.478       25.439 -9.039

    # Linear regression 

    #Null Hypothesis: The coefficients associated with the variables is equal to zero. 
    #Alternate hypothesis: The coefficients are not equal to zero (i.e. there exists a relationship between the independent variable in question and the dependent variable).  

    # Assumptions: 

    # There is a linear trend between x and y.
    # The observations in the sample are independent.
    # x is measured without error.
    # The residuals are normally distributed .
    # The residuals have the same variance for all fitted values of y 

    # Diagnostic plot 1: Quantile-Quantile plot (QQ-plot) 
    plot(x = diff_df$Distance_cm, y = diff_df$O_R)
    abline(h = 0)

![](Scale_bar_files/figure-markdown_strict/Scale%20bar%20error%20trials-2.png)

    #Heteroskedasticity 

    # Diagnostic plot 2: Fitted vs residuals plot
    qqnorm(diff_df$O_R)
    qqline(diff_df$O_R)

![](Scale_bar_files/figure-markdown_strict/Scale%20bar%20error%20trials-3.png)

    #Skewed residual distribution 


    linscale <- glm(O_R ~ Distance_cm, family = 
                      gaussian(), 
                    data = diff_df)
    summary(linscale)

    ## 
    ## Call:
    ## glm(formula = O_R ~ Distance_cm, family = gaussian(), data = diff_df)
    ## 
    ## Deviance Residuals: 
    ##      Min        1Q    Median        3Q       Max  
    ## -0.48815  -0.10985  -0.00435   0.17463   0.36311  
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) -0.36311    0.12387  -2.932   0.0126 *  
    ## Distance_cm -1.33073    0.03239 -41.085  2.8e-14 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for gaussian family taken to be 0.05966624)
    ## 
    ##     Null deviance: 101.43327  on 13  degrees of freedom
    ## Residual deviance:   0.71599  on 12  degrees of freedom
    ## AIC: 4.1063
    ## 
    ## Number of Fisher Scoring iterations: 2

    #Regression coefficients:
    #Intercept = -0.36, t(13) = -2.93, p-value < 0.05
    #Distance_cm = -1.33, t(13) = 0.-41.09, p-value < 0.001

    #Interpretation:
      #As the p-value for regression coefficients Intercept (p-value < 0.001) & interest_rate (p-value ) we reject the null hypothesis, thus the coefficients are not equal to zero (i.e. there exists a relationship between the independent variable in question and the dependent variable).  


    # Correlation assesses the linear association or strength of relationship between two variables.

    #Spearman’s rank correlation: 

    # H0: There is no signifiant association between the two variables.
    # H1: There is a significant association between the two variables.

    # Assumptions:
    #Variables measured on an ordinal or interval or ratio scale
    #There is a monotonic relationship between the variables

    cor_scale <- with(diff_df,
    cor.test(x = Distance_cm, y = O_R,
        method = 'spearman'))
    # rho(13) = -1,  p-value < 0.001 
    # Interpretation 
    # As p-value < 0.001, we reject H0, thus there is significant association between the two variables

    cor_scale

    ## 
    ##  Spearman's rank correlation rho
    ## 
    ## data:  Distance_cm and O_R
    ## S = 910, p-value < 2.2e-16
    ## alternative hypothesis: true rho is not equal to 0
    ## sample estimates:
    ## rho 
    ##  -1
