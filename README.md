
Data obtained from [GSS Data
Explorer](https://gssdataexplorer.norc.org/); you can also access it in
within [this
repository](https://raw.githubusercontent.com/kwlyu/stat230-f24-final_project/main/GSS_commute_happiness.csv).

## Data Wrangling

``` r
happiness_raw <- read_csv("GSS_commute_happiness.csv")

happiness_cleaned <- happiness_raw %>%
  select(year, happy, commute, realrinc, educ, race, gender1) %>% 
  filter(commute != ".i:  Inapplicable",
         realrinc > 0,
         happy != ".n:  No answer") %>% 
  mutate(
    educ = case_when(
      str_detect(educ, "grade") ~ as.numeric(str_extract(educ, "\\d+")),
      str_detect(educ, "college") ~ as.numeric(str_extract(educ, "\\d+")) + 12,
      str_detect(educ, "No formal schooling") ~ 0,
      TRUE ~ NA
    ),
    commute = case_when(
      str_detect(commute, "\\d+") ~ as.numeric(str_extract(commute, "\\d+")),
      TRUE ~ NA
    ),
    race = if_else(race == "White", "White", "Non White"),
    gender = if_else(gender1 == "MALE", "Male", "Female")
  ) %>% 
  select(-gender1)

happiness_recode <- happiness_cleaned %>% 
  mutate(happy = if_else(happy == "Not too happy", 0, 1)) %>% 
  drop_na() 

write.csv(happiness_recode, file = "happiness_recode.csv")
```

## EDA

``` r
ggpairs(happiness_cleaned)
```

![](README_files/figure-gfm/Boxplots%20and%20Barplots-1.png)<!-- -->

``` r
# Density plot for happiness by commute time
ggplot(happiness_cleaned, aes(x = happy, y = commute, fill = happy)) +
  geom_boxplot() +
  labs(title = "Commute Time Distribution by Happiness Level", 
       x = "Commute Time (minutes)", 
       fill = "Happiness Level")
```

![](README_files/figure-gfm/Boxplots%20and%20Barplots-2.png)<!-- -->

``` r
# Boxplot of income by happiness level
ggplot(happiness_cleaned, aes(x = happy, y = realrinc, fill = happy)) +
  geom_boxplot() +
  labs(title = "Income Distribution by Happiness Level", 
       x = "Happiness Level", 
       y = "Real Income",
       fill = "Happiness Level")
```

![](README_files/figure-gfm/Boxplots%20and%20Barplots-3.png)<!-- -->

``` r
# Bar plot of happiness level by education level
ggplot(happiness_cleaned, aes(x = educ, fill = happy)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Happiness Levels by Education Level", 
       x = "Education Level", 
       y = "Proportion",
       fill = "Happiness Level") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
```

![](README_files/figure-gfm/Boxplots%20and%20Barplots-4.png)<!-- -->

``` r
# Faceted bar plot for happiness levels by race and gender
ggplot(happiness_cleaned, aes(x = gender, fill = happy)) +
  geom_bar(position = "fill") +
  facet_wrap(~ race) +
  labs(title = "Proportion of Happiness by Race and Gender", 
       x = "Gender", 
       y = "Proportion",
       fill = "Happiness Level")
```

![](README_files/figure-gfm/Boxplots%20and%20Barplots-5.png)<!-- -->

## Logistic Regression

``` r
happiness_glm <- glm(happy ~ commute + realrinc + educ + race + gender, 
                     data = happiness_recode, family = quasibinomial)

summary(happiness_glm)
```

    ## 
    ## Call:
    ## glm(formula = happy ~ commute + realrinc + educ + race + gender, 
    ##     family = quasibinomial, data = happiness_recode)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)   
    ## (Intercept)  1.221e+00  6.479e-01   1.884  0.05987 . 
    ## commute     -1.330e-03  7.569e-03  -0.176  0.86061   
    ## realrinc     3.730e-05  1.188e-05   3.140  0.00174 **
    ## educ        -2.136e-04  4.661e-02  -0.005  0.99635   
    ## raceWhite    1.111e-02  3.345e-01   0.033  0.97352   
    ## genderMale   7.433e-01  2.526e-01   2.943  0.00334 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for quasibinomial family taken to be 1.013288)
    ## 
    ##     Null deviance: 530.41  on 873  degrees of freedom
    ## Residual deviance: 503.81  on 868  degrees of freedom
    ## AIC: NA
    ## 
    ## Number of Fisher Scoring iterations: 6
