# CHARLS

[CHARLS](http://charls.pku.edu.cn/)是中国中老年人调查数据，由北大发起的关于中国中老年人的社会调查。


## 数据导入



```r
library(haven)
```



```r
getwd()
```

```
## [1] "/Users/a182501/project_set/rrp"
```

```r
charls2018cogn <- read_dta("/Volumes/Expansion/micro-base-data/charls/2018/Cognition.dta")
head(charls2018cogn)
```

```
## # A tibble: 6 × 219
##   ID        householdID communityID dc001_w4 dc002_w4 dc003_w4 dc005_w4 dc006_w4
##   <chr>     <chr>       <chr>       <dbl+lb> <dbl+lb> <dbl+lb> <dbl+lb> <dbl+lb>
## 1 09400411… 0940041130  0940041     1 [1 Co… 1 [1 Co… 5 [5 Er… 1 [1 Co… 1 [1 Co…
## 2 09400411… 0940041110  0940041     1 [1 Co… 1 [1 Co… 1 [1 Co… 1 [1 Co… 1 [1 Co…
## 3 09400411… 0940041110  0940041     1 [1 Co… 1 [1 Co… 1 [1 Co… 1 [1 Co… 1 [1 Co…
## 4 09400411… 0940041120  0940041     1 [1 Co… 1 [1 Co… 1 [1 Co… 1 [1 Co… 1 [1 Co…
## 5 09400411… 0940041180  0940041     1 [1 Co… 1 [1 Co… 5 [5 Er… 1 [1 Co… 1 [1 Co…
## 6 09400411… 0940041180  0940041     1 [1 Co… 1 [1 Co… 1 [1 Co… 1 [1 Co… 1 [1 Co…
## # ℹ 211 more variables: dc007_w4 <dbl+lbl>, dc008_w4 <dbl+lbl>,
## #   dc009_w4 <dbl+lbl>, dc010_w4 <dbl+lbl>, dc012_w4 <dbl+lbl>,
## #   dc004 <dbl+lbl>, dc013_w4_1_s1 <dbl+lbl>, dc013_w4_1_s2 <dbl+lbl>,
## #   dc013_w4_1_s3 <dbl+lbl>, dc013_w4_1_s4 <dbl+lbl>, dc013_w4_1_s97 <dbl+lbl>,
## #   dc013_w4_2_s1 <dbl+lbl>, dc013_w4_2_s2 <dbl+lbl>, dc013_w4_2_s3 <dbl+lbl>,
## #   dc013_w4_2_s4 <dbl+lbl>, dc013_w4_2_s97 <dbl+lbl>, dc013_w4_3_s1 <dbl+lbl>,
## #   dc013_w4_3_s2 <dbl+lbl>, dc013_w4_3_s3 <dbl+lbl>, …
```



```r
library(purrr)
get_var_label <- function(dta) {
  labels <- map(dta, function(x) attr(x, "label"))
  data_frame(
    name = names(labels),
    label = as.character(labels)
  )
}
```


```r
#View(charls2018cogn)
charls2018cogn%>%
  select(starts_with("dc014"))%>%
  get_var_label()
```

```
## Warning: `data_frame()` was deprecated in tibble 1.1.0.
## ℹ Please use `tibble()` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## # A tibble: 11 × 2
##    name         label                               
##    <chr>        <chr>                               
##  1 dc014_w4_1   Reason for Missing from 100-7       
##  2 dc014_w4_1_1 Specific Result from 100-7          
##  3 dc014_w4_2   Reason for Missing from dc014_w4_1-7
##  4 dc014_w4_2_1 Specific Result from dc014_w4_2-7   
##  5 dc014_w4_3   Reason for Missing from dc014_w4_2-7
##  6 dc014_w4_3_1 Specific Result from dc014_w4_3-7   
##  7 dc014_w4_4   Reason for Missing from dc014_w4_3-7
##  8 dc014_w4_4_1 Specific Result from dc014_w4_4-7   
##  9 dc014_w4_5   Reason for Missing from dc014_w4_4-7
## 10 dc014_w4_5_1 Specific Result from dc014_w4_5-7   
## 11 dc014        I Felt Fearful
```




