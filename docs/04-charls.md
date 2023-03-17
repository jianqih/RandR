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
## [1] "/Users/a182501/rproject/rrp"
```

```r
charls2018cogn <- read_dta("data/charls/2018/Cognition.dta")
head(charls2018cogn)
```

```
## # A tibble: 6 × 219
##   ID     house…¹ commu…² dc001…³ dc002…⁴ dc003…⁵ dc005…⁶ dc006…⁷ dc007…⁸ dc008…⁹
##   <chr>  <chr>   <chr>   <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l> <dbl+l>
## 1 09400… 094004… 0940041 1 [1 C… 1 [1 C… 5 [5 E… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C…
## 2 09400… 094004… 0940041 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C…
## 3 09400… 094004… 0940041 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C…
## 4 09400… 094004… 0940041 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C…
## 5 09400… 094004… 0940041 1 [1 C… 1 [1 C… 5 [5 E… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C…
## 6 09400… 094004… 0940041 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C… 1 [1 C…
## # … with 209 more variables: dc009_w4 <dbl+lbl>, dc010_w4 <dbl+lbl>,
## #   dc012_w4 <dbl+lbl>, dc004 <dbl+lbl>, dc013_w4_1_s1 <dbl+lbl>,
## #   dc013_w4_1_s2 <dbl+lbl>, dc013_w4_1_s3 <dbl+lbl>, dc013_w4_1_s4 <dbl+lbl>,
## #   dc013_w4_1_s97 <dbl+lbl>, dc013_w4_2_s1 <dbl+lbl>, dc013_w4_2_s2 <dbl+lbl>,
## #   dc013_w4_2_s3 <dbl+lbl>, dc013_w4_2_s4 <dbl+lbl>, dc013_w4_2_s97 <dbl+lbl>,
## #   dc013_w4_3_s1 <dbl+lbl>, dc013_w4_3_s2 <dbl+lbl>, dc013_w4_3_s3 <dbl+lbl>,
## #   dc013_w4_3_s4 <dbl+lbl>, dc013_w4_3_s97 <dbl+lbl>, …
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


