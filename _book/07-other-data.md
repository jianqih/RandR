# 其他来源数据

其他数据来源包括相关领域内的学者对于在其文章中的数据以及公开的数据集中的数据。
比如哈佛大学的王教授收集了关于地方的数据




## 方言数据

```r
library(mapchina)
library(sysfonts)
library(showtextdb)
library(showtext)
library(tidyverse)
library(sf)
```


徐现祥老师在其个人网站上公开了其地方的方言数据，也就是
连享会gitee仓库有[相关数据集合](https://gitee.com/arlionn/IRE/tree/master/)




```r
library(haven)
df <- read_dta("data/China_dialect_diversity_index.dta")
head(df)
```

```
## # A tibble: 6 × 3
##   city             diversity1 diversity2
##   <dbl+lbl>             <dbl>      <dbl>
## 1 256 [阿勒泰地区]      0.650          2
## 2  75 [安康市]          0.475          2
## 3  74 [安庆市]          0.477          2
## 4  77 [安顺市]          0.570          1
## 5  76 [安阳市]          0.463          2
## 6 261 [鞍山市]          0.428          3
```



```r
lab <- attributes(df$city)$labels
lab <- as.data.frame(lab)
lab_name <- rownames(lab)
lab_df <- cbind(lab,lab_name)
colnames(lab_df) <- c("city","city_name")
df_lab <- merge(df,lab_df,by = "city")
head(df_lab)
```

```
##   city diversity1 diversity2 city_name
## 1    1     0.1071          1  七台河市
## 2    2     0.5067          1    三亚市
## 3    3     0.4763          2  三门峽市
## 4    4     0.0126          1    上海市
## 5    5     0.5792          4    上饶市
## 6    6     0.0603          1    东莞市
```


我们这里使用的是[mapchina](https://github.com/xmc811/mapchina)包，能够在不同的行政层级上绘制中国区划地图。


```r
sf_use_s2(FALSE)
df1 <- china%>%
  dplyr::filter(is.na(china$Name_Perfecture))%>%
  mutate(Name_Perfecture=Name_Province)

df2 <- china%>%
  dplyr::filter(is.na(china$Name_Perfecture)==F)

df3 <- rbind(df1,df2)
dim(df3)
```

```
## [1] 2901   14
```

```r
dim(china)
```

```
## [1] 2901   14
```

```r
df3_perf <- df3 %>%
  group_by(Name_Perfecture) %>%
  summarise(geometry = st_union(geometry))
colnames(df_lab) <- c("city","d1","d2","Name_Perfecture")
df_all <- left_join(df3_perf,df_lab,by = "Name_Perfecture")
```



地区的不同方言类别：


```r
ggplot(data = df_all) +
  geom_sf(aes(fill = d2)) +
  scale_fill_distiller(palette = "YlOrRd")+
  theme_bw()
```

![](07-other-data_files/figure-epub3/unnamed-chunk-5-1.png)<!-- -->



地方方言的多样性指数：


```r
ggplot(data = df_all) +
  geom_sf(aes(fill = d1)) +
  scale_fill_distiller(palette = "YlOrRd")+
  theme_bw()
```

![](07-other-data_files/figure-epub3/unnamed-chunk-6-1.png)<!-- -->




## 夜间灯光数据

我们想要复刻一下上面的案例，但缺乏城市GDP数据，但我又懒于去用学校wind或数据网站上爬。就想要看下国内有没有可以用的API可以获得相关数据。

### 获取方法

nightlight data


### API









## 政治数据





