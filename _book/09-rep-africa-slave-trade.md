# 文献复刻：The Long-term Effects of Africa's Slave Trades

@Nunn2008 这篇文献可称为是在学习IV时候的经典文献，2008年发表在QJE。

## 文献回顾





## 数据来源



```r
if (!require("pacman")) install.packages("pacman")
```

```
## Loading required package: pacman
```

```r
pacman::p_load(
  sf, # vector data operations
  tidyverse, # data wrangling
  units,
  rmapshaper,
  lwgeom,
  tictoc,
  haven
)
```



```r
#--- coast line ---#
coast <-
  sf::st_read(here::here("data/nunn_2008/input/10m-coastline/10m_coastline.shp")) %>%
  st_transform(3857)
```

```
## Reading layer `10m_coastline' from data source 
##   `/Users/a182501/rproject/rrp/data/nunn_2008/input/10m-coastline/10m_coastline.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 4177 features and 3 fields
## Geometry type: MULTILINESTRING
## Dimension:     XY
## Bounding box:  xmin: -180 ymin: -85.22198 xmax: 180 ymax: 83.6341
## Geodetic CRS:  WGS 84
```

```r
#--- African countries ---#
countries <-
  sf::st_read(here::here("data/nunn_2008/input/gadm36_africa/gadm36_africa.shp")) %>%
  st_transform(3857)
```

```
## Reading layer `gadm36_africa' from data source 
##   `/Users/a182501/rproject/rrp/data/nunn_2008/input/gadm36_africa/gadm36_africa.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 54 features and 2 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: -25.3618 ymin: -34.83514 xmax: 63.50347 ymax: 37.55986
## Geodetic CRS:  WGS 84
```

```r
#--- ethnic regions ---#
ethnic_regions <-
  sf::st_read(here::here("data/nunn_2008/input/Murdock_shapefile/borders_tribes.shp")) %>%
  st_transform(3857)
```

```
## Reading layer `borders_tribes' from data source 
##   `/Users/a182501/rproject/rrp/data/nunn_2008/input/Murdock_shapefile/borders_tribes.shp' 
##   using driver `ESRI Shapefile'
## Simple feature collection with 843 features and 4 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: -25.35875 ymin: -34.82223 xmax: 63.50018 ymax: 37.53944
## Geodetic CRS:  WGS 84
```



```r
# lat/long for slave trade centers
trade_centers <- readxl::read_xls(here::here("data/nunn_2008/input/nunn2008.xls"))
```



## 计算最近的贸易距离


```r
countries_simp <- rmapshaper::ms_simplify(countries)
```



```r
(
  g_countries <-
    ggplot(data = countries_simp) +
    geom_sf() +
    theme_void()
)
```

![](09-rep-africa-slave-trade_files/figure-epub3/unnamed-chunk-7-1.png)<!-- -->


用`st_centroid()`来发现每一个国家的质心.


```r
countries_centroid <- st_centroid(countries)
```

```
## Warning in st_centroid.sf(countries): st_centroid assumes attributes are
## constant over geometries of x
```

```r
ggplot()+
  geom_sf(data = countries_simp)+
  geom_sf(data = countries_centroid,color='red',size =0.5)
```

![](09-rep-africa-slave-trade_files/figure-epub3/unnamed-chunk-8-1.png)<!-- -->




```r
(
  coast_union <- st_union(coast)
)
```

```
## Geometry set for 1 feature 
## Geometry type: MULTILINESTRING
## Dimension:     XY
## Bounding box:  xmin: -20037510 ymin: -20261860 xmax: 20037510 ymax: 18428920
## Projected CRS: WGS 84 / Pseudo-Mercator
```

```
## MULTILINESTRING ((4926419 -2317631, 4926038 -23...
```


```r
minum_dist_to_coast <- st_nearest_points(countries_centroid, coast_union)
```




```r
(
  g_min_dist_line <-
    ggplot() +
    geom_sf(data = countries_simp) +
    geom_sf(data = minum_dist_to_coast, color = "red") +
    theme_void()
)
```

![](09-rep-africa-slave-trade_files/figure-epub3/unnamed-chunk-11-1.png)<!-- -->



```r
closest_pt_on_coast <- lwgeom::st_endpoint(minum_dist_to_coast)
```



```r
g_min_dist_line +
  geom_sf(
    data = closest_pt_on_coast,
    color = "blue",
    size = 2
  ) +
  theme_void()
```

![](09-rep-africa-slave-trade_files/figure-epub3/unnamed-chunk-13-1.png)<!-- -->




```r
countries_simp$nearest_pt <- closest_pt_on_coast
```



```r
(
  trade_centers_sf <-
    trade_centers %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    st_transform(crs = 3857)
)
```

```
## Simple feature collection with 9 features and 2 fields
## Geometry type: POINT
## Dimension:     XY
## Bounding box:  xmin: -9168273 ymin: -2617513 xmax: -4288027 ymax: 4418219
## Projected CRS: WGS 84 / Pseudo-Mercator
## # A tibble: 9 × 3
##   name           fallingrain_name             geometry
## * <chr>          <chr>                     <POINT [m]>
## 1 Virginia       Virginia Beach     (-8458055 4418219)
## 2 Havana         Habana             (-9168273 2647748)
## 3 Haiti          Port au Prince     (-8051739 2100853)
## 4 Kingston       Kingston           (-8549337 2037549)
## 5 Dominica       Roseau             (-6835017 1723798)
## 6 Martinique     Fort-de-France     (-6799394 1644295)
## 7 Guyana         Georgetown        (-6473228 758755.9)
## 8 Salvador       Salvador da Bahia (-4288027 -1457447)
## 9 Rio de Janeiro Rio               (-4817908 -2617513)
```




```r
ggplot() +
  geom_sf(data = trade_centers_sf, color = "red") +
  geom_sf(data = countries_simp, aes(geometry = geometry)) +
  theme_void()
```

![](09-rep-africa-slave-trade_files/figure-epub3/unnamed-chunk-16-1.png)<!-- -->










