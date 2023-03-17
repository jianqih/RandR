---
title: "基于R的文献复刻：利用中国微观数据库"
author: "黄建祺"
date: "2023-03-12"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib, packages.bib,article.bib]
#url: your book url like https://bookdown.org/r-replication
#cover-image: path to the social sharing image like images/cover.jpg
description: |
  This book written based on the data waranting using micro-database in China.
link-citations: yes
github-repo: rstudio/bookdown-demo
---
---
title: "基于R的文献复刻：利用中国微观数据库"
author: "黄建祺"
date: "2023-03-12"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib, packages.bib,article.bib]
#url: your book url like https://bookdown.org/r-replication
#cover-image: path to the social sharing image like images/cover.jpg
description: |
  This book written based on the data waranting using micro-database in China.
link-citations: yes
github-repo: rstudio/bookdown-demo
---



# 前言{-}
这本书是基于中国的几大微观数据库及相关的顶刊上的文章为主要内容写成的自我技术修炼笔记，为最大化他的社会效益，以 [@R-bookdown] 形式生成。希望能够对你有所帮助。

在经济学研究中，近些年来对于微观数据的使用变得尤为重要，尤其是大型的微观数据库的使用。但常用的处理方法往往是选择在Stata中进行操作。但在另一方面，R相对于Stata的使用有其独特的优势，尤其是在使用多个数据框操作时候，在今天的一篇文章中很难就单单一个数据源就能完成一篇好的文章写作，因此对于不同数据源进行交互性操作愈发重要。因此有必要使用R来对数据进行相应的操作。同时R也是支持于`.dta`数据的读取。

关于R的入门这里不多介绍，bookdown中有大量关于R的入门书籍。这里可以做出一定的推荐：

英文比较好的话：

- [R for Data Science](https://r4ds.had.co.nz/)


中文更有优势的话：

- [R语言教程](https://www.math.pku.edu.cn/teachers/lidf/docs/Rbook/html/_Rbook/index.html)

- [数据科学中的R语言](https://bookdown.org/wangminjie/R4DS/)

需要注意的是在R中进行数据处理，必然无法避开学习和使用tidyverse，因为这才是数据科学学习R的优势之处。



这篇所需要使用的R包：使用`pacman`免去验证是否安装的烦恼😄。


```r
library(pacman)
p_load(tidyverse,purrr,haven,visdat,conflicted)
```




<!--chapter:end:index.Rmd-->


# 土地流转研究

Placeholder


## 载入包
## 导入数据
## 查看变量标签
## 数据规整
### 农业生产效率
### 流动人口
## 模型建立

<!--chapter:end:01-land.Rmd-->

# 文献复刻：《劳动力流动如何影响农户借贷》

## 文献回顾
[这篇文章](https://www.cnki.com.cn/Article/CJFDTOTAL-SJJJ202112006.htm)主要发现劳动力流动导致农户借出的概率和金额显著增加。

- 核心的被解释变量为家庭是否有借给亲戚、朋友等的借出款项和为家庭人均借出金额的对数值。降低极端值影响，进行上下2%缩尾处理。

- 劳动力流动：是否有劳动力流动以及家庭劳动力流动人数。

- 控制变量：


## 数据处理


加载cfps2018数据:







## 统计描述




## 模型设定

<!--chapter:end:02-rep-migration-loan.Rmd-->


# 宗族文化 {#culture}

Placeholder


## 数据载入
## 可视化
## 地图
## 其他数据源

<!--chapter:end:03-clan.Rmd-->


# CHARLS

Placeholder


## 数据导入

<!--chapter:end:04-charls.Rmd-->

# 文献复刻：《新型农村社会养老保险政策效果评估》


这篇文章是使用断点回归和DID的方法，

实际上是利用领取养老金的年龄规则，只有年满60周岁的参保人员才能领取：


$$D_i= \begin{cases}1,z_i\ge 60,\\0,z_i>60\end{cases}$$

因变量：家户总收入、家户人均收入、个人收入、个人非劳动收入；






## 数据导入

<!--chapter:end:05-rep-policy-insur.Rmd-->

# CHFS

[CHFS](https://chfs.swufe.edu.cn/)是西南财经大学组织的中国家庭金融调查

## 数据


<!--chapter:end:06-cfps.Rmd-->


# 参考文献 {-}


<!--chapter:end:07-references.Rmd-->

