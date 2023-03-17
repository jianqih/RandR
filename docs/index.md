---
title: "基于R的文献复刻：利用中国微观数据库"
author: "黄建祺"
date: "2023-03-17"
site: bookdown::bookdown_site
documentclass: book
bibliography: [book.bib, packages.bib,article.bib]
#url: your book url like https://bookdown.org/r-replication
#cover-image: path to the social sharing image like images/cover.jpg
description: |
  This book written based on the data waranting using micro-database in China.
link-citations: yes
biblio-style: apa
classoption: oneside
github-repo: rstudio/bookdown-demo
---



# 前言{-}


## 缘起
突然想要研究某一个中国的微观数据库，但又无奈苯人掌握Stata技法有限，想要再去学有点学不动了。忽然一次在网上看到林敏杰老师的[《CFPS 之R语言学习笔记》](https://bookdown.org/wangminjie/R4cfps/) 瞬间为我打开了一扇窗，所以尝试使用R来实现一些在实证经济学的一些应用或称为复刻，另外加上一丁点的探索。之前也看到国外的在利用**shiny**搭建的网站^[https://ejd.econ.mathematik.uni-ulm.de/]做的文献复刻，因此就有一些用`R`来做的冲动。


## 为何是R

为何选择使用R呢。现在在复刻文献的主流方法是使用Stata，同时在一些wiki 或者期刊网站、数据网站^[这篇知乎总结了所有可以发现代码的地方] 上大部分是使用stata来实现复刻或者源代码的。但目前，一些经济学、政治学学者开始在利用R做基本的计量分析，甚至开发专门的R包来实现一些高级计量方法，在R中有很多超越于Stata的优势。


## 如何食用

这本书是基于中国的几大微观数据库及相关的顶刊上的文章为主要内容写成的自我技术修炼笔记，为最大化他的社会效益，以**bookdown**形式生成。希望能够对你有所帮助。


在经济学研究中，近些年来对于微观数据的使用变得尤为重要，尤其是大型的微观数据库的使用。但常用的处理方法往往是选择在Stata中进行操作。但在另一方面，R相对于Stata的使用有其独特的优势，尤其是在使用多个数据框操作时候，在今天的一篇文章中很难就单单一个数据源就能完成一篇好的文章写作，因此对于不同数据源进行交互性操作愈发重要。因此有必要使用R来对数据进行相应的操作。同时R也是支持于`.dta`数据的读取。

同时再深了讲在不同语言之间的比较，@ARUOBA2015265 有对比不同的编程语言的运行速度，同时需要强调的是C++/Fortran/Java对比其他的语言是更难学习的，由其本身的特性所决定的，所以对于经济学家来说没有那么多的时间成本学习前三种语言，但可以作为一个参照。最后发现Julia的表现尤其突出，甚至超过于Java，远甩身后的Matlab好几米；Python与R基本上持平，但一用上Pypy编译器立马加速；最慢的是Mathemetica（虽然我也没用过）
^[文章的代码可以[在github仓库中找到](https://github.com/jesusfv/Comparison-Programming-Languages-Economics)]

<div class="figure" style="text-align: center">
<img src="image/com-jedc.png" alt="图来自上述文章" width="679" />
<p class="caption">(\#fig:com-jedc)图来自上述文章</p>
</div>

Julia的强劲表现让很多经济学者极力推崇^[以诺奖得主、宏观学者Sargent为代表] 相关的学习材料在[Quantecon](https://quantecon.org/) 就很丰富了。

但我们这里并不需要大量的数据，因此对于编译速度的要求并不高，RStudio能够满足日常的基本需要。


## R入门

关于R的入门这里不多介绍，[bookdown](www.bookdown.org)中有大量关于R的入门书籍。这里可以做出一定的推荐：

英文比较好的话：

- [官方manual](file:///Library/Frameworks/R.framework/Versions/4.2-arm64/Resources/doc/manual/R-intro.html)

- [R for Data Science](https://r4ds.had.co.nz/)

- [modern dive](https://moderndive.com/)

- [Big book of R](https://www.bigbookofr.com/)：和字典一样厚，作者至今还在更新。

看中文更有优势的话：

- [R语言教程](https://www.math.pku.edu.cn/teachers/lidf/docs/Rbook/html/_Rbook/index.html)

- [数据科学中的R语言](https://bookdown.org/wangminjie/R4DS/)


当然因为这里主要介绍一些经济学论文和经济学方法，不可避免要学习和使用计量及R上的应用。同样有很多出色的线上教材。

- [Introduction to Econometric with R](https://www.econometrics-with-r.org/)
- [Causal Inference: The Mixtape](https://mixtape.scunning.com/): 有三种语言的代码任你选择。

需要注意的是在R中进行数据处理，必然无法避开学习和使用**tidyverse**，因为这才是数据科学学习R的优势之处。



这篇所需要使用的R包：使用`pacman`免去验证是否安装的烦恼。


```r
if (!require("pacman")) install.packages("pacman")
```

```
## Loading required package: pacman
```

```r
p_load(tidyverse,
       purrr,
       haven,
       visdat,
       sf,
       units,
       lwgeom,
       rmapshaper,
       tictoc)
```




