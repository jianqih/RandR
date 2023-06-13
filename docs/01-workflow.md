# R-econometric workflow

应用微观的经济学家的工作流会选择使用Stata（宏观学者可能会用Matlab或dynare）加上LaTeX（包括做slides）进行，若可能需要的话加上一些数据库的知识。但这样的工作方法会存在一个比较尴尬的问题：若在跑回归的时候出现一些变量/数据/模型的调整，最后输出结果（一般从Stata转换为LaTeX代码）也会跟着改变。这样就不得不将原有的结果进行相应的变化。


这里使用woodlridge包中大学GPA数据来作为演示。

## Stargazer



```r
library(wooldridge)
library(psych)
```

```
## 
## Attaching package: 'psych'
```

```
## The following objects are masked from 'package:ggplot2':
## 
##     %+%, alpha
```

```r
library(haven)
library(stargazer)
```

```
## 
## Please cite as:
```

```
##  Hlavac, Marek (2022). stargazer: Well-Formatted Regression and Summary Statistics Tables.
```

```
##  R package version 5.2.3. https://CRAN.R-project.org/package=stargazer
```

```r
data("gpa1")
```

选出我们所关心的几个变量。


```r
gpa.fun <- colGPA~hsGPA+ACT+skipped
gpa_var <- gpa1%>%
  select(colGPA,hsGPA,ACT,skipped)
```


```r
stargazer(describe(gpa_var,trim = F,skew = F,ranges = F))
```


% Table created by stargazer v.5.2.3 by Marek Hlavac, Social Policy Institute. E-mail: marek.hlavac at gmail.com
% Date and time: Fri, Mar 31, 2023 - 13:12:38
\begin{table}[!htbp] \centering 
  \caption{} 
  \label{} 
\begin{tabular}{@{\extracolsep{5pt}}lccccc} 
\\[-1.8ex]\hline 
\hline \\[-1.8ex] 
Statistic & \multicolumn{1}{c}{N} & \multicolumn{1}{c}{Mean} & \multicolumn{1}{c}{St. Dev.} & \multicolumn{1}{c}{Min} & \multicolumn{1}{c}{Max} \\ 
\hline \\[-1.8ex] 
vars & 4 & 2.500 & 1.291 & 1 & 4 \\ 
n & 4 & 141.000 & 0.000 & 141 & 141 \\ 
mean & 4 & 7.923 & 10.871 & 1.076 & 24.156 \\ 
sd & 4 & 1.156 & 1.179 & 0.320 & 2.844 \\ 
se & 4 & 0.097 & 0.099 & 0.027 & 0.240 \\ 
\hline \\[-1.8ex] 
\end{tabular} 
\end{table} 
需要记得的是在option中添加`results='asis'`



```r
lm4 <- lm(gpa.fun, data = gpa1)
```










## Presentation






