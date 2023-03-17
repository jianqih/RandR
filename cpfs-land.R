library(tidyverse)
library(here)
library(fs)
library(haven)
library(broom)


cfps2014adult <- read_dta("data/cfps/2014/cfps2014famecon_201906.dta")
vars_select <- c(
  "fid14", "provcd14", "fa1",
  "fa7_s_1", "fa7_s_2", "fa7_s_3", "fk1l",
  "fn1_s_1", "fn1_s_2", "fn1_s_3", "fn1_s_4",
  "fn101", "fn2", "fn3", "fo1", "fp510",
  "fincome1_per"
)
cfps2014adult %>% select(vars_select) %>% glimpse()


library(purrr)
get_var_label <- function(dta) {
  labels <- map(dta, function(x) attr(x, "label"))
  tibble(
    name = names(labels),
    label = as.character(labels)
  )
}

cfps2014adult %>% dplyr::select(vars_select) %>% get_var_label()


cfps2014adult %>%
  summarise_all(n_distinct) %>%
  tidyr::gather(vars_name, num_distinct_answers) %>%
  arrange(desc(num_distinct_answers))


cfps2014adult %>%
  select(vars_select) %>%
  map(~ count(data.frame(x = .x), x))





cfps2014adult %>%
  select(vars_select) %>%
  map(~ count(data.frame(x = .x), x))




library(visdat)
cfps2014adult %>%
  select(vars_select) %>%
  vis_dat()

cfps2014adult%>%
  select(fa1)

library(conflicted)
conflicted::conflict_prefer("filter","dplyr")
a <- cfps2014adult %>%
  select(vars_select) %>%
  # 过滤符合情形的
  dplyr::filter(fa1 == 5) %>%
  # 删除缺失值
  filter(!is.na(fa7_s_1)) %>%
  # 住房困难fa7_s
  filter(!fa7_s_1 %in% c(-10, -8, -2, -1)) %>%
  # 新建housing变量
  # mutate(housing = if_else(fa7_s_1 == 78 | fa7_s_2 == 78 | fa7_s_3 == 78, 0, 1)) %>%
  mutate(housing = if_else(fa7_s_1 %in% c(78) | fa7_s_2 %in% c(78) | fa7_s_3 %in% c(78), 0, 1)) %>% #
  # 过滤
  dplyr::filter(!fn1_s_1 %in% c(-10, -9, -8, -2, -1)) %>%
  # 替换
  mutate_at(vars(fn101), funs(replace(., . == -8, 0))) %>%
  # 家庭收到政府补助
  filter(!fn101 %in% c(-1, -2, -9, -10)) %>%
  # 否收到社会捐助
  filter(!fn2 %in% c(-1)) %>%
  # 是否有人领取离退休金和养老金
  filter(!fn3 %in% c(-1, NA)) %>%
  # 是否打工
  filter(!is.na(fo1)) %>%
  # 过去12月的教育支出
  filter(!fp510 %in% c(-10, -9, -8, -2, NA)) %>%
  # 是否进行人力资本投入
  # mutate(Y = if_else(fp510 == 0, 0, 1)) %>%
  mutate(Y = if_else(fp510 %in% c(0), 0, 1)) %>%
  # 删除缺失值
  filter(!is.na(fincome1_per)) %>%
  # 虚拟变量
  fastDummies::dummy_cols("provcd14") %>%
  # 家庭人均收入fincome1_per的自然对数
  mutate(ln_fincome1_per = log(fincome1_per)) %>%
  # 政府补助总额+1的自然对数
  mutate(ln_fn101 = log(fn101 + 1)) %>%
  # 教育支出计算其自然对数
  mutate(ln_fp510 = log(fp510 + 1)) %>%
  # 是否外出打工与收入对数的交乘项
  mutate(fo1xln_fincome1_per = ln_fincome1_per * fo1) %>%
  # 收入的自然对数的二次项
  mutate(ln_fincome1_perxln_fincome1_per = ln_fincome1_per * ln_fincome1_per)



a %>% summarise_all(~ sum(is.na(.)))

library(naniar)
a %>%
  miss_var_summary() %>%
  filter(n_miss > 0)


df <- a %>% select(
  Y, fo1, fo1xln_fincome1_per, ln_fincome1_per,
  ln_fincome1_perxln_fincome1_per, ln_fn101,
  fn2, fk1l, housing, fn3,
  starts_with("provcd14_")
)

df %>%
  map(~ count(data.frame(x = .x), x))
data <- df %>%
  mutate_at(vars(fo1, fn2, fk1l, housing, fn3), as.factor) %>%
  mutate_at(vars(
    fo1xln_fincome1_per, ln_fincome1_per,
    ln_fincome1_perxln_fincome1_per, ln_fn101
  ), funs((. - mean(.)) / sd(.)))

probit_t <- glm(
  formula = Y ~ .,
  family = binomial(link = "probit"), # canonical link function
  data = data
)


library(mfx)
probitmfx(formula = Y ~ ., data = data)

summary(probit_t)
library(margins)
m <- margins(probit_t)




#fo11                             1.524573   0.242431   6.289 3.20e-10 ***
#fo1xln_fincome1_per             -0.652962   0.123509  -5.287 1.24e-07 ***
#fk1l1                            0.194262   0.033043   5.879 4.13e-09 ***
#housing1                         0.269428   0.037764   7.135 9.71e-13 ***
#fn35                             0.196196   0.029714   6.603 4.03e-11 ***
