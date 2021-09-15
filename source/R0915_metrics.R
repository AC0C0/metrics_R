# replicate acemoglu democracy and development

# load pkg ----------------------------------------------------------------

library(dplyr)
library(readxl)
library(ggplot2)
library(ggrepel)

# read data ---------------------------------------------------------------
ajry_f1 <- read_xls('data/ajry.xls', sheet = 'F1')
glimpse(ajry_f1)

# rename
ajry_f1 <- ajry_f1 %>% rename(freedom_house = fhpolrigaug, log_gdp_pc = lrgdpch)

# plot figure 1 -----------------------------------------------------------

f1 <- ggplot(ajry_f1) +
  # geom_point(aes(x = log_gdp_pc, y = freedom_house)) +
  geom_text_repel(aes(label = code, x = log_gdp_pc, y = freedom_house), size = 2, max.overlaps = 30) +
  geom_smooth(aes(x = log_gdp_pc, y = freedom_house), method = lm, se = FALSE, color = 'black', size = 0.5) + 
  theme_bw() +
  labs(x = 'log gdp per capita',
       y = 'freedom house measure of democracy')
# save graph
if (!dir.exists('out_fig')){
  dir.create('out_fig')
}  # if the folder doesn't exist, create one

ggsave('out_fig/f1.png',plot = f1, width = 8, height = 6, dpi = 300)


# create main table -------------------------------------------------------
library(fixest)
library(modelsummary)
# read data
ajry <- read_xls('data/ajry.xls', sheet = '5 Year Panel')
# rename
ajry <- ajry %>% rename(freedom_house = fhpolrigaug, log_gdp_pc = lrgdpch)
# create lag variables
ajry <- ajry %>% group_by(code_numeric) %>% mutate(lag_log_gdp_pc = lag(log_gdp_pc, order_by = year_numeric), 
                                          lag_freedom_house = lag(freedom_house, order_by = year_numeric))
ajry <- ajry %>% filter(sample == 1)

# run ols
pooled <- lm(freedom_house ~ lag_freedom_house + lag_log_gdp_pc + factor(year), data = ajry)  #factor(year) create year dummies
summary(pooled)

# robust std error
# first an ugly way to do it
library(multiwayvcov)
vcov_country_pool <- cluster.vcov(pooled, ajry$code_numeric) #create cov matrix with clustered std error
library(lmtest)
coeftest(pooled, vcov_country_pool) # hypo testing with clustered error
# or use package fixest
library(fixest)
feols(freedom_house ~ lag_freedom_house + lag_log_gdp_pc | year_numeric, data = ajry)  # | year_numeric gives year FE, will cluster on year automatically
feols(freedom_house ~ lag_freedom_house + lag_log_gdp_pc | year_numeric, se = 'standard', data = ajry) # now doesn't cluster std error
feols(freedom_house ~ lag_freedom_house + lag_log_gdp_pc | year_numeric, se = 'hetero', data = ajry) # hetero std error
feols(freedom_house ~ lag_freedom_house + lag_log_gdp_pc | year_numeric, cluster = c('code_numeric'), data = ajry) # cluster on code_numeric
fixest_pooled <- feols(freedom_house ~ lag_freedom_house + lag_log_gdp_pc | year_numeric, cluster = ~ code_numeric, data = ajry) # cluster on code_numeric(country), another way to write it

# add country FE
feols(freedom_house ~ lag_freedom_house + lag_log_gdp_pc | year_numeric + country, cluster = ~ code_numeric, data = ajry)
fixest_fe <- feols(freedom_house ~ lag_freedom_house + lag_log_gdp_pc | year_numeric + country, cluster = ~ code_numeric, dof = dof(fixef.K = 'full'), data = ajry)  # to specify which dof to use

# add instruments, use saving rate in t-2 to instrument for gdp pc in t-1
# first create the saving rate lag
ajry <- ajry %>% group_by(country) %>% mutate(lag2_save = lag(nsave, 2, order_by = year_numeric))
# now iv reg
feols(freedom_house ~ lag_freedom_house | year_numeric + country | lag_log_gdp_pc ~ lag2_save, cluster = ~ code_numeric, dof = dof(fixef.K = 'full'), data = ajry)


# look at regression results ----------------------------------------------
iv <- feols(freedom_house ~ lag_freedom_house | year_numeric + country | lag_log_gdp_pc ~ lag2_save, cluster = ~ code_numeric, dof = dof(fixef.K = 'full'), data = ajry)
#method 1
summary(iv)
summary(iv, stage = 1)
# method 2
library(broom)
iv_2nd <- tidy(iv)  # gives a tibble
iv_1st <- tidy(iv, stage = 1)
stats <- glance(iv)

# create reg table --------------------------------------------------------
library(modelsummary)
# ols and FE
models <- list('Pooled' = fixest_pooled, 'FE' = fixest_fe)  #put the models into a list first
modelsummary(models)
# coeff map
cm <- c('lag_freedom_house' = 'democracy$_{t-1}$',
        'lag_log_gdp_pc' = 'log GDP $_{t-1}$')
modelsummary(models, coef_map = cm)
# remove some stats
omit_stat = 'AIC|BIC|Log.Lik.|R2 Within|R2 Pseudo|R2$'  # will omit all stats containing these strings, e.g., if write R2, will get rid of all R2s, but if write R2$, will only get rid of R2,(this is regular expression)
modelsummary(models, coef_map = cm, gof_omit = omit_stat)

# rename stats
pooled_stats <- glance(pooled)
stat_table <- tribble(~raw, ~clean, ~fmt, 'adj.r.squared', 'Adj. R$^2$', 2, 'nobs', 'N', 0)  # use 2 digits for R^2, 0 digit for N
modelsummary(models, coef_map = cm,gof_map = stat_table )

# add stars
modelsummary(models, coef_map = cm,gof_map = stat_table, stars = TRUE)
modelsummary(models, coef_map = cm,gof_map = stat_table, stars = c('*'  = 0.1, '**' = 0.05, '***' = 0.01))

# add rows
rows <- tribble(~term, ~m1, ~m2, 'time effects', 'yes', 'yes',
                'country effects', 'no', 'yes')
modelsummary(models, coef_map = cm,gof_map = stat_table, add_rows = rows)
attr(rows, 'position') <- c(5,6)  # change location of the rows
modelsummary(models, coef_map = cm,gof_map = stat_table, add_rows = rows)

# save to latex
if (!dir.exists('out_table')){
  dir.create('out_table')
}
modelsummary(models, coef_map = cm,gof_map = stat_table, stars = c('*'  = 0.1, '**' = 0.05, '***' = 0.01), escape = FALSE, output = 'out_table/main_table.tex') # ESCAPE IS for using latex code here in the code









