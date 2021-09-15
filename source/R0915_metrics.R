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












