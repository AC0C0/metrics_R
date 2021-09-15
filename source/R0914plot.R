rm(list = ls())  #but it doesn't remove loaded packages, to do that, need to go to session-restart R
library(ggplot2)
library(dplyr)
library(tibble)

# load data ---------------------------------------------------------------
diamonds_df <- as_tibble(diamonds)
head(diamonds_df)

# how to check the order of ordered factor var?
ordered(c('a','b','c'), levels = c('a', 'b', 'c'))
ordered(c('a','b','c'), levels = c('a', 'b', 'c')) %>% levels()
ordered(c('a','b','c'), levels = c('b', 'a', 'c'))
ordered(c('a','b','c'), levels = c('b', 'a', 'c')) %>% levels()
levels(diamonds_df$cut) # this already gives the correct order
levels(diamonds_df$clarity)
# plot --------------------------------------------------------------------

ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price))

ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price, color = clarity))

ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price, alpha = clarity)) # alpha: transparancy
? geom_point

ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price, size = clarity))

ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price, color = clarity > 'VS2')) #color is based on whether the condition is true

ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price), color = 'blue') # just to change color to blue, write color outside aes function, inside are things depending on the data itself

ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price), color = 'blue', size = 0.2, alpha = 0.2)

#subplot(multiple plots) by 'cut'
ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price)) +
  facet_wrap(~cut, nrow = 2)

ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price)) +
  facet_grid(clarity~cut)  # for each combination of clarity and cut


# different plots ----------------------------------------------------------
# smooth function
ggplot(data = diamonds_df) +
  geom_smooth(aes(x = carat, y = price))

ggplot(data = diamonds_df) +
  geom_smooth(aes(x = carat, y = price), method = 'lm')

ggplot(data = diamonds_df) +
  geom_smooth(aes(x = carat, y = price, color = cut))

# bar graph
ggplot(data = diamonds_df) +
  geom_bar(aes(x = clarity))   # gives number of obs by clarity
counts_by_clarity <- diamonds_df %>% group_by(clarity) %>% summarise(n = n())   # summarise collapse dataset



# combine plots -----------------------------------------------------------
ggplot(data = diamonds_df) +
  geom_point(aes(x = carat, y = price, color = clarity)) +
  geom_smooth(aes(x = carat, y = price))

ggplot(data = diamonds_df, aes(x = carat, y = price)) +  # if the axis are same in all plots, can write in ggplot()
  geom_point(size = 0.2, alpha = 0.5) +
  geom_smooth()


# positions ---------------------------------------------------------------
#(positions in bar plot)
ggplot(data = diamonds_df) +
  geom_bar(aes(x = cut, fill = clarity))
ggplot(data = diamonds_df) +
  geom_bar(aes(x = cut, fill = clarity), 
           position = 'identity',   # doesn't stack on each other, but all start from 0
           alpha = 0.2
  )
ggplot(data = diamonds_df) +
  geom_bar(aes(x = cut, fill = clarity), 
           position = 'fill',   # gives fraction of clarity of each cut
  )

ggplot(data = diamonds_df) +
  geom_bar(aes(x = cut, fill = clarity), 
           position = 'dodge',   # gives hist for each group
  )
#another position: jitter, can use in geom_point, when several dots are at the same location


# coordinate system -------------------------------------------------------
ggplot(diamonds_df) +
  geom_point(aes(x = carat, y = price)) +
  coord_flip()

ggplot(diamonds_df) +
  geom_point(aes(x = carat, y = price)) +
  coord_polar()


# label -------------------------------------------------------------------
ggplot(diamonds_df) +
  geom_point(aes(x = carat, y = price, color = clarity)) +
  labs( x = 'Weight in Carat', 
        y = 'Price in Dollars',
        color = 'clarity',
        title = 'relationship between weight and price of diamonds')

ggplot(diamonds_df) +
  geom_point(aes(x = carat, y = price, color = clarity)) +
  labs( x = 'Weight in Carat', 
        y = 'Price in Dollars',
        color = 'clarity',
        title = 'relationship between weight and price of diamonds') +
  scale_x_continuous(breaks = seq(0,5,0.5)) + # change scale of axis
  scale_color_discrete(labels = 1:8) +  # 1:8 can write what different clarity means
  # scale_color_manual() + # can define different colors for different levels
  theme_bw()

### exercise
library(readr)
econ <- read_csv('EconomistData.csv')
## rename var
econ_rename <- rename(econ, country = Country, hdi_rank = HDI.Rank, hdi = HDI, cpi = CPI, region = Region)
# do it in an easier way
library(janitor)
rename <- econ %>% clean_names('snake')

# one var graph
ggplot(rename) +
  geom_histogram(aes(x = hdi), bins = 15)

ggplot(rename) +
  geom_density(aes(x = hdi))

ggplot(rename) +
  geom_density(aes(x = hdi, color = region))

ggplot(rename) +
  geom_density(aes(x = hdi, fill = region), alpha = 0.4)

ggplot(rename) +
  geom_density(aes(x = hdi), fill = 'blue') + 
  facet_wrap(~region, nrow = 2)

# same for cpi
  
ggplot(rename) +
  geom_histogram(aes(x = cpi), bins = 15)
  
ggplot(rename) +
  geom_density(aes(x = cpi))
  
ggplot(rename) +
  geom_density(aes(x = cpi, color = region))

ggplot(rename) +
  geom_density(aes(x = cpi), fill = 'blue') + 
  facet_wrap(~region, nrow = 2)

## plot with 2 vars
#scatter
ggplot(rename) +
  geom_point(aes(x = cpi, y = hdi))

ggplot(rename) +
  geom_point(aes(x = cpi, y = hdi), color = 'blue')

ggplot(rename) +
  geom_point(aes(x = cpi, y = hdi, color = region))

ggplot(rename) +
  geom_point(aes(x = cpi, y = hdi, color = region, size = hdi))

#scatter + smooth
ggplot(rename, aes(x = cpi, y = hdi)) +
  geom_point(aes(color = region)) +
  geom_smooth()

ggplot(rename, aes(x = cpi, y = hdi)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = lm, color = 'red', se = FALSE)

ggplot(rename, aes(x = cpi, y = hdi)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = lm, formula = y ~ x + log(x), color = 'red', se = FALSE)

# add country names
ggplot(rename, aes(x = cpi, y = hdi)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = lm, formula = y ~ x + log(x), color = 'red', se = FALSE) +
  geom_text(aes(label = country))
#only add specific names
points_to_label <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                     "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                     "India", "Italy", "China", "South Africa", "Spain",
                     "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                     "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                     "New Zealand", "Singapore")
df_select <- rename %>% filter(country %in% points_to_label)
ggplot(rename, aes(x = cpi, y = hdi)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = lm, formula = y ~ x + log(x), color = 'red', se = FALSE) +
  geom_text(aes(label = country), data = df_select)

# make labels not overlapping each other
library(ggrepel)
ggplot(rename, aes(x = cpi, y = hdi)) +
  geom_point(aes(color = region)) +
  geom_smooth(method = lm, formula = y ~ x + log(x), color = 'red', se = FALSE) +
  geom_text_repel(aes(label = country), data = df_select)

# make a presentable plot
cols <- c("Americas" = "red", "Asia Pacific" = "blue", "East EU Cemt Asia" = "darkgreen", "EU W. Europe" = "orange", "MENA" = "brown", "SSA" = "violet" )

my_plot <- ggplot(rename, aes(x = cpi, y = hdi)) +
  geom_point(aes(color = region), size = 1.5, shape = 1) +
  geom_smooth(method = lm, formula = y ~ x + log(x), color = 'red', se = FALSE) +
  geom_text_repel(aes(label = country), data = df_select) +
  scale_color_manual(values = cols) +
  # or scale_color_brewer(palette = 'Blues') +
  scale_x_continuous(breaks = 0:10, limits = c(0,10)) +
  scale_y_continuous(breaks = seq(0.2,1,0.1), limits = c(0.2,1)) +
  labs( x = 'Corruption perception index', 
        y = 'Human development index',
        color = 'Region',
        title = 'Relationship between HPI and CPI',
        caption = 'data from TIUNDP') + 
  theme_bw() + # can't write this after next theme line, or will overwrite that
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = 'bottom') # center the title, and bottom the legend

ggsave('my_plot.png', my_plot)  # save the graph



  
  
  
  
  
  
  


