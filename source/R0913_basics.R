y <- 1
x <- 2  
# shortcut of arrow: option + - for mac, alt + - for windows
# command + enter to run code
ls() # list objects
rm(x) # remove x
rm(list = ls()) # delete everything in working space
install.packages('purrr')
library(purrr)
? install.packages   # ask for help

#logical values
TRUE
FALSE
NA
3 > 5

# see data type
typeof(FALSE)
typeof(2)
typeof(2.0)  # same, mostly numbers are stored as double
typeof(Inf) # type of infinity

hal <-"I'm sorry, I'm afraid I can't do that."   #strings

as.character(1) # a function that make sth to character
TRUE + FALSE 
x <- 0
y <- 2
if (x) {     # means: if x is true, x=0, seen as false, won't print hello
  print('hello')
}
if (y) {
  print('world')
}

#check either sth is logical vector
is.character(1)
purrr::is_character(1)

my_strings <- c('this', 'is', 'a','vector')  #combine the strings
print(my_strings)
my_numbers <- c(1,2,3,4)
my_num2 <- 1:4
my_num3 <-  seq(1,10,2)
c(TRUE, 1, 'hello') # will see every element as string
length(my_numbers)

c(1,2,3) * 2
c(1,2,3) + 2
c(1,2,3) + c(1,2,3)
c(1,2,3,4) * c(1,2)  # will recycle the short vector, can be dangerous
c(1,2,3,4) * c(1,2,3) # this will give warning

# vector subsetting
my_named_vector <-c(a=1,b=2,c=3,d=4) # name a as a, 2 as b...
my_named_vector['b'] # like a dictionary
my_letters <- c('A', 'B', 'C','D')
my_letters[1] # index start from 1
my_letters[c(1,3)]
my_letters [-1]  # takes everything out, except for the first
my_letters[length(my_letters)] # gives the last
my_letters[length(my_letters)-1]
# logical subsetting
a <- 1:10
a < 5 # compare each with 5
a[a < 5]
c <- 11:20
c[a<5]  # gives the position that satisfy the condition for c


# lists -------------------------------------------------------------------

# (cmd + shift + r: create the section breaker above)
my_list <- list(1,2,3,4) # a list with 4 elements, each with length 1
str(my_list) # look at the structure
my_list2 <- list(c(1,2,3,4)) # a list with 1 element, which stores 4 elements in it
str(my_list2)
my_list3 <- list(1, TRUE, 'hello')
str(my_list3)
my_list4 <- list(list(1,2), list('a','b'))
str(my_list4)

# subset lists
fancy_list <- list(a = 1:12, b = 'Pancakes are lovely', c = TRUE, d = list(-99, 1))
str(fancy_list)
str(fancy_list[4]) # get the 4th element, as a list itself
str(fancy_list[[4]]) # get the things in the 4th element, but not as a list
str(fancy_list[1]) # get a list
str(fancy_list[[1]]) # get the elements, the vector, from the list 
str(fancy_list['a'])
str(fancy_list[['a']])
fancy_list[[4]][1] # gives the first value of the 4th list


# conditionals ------------------------------------------------------------

recession <- TRUE
if (recession) {   # different from python, need to put condition in (), put what should happen in {}
  print('booh')
} else {
  print ('Yay')
}

x <- 1
y <- 2

operation <- 'plus'
switch(operation,   #can use switch if there are many conditions to check
       plus = x + y,  # inside a function, use = , not <- 
       minus = x - y,
       times = x * y,
       divide = x / y,
       stop('You specified an unknown operation')
       )

# exercise
color <- 'red'
if(color == 'red') {
  print('tomato')
} else if(color == 'yellow') {
  print('pepper')
} else if(color == 'violet') {
  print('onion')
} else {
  print('no idea')
}
# same ex with switch
color <- 'yellow'
switch(color, 
       red = print('tomato'),
       yellow = print('pepper'),
       violet = print('onion'),
       stop('no idea'))
#fizzbuzz
input <- 15
if (input%%3 == 0 && input %%5 != 0){  # double & makes sure only one value is returned in conditionals
  print('fizz')
} else if (input%%3 != 0 && input%%5 == 0) {
  print('buzz')
} else if (input%%3 == 0 && input%%5 == 0) {
  print('fizzbuzz')
} else {
  print(input)
}

#some tricky stuff about conditionals
a <- c(1,2,3,4)
b <- c(1,3,4,5)
if (a==b) {
  print('equal')
} else {
  print('not equal')
}  # GIVES EQUAL, bc it returns 4 values TFFF, but it only takes the first value
if (identical(a,b)) {
  print('equal')
} else {
  print('not equal')
} # this gives not equal properly

1-1/3-1/3-1/3 == 0  # because of rounding issues, 
dplyr::near(1-1/3-1/3-1/3,0)  # :: is to use one function frim one package without importing whole package

# functions ---------------------------------------------------------------

cobb_douglas <- function(x, a = 0.5, b = 0.5) { # x is the vector
  u = x[1]**a + x[2]**b
  return(u)
}
goods <- c(1,2)
cobb_douglas(goods)
cobb_douglas(b = 0.6, a = 0.4, x = goods) # if name specificly, can use different order, if not, have to use the original order
# exercise
temp_translate <- function(c) {
  f = c * 9/5 + 32
  return(f)
}
temp_translate(22)

# create a dataframe and use function on it
df <- tibble::tibble(
  a = rnorm(10),
  b = c(rnorm(9), NA),   # b has one NA
  c = rnorm(10),
  d = rnorm(10)
)
library(purrr)
map_dbl(df, median)  # gives median of every column, return as double vector, can avoid loops
map_chr(df, median) # return as character
map_dbl(df, mean)
map_dbl(df, mean, na.rm = TRUE) # ignore NA
mean(df[['b']], na.rm = TRUE)



# data analytics ----------------------------------------------------------
install.packages('Ecdat')
library(Ecdat)
head(Fair)

library(tibble)
affairs <- as_tibble((Fair)) # convert the dataframe Fair to a tibble
head(affairs)
glimpse(affairs)
summary(affairs)

library(dplyr)
### modify rows
mens_affairs <- filter(affairs, sex =='male')   
childless_mens_affairs <- filter(affairs, sex == 'male' & child == 'no') # can also use , instead of &
religious_men_affairs <- filter(affairs, sex == 'male' & (religious == 4 | religious == 5))

slice(affairs, 1)
slice(affairs, 1:5)
slice(affairs, c(1,5,8:10))  # slice certain rows

arrange(affairs, age) # sort by age
arrange(affairs, age, religious)
arrange(affairs, desc(age)) # descending

### modify columns
select(affairs, age, sex, rate, education)  # select certain var, note: also reorder the columns based on your select
select(affairs, -child) # select all var except for child
select(affairs, age:occupation) # select from age to occupation
select(affairs, age, educ = education) # rename education to educ
select(affairs, age, sex, everything()) # same dataset, just reordered age and sex as first 2 vars
relocate(affairs, age, sex) #  reorder var
rename(affairs, educ = education) # rename var

mutate(affairs, age_in_decades = age/10) # create new var
mutate(affairs, age_in_decades = age/10, log_age = log(age)) # create multiple vars
mutate(affairs, relig_dummy = religious >= 4) # use condition to create dummy

transmute(affairs, log_age = log(age), relig_dummy = religious >= 4, sex) # create vars and select certain vars

summarize(affairs, mean_age = mean(age))
summarize(affairs, mean_age = mean(age, na.rm = TRUE), median_age = median(age), min_edu = min(education))  # multiple stats; ignore NA in age

pull(affairs, age) # get age for all obs in one vector
mean(pull(affairs, age))

affairs_by_sex <- group_by(affairs, sex) # it gives the same dataset, but now use functions on this new dataset, will do the func by group
summarise(affairs_by_sex, mean_age = mean(age)) 
affairs_grouped <- group_by(affairs, sex, religious) # group both by sex and religion
summarise(affairs_grouped, mean_age = mean(age))
affair_ungrouped <- ungroup(affairs)  # ungroup the grouped dataset
summarise(affair_ungrouped, mean_age = mean(age))

mutate(affairs_by_sex, mean_age = mean(age)) # create new var based on grouped summary

affairs_men <- filter(affairs, sex == 'male')
affairs_men_smaller <- select(affairs_men, sex, age, ym) # first filter by sex, then select certain vars
affairs_men_smaller <- select(filter(affairs, sex == 'male'), sex, age, ym) # equivalent, use function inside a function
affairs_men_smaller <- affairs %>% filter(sex == 'male') %>% select(sex, age, ym) # or use pipe, command + shift + m

affairs %>% group_by(sex) %>% summarise(mean_age = mean(age), median_age = median(age))

# joints ------------------------------------------------------------------

first_df <- tibble(
  country = c('Afghanistan', 'Belgium', 'China', 'Denmark'),
  population = c(333, 11, 1382, 57)
)
second_df <- tibble(
  country = c('Afghanistan', 'Belgium', 'Denmark', 'Germany'),
  gdp = c(35, 422, 211, 3232)
)
left_join(first_df, second_df)
left_join(first_df, second_df, by = "country") # or can specify which var do you want to base joint on
right_join(first_df, second_df)
inner_join(first_df, second_df)
full_join(first_df, second_df)
semi_join(first_df, second_df) # like inner joint, but only have population(from left dataset), no gdp
anti_join(first_df, second_df) # opposite to semi joint

# reshape dataset ------------------------------------------------------------
library(tidyr)
table4a
dirty_df <- table4a
pivot_longer(dirty_df, c("1999", "2000"))
tidy <- pivot_longer(dirty_df, c("1999", "2000"), names_to = 'year', values_to = 'gdp')
pivot_wider(tidy, id_cols = 'country', names_from = 'year', values_from = 'gdp', names_prefix = 'year_')

# exercise
data(Gasoline, package = 'plm')
gasoline <- as_tibble(Gasoline)
sub1 <- filter(Gasoline, year>=1969 & year<=1973)
sub1 <- filter(Gasoline, year %in% 1969:1973)
sub1 <- filter(gasoline, between(year, 1969,1973))

sub2 <- Gasoline %>% filter(year %in% c(1969, 1973, 1977)) %>% select(country, year, lrpmg)
sub3 <- Gasoline %>% select(-c('country', 'year','lrpmg'))
Gaso_rename <- rename(Gasoline, date = year)

gasoline_8 <- gasoline %>% select(starts_with('l'))

pull(Gasoline, lrpmg)

country <- group_by(Gasoline, country)
summarise(country, mean_lrpmg = mean(lrpmg))
un_country <- ungroup(country)
summarise(un_country, mean_lrpmg = mean(lrpmg))

Gasoline %>% filter(country == 'FRANCE') %>% summarise(avg_lgaspcar = mean(lgaspcar, na.rm = TRUE)) %>% pull(avg_lgaspcar)

sum <- summarise(country, mean_lga = mean(lgaspcar), sd_lga = sd(lgaspcar), min_lga = min(lgaspcar), max_lga = max(lgaspcar))

gasoline %>% group_by(country) %>% summarise(avg_lgaspcar = mean(lgaspcar, na.rm = TRUE)) %>% arrange(desc(avg_lgaspcar)) %>% slice(1) %>% pull(country)
gasoline %>% group_by(country) %>% summarise(avg_lgaspcar = mean(lgaspcar, na.rm = TRUE)) %>% filter(avg_lgaspcar == max(avg_lgaspcar))

gasoline %>% group_by(country) %>% summarise(count = n())
gasoline %>% group_by(country) %>% summarise(count = n()) %>% summarise(min_obs = min(count), max_obs = max(count)) %>% mutate(balanced = (min_obs == max_obs)) %>% pull(balanced)

gaso_19 <- gasoline %>% mutate(spam = exp(lgaspcar+lincomep))
gaso_19_2 <- gasoline %>% transmute(spam = exp(lgaspcar+lincomep))

gaso_20 <- gasoline %>% group_by(country) %>% mutate(lag_lgaspcar = lag(lgaspcar, order_by = year), lead_lgaspcar = lead(lgaspcar, order_by = year))

regions <- tibble(
  country = c('FRANCE', 'ITALY', 'GERMANY', 'CANADA', 'AUSTRIA'),
  region = c('med', 'med','ce', 'anglo', 'ce')
)
left_join(gasoline, regions, by = 'country')

gaso_25 <- gasoline %>% group_by(country) %>% mutate(avg_lgaspcar = mean(lgaspcar)) %>% mutate(high_cons = lgaspcar >= avg_lgaspcar)










