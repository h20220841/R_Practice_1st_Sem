# library(tidyverse)
# @@@ dplyr
# Writing the code with dplyr involves using the " grammer of data" to perform 
 # filter <----- filtering rows
# select <----- selecting columns
# group_by <----- grouing the data
# mutate <----- changing or adding columns


# we pipe the results of one function into another using the (%>%) operator.

diamonds   # data
?diamonds
glimpse(diamonds) # shows overview of the data    dbl<---decimal, 
str(diamonds)    # overview of data
summary(diamonds)  # Shows statistical summary of the data
skim(diamonds)  # skimr package needed; better use of this

# ***pipes***

# ** when using the pipe, the left-hand side becomes the first argument of the
# * function of the right-hand side**

diamonds %>%
  head(5) %>%    # head <----- shows top 6-rows in default
  dim()          # dm() <----- dimension

# ** select function**
# the select function takes a data.frame(or tbl) as its first argument then the desired
# columns as subsequent argument.

colnames(diamonds)  # shows the columns name of the data
select(diamonds,carat,price)  # select only data of carat and price from diamons dataset

# or a better way is to use pipe

diamonds %>%
  colnames()    # show column name of the dataset of diamonds

diamonds %>%
  select(carat,price)   # select only mention the dataset

diamonds %>%
  select(c(carat,price))

diamonds %>%
  select('carat', 'price') %>%
  print(n=15)


thecols <-c('carat','price')  # name stored in a variable
View(thecols)

diamonds %>%
  select(all_of(thecols))

diamonds %>%
  select(1,7)

# searching for a partial match is done with the dplyr function start_with, end_with
# and contains

diamonds %>%
  select(starts_with('c'))  # show columns start with 'c'

diamonds %>%
  select(ends_with('e'))   # show columns end with 'e'

diamonds %>%
  select(contains('1'))  # show columns contains '1' in name



# Regular expression searches are done with mtches. the following code searches for 
# columns that contains the letter "r", followed by any number of wildcard matches and 
# the letter "t"

diamonds %>%
  select(matches('r.+t'))


# columns can be designated not to be selected by preceding the coli=umn names or
# number with the minus sign (-)
#by name
diamonds %>%
  select(-carat,price)



#by number
diamonds %>%
  select(-1,-2)

diamonds %>%
  select(c(1,2))

#***filter*** function ##
# Specifying rows based on a logical expression is done with filter.
diamonds %>%
  filter(cut == 'Ideal')

diamonds %>%
  filter(cut == 'Ideal') %>%
  nrow()                      # nrow <----- number of rows

diamonds[diamonds$cut == 'Ideal', ]   #old version 

# to filter in a column being equal to one of many possible values the %in% operator
#is used

diamonds %>%
  filter(cut %in% c('Ideal', 'Good')) %>%
  skim()

# all the standard equality operators can all be used with the filter.

diamonds %>%
  filter(price >= 1000)

diamonds %>%
  filter(price != 1000)

diamonds %>%
  filter(price <= 1000)
#compound filtering is accomplished by either separating the expression with a
#comma (,) or an ampersand (&)

diamonds %>%
  filter(carat > 2, price < 14000)

#A logical  *or* statement is expressed with a vertical pipe (l).

diamonds %>%
  filter(carat < 1 | carat >5)

#**slice** 
#While filter is used for specifying rows based on a logical expression, slice is used for
#specifying rows by row number. the desired indices are passed as a vector to slice

diamonds %>%
  slice(1:5)

diamonds %>%
  slice(1:5,8,15:20)

# Negative indices are used to indicate rows that should not be returned

diamonds %>%
  slice(-1)

#**mutate function**
#creating new columns or modifying existing cloumns is done with the mutate function


#creating a new columns that is he ratio of price and carat is as simple as
#providing that ratio  as an argumnt to mutate
diamonds %>%
  mutate(price/carat)


#depending on the size of the terminal, not all columns will be priented to the screen. to
#ensure this new cloumns can fit on the screen , we select a few columns of intereset using 
#select and then pipe that result into mutate.

diamonds %>%
  select(carat,price) %>%
  mutate(price/carat)

#the resulting columns is unnamed which is easily remedid by assigning ythe expression
#(carat/price)


diamonds %>%
  select(carat,price) %>%
  mutate(Ratio=price/carat, double=Ratio*2)




#**summarize function**
#summarize applies functions that return a resut of lengrth one such as mean, max, median or

summarize(diamonds,mean(price))

diamonds %>%
  summarize(mean(price))

#perform multiple calculation in same call
diamonds %>%
  summarize(Avgprice=mean(price),
            medinprice=median(price),
            avgcarat=mean(carat))

#**group_by**
#to split the data according to a variable and then apply a summary function
#to each partition , the data is first passed to group_by AND the resulting group
#data.frame or tbl is passed to summafrise, which allows functions to be applied to
#individual columns

#indiviual columns.

diamonds %>%
  group_by(cut)%>%
  summarize(AvgPrice=mean(price))

diamonds %>%
  group_by(cut)%>%
  summarize(AvgPrice=mean(price),
            SumCarat=sum(carat))

diamonds %>%
  group_by(cut, color)%>%
  summarize(AvgPrice=mean(price),
            SumCarat=sum(carat))


#**arrange function**
#sorting is performed with the arrange function
diamonds %>%
  group_by(cut)%>%
  summarize(AvgPrice=mean(price),
            SumCarat=sum(carat)) %>% 
  arrange(AvgPrice)   #ascending is the default type
#see the ligical flow of the function

diamonds %>%
  group_by(cut)%>%
  summarize(AvgPrice=mean(price),
            SumCarat=sum(carat)) %>% 
  arrange(desc(AvgPrice))   #descending sort

#reashaping the data in the tidyverse
library(tidyverse)

#bind_rows, bind_cols, can   bind multiple tibbles (or data,frame) together
# create a two_column

DiamondColors<-read.csv("DiamondColors.csv")
head(DiamondColors)
summary(DiamondColors)

# left join statement
left_join_data<-left_join(diamonds,DiamondColors,by=c("color"="Color"))
View(left_join_data)
dim(left_join_data)

# right join statement
