# Download data from https://data.sfgov.org/Public-Safety/SFPD-Incidents-Previous-Year-2015-/ritf-b9ki
# Rename the csv to 'sfpd.csv'



#############################################
# Base R
#############################################

2 + 3

runif(100)

# Create a vector called unif with 100 uniform samples

plot(unif)

hist(unif)

# Create a vector called norm with 100 normally distributed samples

plot(norm)
hist(norm)

# create a dataframe called df with columns unif and norm

df$comp <- ifelse(df$unif > df$norm, 1, 2)

table(df$comp)

plot(df$norm, df$unif, col=df$comp, cex=df$comp)


############################################
# Read Data
############################################

sfpd <- read.csv('sfpd.csv')

# Note: If this data set bogs down your machine, take a subset with
# sfpd <- sfpd[1:10000, ]
# This will take the first 10,000 rows instead of the 150K rows

names(sfpd)

View(sfpd)

summary(sfpd)

table(sfpd$DayOfWeek)

plot(sfpd$X, sfpd$Y)

# plot this lat long with size and color options to get a usable graph



###########################################
# Packages
###########################################

# Install the packages below

library(dplyr)
library(ggplot2)


crime_centers <-
  sfpd %>%
  group_by(Category) %>%
  summarize(
    count=n(),
    lon_mean=mean(X),
    lat_mean=mean(Y),
    resolved=sum(ifelse(Resolution=='NONE', 0, 1)),
    resolved_pct=resolved / count
  )

plot(sfpd$X, sfpd$Y, cex=.1, col=sfpd$Category)
#Use the points command to overlay crime centers on the plot




#Now for ggplot

ggplot() +
  geom_point(
    data=sfpd,
    aes(x=X, y=Y, color=Category),
    size=.1
  )

# Use the + command in ggplot to overlay crime centers


crime_centers <-
  crime_centers %>%
  arrange(resolved_pct)

View(crime_centers)

ggplot() +
  geom_bar(
    data=crime_centers,
    aes(x=Category, y=resolved_pct),
    stat="identity"
  )

# Factor order is what dominates here

crime_centers$Category <-
  factor(
    crime_centers$Category,
    levels = crime_centers$Category[order(crime_centers$resolved_pct)]
  )

ggplot() +
  geom_bar(
    data=crime_centers,
    aes(x=Category, y=resolved_pct),
    stat="identity"
  )


# Make the above ggplot pretty
# Hint element_text(angle = 90, hjust = 1) will help with the axis labels



###############################################
# Machine Learning
###############################################

# Install party package

library(party)

# Feel Free to run this on the whole data set, but it will take a long time
# I will trim the data down as a demo

#Let's set a target column. Numerical targets are needed in this case



# Create a column sfpd$res with a 0 value if Resolution=='None', 1 otherwise

table(sfpd$res)

set.seed(7463)

train_rows <-
  sample(nrow(sfpd), nrow(sfpd) * .10)

train_df <-
  sfpd %>%
  slice(train_rows)

# Create test_df with non train_rows


# Crete a formula like res ~ Category
sfpd_cforest <-
  cforest(
    # ...,
    data=train_df,
    control = cforest_unbiased(mtry = 2, ntree = 50)
  )

sfpd_cforest

varimp(sfpd_cforest)

#This takes a long time, so I will cut the test_df down as well

test_df <-
  test_df %>%
  slice(1:1000)

prediction <-
  predict(sfpd_cforest, newdata=test_df)

str(prediction)

prediction <-
  as.numeric(prediction)

table(test_df$res, prediction > .5)

test_df$pres_bin <- ifelse(prediction > .5, 1, 0)
test_df$matches <- ifelse(test_df$res == test_df$pres_bin, 1, 0)

# Use the summary command to create a dataframe with columns ct, correct, pred_pct
# ct is the count, correct is pres_bin matches res and pred_pct is the ratio of predicted to ct
prediction_accuracy <-
  test_df %>%
  group_by(Category) %>%
# ...


prediction_accuracy$Category <-
  factor(
    prediction_accuracy$Category,
    levels = prediction_accuracy$Category[order(prediction_accuracy$pred_pct)]
  )

# Using ggplot, plot the prediction accuracy by Category using ggplot
