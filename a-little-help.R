# Download data from https://data.sfgov.org/Public-Safety/SFPD-Incidents-Previous-Year-2015-/ritf-b9ki
# Rename the csv to 'sfpd.csv'



#############################################
# Base R
#############################################

2 + 3

runif(100)

unif <- runif(100)

plot(unif)

hist(unif)

norm <- rnorm(100)

plot(norm)
hist(norm)

df <- data.frame(unif=unif, norm=norm)

df$comp <- ifelse(df$unif > df$norm, 1, 2)

table(df$comp)

plot(df$norm, df$unif, col=df$comp, cex=df$comp)


############################################
# Read Data
############################################

sfpd <- read.csv('sfpd.csv')

#Note: If this data set bogs down your machine, take a subset with
#sfpd <- sfpd[1:10000, ]
#This will take the first 10,000 rows instead of the 150K rows

names(sfpd)

View(sfpd)

summary(sfpd)

table(sfpd$DayOfWeek)

plot(sfpd$X, sfpd$Y)


# Plot the above with descreased size (cex is the size option)
# plot(sfpd$X, sfpd$Y, ...)

# Plot the above with descreased size (cex is the size option)
# and color by Category
# plot(sfpd$X, sfpd$Y, ...)



###########################################
# Packages
###########################################


#Install the packages below

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
# Use the points command to overlay crime centers

## points(..., pch=3, col='yellow')


#Now for ggplot

ggplot() +
  geom_point(
    data=sfpd,
    aes(x=X, y=Y, color=Category),
    size=.1
  )


# Add crime centers using ggplot
ggplot() +
  geom_point(
    data=sfpd,
    aes(x=X, y=Y, color=Category),
    size=.1
  ) +
  # geom_point(
  #   data=crime_centers,
  #   aes(...),
  #   size=2,
  #   shape=4
  # ) +
  # theme_bw()

# Use the arrange command to order crime_centers by resolved_pct
# crime_centers <-
#   crime_centers %>%
#   ...

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

# Add a title using ggtitle, axis lables using xlab and ylab
# and use theme_bw() to make the plot prettier.
ggplot() +
  geom_bar(
    data=crime_centers,
    aes(x=Category, y=resolved_pct, fill=resolved_pct),
    stat="identity"
  ) +
  # ...
  # theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # ...



###############################################
# Machine Learning
###############################################

# Install the party package

library(party)

# Feel Free to run this on the whole data set, but it will take a long time
# I will trim the data down as a demo

#Let's set a target column. Numerical targets are needed in this case

sfpd$res <- ifelse(sfpd$Resolution=='NONE', 0, 1)

table(sfpd$res)

set.seed(7463)

train_rows <-
  sample(nrow(sfpd), nrow(sfpd) * .10)

train_df <-
  sfpd %>%
  slice(train_rows)


# Use the slice command to take non-train rows for our test set
test_df <-
  sfpd %>%
 # ...

sfpd_cforest <-
  cforest(
    res ~ Category + DayOfWeek + PdDistrict + X + Y,
    data=train_df,
    control = cforest_unbiased(mtry = 2, ntree = 50)
  )

sfpd_cforest

varimp(sfpd_cforest)



# This takes a long time, so I will cut the test_df down as well

test_df <-
  test_df %>%
  slice(1:1000)


prediction <-
  predict(sfpd_cforest, newdata=test_df)


str(prediction)

# have to convert from a N x 1 matrix to a vector
prediction <-
  as.numeric(prediction)


table(test_df$res, prediction > .5)

# Write an ifelse statement to assign a 1 and 0 if prediciton is >= .5
# test_df$pres_bin <- ifelse(..., 1, 0)
test_df$matches <- ifelse(test_df$res == test_df$pres_bin, 1, 0)

#Add a column called pred_pct which is the ratio of correct to ct
prediction_accuracy <-
  test_df %>%
  ungroup() %>%
  group_by(Category) %>%
  summarize(
    ct=n(),
    correct=sum(matches),
#    ...
  )

# Copy the factor ordering code above to order our new data set by pred_pct
prediction_accuracy$Category <-
  factor(
    prediction_accuracy$Category,
#    ...
  )


# Make this plot prettier
ggplot() +
  geom_bar(
    data=prediction_accuracy,
    aes(x=Category, y=pred_pct, fill=pred_pct),
    stat="identity"
  ) +
#


