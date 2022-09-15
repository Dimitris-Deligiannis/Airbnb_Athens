install.packages(dplyr)
install.packages(tidyverse)
library(dplyr)
library(tidyverse)

# Input data from file Project --> listings.csv
airbnb <- read.csv("listings.csv", header = TRUE,encoding="UTF-8")

# Show dimensions, data and names
dim(airbnb)
glimpse(airbnb)
names(airbnb)

# remove unavailable listings
airbnb <- filter(airbnb, has_availability=='t')

# remove NA listings which have NA or 0 reviews
airbnb <- filter(airbnb, number_of_reviews > 0)

# remove features id, listing_url, scrape_id, name...
airbnb <- select(airbnb, -c(
  id,
  listing_url,
  scrape_id,
  name,
  description,
  picture_url,
  calendar_last_scraped,
  calendar_updated,
  has_availability,
  
  host_url,
  host_name,
  host_since,
  host_id,
  host_location,
  host_about,
  host_response_rate,
  host_acceptance_rate,
  host_thumbnail_url,
  host_picture_url,
  host_neighbourhood,
  host_total_listings_count,
  host_has_profile_pic,
  host_verifications,
  
  neighbourhood,
  neighbourhood_group_cleansed,
  neighborhood_overview,
  bathrooms,
  # remove bedrooms because of NAs
  bedrooms,
  # remove scores_rating, same as scores_value
  review_scores_rating,
  
  minimum_nights,
  minimum_minimum_nights,
  minimum_maximum_nights,
  minimum_nights_avg_ntm,
  maximum_nights,
  maximum_minimum_nights,
  maximum_maximum_nights,
  maximum_nights_avg_ntm,
  calculated_host_listings_count,
  calculated_host_listings_count_entire_homes,
  calculated_host_listings_count_private_rooms,
  calculated_host_listings_count_shared_rooms,
  
  license))

# view new dataset
glimpse(airbnb)
dim(airbnb)

# t/f to 1/0 conversion
airbnb <- mutate(airbnb, 
                 host_is_superhost = as.integer(ifelse(host_is_superhost == 't', 1, 0)),
                 host_identity_verified = as.integer(ifelse(host_identity_verified == 't', 1, 0)),
                 instant_bookable = as.integer(ifelse(instant_bookable == 't', 1, 0))
                 )

library(stringr)
# Remove dollar sign from price
airbnb$price<-as.integer(str_replace_all(airbnb$price,"\\$|,",""))

# Dates processing
library(lubridate)
# Chr to date conversion Year - Month - Day
airbnb <- mutate(airbnb,
                 first_review = ymd(first_review),
                 last_review = ymd(last_review),
                 last_scraped = ymd(last_scraped)
                 )

# Days listing duration <- abs(last_scraped - first_review)
# Days last review - first review <- abs(last_review - first_review)
# Days from last review <- abs(last_scraped - last_review)
airbnb <- airbnb %>%
          mutate (
                 listing_dur = abs(as.integer(difftime(last_scraped,first_review, units = 'days'))),
                 lastrev_firstrev_dur = abs(as.integer(difftime(last_review,first_review, units = 'days'))),
                 from_lastrev_dur = abs(as.integer(difftime(last_scraped,last_review, units = 'days')))
                 ) %>%
          # Remove date columns (first_review, last_review, last_scraped)
          select (
            -c(first_review, last_review, last_scraped)
            )

# Convert bathrooms_text to numeric and shared/non-shared, private/non-private bathrooms
airbnb <- airbnb %>% 
          mutate ( 
                 # Replace half or Half in bathrooms_text with 0.5
                 bathrooms_text = gsub("half", "0.5", bathrooms_text, ignore.case = TRUE),
                 
                 # Create new column bath_sum and extract only numeric values from bathrooms_text
                 bath_sum = as.numeric(str_extract(bathrooms_text,"\\d\\.{0,1}\\d{0,1}")),
                 
                 # 1 or 0 if bathroom/s shared or private, Drop bathrooms_text
                 bath_shared = as.integer(ifelse(str_detect(bathrooms_text,"share"), 1,0)),
                 bath_private = as.integer(ifelse(str_detect(bathrooms_text,"priv"), 1,0)),
                 bathrooms_text = NULL
                 ) %>%
          # Keep listings which aren't NAs in bath_sum
          filter (
            !is.na(bath_sum)
            )
glimpse(airbnb)

#NAs handling
library(mice)
library(glmnet)
# mice: Multivariate Imputation by Chained Equations
impute <- mice(airbnb, 
               m=5,
               seed=123,
               method =
                 # Experiments with different methods (final=pmm, 3rd iteration)
                 #"norm.predict"
                 #"lasso.select.norm"
                 "pmm"
                 #"midastouch" 
               )
#Print impute object and create plots to examine imputation
print(impute)
stripplot(impute, beds)
stripplot(impute, host_listings_count)
stripplot(impute, review_scores_value)

# Complete data imputation and remove "impute" object
airbnb <- complete(impute, 3)
rm(impute)

# Create variables
airbnb <- airbnb %>% mutate(
                 # Amenities count
                 total_amenities = ifelse(nchar(amenities)>2, str_count(amenities, ',') +1, 0),
                 # Is in top 100 in ranking
                 is_top_100 = ifelse(rank(-number_of_reviews, ties.method = "average") <= 100, 1, 0)
                 )

# Replace anything which isn't chr or digit with "_"
airbnb <- airbnb %>% mutate(
              neighbourhood_cleansed = str_replace_all(neighbourhood_cleansed, "[^[:alnum:]]", "_"),
              room_type = str_replace_all(room_type, "[^[:alnum:]]", "_"),
              property_type = str_replace_all(property_type, "[^[:alnum:]]", "_"),
              host_response_time = str_replace_all(host_response_time, "[^[:alnum:]]", "_")
              )

# Creating dummy variables 0/1 on neighbourhood_cleansed, room_type, property_type, host_response_time
library(dummies)
nb_dummy <- dummy(airbnb$neighbourhood_cleansed, sep = "_")
room_type_dummy <- dummy(airbnb$room_type, sep = "_")
property_type_dummy <- dummy(airbnb$property_type, sep = "_")
host_response_time_dummy <- dummy(airbnb$host_response_time, sep = "_")

# Bind dummy dataframes to airbnb dataframe
airbnb <- airbnb %>%
  cbind(nb_dummy) %>%
  cbind(room_type_dummy) %>%
  cbind(property_type_dummy) %>%
  cbind(host_response_time_dummy)
# Remove dummy dataframes
rm(host_response_time_dummy, room_type_dummy, nb_dummy, property_type_dummy)

##### Export Rapidminer for tf_idf processing in amenities
# Write data to csv
library(writexl)
write_xlsx(airbnb, path = "airbnb_imputed.xlsx")

#### Rapidminer processing
# Read excel 
# Nominal to text
# Process Documents from Data, TF-EDF, prune <= 10% AND prune => 80%
# Transform cases, Tokenize, Filter words, Filter Stopwords, Stem, Create n-Grams

##### ----Import data from Rapidminer | Data file = airbnb_tfidf.csv ----#######
airbnb_2 <- read.csv("airbnb_tfidf.csv", header = TRUE,encoding="UTF-8", sep = ";")

# Renaming columns names in 3 categories bs = basic , dm = dummy, tf = tfidf
colnames(airbnb_2)[1:35] <- paste('bs', colnames(airbnb_2[1:35]), sep = ".")
colnames(airbnb_2)[36:131] <- paste('dm', colnames(airbnb_2[36:131]), sep = ".")
colnames(airbnb_2)[132:314] <- paste('tf', colnames(airbnb_2[132:314]), sep = ".")

# Convert character columns to factor (host_response_time,neighborhood,room_type, property_type)
airbnb_2 <- as.data.frame(unclass(airbnb_2), stringsAsFactors = TRUE)
str(airbnb_2)

#log Transform price variable
#log(price) which helps us manage the outliers with excessively large prices
airbnb_2$bs.log_price <- log10(airbnb_2$bs.price)


############## Exploratory analysis & ploting ##############
install.packages(ggplot2)
library(ggplot2)
# Count bs.neighbourhood_cleansed
ggplot(data=airbnb_2,
       aes(x=reorder(bs.neighbourhood_cleansed,bs.neighbourhood_cleansed,
                     function(x)length(x))))+
  geom_bar(width=0.5)+
  labs(y= "Number of listings",x="Area",title = "Number of listings per area")+
  theme(plot.title = element_text(hjust = 0.5))+
  coord_flip()

## Box plot bs.neighbourhood_cleansed with bs.price
ggplot(data = airbnb_2,
       aes(x = reorder(bs.neighbourhood_cleansed, bs.price, median), 
           y = bs.price, 
           fill= bs.neighbourhood_cleansed
           )) +
  geom_boxplot(show.legend = FALSE) +
  labs(y= "Price (log scale)",x="Neighbourhoods",title = "Price in descending order based on median price grouped by neighbourhoods")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_hue() +
  scale_y_log10() +
  coord_flip()

## Box plot bs.property_type with bs.price
ggplot(data = airbnb_2,
       aes(x = reorder(bs.property_type, bs.price, median), 
           y = bs.price, 
           fill= bs.property_type
       )) +
  geom_boxplot(show.legend = FALSE) +
  labs(y= "Price (log scale)",x="Property Type",title = "Price in descending order based on median price grouped by property type")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_hue() +
  scale_y_log10() +
  coord_flip()

## scatter plot bs.price with bs.number_of reviews 
ggplot(airbnb_2, aes(y=bs.number_of_reviews, x=bs.price)) +
  labs(x= "Price (log scale)",y="Number of reviews",title = "Price per number of reviews")+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_point()+
  scale_x_log10()

##pie chart bs.room_type #
ggplot(airbnb_2, aes(x="", y=bs.room_type, fill=bs.room_type)) +
  geom_bar(stat="identity", width=1) +
  labs(title = "Number of room type")+
  coord_polar("y", start=0)+
  theme_void()

## Box plot bs.room_type with bs.price
ggplot(data = airbnb_2,
       aes(x = reorder(bs.room_type, bs.price, median), 
           y = bs.price, 
           fill= bs.room_type
       )) +
  geom_boxplot(show.legend = FALSE) +
  labs(y= "Price (log scale)",x="Room type",title = "Price in descending order based on median price grouped by room type")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_hue() +
  scale_y_log10() +
  coord_flip()



##Create new feature bs.price_groups
library (dplyr)
airbnb_2 <- mutate(airbnb_2, 
        bs.price_groups = case_when(
        bs.price<=30 ~ "low",
        bs.price <= 70 ~ "medium",
        bs.price <= 200 ~ "high",
        bs.price <= 5000 ~ "super high",
        bs.price<= 10000 ~ "extreme high"
))
## Create new feature bs.colour_group
airbnb_2 <- mutate (airbnb_2, bs.colour_group = cut(airbnb_2$bs.price, breaks = c("1", "70", "200", "5000", "10000"),
            labels = c("green","orange","red","purple"),include.lowest = TRUE))

## Read colours as they created in the feature bs.colour_group
install.packages(leaflet)
library(leaflet)
icons <- awesomeIcons(icon = "ios-close",
                      iconColor = "black",
                      library = "ion",
                      markerColor = airbnb_2$bs.colour_group)

## Map plot bs.latitude, bs.longitude with bs.price , 
m <- leaflet() %>% setView(lng = 23.7298, lat = 37.9707, zoom = 13)
m %>% addTiles() %>%
addAwesomeMarkers(lat=airbnb_2$bs.latitude,lng=airbnb_2$bs.longitude,popup=airbnb_2$bs.price,icon = icons,label=airbnb_2$bs.price)


########### PCA on tf ############
# Select 183 tf variables
pca_airbnb_tf <- select(airbnb_2, starts_with('tf'))
# Create components
myPr <- prcomp(pca_airbnb_tf)
# Examine components with threshold 0.8
summary(myPr)
# Select 33 components with cumulative proportion and bind to airbnb_2
airbnb_2 <- cbind(airbnb_2, myPr$x[,1:33])

# Remove 183 tf variables
airbnb_2 <- select(airbnb_2, !starts_with('tf'))

# Remove 2 price variables used in plotting
airbnb_2 <- select(airbnb_2, -c(bs.price_groups, bs.colour_group))

# Create new dataframe without dummy variables
airbnb_3 <- select(airbnb_2, !starts_with('dm'))

#Remove bs.price from the new dataframe
airbnb_3$bs.price <- NULL

# Remove unused objects
rm(myPr, pca_airbnb_tf)

########## Modeling ##########

###Multiple Linear Regression -- rmse: 0.1817
#install.packages('caret', dependencies=T)
library(caret)
library(lattice)
set.seed(100) # analysis reapeatability 
# 80% training set & 20% test set
index <- createDataPartition(airbnb_3$bs.log_price, p = 0.8, list = FALSE)

# Create training set
#trainData <- airbnb_3[index,c(2:4,10:68)] # Select only numeric
# Select all data
trainData <- airbnb_3[index,]

# Create test set
#testData <- airbnb_3[-index,c(2:4,10:68)] # Select only numeric
# Select all data
testData <- airbnb_3[-index,]

# 5-fold cross-validation 5 repeats
control <- trainControl(
  method = "repeatedcv", 
  number = 5, 
  repeats = 5)

# mlr_model
set.seed(123)
mlr_model <- train(
  bs.log_price ~ ., 
  data = trainData, 
  method = "lm",
  trControl = control)
pred_mlr <- predict(mlr_model, newdata = testData)
forecast::accuracy(pred_mlr, testData$bs.log_price)

summary(mlr_model)
varImp(mlr_model)

rm(mlr_model, pred_mlr)

###RANDOM FOREST (400 trees) -- rmse : 0,1704

# rf_model
library(randomForest)
set.seed(123)
rf_model <- train(
  bs.log_price ~ ., 
  data = trainData, 
  method = "rf",
  trControl = control,
  ntree=400,
  importance = TRUE)

pred_rf <- predict(rf_model, newdata = testData)
forecast::accuracy(pred_rf, testData$bs.log_price)

summary(rf_model)
varImp(rf_model)

rm(rf_model, pred_rf)

# airbnb_4 only numeric and dummy variables
airbnb_4 <- select(airbnb_2, -c(
  bs.price,
  bs.host_response_time,
  bs.neighbourhood_cleansed,
  bs.property_type,
  bs.room_type
))

##normalization(scaling) airbnb_4
scaled_airbnb_4 = as.data.frame(scale(airbnb_4)) 

###svm rmse -- 0.663
library(e1071)
set.seed(100) # analysis reapeatability 
# 80% training set & 20% test set
index2 <- createDataPartition(scaled_airbnb_4$bs.log_price, p = 0.8, list = FALSE)
# Create train set
trainData2 <- scaled_airbnb_4[index,]
# Create test set
testData2 <- scaled_airbnb_4[-index,]

model_svm = svm(bs.log_price~.,trainData2)
pred_svm = predict(model_svm, testData2)

forecast::accuracy(pred_svm, testData2$bs.log_price)
summary(model_svm)