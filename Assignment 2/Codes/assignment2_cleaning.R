library(tidyverse)
df <- read_csv('listings.csv')

# Dropping unnecessary variables
drops <- c("name",
           "scrape_id",
           "source",
           "host_name",
           "license",
           "listing_url", 
           "last_scraped", 
           "description",
           "host_thumbnail_url",
           "neighborhood_overview",
           "host_picture_url",
           "thumbnail_url",
           "medium_url",
           "picture_url",
           "xl_picture_url",
           "host_url",
           "experiences_offered", 
           "notes", 
           "transit", 
           "access", 
           "interaction", 
           "house_rules", 
           "host_about", 
           "summary", 
           "space", 
           "host_location",
           "host_total_listings_count",
           "host_verifications",
           "minimum_minimum_nights","maximum_maximum_nights","minimum_maximum_nights",
           "maximum_minimum_nights","minimum_nights_avg_ntm","maximum_nights_avg_ntm", 
           "number_of_reviews_ltm", "is_business_travel_ready", 
           "calculated_host_listings_count_entire_homes", 
           "calculated_host_listings_count_private_rooms", 
           "calculated_host_listings_count_shared_rooms",
           "calendar_updated", "review_scores_accuracy",
           "review_scores_checkin", "review_scores_location",
           "review_scores_value", "review_scores_communication",
           "host_since"
)

df <- df[ , !(names(df) %in% drops)]

# Checking which variables have a lot of missing values
colSums(is.na(df))

# A lot of NAs for the following variables that may be not that significant:
# "host_neighbourhood" 8573
# "neighbourhood" 16932
# "bathrooms" 40438
# "bedrooms" 3603
# "last_review" 10585

# Dropping them 
drops <- c("host_neighbourhood", "neighbourhood", "bathrooms", "bedrooms", "last_review",
           "host_response_time", "host_response_rate", "host_acceptance_rate",
           "review_scores_cleanliness")
df <- df[ , !(names(df) %in% drops)]

# Converting price to numeric column
df$price <- as.numeric(gsub( "[\\$,]", "", as.character(df$price)))
#Checking price variable
summary(df$price) # doesn't make sense to have prices of 0 -> drop these observations
df <- df %>% filter(price > 0)
quantile(df$price, .95) # 95 percentile is 800, however, the max value is 99999 -> filter for prices < 800 USD
df <- df %>% filter(price <= 800)

# Formating binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified", "has_availability", "instant_bookable")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

# Checking `bathroom_text` variable and converting to numerical
df <- df %>% rename(bathrooms = bathrooms_text)
unique(df$bathrooms)

half_bath_index <- grep("half-bath", df$bathrooms)
df$bathrooms[half_bath_index] <- 0.5
df$bathrooms <- df$bathrooms %>% replace(df$bathrooms == 'Half-bath', "0.5")

df$bathrooms <- gsub("baths","",df$bathrooms)  
df$bathrooms <- gsub("bath","",df$bathrooms)
df$bathrooms <- gsub("private", "", df$bathrooms)
df$bathrooms <- gsub("shared", "", df$bathrooms)
df$bathrooms <- as.numeric(trimws(df$bathrooms))

# Amenities
df$amenities <- gsub("\\[","",df$amenities)
df$amenities <- gsub("\\]","",df$amenities)
df$amenities <- gsub('\\"',"",df$amenities)
df$amenities <- gsub('2013',"", as.character(df$amenities)) #removing \u2013
df$amenities <- gsub('2014',"", as.character(df$amenities)) #removing \u2014
df$amenities <- gsub('2019s',"", as.character(df$amenities)) #removing \u2019s
df$amenities <- gsub('2019',"", as.character(df$amenities)) # removing \u2019
df$amenities <- gsub('\\\\u',"", as.character(df$amenities)) # removing \u
df$amenities <- as.list(strsplit(df$amenities, ","))

unique(unlist(df$amenities)) # checking unique values for amenities and choosing the ones that may seriously increase the price

df$d_TV <- sapply(df$amenities, function(x) ifelse(length(grep("TV", x)) > 0, 1, 0))
df$d_hot_tub <- sapply(df$amenities, function(x) ifelse(length(grep("hot tub", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_wifi <- sapply(df$amenities, function(x) ifelse(length(grep("wifi", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_gym <- sapply(df$amenities, function(x) ifelse(length(grep("gym", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_sound_system <- sapply(df$amenities, function(x) ifelse(length(grep("sound system", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_dishwasher <- sapply(df$amenities, function(x) ifelse(length(grep("dishwasher", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_pool <- sapply(df$amenities, function(x) ifelse(length(grep("pool", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_balcony <- sapply(df$amenities, function(x) ifelse(length(grep("balcony", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_fridge <- sapply(df$amenities, function(x) ifelse(length(grep("fridge", x, ignore.case = TRUE)) > 0 |
                                                         length(grep("refrigerator", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_stove <- sapply(df$amenities, function(x) ifelse(length(grep("stove", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_coffee_machine <- sapply(df$amenities, function(x) ifelse(length(grep("coffee", x, ignore.case = TRUE)) > 0 |
                                                                 length(grep("espresso", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_good_view <- sapply(df$amenities, function(x) ifelse(length(grep("view", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_hair_dryer <- sapply(df$amenities, function(x) ifelse(length(grep("hair dryer", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_air_conditioner <- sapply(df$amenities, function(x) ifelse(length(grep("air conditioning", x, ignore.case = TRUE)) > 0 |
                                                                  length(grep("espresso", x, ignore.case = TRUE)) > 0, 1, 0))   
df$d_breakfast <- sapply(df$amenities, function(x) ifelse(length(grep("breakfast", x, ignore.case = TRUE)) > 0, 1, 0))
df$d_wardrobe <- sapply(df$amenities, function(x) ifelse(length(grep("wardrobe", x, ignore.case = TRUE)) > 0, 1, 0))

df <- df %>% select(-amenities)
#______________________________________________________________
# Filtering for the number of accommodates
df <- df %>% filter(df$accommodates >= 2 & df$accommodates <= 6)

# Keeping only apartments
unique(df$property_type)
df <- df %>% filter(property_type %in% c("Entire condo","Entire loft", "Entire serviced apartment", 
                                         "Entire home/apt", "Entire rental unit"))
unique(df$room_type)
df <- df %>% select(-room_type) #dropping `room_type` as there is only one value

# Renaming the property types
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire home/apt', "Apartment")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire serviced apartment', "Apartment")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire condo', "Condominium")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire loft', "Loft")
df$property_type <- df$property_type %>% replace(df$property_type == 'Entire rental unit', "Entire Unit")

# Factoring property types
df <- df %>% 
  mutate(f_property_type = factor(property_type))
df <- df %>% select(-property_type)

# Factoring `neighborhood_group_cleansed`
df <- df %>% mutate(f_neighbourhood_group_cleansed = factor(neighbourhood_group_cleansed))
df <- df %>% select(-neighbourhood_group_cleansed)

# Factoring `neighborhood_cleansed`
df <- df %>% mutate(f_neighbourhood_cleansed = factor(neighbourhood_cleansed))
df <- df %>% select(-neighbourhood_cleansed)

# Create dummy variables
dummies <- c("host_is_superhost", "host_identity_verified", "host_has_profile_pic" )
df <- df %>%
  mutate_at(vars(dummies), funs("d"= (.)))
dnames <- df %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(df))
colnames(df)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))
df <- df %>% select(-c(host_is_superhost, host_has_profile_pic, host_identity_verified))

# Create days since first review
df <- df %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))
df <- df %>% select(-c(calendar_last_scraped, first_review))

#Checking one more time which variables have missing values
colSums(is.na(df))

# Imputing values when there are not many of them and not very important
df <- df %>%
  mutate(
    review_scores_rating = ifelse(is.na(review_scores_rating), 0, review_scores_rating),
    reviews_per_month = ifelse(is.na(reviews_per_month), 0, reviews_per_month),
    n_days_since = ifelse(is.na(n_days_since), 0, n_days_since),
    bathrooms =  ifelse(is.na(bathrooms), median(bathrooms, na.rm = T), bathrooms), #assume at least 1 bath
    beds = ifelse(is.na(beds), accommodates, beds),
    d_host_is_superhost = ifelse(is.na(d_host_is_superhost), 0, d_host_is_superhost) # imputed 0 for one obs, as it is more frequent
  )

# Pool accommodations with 0,1,2,5 bathrooms
summary(df$bathrooms)
df <- df %>%
  mutate(f_bathroom = cut(bathrooms, c(0,1,2,5), labels=c(0,1,2), right = F))

# Pool num of reviews to 3 categories
summary(df$number_of_reviews)
df <- df %>%
  mutate(f_number_of_reviews = cut(number_of_reviews, c(0,1,51,max(df$number_of_reviews)), labels=c(0,1,2), right = F))

# Pool and categorize the number of minimum nights: 1,2,3, 3+
df <- df %>%
  mutate(f_minimum_nights= cut(minimum_nights, c(1,2,3,max(df$minimum_nights)), labels=c(1,2,3), right = F))

# Squares and further values to create
df <- df %>%
  mutate(accommodates2=accommodates^2, 
         ln_accommodates=log(accommodates) ,
         ln_accommodates2=log(accommodates)^2,
         ln_beds = log(beds),
         ln_number_of_reviews = log(number_of_reviews+1)
  )

df <- df %>%
  mutate(
    ln_days_since = log(n_days_since+1),
    ln_days_since2 = log(n_days_since+1)^2,
    ln_days_since3 = log(n_days_since+1)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3,
    ln_review_scores_rating = log(review_scores_rating)
  )
df <- df %>%
  mutate(ln_beds = ifelse(is.na(ln_beds), log(accommodates), ln_beds),
         f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
         f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
         f_bathroom = ifelse(is.na(f_bathroom), 1, f_bathroom))

write.csv(df, "cleaned.csv")
