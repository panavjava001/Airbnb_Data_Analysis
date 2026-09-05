library(dplyr) 





originalairbnb =  read.csv("AirbnbSydney.csv")
min_reviews = 15

#filtering data and storing it in a new variable for data manipulation & analysis

airbnb <- originalairbnb %>%
  filter(
    room_type == "Entire home/apt",
    number_of_reviews >= min_reviews
  )


hist(
  airbnb$price, 
  freq = FALSE, #for probability instead of count
  breaks = 50, #for more detail & to look better
  main = "Histogram of Price", 
  xlab = "Price ($)", 
  ylab = "Probability Density",
  col = "lightblue" #to look good
)



mu = mean(log(airbnb$price)) 
sg = sd(log(airbnb$price)) 

x = seq(min(airbnb$price),max(airbnb$price),length.out=1000)


y = dlnorm(x, meanlog = mu, sdlog = sg)


lines(x, y, col = "red", lwd = 2.5)


#mutate means adding new column to the dataset , adding weighted_score to dataset as a new column
airbnb = airbnb%>%
mutate(weighted_score = 0.5*airbnb$review_scores_rating + 0.1*airbnb$review_scores_accuracy+0.1*airbnb$review_scores_cleanliness+0.1*airbnb$review_scores_checkin+0.1*airbnb$review_scores_communication+0.1*airbnb$review_scores_location)


#filtering data to just get data for Warringah
warringah_data = airbnb%>%
  filter(neighbourhood=="Warringah")

warringah_review_mean = mean(warringah_data$weighted_score, na.rm=TRUE)

warringah_review_median = median(warringah_data$weighted_score, na.rm=TRUE)

warringah_review_sd = sd(warringah_data$weighted_score, na.rm=TRUE)


print(paste("Mean ",round(warringah_review_mean,6)))
print(paste("Median ",round(warringah_review_median,6)))
print(paste("Standard Deviation: ",round(warringah_review_sd,6)))


q10 = quantile(airbnb$weighted_score, 0.10, na.rm = TRUE)
q35 = quantile(airbnb$weighted_score, 0.35, na.rm = TRUE)
q65 = quantile(airbnb$weighted_score, 0.65, na.rm = TRUE)
q90 = quantile(airbnb$weighted_score, 0.90, na.rm = TRUE)

airbnb = airbnb%>%
  mutate( 
      Score.Bracket = case_when( #Casewhen statement (multiple if-else statements) to decide what value Score.bracket will get depending on the weighed_score
      weighted_score < q10 ~ "Low",
      weighted_score >= q10 & weighted_score < q35 ~ "Medium-Low",
      weighted_score >= q35 & weighted_score < q65 ~ "Medium",
      weighted_score >= q65 & weighted_score < q90 ~ "Medium-High",
      weighted_score >= q90 ~ "High"
    ),
     Score.Bracket = factor(Score.Bracket, levels = c("Low", "Medium-Low", "Medium", "Medium-High", "High"))
    )


mean_low = mean(filter(airbnb, Score.Bracket == "Low")$price, na.rm = TRUE)
mean_med_low = mean(filter(airbnb, Score.Bracket == "Medium-Low")$price, na.rm = TRUE)
mean_med = mean(filter(airbnb, Score.Bracket == "Medium")$price, na.rm = TRUE)
mean_med_high = mean(filter(airbnb, Score.Bracket == "Medium-High")$price, na.rm = TRUE)
mean_high = mean(filter(airbnb, Score.Bracket == "High")$price, na.rm = TRUE)


print(paste("Mean Price for Low Bracket:", round(mean_low, 4)))
print(paste("Mean Price for Medium-Low Bracket:", round(mean_med_low, 4)))
print(paste("Mean Price for Medium Bracket:", round(mean_med, 4)))
print(paste("Mean Price for Medium-High Bracket:", round(mean_med_high, 4)))
print(paste("Mean Price for High Bracket:", round(mean_high, 4)))

boxplot(
  price ~ Score.Bracket, 
  data = airbnb,
  ylim = c(0, 1150), #limiting the y-axis to $1000 so crazy high outliers don't squish the boxes
  main = "Price Distribution by Score Bracket",
  xlab = "Score Bracket",
  ylab = "Price ($ per night)",
  col = "green"
)



#if a host gets the Superhost badge from Airbnb, do they charge more money per night??

q5_data <- filter(originalairbnb, host_is_superhost == "t" | host_is_superhost == "f")


q5_data <- mutate(q5_data, Host_Type = ifelse(host_is_superhost == "t", "Superhost", "Regular Host"), price_per_guest = price/accommodates)

mean_superhost <- mean(q5_data$price_per_guest[q5_data$Host_Type == "Superhost"], na.rm = TRUE)
mean_regular   <- mean(q5_data$price_per_guest[q5_data$Host_Type == "Regular Host"], na.rm = TRUE)

mean_superhost
mean_regular
q5_prices <- c("Regular Host" = mean_regular, "Superhost" = mean_superhost)

barplot(
  q5_prices,
  col = c("lightblue", "red"),
  main = "Average Price per Guest by Host Type",
  ylab = "Price per Guest ($)",
  ylim = c(0,100) #to make it look neater
)
