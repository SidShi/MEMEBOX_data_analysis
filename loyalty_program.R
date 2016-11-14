library(RMySQL)
library(zoo)
library(dplyr)
library(MASS)
library(BTYD)
library(lubridate)

## Connect to MySQL database 'con'

# When needed to disconnect from the database
dbDisconnect(con)

# Get customer registration time
customer_registration <- dbGetQuery(con, "SELECT entity_id, created_at FROM customer_entity")
customer_registration$created_at <- substr(customer_registration$created_at,1,10)
names(customer_registration) <- c("customer_id", "registration_time")

# Get all successful purchase history of customers
cus_purchase <- dbGetQuery(con, "SELECT customer_id, created_at, grand_total, is_gwp FROM sales_flat_order where status = 'complete'")
cus_purchase$created_at <- substr(cus_purchase$created_at,1,10)
# If needed to adjust the time period of studying, for example here I'm extracting all purchase history before 7/1/16
cus_purchase <- cus_purchase[cus_purchase$created_at < "2016-07-01",]


# Clean unreasonable and useless data
cus_purchase <- cus_purchase[!is.na(cus_purchase$grand_total) & cus_purchase$grand_total > 0 & cus_purchase$grand_total < 50000 & cus_purchase$is_gwp == 0,]
cus_purchase <- cus_purchase[!is.na(cus_purchase$customer_id),]
cus_purchase <- cus_purchase[,-4]





## FIRST method of loyalty division (with the original method -- using purchase period, number of purchases and grand total)

# Calculate the number of years between the purchase and the set time
cus_purchase$yeartillnow <- ceiling(as.yearmon("2016-07-01") - as.yearmon(cus_purchase$created_at))
cus_purchase$yeartillnow <- sapply(cus_purchase$yeartillnow, function(x) ifelse(x == 0,1,x))
# Calculate the number of quarters between the purchase and the set time
cus_purchase$quartertillnow <- ceiling(((as.yearmon("2016-07-01")-as.yearmon(cus_purchase$created_at))*12)/3)
cus_purchase$quartertillnow <- sapply(cus_purchase$quartertillnow, function(x) ifelse(x == 0, 1, x))

# Summarise for each time period for each customers
# one quarter
cus_1q <- cus_purchase[cus_purchase$quartertillnow == 1,]
summary_1q <- cus_1q %>% group_by(customer_id) %>% summarise(total_1q = sum(grand_total), purchase_1q = n())

# two quarters
cus_2q <- cus_purchase[cus_purchase$quartertillnow == 1 | cus_purchase$quartertillnow == 2,]
summary_2q <- cus_2q %>% group_by(customer_id) %>% summarise(total_2q = sum(grand_total), purchase_2q = n())

# one year
cus_1y <- cus_purchase[cus_purchase$yeartillnow == 1,]
summary_1y <- cus_1y %>% group_by(customer_id) %>% summarise(total_1y = sum(grand_total), purchase_1y = n())

# two years
summary_2y <- cus_purchase %>% group_by(customer_id) %>% summarise(total_2y = sum(grand_total), purchase_2y = n())

# Merge
summary_12q <- merge(summary_1q, summary_2q, all.y = TRUE, by = "customer_id")
summary_q1y <- merge(summary_12q, summary_1y, all.y = TRUE, by = "customer_id")
summary_12y <- merge(summary_q1y, summary_2y, all.y = TRUE, by = "customer_id")
summary_all <- merge(summary_12y, customer_registration, all.y = TRUE, by = "customer_id")

# clean
summary_all <- summary_all[as.yearmon(summary_all$registration_time) < as.yearmon("2016-07-01"),]
summary_all <- summary_all[summary_all$registration_time != "1980-01-01",]
summary_all$total_1q <- sapply(summary_all$total_1q, function(x) ifelse(is.na(x),0,x))
summary_all$purchase_1q <- sapply(summary_all$purchase_1q, function(x) ifelse(is.na(x),0,x))
summary_all$total_2q <- sapply(summary_all$total_2q, function(x) ifelse(is.na(x),0,x))
summary_all$purchase_2q <- sapply(summary_all$purchase_2q, function(x) ifelse(is.na(x),0,x))
summary_all$total_1y <- sapply(summary_all$total_1y, function(x) ifelse(is.na(x),0,x))
summary_all$purchase_1y <- sapply(summary_all$purchase_1y, function(x) ifelse(is.na(x),0,x))
summary_all$total_2y <- sapply(summary_all$total_2y, function(x) ifelse(is.na(x),0,x))
summary_all$purchase_2y <- sapply(summary_all$purchase_2y, function(x) ifelse(is.na(x),0,x))







# Categorize according to the standards (this one is for the new standard I design)
summary_all$grade <- NA
for (i in 1:nrow(summary_all)) {
  if (summary_all$total_2y[i] >= 15000 & summary_all$purchase_2y[i] >= 40) {
    summary_all$grade[i] <- 8
  } else if (summary_all$total_2y[i] >= 7500 & summary_all$purchase_2y[i] >= 30) {
    summary_all$grade[i] <- 7
  } else if (summary_all$total_1y[i] >= 3500 & summary_all$purchase_1y[i] >= 15) {
    summary_all$grade[i] <- 6
  } else if (summary_all$total_1y[i] >= 2000 & summary_all$purchase_1y[i] >= 7) {
    summary_all$grade[i] <- 5
  } else if (summary_all$total_1q[i] >= 400 & summary_all$purchase_1q[i] >= 2) {
    summary_all$grade[i] <- 4
  } else if (summary_all$total_2y[i] >= 250 & summary_all$purchase_2y[i] >= 2) {
    summary_all$grade[i] <- 3
  } else if (summary_all$total_2y[i] == 0) {
    summary_all$grade[i] <- 1
  } else {
    summary_all$grade[i] <- 2
  }
}




## SECOND method of loyalty division (method 3 in the document, division related to "charging")

# Find out for each customer the earliest purchase and total points earned by purchasing
summary_charge <- cus_purchase %>% group_by(customer_id) %>% summarise(earliest_purchase = min(created_at), total_points = sum(grand_total))
# Calculate how many points need to be deducted, for example here I'm also studying 7/1/16
summary_charge$subpoints <- ymd("2016-07-01")-ymd(summary_charge$earliest_purchase)
summary_charge$points <- NA
summary_charge$points <- summary_charge$total_points-summary_charge$subpoints
# The lowest point for customers who have made purchase is 0
summary_charge$points <- sapply(summary_charge$points, function(x) max(0,x))

sum_all <- merge(summary_charge, customer_registration, all.y = TRUE, by = "customer_id")
charge_grade <- sum_all[,c(1,5)]
# For those who haven't made any purchase, the point is -1
charge_grade$points <- sapply(charge_grade$points, function(x) ifelse(is.na(x),-1,x))
charge_grade$grade <- NA
# Categorize
charge_grade[charge_grade$points == -1,]$grade <- 1
charge_grade[charge_grade$points >= quantile(charge_grade$points, 0.93),]$grade <- 3
charge_grade[charge_grade$points >= quantile(charge_grade$points, 0.988),]$grade <- 4
charge_grade[charge_grade$points >= quantile(charge_grade$points, 1-2.74e-5-1.05e-4-3.84e-4-1.62e-3),]$grade <- 5
charge_grade[charge_grade$points >= quantile(charge_grade$points, 1-2.74e-5-1.05e-4-3.84e-4),]$grade <- 6
charge_grade[charge_grade$points >= quantile(charge_grade$points, 1-2.74e-5-1.05e-4),]$grade <- 7
charge_grade[charge_grade$points >= quantile(charge_grade$points, 1-2.74e-5),]$grade <- 8
charge_grade[is.na(charge_grade$grade),] <- 2
