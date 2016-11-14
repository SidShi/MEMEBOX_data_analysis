library(RMySQL)

# Build connection to the MySQL database 'con'

### FOR DATA CLEANING PROCESS AND METHOD, PLEASE REFER TO THE ATTACHED DOCUMENT

## Get the valid customer registration time
customer_entityc <- dbGetQuery(con, "SELECT entity_id, created_at FROM customer_entity")
customer_entityc$created_at <- strptime(customer_entityc$created_at, format = "%Y-%m-%d %H:%M:%S")
t_ws <- strptime("2015-05-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
customer_entity <- customer_entityc[customer_entityc$created_at >= t_ws,]
names(customer_entity) <- c("customer_id", "registration_time")


## Get the region the customer is from
customer_address <- dbGetQuery(con, "SELECT customer_id, region_id, postcode FROM sales_flat_order_address")
customer_address <- na.omit(customer_address)

customer_addressuni <- unique(customer_address)
customer_addresszip <- customer_addressuni[nchar(customer_addressuni$postcode) == 3 | nchar(customer_addressuni$postcode) == 6 | nchar(customer_addressuni$postcode) == 5,]
customer_addresszip$region_id <- as.character(customer_addresszip$region_id)
customer_addresszip1 <- customer_addresszip[-(nchar(customer_addresszip$region_id) == 3 & nchar(customer_addresszip$postcode) == 3),]
for (i in 1:nrow(customer_addresszip1)) {
  if (nchar(customer_addresszip1$postcode[i]) == 3) {
    if (nchar(customer_addresszip1$region_id[i]) == 6) {
      temp1 <- customer_addresszip1$postcode[i]
      customer_addresszip1$postcode[i] <- customer_addresszip1$region_id[i]
      customer_addresszip1$region_id[i] <- temp1
    } else {
      temp2 <- customer_addresszip1$postcode[i]
      customer_addresszip1$postcode[i] <- paste("0",customer_addresszip1$region_id[i],sep = "")
      customer_addresszip1$region_id[i] <- temp2
    }
  } else if (nchar(customer_addresszip1$postcode[i]) == 5) {
    customer_addresszip1$postcode[i] <- paste("0",customer_addresszip1$postcode[i],sep = "")
  }
}

customer_address2 <- customer_addresszip1[nchar(customer_addresszip1$postcode) == 6,]
customer_address <- customer_address2[,-1]
customer_address <- unique(customer_address)
customer_address$regioncode <- as.numeric(substr(customer_address$postcode,1,2))
customer_address <- na.omit(customer_address)
customer_address$regionname <- NA
for (i in 1:nrow(customer_address)) {
  if (customer_address$regioncode[i] <= 10 | customer_address$regioncode[i] == 30)
    customer_address$regionname[i] <- "Northern"
  else if (customer_address$regioncode[i] <= 16 & customer_address$regioncode[i] >= 11)
    customer_address$regionname[i] <- "Northeastern"
  else if (customer_address$regioncode[i] <= 27 & customer_address$regioncode[i] >= 20)
    customer_address$regionname[i] <- "Eastern"
  else if (customer_address$regioncode[i] <= 36 & customer_address$regioncode[i] >= 31)
    customer_address$regionname[i] <- "Eastern"
  else if (customer_address$regioncode[i] <= 47 & customer_address$regioncode[i] >= 41)
    customer_address$regionname[i] <- "Central"
  else if (customer_address$regioncode[i] <= 57 & customer_address$regioncode[i] >= 51)
    customer_address$regionname[i] <- "Southern"
  else if (customer_address$regioncode[i] <= 67 & customer_address$regioncode[i] >= 61)
    customer_address$regionname[i] <- "Southwestern"
  else if (customer_address$regioncode[i] == 40)
    customer_address$regionname[i] <- "Southwestern"
  else if (customer_address$regioncode[i] <= 75 & customer_address$regioncode[i] >= 71)
    customer_address$regionname[i] <- "Northwestern"
  else if (customer_address$regioncode[i] <= 86 & customer_address$regioncode[i] >= 81)
    customer_address$regionname[i] <- "Western"
  else if (customer_address$regioncode[i] == 99 & customer_address$regioncode[i] == 8)
    customer_address$regionname[i] <- "HMT"
}
customer_address <- na.omit(customer_address)
customer_address <- unique(customer_address[,c(1,5)])
customer_address <- customer_address[order(customer_address$customer_id),]

library(dplyr)
cus <- customer_address %>% distinct(customer_id, .keep_all = TRUE)
customer_address <- cus


## Other data
# Read files
memetotal$order_date <- Order_Detail$order_date
memetotal$order_date <- paste(memetotal$order_date, ":00", sep = "")
memetotal$order_date <- strptime(memetotal$order_date, format = "%m/%d/%Y %H:%M:%S")
meme_total <- memetotal[memetotal$order_date >= t_ws,]
meme_total <- meme_total[,-c(1,8,11)]
meme_total <- meme_total[order(meme_total$order_date),]
meme_total$value <- as.character(meme_total$value)
meme_total <- meme_total[!is.na(meme_total$customer_id),]
meme_total$order_date <- as.character(meme_total$order_date)

# Collapse the data for each order
library(data.table)
setDT(meme_total)
test <- meme_total[, .(inventory = paste(inventory, collapse = ","), sku = paste(sku, collapse = ","),
                       subtotal = c(subtotal)[1], shipping_amount = c(shipping_amount)[1],
                       value = paste(value, collapse = ","), customer_id = c(customer_id)[1],
                       discount_amount = c(discount_amount)[1], rewardpoints = c(rewardpoints)[1],
                       platform = c(platform)[1], os = c(os)[1], order_date = c(order_date)[1], 
                       rule_name = c(rule_name)[1]),
                   by = order_id]
setDF(test)
meme_totalf <- test[,c(7,12,1:6,8:11,13)]
meme_totalf <- meme_totalf[,-3]


# Get the first order of each customer
tot <- meme_totalf %>% distinct(customer_id, .keep_all = TRUE)

# Organize the data
tot$order_date <- strptime(tota$order_date, format = "%Y-%m-%d %H:%M:%S")
tot$value <- sapply(tot$value, function(x) paste(sort(unique(unlist(strsplit(x, split = ",")))), collapse = " "))
tot$inventory <- sapply(tota$inventory, function(x) paste(sort(unique(unlist(strsplit(x, split = ",")))), collapse = " "))

tot$skunum <- NA
tot$skunum <- sapply(tot$sku, function(x) length(sort(unique(unlist(strsplit(x, split = ","))))))

junk <- tot[,-c(4,7,13)]
junk$chn_shipping <- NA
junk$kor_shipping <- NA
junk$kor_shipping <- ifelse(grepl("34", junk$value), 1, 0)
junk$chn_shipping <- ifelse(grepl("36", junk$value) | grepl("52", junk$value), 1, 0)
meme_1purchase <- junk


## customer age
library(lubridate)
customer_idcard <- dbGetQuery(con, "SELECT customer_id, idcard FROM sales_flat_order_address 
                              WHERE idcard != ''")
customer_idcard <- na.omit(customer_idcard)
customer_idcard <- customer_idcard[nchar(customer_idcard$idcard) == 18,]
customer_idcard$birth <- substr(customer_idcard$idcard, 7, 14)
customer_idcarduni <- customer_idcard %>% distinct(customer_id, .keep_all = TRUE)
customer_idcarduni$birthdate <- as.Date(customer_idcarduni$birth, format = "%Y%m%d")
customer_idcarduni$age <- year(Sys.Date()) - year(customer_idcarduni$birthdate)
customer_idcarduni <- customer_idcarduni[customer_idcarduni$age >= 15 & customer_idcarduni$age <= 60,]



## customer referral
refer1 <- dbGetQuery(con, "SELECT new_customer_id FROM customer_referral WHERE (is_purchased = 1)")
names(refer1) <- "customer_id"

refer2 <- dbGetQuery(con, "SELECT customer_id FROM itoris_regfields_customer_options as op WHERE op.key = 'referal_ID'")
customer_referral <- unique(rbind(refer1,refer2))
customer_referral$referred <- 1

customer_referral <- customer_referral[order(customer_referral$customer_id),]








## merge data frames
late_time <- strptime("2016-08-01 00:00:00", format = "%Y-%m-%d %H:%M:%S")
customer_entity <- customer_entity[customer_entity$registration_time < late_time,]

# Calculate the time difference between registration and the first purchase and keep the reasonable data
customer_regi1purch <- merge(customer_entity, meme_1purchase, by = "customer_id")
customer_regi1purch$registration_time <- strptime(customer_regi1purch$registration_time, format = "%Y-%m-%d %H:%M:%S")
customer_regi1purch$order_date <- strptime(customer_regi1purch$order_date, format = "%Y-%m-%d %H:%M:%S")

customer_regi1purch$diffday <- day(customer_regi1purch$order_date) - day(customer_regi1purch$registration_time)
customer_regi1purch <- customer_regi1purch[customer_regi1purch$diffday >= 0,]

customer_regi1purch$diffhour <- difftime(customer_regi1purch$order_date, customer_regi1purch$registration_time, units = "hours")
customer_regi1purch$diffday <- difftime(customer_regi1purch$order_date, customer_regi1purch$registration_time, units = "days")


# Merge other data frames and organize
customer_addaddress <- merge(customer_address, customer_regi1purch, by = "customer_id")

customer_addreferral <- merge(customer_addaddress, customer_referral, all.x = TRUE, by = "customer_id")
customer_addreferral$referred <- ifelse(!is.na(customer_addreferral$referred), 1, 0)

customer_addage <- merge(customer_addreferral, customer_age, all.x = TRUE, by = "customer_id")
customer_addage <- customer_addage[!is.na(customer_addage$platform),]

names(customer_addage)[13] <- "Activity"
customer_addage$Activity <- ifelse(!is.na(customer_addage$Activity), "Yes", "No")

customer_addage$os <- tolower(customer_addage$os)
for (i in 1:nrow(customer_addage)) {
  if (grepl("android", customer_addage$os[i])) {
    customer_addage$os[i] <- "Android"
  } else if (customer_addage$os[i] == "ios") {
    customer_addage$os[i] <- "IOS"
  } else if (customer_addage$os[i] == "h5") {
    customer_addage$os[i] <- "h5"
  } else if (customer_addage$platform[i] == "pc") {
    customer_addage$os[i] <- "pc"
  } else {
    customer_addage$os[i] <- "unknown"
  }
}

meme_customer <- customer_addage

names(meme_customer)[c(2,4,12,13,17,18)] <- c("region", "first_order_date", "sku_num", "activity", "diff_day", "diff_hour")

meme_customer$activity <- ifelse(meme_customer$activity == "No", 0, 1)
meme_customer <- meme_customer[,-14]

meme_customer$registration_hour <- hour(meme_customer$registration_time)
meme_customer$registration_day_night <- ifelse(meme_customer$registration_hour >= 6 & meme_customer$registration_hour < 18, "Day", "Night")
meme_customer <- meme_customer[,-20]

for (i in 1:nrow(meme_customer)) {
  if (meme_customer$platform[i] == "h5-order")
    meme_customer$platform[i] <- "H5"
}

meme_customer$inventory <- gsub("NA ", "", meme_customer$inventory)

meme_customer$activity <- as.character(meme_customer$activity)
meme_customer$chn_shipping <- as.character(meme_customer$chn_shipping)
meme_customer$kor_shipping <- as.character(meme_customer$kor_shipping)
meme_customer$referred <- as.character(meme_customer$referred)
meme_customer$discount_amount <- abs(meme_customer$discount_amount)

meme_customer <- meme_customer[meme_customer$subtotal < 10000,]
meme_customer <- meme_customer[!is.na(meme_customer$inventory),]


meme_customer <- meme_customer[-(meme_customer$platform == "pc" & meme_customer$os != "pc"),]



## Add a column of the month and weekday of registration
library(lubridate)
meme_customer$registration_time <- strptime(meme_customer$registration_time, format = "%Y-%m-%d %H:%M:%S")
meme_customer$month <- months(meme_customer$registration_time, abbreviate = TRUE)
meme_customer$month <- factor(meme_customer$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
meme_customer$weekday <- weekdays(meme_customer$registration_time, abbreviate = TRUE)
meme_customer$weekday <- factor(meme_customer$weekday, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))

meme_customer$registration_time <- as.character(meme_customer$registration_time)