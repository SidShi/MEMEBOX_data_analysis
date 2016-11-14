library(RMySQL)
library(zoo)
library(dplyr)
library(MASS)
library(BTYD)
library(fAsianOptions)

# Build a connection to the MySQL Database 'con'



## Get registration time
# The purpose of obtaining this is to calculate the time period of observation/analysis
# The unit of analysis is month right now, later can use year
customer_registration <- dbGetQuery(con, "SELECT entity_id, created_at FROM customer_entity")
customer_registration$created_at <- substr(customer_registration$created_at,1,10)
names(customer_registration) <- c("customer_id", "registration_time")

# Calculate the interval (in month)
customer_registration$T <- (as.yearmon(Sys.Date())-as.yearmon(customer_registration$registration_time))*12
customer_registration$T <- sapply(customer_registration$T, function(x) ifelse(x == 0,1,x))




## Get purchase history
# We wish to get the order time and total amount of money spent in that order
# We need these data in order to get, for each customer: total number of months that he/she makes purchase; average money spent in those months; the most recent order (how many months from registration)
customer_purchase <- dbGetQuery(con, "SELECT customer_id, created_at, grand_total FROM sales_flat_order where status = 'complete'")
customer_purchase$created_at <- substr(customer_purchase$created_at,1,10)

# Clean unreasonable and useless data
customer_purchase <- customer_purchase[!is.na(customer_purchase$grand_total) & customer_purchase$grand_total > 0 & customer_purchase$grand_total < 50000,]
customer_purchase <- customer_purchase[!is.na(customer_purchase$customer_id),]
customer_purchase$created_at <- as.character(as.yearmon(customer_purchase$created_at))

# For all customers, get average grand_total in months that customers make purchase
customer_summary <- customer_purchase %>% group_by(customer_id, created_at) %>% summarise(mean_total = mean(grand_total))
names(customer_summary)[2] <- c("purchase_time")



## Merge the above two data frames
meme_summary <- merge(customer_registration, customer_summary, by = "customer_id", all.x = TRUE)
# Clean the data
meme_summary <- meme_summary[is.na(meme_summary$purchase_time) | as.yearmon(meme_summary$registration_time) <= as.yearmon(meme_summary$purchase_time),]
# Calculate time from registration to purchase
meme_summary$xt <- (as.yearmon(meme_summary$purchase_time)-as.yearmon(meme_summary$registration_time))*12+1
# Collapse the data: get most recent purchase time, total time interval, number of months that have purchase and average spend
individual_summary <- meme_summary %>% group_by(customer_id) %>% summarise(T = median(T), xt = max(xt), x = n(), mx = mean(mean_total))

# Clean data
individual_summary$xt <- sapply(individual_summary$xt, function(x) ifelse(is.na(x),0,x))
individual_summary$mx <- sapply(individual_summary$mx, function(x) ifelse(is.na(x),0,x))
for (i in 1:nrow(individual_summary)) {
  if (individual_summary$xt[i] == 0)
    individual_summary$x[i] <- 0
}




## Calculate parameters needed to calculate the CLV

# Extract data that can be used to calculate parameters of gamma distribution
# The result we have in this part is p,q and gam
# p is the shape parameter for transactions of customers
# q and gam are the shape and scale parameters of v's, where v's are the scale parameters of all transactions of each customer
ind_g <- na.omit(meme_summary)
ind_g <- aggregate(mean_total~customer_id, data = ind_g, c)
ind_g$len <- sapply(ind_g$mean_total, function(x) length(unlist(x)))
ind_g <- ind_g[ind_g$len > 1,]
ind_g$rp <- sapply(ind_g$mean_total, function(x) dim(table(unlist(x))))
ind_g <- ind_g[ind_g$rp > 1,]

for (i in 1:nrow(ind_g)) {
  ind_g$v[i] <- tryCatch({fitdistr(unlist(ind_g$mean_total[i]), "gamma")[[1]][[2]]}, error = function(a) {print("ah"); return(NA)}, finally = print("yeah"))
}

p <- fitdistr(individual_summary[individual_summary$mx != 0,]$mx, 'gamma')[[1]][[1]]
q <- fitdistr(ind_g[!is.na(ind_g$v) & (ind_g$v < 1),]$v, 'gamma')[[1]][[1]]
gam <- fitdistr(ind_g[!is.na(ind_g$v) & (ind_g$v < 1),]$v, 'gamma')[[1]][[2]]





# Get the data to calculate the four parameters: r, alpha, s and bet for the Pareto/NBD model
ind_pareto <- as.data.frame(individual_summary[,2:4])
ind_pareto <- data.matrix(ind_pareto)
colnames(ind_pareto) <- c("T.cal","t.x","x")

param <- pnbd.EstimateParameters(ind_pareto)
r <- param[1]
alpha <- param[2]
s <- param[3]
bet <- param[4]
# The other computer runs:
# r <- 5.680075
# alpha <- 37.738569
# s <- 1.746665
# bet <- 5.284165

# Below in comment is a method to calculate the four parameters if running the above command is too slow/gives errors
# The method takes samples from the whole data frame, calculate the four parameters for each one and take the average

# sumr <- 0
# sumalpha <- 0
# sums <- 0
# sumbeta <- 0
# temp <- c(1,1,1,1)
# parapre <- c(1,1,1,1)
# for (i in 1:100) {
#   ind <- ind_pareto[sample(1:nrow(ind_pareto),1000),]
#   para <- tryCatch({pnbd.EstimateParameters(ind, par.start = temp)}, error = function(a) {print("ah"); return(NA)}, finally = {print("yeah")})
#   if (length(para) == 1) {
#     temp <- parapre
#   } else {
#     temp <- para
#     parapre <- para
#   }
#   sumr <- sum(sumr,para[1],na.rm = TRUE)
#   sumalpha <- sum(sumalpha,para[2],na.rm = TRUE)
#   sums <- sum(sums,para[3],na.rm = TRUE)
#   sumbeta <- sum(sumbeta,para[4],na.rm = TRUE)
# }
# r <- sumr/90
# alpha <- sumalpha/90
# s <- sums/90
# beta <- sumbeta/90





## Calculate Customer Lifetime Value
# This is the pareto likelihood function and use it to calculate the pareto likelihood value for each customer
pareto_likelihood <- function(x,tx,T) {
  Lpart <- 0
  for (i in 0:(T-tx-1)) {
    Lpart <- Lpart + beta(r+x,alpha+tx-x+i)*beta(s+1,bet+tx+i)/beta(r,alpha)/beta(s,bet)
  }
  L <- beta(r+x,alpha+T-x)*beta(s,bet+T)/beta(r,alpha)/beta(s,bet)+Lpart
  return(L)
}

for (j in 1:nrow(individual_summary)) {
  individual_summary$pareto_like[j] <- pareto_likelihood(individual_summary$x[j],individual_summary$xt[j],individual_summary$T[j])
}


# Method 1:
# This method however, has one defect in the formula and the calculated values are not reasonable and cannot be used
# There are no corrections and updates about this method online so far
# delta is the rate of interest
# delta <- 0.05

# individual_summary$clv <- alpha^r*bet^s*delta^(s-1)*gamma(r+individual_summary$x+1)*Re(kummerU(delta*(bet+individual_summary$T),s,s))/
#   gamma(r)/(alpha+individual_summary$T)^(r-log(individual_summary$x+1)+1)/individual_summary$pareto_like*(gam+individual_summary$mx*individual_summary$x)*p/(p*individual_summary$x+q-1)



# Method 2:
# The two predictions below are for the next twelve months
nstar <- 12
# Customer Lifetime Value
individual_summary$clv <- beta(r+individual_summary$x+1,alpha+individual_summary$T-individual_summary$x)*
  (beta(s-1,bet+individual_summary$T+1)-beta(s-1,bet+individual_summary$T+nstar+1))/beta(r,alpha)/
  beta(s,bet)/individual_summary$pareto_like*(gam+individual_summary$mx*individual_summary$x)*p/
  (p*individual_summary$x+q-1)

# Expected number of purchase
individual_summary$expected_purchase <- beta(r+individual_summary$x+1,alpha+individual_summary$T-individual_summary$x)*
  (beta(s-1,bet+individual_summary$T+1)-beta(s-1,bet+individual_summary$T+nstar+1))/beta(r,alpha)/
  beta(s,bet)/individual_summary$pareto_like

# The result data frame has three columns: customer_id, CLV and expected number of purchase
# The data frame is sorted by CLV in a decreasing order
individual_clv <- individual_summary[,c(1,7,8)]
individual_clv <- individual_clv[order(individual_clv$clv, decreasing = TRUE),]
names(individual_clv)[2] <- "customer_lifetime_value"








## Merge the CLV and loyalty program
cus <- customer_purchase %>% group_by(customer_id) %>% summarise(purchase = n(), mean_total = mean(grand_total))

a <- merge(customer_registration, individual_clv, by = "customer_id")
b <- merge(a, individual_summary[,c(1,3)], by = "customer_id")
c <- merge(b, cus, all.x = TRUE, by = "customer_id")
clv_mod <- c[,c(1:3,6:8,4:5)]
clv_mod$purchase <- sapply(clv_mod$purchase, function(x) ifelse(is.na(x),0,x))
clv_mod$mean_total <- sapply(clv_mod$mean_total, function(x) ifelse(is.na(x),0,x))

# all customers
clv_loyalty <- merge(clv_mod, meme_loyalty, all.x = TRUE, by = "customer_id")
# only those who have already activated loyalty program
clv_withloyalty <- clv_loyalty[!is.na(clv_loyalty$grade),]





## Clean some unreasonable account
# Get all sku numbers in each order
order_sku <- dbGetQuery(con, "SELECT order_id, sku FROM sales_flat_order_item")
ordersku_agg <- aggregate(sku~order_id, data = order_sku, function(x) paste(x, collapse = ","))

# Link orders with customers so that we know the sku each customer purchases
customer_order <- dbGetQuery(con, "SELECT entity_id as order_id, customer_id FROM sales_flat_order WHERE status = 'complete'")
cus_sku <- merge(customer_order,ordersku_agg, by = "order_id")
customer_sku <- aggregate(sku~customer_id, data = cus_sku, function(x) paste(x, collapse = ","))
clv_sku <- merge(customer_sku, clv_loyalty, all.y = TRUE, by = "customer_id")
# Clean customers who buy certain sku
clv_sku <- clv_sku[!grepl("test",clv_sku$sku) & !grepl("mkt",clv_sku$sku) & !grepl("b2b",clv_sku$sku),]
clv_sku <- clv_sku[,c(1,3:10,2)]
clv_sku <- clv_sku[order(clv_sku$customer_lifetime_value, decreasing = TRUE),]
row.names(clv_sku) <- 1:nrow(clv_sku)
