alng <- function(x) {
  # Logarithm gamma function, x > 0
  b <- c(8.33333333333333e-2,2.33333333333333e-2,2.52380952380952e-1,5.25606469002695e-1,
         1.01152306812684,1.51747364915329,2.26948897420496,3.00991738325940)
  
  if (x < 8) {
    xx <- x + 8
    indx <- TRUE
  } else {
    indx <- FALSE
    xx <- x
  }
  
  fterm <- (xx-0.5)*log(xx)-xx+0.91893853320467
  sum <- b[1]/(xx+b[2]/(xx+b[3]/(xx+b[4]/(xx+b[5]/(xx+b[6]/(xx+b[7]/(xx+b[8])))))))
  res <- sum + fterm
  
  if (indx) {
    res <- res-log(x+7)-log(x+6)-log(x+5)-log(x+4)-log(x+3)-log(x+2)-log(x+1)-log(x)
  }
  return(res)
}

poipr <- function(k, el) {
  # This function computes the P(X = k), where X is a Poisson random
  # variable with mean defective rate = el, number of defective items = k
  res <- exp(-el+k*log(el)-alng(k+1))
  return(res)
}

sumi2 <- function(iside, n1, n2, elhat2, t_k1k2, i1, pi1, i2mode, pi2mode, d, pvalue) {
  # Here we carry out the sum over i2 to compute the p-value of the E-test
  pi2 <- pi2mode
  for (i2 in i2mode:1000) {
    if (i2 > 1000)
      break
    if (pi2 < 1e-7)
      break
    elhati1 <- i1/n1
    elhati2 <- i2/n2
    diffi <- elhati1 - elhati2 - d
    va <- elhati1/n1 + elhati2/n2
    if (iside == "right-sided") {
      if ((i1/n1-i2/n2) <= d) {
        t_i1i2 <- 0
      } else {
        t_i1i2 <- diffi/sqrt(va)
      }
      if (t_i1i2 >= t_k1k2)
        pvalue <- pvalue + pi1*pi2
    } else if (iside == "both-sided") {
      if ((abs(i1/n1-i2/n2)) <= d) {
        t_i1i2 <- 0
      } else {
        t_i1i2 <- diffi/sqrt(va)
      }
      if (abs(t_i1i2) >= abs(t_k1k2))
        pvalue <- pvalue + pi1*pi2
    }
    pi2 <- elhat2*pi2/(i2+1)
  }
  
  pi2 <- pi2mode
  pi2 <- i2mode*pi2/elhat2
  for (i2 in (i2mode-1):0) {
    if (i2 < 0)
      break
    if (pi2 < 1e-7)
      break
    elhati1 <- i1/n1
    elhati2 <- i2/n2
    diffi <- elhati1-elhati2-d
    va <- elhati1/n1+elhati2/n2
    if (iside == "right-sided") {
      if ((i1/n1-i2/n2) <= d) {
        t_i1i2 <- 0
      } else {
        t_i1i2 <- diffi/sqrt(va)
      }
      if (t_i1i2 >= t_k1k2)
        pvalue <- pvalue + pi1*pi2
    } else if (iside == "both-sided") {
      if ((abs(i1/n1-i2/n2)) <= d) {
        t_i1i2 <- 0
      } else {
        t_i1i2 <- diffi/sqrt(va)
      }
      if (abs(t_i1i2) >= abs(t_k1k2))
        pvalue <- pvalue + pi1*pi2
    }
    pi2 <- i2*pi2/elhat2
  }
  
  return(pvalue)
}

poistest <- function(iside, n1, n2, elhatk, t_k1k2, d) {
  # Function for computing the p-value of the unconditional test
  # computing estimates of el1*n1 and el2*n2 under H_0
  pvalue <- 0
  elhat1 <- n1*(elhatk+d)
  elhat2 <- n2*elhatk
  # computing the modes
  i1mode <- floor(elhat1)
  i2mode <- floor(elhat2)
  # initializing the probability at the i1mode
  pi1mode <- poipr(i1mode,elhat1)
  pi1 <- pi1mode
  # initializing the probability at the i2mode
  pi2mode <- poipr(i2mode,elhat2)
  
  for (i1 in i1mode:1000) {
    if (i1 > 1000)
      break
    if (pi1 < 1e-7)
      break
    pvalue <- sumi2(iside, n1, n2, elhat2, t_k1k2, i1, pi1, i2mode, pi2mode, d, pvalue)
    pi1 <- elhat1*pi1/(i1+1)
  }
  
  pi1 <- pi1mode
  pi1 <- i1mode*pi1/elhat1
  for (i1 in (i1mode-1):0) {
    if (i1 < 0)
      break
    if (pi1 < 1e-7)
      break
    pvalue <- sumi2(iside, n1, n2, elhat2, t_k1k2, i1, pi1, i2mode, pi2mode, d, pvalue)
    pi1 <- i1*pi1/elhat1
  }
  return(pvalue)
}

e_test <- function(iside = "both-sided", k1, k2, n1, n2, d = 0) {
  # Compute the p-value of the unconditional test for testing one and two-sided hypotheses about the means
  # of two Poisson distributions
  # INPUT:
  # iside: side of the test, default to two-sided
  # ki: count of the ith population, i = 1,2
  # ni: sample size from the ith population, i = 1,2
  # d: the difference mean1 - mean2 under H_0, default to 0
  # OUTPUT:
  # p-value: p-value of the unconditional test
  elhatk <- (k1+k2)/(n1+n2)-d*n1/(n1+n2)
  va <- k1/n1^2+k2/n2^2
  t_k1k2 <- (k1/n1-k2/n2-d)/sqrt(va)
  pvalue <- poistest(iside, n1, n2, elhatk, t_k1k2, d)
  return(pvalue)
}

read_input <- function() {
  n <- readline(prompt = "Select the type of A/B testing: 1. User Revenue related; 2. User Click related; 3. User Transactions related; 4. Products Purchased related; 5. Others but analysis on users")
  return(as.integer(n))
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  #
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  #
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

library(tidyr)
library(dplyr)
library(ggplot2)



# The data I need should be like this: the first column denotes the customer/product ID,
# the second denotes the group the customer (product) is in, the third denotes the time,
# the fourth denotes Revenue/Transactions/Number of Products Purchased
# If the analysis is on clicks, the fourth denotes total clicks and the fifth denotes total views of the page
# If the data is about users, then each row corresponds to one UserID
# If the data is about products, then each row corresponds to one type of product (or ProductID)
names(test1) <- c("ID","group","time")

test1$ymd <- substr(test1$time,1,10)



user_input <- read_input()
while (user_input != 1 & user_input != 2 & user_input != 3 & user_input != 4 & user_input != 5) {
  print("The choice has to be 1 or 2 or 3 or 4 or 5. Select Again")
  user_input <- read_input()
}

# Here we consider the 95% confidence interval
if (user_input == 1) {
  # If the analysis is on other statistics of revenue, change all below "mean" to the specific stats
  # For example, change all "mean" to "sum"
  names(test1)[4] <- "value"
  test <- test1[,c("ID","group","value","ymd")]
  revenue_day <- test %>% group_by(group,ymd) %>% summarise(mean = mean(value))
  
  ggplot(data = revenue_day, aes(x = ymd, y = mean, color = group, group = group)) + geom_point() + geom_line()
  + labs(title = "Average Revenue per User of two Groups in A/B Testing per Day", x = "Date", y = "Average Revenue")
  
  # Analysis
  pvalue <- t.test(value ~ group, data = test, alternative = "less")$p.value
  # If p-value is smaller than 0.05 or the confidence interval does not contain 0
  # we reject the null hypothesis that the two groups do not have any differences and conclude that
  # one group is statistically significantly better than the other one (can read directly)
} else if (user_input == 2) {
  # The following analysis is done on Click Through Rate, other Click Analysis needs to change the code accordingly
  # For example, if on total clicks, then doesn't need to bother with total views of page
  names(test1)[4] <- "clicks"
  names(test1)[5] <- "views"
  test1$value <- test1$clicks/test1$views
  test <- test1[,c("ID","group","value","ymd","clicks","views")]
  clicks_day <- test %>% group_by(group,ymd) %>% summarise(total_clicks = sum(clicks), total_views = sum(views))
  %>% mutate(ctr = total_clicks/total_views)
  
  ggplot(data = clicks_day, aes(x = ymd, y = ctr, color = group, group = group)) + geom_point() + geom_line()
  + labs(title = "Click Through Rate per User of two Groups in A/B Testing per Day", x = "Date", y = "CTR")
  
  # Analysis
  clicks_day$certainperc <- ifelse(clicks_day$ctr > 0.2, "Larger than 20 percent", "Smaller than 20 percent")
  pvalue <- fisher.test(table(test$certainperc,test$group), alternative = "less")$p.value
  # Check p-value
} else if (user_input == 3) {
  # If the analysis is on other statistics of transactions, change all below "mean" to the specific stats
  # For example, change all "mean" to "sum"
  names(test1)[4] <- "value"
  test <- test1[,c("ID","group","value","ymd")]
  transactions_day <- test %>% group_by(group,ymd) %>% summarise(mean = mean(value))
  
  ggplot(data = transactions_day, aes(x = ymd, y = mean, color = group, group = group)) + geom_point() + geom_line()
  + labs(title = "Average Transactions per User of two Groups in A/B Testing per Day", x = "Date", y = "Average Transactions")
  
  # Analysis
  summ <- test %>% group_by(group) %>% summarise(n = n(), sum = sum(value))
  k1 <- summ$sum[1]
  k2 <- summ$sum[2]
  n1 <- summ$n[1]
  n2 <- summ$n[2]
  pvalue <- 1 - e_test(iside = "right-sided",k1=k1,k2=k2,n1=n1,n2=n2)
  # Check p-value
} else if (user_input == 4) {
  names(test1)[4] <- "value"
  test <- test1[,c("ID","group","value","ymd")]
  # Here the plot is about the total number of products sold per day
  # If we want to demonstrate the multinomial distribution we are working on, plotting for every product, then we would have too many graphs
  # We could do that but it is not efficient
  purchased_day <- test %>% group_by(group,ymd) %>% summarise(sum = sum(value))
  
  ggplot(data = purchased_day, aes(x = ymd, y = sum, color = group, group = group)) + geom_point() + geom_line()
  + labs(title = "Total Products Purchased of two Groups in A/B Testing per Day", x = "Date", y = "Total Number of Purchase")
  
  # We can show the curve of the top five best-sellers instead
  purchased_id <- test %>% group_by(ID) %>% summarise(sum = sum(value)) %>% arrange(desc(sum))
  top_id <- purchased_id$ID[1:5]
  pl <- list()
  for (i in top_id) {
    item_spec <- test[test$ID == i,]
    item_plot <- item_spec %>% group_by(group,ymd) %>% summarise(sum = sum(value))
    plo <- ggplot(data = item_plot, aes(x = ymd, y = sum, color = group, group = group)) + geom_point() + geom_line()
    + labs(title = "Total Purchase of A Product of two Groups in A/B Testing per Day", x = "Date", y = "Total Number of Purchase")
    pl <- list(pl,plo)
  }
  multiplot(pl[[1]],pl[[2]],pl[[3]],pl[[4]],pl[[5]])
  
  # Analysis
  t <- test %>% group_by(ID, group) %>% summarise(num = sum(value))
  product_df <- spread(t,group,num)
  pvalue <- 1 - chisq.test(product_df[,2:3])$p.value
  # Check p-value
} else {
  pvalue <- wilcox.test(value~group, data = test, alternative = "less")$p.value
  # Check p-value
}

if (pvalue < 0.05) {
  print("The p-value is statistically significant. We reject the null hypothesis that the Control Group is
        not worse than the Experiment Group. Therefore making this change does make essential improvement.")
} else {
  print("The p-value is not statistically significant. We fail to reject the null hypothesis that the Control
        Group is not worse than the Experiment Group. Therefore it does not make sense to make this change.")
}
