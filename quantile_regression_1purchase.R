## Read data and a little bit organization
meme_customer$month <- factor(meme_customer$month, levels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
meme_customer$weekday <- factor(meme_customer$weekday, levels = c("Sun","Mon","Tue","Wed","Thu","Fri","Sat"))
attach(meme_customer)
detach(meme_customer)
library(quantreg)

# Explanation of AIC:
# It is a parameter of the model that both considers the ability to explain the variability and penals of using variables
# Normally it is used to deal with problem of overfitting; generally, the smaller the AIC, the better the model



## The function is to calculate residual sum of squares (RSS)
rho <- function(u,tau=.5)u*(tau - (u < 0))

# If no x variable, the residual sum of squares of these models are the total sum of squares of models with x variables
r_blank <- rq(diff_day~1, tau = 0.45)
r_blanklog <- rq(log(diff_day)~1, tau = 0.45)




### Single variable regression

## region
r_region <- rq(diff_day~region, data = meme_customer, tau = 0.45)
summary(r_region)
r_re <- 1-r_region$rho/r_blank$rho


## platform
r_platform <- rq(diff_day~platform, data = meme_customer, tau = 0.45)
summary(r_platform)
r_pl <- 1-r_platform$rho/r_blank$rho


## os
meme_mobile <- meme_customer[meme_customer$os != "pc",]
r_os0 <- rq(diff_day~1, data = meme_mobile, tau = 0.45)
r_os <- rq(diff_day~os, data = meme_mobile, tau = 0.45)
summary(r_os)
r_osrho <- 1-r_os$rho/r_os0$rho


## inventory
r_inventory <- rq(diff_day~inventory, tau = 0.45)
summary(r_inventory)
r_in <- 1-r_inventory$rho/r_blank$rho


## activity
r_activity <- rq(diff_day~activity, tau = 0.45)
summary(r_activity)
r_ac <- 1-r_activity$rho/r_blank$rho


## shipping from China
r_chn <- rq(diff_day~chn_shipping, tau = 0.45)
summary(r_chn)
r_ch <- 1-r_chn$rho/r_blank$rho


## shipping from Korea
r_kor <- rq(diff_day~kor_shipping, tau = 0.45)
summary(r_kor)
r_ko <- 1-r_kor$rho/r_blank$rho


## referrence
r_referred <- rq(diff_day~referred, tau = 0.45)
summary(r_referred)
r_ref <- 1-r_referred$rho/r_blank$rho


## registration period
r_registration <- rq(diff_day~registration_day_night, tau = 0.45)
summary(r_registration)
r_reg <- 1-r_registration$rho/r_blank$rho


## registration month
r_month <- rq(diff_day~month, tau = 0.45)
summary(r_month)
r_mo <- 1-r_month$rho/r_blank$rho


## registration weekday
r_weekday <- rq(diff_day~weekday, tau = 0.45)
summary(r_weekday)
r_we <- 1-r_weekday$rho/r_blank$rho


## subtotal
junk <- meme_customer[subtotal != 0,]
# linear (attempt to use or not to use log)
try <- rq(log(diff_day)~log(subtotal), tau = 0.45, data = junk)
try0 <- rq(log(diff_day)~1, tau = 0.45, data = junk)
tr <- rq(diff_day~1, tau = 0.45, data = junk)
1-try$rho/try0$rho
r_subtotal <- rq(diff_day~subtotal, tau = 0.45)
summary(r_subtotal)
r_su <- 1 - r_subtotal$rho/r_blank$rho
# second order
junk$logsub2 <- (log(junk$subtotal))^2
r_logsub2 <- rq(log(diff_day)~logsub2+log(subtotal), data = junk, tau = 0.45)
summary(r_logsub2)
1-r_logsub2$rho/try0$rho
# 0.34%
# third order
junk$logsub3 <- (log(junk$subtotal))^3
r_logsub3 <- rq(log(diff_day)~logsub3+logsub2+log(subtotal), data = junk, tau = 0.45)
summary(r_logsub3)
1-r_logsub3$rho/try0$rho
# tiny benefits
# inverse
junk$sub1 <- 1/(junk$subtotal)
r_sub1 <- rq(diff_day~sub1, data = junk, tau = 0.45)
summary(r_sub1)
1-r_sub1$rho/tr$rho
# totally not good



## shipping amount
# linear
r_shipping <- rq(diff_day~shipping_amount, tau = 0.45)
summary(r_shipping)
r_sh <- 1-r_shipping$rho/r_blank$rho
# pseudo R-squared: 7.58e-3%
# second order
meme_customer$ship2 <- shipping_amount^2
r_ship2 <- rq(diff_day~ship2+shipping_amount, data = meme_customer, tau = 0.45)
1-r_ship2$rho/r_blank$rho
# tiny benefits


## discount amount
# linear
r_discount <- rq(diff_day~discount_amount, tau = 0.45)
summary(r_discount)
r_di <- 1-r_discount$rho/r_blank$rho
# second order
meme_customer$dis2 <- discount_amount^2
r_dis2 <- rq(diff_day~dis2+discount_amount, data = meme_customer, tau = 0.45)
summary(r_dis2)
1-r_dis2$rho/r_blank$rho
# Not good


## rewardpoints
# linear
r_reward <- rq(diff_day~rewardpoints, tau = 0.45)
summary(r_reward)
r_rew <- 1-r_reward$rho/r_blank$rho
AIC.rq(r_reward)
# second order
meme_customer$rew2 <- rewardpoints^2
r_rew2 <- rq(diff_day~rew2+rewardpoints, tau = 0.45, data = meme_customer)
summary(r_rew2)
1-r_rew2$rho/r_blank$rho
AIC.rq(r_rew2)
# much better but still 0.51%
# third order
meme_customer$rew3 <- rewardpoints^3
r_rew3 <- rq(diff_day~rew3+rew2+rewardpoints, tau = 0.45, data = meme_customer)
summary(r_rew3)
1-r_rew3$rho/r_blank$rho
AIC.rq(r_rew3)
# 0.78%
# fourth order
meme_customer$rew4 <- rewardpoints^4
r_rew4 <- rq(diff_day~rew4+rew3+rew2+rewardpoints, tau = 0.45, data = meme_customer)
summary(r_rew4)
1-r_rew4$rho/r_blank$rho
AIC.rq(r_rew4)
# cube is best


## sku number
# linear
r_sku <- rq(log(diff_day)~log(sku_num), tau = 0.45)
summary(r_sku)
1-r_sku$rho/r_blanklog$rho
AIC.rq(r_sku)
# second order
meme_customer$logsku2 <- (log(sku_num))^2
r_sku2 <- rq(log(diff_day)~logsku2+log(sku_num), tau = 0.45, data = meme_customer)
summary(r_sku2)
1-r_sku2$rho/r_blanklog$rho
AIC.rq(r_sku2)
# 1.07%
# third order
meme_customer$logsku3 <- (log(sku_num))^3
r_sku3 <- rq(log(diff_day)~logsku3+log(sku_num), tau = 0.45, data = meme_customer)
summary(r_sku3)
1-r_sku3$rho/r_blanklog$rho
AIC.rq(r_sku3)
# worse than using square


## age
meme_age <- meme_customer[!is.na(age),]
r_age0 <- rq(log(diff_day)~1, tau = 0.45, data = meme_age)
r_age <- rq(log(diff_day)~log(age), tau = 0.45, data = meme_age)
summary(r_age)
r_ag <- 1-r_age$rho/r_age0$rho




### Multiple variables regression
## Use all variables
r_all <- rq(diff_day~sku_num+month+rewardpoints+platform+weekday+subtotal+inventory+chn_shipping+activity+region+kor_shipping+shipping_amount+referred+registration_day_night+discount_amount, tau = 0.45)
summary(r_all)
1-r_all$rho/r_blank$rho
AIC.rq(r_all)

## Modification of r_all
r_mod1 <- rq(diff_day~sku_num+month+rewardpoints+platform+weekday+subtotal+inventory+chn_shipping+activity+kor_shipping+shipping_amount+discount_amount, tau = 0.45)
summary(r_mod1)
1-r_mod1$rho/r_blank$rho
AIC.rq(r_mod1)

## Modification of r_mod1
r_mod2 <- rq(diff_day~sku_num+month+rewardpoints+weekday+subtotal+inventory+chn_shipping+activity+kor_shipping+shipping_amount+discount_amount, tau = 0.45)
summary(r_mod2)
1-r_mod2$rho/r_blank$rho
AIC.rq(r_mod2)

## Also modification of r_mod1 but take log
r_mod2 <- rq(log(diff_day)~log(sku_num)+month+rewardpoints+platform+weekday+subtotal+inventory+chn_shipping+activity+kor_shipping+shipping_amount+discount_amount, tau = 0.45)
summary(r_mod2)
1-r_mod2$rho/r_blanklog$rho
AIC.rq(r_mod2)

## Modification of r_mod2
r_mod3 <- rq(diff_day~log(sku_num)+month+rewardpoints+platform+weekday+log(subtotal)+inventory+chn_shipping+activity+kor_shipping+shipping_amount+discount_amount,tau = 0.45, data = junk)
summary(r_mod3)
try00 <- rq(diff_day~1, tau = 0.45, data = junk)
1-r_mod3$rho/try00$rho
AIC.rq(r_mod3)





## Create higher-order terms by multiplying variables together
# discount * shipping
meme_customer$ship_dis <- discount_amount*shipping_amount
r_ship_dis <- rq(diff_day~ship_dis+shipping_amount+discount_amount, data = meme_customer, tau = 0.45)
summary(r_ship_dis)
1-r_ship_dis$rho/r_blank$rho
# 4.93e-3% still bad

# shipping * rewardpoints
meme_customer$ship_rew <- shipping_amount*rewardpoints
r_ship_rew <- rq(diff_day~ship_rew+shipping_amount+rewardpoints, data = meme_customer, tau = 0.45)
summary(r_ship_rew)
1-r_ship_rew$rho/r_blank$rho
# 0.02% still bad

# discount * rewardpoints
meme_customer$dis_rew <- discount_amount*rewardpoints
r_dis_rew <- rq(diff_day~dis_rew+discount_amount+rewardpoints, data = meme_customer, tau = 0.45)
summary(r_dis_rew)
1-r_dis_rew$rho/r_blank$rho
# p value for dis_rew not statistically significant

# discount * discount * rewardpoints
meme_customer$ship_dis_rew <- shipping_amount*discount_amount*rewardpoints
r_ship_dis_rew <- rq(diff_day~ship_dis_rew+ship_rew+ship_dis+dis_rew+shipping_amount+discount_amount+rewardpoints, data = meme_customer, tau = 0.45)
summary(r_ship_dis_rew)
1-r_ship_dis_rew$rho/r_blank$rho
# ship_rew and dis_rew not significant
r_ship_dis_rew2 <- rq(diff_day~ship_dis_rew+ship_dis+shipping_amount+discount_amount+rewardpoints, data = meme_customer, tau = 0.45)
summary(r_ship_dis_rew2)
1-r_ship_dis_rew2$rho/r_blank$rho
# 0.03%

# log(subtotal) * log(sku_num)
junk$log_sub_sku <- log(junk$subtotal)*log(junk$sku_num)
r_sub_sku <- rq(log(diff_day)~log_sub_sku+log(sku_num)+log(subtotal), data = junk, tau = 0.45)
summary(r_sub_sku)
1-r_sub_sku$rho/try0$rho
# log(sku_num) not significant
r_sub_sku2 <- rq(log(diff_day)~log_sub_sku+log(subtotal), data = junk, tau = 0.45)
summary(r_sub_sku2)
1-r_sub_sku2$rho/try0$rho
# worse than using log(sku_num) squared

# log(subtotal) * (log(sku_num))^2
junk$logsub_logsku2 <- log(junk$subtotal)*(log(junk$sku_num))^2
r_logsub_logsku2 <- rq(log(diff_day)~logsub_logsku2, data = junk, tau = 0.45)
summary(r_logsub_logsku2)
1-r_logsub_logsku2$rho/try0$rho
# worse than using log(sku_num) squared

# All numbers multiplied together
junk$all_num <- junk$shipping_amount*junk$discount_amount*junk$rewardpoints*log(junk$subtotal)*log(junk$sku_num)
r_all_num <- rq(log(diff_day)~all_num, data = junk, tau = 0.45)
summary(r_all_num)
1-r_all_num$rho/try0$rho
# 0.04% bad

# All variables with higher-order terms
junk$logsku2 <- (log(junk$sku_num))^2
junk$rew3 <- (junk$rewardpoints)^3
junk$rew2 <- (junk$rewardpoints)^2
junk$dis_ship <- junk$discount_amount*junk$shipping_amount
r_all2 <- rq(log(diff_day)~logsku2+month+rew3+platform+weekday+logsub2+inventory+chn_shipping+activity+region+kor_shipping+shipping_amount+referred+registration_day_night+discount_amount+dis_ship, tau = 0.45, data = junk)
summary(r_all2)

# Modification of r_all2
r_all3 <- rq(log(diff_day)~logsku2+month+rew2+platform+weekday+logsub2+inventory+chn_shipping+activity+shipping_amount+referred+discount_amount+dis_ship, tau = 0.45, data = junk)
summary(r_all3)
1-r_all3$rho/try0$rho
AIC.rq(r_all3)
# 5.43%

# Modification of r_all3
r_all4 <- rq(log(diff_day)~logsku2+month+rew2+weekday+logsub2+inventory+chn_shipping+activity+shipping_amount+discount_amount+dis_ship, tau = 0.45, data = junk)
summary(r_all4)
1-r_all4$rho/try0$rho
AIC.rq(r_all4)
# above is so far the best