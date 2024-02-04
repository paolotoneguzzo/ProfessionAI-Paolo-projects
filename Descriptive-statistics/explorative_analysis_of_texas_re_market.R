re_texas = read.csv("realestate_texas.csv", sep = ",")
install.packages("moments")
install.packages("RColorBrewer")

library(ggplot2)
library(moments)
library(dplyr)
library(RColorBrewer)
attach(re_texas)


#3) CALCULATES POSITION & VARIABILITY INDEXES


coeff.vaiation = function(x){
  return((sd(x)/mean(x))*100)
}


#1 city

table(city) 
ni1 = table(city)
fi1 = table(city)/dim(re_texas)[1]
Ni1 = cumsum(ni1)
Fi1 = cumsum(fi1)
freq_RET_city = cbind(ni1, fi1,Ni1,Fi1)


#2 year

table(year)
ni2 = table(year)
fi2 = ni2/dim(re_texas)[1]
Ni2 = cumsum(ni2)
Fi2 = cumsum(fi2)
freq_RET_year=cbind(ni2,fi2,Ni2,Fi2)


#3 month

table(month)
ni3 = table(month)
fi3 = ni3/dim(re_texas)[1]
Ni3 = cumsum(ni3)
Fi3 = cumsum(fi3)
freq_RET_month=cbind(ni3,fi3,Ni3,Fi3)


#4 sales

mu4 = mean(sales)
med4 = median(sales)

r4 = range(sales)
iqr4 = IQR(sales)
v4 = var(sales)
sd4 = sd(sales)
cv4 = coeff.vaiation(sales)

plot(density(sales),xlab = " total number of sales" ,ylab = "frequency", main = "Sales distribution")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(sales), col = "red")
abline(v = median(sales))

mu3_4 = skewness(sales)
mu4_4 = kurtosis(month)-3


#5 volume

mu5 = mean(volume)
med5 = median(volume)

r5 = range(volume)
iqr5 = IQR(volume)
v5 = var(volume)
sd5 = sd(volume)
cv5 = coeff.vaiation(volume)

plot(density(volume),xlab = "value of total sales (million $)" ,ylab = "frequency", main = "volume distribution")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(volume), col = "red")
abline(v = median(volume))

mu3_5 = skewness(volume)
mu4_5 = kurtosis(volume)-3


#6 median_price

mu6 = mean(median_price)
med6 = median(median_price)

r6 = range(median_price)
iqr6 = IQR(median_price)
v6 = var(median_price)
sd6 = sd(median_price)
cv6 = coeff.vaiation(median_price)

plot(density(median_price),xlab = "median price ($)" ,ylab = "frequency", main = "median price distribution")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(median_price), col = "red")
abline(v = median(median_price))

mu3_6 = skewness(median_price)
mu4_6 = kurtosis(median_price)-3


#7 listings

mu7 = mean(listings)
med7 = median(listings)

r7 = range(listings)
iqr7 = IQR(listings)
v7 = var(listings)
sd7 = sd(listings)
cv7 = coeff.vaiation(listings)

plot(density(listings),xlab = "total number of active listings" ,ylab = "frequency", main = "listings distribution")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(listings), col = "red")
abline(v = median(listings))

mu3_7 = skewness(listings)
mu4_7 = kurtosis(listings)-3


#8 months inventory

mu8 = mean(months_inventory)
med8 = median(months_inventory)

r8 = range(months_inventory)
iqr8 = IQR(months_inventory)
v8 = var(months_inventory)
sd8 = sd(months_inventory)
cv8 = coeff.vaiation(months_inventory)

plot(density(months_inventory),xlab = "time to sell the active listings at the current rate (month)" ,ylab = "frequency", main = "months inventory distribution")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(months_inventory), col = "red")
abline(v = median(months_inventory))

mu3_8 = skewness(months_inventory)
mu4_8 = kurtosis(months_inventory)-3


#4) MOST VARYING AND ASSYMETRIC VARIABLES

CVs = cbind(cv4,cv5,cv6,cv7,cv8)
CVs
MU3s = cbind(mu3_4,mu3_5,mu3_6,mu3_7,mu3_8)
MU3s


#5) FREQUENCY DISTRIBUTION, BAR GRAPH AND GINI'S INDEX
# OF ONE VARIABLE OF CHOICE

# building the frequency table

mu5
med5 
r5
iqr5

re_texas$vol_cl = cut(volume,breaks = c(8,16,24,32,40,48,56,64,72,80,88))
attach(re_texas)

ni = table(vol_cl)
fi =ni/dim(re_texas)[1]
Ni = cumsum(ni)
Fi = cumsum(fi)
freq_table_volume_cl = cbind(ni,fi,Ni,Fi)

ggplot(re_texas) + geom_bar(aes(vol_cl),stat = "count", fill = "lightblue", col = "black") + labs(x = "value of total sales (million $)", y = "frequency", title = "Volume of sales in Texas (2010-2014)") +
  scale_y_continuous(breaks = seq(0,50,10),labels = seq(0,0.5,0.1)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -1.5))+
  guides(fill= "none")
ggsave("Volume of sales in Texas (2010-2014).png")

Gini_index = function(x){
  ni = table(x)
  fi = ni/length(x)
  j = length(table(x))
  G=1-sum(fi^2)
  Gi = G/((j-1)/j)
  return(Gi)
}

Gini_vol_cl = Gini_index(vol_cl)


#7) PROBABILITY THAT THE CITY IS BEAUMONT

nrow(re_texas[city == "Beaumont",])/nrow(re_texas)


#PROBABILITY THAT THE MONTH IS JULY

nrow(re_texas[month == 7,])/nrow(re_texas)


#PROBABILITY THAT THE PERIOD OF THE YEAR IS DECEMBER 2012

nrow(re_texas[c(month == 12 & year == 2012),])/nrow(re_texas)  


#8) CREATE A MEAN PRICE COLUMN
  
re_texas$mean_price = volume/sales*1000000
attach(re_texas)

mu9 = mean(mean_price)
med9 = median(mean_price)

r9 = range(mean_price)
iqr9 = IQR(mean_price)
v9 = var(mean_price)
sd9 = sd(mean_price)
cv9 = coeff.vaiation(mean_price)

plot(density(mean_price),xlab = "mean price ($)" ,ylab = "frequency", main = "Mean Price distribution")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(mean_price), col = "red")
abline(v = median(mean_price))

mu3_9 = skewness(mean_price)
mu4_9 = kurtosis(mean_price)-3


#9) LISTINGS EFFICACY

re_texas$list_efficacy = sales/listings*100
attach(re_texas)

mu10 = mean(list_efficacy)
med10 = median(list_efficacy)

r10 = range(list_efficacy)
iqr10 = IQR(list_efficacy)
v10 = var(list_efficacy)
sd10 = sd(list_efficacy)
cv10 = coeff.vaiation(list_efficacy)

plot(density(list_efficacy),xlab = "sold listings on total number (%)" ,ylab = "frequency", main = "Listing efficacy distribution")
legend("topright", legend = c("mean","median"), pch = "|", col = c("red","black"))
abline(v = mean(list_efficacy), col = "red")
abline(v = median(list_efficacy))

mu3_10 = skewness(list_efficacy)
mu4_10 = kurtosis(list_efficacy)-3


re_texas$year_and_month = as.factor(paste(month,year,sep = "/"))
re_texas$year_and_month = factor(re_texas$year_and_month, levels = re_texas$year_and_month[1:60])
attach(re_texas)

ggplot(re_texas) + geom_col(aes(year_and_month,list_efficacy, fill = year),position = "dodge") + 
  labs( x = "time (month)", y = "sold listings on total number (%)", title ="Listings efficacy over the years (2010-2014)") +
  scale_x_discrete(breaks = c("7/2010","7/2011","7/2012","7/2013","7/2014"), labels = c(2010,2011,2012,2013,2014))+
  theme(plot.title = element_text(hjust = 0.5),
       axis.title.y = element_text(vjust = +3),
       axis.title.x = element_text(vjust = -1.5))+
  facet_wrap(~city)


#10) SUMMARY OF SOME VARIABLES (MEAN AND SD) THROUGH DPLYR

#a)

re_texas%>%
  group_by(city) %>%
  summarise(media=mean(median_price),
            deviazione_standard =sd(median_price))

re_texas%>%
  group_by(month) %>%
  summarise(media=mean(median_price),
            deviazione_standard =sd(median_price))

re_texas%>%
  group_by(year) %>%
  summarise(media=mean(median_price),
            deviazione_standard =sd(median_price))


#b)

re_texas%>%
  group_by(city) %>%
  summarise(media=mean(mean_price),
            deviazione_standard =sd(mean_price))

re_texas%>%
  group_by(month) %>%
  summarise(media=mean(mean_price),
            deviazione_standard =sd(mean_price))

re_texas%>%
  group_by(year) %>%
  summarise(media=mean(mean_price),
            deviazione_standard =sd(mean_price))


#11) GRAPHS

#boxplot of median price per city

re_texas%>%
  group_by(city) %>%
  summarise(mediana=median(median_price),
            deviazione_standard =sd(median_price))

ggplot(re_texas)+ geom_boxplot(aes(city,median_price, fill = city))+ labs(x = "city of Texas", y = "median price ($)", title = "Median price per Texas city (2010-2014)") +
  theme(plot.title = element_text(hjust = 0.5))+
  guides(fill= "none")


# altrnative of boxplot for volume over the years

re_texas%>%
  group_by(city) %>%
  summarise(mediana=median(volume),
            deviazione_standard =sd(volume))

re_texas%>%
  group_by(year) %>%
  summarise(mediana=median(volume),
            deviazione_standard =sd(volume))


# volume x city (fill = city)

ggplot(re_texas)+ geom_boxplot(aes(city,volume, fill = city))+ labs(x = "city", y = "value of total sales (million $)", title = "Overall sales per city in Texas (2010-2014)") + 
  theme_gray()  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(0,90,10))+
  scale_fill_brewer(palette = "Set2")+
  guides(fill= "none")


#volume x year (fill = year)

re_texas$year = factor(year)
attach(re_texas)

ggplot(re_texas)+ geom_boxplot(aes(year,volume, fill = year))+ labs(x = "year", y = "value of total sales (million $)", title = "Overall sales in Texas per year")+
  theme_gray()  +
  theme(plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = seq(0,90,10))+
  scale_fill_brewer(palette = "Set3")+
  guides(fill= "none")


# barplot per each year

#2010

re_texas_2010 = subset(re_texas, year == "2010")
monthf_10 = as.factor(re_texas_2010$month)
monthf_10

ggplot(re_texas_2010) + geom_col(aes(monthf_10,volume, fill = city ),position = "stack")+ 
  labs(x = "month", y = "value of total sales (million $)", title = "Total Texas sales in 2010") + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Paired") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,150,10))
ggsave("sales_per_month_2010.png")


#norm

ggplot(re_texas_2010) + geom_col(aes(monthf_10,volume, fill = city ),position = "fill")+ 
  labs(x = "month", y = "normalized value of total sales (%)", title = "Total Texas sales in 2010") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Paired") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,1,0.1),labels = seq(0,100,10))
ggsave("sales_per_month_2010_norm.png")


#2011

re_texas_2011 = subset(re_texas, year == "2011")
monthf_11 = as.factor(re_texas_2011$month)

ggplot(re_texas_2011) + geom_col(aes(monthf_11,volume, fill = city ),position = "stack")+ 
  labs(x = "month", y = "value of total sales (million %)", title = "Total Texas sales in 2011") + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Set1") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,150,10))
ggsave("sales_per_month_2011.png")


#norm

ggplot(re_texas_2011) + geom_col(aes(monthf_11,volume, fill = city ),position = "fill")+ 
  labs(x = "month", y = "normalized value of total sales (%)", title = "Total Texas sales in 2011") +  theme_classic()+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Set1") + scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,1,0.1),labels = seq(0,100,10))
ggsave("sales_per_month_2011_norm.png")


#2012

re_texas_2012 = subset(re_texas, year == "2012")
monthf_12 = as.factor(re_texas_2012$month)

ggplot(re_texas_2012) + geom_col(aes(monthf_12,volume, fill = city ),position = "stack")+ 
  labs(x = "month", y = "value of total sales (million $)", title = "Total Texas sales in 2012") + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,150,10))
ggsave("sales_per_month_2012.png")


#norm

ggplot(re_texas_2012) + geom_col(aes(monthf_12,volume, fill = city ),position = "fill")+ 
  labs(x = "month", y = "normalized value of total sales (%)", title = "Total Texas sales in 2012") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Dark2") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(breaks = seq(0,1,0.1),labels = seq(0,100,10))
ggsave("sales_per_month_2012_norm.png")


#2013

re_texas_2013 = subset(re_texas, year == "2013")
monthf_13 = as.factor(re_texas_2013$month)

ggplot(re_texas_2013) + geom_col(aes(monthf_13,volume, fill = city ),position = "stack")+ 
  labs(x = "month", y = "value of total sales (million $)", title = "Total Texas sales in 2013")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "RdYlGn") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,200,10))
ggsave("sales_per_month_2013.png")


#norm

ggplot(re_texas_2013) + geom_col(aes(monthf_13,volume, fill = city ),position = "fill")+ 
  labs(x = "month", y = "normalized value of total sales (%)", title = "Total Texas sales in 2013") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "RdYlGn") + scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(breaks = seq(0,1,0.1),labels = seq(0,100,10))
ggsave("sales_per_month_2013_norm.png")


#2014

re_texas_2014 = subset(re_texas, year == "2014")
monthf_14 = as.factor(re_texas_2014$month)

ggplot(re_texas_2014) + geom_col(aes(monthf_14,volume, fill = city ),position = "stack")+ 
  labs(x = "month", y = "values of total sales (million $)", title = "Total Texas sales in 2014") + 
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "PuOr") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,230,10))
ggsave("sales_per_month_2014.png")


#norm

ggplot(re_texas_2014) + geom_col(aes(monthf_14,volume, fill = city ),position = "fill")+ 
  labs(x = "month", y = "normalized value of total sales (%)", title = "Total Texas sales in 2014") +
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "PuOr") + 
  scale_x_discrete(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  scale_y_continuous(breaks = seq(0,1,0.1),labels = seq(0,100,10))
ggsave("sales_per_month_2014_norm.png")



#(2010-2014)


ggplot(re_texas) + geom_col(aes(month,volume, fill = city),position = "stack")+ 
  labs(x = "month", y = "value of total sales (million $)", title = "Total Texas sales per each year (2010-2014)") +
  theme_grey()+
  scale_x_continuous(breaks = seq(1,12,1),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,220,20))+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Spectral") +
  facet_wrap(~year)
ggsave("tot_sales_2010-2014.png")


#norm (2010-2014)


ggplot(re_texas) + geom_col(aes(month,volume, fill = city),position = "fill")+ 
  labs(x = "month", y = "normalized value of total sales (million $)", title = "Total Texas sales per each year (2010-2014)") +
  theme_grey()+
  scale_x_continuous(breaks = seq(1,12,1),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))+
  scale_y_continuous(breaks = seq(0,1,0.1),labels = seq(0,100,10))+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_brewer(palette = "Set1") +
  facet_wrap(~year)
ggsave("tot_sales_2010-2014-norm.png")


#line chart of a variable of interest

re_texas$time = rep(seq(1,60,1),4)
                     
ggplot(re_texas)+ geom_line(aes(time ,sales, col = city), lwd = 1.6)+
  geom_point(aes(time,sales, col = city),size = 3)+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = +3),
        axis.title.x = element_text(vjust = -0.75))+
  labs(x = "month", y = "number of sales", title = "Total number of sales from 2010 to 2014") +
  scale_x_continuous(breaks = seq(1,60,4), labels = c("Jan/2010","May/2010","Sep/2010","Jan/2011","May/2011","Sep/2011","Jan/2012","May/2012","Sep/2012","Jan/2013","May/2013","Sep/2013","Jan/2014","May/2014","Sep/2014"))+
  scale_y_continuous(breaks = seq(0,430,25))
  ggsave("sales_over_time.png")

