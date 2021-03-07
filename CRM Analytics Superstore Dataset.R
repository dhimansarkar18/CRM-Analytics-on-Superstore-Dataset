#Read the data
setwd("/Users/dhimansarkar/Desktop")
data.df <- read.csv("group.csv")

#Understanding Data
data.df <- group.csv
head(data.df)
variable.names(data.df)

#Importing Libraries
library(dplyr)
library(boot)
library(car)
library(moments)
library(tidyr)
variable.names(data.df)
#"Row.ID"         "Order.ID"       "Order.Date"     "Ship.Date"      "Ship.Mode"      "Customer.ID"    "Customer.Name" 
#"Segment"        "City"           "State"          "Country"        "Postal.Code"    "Market"         "Region"        
#"Product.ID"     "Category"       "Sub.Category"   "Product.Name"   "Sales"          "Quantity"       "Discount"      
#"Profit"         "Shipping.Cost"  "Order.Priority"

total_profit <- data.df %>%
  group_by(Customer.Name) %>%
  summarise(total_profit = sum(Profit),
            total_discount = sum(Discount),
            total_sales = sum(Sales))

total_profit

data.df <- total_profit %>% left_join(data.df)
head(data.df,20)
str(data.df)

#changing date from character to date structure
a <- as.Date(data.df$Order.Date,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(data.df$Order.Date,format="%d-%m-%Y") # Produces NA when format is not "%d-%m-%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both while keeping their ranks
data.df$Order.Date <- a

summary(data.df$Order.Date)
rfm.df<-data.df
#define frequency
frequency <- rfm.df %>%count(Customer.Name) 
frequency
rfm.df <- frequency %>% left_join(rfm.df)

#define recency
recency <- rfm.df %>%
  group_by(Customer.Name) %>%
  summarise(last_date = max(Order.Date))
rfm.df <- recency %>% left_join(rfm.df)
variable.names(rfm.df)#check to make sure last_date and n columns have been added to rfm.df
rfm.df %>% mutate(frequency=n,recency=last_date,monetary=total_profit)->rfm.df
head(rfm.df)
summary(rfm.df$frequency)
rfm.df %>% mutate(Findex=cut(frequency,breaks=c(28,58,67,77,109),labels=c("1","2","3","4")))->rfm.df
summary(rfm.df$recency)
date1 <- as.Date("2015-01-01", tz="UTC")

as.numeric(difftime(date1, rfm.df$recency, units="days"))->rfm.df$Rdays
summary(rfm.df$Rdays)
rfm.df %>% mutate(Rindex=cut(frequency,breaks=c(0,7,17,36,430),labels=c("1","2","3","4")))->rfm.df             
summary(rfm.df$total_profit)
rfm.df %>% mutate(Mindex=cut(frequency,breaks=c(-6153,1040,1834,2673,8674),labels=c("1","2","3","4")))->rfm.df
variable.names(rfm.df)
# you can combine these three indices in anyway you want; to segment your market
# for example, have a linear combination
rfm.df %>% mutate(CLV=33.33*as.numeric(Findex)/5+33.33*as.numeric(Rindex)/5+33.33*as.numeric(Mindex)/5)->rfm.df
summary(rfm.df$CLV) #33.33% Frequency, 33.33% Recency, 33.33% Monetary
#Findex = frequency index, Rindex = recency index, Mindex = monetary index


Customer_Freq<-as.data.frame(table(rfm.df$Customer.Name))
Customer_Freq #795 unique Customer Names

#Questions we want to answer

#Who is most valuable?
rfm.df %>% group_by(Customer.Name,clmns=CLV) %>% summarize(N=n())->CLV.df
CLV.df[order(CLV.df$clmns,decreasing=TRUE),]->CLV.df
head(CLV.df,n=50)->Top50_CLV
Top50_CLV #shows top 50 Customer Names and their CLV in clmns
#customers who have the same CLV are ordered alphabetically
#the max CLV is 60


#Locations of our most valuable customers?
rfm.df %>% group_by(Customer.Name,clmns=CLV) %>% summarize(N=n(),Region=Region,Market=Market,Country=Country)->CLV2.df
CLV2.df #includes Region, Market,and Country. Customer Names repeats if the customer has ordered from multiple locations
#also shows frequency of each location
CLV2.df[order(CLV.df$clmns,decreasing=TRUE),]->CLV2.df
head(CLV2.df,n=1000)->Top1000_CLV_Location
Top1000_CLV_Location #shows one line for each order a top customer placed

#Most frequent country of top customers(1000 orders)
table(Top1000_CLV_Location$Country)
country_freq.df<-as.data.frame(table(Top1000_CLV_Location$Country))
country_freq.df[order(country_freq.df$Freq,decreasing=TRUE),]->country_freq.df
head(country_freq.df,n=10)->Mostvaluable_countries
Mostvaluable_countries #shows 10 countries that appear most in top 1000 orders of top customers

#Most frequent market of top customers
market_freq.df<-as.data.frame(table(Top1000_CLV_Location$Market))
market_freq.df[order(market_freq.df$Freq,decreasing=TRUE),]->market_freq.df
head(market_freq.df,n=10)->Mostvaluable_markets
Mostvaluable_markets

#Most frequent region of top customers
region_freq.df<-as.data.frame(table(Top1000_CLV_Location$Region))
region_freq.df[order(region_freq.df$Freq,decreasing=TRUE),]->region_freq.df
head(region_freq.df,n=10)->Mostvaluable_regions
Mostvaluable_regions

#Create df with Customer Name, CLV, product category, and sub category
rfm.df %>% group_by(Customer.Name,clmns=CLV) %>% summarize(N=n(),Product_Category=Category,Sub_Category=Sub.Category)->CLV3.df
CLV3.df#includes product category and sub-category
CLV3.df[order(CLV3.df$clmns,decreasing=TRUE),]->CLV3.df
head(CLV3.df,n=1000)->Top1000_CLV_Products

#Most frequent product category
category_freq.df<-as.data.frame(table(Top1000_CLV_Products$Product_Category))
category_freq.df[order(category_freq.df$Freq,decreasing=TRUE),]->category_freq.df
head(category_freq.df,n=3)->Mostvaluable_productcategories #only 3 product categories
Mostvaluable_productcategories

#Most frequent sub-category
sub_freq.df<-as.data.frame(table(Top1000_CLV_Products$Sub_Category))
sub_freq.df[order(sub_freq.df$Freq,decreasing=TRUE),]->sub_freq.df
head(sub_freq.df,n=20)->Mostvaluable_subcategories
Mostvaluable_subcategories

###EVERYTHING BELOW DOES NOT FACTOR IN CLV
#Where are our customers they located? Only frequency, Not factoring in CLV
country_count.df<-as.data.frame(table(rfm.df$Country))
country_count.df[order(country_count.df$Freq,decreasing=TRUE),]->country_count.df
head(country_count.df,n=10)->Top10_Countries
Top10_Countries

state_count.df<-as.data.frame(table(rfm.df$State))
state_count.df[order(state_count.df$Freq,decreasing = TRUE),]->state_count.df
state_count.df
head(state_count.df,n=50)->Top50_States
Top50_States

city_count.df<-as.data.frame(table(rfm.df$City))
city_count.df[order(city_count.df$Freq,decreasing=TRUE),]->city_count.df
head(city_count.df,n=50)->Top50_Cities
Top50_Cities

region_count.df<-as.data.frame(table(rfm.df$Region))
region_count.df[order(region_count.df$Freq,decreasing=TRUE),]->region_count.df
head(region_count.df,n=5)->Top5_Regions #13 regions total
Top5_Regions

market_count.df<-as.data.frame(table(rfm.df$Market))
market_count.df[order(market_count.df$Freq,decreasing=TRUE),]->market_count.df
head(market_count.df,n=10)->Top_Market_Freq #only 7 markets so I just did frequency of each
Market_Freq

#Ship Mode is string so, summary not helpful
summary(data.df$Ship.Mode)
#check for Na
which(is.na(data.df))
which(duplicated(data.df)) # no duplicates
#What is the volume of different shipping modes?
table(data.df$Ship.Mode)
# First Class:7505,Same Day:2701, Second Class:10309, Standard Class: 30775
#Unsurprisingly, a majority is in standard, each increase in ship mode has fewer packages
# Now we want to see how profit is relating to shipping mode, 
#start with grouping and average, then regression
profit_by_ship_mode <- data.df %>%
  group_by(Ship.Mode) %>%
  summarise(total_profit = sum(Profit),
            mean_profit = mean(Profit),
            mean_sales = mean(Sales))
profit_by_ship_mode
# based on this grouping, mean profit increases as the classes are less quick
#mean sales also increase as class decreases, but now doing regression to see if this is significant
shipanova.aov<-aov(Profit~Ship.Mode, data=data.df)
summary(shipanova.aov) # p - value is well above acceptable
TukeyHSD(shipanova.aov) # Tukey test confirms that p-value is too high
plot(shipanova.aov,1) # removing outliers to see if any changes in significance
shipanova1.aov <- aov(Profit~Ship.Mode, data=data.df[-c(38849,8899,43454,29947,30125,37930),])
summary(shipanova1.aov)
plot(shipanova1.aov,1)
leveneTest(Profit~Ship.Mode,data=data.df)
plot(shipanova.aov,2) # does not look normal
aov_residuals<-residuals(object = shipanova.aov)
ks.test(x=aov_residuals, y='pnorm',alternative='two.sided')
#data not normal, so transforming for normality
HSK(log(data.df$Profit))
data.df$logp=log(data.df$Profit)
ship2.aov<-aov(logp~Ship.Mode, data=data.df)
summary(ship2.aov)
TukeyHSD(ship2.aov)
# Did not work, try to re-run later
#Most and least profitable products
prod.profit<- data.df %>% group_by(Product.Name) %>% summarize(N=n())
data.df%>% group_by(Product.Name) %>% summarise(profit.byproduct = mean(Profit), sales.byproduct=mean(Sales), N=n())-> prod.name.profit
head(prod.name.profit)
prod.name.profit$rank<-rank(prod.name.profit$profit.byproduct)

prod.name.profit[order(prod.name.profit$rank, decreasing=TRUE),]
# most profitable product on avg is Canon imageClass 2200 Advanced Copier
prod.name.profit[order(prod.name.profit$rank, decreasing=FALSE),]
# least profitable product on avg is the Cubify CubeX 3d Printer Triple Head
#Most and least median products
prod.median<- data.df %>% group_by(Product.Name) %>% summarize(N=n())
data.df%>% group_by(Product.Name) %>% summarise(median.byproduct = median(Profit), medsales.byproduct=mean(Sales), N=n())-> prod.median
head(prod.median)
prod.median$rank<-rank(prod.median$median.byproduct)

prod.median[order(prod.median$rank, decreasing=TRUE),]
# most profitable product on median is Canon imageClass 2200 Advanced Copier
prod.median[order(prod.median$rank, decreasing=FALSE),]
# least profitable product on median is the Cubify CubeX 3d Printer Triple Head
#Most and least profitable & sales by sub-category
subcat.profit<- data.df %>% group_by(Sub.Category) %>% summarize(N=n())
data.df%>% group_by(Sub.Category) %>% summarise(profit.bysubcat = mean(Profit), sales.bysubcat=mean(Sales), N=n())-> subcat.profit
head(subcat.profit)
subcat.profit$rank<-rank(subcat.profit$profit.bysubcat)

subcat.profit[order(subcat.profit$rank, decreasing=TRUE),]
# most profitable subcategory on avg is Copiers w/$116
subcat.profit[order(subcat.profit$rank, decreasing=FALSE),]
# least profitable subcategory on avg is Tables w/-$74.4
#Most and least median subcat
subcat.median<- data.df %>% group_by(Sub.Category) %>% summarize(N=n())
data.df%>% group_by(Sub.Category) %>% summarise(median.bysubcat = median(Profit), medsales.bysubcat=mean(Sales), N=n())-> subcat.median
head(subcat.median)
subcat.median$rank<-rank(subcat.median$median.bysubcat)

subcat.median[order(subcat.median$rank, decreasing=TRUE),]
# most profitable Subcategory on median is Copiers w/$65.90
subcat.median[order(subcat.median$rank, decreasing=FALSE),]
# least profitable Subcategory on median is Tables w/ -$34.60
# Most and Least profitable/Sales Categories
cat.profit<- data.df %>% group_by(Category) %>% summarize(N=n())
data.df%>% group_by(Category) %>% summarise(profit.bycat = mean(Profit), sales.bycat=mean(Sales), N=n())-> cat.profit
head(cat.profit)
cat.profit$rank<-rank(cat.profit$profit.bycat)

cat.profit[order(cat.profit$rank, decreasing=TRUE),]
# most profitable category on avg is Technology with $65.50 in profit and $468 in sales
cat.profit[order(cat.profit$rank, decreasing=FALSE),]
#Least profitable category on avg is Office Supplies with $16.60 in profit and $121 in sales
#Most and least median categories
cat.median<- data.df %>% group_by(Category) %>% summarize(N=n())
data.df%>% group_by(Category) %>% summarise(median.bycat = median(Profit), medsales.bycat=mean(Sales), N=n())-> cat.median
head(cat.median)
cat.median$rank<-rank(cat.median$median.bycat)

cat.median[order(cat.median$rank, decreasing=TRUE),]
# most profitable Category on Median is Technology w/ $29.90
cat.median[order(cat.median$rank, decreasing=FALSE),]
# least profitable Category on Median is Office Supplies w/$6.55
# Precentage of order priorities
summary(data.df$Order.Priority)
table(data.df$Order.Priority) # Most to least common: Medium, High,Critical,Low
prop.table(data.df$Order.Priority)*100
#Most and Least profitable/Sales Regions
region.profit<- data.df %>% group_by(Region) %>% summarize(N=n())
data.df%>% group_by(Region) %>% summarise(profit.byregion = mean(Profit), sales.byregion=mean(Sales), N=n())-> region.profit
head(region.profit)
region.profit$rank<-rank(region.profit$profit.byregion)

region.profit[order(region.profit$rank, decreasing=TRUE),]
# most profitable region on avg is North Asia w/$70.80 per order and 363 sales
region.profit[order(region.profit$rank, decreasing=FALSE),]
#Least profitable region on avg is Southeast Asia w/$5.71 per order w/ 283 sales
#Most and Least profitable/Sales Segments
seg.profit<- data.df %>% group_by(Segment) %>% summarize(N=n())
data.df%>% group_by(Segment) %>% summarise(profit.byseg = mean(Profit), sales.byseg=mean(Sales), N=n())-> seg.profit
head(seg.profit)
seg.profit$rank<-rank(seg.profit$profit.byseg)

seg.profit[order(seg.profit$rank, decreasing=TRUE),]
# most profitable segment on avg is Home Office w/$29.60
seg.profit[order(seg.profit$rank, decreasing=FALSE),]
#Least profitable segment on avg is Consumer w/$28.30
#Grouping Country by profit, emphasis on 5 most profitable w/ large number of sales
# group countries
data.df %>% group_by(Country) %>% summarise(N=n())
data.df %>% group_by(Country) %>% filter(n()>=500)->big.country.df
big.sales<-big.country.df %>% group_by(Country) %>% summarise(totalsales=sum(Sales),
                                                              avgsales=mean(Sales),
                                                              avgprofit=mean(Profit),
                                                              totalprofit=sum(Profit))
rank(big.sales$avgprofit)->big.sales$rank
big.sales
#generate rank order by country based on avg profit
order.big.sales<-big.sales[sort(big.sales$rank),]#sort will not change the display
order.big.sales<-big.sales[order(big.sales$rank),] # will be ascending
order.big.sales
#Five countries with lowest avg profit: Nigeria, Turkey,
#Honduras,Philippines,Dominican Republic
order.big.sales<-big.sales[order(big.sales$rank,decreasing = TRUE),]#can change to descending
order.big.sales
# Five Countries with highest avg profit: India, China, UK, Spain, El Salvador
#Cleaning dates for seasonality analysis 
data.df$Order.Date<-as.character(data.df$Order.Date)
right = function (string, char) {
  substr(string,nchar(string)-(char-1),nchar(string))
}
right(data.df$Order.Date,4)->data.df$year
table(data.df$year)
#Count of orders increases over each year

#we have dates in different formats
library(tidyr)
data.df$order<-as.character(data.df$Order.Date)
data.df$Order.Date<-as.character(data.df$Order.Date)
right = function (string, char) {
  substr(string,nchar(string)-(char-1),nchar(string))
}

summary(data.df$Order.Date)
a <- as.Date(data.df$Order.Date,format="%m/%d/%Y") # Produces NA when format is not "%m/%d/%Y"
b <- as.Date(data.df$Order.Date,format="%d-%m-%Y") # Produces NA when format is not "%d-%m-%Y"
a[is.na(a)] <- b[!is.na(b)] # Combine both to have new cleaned dates
data.df$Order.Date <- a # Put it back into the dataframe
#Now dates formats are cleaner
summary(data.df$Order.Date)
data.df$days<-weekdays(data.df$Order.Date)
table(data.df$days) #creating day of week var
data.df$year<-substring(data.df$Order.Date,1,4)
data.df$month<-substring(data.df$Order.Date,6,7)
table(data.df$month) #creating month var
data.df$dates<-substring(data.df$Order.Date,9,10)
table(data.df$dates) #creating date var
variable.names(data.df)

# visually showing sales and profit change by day of week, month, and date
# _____Double check to make sure you need these visualizations_______
library(ggplot2)
ggplot(aes(x=days,y=Sales),data=data.df)+geom_point()
ggplot(aes(x=dates,y=Sales),data=data.df)+geom_point()
ggplot(aes(x=month,y=Sales),data=data.df)+geom_line()
ggplot(aes(x=Market,y=Sales,fill=Market),data=data.df)+geom_bar(stat="identity")
#EDA
ggplot(data.df,aes(Sub.Category,fill=Sub.Category)) + geom_bar() + labs(title="Sub Category Sales count")
ggplot(data.df,aes(Market,fill=Market)) + geom_bar() + labs(title=" Order count by Market") 
ggplot(data.df,aes(Order.Priority,fill=Order.Priority)) + geom_bar() + labs(title="Orders by Priority Level") 

#detecting outliers
#data.df %>% filter(month=="01")->data.df
table(data.df$month)
a<-boxplot(data.df$Sales)
outliers<-a$out
outliers



#Multiple Regression to find what factors affect profit
# create category variables
variable.names(data.df)
data.df$orderfactor <- as.factor(data.df$Order.ID)
data.df$custfactor <- as.factor(data.df$Customer.ID)
data.df$shipfactor <- as.factor(data.df$Ship.Mode)
data.df$yearfactor <- as.factor(data.df$year)
data.df$dayfactor <- as.factor(data.df$days)
data.df$monthfactor <- as.factor(data.df$month)
data.df$priorityfactor <- as.factor(data.df$Order.Priority)
data.df$pcfactor <- as.factor(data.df$Postal.Code)
#Checking which years are most and least profitable by avg
year.profit<- data.df %>% group_by(year) %>% summarize(N=n())
data.df%>% group_by(year) %>% summarise(profit.byyear = mean(Profit), sales.byyear=mean(Sales), N=n())-> year.profit
head(year.profit)
year.profit$rank<-rank(year.profit$profit.byyear)

year.profit[order(year.profit$rank, decreasing=TRUE),]
# most profitable year on avg is 2013 w/ $29.50
year.profit[order(year.profit$rank, decreasing=FALSE),]
#Least profitable year on avg is 2011 w/$27.70
# trying naive model
profit.m1<-lm(Profit~Sales+Category+Quantity+Country+Sub.Category+Region, data=data.df)
summary(profit.m1)
library(car)
library(corrplot)
outlierTest(profit.m1) #some outliers we will remove for AIC
# There are significant differences, We will go to AIC to see what
#efficient profit model
start=lm(Profit~1,data=data.df[c(-28613,-43454,-15887,-8899,-43137,-42768,-38849,-28814,-11890,-22944),])
profit.aic<-step(start,
                 scope=Profit~Ship.Mode+Segment+Country
                 +Market+Region+Category+Sub.Category+Sales+Quantity+Discount+Shipping.Cost
                 +Order.Priority+dayfactor+monthfactor,
                 direction = "both",rm.na=T)
summary(profit.aic)
#AIC Best Model
profit.m2<- lm(Profit~Sales+Discount+Sub.Category+Quantity+Country+Shipping.Cost
               +Region+Order.Priority+Ship.Mode, data = data.df[c(-28613,-43454,-15887,-8899,-43137,-42768,-38849,-28814,-11890,-22944),])
summary(profit.m2)
# significant p-value, but only predicts 40% of profit
plot(profit.m2)
#Checking VIF to see what factors we need to remove
vif(profit.m2)# error because aliased coefficients
profit.m3<- lm(Profit~Sales+Discount+Sub.Category+Quantity+Country+Shipping.Cost
               +Region+Order.Priority, data = data.df[c(-28613,-43454,-15887,-8899,-43137,-42768,-38849,-28814,-11890,-22944),])
summary(profit.m3)
#Test without order priority
profit.m4<- lm(Profit~Sales+Discount+Sub.Category+Quantity+Country+Shipping.Cost
               +Region, data = data.df[c(-28613,-43454,-15887,-8899,-43137,-42768,-38849,-28814,-11890,-22944),])
summary(profit.m4) # no change, but fewer variables, so it is better
anova(profit.m1,profit.m2)
anova(profit.m2,profit.m3) #no significant difference between model 2 and 3
#Our most effective model says Sales, Discount,Sub.Category,Quantity,Country,
#Shipping.Cost, and order priority are all important factors for profit prediction
variable.names(data.df)
# Power transform and re-running profit regression to see if there are any change
#powerTransform(data.df$Profit) #
#lambda<-coef(powerTransform(data.df$Profit)) # does not work because profit is not positive in some cases
#bcPower(data.df$Profit,lambda)->data.df$transformedProfit
# will try to run with logp(Log Profit)
# Log Profit also does not work
#logprofit.m1<-lm(logp~Sales+Category+Quantity+Country+Sub.Category+Region, data=data.df)
#summary(logprofit.m1)
# Running a multiple regression on sales to test if there is seasonality
# as well as other factors involved, naive model first
sales.m1 <- lm(Sales~Ship.Mode+Segment+Country+Market+Region+Category+
                 Sub.Category+Quantity+Discount+Shipping.Cost
               +Order.Priority+dayfactor+monthfactor,data.df[c(-28613,-43454,-15887,-8899,-43137,-42768,-38849,-28814,-11890,-22944),])
summary(sales.m1)
# These factors do a pretty good job of describing sales,
#approx 70%, with significant p-value
#improving withAIC
start=lm(Sales~1,data=data.df[c(-28613,-43454,-15887,-8899,-43137,-42768,-38849,-28814,-11890,-22944),])
sales.aic<-step(start,
                scope=Sales~Ship.Mode+Segment+Country+Market+Region+Category+
                  Sub.Category+Quantity+Discount+Shipping.Cost
                +Order.Priority+Product.Name,
                direction = "both",rm.na=T)
#AIC Best Model
sales.m2 <- lm(Sales~Shipping.Cost+Order.Priority+Sub.Category+Quantity
               +Ship.Mode+Discount+Market+Region,data=data.df[c(-28613,-43454,-15887,-8899,-43137,-42768,-38849,-28814,-11890,-22944),] )
summary(sales.m2)
#AIC Model doesnt look much better than naive model
anova(sales.m1,sales.m2) # the difference is not significant, going to
# try to test for covariance
# no seasonality as the month and the day of the week have no significant
#effect on sales in either model
library(corrplot)
#not useful data.df$daynumeric <-as.numeric(data.df$days) # not useful
# not useful data.df$monthnumeric <-as.numeric(data.df$month) # not useful
corrplot.mixed(corr=cor(data.df[,c(19,20,21,22,23)], use="complete.obs"), 
               upper="ellipse", tl.pos="lt")
cor(data.df[,c(19:23)]) # strongest correlation is between Sales and Shipping Cost, negative


# Market Basket Analysis for Group Presentation 

library(arules)
library(arulesViz)


basket <- group.csv
dim(basket)  #51290 rows, 24 Cols, Row.ID is the rowid column from excel & not available in Data Dictionary. Need to remove it.
head(basket)
basket <- basket[,-1]
dim(basket)  #51290 rows, 23 Cols, Order.ID is the key column.

str(basket) #Most of the cols are character/ Numeric data type. Our consideration will be sub-category
summary(basket)

#Lets find unique order id count
length(unique(basket$Order.ID)) #--> 25035 unique Order.ID
length(unique(basket$Sub.Category)) #17 unique sub category --> important for us 


#Converting the DF to transactional format. 

#Using read.transactions
#create mini DF (only required columns in  factor format)
#to a csv file and then read it using read.transactions function
#removes any irregularities possible in the data

basket_mini <- basket[,c("Order.ID","Sub.Category")] # ---> making subset(don't care about customer)
head(basket_mini) 
write.csv(basket_mini,"transdata",row.names = F) #force the DF into csv
transdata<-read.transactions(file="transdata",format="single",sep=",",cols=c("Order.ID","Sub.Category"), rm.duplicates = T, header = T)

basket_transactions<-as(transdata,"transactions")
summary(basket_transactions)
#visually showing most frequent items

# For Analysis --> Appliances 3rd least frequent item 
# Binders --> Number 1 most frequent item 

itemFrequencyPlot(basket_transactions,topN=20,type="absolute")
# find some initial rules

basket.rules <- apriori(basket_transactions, parameter=list(support=0.0006, conf=0.5, 
                                                      target="rules"))

#takes a lot of trial-error to arrive at this final results
#look at density (support level(min supoort I care about) around density)
#increased the support by 0.0001 to get less rules 
#low support because of the 
inspect(basket.rules)

#discover strong relationship btewen appliances and Binders 

best_lift<-head(basket.rules, n=5, by= "lift")
inspect(best_lift)



group.hi<-head(sort(basket.rules,by="lift"),5)
plot(group.hi,method="graph",control=list(type="items"))

# darker color --> higher lift 
# {Fasteners,Labels,Machines}      => {Art} 
# Fasteners bring the least profit so we can offer a discount 



# circle --> set of items 
# size of circle --> how frequently purchased 
# {Appliances,Paper,Storage}       => {Binders} --> most frequent observation/transaction 

# Storage least profitable sub-categroy --> offer discount 
# but relationship not as strong (brighter color)
#
#

