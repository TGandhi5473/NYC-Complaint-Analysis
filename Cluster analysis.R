data <- read.csv('/Users/Robin/Desktop/Research proposal/311_analysis_data.csv')
str(data)

library(tidyverse)

#Subset needs-based variables including weather
data_cluster_with_weather <- data %>%
  select(Density_per_square_mile, Male_percentage, Median_age_years, ratio_no_education, ratio_at_least_lower_school, ratio_at_least_high_school, ratio_finished_higher_degree, Median_income, temp_f, precip_inch)
str(data_cluster_with_weather)
sum(is.na(data_cluster_with_weather)) #no missing values

#Scale
data_cluster_with_weather = scale(data_cluster_with_weather)

#Distribute work multiple system cores 
library(parallel)
num_cores <- 3  # Reserve core for system tasks

#Total within sum of squares plot - zip code variables and weather
within_ss = mclapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = data_cluster_with_weather,centers = x,iter.max = 1000,nstart = 25)$tot.withinss}, mc.cores = num_cores)

within_ss <- unlist(within_ss)

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#K-means Clustering with weather - 3 centers
set.seed(617)
km = kmeans(x = data_cluster_with_weather,centers = 3,iter.max=10000,nstart=25)

table(km$cluster)
k_segments <- km$cluster
data2 = cbind(data, k_segments)
str(data2)

data2 %>%
  select(Density_per_square_mile, Male_percentage, Median_age_years, ratio_no_education, ratio_at_least_lower_school, ratio_at_least_high_school,  ratio_finished_higher_degree, Median_income, temp_f, precip_inch, k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),0))%>%
  data.frame()

#Add graphs
plot1 <- data2 %>%
  select(Density_per_square_mile,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,Density_per_square_mile)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot2 <- data2 %>%
  select(Male_percentage,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,Male_percentage)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot3 <- data2 %>%
  select(Median_age_years,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,Median_age_years)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot4 <- data2 %>%
  select(ratio_no_education, ratio_at_least_lower_school, ratio_at_least_high_school,ratio_finished_higher_degree,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,ratio_no_education, ratio_at_least_lower_school, ratio_at_least_high_school,ratio_finished_higher_degree)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot5 <- data2 %>%
  select(Median_income,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,Median_income)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot6 <- data2 %>%
  select(temp_f,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,temp_f)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot7 <- data2 %>%
  select(precip_inch,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,precip_inch)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()

library('gridExtra')
grid.arrange(plot1, plot2, plot3, plot4, plot5,plot6,plot7, ncol = 1)



#Subset needs-based variables excluding weather
data_cluster <- data %>%
  select(Density_per_square_mile, Male_percentage, Median_age_years, ratio_no_education, ratio_at_least_lower_school, ratio_at_least_high_school,  ratio_finished_higher_degree, Median_income)
sum(is.na(data_cluster)) #no missing values

#Scale
data_cluster = scale(data_cluster)

within_ss = mclapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)$tot.withinss}, mc.cores = num_cores)

within_ss <- unlist(within_ss)

ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
#Ratio plot - excluding weather, multiple education levels
ratio_ss = unlist(mclapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x = data_cluster,centers = x,iter.max = 1000,nstart = 25)
  km$betweenss/km$totss}, mc.cores = num_cores ))
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#K-means Clustering all edu vars, no weather - 3 centers
set.seed(617)
km = kmeans(x = data_cluster,centers = 3,iter.max=10000,nstart=25)

table(km$cluster)
k_segments <- km$cluster
data2 = cbind(data, k_segments)
str(data2)

data2 %>%
  select(Density_per_square_mile, Male_percentage, Median_age_years, ratio_no_education, ratio_at_least_lower_school, ratio_at_least_high_school,  ratio_finished_higher_degree, Median_income,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),0))%>%
  data.frame()

#Add graphs
plot1 <- data2 %>%
  select(Density_per_square_mile,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,Density_per_square_mile)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot2 <- data2 %>%
  select(Male_percentage,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,Male_percentage)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot3 <- data2 %>%
  select(Median_age_years,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,Median_age_years)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot4 <- data2 %>%
  select(ratio_no_education, ratio_at_least_lower_school, ratio_at_least_high_school,ratio_finished_higher_degree,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,ratio_no_education, ratio_at_least_lower_school, ratio_at_least_high_school,ratio_finished_higher_degree)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
plot5 <- data2 %>%
  select(Median_income,k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key = var,value = value,Median_income)%>%
  ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position='dodge')+
  coord_flip()
library('gridExtra')
grid.arrange(plot1, plot2, plot3, plot4, plot5, ncol = 1)

#Creating data for further analysis including clusters 

#Creating analysis data subset
analysis_data <- data2 %>%
  filter(!is.na(Duration)) %>% 
  group_by(Incident.Zip) %>%
  summarise(
    Density_per_square_mile = first(Density_per_square_mile),
    Male_percentage = first(Male_percentage),
    Median_age_years = first(Median_age_years),
    ratio_no_education = first(ratio_no_education),
    ratio_at_least_lower_school = first(ratio_at_least_lower_school),
    ratio_at_least_high_school = first(ratio_at_least_high_school),
    ratio_finished_higher_degree = first(ratio_finished_higher_degree),
    Median_income = first(Median_income),
    k_segments = first(k_segments),
    Number_complaints_per_resident = n() / first(Total_population),
    Duration = median(Duration)
  ) %>%
  rename(Zip = Incident.Zip)

write.csv(analysis_data, "analysis_data.csv", row.names = FALSE)

#Predict duration
library(caret)
set.seed(1706)
split = createDataPartition(y=analysis_data$Duration,p = 0.7,list = F)
train = analysis_data[split,]
test = analysis_data[-split,]

str(train)
lm_duration = lm(Duration~ratio_no_education+ratio_at_least_lower_school+ratio_at_least_high_school+ratio_finished_higher_degree+Median_income+k_segments+Number_complaints_per_resident,train)
summary(lm_duration)

library(rpart)
library(rpart.plot)
tree = rpart(Duration~ratio_no_education+ratio_at_least_lower_school+ratio_at_least_high_school+ratio_finished_higher_degree+Median_income+k_segments+Number_complaints_per_resident,train,minbucket=10)
rpart.plot(tree)
predTree = predict(tree,newdata=test)
rmseTree = sqrt(mean((predTree - test$Duration)^2)); rmseTree

#Predict number of complaints 
library(caret)
set.seed(1706)
split = createDataPartition(y=analysis_data$Number_complaints_per_resident,p = 0.7,list = F)
train = analysis_data[split,]
test = analysis_data[-split,]

lm_number_complaints = lm(Number_complaints_per_resident~Density_per_square_mile+Male_percentage+Median_age_years+ratio_no_education+ratio_at_least_lower_school+ratio_at_least_high_school+ratio_finished_higher_degree+Median_income+k_segments,train)
summary(lm_number_complaints)

pred <- predict(lm_number_complaints, newdata = test)
rmse_lm_number_of_complaints = sqrt(mean((pred-test$Number_complaints_per_resident)^2)); rmse_lm_number_of_complaints

library(lm.beta)
sort(lm.beta(lm_number_complaints)$standardized.coefficients,decreasing = T)

#Male percentage and median income significant. Median age very significant. 
#Male percentage associated with higher complaints per resident
#Higher median income associated with fewer complaints per resident. 
#Higher Median age associated with fewer complaints per resident.


library(rpart)
library(rpart.plot)
tree = rpart(Number_complaints_per_resident~Density_per_square_mile+Male_percentage+Median_age_years+ratio_no_education+ratio_at_least_lower_school+ratio_at_least_high_school+ratio_finished_higher_degree+Median_income+k_segments,train,minbucket=10)
rpart.plot(tree)
predTree = predict(tree,newdata=test)
rmseTree_complaints = sqrt(mean((predTree - test$Number_complaints_per_resident)^2)); rmseTree_complaints
#Tree has lower RMSE than regression

#Subset duration for noise residential complaints
analysis_data_noise_residential <- data2 %>%
  filter(!is.na(Duration)) %>% 
  filter(Complaint.Type == "Noise - Residential") %>%
  group_by(Incident.Zip) %>%
  summarise(
    Density_per_square_mile = first(Density_per_square_mile),
    Male_percentage = first(Male_percentage),
    Median_age_years = first(Median_age_years),
    ratio_no_education = first(ratio_no_education),
    ratio_at_least_lower_school = first(ratio_at_least_lower_school),
    ratio_at_least_high_school = first(ratio_at_least_high_school),
    ratio_finished_higher_degree = first(ratio_finished_higher_degree),
    Median_income = first(Median_income),
    k_segments = first(k_segments),
    Number_complaints_per_resident = n() / first(Total_population),
    Duration = median(Duration)
  ) %>%
  rename(Zip = Incident.Zip)

#Predict number of complaints for noise residential complaints with tree
library(caret)
set.seed(1706)
split = createDataPartition(y=analysis_data_noise_residential$Number_complaints_per_resident,p = 0.7,list = F)
train = analysis_data_noise_residential[split,]
test = analysis_data_noise_residential[-split,]

tree_noise_residential = rpart(Number_complaints_per_resident~Density_per_square_mile+Male_percentage+Median_age_years+ratio_no_education+ratio_at_least_lower_school+ratio_at_least_high_school+ratio_finished_higher_degree+Median_income+k_segments,train,minbucket=10)
rpart.plot(tree_noise_residential)
predTree_noise_residential = predict(tree_noise_residential,newdata=test)
rmseTree_complaints_noise = sqrt(mean((predTree_noise_residential - test$Number_complaints_per_resident)^2)); rmseTree_complaints_noise