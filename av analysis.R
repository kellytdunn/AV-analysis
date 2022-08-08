setwd("C:/Users/kelly/OneDrive/Documents/Stats with R Certificate/Quarter 3/Homework/Final Project")

library(readr)
library(ggplot2)

#access person-level survey data for AV attitude data
people = read.csv("HTS person level.csv")
people = people[people$survey_year == "2019",] #isolate to 2019 for AV question because it was skipped in 2021


people2 = people

#remove missing values
people2 = people2[people2$av_interest_1 != 'Missing: Skip logic',]

#define order since data is ordinal.
avorder = c("Not at all interested", "Somewhat uninterested", "Don't know", "Neutral", "Somewhat interested", "Very interested")

#convert to factor and define levels
people2$av_interest_1 <- factor(people2$av_interest_1, levels = avorder) 

#explore distribution of av_interest_1
ggplot(people2, aes(x = av_interest_1)) + geom_histogram(stat = "count") +
  labs(title="Histogram", x ="AV interest", y = "Number of People") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

#create df for unsupervised clustering based on AV interest or concern (kmeans cannot handle NAs or non-numeric data). 
av_data = people %>% select(c(av_interest_1, av_interest_2, av_interest_5, av_interest_6, av_interest_7, av_concern_1, av_concern_2, av_concern_3, av_concern_4, av_concern_5))


summary(av_data)


#recode to numeric ordinal data. Assume 'don't know' and 'neutral' are basically the same.
av_data$av_interest_1 = dplyr::recode(av_data$av_interest_1,
                                      `Very interested` =1, `Somewhat interested`=2, `Neutral`=3, `Somewhat uninterested` =4, 
                                      `Not at all interested`=5, `Don't know` = 3, `Missing: Skip logic` = 99)
av_data$av_interest_2 = dplyr::recode(av_data$av_interest_2,
                                      `Very interested` =1, `Somewhat interested`=2, `Neutral`=3, `Somewhat uninterested` =4, 
                                      `Not at all interested`=5, `Don't know` = 3, `Missing: Skip logic` = 99)

av_data$av_interest_5 = dplyr::recode(av_data$av_interest_5,
                                      `Very interested` =1, `Somewhat interested`=2, `Neutral`=3, `Somewhat uninterested` =4, 
                                      `Not at all interested`=5, `Don't know` = 3, `Missing: Skip logic` = 99, `Missing: Non-response` = 99)

av_data$av_interest_6 = dplyr::recode(av_data$av_interest_6,
                                      `Very interested` =1, `Somewhat interested`=2, `Neutral`=3, `Somewhat uninterested` =4, 
                                      `Not at all interested`=5, `Don't know` = 3, `Missing: Skip logic` = 99, `Missing: Non-response` = 99)

av_data$av_interest_7 = dplyr::recode(av_data$av_interest_7,
                                      `Very interested` =1, `Somewhat interested`=2, `Neutral`=3, `Somewhat uninterested` =4, 
                                      `Not at all interested`=5, `Don't know` = 3, `Missing: Skip logic` = 99, `Missing: Non-response` = 99)

av_data$av_concern_1 = dplyr::recode(av_data$av_concern_1,
                                     `Very concerned` =1, `Somewhat concerned`=2, `Neutral`=3, `Somewhat unconcerned` =4, 
                                     `Not at all concerned`=5, `Don't know` = 3, `Missing: Skip logic` = 99, `Missing: Non-response` = 99)
av_data$av_concern_2 = dplyr::recode(av_data$av_concern_2,
                                     `Very concerned` =1, `Somewhat concerned`=2, `Neutral`=3, `Somewhat unconcerned` =4, 
                                     `Not at all concerned`=5, `Don't know` = 3, `Missing: Skip logic` = 99, `Missing: Non-response` = 99)
av_data$av_concern_3 = dplyr::recode(av_data$av_concern_3,
                                     `Very concerned` =1, `Somewhat concerned`=2, `Neutral`=3, `Somewhat unconcerned` =4, 
                                     `Not at all concerned`=5, `Don't know` = 3, `Missing: Skip logic` = 99, `Missing: Non-response` = 99)
av_data$av_concern_4 = dplyr::recode(av_data$av_concern_4,
                                     `Very concerned` =1, `Somewhat concerned`=2, `Neutral`=3, `Somewhat unconcerned` =4, 
                                     `Not at all concerned`=5, `Don't know` = 3, `Missing: Skip logic` = 99, `Missing: Non-response` = 99)
av_data$av_concern_5 = dplyr::recode(av_data$av_concern_5,
                                     `Very concerned` =1, `Somewhat concerned`=2, `Neutral`=3, `Somewhat unconcerned` =4, 
                                     `Not at all concerned`=5, `Don't know` = 3, `Missing: Skip logic` = 99, `Missing: Non-response` = 99)

av_data = av_data[av_data$av_concern_1 != 99,] #remove 99s

# Unsupervised Clustering: AV attitudes

data1_dist = dist(av_data, method='euclidian') #only works with numeric data

clusters = hclust(data1_dist)

head(clusters)

#make margins smaller to fit plot
par(mar = c(1, 1, 1, 1))
plot(clusters)

av_data$cluster = cutree(clusters, 4)

table(av_data$cluster) 

clust1 = av_data %>% filter(av_data$cluster == 1) 
clust2 = av_data %>% filter(av_data$cluster == 2)
clust3 = av_data %>% filter(av_data$cluster == 3)
clust4 = av_data %>% filter(av_data$cluster == 4)
summary(clust1) #not interested and very concerned (Fearful)
summary(clust2) #very interested and not very concerned (enthusiastic)
summary(clust3) #somewhat interested and somewhat concerned (cautious)
summary(clust4) #not interested and not concerned (Apathetic)

#calculate each cluster's share of the whole
(nrow(clust1)/nrow(av_data))*100
(nrow(clust2)/nrow(av_data))*100
(nrow(clust3)/nrow(av_data))*100
(nrow(clust4)/nrow(av_data))*100
