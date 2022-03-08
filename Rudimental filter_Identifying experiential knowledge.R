#************************************************************************************

#Rudimental filter for identifying experience-based knowledge in Facebook comments
#Prior step: Sentiment analysis using pattern.nl with python 

#************************************************************************************

setwd("C:/Users/...")
fb_s <- read.csv(file = 'FB_sent_sub.csv',na.strings=c("", "NA"),encoding = "UTF-8", sep = ",")

#Avg. sentiment & subjectivity scores
mean(fb_s_neg$Sentiment) 
fb_s_neg <- fb_s[fb_s$Sentiment <="0",] #avg only negative /positive comment scores
mean(fb_s$Subjectivity) 

#Drop first two rows
fb_s = subset(fb_s, select = -c(X,Unnamed..0) )
colnames(fb_s)

# plot sentiment scores
library(ggplot2)
a <- ggplot(fb_s, aes(x = Sentiment))
#histogram
a + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(Sentiment)), 
             linetype = "dashed", size = 0.6)

# Plot Subjectivity Scores
b <- ggplot(fb_s, aes(x = Subjectivity))
#histogram
b + geom_histogram(bins = 30, color = "black", fill = "gray") +
  geom_vline(aes(xintercept = mean(Subjectivity)), 
             linetype = "dashed", size = 0.6)


#*********************************************************************************************************************
# Filter for longer comments with higher sentiment and higher subjectivity scores
#*********************************************************************************************************************

# Get long comments with high sentiment (+/-) AND high subjectivity score
fb_subjective <- fb_s[(fb_s$Sentiment >= 0.25) | (fb_s$Sentiment <= -0.25), ]

fb_subjective <- subset(fb_subjective, Subjectivity >= 0.4)

#Drop rows with very short comments
fb_subjective$message <- as.character(fb_subjective$message)
fb_subjective = fb_subjective[(which(nchar(fb_subjective$message) >= 250)),]

#Drop less important columns
fb_subjective = subset(fb_subjective, select = -c(X.U.FEFF.level, object_type,query_status,query_time, 
                                                  query_type) )

#Sort by Subjectivity decreasing before start reading comments
fb_subjective <-fb_subjective[order(fb_subjective$Subjectivity, decreasing = TRUE),]
head(fb_subjective)


install.packages("writexl")
library("writexl")
write_xlsx(fb_subset, "Fb_long-high-sent_subset.xlsx")

#*********************************************************************************************************************
# Filter for 'mijn' + close personal contact
#*********************************************************************************************************************

#fb_s <- read.csv(file = 'Fb_long-high-sent_subset.csv',na.strings=c("", "NA"),encoding = "UTF-8", sep = ",")

library(dplyr)
#List of family members: moeder, vader, ouders, schoonvader, schoonmoeder, kind, man, vrouw, zoon, dochter, broer, zus, opa, oma, neef, nicht, tante, oom
mijn_connection = dplyr::filter(fb_subjective, grepl(".mijn vader.|.mijn moeder.|.mijn ouders.|.mijn schoonvader.|.mijn schoonmoeder.|.mijn kind|.mijn man.|.mijn vrouw.|.mijn zoon|
                                              .mijn dochter|.mijn broer|.mijn zus|.mijn opa.|.mijn oma.|.mijn neef|.mijn nicht|.mijn tante|.mijn oom", fb_subjective$message , ignore.case = TRUE))

library(writexl)

write_xlsx(mijn_connection, "Fb_1_Mijn_connection.xlsx")
