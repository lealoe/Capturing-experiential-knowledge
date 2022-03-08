#####################################################################
## 
## Topic model analysis: Facebook subset 01.03.-08.06.21
## 17.09.2021
#####################################################################

setwd("C:/Users/leale/Desktop/PhD/3-Coding")

#Read data
install.packages("readxl")
library("readxl")

data_raw <- read_excel("Fb_03-06_21_LhSS.xlsx")

##########################################################################
# Data preparation
##########################################################################

#Drop missing reports and empty comment text

#Remove doubles
# automatic language detection 
install.packages("cld2")
library(cld2)
options(max.print=10000)
string <- (data_raw$message)
sum(detect_language(string))
detect_language(string)
summary(detect_language(string) == "nl")

#Keep only Dutch ones
nl<- which(detect_language(string) == "nl")
data_nl <- data_raw[c(nl), ]

###################################################

library(stm)
library(quanteda)

#Drop less important columns
data_nl = subset(data_nl, select = -c(parent_id, object_id) )

#Format created time as 'date'
class(data_nl$created_time)
data_nl$created_time<-strptime(data_nl$created_time,format="%Y-%m-%d") #defining what is the original format of your date
data_nl$created_time<- as.Date(data_nl$created_time,format="%Y-%m-%d")

#Plot distribution of comments over time
table(data_nl$created_time)
Data2 <- table(data_nl$created_time)
plot(Data2, type = "l")  


#Remove emojis(format: <U+ ... >)
data_nl$message <- gsub("<U[^>]+>", "",data_nl$message)


## Create a text corpus

quantedacorpus <- corpus(data_nl,
                  text_field = "message",
                  meta = list("created_time", "comment_count", "like_count","love_count", "haha_count",
                              "wow_count", "sad_count", "angry_count", "Sentiment", "Subjectivity"),
                  unique_docnames = TRUE)

view(quantedacorpus)

#Prepare data
stopwords('dutch')
#more extensive list of dutch stopwords:
mystopwords <- read.table("stop_words_dutch.txt", header = TRUE)
class(mystopwords)
stop_vec = as.vector(mystopwords$Custom_stopwords)
class(stop_vec)


dfm_data_nl <- dfm(quantedacorpus, 
                  tolower = TRUE, 
                  remove_numbers = TRUE, 
                  remove_punct = TRUE, 
                  remove_url = TRUE, 
                  remove_symbols = TRUE,
                  remove = c(stop_vec, stopwords('dutch')),
                  stem = TRUE)

#Remove extremely frequent and rare terms
dfm.trim <- dfm_trim(dfm_data_nl, min_docfreq = 0.0025, max_docfreq = 0.95, 
                     docfreq_type = "prop", verbose = TRUE)


#Get most frequent terms
topfeatures(
  dfm.trim,
  n = 50,
  decreasing = TRUE,
  scheme = c("count", "docfreq"),
  groups = NULL
)


#Read sample comments
options(max.print=1000)
print(as.character(quantedacorpus[30]))
print(as.character(quantedacorpus[152]))
print(as.character(quantedacorpus[141]))
print(as.character(quantedacorpus[643]))
print(as.character(quantedacorpus[142]))
print(as.character(quantedacorpus[164]))

#Process the data for analysis.

dfm2stm <- convert(dfm.trim, to = "stm")
docs<-dfm2stm$documents
vocab<-dfm2stm$vocab
meta <-dfm2stm$meta

##########################################################################
# Determine no. of topics K
##########################################################################

dfm2stm$meta$created_time <- as.Date(dfm2stm$meta$created_time, format = "%y/%m/%d")

kResult <- searchK(docs, vocab, K = c(10,13,15,17), init.type="Spectral", prevalence =~ created_time, data=meta) 

# Plot diagnostic results
plot(kResult)
# Semantic coherence-exclusivity plot using function plot()
plot(kResult$results$semcoh, kResult$results$exclus, xlab = "Semantic Coherence", ylab = "Exclusivity")
# Add labels to semantic coherence-exclusivity plot using function text()
text(kResult$results$semcoh, kResult$results$exclus, labels = paste("K", kResult$results$K), pos = 1)
# Semantic coherence-exclusivity table
knitr::kable(kResult$results)

#Run models with different number of K
model5 <- stm(docs, vocab, K=5, init.type = "Spectral", prevalence =~ created_time, data=meta)
model10 <- stm(docs, vocab, K=10, init.type = "Spectral", prevalence =~ created_time, data=meta)
model13 <- stm(docs, vocab, K=13, init.type = "Spectral", prevalence =~ created_time, data=meta)
model14 <- stm(docs, vocab, K=14, init.type = "Spectral", prevalence =~ created_time, data=meta)
model15 <- stm(docs, vocab, K=15, init.type = "Spectral", prevalence =~ created_time, data=meta)
model17 <- stm(docs, vocab, K=17, init.type = "Spectral", prevalence =~ created_time, data=meta)
model20<-stm(docs,vocab, K=20, init.type = "Spectral", prevalence =~ created_time, data=meta)


#Plot semantic coherence and exclusivity for each model
M5ExSem <-as.data.frame(cbind(c(1:5), exclusivity(model5), semanticCoherence(model=model5, docs), "Mod5"))
M10ExSem<-as.data.frame(cbind(c(1:10),exclusivity(model10), semanticCoherence(model=model10, docs), "Mod10"))
M13ExSem<-as.data.frame(cbind(c(1:13),exclusivity(model13), semanticCoherence(model=model13, docs), "Mod13"))
M14ExSem<-as.data.frame(cbind(c(1:14),exclusivity(model14), semanticCoherence(model=model14, docs), "Mod14"))
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(model15), semanticCoherence(model=model15, docs), "Mod15"))
M17ExSem<-as.data.frame(cbind(c(1:17),exclusivity(model17), semanticCoherence(model=model17, docs), "Mod17"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(model20), semanticCoherence(model=model20, docs), "Mod20"))


ModsExSem<-rbind(M13ExSem, M14ExSem, M15ExSem)
colnames(ModsExSem)<-c("K","Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

options(repr.plot.width=7, repr.plot.height=7, repr.plot.res=100)


install.packages("ggplot2")
library(ggplot2)


plotexcoer<-ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+geom_point(size = 2, alpha = 0.7) + 
  geom_text(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence")

plotexcoer


##################################################################
###                  Structural topic model                    ###
##################################################################
library(stm)
library(tidyverse)

n.topics <- 13
mod <- stm(docs, vocab, K= n.topics, init.type="Spectral")
modell.stm.labels <- labelTopics(mod, 1:n.topics)
par(fig=c(0.05,0.95,0.05,0.95))
mod
#Distribution and top 5 words per topic:
plot.STM(mod, "summary", n=13)

#Table with topic proportions
td_gamma <- tidy(mod, matrix = "gamma",
                 document_names = rownames(docs))
td_gamma

td_beta <- tidy(mod)
td_beta

#Overview over the topics, prevalences and top 7 terms
install.packages("purrr")
library(purrr)
install.packages("knitr")
library(knitr)
install.packages("tidyverse")
install.packages("tidytext")
library(tidytext)
library(tidyverse)
library(ggthemes)
library(tidyr)

top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(12, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest()

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma))


gamma_terms %>%
  select(topic, gamma, terms) %>%
  kable(digits = 3, 
        col.names = c("Topic", "Expected topic proportion", "Top 7 terms"))

####################################################################
#####             Explore STM results                          #####
####################################################################

#Show STM results in data frame:
as.data.frame(t(labelTopics(mod, n = 13)$prob))

#Topic shares on the whole corpus
plot(mod, type = "summary", text.cex = 0.9, main = "Topic shares on the corpus as a whole", xlab = "estimated share of topics")

#Topic shares within the documents 
plot(mod, type = "hist", topics = sample(1:12, size = 9), main = "histogram of the topic shares within the documents")

#Topic terms
plot(mod, type = "labels", topics = c(1), main = "Topic terms")

#Plot topic contrast
plot(mod, type = "perspectives", topics = c(1,2), main = "Topic contrasts")
plot.STM(mod, type = 'perspective', topics = c(4,5))

#Plot highest word probabilities for each topic
AP_topics <- tidy(mod)
ap_top_terms <- 
  AP_topics %>%
  group_by(topic) %>%
  top_n(13, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL, y = expression(beta),
       title = "Highest word probabilities for each topic")


#Beta values for ONE topic
library(ggplot2)
betaT1<- AP_topics %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%filter(topic=="Topic 1")

betaplotT1<-ggplot(betaT1[betaT1$beta>0.01,], aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "Identity")+coord_flip()+labs(x ="Terms", y = expression(beta),
                                                                                  title = "Word probabilities for Topic 1")#plot word probabilities higher than 0.003 for topic 1
betaplotT1

#Complete list of top 10 words per topics, Highest Prob, FREX, lift
?labelTopics
labelTopics(mod, topics=c(13), n=10)
#Top 10 FREX words per topics 5-6-9:
plot.STM(mod, type = 'labels', n = 8, text.cex = 0.8, 
         width = 100, topics = 4, labeltype = 'frex')

#Find passages of text that are assigned to topic
thoughts13 <- findThoughts(mod, texts = quantedacorpus, 
                          n = 20, topics = 13)

print(thoughts13["docs"], max_ndoc = 25, max_nchar = 450)

par(mfrow = c(1, 2), mar = c(0.5, 0.5, 1, 0.5))
plotQuote(thoughts3, width = 30, maxwidth=120, text.cex=1.25, main = "Topic 3")
options(max.print=1000)
print(as.character(quantedacorpus[452]))


######################################
