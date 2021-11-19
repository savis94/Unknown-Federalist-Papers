#load libraries
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(quanteda)

# Creating a predictive model with text mining 


papers <- read.csv('../TextMining/federalist.csv', 
                   stringsAsFactors = F)

dim(papers)
view(papers)

table(papers$Author)

#have 12 unknown, goal is trying to predict the author for the unknown

#need to clean data aka filter out authors that do not really help, we are only looking at Hamilton and Madison for the author. Therefore, filter out the rest.

papers <- papers[which(papers$Author == 'HAMILTON'|
                         papers$Author == 'MADISON'|
                         papers$Author == 'UNKNOWN'),]
dim(papers)
#went from 85 docs to 77

#put them in order by Author to make it easier to pair them together

papers <- papers[order(papers$Author),]

#we now know 
# Text 1-51 = Hamilton
# Text 52-65 = Madison
# Text 66-77 = Unknown

#looking at the data we see that each doc starts with same exact sentence "To the People of the State of New York, '
  #therefore we can get rid of it as it adds no insight
  #instead of stopwords we can remove the sentence by eliminating the number of characters its starts with (which equals 40)

papers$Text <- substring(papers$Text,40)


mycorpus <- corpus(papers$Text)
summary(mycorpus)


#create a frequecy graph

myDfm <- dfm(tokens(mycorpus))
dim(myDfm)

tstat_freq <- textstat_frequency(myDfm)
head(tstat_freq, 20)


#create a text plot (bubble) with word count. Shows that we need to do stopwords, puncutation
textplot_wordcloud(myDfm, max_words = 200)

library(stopwords)

cleanedDfm <- dfm(tokens(mycorpus, 
                         remove_punct = T)) #remove the punctuation

cleanedDfm <- dfm_remove(cleanedDfm, stopwords('en')) #removes stopwords in preset english stopwords

cleanedDfm <- dfm_wordstem(cleanedDfm) #strips words down to their stem (calculates, calculation, calculated, etc; all taken to calcula)

topfeatures(cleanedDfm,30) #looks at the top 30 features (aka words) in data; do this to see if there an any extra words we can remove

# results
# have can, must, 

#create your own stopword list
stopwords1 <- c('may', 'one', 'two', 'can', 'must', 'upon', 'might', 'shall')

cleanedDfm <- dfm_remove(cleanedDfm, stopwords1) #removes the words in the list you created

textplot_wordcloud(cleanedDfm, max_words = 200) #now look at new word cloud after removing the words in 'en' and your list


dim(cleanedDfm)
#still have 4819 words(features) still should try and reduce the dimensionality
  #way to do this is to remove the least frequent words. Words too unique do not help you the same way words that are extremely frequent do not help. 

cleanedDfm <- dfm_trim(cleanedDfm, 
                       min_termfreq = 4, #term must appear in at least 4 times (therefore if it appears 4 times it is kept)
                       min_docfreq = 2) #term must appear in at least 2 docs (therefore if in 2 docs then it is kept)
dim(cleanedDfm) #running the dimensions after trim function                       

# results
# 77 2347
  # 77 different docs and 2347 diff features (words)


doc_dist <- textstat_dist(cleanedDfm)
clust <- hclust(as.dist(doc_dist))
plot(clust, xlab='Distance') 

#looking at the plot they are listed as textxx we sorted them so we know text1-51 = Hamilton, text52-65 = Madison, and text66-77 = Unknown
#we can look at text66-77 and see what they are near other text 



text_similar <- textstat_simil(cleanedDfm,
                               selection = 'text68', #list what document you want to find its similar to 
                               margin ='document', #determines what you are trying to compare
                               method = 'correlation') #gives you a numeric value of how correlated (1 being perfect match)

as.list(text_similar, n=10) #list the top ten results of text_similar
# results
# text66    text62    text63    text56    text52    text65    text60    text19    text11    text76 
# 0.5556120 0.5504849 0.5484339 0.5291173 0.5282585 0.5128294 0.5107572 0.5024262 0.5020311 0.4920556 

#below stopwords were determined below
stopwords2 <- c('state', 'govern', 'power', 'constitut', 'nation', 'peopl')

cleanedDfm <- dfm_remove(cleanedDfm, stopwords2)

term_sim <- textstat_simil(cleanedDfm,
                           selection = 'commerc', #trying to find a specific term (feature). used 'commerc' for stemming purposes
                           margin = 'feature', #looking at each feature  
                           method = 'cosine') #using cosine to measure how similar. Gets value if they are 

as.list(term_sim, n=8) #look at top 8 words/features related to commerc
# results
# traffic      trade intercours   commerci   european   privileg    product   disunion 
# 0.8514140  0.8481307  0.8057795  0.7032858  0.6718657  0.6503049  0.6397442  0.6291341 


#
library(topicmodels)
library(tidytext)

myLda <- LDA(cleanedDfm, k =8, #k is the number of topics you want
             control = list(seed=101)) #control 

myLda #looking at results of your topic model

myLda_td <- tidy(myLda)
myLda_td


library(ggplot2)
library(dplyr)
top_term <- myLda_td %>%
  group_by(topic) %>% #group them by topic (you have 8 topics)
  top_n(8,beta) %>% #get the top 8 results based on their beta for each topic
  ungroup() %>%  #ungroup them
  arrange(topic, -beta) #sort them by descending order by beta

top_term %>%
  mutate(term=reorder(term,beta)) %>%
  ggplot(aes(term, beta, fill=factor(topic))) +
  geom_bar(stat = 'idenity', show.legend = FALSE) + 
  facet_wrap(~topics,scales = 'free') +
  coord_flip()


#results were not ideal as 'power' , 'state', and 'govern' , 'people' appeared everywhere not giving us insight to the topics 
# below code is put into action earlier
      # stopwords2 <- c('state', 'govern', 'power', 'constitut', 'nation', 'peopl')

ap_documents <- tidy(myLda, matrix = 'gamma')
ap_documents

Lda_document <- as.data.frame(myLda@gamma)
View(Lda_document)


modelDfm <- dfm(tokens(mycorpus,
                      remove_punc = T))

modelDfm <- dfm_remove(modelDfm, stopwords('english'))
modelDfm <- dfm_remove(modelDfm,stopwords1)
modelDfm <- dfm_wordstem(modelDfm)

modelDfm <- dfm_trim(modelDfm,
                     min_termfreq = 4,
                     min_docfreq = 2)



dim(modelDfm)


modelDfm_tfidf <- dfm_tfidf(modelDfm)
dim(modelDfm_tfidf)


#still have too many features

modelSvd <- textmodel_lsa(modelDfm_tfidf,
                          nd = 10)
head(modelSvd$docs)
#each text is still a vector, running SVD turn every text (this is the name of the column) into a vector with 10 elements 
  #we are running dimension reduce right (SVD) to run a predictive model. NOT necessary to reduce dim for looking at similarity



modelData <- cbind(papers[,2], #this is getting the second column 'AUTHOR' ; which is our dependent variable 
                   as.data.frame(modelSvd$docs)) #these are the TEXT vectors we created ; are the independent variables

colnames(modelData)[1]<-'Author'
#need to manually split the data because we cannot use the unknown data in our training model

trainData <- subset(modelData,
                    Author == 'HAMILTON' | Author =='MADISON')

testData <- subset(modelData,
                   Author == 'UNKNOWN')

str(trainData)

trainData$Author <- factor(trainData$Author)

regModel <- glm(formula = Author~.,
                family = binomial(link=logit),
                data = trainData)


pred <- predict(regModel,
                newdata = trainData,
                type = 'response')

pred.result <- ifelse(pred>0.5,1,0)
print(table(pred.result,
            trainData$Author))


unknownPred <- predict(regModel,
                       newdata = testData,
                       type = 'response')
unknownPred <- cbind(testData$Author,
                     as.data.frame(unknownPred))



