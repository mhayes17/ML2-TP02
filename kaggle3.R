rm(list=ls())
library(dplyr)
library(tidyr)
library(reshape2)
library(recommenderlab)
options(scipen = 999)

################### TODO: mention we decided to not test on last week ###########

### Exploratory Data Analysis used: https://www.kaggle.com/code/vanguarde/h-m-eda-first-look/notebook
#### UBCF Vs. IBCF: 
######### IBCF : Similarity between items to determine whether user would like/dislike
######### UBCF : Similarity in user consumption patterns and provides content that similar users liked
### RecommenderLab() 
################# TODO: explain how recommenderLab() is generating predictions ################


#################################################
################# LOAD THE DATA #################
#################################################
# IMPORT TIMES:
t <- read.csv('transactions_train.csv') # * 1 min 15 sec, depending on network and background task
a <- read.csv('articles.csv')            # * 3 sec
c <- read.csv('customers.csv')          # * 5 sec

#############################################################################################
#### USING ORIGINAL transactions.csv TAKES TOO LONG SKIP TO GENERATED CSV FOR DATA RANGE ####
#############################################################################################

# start_date = '2020-08-04' 
# end_date = '2020-09-23'
# start_date = '2020-01-01'
# end_date = '2020-09-23'
# start_date = '2020-07-01'
# end_date = '2020-09-23'
start_date = '2020-06-01'
end_date = '2020-09-23'
mask <- (t$t_dat >= start_date) & (t$t_dat <= end_date)
tByDRange <-t[mask,] 
#write.csv(tByDRange, "C:\\Users\\ellie\\Desktop\\KAGGLEML\\tByDRange0609.csv", row.names=FALSE)

##########################################################################
################# START HERE FOR TRANSACTION DATA IMPORT #################
##########################################################################

tByDRange <- read.csv('tByDRange0609.csv') # '2020-06-01'-'2020-09-23'
dim(tByDRange) # 5151470 TOTAL TRANSACTIONS
length(unique(tByDRange$customer_id)) # 599002 CAPTURED CUSTOMER_IDS
length(unique(tByDRange$article_id)) # 45975 CAPTURED ARTICLES
head(tByDRange) 

##################################
##### GENERATE RANDOM SAMPLE #####
##################################

set.seed(123)
sampInd <- sample(nrow(tByDRange), 25000)
customers <- tByDRange[sampInd,]
df <- merge(customers, a, by = 'article_id') 
features = c('product_group_name')
onlyFeatures <- df[,c("customer_id", "article_id",
                      'product_group_name')]


onlyFeatures[,"article_id"] <- as.character(onlyFeatures[,"article_id"])
onlyFeatures[,'product_group_name'] <- as.factor(onlyFeatures[,'product_group_name'])
head(onlyFeatures)
### to be processed in Google Colab ####
write.csv(onlyFeatures, "C:\\Users\\ellie\\Desktop\\KAGGLEML\\dummies_df.csv", row.names=FALSE)

######################################################
##### TOOK FOREVER TO RUN AND GENERATE NORMALIZED FEATURE WEIGHTS BASED ON COLUMN SUMS
##### SO RAN IN COLLAB AND WROTE TO CSV
######################################################

counts <- onlyFeatures%>%
  group_by(customer_id)%>%
  summarise(count = n()) %>%
  as.data.frame()

head(counts)
max(counts$count)
minTransCust <- counts[counts$count >=3,]
dim(minTransCust)
dim(onlyFeatures[onlyFeatures$customer_id %in% minTransCust$customer_id,])

dummies_df <-  onlyFeatures%>% 
  pivot_longer(-c(customer_id, article_id)) %>%
  as.data.frame()
dummies_df$concat <- paste(dummies_df$name, dummies_df$value)
dummies_df$concat <- gsub(" ", "_", dummies_df$concat)
dummies_df$dummy<- 1
dummies_df$name <- NULL
dummies_df$value <- NULL
head(dummies_df)  

str(dummies_df)
dummies_df <- dummies_df%>% 
  group_by(customer_id,article_id) %>%
  mutate(row = row_number()) %>%
  pivot_wider(names_from = concat, values_from = dummy, values_fill = 0) %>%
  as.data.frame() %>%
  select(-row)
print(dummies_df[1,1:5])
dim(dummies_df)


minimum_transaction <- 2
keys <- unique(dummies_df$customer_id)
head(keys)
length(keys)
uniqCID <- dummies_df
temp <- dummies_df %>%
  group_by(customer_id, article_id) %>%
  summarise(ntransaction = n()) %>%
  ungroup()%>%
  as.data.frame()
temp$ntransaction <- temp$ntransaction /10
head(temp)
max(temp$ntransaction)
 temp[temp$customer_id == key,]
l <- list()
customer_ids <- vector()
article_ids <- vector()

for (key in keys) {
  mask <- temp$customer_id == key
  if (length(temp[mask,]$ntransaction >= minimum_transaction) ==1){
    if (temp[mask,]$ntransaction >= minimum_transaction) {
      art <- c(article_ids, uniqCID[uniqCID$customer_id == key,]$article_id)
      foColSum <- uniqCID[uniqCID$customer_id == key,-c(1,2)]
      customer_ids[length(customer_ids) + 1] <- key
      l[[length(l) + 1]] <- colSums(foColSum)
    }
  }
  else{
    res <- temp[mask,]$ntransaction >= minimum_transaction
    for (i in length(res)){
      if (res[i]) {
        art <- c(article_ids, uniqCID[uniqCID$customer_id == key,]$article_id)
        foColSum <- uniqCID[uniqCID$customer_id == key,-c(1,2)]
        customer_ids[length(customer_ids) + 1] <- key
        l[[length(l) + 1]] <- colSums(foColSum)
    }
  }
  }
}

######################################################
######################################################
######################################################

normalized_user_feature <- read.csv("normUF.csv") %>% as.matrix()
item_feature <- read.csv('itemF.csv') %>% as.matrix()
article_ids <- read.csv('aid.csv')
head(article_ids)
customer_ids <- read.csv('cid.csv')
head(customer_ids)
head(normalized_user_feature[,1:5])
head(item_feature[,1:5])
rownames(normalized_user_feature) <- customer_ids$customer_id
rownames(item_feature) <- customer_ids$customer_id
head(item_feature)

scores <- normalized_user_feature %*% t(item_feature)
scores
rownames(scores) <- customer_ids$customer_id
head(scores[,1:5])
dim(scores)
colnames(scores) <-article_ids$article_id 
write.csv(scores, "C:\\Users\\ellie\\Desktop\\KAGGLEML\\scores.csv", row.names=FALSE)


user_feature <- data.frame(matrix(unlist(l), ncol = ncol(dummies_df)-2, byrow = TRUE))
head(user_feature)
colnames(user_feature) <- colnames(dummies_df)[3:ncol(dummies_df)]


########## LOST USER AND ITEM ROW/COLUMN NAMES SO EACH INDEX IS UNIDENTIFIABLE
########## COULD NOT PROCEED :/




