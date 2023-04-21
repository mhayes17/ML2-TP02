rm(list=ls())
library(dplyr)
library(tidyr)
library(reshape2)
library(recosystem)
options(scipen = 999)

### Exploratory Data Analysis used: https://www.kaggle.com/code/vanguarde/h-m-eda-first-look/notebook
#### UBCF Vs. IBCF: 
######### IBCF : Similarity between items to determine whether user would like/dislike
######### UBCF : Similarity in user consumption patterns and provides content that similar users liked
### RecommenderLab() k
################# TODO: explain how recommenderLab() is generating predictions ################


#################################################
################# LOAD THE DATA #################
#################################################
# IMPORT TIMES:
t <- read.csv('transactions_train.csv') # * 1 min 15 sec
a <- read.csv('articles.csv')            # * 3 sec
c <- read.csv('customers.csv')          # * 5 sec

a <- a %>% 
  select(c(article_id, prod_name, product_type_name, index_group_name))
head(a[1,])

#### USING ORIGINAL FILE, TAKES TOO LONG SKIP TO GENERATED CSV FOR DATA RANGE ####
#start_date = '2020-08-04' 
#end_date = '2020-09-23'
# start_date = '2020-01-01'
# end_date = '2020-09-23'
start_date = '2020-07-01'
end_date = '2020-09-23'
mask <- (t$t_dat >= start_date) & (t$t_dat <= end_date)
tByDRange <-t[mask,] 
dim(tByDRange)
head(tByDRange)
#write.csv(tByDRange, "C:\\Users\\ellie\\Desktop\\KAGGLEML\\tByDRange0709.csv", row.names=FALSE)

##########################################################################
################# START HERE FOR TRANSACTION DATA IMPORT #################
##########################################################################

tByDRange <- read.csv('tByDRange.csv') # '2020-09-15'-'2020-09-23'
dim(tByDRange) # 266364 TOTAL TRANSACTIONS
head(tByDRange)
length(unique(tByDRange$customer_id)) # 75481 unique customer_ids 
length(unique(tByDRange$article_id)) # 18684 items 

############################################
##### GET TOP 200 (NEEDS IMPROVEMENT) #####
############################################
# this just gets the most popular items for the date range we have selected
itemPopByTCount <- tByDRange %>%
  group_by(article_id )%>%
  summarise(count = n()) %>%
  arrange(desc(count))
topItems <-itemPopByTCount[1:200,]
head(topItems)
print("Top 10 items in transaction data subset")
print((a[a$article_id %in% topItems$article_id,])[1:10,])

#######################################################
##### SAVE ONLY TRANSACTIONS CONTAINING TOP ITEMS #####
#######################################################

mask2 <- (tByDRange$article_id %in% topItems$article_id) 
data <- tByDRange[mask2,]  %>%
  group_by(customer_id, article_id) %>%
  summarise(prodCount = n()) %>% # kinda like repurchase count
  ungroup()

topUsers <- data  %>%
  group_by(customer_id) %>%
  summarise(tCount = n()) %>%
  arrange(desc(tCount))
head(topUsers) # Most active user made 13 transactions

# add prices back into data so we can make additional features (for user similarity)
prices <- tByDRange[mask2,] %>% select(c(article_id, price))
prices <- prices[!duplicated(prices$article_id), ]
data <- merge(data, prices, by = 'article_id')
# add scaled purchase counts column
df_min <- 0
df_max <- max(data$prodCount) # max num of prods purchased was 30
data$scaledPurchFreq <- (data$prodCount - df_min)/(df_max -df_min)
dim(data) # 47342 transactions
length(unique(data$customer_id)) # 29857 unique customers
length(unique(data$article_id)) # 200 unique items
head(data)

#######################################
##### GENERATING USER-ITEM MATRIX #####
#######################################

head(data)
matrix_data <- data %>% 
  select(-c(prodCount, price))
head(matrix_data)

df_matrix <- matrix_data %>% 
  pivot_wider(names_from = article_id,values_from = scaledPurchFreq, values_fill = 0) %>%
  as.data.frame()
# does not take into account whether user didn't buy because of dislike, or not seeing item
row.names(df_matrix) <- df_matrix$customer_id
df_matrix$customer_id <-NULL
print(df_matrix[1,1:5])
### Calculating sparcity ###
# sparcity = percentage of unpopulated/unrepresented cells
sprintf("Sparcity: %1.1f%%", (sum(df_matrix == 0)/(ncol(df_matrix) * nrow(df_matrix))*100)) 

########################################
##### SVD (IF WE HAD RATINGS DATA) #####
########################################

#  Singular value decomposition decomposes three matrices 
# and the latent factors show the characteristics of the items
# discover the relationship between items

# treating repurchase as indication of like/positive reaction
dfSvd <- svd(as.matrix(df_matrix))
# A = U * d * V^T

cosine_similarity <- function(v, u) {
  (v %*%  u) / (sqrt(sum(v^2)) * sqrt(sum(u^2)))
}

print(dfSvd$u[1,])
u <- dfSvd$u
d <-(dfSvd$d)
v <- dfSvd$v
row.names(u) <- rownames(df_matrix)
rownames(v) <- colnames(df_matrix)

######## predictions for first user in df_matrix #######
buy <- df_matrix[1,df_matrix[1,] != 0]
head(v)
v[colnames(buy[1]),]
similaritiesv1 <- t(cosine_similarity(v[colnames(buy[1]),], t(v)))
similaritiesv1

#predictions <- colSums(u['9fea004114a9e3c533a47ec41e0be19d2bba238d7318008b70c0921cb6d4bcfe',] *d *t(v))
#predictions <- sort(predictions, decreasing = TRUE)
length(predictions)
names(predictions)
alreadyPurch <- df_matrix['9fea004114a9e3c533a47ec41e0be19d2bba238d7318008b70c0921cb6d4bcfe',] != 0
predictions[!(names(predictions) %in% colnames(alreadyPurch)[which(alreadyPurch)])][1:10]

# We could also use this to calculate the similarity between items and users


#####################################################################################
############################# ITEM BASED RECOMMENDATION #############################
#####################################################################################

t2020 <- read.csv('tByDRange0709.csv') # '2020-07-01'-'2020-09-23'
dim(t2020) # 10980132 TOTAL TRANSACTIONS
length(unique(t2020$customer_id)) # 489553 unique customer_ids 
length(unique(t2020$article_id)) # 40772 items 

# gets the most popular items for the date range we have selected
itemPopByTCount <- t2020 %>%
  group_by(article_id )%>%
  summarise(count = n()) %>%
  arrange(desc(count))
topItems <-itemPopByTCount[1:1000,]
print("Top 10 items JULY - SEPT 2020 transactions")
print((a[a$article_id %in% topItems$article_id,])[1:10,])

#######################################################
##### SAVE ONLY TRANSACTIONS CONTAINING TOP ITEMS #####
#######################################################

mask2 <- (t2020$article_id %in% topItems$article_id) 
data <- t2020[mask2,]  %>%
  group_by(customer_id, article_id) %>%
  summarise(prodCount = n()) %>% # kinda like repurchase count
  ungroup()

# add prices back into data so we can make additional features (for user similarity)
prices <- t2020[mask2,] %>% select(c(article_id, price))
prices <- prices[!duplicated(prices$article_id), ]
data <- merge(data, prices, by = 'article_id')
# add scaled purchase counts column
df_min <- 0
df_max <- max(data$prodCount) # max num of prods purchased was 80
data$scaledPurchFreq <- (data$prodCount - df_min)/(df_max -df_min)
dim(data) # 47342 transactions
length(unique(data$customer_id)) # 338246 unique customers
length(unique(data$article_id)) # 1000 unique items
head(data)

###############################################
##### GENERATING ADDITIONAL USER FEATURES #####
###############################################

mean_transactions <- data %>%
  group_by(customer_id) %>%
  summarize(mean_t_price = mean(price))
head(mean_transactions)

max_transactions <- data %>%
  group_by(customer_id) %>%
  summarize(max_t_price = max(price))
head(max_transactions)

min_transactions <- data %>%
  group_by(customer_id) %>%
  summarize(min_t_price = min(price))
head(min_transactions)

median_transactions <- data %>%
  group_by(customer_id) %>%
  summarize(med_t_price = median(price))
head(median_transactions)

sum_transactions <- data %>%
  group_by(customer_id) %>%
  summarize(sum_t_price = sum(price))
head(sum_transactions)

max_min_transactions <- data %>%
  group_by(customer_id) %>%
  summarize(max_minus_min_price = max(price) - min(price))
head(max_min_transactions)

##########################################################
##### CUSTOMER SIMILARITY BASED ON TRANSACTION PRICE #####
##########################################################
tempy <- merge(mean_transactions, max_transactions, by= "customer_id")
tempy2 <- merge(min_transactions, median_transactions, by ="customer_id")
tempy3 <- merge(sum_transactions, max_min_transactions, by = 'customer_id')
calcCustSim <- merge(merge(tempy, tempy2, by='customer_id'), tempy3, by='customer_id')
rownames(calcCustSim) <- calcCustSim$customer_id
calcCustSim$customer_id <- NULL
dim(calcCustSim) # 338246 user rows
calcCustSim <- as.matrix(calcCustSim)
head(calcCustSim[1,])

head(data)
matrix_data <- data %>% 
  select(-c(prodCount, price))
matrix_data <- matrix_data[,c("customer_id","article_id","scaledPurchFreq")]
head(matrix_data)


matrix_data$customer_id <- as.factor(matrix_data$customer_id)
matrix_data$article_id <- as.factor(matrix_data$article_id)
matrix_data$scaledPurchFreq <- as.numeric(matrix_data$scaledPurchFreq)

min(matrix_data$scaledPurchFreq) # 0.0125 BASELINE FOR "GOOD RATING"
set.seed(123)
user_item <- as(matrix_data, 'realRatingMatrix') 
head(sort(rowCounts(user_item),decreasing = TRUE)) # TOP USER MADE 52 TRANSACTIONS
user_item <- user_item[rowCounts(user_item)> 5,]
user_item

testUsers <- user_item[301:303]
testids<-calcCustSim[(rownames(testUsers)),] %>% as.matrix()
mask3 <- (rownames(calcCustSim) %in% unique(rownames(user_item))) & !(rownames(calcCustSim) %in% rownames(testUsers))
newSimMat <- calcCustSim[mask3,] %>% as.matrix()
newSimMat

test1 <- ((newSimMat %*% as.matrix(testids[1,])) /sqrt(rowSums(newSimMat^2) * sum(testids[1,]^2))) %>% as.data.frame() %>% arrange(desc(V1)) 
test1 <- rownames(test1)[1:100]
test2 <- ((newSimMat %*% testids[2,]) /sqrt(rowSums(newSimMat^2) * sum(testids[2,]^2))) %>% as.data.frame() %>% arrange(desc(V1)) 
test2 <- rownames(test2)[1:100]
test3 <- ((newSimMat %*% testids[3,]) /sqrt(rowSums(newSimMat^2) * sum(testids[3,]^2))) %>% as.data.frame() %>% arrange(desc(V1)) 
test3 <- rownames(test3)[1:100]
testcomb = append(test1,test2)
testcomb<- append(testcomb,test3)
length(testcomb)

# Train a user-based collaborative filtering recommender using a small training set.

filteredData <- user_item[rownames(user_item) %in% testcomb]
e <- evaluationScheme(filteredData, method="split", train=0.9,
                       k=1, given=-5)
# given = -5 means that for the test users 'all but 5' randomly selected item is withheld for evaluation
e
# ## user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")

## predictions for the test data using known ratings (see given above,all but 5)
p <- predict(r, getData(e, "known"), type="ratings")
p
 
## compute error metrics averaged per user and then averaged over all
## recommendations
calcPredictionAccuracy(p, getData(e, "unknown"))
head(calcPredictionAccuracy(p, getData(e, "unknown"), byUser=TRUE))


rec <- Recommender(filteredData, method = "UBCF") # getData(e, "train")
rec
## Recommender of type 'UBCF' for 'realRatingMatrix' 
## learned using 300 users.
#  Top-N recommendations for new users (users 301, 302, 303).

## Recommendations as 'topNList' with n = 5 for 3 users.
pre <- predict(rec, testUsers, n = 5)
pre
pre <- as(pre, "list")
for (recom in pre){
  print(a[a$article_id %in% recom,] %>% select(-c(article_id)))
  cat('\n')
}

# 10-fold cross-validation scheme to compare the top-N with several algorithms. 
# articles with  with 0.005 or more normalized purchFreq are considered a good recommendation. 
# plot true negative vs. true positive rate for top-N lists of different lengths.

scheme <- evaluationScheme(user_item[1:500], method = "cross-validation", k = 10, given = -5,
                           goodRating = 0.005)
scheme
## Evaluation scheme using all-but-5 items
## Method: 'cross-validation' with 10 run(s).
## Good ratings: >=4.000000
## Data set: 358 x 1664 rating matrix of class 'realRatingMatrix' with 73610 ratings.
algorithms <- list('random items' = list(name = "RANDOM", param = NULL), 
                   'popular items' = list(name = "POPULAR", param = NULL), 
                   'user-based CF (cosine similarity)' = list(name = "UBCF", param = list(nn = 3, method="cosine", sample=F)),
                   'user-based CF (pearson similarity)' = list(name = "UBCF", param = list(nn = 3, method="pearson", sample=F)),
                   'user-based CF (jaccard similarity)' = list(name = "UBCF", param = list(nn = 3, method="jaccard", sample=F)),
                   'item-based CF' = list(name = "IBCF", param = list(k = 100)))

# Jaccard Similarity: 
#   Similarity is based on the number of users which have rated item A and B divided by the number of users who have rated either A or B
# It is typically used where we don’t have a numeric rating but just a boolean value like a product being bought or an add being clicked
# Cosine Similarity:
#   Similarity is the cosine of the angle between the 2 vectors of the item vectors of A and B
# Closer the vectors, smaller will be the angle and larger the cosine
# Pearson Similarity
# Similarity is the pearson coefficient between the two vectors.
# Lets create a model based on item similarity as follow:


results <- evaluate(scheme, algorithms, type = "topNList", n = c(1, 3, 5, 10), progress = FALSE)

plot(results, annotate = 2, legend = "topleft")

#############################
#### binaryRatingsMatrix ####
#############################

matrix_data2 <- data %>% 
   select(-c(prodCount, price, scaledPurchFreq))
matrix_data2$dummy <- 1
matrix_data2 <- matrix_data2[,c("customer_id","article_id","dummy")]
head(matrix_data2)

matrix_data2$customer_id <- as.factor(matrix_data$customer_id)
matrix_data2$article_id <- as.factor(matrix_data$article_id)
matrix_data2$dummy <- as.numeric(matrix_data$scaledPurchFreq)

user_item <- as(matrix_data2, 'binaryRatingMatrix') 
user_item <- user_item[rowCounts(user_item)> 5,]
user_item

filteredData <- user_item[rownames(user_item) %in% testcomb]
e <- evaluationScheme(filteredData, method="split", train=0.9,
                      k=1, given=-5)
# ## user-based CF recommender using training data
r <- Recommender(getData(e, "train"), "UBCF")

p <- predict(r, getData(e, "known"), type="topNList")
p
calcPredictionAccuracy(p, getData(e, "unknown"), given=4, goodRating=0.005)



######################################################
##### switching to binaryRatingMatrix For TOPN #######
######################################################
scheme <- evaluationScheme(user_item[1:500], method = "cross-validation", k = 10, given = -5,
                           goodRating = 0.005)
scheme
## Evaluation scheme using all-but-5 items
## Method: 'cross-validation' with 10 run(s).
## Good ratings: >=4.000000
## Data set: 358 x 1664 rating matrix of class 'realRatingMatrix' with 73610 ratings.
algorithms <- list('random items' = list(name = "RANDOM", param = NULL), 
                   'popular items' = list(name = "POPULAR", param = NULL), 
                   'user-based CF (cosine similarity)' = list(name = "UBCF", param = list(nn = 3, method="cosine", sample=F)),
                   'user-based CF (pearson similarity)' = list(name = "UBCF", param = list(nn = 3, method="pearson", sample=F)),
                   'user-based CF (jaccard similarity)' = list(name = "UBCF", param = list(nn = 3, method="jaccard", sample=F)),
                   'item-based CF' = list(name = "IBCF", param = list(k = 100)))

results2 <- evaluate(scheme, algorithms, type = "topNList", n = c(1, 3, 5, 10), progress = FALSE)
getConfusionMatrix(results2$`item-based CF`)[[1]]
# Recall:
#   What ratio of items that a user likes were actually recommended.
# If a user likes say 5 items and the recommendation decided to show 3 of them, then the recall is 0.6
# Precision
# Out of all the recommended items, how many the user actually liked?
#   If 5 items were recommended to the user out of which he liked say 4 of them, then precision is 0.8

plot(results2, annotate = 2, legend = "topleft")
results2$`item-based CF`

