### preprocessing the data
yelp <- read.csv("Yelp_train.csv")
yelp_test <- read.csv("Yelp_test.csv")
yelp_validate <- read.csv("Yelp_validate.csv")
yelp_out <- rbind(yelp_test,yelp_validate)
yelp1_out <- rbind(yelp_test,yelp_validate)
yelp1_out$text <- as.character(yelp1_out$text)
yelp$nword <- log(yelp$nword)
yelp$nchar <- log(yelp$nchar)

yelp_out$nword <- log(yelp_out$nword)
yelp_out$nchar <- log(yelp_out$nchar)
yelp$text <- as.character(yelp$text)

yelp_out$text <- as.character(yelp_out$text)
yelp$categories <- as.character(yelp$categories)

yelp_out$categories <- as.character(yelp_out$categories)
yelp <- yelp[,-1]


yelp_out <- yelp_out[,-c(1,2)]  #the yelp_out has another row names

### delete useless columns
dat <- yelp[,-c(1,3:5,9,13)]

yelp_out <- yelp_out[,-c(1:3,7,11)]

library(dplyr)
library(tidytext)
library(tm)
library(stringr)


#load two dictionaries, afinn and bing 

library(tidytext)
library(textdata)
afinn = get_sentiments("afinn")
bing = get_sentiments("bing")
afinn_word = afinn[which(afinn$value<=-3|afinn$value>=3),1]$word

full_names = colnames(new_X)
bing = bing$word

### count the frequency of words in bing and choose those with more than 50 times.
bing_count = setNames(data.frame(matrix(ncol = length(bing), nrow = 1)), bing)
for (i in 1:length(bing)){
  if(bing[i] %in% full_names){
    bing_count[i] = sum(new_X[,bing[i]])
  } else {
    bing_count[i] = 0
  }
}
bing_sort = sort(bing_count, decreasing = TRUE)
bing_50 = colnames(bing_sort[which(bing_sort>50)])

### count the frequency of words in afinn and choose those with more than 10 times.
afinn_count = setNames(data.frame(matrix(ncol = length(afinn_word), nrow = 1)), afinn_word)
for (i in 1:length(afinn_word)){
  if(afinn_word[i] %in% full_names){
    afinn_count[i] = sum(new_X[,afinn_word[i]])
  } else {
    afinn_count[i] = 0
  }
}
afinn_sort = sort(afinn_count, decreasing = TRUE)
afinn_10 = colnames(afinn_sort[which(afinn_sort>=10)])

### delete the duplicate words in the whole word_dictionary we generated
word_dict = c(bing_50, afinn_10)
word_dict = unique(word_dict)
X_dict = new_X[,word_dict]

### combine these words with other predictors
colnames(dat)[which(names(dat) == "useful")] <- "USEFUL_1"
colnames(dat)[which(names(dat) == "cool")] <- "COOL_1"
colnames(dat)[which(names(dat) == "funny")] <- "FUNNY_1"


dat_names = colnames(dat[,-c(1:7,59)])
all_dict = c(word_dict, dat_names)
all_dict = unique(all_dict)

### a function which could add predictors to the original data set
add_predictor = function(text_data,data, new_pred, col_name){
  
  if(!(col_name %in% colnames(data))){
    data$new_col = rep(0, nrow(data))
    colnames(data)[ncol(data)] = col_name
  }
  else {
    return(data)
  }
  for(i in 1:nrow(data)){
    data[i, col_name] = str_count(tolower(text_data$text[i]), new_pred)
  }
  return(data)
}

### add some predictors which we think are useful
all_train = new_X[,all_dict]
all_train = cbind(dat[,c(2:7,59)],all_train)
all_train = add_predictor(yelp,all_train, "omg", "omg")
all_train = add_predictor(yelp,all_train, ":\\)", "smile_face")
all_train = add_predictor(yelp,all_train, ";\\)", "wink_face")
all_train = add_predictor(yelp,all_train, "\\$", "dollar_sign")
all_train = add_predictor(yelp,all_train, "1", "1")
all_train = add_predictor(yelp,all_train, "2", "2")
all_train = add_predictor(yelp,all_train, "3", "3")
all_train = add_predictor(yelp,all_train, "4", "4")
all_train = add_predictor(yelp,all_train, "5", "5")
all_train = add_predictor(yelp,all_train, "minus", "minus")
all_train = add_predictor(yelp,all_train, "!", "exclam_mark")
all_train = add_predictor(yelp,all_train, "\\?", "question_mark")
all_train = add_predictor(yelp,all_train, "\\+", "add_mark")
all_train = add_predictor(yelp,all_train, "\\-", "minus_mark")

#generate predictors in the out data


colnames(yelp_out)[which(names(yelp_out) == "useful")] <- "USEFUL_1"
colnames(yelp_out)[which(names(yelp_out) == "cool")] <- "COOL_1"
colnames(yelp_out)[which(names(yelp_out) == "funny")] <- "FUNNY_1"

ori_col = colnames(dat[,-c(1:7,59)])
extra = c(ori_col, word_dict)
extra = unique(extra)

for (col_name in extra)
{
  
  if(!(col_name %in% colnames(yelp_out)))
  {
    for(i in 1:nrow(yelp_out)){
      yelp_out$new_col = rep(0, nrow(yelp_out))
      colnames(yelp_out)[ncol(yelp_out)] = col_name
      yelp_out[i, col_name] = str_count(tolower(yelp1_out$text[i]), col_name)
    }
  }
}


### add the same words in the out data
yelp_out = add_predictor(yelp1_out,yelp_out, "omg", "omg")
yelp_out = add_predictor(yelp1_out,yelp_out, ":\\)", "smile_face")
yelp_out = add_predictor(yelp1_out,yelp_out, ";\\)", "wink_face")
yelp_out = add_predictor(yelp1_out,yelp_out, "\\$", "dollar_sign")
yelp_out = add_predictor(yelp1_out,yelp_out, "1", "1")
yelp_out = add_predictor(yelp1_out,yelp_out, "2", "2")
yelp_out = add_predictor(yelp1_out,yelp_out, "3", "3")
yelp_out = add_predictor(yelp1_out,yelp_out, "4", "4")
yelp_out = add_predictor(yelp1_out,yelp_out, "5", "5")
yelp_out = add_predictor(yelp1_out,yelp_out, "minus", "minus")
yelp_out = add_predictor(yelp1_out,yelp_out, "!", "exclam_mark")
yelp_out = add_predictor(yelp1_out,yelp_out, "\\?", "question_mark")
yelp_out = add_predictor(yelp1_out,yelp_out, "\\+", "add_mark")
yelp_out = add_predictor(yelp1_out,yelp_out, "\\-", "minus_mark")






all_out <-yelp_out



all_out = all_out[colnames(all_train)]





### use LASSO model
library(glmnet)
Xmat <- as.matrix(all_train)
#Xmat_out <- as.matrix(dat_out)
Ymat <- dat$stars
#election.lasso <- glmnet(Xmat, Ymat) 
set.seed(1)

# Lasso penalty, choosing lambda by cross-validation
election.lasso.cv <- cv.glmnet(Xmat, Ymat, nfold=5)

#all_out <- all_out[, -c(1)]
out_mat = as.matrix(all_out)
#plot(election.lasso.cv)
mse.min <- election.lasso.cv$cvm[election.lasso.cv$lambda == election.lasso.cv$lambda.min]
rmse = sqrt(mse.min)


### predict the out data and round the result 
sub_1 <- predict(election.lasso.cv, newx = out_mat, s = "lambda.min") 
sub_1[which(sub_1 > 5)] = 5
sub_1[which(sub_1 < 1)] = 1
```

### generate the .csv file
colnames(sub_1 ) = "Expected"
Id <- yelp1_out$Id
Expected <- sub_1
submit1 <-cbind(Id, Expected)
write.csv(submit1, "submit1.csv", row.names = F)
