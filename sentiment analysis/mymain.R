
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
all = read.table("data.tsv",stringsAsFactors = F,header = T)
splits = read.table("splits.csv", header = T)
# remove HTML tags
all$review <- gsub('<.*?>', ' ', all$review)
all$review = gsub("[[:punct:][:blank:]]+", " ", all$review)
# library(textstem)
# all$review = lemmatize_strings(all$review)
# all$review = gsub('[[:digit:]]', ' ', all$review)
all$review = gsub('\\s+', ' ', all$review)
s = 1
train = all[-which(all$new_id%in%splits[,s]),]
test = all[which(all$new_id%in%splits[,s]),]


# YOUR CODE
# For example, start loading required libraries
set.seed(2017L)
library(text2vec)
library(glmnet)
library(magrittr)
library(slam)
library(pROC)
start_time = Sys.time()
# define preprocessing function and tokenization function
prep_fun = tolower
tok_fun = word_tokenizer

it = itoken(all$review, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = all$new_id, 
                  progressbar = FALSE)

it_train = itoken(train$review, 
            preprocessor = prep_fun, 
            tokenizer = tok_fun, 
            ids = train$new_id, 
            progressbar = FALSE)

# Note that most text2vec functions are pipe friendly!
it_test = test$review %>% 
  prep_fun %>% tok_fun %>% 
  # turn off progressbar because it won't look nice in rmd
  itoken(ids = test$new_id, progressbar = FALSE)


stop_words = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")
# stop_words = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you",
#                "your", "yours", "yourself", "yourselves", "he", "him", "his", 
#                "himself", "she", "her", "hers", "herself", "it", "its", "itself", 
#                "they", "them", "their", "theirs", "themselves", "what", "which", 
#                "who", "whom", "this", "that", "these", "those", "am", "is", "are", 
#                "was", "were", "be", "been", "being", "have", "has", "had", 
#                "having", "do", "does", "did", "doing", "a", "an", "the", "and", 
#                "but", "if", "or", "because", "as", "until", "while", "of", "at", 
#                "by", "for", "with", "about", "against", "between", "into", 
#                "through", "during", "before", "after", "above", "below", "to", 
#                "from", "up", "down", "in", "out", "on", "off", "over", "under", 
#                "again", "further", "then", "once", "here", "there", "when", 
#                "where", "why", "how", "all", "any", "both", "each", "few", 
#                "more", "most", "other", "some", "such", "no", "nor", "not", 
#                "only", "own", "same", "so", "than", "too", "very", "s", "t", 
#                "can", "will", "just", "don", "should", "now")
# vocab = create_vocabulary(it, stopwords = stop_words)
vocab = create_vocabulary(it, ngram = c(1L, 2L), stopwords = stop_words)
pruned_vocab = prune_vocabulary(vocab, 
                                term_count_min = 10, 
                                doc_proportion_max = 0.5,
                                doc_proportion_min = 0.001)
vectorizer = vocab_vectorizer(pruned_vocab)

dtm_train = create_dtm(it_train, vectorizer)
identical(as.integer(rownames(dtm_train)), train$new_id)
dtm_test = create_dtm(it_test, vectorizer)

# # define tfidf model
# tfidf = TfIdf$new()
# 
# # fit model to train data and transform train data with fitted model
# dtm_train = fit_transform(dtm_train, tfidf)
# dtm_test = transform(dtm_test, tfidf)

# v.size = dim(dtm_train)[2]
# ytrain = train$sentiment
# #summ[1, 1] is the mean frequency of the first vocab of positive sentiment across all documents
# #summ[1, 2] is the variance frequency of the first vocab of positive sentiment across all documents
# #summ[1, 3] is the mean frequency of the first vocab of negative sentiment across all documents
# #summ[1, 4] is the variance frequency of the first vocab of negative sentiment across all documents
# summ = matrix(0, nrow=v.size, ncol=4)
# summ[,1] = apply(dtm_train[ytrain==1, ], 2, mean)
# summ[,2] = apply(dtm_train[ytrain==1, ], 2, var)
# summ[,3] = apply(dtm_train[ytrain==0, ], 2, mean)
# summ[,4] = apply(dtm_train[ytrain==0, ], 2, var)
# n1=sum(ytrain);
# n=length(ytrain)
# n0= n - n1
# myp = (summ[,1] - summ[,3])/
#   sqrt(summ[,2]/n1 + summ[,4]/n0) #identical to words

words = colnames(dtm_train)
# id = order(abs(myp), decreasing=TRUE)[1:2000] #identical to part of words
id = read.table('my_vocab.txt', sep = ',', header = T)[, 1]
# vocab_list = cbind(id, words[id])
# colnames(vocab_list) = c('id', 'words')
# write.table(vocab_list, file = 'my_vocab.txt',sep = ',', row.names = F, quote = F)

# pos.list = words[id[myp[id]>0]]
# neg.list = words[id[myp[id]<0]]
# 
# write.table(pos.list, file = 'pos_list.txt',sep = ',', row.names = F, quote = F)
# write.table(neg.list, file = 'neg_list.txt',sep = ',', row.names = F, quote = F)

set.seed(500)
NFOLDS = 10
mycv = cv.glmnet(x=dtm_train[, id], y=train$sentiment, 
                 family='binomial',type.measure = "auc", 
                 nfolds = NFOLDS, alpha=0)
myfit = glmnet(x=dtm_train[, id], y=train$sentiment, 
               lambda = mycv$lambda.min, family='binomial', alpha=0)
logit_pred = predict(myfit, dtm_test[, id], type = "response")
result = cbind(test$new_id, logit_pred)
colnames(result) = c('new_id', 'prob')
# write.table(result, file = 'Result_1.txt', sep = ',', row.names = F, quote = F)
write.table(result, file = 'mysubmission.txt', sep = ',', row.names = F, quote = F)
# my_roc <- roc(test$sentiment, as.vector(logit_pred))
# best_threshold = coords(my_roc, "best", ret = "threshold") #the best threshold
# pred_class = ifelse(logit_pred >= best_threshold, '1', '0')

# mis_classified = dtm_test[which(test$sentiment != pred_class), id]
# mis_id = sample(1:dim(mis_classified)[1], 5)
# test_mis = test[which(test$sentiment != pred_class), ]
# test_mis = test_mis[mis_id, ]
# mis_classified = mis_classified[mis_id, ]
# for (i in 1:5) {
#   v = mis_classified[i, ][which(mis_classified[i, ] != 0)]
#   filename = paste(i, '.txt')
#   write.table(v, filename)
# }
# write.table(test_mis, 'a.txt')

# nmis_classified = dtm_test[which(test$sentiment == pred_class), id]
# 
# colnames(mis_classified)
# 
# which(mis_classified[1, ] != 0)
# 
# mean(mis_classified)
# mean(nmis_classified)

auc(test$sentiment, as.vector(logit_pred))

time_diff = difftime(Sys.time(), start_time, units = 'mins')
print(paste('Run time is:', time_diff, 'mins'))








