rm(list = ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
set.seed(9701)
library(dplyr)
library(ggplot2)
library(recommenderlab)  
library(reshape2)  
ratings = read.csv('ratings.dat', sep = ':', 
                   colClasses = c('integer', 'NULL'), header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
dim(ratings) 

# movies data
# In movies.dat, some movie names contain single colon (:), so the above 
# method does not work. 

movies = readLines('movies.dat')
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Genres = ifelse(grepl('\\|', movies$Genres), "Multiple", 
                       movies$Genres)
rating_merged = merge(x = ratings, y = movies, by.x = "MovieID")

ggplot(rating_merged, aes(x = factor(Genres), y = Rating), 
       color = factor(vs)) + stat_summary(fun.y = mean, position =
                                            position_dodge(), geom = "bar") + labs(x = "Genres", y = "Mean
                                                                                   ratings", title = "Mean ratings by genres") + theme(axis.text.x =
                                                                                                                                         element_text(angle = 90, hjust = 1))


train.id = sample(nrow(ratings), floor(nrow(ratings)) * 0.6)
train = ratings[train.id, ]
write.table(train, file = 'train.dat', sep = '::', row.names = FALSE,
            col.names = FALSE
)

test = ratings[- train.id, ]
test.id = sample(nrow(test), floor(nrow(test)) * 0.5)
test = test[test.id, ]
test$Timestamp = NULL
test$ID = 1:nrow(test)
label = test[c('ID', 'Rating')]
test$Rating = NULL
test = test[c('ID', 'UserID', 'MovieID')]
colnames(test) = c('ID', 'user', 'movie')
write.table(test, file = 'test.csv', sep = ',', row.names = FALSE)
write.table(label, file = 'label.csv', sep = ',', row.names = FALSE)
# remove timestamp column
train$Timestamp = NULL
head(train)
R = acast(train, UserID ~ MovieID)
R = as(R, 'realRatingMatrix')
R_m = normalize(R)
head(getRatingMatrix(R_m))

recommenderRegistry$get_entries(dataType = "realRatingMatrix")
rec = Recommender(R, method = 'UBCF',
                  parameter = list(normalize = 'Z-score', method = 'Cosine', nn = 5)
)

recom = predict(rec, R, type = 'ratings')  # predict ratings. This may be slow.
rec_list = as(recom, 'list')
rec_matrix = as(recom, 'matrix')

test = read.csv('test.csv', header = TRUE)
test$rating = NA
for (u in 1:nrow(test)){
  
  # Read userid and movieid from columns 2 and 3 of test data
  userid = as.character(test$user[u])
  movieid = as.character(test$movie[u])
  
  rating = rec_list[[userid]][movieid]
  # 2.5 may be too arbitrary
  test$rating[u] = ifelse(is.na(rating), 2.5, rating)
  
}
y = read.csv('label.csv', header = T)
res = merge(test, y, by.x = 'ID')
