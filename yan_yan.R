rm(list = ls())
library(tm)
# import ingredients
ingredients <- read_csv("Facebook/ingredients.txt", 
                        col_names = FALSE)
keywords = unique(unlist(strsplit(tolower(ingredients$X1)," ")))

#   DTM
mystopwords = c("http","food","just","like","great","well","recipe","will","love","good","make","today","thank")
docs_all<-Corpus(DirSource("~/Facebook/fbdata") )
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T,
                           stopwords= c(stopwords("english"),mystopwords))
dtm.full = DocumentTermMatrix(docs_all, control = dtm.control)
dtm_all = removeSparseTerms(dtm.full, 0.99)
idx = colSums (as.matrix(dtm_all))>0
dtm_all = dtm_all[,idx]
X = as.matrix(dtm_all)
tdm_all = t(X)

# create time vector
transform_file_name = function(filename) {
  extract_number = str_match(filename,pattern = "^fpost-([0-9]+)-([0-9]+).csv$")[,-1]
  add_date = paste0(extract_number[,1],"/",extract_number[,2],"/01")
  dt = as.Date(add_date,"%Y/%m/%d")
  return(dt)
}
time = transform_file_name(rownames(X))


# find keywords
find_topwords = function(x, top_n = 100) {
  topwords = sort(x, decreasing = TRUE)[1:top_n]
  return(names(topwords))
}
term_time_matrix = apply(X,MARGIN =1, FUN = find_topwords)
# keywords = unique(melt(term_time_matrix)$value, tolower(ingredients))

new_dtm = X[,colnames(X) %in% keywords]
# transform dtm into long format and rename
melt_ttm = melt(new_dtm)
colnames(melt_ttm)[3] = "count"


# ========  HIERACHICAL CLUSTER ==
library(cluster)
library(dendextend)
library(tidyverse)
hc <- agnes(t(new_dtm), method = "ward")
pltree(hc, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 
# Dissimilarity matrix
d <- dist(t(new_dtm), method = "euclidean")
# Ward's method
hc2 <- hclust(d, method = "ward.D2" )
# Cut tree into 13 groups
{ height = 10000
  k = max(cutree(hc2, h =height))
  sub_grp <- cutree(hc2, k)
  table(sub_grp)
  plot(hc2, cex = 0.5)
  rect.hclust(hc2, k, border = 2:5)
  k
}
# k-means
set.seed(122)
kmeansResult <- kmeans(t(new_dtm), k)
# store cluster results
keyword_cluster = data.frame("word" = names(kmeansResult$cluster),"sub_grp" = unlist(kmeansResult$cluster),row.names = NULL,stringsAsFactors = FALSE)
barplot(table(keyword_cluster$sub_grp))
# ========== Understand Clusters ===== 
for (i in 1:k) {
  s <- sort(kmeansResult$centers[i,], decreasing = T)[1:10]
  period = transform_file_name(names(s))
  df = data.frame(period,s, row.names = NULL)
  df= df[order(period),]
  df$period = substr(df$period,1,7)
  plot = ggplot(df, aes(period, s)) + 
          geom_bar(stat = "identity") + 
          labs(title = paste0("cluster",i), y = "distance to center", x= "time")+
          theme(axis.text.x=element_text(angle=45, hjust=1))
  assign(sprintf('p%d',i),plot)
}

# library(gridExtra)
# grid.arrange(p1,p2,p3, nrow =, ncol =3)
# grid.arrange(p4,p5,p6, nrow =1, ncol =3)
# grid.arrange(p7,p8,p9, nrow =1, ncol =3)

# ========== Frequency of Words in Each Cluster By Documents (time) ==
# define function to calculate average word frequency of each cluster by documents
avg_frequency = function(cluster) {
  words = keyword_cluster$word[keyword_cluster$sub_grp == cluster]
  sub_df = melt_ttm[melt_ttm$Terms %in% words,]
  avg_freq = aggregate(
    formula = count ~ Docs,
    data = sub_df,
    FUN = mean
  )
  return(round(avg_freq$count,0))
}
# create new dataframe, first column as time (file)
avg_freq_satis = data.frame("time" = time)
# calculate word frequency
{
  for (i in 1:k) {
    col_name = paste0("cluster",i)
    avg_freq_satis[,col_name] = avg_frequency(i)
  }
  melt_statis = melt(avg_freq_satis,id = c("time"), value.name = c("count"))
}
# plot trend
library(ggplot2)
p = ggplot(melt_statis, aes(time,count)) + geom_line() 
p + facet_wrap(~ variable)


# 5,6,9,10,13
trending_groups = c(5,6,9,10,13)
cluster_words = list()
for (i in trending_groups) {
  words = keyword_cluster$word[keyword_cluster$sub_grp == i]
  cluster_words[[paste0("cluster",i)]] = words
  cat(paste("cluster ", i, ": ", sep = ""))
  cat(words, "\n")
  if (length(words) > 1) {
    word_seq = sort(colSums(new_dtm[,colnames(new_dtm) %in% words]), decreasing = TRUE)
    png(paste("~/HM5/word_cloud_cluster_",i,".png"),width = 350, height = 350)
    wordcloud(names(word_seq), word_seq, scale = c(1,.5),
              max.words = 20,colors=brewer.pal(6,"Dark2"))
    dev.off()
  } 
}

# ========== Coorelation ====
find_assos_words = function(target_word) {
  dtm2 = dtm_all[,colnames(dtm_all) %in% c(target_word, keywords)]
  assos_words = sort(findAssocs(dtm2, target_word, corlimit=0.1)[[1]], decreasing = TRUE)[1:5]
  return(assos_words)
}
find_assos_words(cluster_words$cluster5) 
find_assos_words(cluster_words$cluster6)
find_assos_words(cluster_words$cluster9)
find_assos_words(cluster_words$cluster10)
find_assos_words(cluster_words$cluster13)


