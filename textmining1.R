install.packages(c("tm", "Matrix", "SnowballC","wordcloud", "lsa", "pdftools","qdapTools", "stringr"))

library(tm)
library(Matrix)
library(SnowballC)
library(wordcloud)
library(lsa)
library(pdftools)
library(qdapTools)
library(stringr)

filepath = "E:/Lc10-Preprocessing2/ClassDemonstration/txt"
setwd(filepath)
dir(filepath)
corpustxt= Corpus(DirSource(filepath))
corpustxt

#PDF
filepath = "C:/Users/Hannan Qazi/Desktop/ClassDemonstration/pdf"
setwd(filepath)
dir(filepath)
files = list.files(pattern="pdf$")
files
text = lapply(files, pdf_text)
text = lapply(text, str_replace_all,"[\n]", "")
text = lapply(text, str_replace_all,"[\r]", "")0
corpustxt= Corpus(DirSource(filepath))
corpustxt

#HBO
filepath = "C:/Users/Hannan Qazi/Desktop/ClassDemonstration"
setwd(filepath)
file = "HBO_NOW.txt"
file =file(file, open= "r")
text.dec = readLines(file)

corpus_txt = Corpus( VectorSource(text.dec))
corpus_txt

corpus_HBO = tm_map(corpus_txt, PlainTextDocument)
corpus_HBO = tm_map(corpus_HBO, tolower)
corpus_HBO = tm_map(corpus_HBO, removeNumbers)
corpus_HBO = tm_map(corpus_HBO, removePunctuation)

stopwords("english")
mystopwords = c("hbo", "now" , "app")

corpus_HBO_clean <- tm_map(corpus_HBO, removeWords, c(stopwords("english"),mystopwords))

corpus.tdm = TermDocumentMatrix(corpus_HBO_clean)
write.csv(as.matrix(corpus.tdm), file=file.path("tdm.csv"))

freq <- rowSums(as.matrix(corpus.tdm))
freq <- sort(freq, decreasing = TRUE)
head(freq)

words <- names(freq)
wordcloud(words[1:40], freq[1:40], scale = c(2,0.8), colors = brewer.pal(8,"Dark2"))

#Text analysis

corpus.tdm.weight=weightTfIdf(corpus.tdm,normalize=TRUE)
write.csv(as.matrix(corpus.tdm.weight),file=file.path("tdm_w.csv"))

#speccify dimensions
mydimensions=5     #start with 100

#LSA
corpus.tm.weight.lsa=lsa(corpus.tdm.weight,dims = mydimensions)
summary(corpus.tm.weight.lsa)

tk=as.matrix(corpus.tm.weight.lsa$tk)
#corpus.tm.weigh.lsa$sk
sk=Diagonal(n=mydimensions,as.matrix(corpus.tm.weight.lsa$sk))
dk=as.matrix(corpus.tm.weight.lsa$dk)

#term loading
termloading=tk%*%sk
write.csv(as.matrix(termloading),file=file.path("term_loading.csv"))

#document loading
docloading=dk%*%sk
write.csv(as.matrix(docloading),file=file.path("doc_loading.csv"))

y=corpus.tm.weight.lsa$sk
x=seq(1,mydimensions,1)
plot(x,y,xlab="numbers of factors",
     ylab="singular values",main="Let's go Home",
     ylim=c(0,10),xlim = c(0,mydimensions),pch=10,col="blue")

#term loadings rotation
termloading.rotation=varimax(as.matrix(termloading),normalize=TRUE)

summary(termloading.rotation)
write.csv(termloading.rotation$loadings,file=file.path("termloadingrotation.csv"))
