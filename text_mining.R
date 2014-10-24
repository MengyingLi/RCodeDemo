##Q3_a
speech = readLines("E:/Applied Data Mining/Homework/union_speach.txt")
##Q3_b
k = grep("^\\*{3}$",speech)

##Q3_c
## The location of date
k_1 = k+4
date = speech[k_1][seq(1,214)]

##Q3_d
year = substring(date,regexpr("[,][[:blank:]][[:digit:]]{4}",date)+2,regexpr("[,][[:blank:]][[:digit:]]{4}",date)+5)

##Q3_e
month = substring(date,1,regexpr("[[:alpha:]][[:blank:]][[:digit:]]",date))

##Q3_f
###Location of Presidents' names
k_2 = k+3
president = speech[k_2][seq(1,214)]

##Q3_g
##Find the beginning and the end of each speech
speech.list = matrix(nrow = 214,ncol = 2)
for (i in 1:214){
  speech.list[i,1]=k[i]+6
  speech.list[i,2]=k[i+1]-2
}
##Get the content of each speech
speech_new = as.data.frame(speech,stringsAsFactors = FALSE)
speech_new_list = list()
for(j in 1:214){
    speech_new_list[[j]]=speech_new[seq(speech.list[j,1],speech.list[j,2]),]
}
### Make each speech to a character vector
speech_list2 = list()
for (k in 1:nrow(speech.list)){
  a = NULL
  for (l in speech.list[k,1]:speech.list[k,2])
  {a= paste(a, speech[l], sep=" ")}
  speech_list2[k]= a 
}

##Q3_h
##Devide each speech into sentence
sentence = list()
for (i in 1:nrow(speech.list)){
  sentence[i]=strsplit(speech_list2[[i]],"\\. |\\? |\\!")
}


##Q3_i
cleanfct1= function(x) {
  x = gsub("'","",x)
  x =gsub("\\([Aa]pplause\\.*\\)","",x)
  x= gsub("[0-9]","",x)
  return(x)
}
clean1 = lapply(sentence,cleanfct1)

##Q3_j
clean2 = lapply(clean1,function(x)(tolower(x)))

##Q3_k
clean3 = list()
for(i in 1:length(clean2)){
  clean3[[i]]=unlist(lapply(clean2[[i]],strsplit,"[[:blank:][:punct:]]"))
}

##Q3_l 
clean4 = list()
for(i in 1:length(clean3)){
  clean4[[i]]=clean3[[i]][nchar(clean3[[i]])>0]
}

##Q3_m
library(SnowballC)
stem_word = list()
for (i in 1:length(clean4)){
  stem_word[[i]]=unlist(lapply(clean4[[i]],wordStem))
}


##Q3_n
bagofwords = sort(unique(unlist(stem_word)))
##Explore the bag of words
head(bagofwords)
length(bagofwords)

##Q3_o
word_vector = list()
for(i in 1:length(stem_word)){
  word_vector[[i]]=unlist(stem_word[[i]])
}


##Q3_p
freq_new= matrix(nrow = length(bagofwords), ncol = length(word_vector),dimnames = list(bagofwords,1:214))

for (i in 1:length(bagofwords)){
  for (j in 1:length(word_vector)){
    freq_new[i,j]=sum(word_vector[[j]]==bagofwords[i])
  }
}



#Q3_q
##Find the length of sentence in each speech
sentence_length = sapply(sentence, length)
sum(sentence_length)
##Find the long words in the speech
whole_speech = sort(unique(unlist(clean4)))
long_word = whole_speech[nchar(whole_speech)>=15]
## Find the parties of each speaker
Parties = read.table("E:/Applied Data Mining/Homework/Parties.txt")
S = matrix(nrow = 214,ncol = 1)
for (j in 1:214)
  for(i in 1:41){
    if(president[j]==Parties[i,2]){
      S[j, ]=as.character(Parties[i,1])
    }
  }

#Q3_r
computeSJDistance =
  function(tf, df, terms, logdf = TRUE, verbose = TRUE)
  {
    # terms - a character vector of all the unique words, length numTerms
    # df - a numeric vector, length numTerms, number of docs that contains term
    # tf - matrix, numTerms rows, numCols cols (number of documents)
    
    numTerms = nrow(tf)
    numDocs = ncol(tf)
    
    tfn =  t(tf)/colSums(tf)
    if (logdf) tfidfs = t(tfn) * (log(numDocs) - log(df))
    else  tfidfs = numDocs * ( t(tfn) / df)
    
    D.SJ = matrix(0, numDocs, numDocs)
    for(i in 1:(numDocs -1)) {
      for(j in (i+1):numDocs) { 
        D.SJ[i,j] = D.SJ[j,i] = D.JensenShannon(tfidfs[, i], tfidfs[, j])
      }
      if(verbose)
        print(i)  
    }
    return(D.SJ)
  }

D.BasicKullbackLeibler = function(p, q)
{
  tmp = !(p == 0 | q == 0)
  p = p[tmp]
  q = q[tmp]
  
  sum(- p*log(q) + p*log(p) )
}

D.JensenShannon = function(p, q)
{
  T = 0.5 * (p + q)  
  0.5 * D.BasicKullbackLeibler(p, T) + 0.5 * D.BasicKullbackLeibler(q, T)
}  
##Q3s
normTermFreq = matrix(0, nrow = nrow(freq_new), ncol = ncol(freq_new))

wordsInDoc = apply(freq_new, 2, sum)

for (i in 1:ncol(freq_new)) {
  normTermFreq[, i] = freq_new[, i]/wordsInDoc[i]
}

idf = apply(freq_new, 1, function(x) sum(x > 0))


simMatrix = computeSJDistance(tf = freq_new, 
                              df = idf, terms = bagOfWords, 
                              logdf = FALSE)



rownames(simMatrix) = seq(1:214)
colnames(simMatrix) = seq(1:214)

##Q3_s
documents = as.dist(simMatrix)
hc = hclust(documents)
plot(hc)

hc2 = hclust(documents,"single" )
plot(hc2)
par(cex = 0.5)

mds =cmdscale(simMatrix)
plot(mds, type = "n", xlab = "", ylab = "", main = "Documents")
text(mds, rownames(simMatrix))

