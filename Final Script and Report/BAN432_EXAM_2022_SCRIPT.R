options(scipen = 999)
rm(list = ls())

library(ggplot2) # Plots
library(tidyverse) # Dplyr etc. 
library(quanteda)
library(tm)
library(topicmodels)
library(slam)
library(udpipe)
library(parallel)
library(kableExtra)
library(wordcloud)
library(readr)
library(rvest)
library(httr)
library(XML)
library(htm2txt) # Convert html documents to txt
library(lubridate) # Date formating
library(dplyr)
library(rrapply) # Convert from list to df
library(stats)
library(stargazer) # Regression outputs
library(textir) # MNIR ML model
library(tidytext) # Clean text
library(textmineR) # DTM and TF-IDF filtering
library(lmtest) # Robust SE
library(sandwich) # Robust SE


setwd("")

###############################################################################
# LOAD RETURN DATA
###############################################################################

# Load earnigns calls and return data:
ret <- read.csv2("dailyReturn.csv", sep = ",") 


# RETURN DATA:
# Clean return data:
ret <- ret %>% 
  mutate(date = as.Date(date),
         prc = as.double(prc),
         vol = as.double(vol),
         ret = as.double(ret),
         shrout = as.double(shrout)) %>%
  as_tibble()

# Create a return over a three day window (Return(t+1)/Return(t-1))
ret <- ret %>% 
  group_by(ticker) %>% 
  arrange(date) %>% 
  mutate(
    lead_ret = lead(ret),
    lag_ret = lag(ret),
    lag_ret2 = lag(ret, 2),
    lead_ret2 = lead(ret, 2),
    ret_contemp = (1+lag_ret)*(1+ret)*(1+lead_ret)-1)

###############################################################################
# LOAD WSJ DATA 
# NB: this takes time to run!
###############################################################################

# Load HTML-files:
length(unique(ret$ticker))
tickers <- unique(ret$ticker) %>% sort()
html.files <- list.files(path = paste0(getwd(),"/WSJdata/", tickers),
                         pattern="\\.htm.*",
                         recursive = T,
                         full.names = T)

# Read all html documents:
files <- html.files %>% 
  set_names() %>% 
  map(read_html)

# Extract tables from all html files:
wsj <- lapply(files, function(x) html_nodes(x, "table"))

# Only show file names:
wsj.names <- list.files(path = paste0(getwd(),"/WSJdata/", tickers), 
                        pattern="\\.htm.*", 
                        recursive = F, 
                        full.names = F) %>% 
  str_replace_all(".htm.*","") %>% # Replace .html with nothing e.g. WHR01.html --> WHR01
  str_replace_all("\\d+","") # Removes numbers from names e.g. WHR01 --> WHR

names(wsj) <- wsj.names

# Convert all elements of list to tibbles from html table: 
wsj <- lapply(wsj, function(x) html_table(x, fill = T))

# Remove tables that are not necessary: 
for(i in 1:length(wsj)){
  wsj[[i]][[1]] <- NULL
  wsj[[i]][[length(wsj[[i]])]] <- NULL
  wsj[[i]][[length(wsj[[i]])]] <- NULL
}

# Merge all and unnest sub-list: 
wsj <- lapply(unique(wsj.names), function(n) wsj[wsj.names %in% n])
names(wsj) <- unique(wsj.names)

# Unnest sub-list (e.g. all articles for AL are shown under AL as list elements):
for(i in 1:length(wsj)){
  wsj[[i]] <- unlist(unname(wsj[[i]]), recursive = F)
}


###############################################################################
# EXTRACT ARTICLE DATES AND ARTICLE TEXT FROM WSJ-ARTICLE FILES 
# NB: this takes time to run!
###############################################################################

# 1. Save dates (nested lapply is used): 
art.date <- lapply(wsj, function(y) {                 # y = ticker
  lapply(y, function(x) {                             # x = table under ticker
    x %>% 
      filter(X1 == "PD") %>%                          # We want to extract the PD row from the table
      select(X2) %>%                                  # We only want column 2
      unlist()}                                       # Unlist to view column 2 as values 
  )
}
) 

# 1.1 Convert listed table to tibble with rrapply (this unnest the list):
art.date <- rrapply(art.date, how = "melt")

# 2. Save text:
art.text <- lapply(wsj, function(y) {                # y = ticker
  lapply(y, function(x) {                            # x = table under ticker
    x %>% 
      filter(X1 %in% "TD") %>%                       # We want to extract the PD row from the table
      select(X2) %>%                                 # We only want column 2
      unlist()}                                      # Unlist to view column 2 as values 
  )
}
)

# 2.1 Melt text list 
art.text <- rrapply(art.text, how = "melt") 

# 3. Save Word count in a list
art.WC <- lapply(wsj, function(y) {                  # y = ticker
  lapply(y, function(x) {                            # x = table under ticker
    x %>% 
      filter(X1 %in% "WC") %>%                       # We want to extract the PD row from the table
      select(X2) %>%                                 # We only want column 2
      unlist()}                                      # Unlist to view column 2 as values 
  )
}
)

# 3.1 Melt text list 
art.WC <- rrapply(art.WC, how = "melt") 

# 4. Company count: 
art.CO <- lapply(wsj, function(y) {                  # y = ticker
  lapply(y, function(x) {                            # x = table under ticker
    x %>% 
      filter(X1 %in% "CO") %>%                       # We want to extract the PD row from the table
      select(X2) %>%                                 # We only want column 2
      unlist()}                                      # Unlist to view column 2 as values 
  )
}
)

# 4.1 Melt text list 
art.CO <- rrapply(art.CO, how = "melt") 


###############################################################################
################################## MNIR MODEL ################################# 
###############################################################################

# COMMENT: 
# line 200-450 is functions for the different elements of the model

###############################################################################
# CREATE META DATA CONSISTING OF RETURNS, TICKERS, WC, AND TEXTS
###############################################################################

meta_function = function(max_ret, min_ret, art_co_count) {
  
  # Calculate total number of articles: 
  art.tot.count <- art.text %>% 
    group_by(L1) %>% 
    mutate(tot_articles = n()) %>% 
    select(L1, L2, tot_articles)

   # Create meta DF:
    meta <- art.text %>% 
      # Add dates to meta data:
      left_join(art.date, by = c("L1", "L2")) %>%
      
      # Add word count to meta data:
      left_join(art.WC, by = c("L1", "L2")) %>% 
      
      # Add company count per article to meta data:
      left_join(art.CO, by = c("L1", "L2")) %>% 
      
      # Add total number of articles per firm count: 
      left_join(art.tot.count, by = c("L1", "L2")) %>% 
      
      # Rename columns:
      rename(ticker = L1, 
             article_nr = L2,
             text = value.x,
             date = value.y,
             wc = value.x.x, 
             co = value.y.y) %>%
      
      # Clean data:
      mutate(text = tolower(text),
             text = gsub("[\n]", " ", text),
             text = gsub("[\r]", " ", text),
             text = gsub("license this article(.*$)", " ", text),
             text = gsub("subscribe to wsj(.*$)", " ", text),
             text = gsub("\\s+", " ", text),
             date = dmy(date),
             wc = as.double(gsub("words", "", wc)),
             co_count = str_count(co, "[|]") + 1) %>%
      
      # Add returns to meta data:
      left_join(ret, by = c("date", "ticker")) %>% 
      
      # Filter out articles that regard a high number of different firms and extreme returns:
      filter(co_count <= art_co_count,
             year(date) >= 2000,
             ret_contemp < max_ret, 
             ret_contemp > -min_ret) 
    
    
    # Create training period and hold out period for meta:
    ## Assign random groups:
    set.seed(2) # set seed for reproducability
    
    training <- sample(unique(meta$ticker), round(length(unique(meta$ticker))/2,0))
    
    # Create hold out and training period for MNIR:
    meta <- meta %>% 
      mutate(
        group = ifelse(ticker %in% training, "training", "holdout")) 
    
}

###############################################################################
# Tokenize:
###############################################################################

tokenize_function = function(stopwords_to_keep, ngram) {
  
  toks.ngram <- tokens_ngrams(
    as.vector(meta$text) %>% 
      tokens(what = "word",
             remove_numbers = T,                                # Remove numbers
             remove_punct = T,                                  # Remove punctation
             remove_url = T,                                    # Remove URL
             remove_symbols = T) %>%
      tokens_keep(
        min_nchar = 2L,     
        max_nchar = 20L) %>%                                    # Remove terms with less than 3 or more than 20 characters
      tokens_remove(stopwords() %>% 
                      setdiff(stopwords_to_keep)),
    n = ngram)                                                  # Transform into bigrams
  
  
}

###############################################################################
# DTM w/ filter:
###############################################################################

dtm_function = function(filter1_idf_max, filter2_nr_top_terms, filter3_min_terms_across_tickers) {

  # Create a corpus
  corpus <- VCorpus(VectorSource(toks.ngram)) 
  
  # Create dtm:
  dtm <- DocumentTermMatrix(corpus)
  #control = list( 
  # wordLengths = c(4, 20)))
  # Convert to sparse matrix:
  dtm <- sparseMatrix(i=dtm$i, 
                      j=dtm$j, 
                      x=dtm$v, 
                      dims=c(dtm$nrow, dtm$ncol),
                      dimnames = dtm$dimnames)
  
  
  
  
  ## FILTERS:
  # Delete terms that are not used in the training sample
  dtm <- dtm[, col_sums(dtm[meta$group == "training",]) != 0 ]
  
  # Filter 1: remove terms that occour in few docs and have low frequency (we use idf score):
  filter1 <- TermDocFreq(dtm) %>% 
    filter(idf < filter1_idf_max) %>% 
    select(term) %>% 
    as_vector() 
  
  dtm <- dtm[,filter1]
  
  # Filter 2: only use top terms in each article:
  filter2 <- t(GetTopTerms(dtm, filter2_nr_top_terms, return_matrix = F))
  
  # Filter 3: Remove company specific terms (terms mentioned in less than 20 :
  filter3 <- filter2 %>% 
    as_tibble() %>% 
    add_column(ticker = meta$ticker,
               ret = meta$ret_contemp) %>% 
    pivot_longer(1:filter2_nr_top_terms, names_to = "term", values_to = "value") %>% 
    group_by(value) %>% 
    mutate(count = n_distinct(ticker)) %>% 
    filter(count > filter3_min_terms_across_tickers) %>% # number of terms mentioned across tickers | = 60 for unigrams | = 30 for bigrams
    select(value) %>% 
    distinct() %>% 
    as_vector() 
  
  dtm <- dtm[,filter3] # Filter 2 is implemented in filter 3
  
}

###############################################################################
# Run MNLN:
###############################################################################

mnir_function = function(cluster_cores) {
  
  # Parallel session: 
  detectCores()
  
  # initialize session and cluster cores: 
  cl <- makeCluster(cluster_cores)
  
  taddy <- dmr(cl, 
               covars = meta[meta$group == "training", "ret_contemp"],
               counts = dtm[meta$group == "training",],
               bins = NULL, 
               gamma = 10, 
               nlambda = 10, 
               verb = 2,
               trace = T)
  
  # close cluster
  stopCluster(cl)
  
  return(taddy)
  
}

###############################################################################
# Inspect MNLN:
###############################################################################

top_mnir_terms = function(rows){
  
  mnir.coef <- coef(taddy)[2,]
  mnir.coef <- sort(mnir.coef)
  
  mnir_top_neg <- rrapply(split(head(mnir.coef, rows), 
                                names(head(mnir.coef, rows))),
                          how = "melt") %>% 
    as_tibble() %>% 
    rename(Bigram = L1, 
           Loadings = value) %>% 
    arrange(by = Loadings) %>% 
    mutate(
      Bigram = gsub( "_", " ", Bigram),
      Loadings = round(Loadings, 2)
    )
  
  
  mnir_top_pos <- rrapply(split(tail(mnir.coef, rows), 
                                names(tail(mnir.coef, rows))),
                          how = "melt") %>% 
    as_tibble() %>% 
    rename(Bigram = L1, 
           Loadings = value) %>% 
    arrange(desc(by = Loadings)) %>% 
    mutate(
      Bigram = gsub( "_", " ", Bigram),
      Loadings = round(Loadings, 2)
    ) 
  
  cbind(mnir_top_neg, mnir_top_pos) %>% 
    kbl(format = "html") %>%
    kable_classic_2(html_font = "Times New Roman", full_width = F) %>% 
    add_header_above(c("Negative" = 2,"Positive" = 2))
  
}

###############################################################################
# Create dictionary
###############################################################################

neg_dict = function(words) {
  
  mnir.coef <- coef(taddy)[2,]
  mnir.coef <- sort(mnir.coef)
  
  # Creating negative dictionary
  mnir_top_neg <- rrapply(split(head(mnir.coef, words), 
                                names(head(mnir.coef, words))),
                          how = "melt") %>% 
    as_tibble() %>% 
    rename(Bigram = L1, 
           Neg_score = value) %>% 
    arrange(by = Neg_score) %>% 
    mutate(
      Neg_score = round(Neg_score, 2))

  
}

pos_dict = function(words) {
  
  mnir.coef <- coef(taddy)[2,]
  mnir.coef <- sort(mnir.coef)
  
  # Creating positive dictionary
  mnir_top_pos <- rrapply(split(tail(mnir.coef, words), 
                                names(tail(mnir.coef, words))),
                          how = "melt") %>% 
    as_tibble() %>% 
    rename(Bigram = L1, 
           Pos_score = value) %>% 
    arrange(by = Pos_score) %>% 
    mutate(
      Pos_score = round(Pos_score, 2))

  
}

###############################################################################
# INTERNAL VALIDITY PLOT
###############################################################################

internal_val_plot_function = function() {
  proj <- srproj(taddy, as.matrix(dtm))
  head(proj)
  
  meta$z <- proj[,1]
  meta$z <- scale(meta$z)
  
  ## SENTIMENT SCORE VS. RETURN PLOT:
  meta$z.group <- cut(meta$z, 
                      seq(quantile(meta$z, .1), quantile(meta$z,.9), length.out = 11),
                      include.lowest = T, 
                      labels = paste0("G", 0:9))
  
  # compute average return per group seperately for each sample
  meta.agg <- aggregate(meta$ret_contemp, by = list(meta$z.group, meta$group), mean) %>% 
    mutate(Group.2 = as.character(Group.2), 
           Group.2 = gsub("holdout", "Hold Out", Group.2),
           Group.2 = gsub("training", "Training", Group.2))
  
  colnames(meta.agg) <- c("Bin", "Sample", "Av.ret")
  meta.agg$Bin <- factor(meta.agg$Bin)
  
  # plot relationship between Bin and return separately for training and hold-out
    plot <- ggplot(meta.agg, aes(x = as.numeric(Bin), y = Av.ret*100, color = Sample)) + 
    geom_line(linetype = "dashed", size = 1) + 
    geom_point(size = 4) + 
    theme_bw() +
    scale_color_manual(values=c("dodgerblue4", "darkgoldenrod2")) +
    scale_x_continuous(breaks=1:10, labels=paste0("G", 0:9)) + 
    xlab("Groups based on SR-scores for each document (z)") + 
    ylab("Average return per group") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=9),
          axis.title=element_text(size=12,face="bold")) 
    
    return(list(plot))
    
}


###############################################################################
# CREATE DTM FOR TRANSCRIPTS (USED FOR EXTERNAL VALIDITY)
###############################################################################
load("earningsCallsTranscripts.Rdata")

#Creating one variable that pastes qa to end of intro (transcripts data)
transcripts_comb <- paste(transcripts$intro, transcripts$qa) %>% data.frame()

transcripts_comb <- setNames(transcripts_comb, "intro") #setting name so that data can be joined 

#merging old transcripts with edited transcripts
transcripts_comb <- 
  left_join(
    x = transcripts_comb,
    y = transcripts
  )
transcripts_comb$id <- transcripts$id
transcripts_comb$publishedOn <- transcripts$publishedOn
transcripts_comb$permno <- transcripts$permno
transcripts_comb <- rename(transcripts_comb, intro_qa = intro) %>% #rename variable
  select(-c(qa))
transcripts_comb[10,]

irrelevant_stopwords <- stopwords() %>% 
  setdiff(c("up", "down", "over", "under", "above", "below", "no", "not"))

transcripts_clean <- transcripts_comb %>% 
  mutate(text = gsub("THE INFORMATION CONTAINED HERE IS A TEXTUAL(.*$)", " ", intro_qa), #Removing end of qa that contains information about where the text is retrieved from
         text = gsub('[[:punct:] ]+', ' ', text), #remove punctuation
         text = gsub('[[:digit:]]+', ' ', text), #remove digits
         text = gsub("\\b[[:alpha:]]{1}\\b"," ", text), #remove 1-letter words
         text = gsub("\\b[[:alpha:]]{21,}\\b"," ", text), 
         text = gsub(paste0('\\b', irrelevant_stopwords, '\\b', collapse = "|"), ' ', text),
         text = gsub('\\s+', " ", text))  #remove whitespace 

transcripts_clean$text <- transcripts_clean$text %>% tolower() #Convert characters to lower case

call_clean <- read_rds("transcripts_clean_data.RData")

# join transcripts data and return data
meta_transcripts <- call_clean %>%
  rename(date = publishedOn) %>% 
  mutate(date = as.Date(date)) %>% 
  left_join(ret, by = c("permno", "date")) %>%
  select(-id, -permno, -prc, -vol, -shrout) %>% 
  na.omit() 


# Add word count: 
meta_transcripts$wc <- str_count(meta_transcripts$text, "\\w+")


# add article count per ticker
trans.tot.count <- meta_transcripts %>% 
  group_by(ticker) %>% 
  mutate(tot_articles = n()) %>% 
  select(ticker, date,  tot_articles)

meta_transcripts <- meta_transcripts %>%  
  left_join(trans.tot.count, by = c("ticker", "date"))


dtm_for_transcript = function(ngram) {
  

  # create tokens: 
  toks_transcripts <- as.vector(as.vector(meta_transcripts[,3])) %>% 
    tokens(what = "word",                    # Which tokenizer to use -- we use the default here
           remove_numbers = T,               # Remove numbers
           remove_punct = T,                 # Remove punctation
           remove_url = T,                   # Remove URL
           remove_symbols = T) %>%      
    tokens_keep(
      min_nchar = 2L,     
      max_nchar = 20L) %>%                   # Remove terms with less than 2 or more than 20 characters
    tokens_remove(stopwords() %>% 
                    setdiff(c("cannot", 
                              "can't", 
                              "above", 
                              "under", 
                              "over", 
                              "hasn't", 
                              "wouldn't", 
                              "below", 
                              "up", 
                              "down", 
                              "further", 
                              "don't", 
                              "isn't")))               # Remove stopwords
  
  #  Create ngrams (quanteda is used):
  toks.ngram_transcripts <- tokens_ngrams(toks_transcripts,
                                          n = ngram)
  
  # Create a corpus of transcript data
  corpus <- VCorpus(VectorSource(toks.ngram_transcripts)) 
  
  # Create dtm:
  dtm.external <- DocumentTermMatrix(corpus,
                                     control = list( 
                                       wordLengths = c(4, 20)))
  
  
  
  # Convert to sparse matrix:
  dtm.external <- sparseMatrix(i=dtm.external$i, 
                               j=dtm.external$j, 
                               x=dtm.external$v, 
                               dims=c(dtm.external$nrow, dtm.external$ncol),
                               dimnames = dtm.external$dimnames)

  
}

###############################################################################
# FUNCTION RUN ON UNI AND BIGRAMS:
###############################################################################
#-----------------------------------------------------------------------------
# BIGRAMS:
#------------------------------------------------------------------------------
    #1: Create Meta data: 
    meta <- meta_function(max_ret = 0.3, min_ret = 0.3, art_co_count = 25)
    
    #2: Create bigrams:
    toks.ngram <- tokenize_function(stopwords_to_keep = c("cannot", 
                                                          "can't", 
                                                          "above", 
                                                          "under", 
                                                          "over", 
                                                          "hasn't", 
                                                          "wouldn't", 
                                                          "below", 
                                                          "up", 
                                                          "down", 
                                                          "further", 
                                                          "don't", 
                                                          "isn't"), ngram = 2)
    
    #3: Run DTM w/ filters:
    dtm <- dtm_function(filter1_idf_max = 5, filter2_nr_top_terms = 20, filter3_min_terms_across_tickers = 10)
    
    #4: Run MNIR:
    taddy <- mnir_function(cluster_cores = 10)
    
    #5: View top terms: 
    top_mnir_terms(rows = 20)
    
    #6 plot: 
    internal_val_plot_function()
    
    #7 Check for overfitting:  
    proj <- srproj(taddy, as.matrix(dtm))
    head(proj)
    
    meta$z <- proj[,1]
    meta$z <- scale(meta$z)
    meta$m <- proj[,2]
    
    #(Returns = Beta*Z + m)
    bigram.holdout <- lm(ret_contemp ~ z + m, data = meta %>% 
                           filter(group == "holdout"))
    
    bigram.training <- lm(ret_contemp ~ z + m, data = meta %>% 
                            filter(group == "training"))
    
    
    #8: Check internal validity:
      mnir_top_pos <- pos_dict(words = 150)
      mnir_top_neg <- neg_dict(words = 150)
        
      # Compute sentiment scores:
      neg_sent <- row_sums(dtm[, colnames(dtm) %in% mnir_top_neg$Bigram], na.rm=T) / row_sums(dtm)
      neg_sent <- neg_sent/sd(neg_sent, na.rm = T)
      meta$neg_sent <- neg_sent
      
      pos_sent <- row_sums(dtm[, colnames(dtm) %in% mnir_top_pos$Bigram], na.rm=T) / row_sums(dtm)
      pos_sent <- pos_sent/sd(pos_sent, na.rm = T)
      meta$pos_sent <- pos_sent
      
      # Regressions: 
      internal = list()
      
      internal[[1]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                                 filter(group == "holdout"))
      internal[[2]] = lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                           filter(group == "training"))
      
      
      # Split the remaining firms in Group B in firms with a lot of news articles and the ones with few articles
      internal.split.article.freq = list()
      
      internal.split.article.freq[[1]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                                               filter(group == "holdout",
                                                      tot_articles > 275)) 
      
      internal.split.article.freq[[2]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                                              filter(group == "holdout",
                                                     tot_articles <= 275))
      
   #9. Check for external validity:
      dtm.external <- dtm_for_transcript(ngram = 2)
      
      neg_sent <- row_sums(dtm.external[, colnames(dtm.external) %in% mnir_top_neg$Bigram], na.rm = T) / row_sums(dtm.external)
      neg_sent <- neg_sent/sd(neg_sent, na.rm = T)
      meta_transcripts$neg_sent <- neg_sent
      
      pos_sent <- row_sums(dtm.external[, colnames(dtm.external) %in% mnir_top_pos$Bigram], na.rm = T) / row_sums(dtm.external)
      pos_sent <- pos_sent/sd(pos_sent, na.rm = T)
      meta_transcripts$pos_sent <- pos_sent
      
      # Regressions: 
      external = list()
      
      external[[1]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta_transcripts)
      
      summary(external[[1]])
      
      # Apply the same split in Group A and B. Does it do equally well as in Task 3?
      
      external.split.article.freq = list()
      
      external.split.article.freq[[1]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta_transcripts %>% 
                                              filter(tot_articles > 9))
      
      external.split.article.freq[[2]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta_transcripts %>% 
                                               filter(tot_articles <= 9))
      
      
      test <- meta_transcripts %>% 
      filter(tot_articles > 9)
      
      length(unique(test$ticker))
#------------------------------------------------------------------------------
# UNIGRAMS:
#------------------------------------------------------------------------------
      #1: Create Meta data: 
      meta <- meta_function(max_ret = 0.3, min_ret = 0.3, art_co_count = 25)
      
      #2: Create bigrams:
      toks.ngram <- tokenize_function(stopwords_to_keep = c("cannot", 
                                                            "can't", 
                                                            "above", 
                                                            "under", 
                                                            "over", 
                                                            "hasn't", 
                                                            "wouldn't", 
                                                            "below", 
                                                            "up", 
                                                            "down", 
                                                            "further", 
                                                            "don't", 
                                                            "isn't"), ngram = 1)
      
      #3: Run DTM w/ filters:
      dtm <- dtm_function(filter1_idf_max = 5, filter2_nr_top_terms = 20, filter3_min_terms_across_tickers = 60)
      
      #4: Run MNIR:
      taddy <- mnir_function(cluster_cores = 10)
      
      #5: View top terms: 
      top_mnir_terms(rows = 20)
      
      #6 plot: 
      internal_val_plot_function()
      
      #7 Check for overfitting:  
      proj <- srproj(taddy, as.matrix(dtm))
      head(proj)
      
      meta$z <- proj[,1]
      meta$z <- scale(meta$z)
      meta$m <- proj[,2]
      
      #(Returns = Beta*Z + m)
      bigram.holdout <- lm(ret_contemp ~ z + m, data = meta %>% 
                             filter(group == "holdout"))
      
      bigram.training <- lm(ret_contemp ~ z + m, data = meta %>% 
                              filter(group == "training"))
      
      
      #8: Check internal validity:
      mnir_top_pos <- pos_dict(words = 75)
      mnir_top_neg <- neg_dict(words = 75)
      
      # Compute sentiment scores:
      neg_sent <- row_sums(dtm[, colnames(dtm) %in% mnir_top_neg$Bigram], na.rm=T) / row_sums(dtm)
      neg_sent <- neg_sent/sd(neg_sent, na.rm = T)
      meta$neg_sent <- neg_sent
      
      pos_sent <- row_sums(dtm[, colnames(dtm) %in% mnir_top_pos$Bigram], na.rm=T) / row_sums(dtm)
      pos_sent <- pos_sent/sd(pos_sent, na.rm = T)
      meta$pos_sent <- pos_sent
      
      # Regressions: 
      internal[[3]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                            filter(group == "holdout"))
      internal[[4]] = lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                           filter(group == "training"))
      
      
      # Split the remaining firms in Group B in firms with a lot of news articles and the ones with few articles
      
      internal.split.article.freq[[3]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                                               filter(group == "holdout",
                                                      tot_articles > 275)) 
      
      internal.split.article.freq[[4]] = lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                                              filter(group == "holdout",
                                                     tot_articles <= 275))
      
      #9. Check for external validity:
      dtm.external <- dtm_for_transcript(ngram = 1)
      
      neg_sent <- row_sums(dtm.external[, colnames(dtm.external) %in% mnir_top_neg$Bigram], na.rm = T) / row_sums(dtm.external)
      neg_sent <- neg_sent/sd(neg_sent, na.rm = T)
      meta_transcripts$neg_sent <- neg_sent
      
      
      
      pos_sent <- row_sums(dtm.external[, colnames(dtm.external) %in% mnir_top_pos$Bigram], na.rm = T) / row_sums(dtm.external)
      pos_sent <- pos_sent/sd(pos_sent, na.rm = T)
      meta_transcripts$pos_sent <- pos_sent
      
      # Regressions: 
      
      external[[2]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta_transcripts)
      
      # Apply the same split in Group A and B. Does it do equally well as in Task 3?
      coef(taddy)[2]  
      external.split.article.freq[[3]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta_transcripts %>% 
                                               filter(tot_articles > 9))
      
      external.split.article.freq[[4]] <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta_transcripts %>% 
                                               filter(tot_articles <= 9))
      summary(external.split.article.freq[[3]])
      
#------------------------------------------------------------------------------     
# Stargazer output of unigrams and bigrams:      
#------------------------------------------------------------------------------    
    
# Stargazer to view regressions:
# Stargazer table for internal validity (GROUP A AND B):
stargazer(internal,
          type = "html",
          out = "internal_val.html",
          report = ('vc*t'),
          dep.var.labels = c("Returns"),
          column.labels = c("Group B (Bi)", "Group A (Bi)", "Group B (Uni)", "Group A (Uni)"),
          omit = c("Constant"),
          covariate.labels = c("Positive", "Negative", "Log(Word Count)"),
          keep.stat = c("n", "adj.rsq"))
    
    
    
# Stargazer table for firms with a lot vs. few articles: 
stargazer(internal.split.article.freq,
          type = "html",
          out = "group_b_art_freq.html",
          report = ('vc*t'),
          dep.var.labels = c("Returns"),
          column.labels = c("High Freq. (Bi)", "Low Freq. (Bi)", "High Freq. (Uni)", "Low Freq. (Uni)"),
          omit = c("Constant"),
          covariate.labels = c("Positive", "Negative", "Log(Word Count)"),
          keep.stat = c("n", "adj.rsq"))
    

# External validity:
stargazer(external,
          type = "html",
          out = "external_val.html",
          report = ('vc*t'),
          dep.var.labels = c("Returns"),
          column.labels = c("Bigrams", "Unigrams"),
          omit = c("Constant"),
          covariate.labels = c("Positive", "Negative", "Log(Word Count)"),
          keep.stat = c("n", "adj.rsq"))

# Stargazer table for firms with a lot vs. few articles: 
stargazer(external.split.article.freq,
          type = "html",
          out = "group_a_and_b.html",
          report = ('vc*t'),
          dep.var.labels = c("Returns"),
          column.labels = c("High Freq. (Bi)", "Low Freq. (Bi)", "High Freq. (Uni)", "Low Freq. (Uni)"),
          omit = c("Constant"),
          covariate.labels = c("Positive", "Negative", "Log(Word Count)"),
          keep.stat = c("n", "adj.rsq"))


###############################################################################
# Sensitivity Analysis
###############################################################################
#-----------------------------------------------------------------------------
# SENSITIVITY 1:
# Bigram holdout w/ minimum level of term freq. across firms as variable
#-----------------------------------------------------------------------------
sensi.mat.1 <- matrix(NA, nrow = 50, ncol = 2)

for(i in c(10, 20, 30, 40, 50)){
  #1: Create Meta data: 
  meta <- meta_function(max_ret = 0.3, min_ret = 0.3, art_co_count = 25)
  
  #2: Create bigrams:
  toks.ngram <- tokenize_function(stopwords_to_keep = c("cannot", 
                                                        "can't", 
                                                        "above", 
                                                        "under", 
                                                        "over", 
                                                        "hasn't", 
                                                        "wouldn't", 
                                                        "below", 
                                                        "up", 
                                                        "down", 
                                                        "further", 
                                                        "don't", 
                                                        "isn't"), ngram = 2)
  
  #3: Run DTM w/ filters:
  dtm <- dtm_function(filter1_idf_max = 5, filter2_nr_top_terms = 20, filter3_min_terms_across_tickers = i)
  
  #4: Run MNIR:
  taddy <- mnir_function(cluster_cores = 10)

  #7 Check for overfitting:  
  proj <- srproj(taddy, as.matrix(dtm))
  head(proj)
  
  meta$z <- proj[,1]
  meta$z <- scale(meta$z)
  meta$m <- proj[,2]
  
  #8: Check internal validity:
  mnir_top_pos <- pos_dict(words = 150)
  mnir_top_neg <- neg_dict(words = 150)
  
  # Compute sentiment scores:
  neg_sent <- row_sums(dtm[, colnames(dtm) %in% mnir_top_neg$Bigram], na.rm=T) / row_sums(dtm)
  neg_sent <- neg_sent/sd(neg_sent, na.rm = T)
  meta$neg_sent <- neg_sent
  
  pos_sent <- row_sums(dtm[, colnames(dtm) %in% mnir_top_pos$Bigram], na.rm=T) / row_sums(dtm)
  pos_sent <- pos_sent/sd(pos_sent, na.rm = T)
  meta$pos_sent <- pos_sent
  
  # Regressions: 
  bigram.holdout.sensi <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                                           filter(group == "holdout"))
  
  bigram.training.sensi = lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                                          filter(group == "training"))
  
  
  sensi.mat.1[i,1] <- summary(bigram.holdout.sensi)$adj.r.squared
  sensi.mat.1[i,2] <- summary(bigram.training.sensi)$adj.r.squared
  
}

sensi.mat.1.1 <- sensi.mat.1 %>% 
  as_tibble %>% 
  na.omit %>% 
  add_column(min_nr_of_unique_firms_using_term = c(10, 20, 30, 40, 50)) %>% 
  rename(holdout = V1,
         training = V2)
  #pivot_longer(c("holdout", "training"), names_to = "sample", values_to =  "Adj.R2")

sensi.mat.1.1 %>% 
  ggplot(aes(x = min_nr_of_unique_firms_using_term, y = holdout)) +
  geom_vline(xintercept = 30, linetype = "dashed", color = "darkgoldenrod2", size = 1) +
  geom_hline(yintercept = summary(internal[[1]])$adj.r.squared, linetype = "dashed", color = "darkgoldenrod2", size = 1) +
  geom_line(color = "dodgerblue4", size = 1.5) +
  geom_point(size = 4, color = "dodgerblue4") +
  theme_bw() +
  xlab("\n Minimum level of term frequency across firms") + 
  ylab("Adj. R2 \n") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13, face = "bold")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50))


sensi.mat.1.1 %>% 
  ggplot(aes(x = min_nr_of_unique_firms_using_term, y = training)) +
  geom_vline(xintercept = 30, linetype = "dashed", color = "darkgoldenrod2", size = 1) +
  geom_hline(yintercept = summary(internal[[2]])$adj.r.squared, linetype = "dashed", color = "darkgoldenrod2", size = 1) +
  geom_line(color = "dodgerblue4", size = 1.5) +
  geom_point(size = 4, color = "dodgerblue4") +
  theme_bw() +
  xlab("\n Minimum level of term frequency across firms") + 
  ylab("Adj. R2 \n") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13, face = "bold")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(10, 20, 30, 40, 50))



#-----------------------------------------------------------------------------
# SENSITIVITY 2: 
# Bigram holdout w/ minimum return as variable
#-----------------------------------------------------------------------------
sensi.mat.2 <- matrix(NA, nrow = 30, ncol = 2)

for(i in c(10, 15, 20, 25, 30)){
  #1: Create Meta data: 
  meta <- meta_function(max_ret = 0.3, min_ret = 0.3, art_co_count = 25)
  
  #2: Create bigrams:
  toks.ngram <- tokenize_function(stopwords_to_keep = c("cannot", 
                                                        "can't", 
                                                        "above", 
                                                        "under", 
                                                        "over", 
                                                        "hasn't", 
                                                        "wouldn't", 
                                                        "below", 
                                                        "up", 
                                                        "down", 
                                                        "further", 
                                                        "don't", 
                                                        "isn't"), ngram = 2)
  
  #3: Run DTM w/ filters:
  dtm <- dtm_function(filter1_idf_max = 5, filter2_nr_top_terms = i, filter3_min_terms_across_tickers = 30)
  
  #4: Run MNIR:
  taddy <- mnir_function(cluster_cores = 10)
  
  #7 Check for overfitting:  
  proj <- srproj(taddy, as.matrix(dtm))
  head(proj)
  
  meta$z <- proj[,1]
  meta$z <- scale(meta$z)
  meta$m <- proj[,2]
  
  #8: Check internal validity:
  mnir_top_pos <- pos_dict(words = 150)
  mnir_top_neg <- neg_dict(words = 150)
  
  # Compute sentiment scores:
  neg_sent <- row_sums(dtm[, colnames(dtm) %in% mnir_top_neg$Bigram], na.rm=T) / row_sums(dtm)
  neg_sent <- neg_sent/sd(neg_sent, na.rm = T)
  meta$neg_sent <- neg_sent
  
  pos_sent <- row_sums(dtm[, colnames(dtm) %in% mnir_top_pos$Bigram], na.rm=T) / row_sums(dtm)
  pos_sent <- pos_sent/sd(pos_sent, na.rm = T)
  meta$pos_sent <- pos_sent
  
  # Regressions: 
  bigram.holdout.sensi <- lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                               filter(group == "holdout"))
  
  bigram.training.sensi = lm(ret_contemp ~ pos_sent + neg_sent + log(wc), data = meta %>% 
                               filter(group == "training"))
  
  
  sensi.mat.2[i,1] <- summary(bigram.holdout.sensi)$adj.r.squared
  sensi.mat.2[i,2] <- summary(bigram.training.sensi)$adj.r.squared
  
}

sensi.mat.2.1 <- sensi.mat.2 %>% 
  as_tibble %>% 
  na.omit %>% 
  add_column(min_nr_of_unique_firms_using_term = c(10, 15, 20, 25, 30)) %>% 
  rename(holdout = V1,
         training = V2)
#pivot_longer(c("holdout", "training"), names_to = "sample", values_to =  "Adj.R2")

sensi.mat.2.1 %>% 
  ggplot(aes(x = min_nr_of_unique_firms_using_term, y = holdout)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "darkgoldenrod2", size = 1) +
  geom_hline(yintercept = summary(internal[[1]])$adj.r.squared, linetype = "dashed", color = "darkgoldenrod2", size = 1) +
  geom_line(color = "dodgerblue4", size = 1.5) +
  geom_point(size = 4, color = "dodgerblue4") +
  theme_bw() +
  xlab("\n Number of top terms kept from each article") + 
  ylab("Adj. R2 \n") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13, face = "bold")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30))


sensi.mat.2.1 %>% 
  ggplot(aes(x = min_nr_of_unique_firms_using_term, y = training)) +
  geom_vline(xintercept = 20, linetype = "dashed", color = "darkgoldenrod2", size = 1) +
  geom_hline(yintercept = summary(internal[[2]])$adj.r.squared, linetype = "dashed", color = "darkgoldenrod2", size = 1) +
  geom_line(color = "dodgerblue4", size = 1.5) +
  geom_point(size = 4, color = "dodgerblue4") +
  theme_bw() +
  xlab("\n Number of top terms kept from each article") + 
  ylab("Adj. R2 \n") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13, face = "bold")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(10, 15, 20, 25, 30))


###############################################################################
# DESCRIPTIVE TABLES
###############################################################################
# Add market cap and scaled lag/lead returns +/- 2 days 
ret <- 
  ret %>% 
  group_by(ticker) %>% 
  mutate(mcap = abs(prc)*shrout,
         std_ret = scale(ret),
         std_lag_ret = scale(lag_ret),
         std_lag_ret2 = scale(lag_ret2),
         std_lead_ret = scale(lead_ret),
         std_lead_ret2 = scale(lead_ret2)) %>% 
  ungroup(ticker)

# Create plot DF:
plot.df <- art.text %>% 
  # Add dates to meta data:
  left_join(art.date, by = c("L1", "L2")) %>%
  
  # Add word count to meta data:
  left_join(art.WC, by = c("L1", "L2")) %>% 
  
  # Rename columns:
  rename(ticker = L1, 
         article_nr = L2,
         text = value.x,
         date = value.y,
         wc = value) %>%
  
  # Clean data:
  mutate(date = dmy(date),
         wc = as.double(gsub("words", "", wc)),
         year = year(date)) %>%
  
  # Add returns to meta data:
  left_join(ret, by = c("date", "ticker"))  


## PLOT 1: article frequency vs. years  
plot1 <- plot.df %>% 
  group_by(year) %>% 
  summarise(nr_articles = n())  

plot1 %>% 
  ggplot(aes(x=year, y=nr_articles)) +
  geom_bar(stat="identity", fill="dodgerblue4", width=.8) +
  xlab("\nYear") +
  ylab("Number of articles\n") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold"))


## PLOT 2: article frequency vs. ticker
plot2 <- plot.df %>% 
  group_by(ticker) %>% 
  summarise(nr_articles = n())  

plot2 %>%
  mutate(ticker = fct_reorder(ticker, nr_articles)) %>%
  ggplot( aes(x=ticker, y=nr_articles)) +
  geom_bar(stat="identity", fill="dodgerblue4", width=.4) +
  coord_flip() +
  xlab("") +
  ylab("\nNumber of articles") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=8),
        axis.title=element_text(size=12,face="bold"))


## PLOT 3: Relationship between median Market Cap and Articles Published in the period
plot3 <-
  plot.df %>% 
  group_by(ticker) %>% 
  summarize(years = n_distinct(year(date)),
            articles_py = as.numeric(n()) / years,
            median_mcap = median(mcap, na.rm = T))

plot3 %>% 
  ggplot(aes(x = median_mcap, y = articles_py)) +
  geom_point(aes(colour = factor(ticker)), show.legend = F) +
  geom_smooth(show.legend = F, se = F, colour = "dodgerblue4", method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = scales::label_number()) +
  xlab("\nMarket Cap (in thousands USD)") + 
  ylab("Articles Published Per Year\n") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13,face="bold"))

## PLOT 4: Relationship between average absolute returns and Articles Published in the period
plot4 <-
  plot.df %>% 
  group_by(ticker) %>% 
  summarize(years = n_distinct(year(date)),
            articles_py = as.numeric(n()) / years,
            avg_abs_ret = mean(abs(ret), na.rm = T))

plot4 %>% 
  ggplot(aes(x = avg_abs_ret, y = articles_py)) +
  geom_point(aes(colour = factor(ticker)), show.legend = F) +
  geom_smooth(show.legend = F, se = F, colour = "#003f5c", method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = scales::percent) +
  xlab("\nAverage Absolute Daily Returns") + 
  ylab("Articles Published Per Year\n") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13, face = "bold"))

## PLOT 5: Relationship between average daily trading volume (in USD) and Articles Published in the period
plot5 <-
  plot.df %>% 
  group_by(ticker) %>% 
  summarize(years = n_distinct(year(date)),
            articles_py = as.numeric(n()) / years,
            avg_vol_usd = mean(vol*abs(prc), na.rm = T))

plot5 %>% 
  ggplot(aes(x = avg_vol_usd / 1000, y = articles_py)) +
  geom_point(aes(colour = factor(ticker)), show.legend = F) +
  geom_smooth(show.legend = F, se = F, colour = "#003f5c", method = "lm") +
  theme_bw() +
  scale_x_continuous(labels = scales::label_number()) +
  xlab("\nAverage Daily Trading Volume (in thousands USD)") + 
  ylab("Articles Published Per Year\n") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=10),
        axis.title=element_text(size=13, face = "bold"))

## PLOT 6: Relationship between average absolute return and what time an article is published


plot6 <- plot.df %>% 
  group_by(ticker) %>% 
  summarize(day.before = std_lag_ret,
            same.day = std_ret,
            day.after = std_lead_ret,
            two.days.after = std_lead_ret2,
            two.days.before = std_lag_ret2) %>% 
  na.omit() %>% 
  distinct() %>% 
  pivot_longer(2:6,names_to = "day", values_to = "return") %>% 
  group_by(day) %>%
  summarize(avg_mvmt = mean(abs(return)))

plot6 %>% 
  ggplot(aes(x = factor(day, level = c("two.days.before", "day.before", "same.day", "day.after", "two.days.after")), y = avg_mvmt)) +
  geom_vline(xintercept = "same.day", colour = "darkgoldenrod2", alpha = 0.5) +
  geom_line(aes(group = 1), linetype = "dashed", colour = "dodgerblue4", alpha = 0.7) +
  geom_point(colour = "dodgerblue4", size = 3) +
  scale_x_discrete(labels = c("Two Days Before", "One Day Before", "Day of Publication", "Day After", "Two Days After")) +
  xlab("\nDays Relative to Article Publication") +
  ylab("Average Absolute Standardized Return\n") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.text=element_text(size=9),
        axis.title=element_text(size=12,face="bold")) +
  ylim(c(0.5,1.1))












################################################################################
############################## ALTERNATIVE MODEL ###############################
################################################################################

################################################################################
# PRE-PROCESSING
################################################################################

# Nested lapply to extract article texts for all firms:
art.text1 <- lapply(wsj, function(y) {               # y = ticker
  lapply(y, function(x) {                            # x = table under ticker
    x %>% 
      filter(X1 %in% "TD") %>%                       # We want to extract the TD row from the table
      select(X2) %>%                                 # We only want column 2
      unlist()}                                      # Unlist to view column 2 as values 
  )
}
)

# Removing stopwords that can be relevant for term context
irrelevant_stopwords <- stopwords() %>% 
  setdiff(c("up", "down", "over", "under", "above", "below", "no", "not"))

# Cleaning the text data
start.time <- Sys.time()
art.text1 <- lapply(art.text1, function(y) {
  lapply(y, function(x) {
    x %>%
      rm_url() %>%                                   # Remove URL
      tolower() %>%                                  # To lower case letters
      gsub("[\n]", " ",.) %>%                        # Remove "\n"
      gsub("[\r]", " ",.) %>%                        # Remove "\r"
      gsub("license this article(.*$)", " ",.) %>%   # Remove string + everything after
      gsub("subscribe to wsj(.*$)", " ",.) %>%       # Remove string + everything after
      gsub(paste0('\\b', irrelevant_stopwords, '\\b', collapse = "|"), ' ',.) %>% # Remove irrelevant stopwords
      gsub('[[:punct:] ]+', ' ',.) %>%               # Remove punctuation
      gsub('[[:digit:]]+', ' ',.)  %>%               # Remove digits
      gsub("\\b[[:alpha:]]{1}\\b"," ",.) %>%         # Remove 1-letter words
      gsub("\\b[[:alpha:]]{21,}\\b"," ",.) %>%       # Remove words over 21-letters
      gsub('\\s+', " ",.)                            # Remove whitespace
  }
  )
}
)
print(Sys.time() - start.time)

# Melt text list 
art.text1 <- rrapply(art.text1, how = "melt") 

# Join dates and texts:
meta <- art.text1 %>% 
  left_join(art.date, by = c("L1", "L2")) %>%
  
  # Rename columns:
  rename(ticker = L1, 
         article_nr = L2,
         text = value.x,
         date = value.y) %>%
  
  # Format dates:
  mutate(date = dmy(date)) %>%
  
  # Add returns to meta data:
  left_join(ret, by = c("date", "ticker"))

# Formating text column to character vector
meta$text %>% 
  as.character() -> meta$text

# Defining training sample (group A) and hold out sample (group B)

set.seed(2) # Set seed for reproducability 

training <- sample(unique(meta$ticker), round(length(unique(meta$ticker))/2,0))

meta <- meta %>% 
  mutate(
    group = ifelse(ticker %in% training, "training", "holdout")) %>% 
  na.omit()

################################################################################
# SENTIMENT DICTIONARY OF BIGRAMS - ALTERNATIVE
################################################################################

# Tokenizing text column into bigrams
meta %>%
  #filter(group == "training") %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>% 
  na.omit() -> meta_bigrams

# Remove rows with character (0) and creating column combined ticker and article number
meta_bigrams %>% 
  filter(!ngram %in% "character 0") %>% 
  mutate(doc_id = paste0(ticker, article_nr)) -> meta_bigrams

# Counting total number of times the bigrams occur in the articles
meta_bigrams %>%
  mutate(article_nr = as.double(article_nr)) %>% 
  group_by(ticker, ngram) %>% 
  count(doc_id, sort = T, name = "termfreq") %>% 
  ungroup() -> art_freq

# Computing tf-idf score for all bigrams
art_freq %>% 
  bind_tf_idf(term = ngram, document = doc_id, n = termfreq) %>% 
  arrange(desc(tf_idf)) -> bigram_tf_idf

# Creating a filter for desired bigrams
bigram_tf_idf %>% 
  filter(tf < 0.1,       # < 0.1
         idf < 4) %>%    # < 4
  select(ngram) %>% 
  distinct() -> tf_idf_filter

# Filtering bigrams master file 
meta_bigrams %>%
  filter(ngram %in% tf_idf_filter$ngram) %>% 
  na.omit() -> relevant_bigrams

# Creating dictionary
dictionary <- relevant_bigrams %>%
  group_by(ngram) %>% 
  summarize(score = mean(ret_contemp, na.rm = T)) %>% 
  mutate(score = scale(score)) %>% 
  arrange(desc(score))

# Creating a table of top positive and negative terms

# Create table for report:

creative_top_neg <- dictionary %>% 
  tail(20) %>% 
  arrange(-desc(score)) %>% 
  rename(Bigram = ngram,
         Loading = score) %>% 
  mutate(Loading = round(Loading, 2))

creative_top_pos <- dictionary %>% 
  head(20) %>% 
  arrange(desc(score)) %>% 
  rename(Bigram = ngram,
         Loading = score) %>% 
  mutate(Loading = round(Loading, 2))

cbind(creative_top_neg, creative_top_pos) %>% 
  kbl(format = "html") %>%
  kable_classic_2(html_font = "Times New Roman", full_width = F) %>% 
  add_header_above(c("Negative" = 2,"Positive" =2))

################################################################################
# SENTIMENT DICTIONARY OF UNIGRAMS - ALTERNATIVE
################################################################################

# Tokenizing text column into unigrams
meta %>% 
  unnest_tokens(ngram, text, token = "ngrams", n = 1) %>% 
  na.omit() -> meta_unigrams

# Remove rows with character (0) and creating column combined ticker and article number
meta_unigrams %>% 
  filter(!ngram %in% "character 0") %>% 
  mutate(doc_id = paste0(ticker, article_nr)) -> meta_unigrams

# Counting total number of times the unigrams occur in the articles
meta_unigrams %>%
  mutate(article_nr = as.double(article_nr)) %>% 
  group_by(ticker, ngram) %>% 
  count(doc_id, sort = T, name = "termfreq") %>% 
  ungroup() -> art_freq_uni

# Computing tf-idf score for all unigrams
art_freq_uni %>% 
  bind_tf_idf(term = ngram, document = doc_id, n = termfreq) %>% 
  arrange(desc(tf_idf)) -> unigram_tf_idf

# Creating a filter for desired unigrams
unigram_tf_idf %>% 
  filter(tf < 0.1,
         idf < 4) %>% 
  select(ngram) %>% 
  distinct() -> tf_idf_filter_uni

# Filtering bigrams master file 
meta_unigrams %>%
  filter(ngram %in% tf_idf_filter_uni$ngram) %>% 
  na.omit() -> relevant_unigrams

# Creating dictionary
dictionary_uni <- relevant_unigrams %>%
  group_by(ngram) %>% 
  summarize(score = mean(ret_contemp, na.rm = T)) %>% 
  mutate(score = scale(score)) %>% 
  arrange(desc(score))

# Create table for report:

creative_top_neg <- dictionary_uni %>% 
  arrange(-desc(score)) %>% 
  head(20) %>% 
  rename(Unigram = ngram,
         Loading = score) %>% 
  mutate(Loading = round(Loading, 2))

creative_top_pos <- dictionary_uni %>% 
  arrange(desc(score)) %>%
  head(20) %>% 
  rename(Unigram = ngram,
         Loading = score) %>% 
  mutate(Loading = round(Loading, 2))

cbind(creative_top_neg, creative_top_pos) %>% 
  kbl(format = "html") %>%
  kable_classic_2(html_font = "Times New Roman", full_width = F) %>% 
  add_header_above(c("Negative" = 2,"Positive" =2))


########################
