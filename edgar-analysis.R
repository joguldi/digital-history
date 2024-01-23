library(quanteda)
library(dplyr)
library(tidytext)

###########
# load data
setwd("/Users/ellenguldi/Box Sync/#learningtocode/data")

a1 <- read_csv("sec_fulltext.csv")

a1 <- a1 %>% mutate(doc_id = as.character(row_number())) %>%
  select(doc_id, text, everything()) 

c1 <- quanteda::corpus(a1, docid_field = "doc_id", text_field = "text")

#########
# make some dictionaries
renewables <- c("solar", "wind energy", "renewable", "green energy", "sustainability",  "sustainable energy")
emissions <- c("carbon emissions", "carbon",  "greenhouse",
               "emissions",  "reducing emissions", "reduced emissions",
               "hybrid vehicles",
               "clean fuel", "energy saving", "saving energy", 
               "alternative fuel", "save energy", "reduce energy expenditure", "reducing energy expenditure") 
resilience <- c("hurricane-proof","tornado-proof", "climate-resilient") 
environment <- c("remediation","clean air","climate change", "pollution","clean water", "toxic", "hazardous", "recycling", "reuse", "compostable", "waste")      
policy <- c("carbon tax", "carbon offsets")

df1 <- data.frame()
for(df in c("renewables", "emissions", "resilience", "environment", "policy") ){
  df <- as.data.frame(get(paste0(df))) %>%
    mutate(sentiment = paste0(df))
  names(df) <- c("word", "sentiment")
  df1 <- rbind(df1, df)
  
}
preparedness <- df1

word <- c("flood plain", "tsunami", "severe storm", 
          "wildfire", "weather-related losses",
          "severe weather event", "high winds", 
          "forestfire", "destructive weather pattern", 
          "windstorms", "coastal flooding", 
          "emergency assessment", "forest fire", 
          "hurricane", "refugee crisis", "tornado")

disasters <- as.data.frame(word) %>%
  mutate(sentiment = word) 

allwords <- rbind(disasters, preparedness)

######## 
# Quanteda tokens for disaster
toks_disaster <- tokens_keep(tokens(c1), pattern = as.dictionary(disasters), window = 10) # equivalent to tokens_select(selection = 'keep')
toks_nodisaster <- tokens_remove(tokens(c1), pattern = as.dictionary(disasters), window = 10) # equivalent to tokens_select(selection = 'remove')

# Quanteda DFM (document-feature-matrix) and statistical "keyness" for disaster words
dfmat_disaster <- dfm(toks_disaster, remove = stopwords())
dfmat_nodisaster <- dfm(toks_nodisaster, remove = stopwords())
tstat_key_disaster <- textstat_keyness(rbind(dfmat_disaster, dfmat_nodisaster), seq_len(ndoc(dfmat_disaster)))
tstat_key_disaster_subset <- tstat_key_disaster[tstat_key_disaster$n_target > 10, ]
head(tstat_key_disaster_subset, 50)


# semantic network for disaster
mx <- dfm_select(dfmat_disaster, selection = "keep",  
                 pattern = tstat_key_disaster_subset$feature, 
                 min_nchar = 2) # from the dfm, select only the top "keyness" words
mx_col <- fcm(dfmat_disaster) # create a feature co-ocurrence matrix 
mx_nor <- mx_col / rowSums(mx_col) # as a proportion
head(mx_nor) #inspect
feat <- names(topfeatures(mx_col, 100)) # select 100 top features only
mx_col <- fcm_select(mx_col, feat)
textplot_network(mx_col, min_freq = 0.95, edge_size = 5) #plot it


# document feature matrix for each corporation by CIK showing how much each uses
# the words in the dictionary
dfmatcand <- dfm(c1, groups = "cik", verbose = TRUE)
dfmatcand <- dfm_weight(dfmatcand, "prop")
dfmwdictionary<- dfm_lookup(dfmatcand, dictionary = as.dictionary(disasters))
head(dfmwdictionary)
# topfeatures(dfmwdictionary * 100, n = 10) %>%
#   round(2) %>%
#   knitr::kable(col.names = "Percent")
# hurricanes <- as.vector(dfmwdictionary[, "hurricane"])
# names(hurricanes) <- docnames(dfmwdictionary) 
t1 <- tidy(dfmwdictionary) %>% # tidy analysis
  arrange(-count)
docdata <- a1 %>% distinct(cik, name) # get names of corporations 
t2 <- t1 %>% left_join(docdata, by = c("document" = "cik")) %>%
  rename(word = term)
t3 <- t2 %>% distinct(count, name, word) %>% # top 10 corporations per word
  filter(!is.na(name)) %>%
  group_by(word) %>%
 # dplyr::top_n(10, wt = count) %>% 
  ungroup() %>%
  select(name) %>%
  left_join(t2, by = "name") 
# visualize it
t4 %>%
  ggplot(aes(x = reorder(name, count, sum), y = count, fill = word)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "name", y = "differential 'keyness' from average",
       title = "How much does each company talk about natural disasters in its SEC reports?")






##########
#analysis of preparedness

toks_prep <- tokens_keep(tokens(c1), pattern = as.dictionary(preparedness), window = 10) # equivalent to tokens_select(selection = 'keep')
toks_noprep <- tokens_remove(tokens(c1), pattern = as.dictionary(preparedness), window = 10) # equivalent to tokens_select(selection = 'remove')
dfmat_prep <- dfm(toks_prep)
dfmat_noprep <- dfm(toks_noprep)

tstat_key_prep <- textstat_keyness(rbind(dfmat_prep, dfmat_noprep), seq_len(ndoc(dfmat_prep)))
tstat_key_prep_subset <- tstat_key_prep[tstat_key_prep$n_target > 5, ]
head(tstat_key_prep_subset, 50)

dfmatcandp <- dfm(c1, groups = "cik", verbose = TRUE)
dfmatcandp <- dfm_weight(dfmatcandp, "prop")
dfmatcandRIDp <- dfm_lookup(dfmatcandp, dictionary = as.dictionary(preparedness))
head(dfmatcandRIDp)
topfeatures(dfmatcandRIDp * 100, n = 10) %>%
  round(2) %>%
  knitr::kable(col.names = "Percent")
pollution <- as.vector(dfmatcandRIDp[, "pollution"])
names(pollution) <- docnames(dfmatcandRIDp)
dotchart(sort(pollution), 
         xlab = "RID \"Hurricane\" terms used as a proportion of all terms",
         pch = 19, xlim = c(0, .005))

# semantic
mx <- dfm(c1, remove = stopwords())
mx <- dfm_select(mx, selection = "keep", pattern = tstat_key_prep_subset$feature,#pattern = as.dictionary(preparedness),#features = names(topfeatures(mx, 20)), 
                 min_nchar = 2)

mx_col <- fcm(mx)
mx_nor <- mx_col / rowSums(mx_col)
head(mx_nor)

feat <- names(topfeatures(mx_col, 100)) # 最も頻度が高い共起語を選択
mx_col <- fcm_select(mx_col, feat)
textplot_network(mx_col, min_freq = 0.95, edge_size = 5)


#tidy analysis pollution
tt1 <- tidy(dfmatcandRIDp) %>%
  arrange(-count)
docdata <- a1 %>% distinct(cik, name)
tt2 <- tt1 %>% left_join(docdata, by = c("document" = "cik")) %>%
  rename(word = term)
tt3 <- tt2 %>% distinct(count, name, word) %>% 
  filter(!is.na(name)) %>%
  group_by(word) %>%
  dplyr::top_n(5, wt = count) %>% 
  ungroup() %>%
  select(name) %>%
  left_join(tt2, by = "name") 
tt4 <- tt2 %>% distinct(count, name, word) %>% 
  filter(!is.na(name)) %>%
  group_by(word) %>%
  dplyr::top_n(-10, wt = count) %>% 
  ungroup() %>%
  select(name) %>%
  left_join(tt2, by = "name") 
tt5 <- rbind(tt3, tt4)


tt3 %>%
  ggplot(aes(x = reorder(name, count, sum), y = count, fill = word)) +
  geom_col() + 
  coord_flip() + 
  labs(x = "name", y = "differential 'keyness' from average",
       title = "How much does each company talk about preparedness for climate change in its SEC reports?")

# 
# # not working b/c triples not working
# parsed <-
#   spacy_parse(c1,
#               entity=TRUE,
#               dependency = TRUE,
#               lemma = TRUE,
#               pos = TRUE)
# 
# triples <- make_triples(parsed)
