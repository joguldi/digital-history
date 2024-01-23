#install.packages('edgarWebR')
#install.packages('profvis')

library(edgarWebR)
library(edgar)
library(quanteda)
library(dplyr)
library(tidytext)

#index2017 <- getMasterIndex(2016)

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
        


#preparedness <- as.data.frame(word) %>%
 # mutate(sentiment = word) 

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

# download 10000 companies that have climate references somewhere in the full text
output <- data.frame()
for(k in 0:100){ # counter for every 10 pages of 100 results for ea term
  for(j in 1:9){ # counter for every page of 100 results for ea term 
    
    for(i in 1:nrow(allwords)){ # counter for keywords being searched -- up to 9 then pause
      print(paste0("looking for ", paste0(allwords$word[i])))
      
  o2  <- try(
    full_text(q = paste0(allwords$word[i]), 
                   type = "", reverse_order = FALSE, 
                   count = 100,
                    page = j+(k*10),
                   stemming = TRUE, name = "", cik = "", sic = "",
                    from = "", to = "") 
    )
   o2 <- o2 %>%
     mutate(word = paste0(allwords$word[i]))
  output <- rbind(output, o2)

     if(i%%10 == 0){ # every 10 queries pause
     profvis::pause(20) 
     print("pausing")}
  
    print(paste0("counter at ", k, ":", j))
    }
    
    
    write_csv(output, "sec_numbers.csv")
    print("writing output file")
  }
}




setwd(datadir)
write_csv(output, "sec_numbers.csv")
  
# get full text of report for each company listed
a1 <- data.frame()
for(m in seq(from = 28000, to = (nrow(output)-5001), by = 5000)){
  for(l in 1:5000){
    ii <- m+l
    print(paste0("counter ii at ", ii))
  a <- try(parse_filing(output$href[ii]))
 #a <- parse_text_filing(output$href[i], strip = TRUE, include.raw = FALSE, fix.errors = TRUE) %>%
   a <- a %>% mutate(cik =output$cik[ii]) %>%
   mutate(name = output$company_name[ii] )

 a$text <- gsub("<[^>]*>","",as.character(a$text))
 
 a <- a %>% filter(!is.na(text)) %>%
   filter(nchar(text) > 5)
 
a1 <- bind_rows(a1, a)
profvis::pause(20)
  }
  }

a1 <- a1 %>% mutate(doc_id = as.character(row_number())) %>%
  select(doc_id, text, everything()) 

a1

write_csv(a1, "sec_fulltext.csv")

output1 <- output %>% rename(text = content) %>% 
  mutate(doc_id = as.character(row_number())) %>%
  select(doc_id, text, everything()) 

c1 <- quanteda::corpus(output1) #a1)#, docid_field = docid, text_field = text)

#analysis of disaster
toks_disaster <- tokens_keep(tokens(c1), pattern = as.dictionary(disasters), window = 10) # equivalent to tokens_select(selection = 'keep')
toks_nodisaster <- tokens_remove(tokens(c1), pattern = as.dictionary(disasters), window = 10) # equivalent to tokens_select(selection = 'remove')
dfmat_disaster <- dfm(toks_disaster)
dfmat_nodisaster <- dfm(toks_nodisaster)

tstat_key_disaster <- textstat_keyness(rbind(dfmat_disaster, dfmat_nodisaster), seq_len(ndoc(dfmat_disaster)))
tstat_key_disaster_subset <- tstat_key_disaster[tstat_key_disaster$n_target > 1, ]
head(tstat_key_disaster_subset, 50)

dfmatcand <- dfm(c1, groups = "cik", verbose = TRUE)
dfmatcand <- dfm_weight(dfmatcand, "prop")
dfmatcandRID <- dfm_lookup(dfmatcand, dictionary = as.dictionary(disasters))
head(dfmatcandRID)
topfeatures(dfmatcandRID * 100, n = 10) %>%
  round(2) %>%
  knitr::kable(col.names = "Percent")
hurricanes <- as.vector(dfmatcandRID[, "hurricane"])
names(hurricanes) <- docnames(dfmatcandRID)
dotchart(sort(hurricanes), 
         xlab = "RID \"Hurricane\" terms used as a proportion of all terms",
         pch = 19, xlim = c(0, .005))


# semantic
mx <- dfm(c1, remove = stopwords())
mx <- dfm_select(mx, selection = "keep", pattern = tstat_key_disaster_subset$feature,#pattern = as.dictionary(preparedness),#features = names(topfeatures(mx, 20)), 
                 min_nchar = 2)

mx_col <- fcm(mx)
mx_nor <- mx_col / rowSums(mx_col)
head(mx_nor)

feat <- names(topfeatures(mx_col, 100)) # 最も頻度が高い共起語を選択
mx_col <- fcm_select(mx_col, feat)
textplot_network(mx_col, min_freq = 0.95, edge_size = 5)



# tidy analysis
t1 <- tidy(dfmatcandRID) %>%
  arrange(-count)
docdata <- a1 %>% distinct(cik, name)
t2 <- t1 %>% left_join(docdata, by = c("document" = "cik")) %>%
  rename(word = term)
t3 <- t2 %>% distinct(count, name, word) %>% 
  filter(!is.na(name)) %>%
  group_by(word) %>%
  dplyr::top_n(10, wt = count) %>% 
  ungroup() %>%
  select(name) %>%
  left_join(t2, by = "name") 
t4 <- t2 %>% distinct(count, name, word) %>% 
  filter(!is.na(name)) %>%
  group_by(word) %>%
  dplyr::top_n(-10, wt = count) %>% 
  ungroup() %>%
  select(name) %>%
  left_join(t2, by = "name") 
t5 <- rbind(t3, t4)

t5 %>%
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
