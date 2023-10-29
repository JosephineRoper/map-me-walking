# https://towardsdatascience.com/a-light-introduction-to-text-analysis-in-r-ea291a9865a8
library(tm)
library(SnowballC)
library(SentimentAnalysis)
library(dplyr)
library(tidyr)
library(purrr)

answers <- read.csv("map-me_answers_23-05-2023_annotated first page.csv", stringsAsFactors = FALSE)
new_answers <- read.csv("map-me_answers_23-05-2023_annotated_firstpage_19-6.csv", stringsAsFactors = FALSE)
answers <- new_answers

# select columns id_answer to tag list
complete_data <- tibble(answers[c(1:10)])
#complete_data['doc_id']  <- 1:nrow(complete_data)
complete_data['text'] <- complete_data$answer

# where id_subquestion == 26383 or 26526, question = 5
complete_data[complete_data$id_subquestion == 26383 | complete_data$id_subquestion == 26526,'question'] <- as.integer(5)

# turn each entry in all.tags into a list, split by commas
complete_data$tags <- strsplit(complete_data$all.tags, ",")
# remove all starting and trailing spaces from tags
complete_data$tags <- lapply(complete_data$tags, function(x) trimws(x))

#complete_data[complete_data$ï..id_answer == 26656,]$tags[[1]] <- append(complete_data[complete_data$ï..id_answer == 26656,]$tags[[1]], "Don't walk further as would take other modes instead")

# for complete_data$question == 4, change all tags 'distance (-)' to 'distance'	
complete_data[complete_data$question == 4,] <- complete_data[complete_data$question == 4,] %>%
  mutate(tags = map(tags, ~ ifelse(.x == "distance (-)", "distance", .x))) %>%
  mutate(tags = map(tags, ~ ifelse(.x == "shade (-)", "shade", .x))) %>%
  mutate(tags = map(tags, ~ ifelse(.x == "shade (+)", "shade", .x)))

#complete_data <- complete_data %>%
#    mutate(tags = map(tags, ~ ifelse(length(.x) == 0, "unclear answer", .x))) %>%
#    mutate(tags = map(tags, ~ ifelse(grepl("specific distance", .x, fixed = TRUE), "specific time/distance", .x))) %>%
#    mutate(tags = map(tags, ~ ifelse(grepl("specific time", .x, fixed = TRUE), "specific time/distance", .x)))


complete_data[complete_data$id_person == 85532,]


# list the most common tags
complete_data$tags %>% unlist %>% table %>% sort(decreasing = TRUE) %>% head(10)
# group by question then list the most common tags for each question
complete_data %>%
  unnest(tags) %>%
  group_by(question, tags) %>%
  count() %>%
  group_by(question) %>%
  slice_max(n, n=5) %>%
  arrange(question, desc(n)) %>% print(n=34)

# list the most common tags overall
complete_data %>%
  unnest(tags) %>%
  group_by(tags) %>%
  count() %>%
  arrange(desc(n)) %>% print(n=237)

# list the most common tags for question 1 and 2 only
complete_data %>%
  filter(question == 1 | question == 2) %>%
  unnest(tags) %>%
  group_by(tags) %>%  
  count() %>%
  arrange(desc(n)) %>% print(n=179)


complete_data %>% group_by(question) %>% summarise(n = n())

complete_data %>% filter(unlist(map(tags, ~(grepl('roads',.x))))) 

head(complete_data)

# unique question per id_person
complete_data %>% group_by(id_person) %>% summarise(n = n_distinct(question))
# unique id_person per question
complete_data %>% group_by(question) %>% summarise(n = n_distinct(id_person))

# add a column with the length of each answer in words
complete_data %>% mutate(lenan = map(text, lengths(strsplit(.x, " "))))

lengths(strsplit(complete_data$answer, " "))

#for question=1 rows, average answer length
complete_data[complete_data$question == 1,] %>% summarise(mean(lengths(strsplit(text, " "))))
# same but combine text where they have the same id_person
complete_data[complete_data$question == 2,] %>% group_by(id_person) %>% summarise(m = sum(lengths(strsplit(text, " ")))) %>% summarise(min(m), mean(m), max(m))


# extract time from complete_data$timestamp
complete_data$time <- as.POSIXct(complete_data$timestamp, format = "%d/%m/%Y %H:%M", tz = "UTC")

# group by id_person and summarise completiontime
person_times <- complete_data %>% group_by(id_person) %>% summarise(completiontime = as.numeric(max(time) - min(time)))
mean(person_times$completiontime, na.rm = TRUE)
summary(person_times$completiontime)


#~~~~~~ The below is all pretty useless. ~~~~~~~~#
complete_data$doc_id = 1:nrow(complete_data)
# make doc_id the first column
complete_data <- complete_data[,c(12,1:11)]

corpus <- SimpleCorpus(DataframeSource(data.frame(complete_data)))
# And lets see what we have
View(corpus)

corpus2 <- SimpleCorpus(VectorSource(complete_data$text))

dfCorpus <- corpus

# 1. Stripping any extra white space:
dfCorpus <- tm_map(dfCorpus, stripWhitespace)
# 2. Transforming everything to lowercase
dfCorpus <- tm_map(dfCorpus, content_transformer(tolower))
# 3. Removing numbers 
dfCorpus <- tm_map(dfCorpus, removeNumbers)
# 4. Removing punctuation
dfCorpus <- tm_map(dfCorpus, removePunctuation)
# 5. Removing stop words
dfCorpus <- tm_map(dfCorpus, removeWords, stopwords("english"))
# 6. stemming
dfCorpus <- tm_map(dfCorpus, stemDocument)

DTM <- DocumentTermMatrix(dfCorpus)
inspect(DTM)
inspect(DTM[,c("area","mani")])
TDM <- as.matrix(TermDocumentMatrix(dfCorpus))
# how many times is area used in the corpus
FreqMat <- data.frame(ST = rownames(TDM), 
                      Freq = rowSums(TDM), 
                      row.names = NULL)
head(FreqMat, 10)
FreqMat %>% arrange(desc(Freq)) %>% head(30)

sent <- analyzeSentiment(corpus, language = "english", aggregate="id_subquestion")
# were going to just select the Harvard-IV dictionary results ..  
sent <- sent[,1:4]
#Organizing it as a dataframe
sent <- as.data.frame(sent)
# join to the original data frame
sent <- cbind(complete_data, sent)

# Now lets take a look at what these sentiment values look like. 
head(sent)

# group by id_subquestion and summarise SentimentGI
sent %>% group_by(id_subquestion) %>% summarise(SentimentGI = mean(SentimentGI))
