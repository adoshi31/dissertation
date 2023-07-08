
DASH_data <- read.csv(file.choose(), header=TRUE)
library(fastDummies)
 DASH_data_ElSabryLarge <- DASH_data[ , c(4,10) ]
names(DASH_data_ElSabryLarge)
results <- fastDummies::dummy_cols(DASH_data_ElSabryLarge$ElSabry_LargeCat)
names(results)
head(results,100)
names(DASH_data)
names(DASH_data_ElSabryLarge)
summary(DASH_data_ElSabryLarge)
tail(DASH_data_ElSabryLarge)
ElSabry_large <- read.csv(file.choose(), header=TRUE, stringsAsFactors=FALSE)
levels(ElSabry_large$ElSabry_LargeCat)

names(ElSabry_large)
ElSabry_large$story <- ElSabry_large$X
ElSabry_large <- ElSabry_large[,-(3)]
head(ElSabry_large)

hicks_large <- read.csv(file.choose(), header=TRUE)

library(stringr)
library(tm)

remaining_elsabry <- read.csv(file.choose(), header=TRUE)
  # Convert text to lowercase
hicks_large$story <- tolower(hicks_large$story)
  
  # Remove special characters and punctuation
hicks_large$story  <- str_replace_all(hicks_large$story, "[^a-zA-Z0-9 ]", "")
  
  # Remove extra whitespaces
hicks_large$story  <- str_replace_all(hicks_large$story , "\\s+", " ")
  
  # Remove stopwords (optional)
hicks_large$story  <- removeWords(hicks_large$story , stopwords("english"))
  
  # Remove leading/trailing whitespaces
hicks_large$story  <- str_trim(hicks_large$story)
  
write.csv(hicks_large, file="Hicks_cleaned_text_updated.csv")

updatedElsabry <- read.csv(file.choose(), header=TRUE)
updatedDash <- read.csv(file.choose(), header=TRUE)
updatedHicks <- read.csv(file.choose(), header=TRUE)

names(updatedHicks)

newTableHicks <- merge(x = updatedDash, y = updatedHicks, by = "story.id", all.x = T)
write.csv(newTableHicks, file="newTableHicks.csv")

### sociocultural forces

sociocultural <- read.csv(file.choose(), header=TRUE)

# Convert text to lowercase
sociocultural$story <- tolower(sociocultural$story)

# Remove special characters and punctuation
sociocultural$story  <- str_replace_all(sociocultural$story, "[^a-zA-Z0-9 ]", "")

# Remove extra whitespaces
sociocultural$story  <- str_replace_all(sociocultural$story , "\\s+", " ")

# Remove stopwords (optional)
sociocultural$story  <- removeWords(sociocultural$story , stopwords("english"))

# Remove leading/trailing whitespaces
sociocultural$story  <- str_trim(sociocultural$story)

write.csv(sociocultural, file="sociocultural_cleaned_text_updated.csv")

sociocultural_classified <- read.csv(file.choose(), header=TRUE)
newTableSocio3 <- merge(x = updatedDash, y = sociocultural_classified, by = "story.id", all.x = T)
write.csv(newTableSocio3, file="newTableSocio3.csv")


### Extramural (ElSabry)

updatedExtramural <- read.csv(file.choose(), header=TRUE)

# Remove special characters and punctuation
updatedExtramural$story  <- str_replace_all(updatedExtramural$story, "[^a-zA-Z0-9 ]", "")

# Convert text to lowercase
updatedExtramural$story <- tolower(updatedExtramural$story)

# Remove extra whitespaces
updatedExtramural$story  <- str_replace_all(updatedExtramural$story , "\\s+", " ")

# Remove stopwords (optional)
updatedExtramural$story  <- removeWords(updatedExtramural$story , stopwords("english"))

# Remove leading/trailing whitespaces
updatedExtramural$story  <- str_trim(updatedExtramural$story)

write.csv(updatedExtramural, file="updatedExtramural_cleaned_text_updated.csv")

sociocultural_classified <- read.csv(file.choose(), header=TRUE)
newTableSocio3 <- merge(x = updatedDash, y = sociocultural_classified, by = "story.id", all.x = T)
write.csv(newTableSocio3, file="newTableSocio3.csv")


############################################################33
### Evidence-based categories (ElSabry)

updatedEvidence <- read.csv(file.choose(), header=TRUE)

# Convert text to lowercase
updatedEvidence$story <- tolower(updatedEvidence$story)

# Remove special characters and punctuation
updatedEvidence$story  <- str_replace_all(updatedEvidence$story, "[^a-zA-Z0-9 ]", "")

# Remove extra whitespaces
updatedEvidence$story  <- str_replace_all(updatedEvidence$story , "\\s+", " ")

# Remove stopwords (optional)
updatedEvidence$story  <- removeWords(updatedEvidence$story , stopwords("english"))

# Remove leading/trailing whitespaces
updatedEvidence$story  <- str_trim(updatedEvidence$story)

write.csv(updatedEvidence, file="updatedEvidence_cleaned_text_updated.csv")

updatedEvidence_classified <- read.csv(file.choose(), header=TRUE)
updatedDASH <- read.csv(file.choose(), header=TRUE)
newTableDASH <- merge(x = updatedDASH, y = updatedEvidence_classified, by = "story.id", all.x = T)
write.csv(newTableDASH, file="newTableDASH_evidence.csv")


############################################################33
### PERSONAL categories (ElSabry)

updatedPersonal <- read.csv(file.choose(), header=TRUE)

# Remove special characters and punctuation
updatedPersonal$story  <- str_replace_all(updatedPersonal$story, "[^a-zA-Z0-9 ]", "")

# Convert text to lowercase
updatedPersonal$story <- tolower(updatedPersonal$story)

# Remove extra whitespaces
updatedPersonal$story  <- str_replace_all(updatedPersonal$story , "\\s+", " ")

# Remove stopwords (optional)
updatedPersonal$story  <- removeWords(updatedPersonal$story , stopwords("english"))

# Remove leading/trailing whitespaces
updatedPersonal$story  <- str_trim(updatedPersonal$story)

write.csv(updatedPersonal, file="updatedPersonal_cleaned_text_updated_NNLM.csv")

updatedPersonal_classified <- read.csv(file.choose(), header=TRUE)
updatedDASH <- read.csv(file.choose(), header=TRUE)
newTableDASH <- merge(x = updatedDASH, y = updatedPersonal_classified, by = "story.id", all.x = T)
write.csv(newTableDASH, file="newTableDASH_personal.csv")


##################
############################################################33
### Credibility categories (ElSabry)

updatedCred <- read.csv(file.choose(), header=FALSE)

# Remove special characters and punctuation
updatedCred$V2  <- str_replace_all(updatedCred$V2, "[^a-zA-Z0-9 ]", "")

# Convert text to lowercase
updatedCred$V2 <- tolower(updatedCred$V2)

# Remove extra whitespaces
updatedCred$V2  <- str_replace_all(updatedCred$V2 , "\\s+", " ")

# Remove stopwords (optional)
updatedCred$V2  <- removeWords(updatedCred$V2 , stopwords("english"))

# Remove leading/trailing whitespaces
updatedCred$V2  <- str_trim(updatedCred$V2)

write.csv(updatedCred, file="updatedCred.csv")
