# making a new csv with old ones and using functions to add cleaned demographics to new csv

#packages
library(dplyr)
library(plyr) #to rename the column names

# import the csv file, taken from google drive
# all data from round1 and round2
read.csv("all_participants.csv")
# updated_participants has the data we are using for round1, including spin and span
read.csv(".updated_participants.csv")
# demographics we were missing from previous csvs
read.csv("unfinished_demographics.csv")

# creating 2 different dataframes for round 1 and round 2
round1 <-filter(all_participants, data_collection_group == "round1")
# round2 <-filter(all_participants, data_collection_group == "round2")

# selecting specific columns to join
.updated_participants <- .updated_participants %>%
  select(qualtrics_id, subject_pool, participant_id, Date, SurveyVersion, High, Low, span.count)


#joining the 2 dataframes together
round1_clean<-left_join(.updated_participants, round1, by="qualtrics_id")

# renaming unfinished_demo columns date and survey_version
unfinished_demographics_new <- rename(unfinished_demographics, c("date"="Date", "survey_version"="SurveyVersion"))

# join round1_clean and unfinished demo
round1_all <- rbind.fill(round1_clean, unfinished_demographics_new)

# deleting columns by index
round1_all <-round1_clean_all[-c(25:29)]
#updated_round1_clean_new <- updated_round1_clean_new[-c(25:29)]

# getting rid of the na's
round1_all[is.na(round1_all)] <- ""

# functions
round1 = round1_all


#functions for cleaning the demographics
# sexual orientation, gender, race, ethnicity, language (changed old english into english)

sex_or <- function(round1){
  # sexual orientation
  or.clean = round1$sexual_orientation
  # Straight
  or.clean[grep("straight", or.clean)] = "Straight"
  or.clean[grep("Staight", or.clean)] = "Straight"
  or.clean[grep("Heterosexual", or.clean)] = "Straight"
  or.clean[grep("women", or.clean)] = "Straight"
  or.clean[grep("FEMALE", or.clean)] = "Straight"
  
  # bisexual
  or.clean[grep("Bi Sexual", or.clean)] = "Bisexual"
  or.clean[grep("bisexual", or.clean)] = "Bisexual"
  
  # gay
  or.clean[grep("gay", or.clean)] = "Gay"
  
  # Queer
  or.clean[grep("queer", or.clean)] = "Queer"
  
  # Female
  
  
  round1$or.clean = or.clean
  
  round1[round1$or.clean == "heterosexual",]$or.clean = "Straight"
  #round1[round1$or.clean == "female" & round1$gender == "male",]$or.clean = "Straight"
  round1[round1$or.clean == "Female" & round1$gender == "Male",]$or.clean = "Straight"
  round1[round1$or.clean == "Male" & round1$gender == "Female",]$or.clean = "Straight"
  
  # New level for LGBTQIA+
  levels(round1$or.clean) <- c(levels(round1$or.clean),"LGBTQIA+")
  # bisexual
  round1[round1$or.clean == "Bisexual",]$or.clean = "LGBTQIA+"
  # fag
  round1[round1$or.clean == "fag",]$or.clean = "LGBTQIA+"
  #fluid
  round1[round1$or.clean == "fluid",]$or.clean = "LGBTQIA+"
  # gay
  round1[round1$or.clean == "Gay",]$or.clean = "LGBTQIA+"
  # Heteroflexible
  round1[round1$or.clean == "Heteroflexible",]$or.clean = "LGBTQIA+"
  # Pansexual
  round1[round1$or.clean == "Pansexual",]$or.clean = "LGBTQIA+"
  # Queer
  round1[round1$or.clean == "Queer",]$or.clean = "LGBTQIA+"
  # Homosexual
  round1[round1$or.clean == "Homosexual",]$or.clean = "LGBTQIA+"
  # Lesbian
  round1[round1$or.clean == "Lesbian",]$or.clean = "LGBTQIA+"
  # Queer; Demisexual; Pansexual; Bisexual
  round1[round1$or.clean == "Queer;Demisexual;Pansexual;Bisexual",]$or.clean = "LGBTQIA+"
  # Gay as fuck
  round1[round1$or.clean == "Gay as fuck",]$or.clean = "LGBTQIA+"
  # lesiban
  round1[round1$or.clean == "lesiban",]$or.clean = "LGBTQIA+"
  # Queer;Demisexual;Panromantic
  #round1[round1$or.clean == "Queer;Demisexual;Panromantic",]$or.clean = "LGBTQIA+"
  
  
  # New level for no response
  levels(round1$or.clean) <- c(levels(round1$or.clean),"NR")
  #round1[round1$or.clean == "Cisgender",]$or.clean = "NR"
  #round1[round1$or.clean == "MARRIED",]$or.clean = "NR"
  #round1[round1$or.clean == "single",]$or.clean = "NR"
  round1[round1$or.clean == "Normal",]$or.clean = "NR"
  #round1[round1$or.clean == "Bicurious",]$or.clean = "NR"
  #round1[round1$or.clean == "Down Low",]$or.clean = "NR"
  #round1[round1$or.clean == "mostly heterosexual;to be explored or not",]$or.clean = "NR"
  
  #round1[round1$or.clean == "female" & round1$gender == "female",]$or.clean = "NR"
  round1[round1$or.clean == "female" & round1$gender == "Female",]$or.clean = "NR"
  round1[round1$or.clean == "Female" & round1$gender == "Female",]$or.clean = "NR"
  round1[round1$or.clean == "Male" & round1$gender == "Male",]$or.clean = "NR"
  #round1[round1$or.clean == "male" & round1$gender == "Male",]$or.clean = "NR"
  
  
  # New level for the one blank answer
  levels(round1$or.clean) <- c(levels(round1$or.clean),"NA")
  round1[round1$or.clean == "",]$or.clean = "NA"
  round1[round1$or.clean == "na",]$or.clean = "NA"
  
  #drop levels
  round1$or.clean <- droplevels(round1$or.clean)
  #table(round1$or.clean)
  
  
  
  # return a value in the function
  return(round1$or.clean)
}

# language
language.clean <- function(round1){
  
  lang.clean = round1$list_languages
  lang.clean[grep("none", lang.clean)] = "English"
  
  round1$lang.clean = lang.clean
  
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Hawaiian")
  round1[round1$lang.clean == "hawaiin",]$lang.clean = "Hawaiian"
  
  round1[round1$lang.clean == "",]$lang.clean = "English"
  round1[round1$lang.clean == "Old English",]$lang.clean = "English"
  round1[round1$lang.clean == "english",]$lang.clean = "English"
  round1[round1$lang.clean == "Spanish.",]$lang.clean = "Spanish"
  round1[round1$lang.clean == "spanish",]$lang.clean = "Spanish"
  round1[round1$lang.clean == "intermediate spanish",]$lang.clean = "Spanish"
  round1[round1$lang.clean == "little spanish",]$lang.clean = "Spanish"
  
  # Farsi
  round1[round1$lang.clean == "Farsi (Persian)",]$lang.clean = "Farsi"
  round1[round1$lang.clean == "Persian (Farsi)",]$lang.clean = "Farsi"
  
  # French
  round1[round1$lang.clean == "French;Spanish",]$lang.clean = "French"
  
  #Bengli
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Bengali")
  round1[round1$lang.clean == "Bengali;Spanish",]$lang.clean = "Bengali"    
  
  #Cantonese
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Cantonese")
  round1[round1$lang.clean == "Cantonese;French;Spanish",]$lang.clean = "Cantonese"
  round1[round1$lang.clean == "Cantonese;Mandarin",]$lang.clean = "Cantonese"
  round1[round1$lang.clean == "Cantonese;Spanish;French",]$lang.clean = "Cantonese"
  round1[round1$lang.clean == "chinese;cantonese;hokkien;teochew;japanese;english",]$lang.clean = "Cantonese" 
  
  round1[round1$lang.clean == "Chinese;Korean;Japanese;Spanish;German",]$lang.clean = "Chinese"
  
  # multiple language answers: using the first response
  
  # Farsi, Spanish, French
  round1[round1$lang.clean == "Farsi, Spanish, French",]$lang.clean = "Farsi"
  
  # French, Farsi, and some Spanish
  round1[round1$lang.clean == "French, Farsi, and some Spanish",]$lang.clean = "French"
  
  # French, Greek, Spanish
  round1[round1$lang.clean == "French, Greek, Spanish",]$lang.clean = "French"
  
  # Hindi, Marathi
  round1[round1$lang.clean == "Hindi, Marathi",]$lang.clean = "Hindi"
  
  # Hindi, Punjabi
  round1[round1$lang.clean == "Hindi, Punjabi",]$lang.clean = "Hindi"
  
  # Hindi, Spanish
  round1[round1$lang.clean == "Hindi, Spanish",]$lang.clean = "Hindi"
  
  # Hindi, Telugu, Sanskrit
  round1[round1$lang.clean == "Hindi, Telugu, Sanskrit",]$lang.clean = "Hindi"
  
  round1[round1$lang.clean == "Hindi;Kannada",]$lang.clean = "Hindi"
  round1[round1$lang.clean == "Hindi;French (not fluent)",]$lang.clean = "Hindi"
  round1[round1$lang.clean == "Hindi;Marathi",]$lang.clean= "Hindi"
  round1[round1$lang.clean == "Hindi;Marwari",]$lang.clean = "Hindi"
  round1[round1$lang.clean == "Hindi;Tamil;Marathi",]$lang.clean = "Hindi"
  
  #Korean
  round1[round1$lang.clean == "korean",]$lang.clean= "Korean"
  round1[round1$lang.clean == "Korean;Mandarin Chinese",]$lang.clean= "Korean"    
  round1[round1$lang.clean == "Korean;Spanish",]$lang.clean= "Korean"  
  
  
  # Mandarin
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Mandarin")
  round1[round1$lang.clean == "Mandarin Chinese;Spanish",]$lang.clean = "Mandarin"
  round1[round1$lang.clean == "Mandarin Chinese;German",]$lang.clean = "Mandarin"
  round1[round1$lang.clean == "Mandarin Chinese;Cantonese;Hakka",]$lang.clean = "Mandarin"
  round1[round1$lang.clean == "Mandarin Chinese",]$lang.clean = "Mandarin"
  round1[round1$lang.clean == "English;Mandarin Chinese",]$lang.clean = "Mandarin"
  
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Marathi")
  round1[round1$lang.clean == "Marathi;Hindi",]$lang.clean = "Marathi"
  
  # Spanish, ASL (American Sign Language)
  round1[round1$lang.clean == "Spanish, ASL (American Sign Language)",]$lang.clean = "Spanish"
  
  # Spanish, French
  round1[round1$lang.clean == "Spanish, French",]$lang.clean = "Spanish"
  
  # Spanish, Garifuna
  round1[round1$lang.clean == "Spanish, Garifuna",]$lang.clean = "Spanish"
  
  # Spanish, Russian, French
  round1[round1$lang.clean == "Spanish, Russian, French",]$lang.clean = "Spanish"
  
  # Spanish;Tigrinya
  round1[round1$lang.clean == "Spanish;Tigrinya",]$lang.clean = "Spanish"
  
  # Spanish;Portuguese, French
  #round1[round1$lang.clean == "Spanish;Portuguese, French",]$lang.clean = "Spanish"
  # Spanish;Mandarin Chinese
  #round1[round1$lang.clean == "Spanish;Mandarin Chinese",]$lang.clean = "Spanish"
  
  # Spanish;Hindi.
  round1[round1$lang.clean == "Spanish;Hindi.",]$lang.clean = "Spanish"
  
  # Spanish;Hindi;Russian
  #round1[round1$lang.clean == "Spanish;Hindi;Russian",]$lang.clean = "Spanish"
  # Spanish;German
  #round1[round1$lang.clean == "Spanish;German",]$lang.clean = "Spanish"
  
  # Spanish;French
  round1[round1$lang.clean == "Spanish;French",]$lang.clean = "Spanish"
  
  # Spanish;Farsi;Hebrew
  #round1[round1$lang.clean == "Spanish;Farsi;Hebrew",]$lang.clean = "Spanish"
  
  # Spanish;English
  round1[round1$lang.clean == "Spanish;English",]$lang.clean = "Spanish"
  
  # Spanish;Cantonese
  round1[round1$lang.clean == "Spanish;Cantonese",]$lang.clean = "Spanish"
  
  # Spanish;Armenian;Italian
  #round1[round1$lang.clean == "Spanish;Armenian;Italian",]$lang.clean = "Spanish"
  # Spanish;Arabic (Lebanese)
  #round1[round1$lang.clean == "Spanish;Arabic (Lebanese)",]$lang.clean = "Spanish"
  # Spanish;Arabic
  #round1[round1$lang.clean == "Spanish;Arabic",]$lang.clean = "Spanish"
  # Spanish;American Sign Language (ASL);German
  #round1[round1$lang.clean == "Spanish;American Sign Language (ASL);German",]$lang.clean = "Spanish"
  # Spanish, Marathi
  #round1[round1$lang.clean == "Spanish, Marathi",]$lang.clean = "Spanish"
  # Spanish, Italian
  #round1[round1$lang.clean == "Spanish, Italian",]$lang.clean = "Spanish"
  
  # Spanish and Japanese
  round1[round1$lang.clean == "Spanish and Japanese",]$lang.clean = "Spanish"
  
  #spanish
  round1[round1$lang.clean == "English;Spanish",]$lang.clean = "Spanish"
  round1[round1$lang.clean == "english;spanish",]$lang.clean = "Spanish"
  
  #Tagalog
  round1[round1$lang.clean == "English;Tagalog;Spanish",]$lang.clean = "Tagalog"
  
  #Thai
  round1[round1$lang.clean == "Thai;Tagalog",]$lang.clean = "Thai"
  
  # new level for Telugu
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Telugu")
  # Telugu;Hindi
  round1[round1$lang.clean == "Telugu;Hindi",]$lang.clean = "Telugu"
  #Telugu;Hindi;Tami
  round1[round1$lang.clean == "Telugu;Hindi;Tamil",]$lang.clean = "Telugu"
  #Telugu;Spanish
  round1[round1$lang.clean == "Telugu;Spanish",]$lang.clean = "Telugu"
  
  # Urdu
  round1[round1$lang.clean == "urdu;hindi;punjabi",]$lang.clean = "Urdu"
  
  # Indonesian
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Indonesian")
  round1[round1$lang.clean == "Indonesian;Korean;Mandarin Chinese;Malay",]$lang.clean = "Indonesian"
  
  # Kannada
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Kannada")
  round1[round1$lang.clean == "Kannada;Telugu;Hindi",]$lang.clean= "Kannada"
  
  #Hebrew
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Hebrew")
  round1[round1$lang.clean == "Hebrew;Russian;French;Bambara;Hausa;Arabic (Modern Standard)",]$lang.clean= "Hebrew"
  
  #Portuguese
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Portuguese")
  round1[round1$lang.clean == "Portuguese;Spanish",]$lang.clean= "Portuguese"
  
  #Russian
  levels(round1$lang.clean) <- c(levels(round1$lang.clean),"Russian")
  round1[round1$lang.clean == "Russian;German",]$lang.clean= "Russian"
  
  
  round1$lang.clean <- droplevels(round1$lang.clean)
  table(round1$lang.clean)
  
  # return a value in the function
  return(round1$lang.clean)
}

# gender
gender.clean <- function(round1){
  gender.clean = round1$gender
  # dealing with all the males
  gender.clean[grep("^M",gender.clean)] = "male"
  gender.clean[grep("trans", gender.clean)] = "male"
  gender.clean[grep("Identify",gender.clean)] = "male"
  # new
  gender.clean[grep("Cisgender", gender.clean)] = "male"
  gender.clean[grep("FTM", gender.clean)] = "male"
  
  
  # dealing with all the females
  gender.clean[grep("^F",gender.clean)] = "female"
  gender.clean[grep("^f", gender.clean)] = "female"
  gender.clean[grep("ema", gender.clean)] = "female"
  
  #new
  #gender.clean[grep("mail", gender.clean)] = "female"
  
  round1$gender.clean = gender.clean
  #dealing with other
  levels(round1$gender.clean) <- c(levels(round1$gender.clean),"Other")
  round1[round1$gender.clean == "Gender Fluid",]$gender.clean = "Other"
  round1[round1$gender.clean == "",]$gender.clean = "Other"
  round1[round1$gender.clean == "non conforming",]$gender.clean = "Other"
  
  #new
  #round1[round1$gender.clean == "Agender; Femme;",]$gender.clean = "Other"
  #round1[round1$gender.clean == "Non-binary; ",]$gender.clean = "Other"
  
  # drop levels
  round1$gender.clean <- droplevels(round1$gender.clean)
  
  table(round1$gender.clean)
  
  return(round1$gender.clean)
}

# race
race.nih <- function(round1){
  #round1 = round1_match
  #round1 <- droplevels(round1)
  race.nih = round1$race_ethnicity
  
  # cleaning up asian category
  race.nih[grep("Cambodian", race.nih)] = "Asian"
  
  round1$race.nih = race.nih
  
  # american indian
  levels(round1$race.nih) <- c(levels(round1$race.nih),"American Indian")
  round1[round1$race.nih == "Latinx and  Native American",]$race.nih = "American Indian"
  round1[round1$race.nih == "Mexican American;Southern American Indigenous Person;Zapotec",]$race.nih = "American Indian"
  round1[round1$race.nih == "Black Indians in the United States",]$race.nih = "American Indian"
  round1[round1$race.nih == "Native American;Spanish",]$race.nih = "American Indian"
  
  levels(round1$race.nih) <- c(levels(round1$race.nih),"NR")
  round1[round1$race.nih == "Hispanic/Latino",]$race.nih = "NR"
  round1[round1$race.nih == "Latina/ Hispanic",]$race.nih = "NR"
  round1[round1$race.nih == "Latino",]$race.nih = "NR"
  round1[round1$race.nih == "Mexican",]$race.nih = "NR"
  round1[round1$race.nih == "Hispanic",]$race.nih = "NR"
  round1[round1$race.nih == "Latino/a American",]$race.nih = "NR"
  round1[round1$race.nih == "Mexican American",]$race.nih = "NR"
  round1[round1$race.nih == "Mexican American;Latino/a American;Hispanic",]$race.nih = "NR"
  round1[round1$race.nih == "hispanic",]$race.nih = "NR"
  round1[round1$race.nih == "Hispanic;Cuban American",]$race.nih = "NR"
  round1[round1$race.nih == "Mexican-American",]$race.nih = "NR"
  round1[round1$race.nih == "mexican",]$race.nih = "NR"
  round1[round1$race.nih == "Pocho.",]$race.nih = "NR"
  round1[round1$race.nih == "Puerto Rican;Aruban",]$race.nih = "NR"
  round1[round1$race.nih == "BELIZEAN",]$race.nih = "NR"
  round1[round1$race.nih == "Latin",]$race.nih = "NR"
  
  round1[round1$race.nih == "Angelino",]$race.nih = "NR"
  round1[round1$race.nih == "brown",]$race.nih = "NR"
  round1[round1$race.nih == "human",]$race.nih = "NR"
  round1[round1$race.nih == "human race american born mexican decent",]$race.nih = "NR"
  
  levels(round1$race.nih) <- c(levels(round1$race.nih),"NA")
  round1[round1$race.nih == "na",]$race.nih = "NA"
  
  # multiple races, create new variable
  levels(round1$race.nih) <- c(levels(round1$race.nih),"Multiple Races")
  round1[round1$race.nih == "Black;Native American;Irish",]$race.nih = "Multiple Races"
  round1[round1$race.nih == "Caucasian;African American (Nigerian)",]$race.nih = "Multiple Races"
  round1[round1$race.nih == "White;Fillipino;Spanish",]$race.nih = "Multiple Races"
  round1[round1$race.nih == "Black;White;French",]$race.nih = "Multiple Races"
  round1[round1$race.nih == "azteca, cheeroke, spainard, cacasauion.",]$race.nih = "Multiple Races"
  round1[round1$race.nih == "Guyanese;Black-American;Cree Tribe Native American;Dutch;Irish;Jewish;Chinese",]$race.nih = "Multiple Races"
  round1[round1$race.nih == "African-American;Caucasian",]$race.nih = "Multiple Races"
  round1[round1$race.nih == "White,Souix Indian, Sicilian/Italian",]$race.nih = "Multiple Races"
  round1[round1$race.nih == "Han Chinese;Asian;Caucasian",]$race.nih = "Multiple Races"
  round1[round1$race.nih == "Chinese;White",]$race.nih = "Multiple Races"
  
  # specific answers for black 
  round1[round1$race.nih == "African America;Jamaican;Panamanian",]$race.nih = "Black"
  round1[round1$race.nih == "African-American",]$race.nih = "Black"
  round1[round1$race.nih == "African-American;Hispanic",]$race.nih = "Black"
  round1[round1$race.nih == "African-American;Nigerian",]$race.nih = "Black"
  
  round1[round1$race.nih == "African American",]$race.nih = "Black"
  round1[round1$race.nih == "afro american",]$race.nih = "Black"
  round1[round1$race.nih == "AFRO AMERICAN",]$race.nih = "Black"
  round1[round1$race.nih == "American Jewish;Black",]$race.nih = "Black"
  round1[round1$race.nih == "black",]$race.nih = "Black"
  round1[round1$race.nih == "Black American",]$race.nih = "Black"
  round1[round1$race.nih == "Black Jewish",]$race.nih = "Black"
  round1[round1$race.nih == "Black;Caribbean Africa SPINASH",]$race.nih = "Black"
  
  # specifc asian
  round1[round1$race.nih == "asian",]$race.nih = "Asian"
  # round1[round1$race.nih == "Asian; Chinese",]$race.nih = "Asian"
  round1[round1$race.nih == "Asian;Indian",]$race.nih = "Asian"
  round1[round1$race.nih == "Indian",]$race.nih = "Asian"
  round1[round1$race.nih == "Taiwanese",]$race.nih = "Asian"
  round1[round1$race.nih == "Taiwanese-American",]$race.nih = "Asian"
  round1[round1$race.nih == "Thai  (Southeast Asian)",]$race.nih = "Asian"
  round1[round1$race.nih == "Thai;Filipino",]$race.nih = "Asian"
  round1[round1$race.nih == "South Asian",]$race.nih = "Asian"
  round1[round1$race.nih == "East Asian/Asian-American",]$race.nih = "Asian"
  round1[round1$race.nih == "asian american",]$race.nih = "Asian"
  round1[round1$race.nih == "Asian American",]$race.nih = "Asian"
  round1[round1$race.nih == "Asian;Chinese",]$race.nih = "Asian"
  round1[round1$race.nih == "Asian;Korean",]$race.nih = "Asian"
  round1[round1$race.nih == "Asian;Korean American",]$race.nih = "Asian"
  round1[round1$race.nih == "Korean",]$race.nih = "Asian"
  round1[round1$race.nih == "Japanese descendant",]$race.nih = "Asian"
  round1[round1$race.nih == "Indian American",]$race.nih = "Asian"
  round1[round1$race.nih == "Indian;South Asian",]$race.nih = "Asian"
  round1[round1$race.nih == "Han Chinese",]$race.nih = "Asian"
  round1[round1$race.nih == "Asian Pacific American",]$race.nih = "Asian"
  round1[round1$race.nih == "filipino",]$race.nih = "Asian"
  round1[round1$race.nih == "Chinese American",]$race.nih = "Asian"
  round1[round1$race.nih == "Chinese-American",]$race.nih = "Asian"
  round1[round1$race.nih == "Latino/a American;South Asian",]$race.nih = "Asian"
  round1[round1$race.nih == "Filipino;Latin",]$race.nih = "Asian"
  round1[round1$race.nih == "Asian-Indian",]$race.nih = "Asian"
  round1[round1$race.nih == "Asian Pacific American;Spanish;Chinese",]$race.nih = "Asian"

  # specific white
  round1[round1$race.nih == "white",]$race.nih = "White"
  round1[round1$race.nih == "Hispanic;Latina;Peruvian;Italian",]$race.nih = "White"
  round1[round1$race.nih == "White;Hispanic",]$race.nih = "White"
  round1[round1$race.nih == "White;Hispanic;Latino/a American",]$race.nih = "White"
  round1[round1$race.nih == "White;Latina",]$race.nih = "White"
  round1[round1$race.nih == "Caucasian",]$race.nih = "White"
  round1[round1$race.nih == "Egyptian American",]$race.nih = "White"
  round1[round1$race.nih == "white caucasian",]$race.nih = "White"
  round1[round1$race.nih == "white Caucasian",]$race.nih = "White"
  round1[round1$race.nih == "white latina",]$race.nih = "White"
  round1[round1$race.nih == "White-Hispanic;Latino",]$race.nih = "White"
  round1[round1$race.nih == "White/Caucasian",]$race.nih = "White"
  round1[round1$race.nih == "write",]$race.nih = "White"
  round1[round1$race.nih == "Caucasion",]$race.nih = "White"
  round1[round1$race.nih == "Swiss;Greek;Canadian;Russian",]$race.nih = "White"
  round1[round1$race.nih == "Middle Eastern",]$race.nih = "White"
  round1[round1$race.nih == "Persian",]$race.nih = "White"
  round1[round1$race.nih == "European Caucasian",]$race.nih = "White"
  round1[round1$race.nih == "Caucasian;Middle Eastern",]$race.nih = "White"
  round1[round1$race.nih == "Italian American;Hungarian;German American",]$race.nih = "White"
  round1[round1$race.nih == "Italian;Colombian;Chilean;Spanish;Caucasian;Latinx",]$race.nih = "White"
  round1[round1$race.nih == "Hispanic;Puerto Rican;White;Irish;German;English",]$race.nih = "White"
  
  # drop levels
  round1$race.nih <- droplevels(round1$race.nih)
  table(round1$race.nih)
  
  return (round1$race.nih)
}

# ethnicity
ethnicity.nih <- function(round1){
  ethnicity.nih = round1$race_ethnicity
  
  round1$ethnicity.nih = ethnicity.nih
  
  #Not Hispanic/Creating a New Level
  
  levels(round1$ethnicity.nih) <- c(levels(round1$ethnicity.nih),"Not Hispanic")
  round1[round1$ethnicity.nih == "Black Indians in the United States",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Black;Native American;Irish",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Asian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Cambodian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Black",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "White",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Caucasian;African American (Nigerian)",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "African America;Jamaican;Panamanian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "African-American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Black;White;French",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Guyanese;Black-American;Cree Tribe Native American;Dutch;Irish;Jewish;Chinese",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "African-American;Caucasian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "White,Souix Indian, Sicilian/Italian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Han Chinese;Asian;Caucasian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Chinese;White",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "African-American;Nigerian",]$ethnicity.nih = "Not Hispanic"  
  round1[round1$ethnicity.nih == "African American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "afro american",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "AFRO AMERICAN",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "American Jewish;Black",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "black",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Black American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Black Jewish",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "asian",]$ethnicity.nih = "Not Hispanic"
  # round1[round1$ethnicity.nih == "Asian; Chinese",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Asian;Indian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Indian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Taiwanese",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Taiwanese-American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Thai  (Southeast Asian)",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "South Asian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "East Asian/Asian-American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "asian american",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Asian American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Asian;Chinese",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Asian;Korean",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Asian;Korean American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Korean",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Japanese descendant",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Indian American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Indian;South Asian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Han Chinese",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Asian Pacific American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Chinese American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Chinese-American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Asian-Indian",]$ethnicity.nih = "Not Hispanic"
  
  round1[round1$ethnicity.nih == "white",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Caucasian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Egyptian American",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "white caucasian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "white Caucasian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "White/Caucasian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "write",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Caucasion",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Swiss;Greek;Canadian;Russian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Middle Eastern",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Persian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "European Caucasian",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Caucasian;Middle Eastern",]$ethnicity.nih = "Not Hispanic"
  round1[round1$ethnicity.nih == "Italian American;Hungarian;German American",]$ethnicity.nih = "Not Hispanic"
  
  #Hispanic 
  round1[round1$ethnicity.nih == "Hispanic/Latino",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Latina/ Hispanic",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Latino",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Mexican",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Hispanic",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Latino/a American",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Mexican American",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Mexican American;Latino/a American;Hispanic",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "hispanic",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Hispanic;Cuban American",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Mexican-American",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "mexican",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Pocho.",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Puerto Rican;Aruban",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Hispanic;Latina;Peruvian;Italian",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "White;Hispanic",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "White;Hispanic;Latino/a American",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "White;Latina",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Latinx and  Native American",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Mexican American;Southern American Indigenous Person;Zapotec",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Native American;Spanish",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "white latina",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "White-Hispanic;Latino",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Italian;Colombian;Chilean;Spanish;Caucasian;Latinx",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Hispanic;Puerto Rican;White;Irish;German;English",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Thai;Filipino",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Latino/a American;South Asian",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Filipino;Latin",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "filipino",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "White;Fillipino;Spanish",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Black;Caribbean Africa SPINASH",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "African-American;Hispanic",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "human race american born mexican decent",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "azteca, cheeroke, spainard, cacasauion.",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Latin",]$ethnicity.nih = "Hispanic"
  round1[round1$ethnicity.nih == "Asian Pacific American;Spanish;Chinese",]$ethnicity.nih = "Hispanic"
  
  
  
  # no report  
  levels(round1$ethnicity.nih) <- c(levels(round1$ethnicity.nih),"NR")
  round1[round1$ethnicity.nih == "Angelino",]$ethnicity.nih = "NR"
  round1[round1$ethnicity.nih == "brown",]$ethnicity.nih = "NR"
  round1[round1$ethnicity.nih == "human",]$ethnicity.nih = "NR"
  round1[round1$ethnicity.nih == "BELIZEAN",]$ethnicity.nih = "NR"
  
  levels(round1$ethnicity.nih) <- c(levels(round1$ethnicity.nih),"NA")
  round1[round1$ethnicity.nih == "na",]$ethnicity.nih = "NA"
  
  
  round1$ethnicity.nih <- droplevels(round1$ethnicity.nih)
  table(round1$ethnicity.nih)
  
  return (round1$ethnicity.nih)
}



#Running the functions

# sexual orientation
sex_or<-sex_or(round1)

#languages
language.clean <-language.clean(round1)

#gender
gender.clean <-gender.clean(round1)

# race
race.nih <- race.nih(round1)

#ethnicity
ethnicity.nih <- ethnicity.nih(round1)

# cbind to old dataframe, renaming to new dataframe, 
cleaned_round1_all <-cbind(round1, sex_or, language.clean, gender.clean, race.nih, ethnicity.nih)


# CLUSTER ADDING TO DATAFRAME NOT NEEDED

#selecting the cluster columns
# study1_all_clusters1 <- study1_all_clusters %>%
#   select(qualtrics_id, lda.cluster.id, bow.cluster.id, items.cluster.id, types.cluster.id)
# 
# #right join
# round1_clean_new<-right_join(study1_all_clusters1, round1_clean, by="qualtrics_id")


#EXPORT THE CSV
# renamed from updated_round1_clean_new.csv
write.csv(cleaned_round1_all, 'clean_round1_all.csv')


