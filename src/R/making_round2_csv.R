library(dplyr)
library(plyr) #to rename the column names

# import the csv file
# all data from round1 and round2
read.csv("all_participants.csv")

# creating 2 different dataframes for round 1 and round 2
#round1 <-filter(all_participants, data_collection_group == "round1")
round2 <-filter(all_participants, data_collection_group == "round2")

round2<-droplevels(round2)

round2[is.na(round2)] <- ""

# cleaning up demographics: Sexual Orientation, Gender, Race, Ethnicity, Language


# Sexual Orientation
sex_or <- function(round2){
  # sexual orientation
  or.clean = round2$sexual_orientation
  # Straight
  or.clean[grep("Straight", or.clean)] = "Straight"
  or.clean[grep("STARIGHT", or.clean)] = "Straight"
  or.clean[grep("STRAIGHT", or.clean)] = "Straight"
  or.clean[grep("strait", or.clean)] = "Straight"
  or.clean[grep("straight", or.clean)] = "Straight"
  or.clean[grep("[Hh]etero", or.clean)] = "Straight"
  or.clean[grep("HETRO", or.clean)] = "Straight"
  or.clean[grep("wemen", or.clean)] = "Straight"
  #female
  # or.clean[grep("^F", or.clean)] = "Female"
  or.clean[grep("female", or.clean)] = "Female"
  #gay
  or.clean[grep("ay", or.clean)] = "Gay"
  or.clean[grep("[Hh]omo", or.clean)] = "Gay"
  #bi
  or.clean[grep("bi", or.clean)] = "Bisexual"
  # queer
  #or.clean[grep("Queer", or.clean)] = "Queer; "
  
  round2$or.clean = or.clean
  
  # new straight, female gender, men sex.or
  round2[round2$or.clean == "men" & round2$gender == "female",]$or.clean = "Straight"
  
  # New level for LGBTQIA+
  levels(round2$or.clean) <- c(levels(round2$or.clean),"LGBTQIA+")
  # bisexual
  round2[round2$or.clean == "Bisexual",]$or.clean = "LGBTQIA+"
  # bi/pan
  round2[round2$or.clean == "Bi/pan",]$or.clean = "LGBTQIA+"
  # Pansexul
  round2[round2$or.clean == "Pansexual",]$or.clean = "LGBTQIA+"
  # Gay
  round2[round2$or.clean == "Gay",]$or.clean = "LGBTQIA+"
  # Queer
  round2[round2$or.clean == "Queer",]$or.clean = "LGBTQIA+"
  
  round2[round2$or.clean == "Pansexual;Polyamorous",]$or.clean = "LGBTQIA+"
  round2[round2$or.clean == "Queer;Bisexual",]$or.clean = "LGBTQIA+"
  round2[round2$or.clean == "Queer;Pansexual",]$or.clean = "LGBTQIA+"
  
  # New level for no response
  levels(round2$or.clean) <- c(levels(round2$or.clean),"NR")
  round2[round2$or.clean == "???",]$or.clean = "NR"
  round2[round2$or.clean == "Bicurious",]$or.clean = "NR"
  round2[round2$or.clean == "hedralsex",]$or.clean = "NR"
  round2[round2$or.clean == "Menosexual",]$or.clean = "NR"
  round2[round2$or.clean == "Questioning",]$or.clean = "NR"
  round2[round2$or.clean == "Unsure",]$or.clean = "NR"
  round2[round2$or.clean == "male",]$or.clean = "NR"
  round2[round2$or.clean == "Male",]$or.clean = "NR"
  round2[round2$or.clean == "Female",]$or.clean = "NR"
  round2[round2$or.clean == "On the Kinsey scale? I'm married to a woman",]$or.clean = "NR"
  round2[round2$or.clean == "karma sutra",]$or.clean = "NR"
  
  
  #No answer, new level
  levels(round2$or.clean) <- c(levels(round2$or.clean),"NA")
  round2[round2$or.clean == "void",]$or.clean = "NA"
  round2[round2$or.clean == "void survey",]$or.clean = "NA"
  round2[round2$or.clean == "",]$or.clean = "NA"
  
  # transgender gender and men sex.or
  round2[round2$or.clean == "men" & round2$gender== "trangernder",]$or.clean = "NR"
  
  
  #drop levels
  round2$or.clean <- droplevels(round2$or.clean)
  ?table(round2$or.clean)
  
  # return a value in the function
  return(round2$or.clean)
}

#gender
gender.clean <- function(round2){
  gender.clean = round2$gender
  
  # dealing with all the males
  gender.clean[grep("^[Mm]",gender.clean)] = "Male"
  #gender.clean[grep("trans", gender.clean)] = "male"
  #gender.clean[grep("Identify",gender.clean)] = "male"
  # new
  gender.clean[grep("Cisgender", gender.clean)] = "Male"
  gender.clean[grep("FTM", gender.clean)] = "Male"
  
  # dealing with all the females
  gender.clean[grep("^F",gender.clean)] = "Female"
  gender.clean[grep("^f", gender.clean)] = "Female"
  gender.clean[grep("ema", gender.clean)] = "Female"
  # gender.clean[grep("women", gender.clean)] = "female"
  
  #new
  gender.clean[grep("mail", gender.clean)] = "Female"
  
  round2$gender.clean = gender.clean
  
  #dealing with other
  levels(round2$gender.clean) <- c(levels(round2$gender.clean),"NR")
  #new
  round2[round2$gender.clean == "Agender;Femme",]$gender.clean = "NR"
  round2[round2$gender.clean == "every race mixed in one man",]$gender.clean = "NR"
  round2[round2$gender.clean == "Genderfluid (fem/nb)",]$gender.clean = "NR"
  round2[round2$gender.clean == "Non-binary",]$gender.clean = "NR"
  round2[round2$gender.clean == "trangernder",]$gender.clean = "NR"
  round2[round2$gender.clean == "Woman, most of the time",]$gender.clean = "NR"
  round2[round2$gender.clean == "women",]$gender.clean = "NR"
  round2[round2$gender.clean == "",]$gender.clean = "NR"
  
  # no answer, new level
  levels(round2$gender.clean) <- c(levels(round2$gender.clean),"Void")
  round2[round2$gender.clean == "gendervoid",]$gender.clean = "Void"
  round2[round2$gender.clean == "Gendervoid",]$gender.clean = "Void"
  round2[round2$gender.clean == "void survey",]$gender.clean = "Void"
  
  
  
  #round2$gender.clean = gender.clean
  # getting results
  # round2$gender.clean = gender.clean
  # count(round2$gender.clean)
  
  # drop levels
  round2$gender.clean <- droplevels(round2$gender.clean)
  table(round2$gender.clean)
  
  return(round2$gender.clean)
}

#ethnicity
ethnicity.nih <- function(round2){
  ethnicity.nih = round2$race_ethnicity
  
  ethnicity.nih[grep("African Amercan", ethnicity.nih)] = "Black"
  
  round2$ethnicity.nih = ethnicity.nih
  
  
  round2[round2$ethnicity.nih == "American Indian;Mexican American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Hispanic/Latina;Jamaican;White;Chinese",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "White;Filipino",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Puerto Rican;Polish;German",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Filipino;Portuguese;Spanish",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "African-American;Native American;Filipino",]$ethnicity.nih = "Hispanic"
  #****round2[round2$ethnicity.nih == "Indigenous Maya;Chinese",]$ethnicity.nih = "Multiple Races" ***
  round2[round2$ethnicity.nih == "Cuban American;Creole;German American;Native American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Chicano",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Hispanic;Mexican American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Latin",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Latina",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Latino/a American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Latinx",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Hispanic;Latino/a American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Latin;Latino/a American;Hispanic",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "latino",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "latinx",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Mexican American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Mexican;Hispanic",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Mexican",]$ethnicity.nih = "Hispanic"  
  round2[round2$ethnicity.nih == "Peruvian;Persian",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Mexican American;Persian",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "white;Hispanic",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Hispanic White",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Caucasian;Latino/a American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "White;Hispanic",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "White;Hispanic;Latino/a American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "White;Mexican American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "White;Hispanic;European",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "White;Latino/a American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "White;Panamanian",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Hispanic;White",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Latin;Brazilian;White",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "flipino",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Filipino",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Filipino American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Asian;Hispanic",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Mexican;Black",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Hispanic;Black",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Black;Haitian American",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Hispanic;Indian",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Portuguese;German",]$ethnicity.nih = "Hispanic"
  round2[round2$ethnicity.nih == "Indigenous Maya;Chinese",]$ethnicity.nih = "Hispanic"
  
  levels(round2$ethnicity.nih) <- c(levels(round2$ethnicity.nih),"Not Hispanic")
  #round2[round2$ethnicity.nih == "Black",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "African-American",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "Black",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Black;African-American;Brown-American",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Hebrew;Isralite;Black",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "African Amercan",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "African America",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "African American / Black",]$ethnicity.nih= "Not Hispanic"
  round2[round2$ethnicity.nih == "African American, Jamacian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "AFRIKAN",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Afro-Canadian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "black",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Black of African descent",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Black;African American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Black;Christian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Black/ African American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Ethiopian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Negro",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "African American",]$ethnicity.nih = "Not Hispanic"
  
  
  # specifc asian
  #round2[round2$ethnicity.nih == "Asian",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "korean",]$ethnicity.nih = "Not Hispanic"
  
  # round2[round2$ethnicity.nih == "flipino",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Indian",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "Asian",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Asian;Chinese",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "Chinese",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Asian;I'm Chinese citizenship.",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Chinese",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "Indian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Indian;Asian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "asian american",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "asian",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "asian american",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Asian American;Taiwanese;Chinese",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Asian;Korean",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Asian;Vietnamese",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Chinese American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Chinese malaysian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "East Asian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "East Asian;Chinese American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "East Asian;Asian American",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Han Chinese",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "japanes",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Korean",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Korean American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Korean;American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Japanese",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Korean;Asian;Asian-American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "South Asian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "South Indian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Sri Lankan",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Taiwanese",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Taiwanese American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Thai",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Thai, Cambodian, chinese",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Vietnamese",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Asia-Born",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Asian American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Indian American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Indian;Pakistani",]$ethnicity.nih = "Not Hispanic"
  
  
  # specific white
  round2[round2$ethnicity.nih == "white",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "White",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "white",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Caucasion white",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "European American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Middle Eastern",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "Puerto Rican;Polish;German",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "White",]$ethnicity.nih = "Not Hispanic"
  
  #round2[round2$ethnicity.nih == "White",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Creole;White",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;American Jewish",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "caucasian",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Caucasian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Caucasian;Middle Eastern",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Caucasian;European",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "Caucasian",]$ethnicity.nih = "Not Hispanic"
  
  
  round2[round2$ethnicity.nih == "Non-Hispanic Whites",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "white;jewish;Israeli",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White/caucasian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;Russian American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;Italian American;Sephardi Jewish",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;European;Russian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;European;Ashkenazi Jewish",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;Ashkenazi Jewish",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;Armenian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;American;French;Turkish;Egyptian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White (Middle Eastern);Afghan American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Middle Eastern;Coptic",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Italian American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Irish American;German American;Polish American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Armenian",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Armenian American;Assyrian;White",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Ashkenazi Jewish;Israeli",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Egyptian American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "caucasion, non hispanic",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "caucasion",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Caucasian;White;Russian",]$ethnicity.nih = "Not Hispanic"
  
  
  # specific hispanic
  
  
  # Pacific Islander
  #levels(round2$ethnicity.nih) <- c(levels(round2$ethnicity.nih),"Not Hispanic")
  round2[round2$ethnicity.nih == "Pacific Islander",]$ethnicity.nih = "Not Hispanic"
  
  # multiple races
  #levels(round2$ethnicity.nih) <- c(levels(round2$ethnicity.nih),"Not Hispanic")
  round2[round2$ethnicity.nih == "Black-Amerindian",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Native American;White",]$ethnicity.nih = "Not Hispanic"
  
  
  round2[round2$ethnicity.nih == "Black;Chinese Malay",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Black;Middle Eastern;Indian",]$ethnicity.nih = "Not Hispanic" 
  round2[round2$ethnicity.nih == "African-American;Native American;Irish American;Creole",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Asian-American;White",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Chinese;White",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Asian;Caucasian",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Asian Pacific American;Pacific Islander;Caucasian",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "White-Amerindian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;East Asain",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "White;Black",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;Guamanian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;Asian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Caucasian;Pacific Islander",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Caucasian;Native Hawaiian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White;American Indian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Pacific Islander;White",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Asian Pacific American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "white;asian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "white;korean",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "African America;Black;Native American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "African American;Native American",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Japanese;Indian;German;caucasian",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "African American;Native American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Half Japanese and Half Iranian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "black;white;Non-Hispanic Whites",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Thai;Vietnamese;Irish American",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "I have white skin, my family claims mixed European heritage with some indigenous Maori.",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Arab American;African-American",]$ethnicity.nih = "Not Hispanic"
  
  # american indian
  round2[round2$ethnicity.nih == "Amerindian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Native American/Moorish",]$ethnicity.nih = "Not Hispanic"
  
  round2[round2$ethnicity.nih == "Asian",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "Black",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "White",]$ethnicity.nih = "Not Hispanic"
  
  # No Report
  levels(round2$ethnicity.nih) <- c(levels(round2$ethnicity.nih),"NR")
  round2[round2$ethnicity.nih == "god",]$ethnicity.nih = "NR"
  round2[round2$ethnicity.nih == "Honestly, who knows?",]$ethnicity.nih = "NR"
  round2[round2$ethnicity.nih == "Mixed",]$ethnicity.nih = "NR"
  round2[round2$ethnicity.nih == "Multiple",]$ethnicity.nih = "NR"
  round2[round2$ethnicity.nih == "colored",]$ethnicity.nih = "NR"
  #round2[round2$ethnicity.nih == "void survey;",]$ethnicity.nih = "Not Hispanic"
  #round2[round2$ethnicity.nih == "void;",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "mixed",]$ethnicity.nih = "NR"
  #round2[round2$ethnicity.nih == "He's 56 actually; void;",]$ethnicity.nih = "Not Hispanic"
  round2[round2$ethnicity.nih == "-",]$ethnicity.nih = "NR"
  round2[round2$ethnicity.nih == "Moore",]$ethnicity.nih = "NR"
  
  levels(round2$ethnicity.nih) <- c(levels(round2$ethnicity.nih), "Void")
  round2[round2$ethnicity.nih == "He's 56 actually;void",]$ethnicity.nih = "Void"
  round2[round2$ethnicity.nih == "void",]$ethnicity.nih = "Void"
  round2[round2$ethnicity.nih == "void survey",]$ethnicity.nih = "Void"
  
  round2$ethnicity.nih <- droplevels(round2$ethnicity.nih)
  table(round2$ethnicity.nih)
  
  return(round2$ethnicity.nih)
}


#round2$race_ethnicity[is.na(round2$race_ethnicity)] <- ""
# race
race.nih <- function(round2){
  # race together 
  race.nih = round2$race_ethnicity
  
  # cleaning up the black category
  race.nih[grep("African Amercan", race.nih)] = "Black"
  
  round2$race.nih = race.nih
  
  
  #specific black
  round2[round2$race.nih == "Black;Haitian American",]$race.nih = "Black"
  #round2[round2$race.nih == "Black",]$race.nih = "Black"
  round2[round2$race.nih == "African-American",]$race.nih = "Black"
  #round2[round2$race.nih == "Black",]$race.nih = "Black"
  round2[round2$race.nih == "Hispanic;Black",]$race.nih = "Black"
  round2[round2$race.nih == "Black;African-American;Brown-American",]$race.nih = "Black"
  round2[round2$race.nih == "Mexican;Black",]$race.nih = "Black"
  round2[round2$race.nih == "Hebrew;Isralite;Black",]$race.nih = "Black"
  #round2[round2$race.nih == "African Amercan",]$race.nih = "Black"
  round2[round2$race.nih == "African America",]$race.nih = "Black"
  round2[round2$race.nih == "African American / Black",]$race.nih= "Black"
  round2[round2$race.nih == "African American, Jamacian",]$race.nih = "Black"
  round2[round2$race.nih == "AFRIKAN",]$race.nih = "Black"
  round2[round2$race.nih == "Afro-Canadian",]$race.nih = "Black"
  round2[round2$race.nih == "black",]$race.nih = "Black"
  round2[round2$race.nih == "Black of African descent",]$race.nih = "Black"
  round2[round2$race.nih == "Black;African American",]$race.nih = "Black"
  round2[round2$race.nih == "Black;Christian",]$race.nih = "Black"
  round2[round2$race.nih == "Black/ African American",]$race.nih = "Black"
  round2[round2$race.nih == "Ethiopian",]$race.nih = "Black"
  round2[round2$race.nih == "Negro",]$race.nih = "Black"
  round2[round2$race.nih == "African American",]$race.nih = "Black"
  
  
  # specifc asian
  #round2[round2$race.nih == "Asian",]$race.nih = "Asian"
  round2[round2$race.nih == "Asian;Hispanic",]$race.nih = "Asian"
  round2[round2$race.nih == "korean",]$race.nih = "Asian"
  round2[round2$race.nih == "Filipino American",]$race.nih = "Asian"
  # round2[round2$race.nih == "flipino",]$race.nih = "Asian"
  round2[round2$race.nih == "Indian",]$race.nih = "Asian"
  #round2[round2$race.nih == "Asian",]$race.nih = "Asian"
  round2[round2$race.nih == "Filipino",]$race.nih = "Asian"
  round2[round2$race.nih == "Asian;Chinese",]$race.nih = "Asian"
  #round2[round2$race.nih == "Chinese",]$race.nih = "Asian"
  round2[round2$race.nih == "Asian;I'm Chinese citizenship.",]$race.nih = "Asian"
  round2[round2$race.nih == "Chinese",]$race.nih = "Asian"
  round2[round2$race.nih == "Hispanic;Indian",]$race.nih = "Asian"
  #round2[round2$race.nih == "Indian",]$race.nih = "Asian"
  round2[round2$race.nih == "Indian;Asian",]$race.nih = "Asian"
  round2[round2$race.nih == "asian american",]$race.nih = "Asian"
  round2[round2$race.nih == "asian",]$race.nih = "Asian"
  #round2[round2$race.nih == "asian american",]$race.nih = "Asian"
  round2[round2$race.nih == "Asian American;Taiwanese;Chinese",]$race.nih = "Asian"
  round2[round2$race.nih == "Asian;Korean",]$race.nih = "Asian"
  round2[round2$race.nih == "Asian;Vietnamese",]$race.nih = "Asian"
  round2[round2$race.nih == "Chinese American",]$race.nih = "Asian"
  round2[round2$race.nih == "Chinese malaysian",]$race.nih = "Asian"
  round2[round2$race.nih == "East Asian",]$race.nih = "Asian"
  round2[round2$race.nih == "East Asian;Chinese American",]$race.nih = "Asian"
  round2[round2$race.nih == "East Asian;Asian American",]$race.nih = "Asian"
  round2[round2$race.nih == "flipino",]$race.nih = "Asian"
  round2[round2$race.nih == "Han Chinese",]$race.nih = "Asian"
  round2[round2$race.nih == "japanes",]$race.nih = "Asian"
  round2[round2$race.nih == "Korean",]$race.nih = "Asian"
  round2[round2$race.nih == "Korean American",]$race.nih = "Asian"
  round2[round2$race.nih == "Korean;American",]$race.nih = "Asian"
  round2[round2$race.nih == "Japanese",]$race.nih = "Asian"
  round2[round2$race.nih == "Korean;Asian;Asian-American",]$race.nih = "Asian"
  round2[round2$race.nih == "South Asian",]$race.nih = "Asian"
  round2[round2$race.nih == "South Indian",]$race.nih = "Asian"
  round2[round2$race.nih == "Sri Lankan",]$race.nih = "Asian"
  round2[round2$race.nih == "Taiwanese",]$race.nih = "Asian"
  round2[round2$race.nih == "Taiwanese American",]$race.nih = "Asian"
  round2[round2$race.nih == "Thai",]$race.nih = "Asian"
  round2[round2$race.nih == "Thai, Cambodian, chinese",]$race.nih = "Asian"
  round2[round2$race.nih == "Vietnamese",]$race.nih = "Asian"
  round2[round2$race.nih == "Asia-Born",]$race.nih = "Asian"
  round2[round2$race.nih == "Asian American",]$race.nih = "Asian"
  round2[round2$race.nih == "Indian American",]$race.nih = "Asian"
  round2[round2$race.nih == "Indian;Pakistani",]$race.nih = "Asian"
  
  
  # specific white
  round2[round2$race.nih == "white",]$race.nih = "White"
  #round2[round2$race.nih == "White",]$race.nih = "White"
  #round2[round2$race.nih == "white",]$race.nih = "White"
  round2[round2$race.nih == "Caucasion white",]$race.nih = "White"
  round2[round2$race.nih == "European American",]$race.nih = "White"
  round2[round2$race.nih == "Middle Eastern",]$race.nih = "White"
  #round2[round2$race.nih == "Puerto Rican;Polish;German",]$race.nih = "White"
  #round2[round2$race.nih == "White",]$race.nih = "White"
  round2[round2$race.nih == "Hispanic;White",]$race.nih = "White"
  #round2[round2$race.nih == "Hispanic White",]$race.nih = "White"
  round2[round2$race.nih == "Latin;Brazilian;White",]$race.nih = "White"
  #round2[round2$race.nih == "White",]$race.nih = "White"
  round2[round2$race.nih == "White;Hispanic",]$race.nih = "White"
  round2[round2$race.nih == "White;Hispanic;Latino/a American",]$race.nih = "White"
  round2[round2$race.nih == "White;Mexican American",]$race.nih = "White"
  #round2[round2$race.nih == "White;Hispanic",]$race.nih = "White"
  round2[round2$race.nih == "White;Hispanic;European",]$race.nih = "White"
  round2[round2$race.nih == "White;Latino/a American",]$race.nih = "White"
  round2[round2$race.nih == "White;Panamanian",]$race.nih = "White"
  round2[round2$race.nih == "Creole;White",]$race.nih = "White"
  round2[round2$race.nih == "White;American Jewish",]$race.nih = "White"
  round2[round2$race.nih == "caucasian",]$race.nih = "White"
  round2[round2$race.nih == "Caucasian;Latino/a American",]$race.nih = "White"
  round2[round2$race.nih == "Caucasian",]$race.nih = "White"
  round2[round2$race.nih == "Caucasian;Middle Eastern",]$race.nih = "White"
  round2[round2$race.nih == "Caucasian;European",]$race.nih = "White"
  #round2[round2$race.nih == "Caucasian",]$race.nih = "White"
  round2[round2$race.nih == "Portuguese;German",]$race.nih = "White"
  round2[round2$race.nih == "Hispanic White",]$race.nih = "White"
  round2[round2$race.nih == "Non-Hispanic Whites",]$race.nih = "White"
  round2[round2$race.nih == "white;Hispanic",]$race.nih = "White"
  round2[round2$race.nih == "white;jewish;Israeli",]$race.nih = "White"
  round2[round2$race.nih == "White/caucasian",]$race.nih = "White"
  round2[round2$race.nih == "White;Russian American",]$race.nih = "White"
  round2[round2$race.nih == "White;Italian American;Sephardi Jewish",]$race.nih = "White"
  round2[round2$race.nih == "White;European;Russian",]$race.nih = "White"
  round2[round2$race.nih == "White;European;Ashkenazi Jewish",]$race.nih = "White"
  round2[round2$race.nih == "White;Ashkenazi Jewish",]$race.nih = "White"
  round2[round2$race.nih == "White;Armenian",]$race.nih = "White"
  round2[round2$race.nih == "White;American;French;Turkish;Egyptian",]$race.nih = "White"
  round2[round2$race.nih == "White (Middle Eastern);Afghan American",]$race.nih = "White"
  round2[round2$race.nih == "Middle Eastern;Coptic",]$race.nih = "White"
  round2[round2$race.nih == "Italian American",]$race.nih = "White"
  round2[round2$race.nih == "Irish American;German American;Polish American",]$race.nih = "White"
  round2[round2$race.nih == "Armenian",]$race.nih = "White"
  round2[round2$race.nih == "Mexican American;Persian",]$race.nih = "White"
  round2[round2$race.nih == "Armenian American;Assyrian;White",]$race.nih = "White"
  round2[round2$race.nih == "Ashkenazi Jewish;Israeli",]$race.nih = "White"
  round2[round2$race.nih == "Egyptian American",]$race.nih = "White"
  round2[round2$race.nih == "caucasion, non hispanic",]$race.nih = "White"
  round2[round2$race.nih == "caucasion",]$race.nih = "White"
  round2[round2$race.nih == "Caucasian;White;Russian",]$race.nih = "White"
  round2[round2$race.nih == "Peruvian;Persian",]$race.nih = "White"
  
  # specific hispanic
  round2[round2$race.nih == "Chicano",]$race.nih = "Hispanic"
  #round2[round2$race.nih == "Hispanic",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Hispanic;Mexican American",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Latin",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Latina",]$race.nih = "Hispanic"
  #round2[round2$race.nih == "Latina",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Latino/a American",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Latinx",]$race.nih = "Hispanic"
  #round2[round2$race.nih == "Hispanic",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Hispanic;Latino/a American",]$race.nih = "Hispanic"
  #round2[round2$race.nih == "Hispanic",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Latin;Latino/a American;Hispanic",]$race.nih = "Hispanic"
  #round2[round2$race.nih == "Latina",]$race.nih = "Hispanic"
  round2[round2$race.nih == "latino",]$race.nih = "Hispanic"
  round2[round2$race.nih == "latinx",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Mexican American",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Mexican;Hispanic",]$race.nih = "Hispanic"
  round2[round2$race.nih == "Mexican",]$race.nih = "Hispanic"  
  
  # Pacific Islander
  levels(round2$race.nih) <- c(levels(round2$race.nih),"Pacific Islander")
  round2[round2$race.nih == "Pacific Islander",]$race.nih = "Pacific Islander"
  
  # multiple races
  levels(round2$race.nih) <- c(levels(round2$race.nih),"Multiple Races")
  round2[round2$race.nih == "Black-Amerindian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Cuban American;Creole;German American;Native American",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Native American;White",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Indigenous Maya;Chinese",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "African-American;Native American;Filipino",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Black;Chinese Malay",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Black;Middle Eastern;Indian",]$race.nih = "Multiple Races" 
  round2[round2$race.nih == "African-American;Native American;Irish American;Creole",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Asian-American;White",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Chinese;White",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Asian;Caucasian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Filipino;Portuguese;Spanish",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Asian Pacific American;Pacific Islander;Caucasian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Puerto Rican;Polish;German",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "White-Amerindian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "White;East Asain",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "White;Filipino",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "White;Black",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "White;Guamanian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "White;Asian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Caucasian;Pacific Islander",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Caucasian;Native Hawaiian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "White;American Indian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Pacific Islander;White",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Asian Pacific American",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "white;asian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "white;korean",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "African America;Black;Native American",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "African American;Native American",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Hispanic/Latina;Jamaican;White;Chinese",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Japanese;Indian;German;caucasian",]$race.nih = "Multiple Races"
  #round2[round2$race.nih == "African American;Native American",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Half Japanese and Half Iranian",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "black;white;Non-Hispanic Whites",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Thai;Vietnamese;Irish American",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "I have white skin, my family claims mixed European heritage with some indigenous Maori.",]$race.nih = "Multiple Races"
  round2[round2$race.nih == "Arab American;African-American",]$race.nih = "Multiple Races"
  
  # american indian
  levels(round2$race.nih) <- c(levels(round2$race.nih),"American Indian")
  round2[round2$race.nih == "Amerindian",]$race.nih = "American Indian"
  round2[round2$race.nih == "Native American/Moorish",]$race.nih = "American Indian"
  round2[round2$race.nih == "American Indian;Mexican American",]$race.nih = "American Indian"
  
  # No Report
  levels(round2$race.nih) <- c(levels(round2$race.nih),"NR")
  round2[round2$race.nih == "Hispanic",]$race.nih = "NR"
  round2[round2$race.nih == "god",]$race.nih = "NR"
  round2[round2$race.nih == "Honestly, who knows?",]$race.nih = "NR"
  round2[round2$race.nih == "Mixed",]$race.nih = "NR"
  round2[round2$race.nih == "Multiple",]$race.nih = "NR"
  round2[round2$race.nih == "colored",]$race.nih = "NR"
  #round2[round2$race.nih == "void survey;",]$race.nih = "NR"
  #round2[round2$race.nih == "void;",]$race.nih = "NR"
  round2[round2$race.nih == "mixed",]$race.nih = "NR"
  #round2[round2$race.nih == "He's 56 actually; void;",]$race.nih = "NR"
  round2[round2$race.nih == "-",]$race.nih = "NR"
  round2[round2$race.nih == "Moore",]$race.nih = "NR"
  
  levels(round2$race.nih) <- c(levels(round2$race.nih), "Void")
  round2[round2$race.nih == "He's 56 actually;void",]$race.nih = "Void"
  round2[round2$race.nih == "void",]$race.nih = "Void"
  round2[round2$race.nih == "void survey",]$race.nih = "Void"
  
  round2$race.nih <- droplevels(round2$race.nih)
  table(round2$race.nih)
  
  return(round2$race.nih)
}

# language
language.clean <- function(round2){
  # cleaning up other_languages column
  lang.clean = round2$list_languages
  
  # lang.clean[grep("Spanosh", lang.clean)] = "Spanish"
  # lang.clean[grep("Some Spanish", lang.clean)] = "Spanish"
  # #lang.clean[grep("Spanish\n", lang.clean)] = "Spanish"
  # #lang.clean[grep("english;\n", lang.clean)] = "English"
  # lang.clean[grep("Chinese (Mandarin)", lang.clean)] = "Mandarin"
  # lang.clean[grep("Indonesian", lang.clean)] = "Indonesian"
  # 
  # 
  # # adding some people with Hindi as first language written
  # lang.clean[grep("Odia", lang.clean)] = "Hindi"
  # # Mandarin written first
  # lang.clean[grep("Taiwanese", lang.clean)] = "Mandarin"
  # # Hebrew first
  # lang.clean[grep("Yiddish", lang.clean)] = "Hebrew"
  # #Amharic first
  # lang.clean[grep("Amharic", lang.clean)] = "Amharic"
  # #aremian first
  # lang.clean[grep("Assyrian", lang.clean)] = "Armenian"
  # # misspelled korean
  # lang.clean[grep("Koream", lang.clean)] = "Korean"
  # lang.clean[grep("korean", lang.clean)] = "Korean"
  
  # "" and none 
  # lang.clean[grep("", lang.clean)] = "English"
  lang.clean[grep("none", lang.clean)] = "English"
  lang.clean[grep("n/a", lang.clean)] = "English"
  lang.clean[grep("none", lang.clean)] = "English"
  
  round2$lang.clean = lang.clean
  
  
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Hungarian")
  round2[round2$lang.clean == "Hungarian;Spanish",]$lang.clean = "Hungarian"
  
  #levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Amharic")
  #round2[round2$lang.clean == "Amharic; ",]$lang.clean = "Amharic"
  
  round2[round2$lang.clean == "",]$lang.clean = "English"
  
  # Russian
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Russian")
  round2[round2$lang.clean == "Russian;French",]$lang.clean = "Russian"
  round2[round2$lang.clean == "Russian;Spanish",]$lang.clean = "Russian"
  #round2[round2$lang.clean == "Russian; ",]$lang.clean = "Russian"
  
  # Spanish
  #round2[round2$lang.clean == "Spanish ",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "Spanish, Hindi",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "spanish;",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "Spanish;",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "Spanish; ",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;Cebuano",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;Chinese",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;English",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "Spanish; French",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;French",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;Hola",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;Nahuatl",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "spanish",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "spanish and english",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "spanish, italian",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;Arabic (Egyptian)",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "Spanish, ",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "spanish, French",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;French;Arabic (Egyptian);Russian",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;French;German (tourist);Italian (understand)",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;German",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;Hebrew",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;Mandarin Chinese",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;Japanese",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;sign language",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish;Tagalog;Iloko;Pangasinan",]$lang.clean = "Spanish"
  
  #Vietnamese
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Vietnamese")
  #round2[round2$lang.clean == "Vietnamese; ",]$lang.clean = "Vietnamese"
  round2[round2$lang.clean == "Vietnamese, Spanish",]$lang.clean = "Vietnamese"
  round2[round2$lang.clean == "Vietnamese;Mandarin Chinese;Hakka",]$lang.clean = "Vietnamese"
  round2[round2$lang.clean == "Vietnamese;Spanish;Mandarin Chinese",]$lang.clean = "Vietnamese"
  
  #Telugu
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Telugu")
  round2[round2$lang.clean == "Telugu, Spanish",]$lang.clean = "Telugu"
  #round2[round2$lang.clean == "Telugu; ",]$lang.clean = "Telugu"
  
  #Old English
  #levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Old English")
  #round2[round2$lang.clean == "Old English; ",]$lang.clean = "Old English"
  
  #Tagalog
  #levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Tagalog")
  #round2[round2$lang.clean == "Tagalog; ",]$lang.clean = "Tagalog"
  
  #Tulu
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Tulu")
  round2[round2$lang.clean == "Tulu;Kannada;Hindi",]$lang.clean = "Tulu"
  
  #Tamil
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Tamil")
  #round2[round2$lang.clean == "Tamil; ",]$lang.clean = "Tamil"
  round2[round2$lang.clean == "Tamil;Hindi",]$lang.clean = "Tamil"
  round2[round2$lang.clean == "Tamil;Telugu",]$lang.clean = "Tamil"
  
  #Urdu
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Urdu")
  round2[round2$lang.clean == "Urdu;Spanish",]$lang.clean = "Urdu"
  
  #Portuguese
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Portuguese")
  #round2[round2$lang.clean == "Portuguese, french",]$lang.clean = "Portuguese"
  #round2[round2$lang.clean == "Portuguese; ",]$lang.clean = "Portuguese"
  round2[round2$lang.clean == "Portuguese;Spanish",]$lang.clean = "Portuguese"
  
  #Marathi
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Marathi")
  round2[round2$lang.clean == "Marathi;Hindi;Gujarati",]$lang.clean = "Marathi"
  round2[round2$lang.clean == "Marathi;Hindi;Spanish",]$lang.clean = "Marathi"
  
  
  #Mandarin
  # levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Mandarin")
  #round2[round2$lang.clean == "Mandarin Chinese;",]$lang.clean = "Mandarin"
  #round2[round2$lang.clean == "Mandarin Chinese; ",]$lang.clean = "Mandarin"
  round2[round2$lang.clean == "Mandarin Chinese;Cantonese",]$lang.clean = "Mandarin"
  round2[round2$lang.clean == "Mandarin Chinese;French",]$lang.clean = "Mandarin"
  round2[round2$lang.clean == "Mandarin Chinese;Korean;Spanish",]$lang.clean = "Mandarin"
  round2[round2$lang.clean == "Mandarin Chinese;Spanish",]$lang.clean = "Mandarin"
  round2[round2$lang.clean == "Mandarin Chinese;Yue Chinese;Japanese",]$lang.clean = "Mandarin"
  round2[round2$lang.clean == "Mandarin, Cantonese",]$lang.clean = "Mandarin"
  round2[round2$lang.clean == "Chinese (Mandarin)",]$lang.clean = "Mandarin"
  
  
  #Korean
  round2[round2$lang.clean == "Korean and French",]$lang.clean = "Korean"
  round2[round2$lang.clean == "Korean, Spanish",]$lang.clean = "Korean"
  #round2[round2$lang.clean == "Korean;",]$lang.clean = "Korean"
  #round2[round2$lang.clean == "Korean; ",]$lang.clean = "Korean"
  round2[round2$lang.clean == "Korean;Japanese",]$lang.clean = "Korean"
  #round2[round2$lang.clean == "Korean; Spanish",]$lang.clean = "Korean"
  round2[round2$lang.clean == "Korean;Spanish",]$lang.clean = "Korean"
  round2[round2$lang.clean == "Koream",]$lang.clean = "Korean"
  round2[round2$lang.clean == "korean",]$lang.clean = "Korean"
  
  #Japanese
  #round2[round2$lang.clean == "Japanese; ",]$lang.clean = "Japanese"
  
  #Italian
  #round2[round2$lang.clean == "Italian; ",]$lang.clean = "Italian"
  
  #Hindi
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Hindi")
  round2[round2$lang.clean == "Hindi, Bengali, French",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi, Marathi, Spanish",]$lang.clean = "Hindi"
  #round2[round2$lang.clean == "Hindi;",]$lang.clean = "Hindi"
  #round2[round2$lang.clean == "Hindi; ",]$lang.clean = "Hindi"
  #round2[round2$lang.clean == "HindiGerman",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi;Gujarati",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi;Kannada",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi;Konkani;French",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi;Marathi",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi;Marathi;Gujrati",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi;Spanish",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi;Tamil;Gujarati;German",]$lang.clean = "Hindi"
  
  #Hebrew
  #round2[round2$lang.clean == "Hebrew",]$lang.clean = "Hebrew"
  round2[round2$lang.clean == "hebrew",]$lang.clean = "Hebrew"
  #round2[round2$lang.clean == "Hebrew; ",]$lang.clean = "Hebrew"
  round2[round2$lang.clean == "Hebrew;Aramaic;English",]$lang.clean = "Hebrew"
  round2[round2$lang.clean == "HEBREW. RUSSIAN",]$lang.clean = "Hebrew"
  
  #Haitian Creole
  #levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Haitian Creole")
  #round2[round2$lang.clean == "Haitian Creole",]$lang.clean = "Haitian Creole"
  
  #Gujarati
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Gujarati")
  #round2[round2$lang.clean == "Gujarati; ",]$lang.clean = "Gujarati"
  round2[round2$lang.clean == "Gujarati;Hindi",]$lang.clean = "Gujarati"
  
  #German
  round2[round2$lang.clean == "German;Spanish",]$lang.clean = "German"
  
  #French
  round2[round2$lang.clean == "French, Farsi",]$lang.clean = "French"
  round2[round2$lang.clean == "french, spanish",]$lang.clean = "French"
  round2[round2$lang.clean == "French, Vietnamese",]$lang.clean = "French"
  #round2[round2$lang.clean == "French; ",]$lang.clean = "French"
  round2[round2$lang.clean == "French;Italian",]$lang.clean = "French"
  round2[round2$lang.clean == "French;Spanish",]$lang.clean = "French"
  
  #Cantonese
  #round2[round2$lang.clean == "Cantonese; ",]$lang.clean = "Cantonese"
  round2[round2$lang.clean == "Cantonese;French;Mandarin",]$lang.clean = "Cantonese"
  round2[round2$lang.clean == "Cantonese;Mandarin",]$lang.clean = "Cantonese"
  round2[round2$lang.clean == "Cantonese;Mandarin Chinese",]$lang.clean = "Cantonese" 
  
  #Chinese
  round2[round2$lang.clean == "Chinese, French",]$lang.clean = "Chinese"
  #round2[round2$lang.clean == "Chinese",]$lang.clean = "Chinese"
  round2[round2$lang.clean == "Chinese;French;Korean",]$lang.clean = "Chinese"
  
  #Bulgarian
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Bulgarian")
  round2[round2$lang.clean == "Bulgarian",]$lang.clean = "Bulgarian"
  
  #Armenian
  #round2[round2$lang.clean == "Armenian; Assyrian; French;",]$lang.clean = "Armenian"
  round2[round2$lang.clean == "Armenian;Russian",]$lang.clean = "Armenian"
  round2[round2$lang.clean == "Armenian;Spanish",]$lang.clean = "Armenian"
  
  #Arabic
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Arabic")
  round2[round2$lang.clean == "Arabic, French, Japanese",]$lang.clean = "Arabic"
  round2[round2$lang.clean == "Arabic (Modern Standard);French;Spanish",]$lang.clean = "Arabic"
  
  #Arabic (Egyptian)
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Arabic Egyptian")
  round2[round2$lang.clean == "Arabic (Egyptian);French Guianese Creole",]$lang.clean = "Arabic Egyptian"
  
  #More English
  round2[round2$lang.clean == "english",]$lang.clean = "English"
  round2[round2$lang.clean == "English, Thai, japanese",]$lang.clean = "English"
  round2[round2$lang.clean == "English",]$lang.clean = "English"
  round2[round2$lang.clean == "na",]$lang.clean = "English"
  
  #Dari (Persian)
  #levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Dari Persian")
  #round2[round2$lang.clean == "Dari",]$lang.clean = "Dari Persian"
  
  #Serbian
  levels(round2$lang.clean) <- c(levels(round2$lang.clean),"Serbian")
  round2[round2$lang.clean == "Serbian;Spanish;Russian;German;Latin",]$lang.clean = "Serbian"
  
  # new code
  round2[round2$lang.clean == "Arabic Egyptian",]$lang.clean = "Arabic"
  
  #Amharic, But not sure if it should be an indigenous language level
  levels(round2$lang.clean) <- c(levels(round2$lang.clean), "Amharic")
  round2[round2$lang.clean == "Amharic, Italian.some spanish",]$lang.clean = "Amharic"
  
  #Armenian 
  round2[round2$lang.clean == "Armenian;Assyrian;French",]$lang.clean = "Armenian"
  
  #Creole
  levels(round2$lang.clean) <- c (levels(round2$lang.clean), "Creole")
  round2[round2$lang.clean == "Haitian Creole",]$lang.clean = "Creole"
  
  #English
  round2[round2$lang.clean == "Old English",]$lang.clean = "English"
  
  # Farsi
  levels(round2$lang.clean) <- c(levels(round2$lang.clean), "Farsi")
  # round2[round2$lang.clean == "Dari Persian",]$lang.clean = "Farsi"
  round2[round2$lang.clean == "Farsi (Persian);Kurdish;English",]$lang.clean = "Farsi"
  round2[round2$lang.clean == "Dari",]$lang.clean = "Farsi"
  
  #Hebrew
  round2[round2$lang.clean == "Hebrew, Spanish, Yiddish",]$lang.clean = "Hebrew"
  
  #Hindi
  round2[round2$lang.clean == "Hindi;Bengali;Odia",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi;German",]$lang.clean = "Hindi"
  round2[round2$lang.clean == "Hindi;Odia",]$lang.clean = "Hindi"
  
  #Mandarin
  round2[round2$lang.clean == "Mandarin Chinese",]$lang.clean = "Mandarin"
  round2[round2$lang.clean == "Mandarin Chinese;Taiwanese",]$lang.clean = "Mandarin"
  round2[round2$lang.clean == "Mandarin Chinese;Taiwanese;Spanish",]$lang.clean = "Mandarin"
  
  
  #Portuguese 
  round2[round2$lang.clean == "Portuguese, french",]$lang.clean = "Portuguese"
  
  # Spanish
  round2[round2$lang.clean == "Spanosh",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "Spanish;Tagalog;Iloko;Pangasinan",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "Spanish;French;Arabic (Egyptian);Russian",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish, Hindi",]$lang.clean = "Spanish"
  #round2[round2$lang.clean == "spanish, French",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Spanish,",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "Some Spanish",]$lang.clean = "Spanish"
  round2[round2$lang.clean == "spanish, italian",]$lang.clean = "Spanish"
  
  
  round2$lang.clean <- droplevels(round2$lang.clean)
  table(round2$lang.clean)
  
  return(round2$lang.clean)
}


# running the functions 

# sexual orientation
sex_or<-sex_or(round2)

#languages
language.clean <-language.clean(round2)

#gender
gender.clean <-gender.clean(round2)

# race
race.nih <- race.nih(round2)

#ethnicity
ethnicity.nih <- ethnicity.nih(round2)

# cbind to old dataframe, renaming to new dataframe ->round1_match ???? 
round2_new <-cbind(round2, sex_or, language.clean, gender.clean, race.nih, ethnicity.nih)

# delete 18 to 22
# round2_clean_all <-round2_new[-c(18:22)]

#export
write.csv(round2_new, 'round2_clean_all.csv')
