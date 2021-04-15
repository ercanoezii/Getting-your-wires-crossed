library(dplyr)
library(ggplot2)
library(openxlsx)
library(lme4)


data <- read.xlsx("Final-Data-Cleaned.xlsx")
str(data)
SubsetUsage <- data 


### TRANSFORMING DATA ###

#Transforming all answers to numeric values
# 5 = daily
# 4 = weekly
# 3 = monthly
# 2 = less frequently
# 1 = never

Dag <- grepl("dagelijks", SubsetUsage$Friends)
Week <- grepl("wekelijks", SubsetUsage$Friends)
Maand <- grepl("maandelijks", SubsetUsage$Friends)
Minder <- grepl("minder vaak", SubsetUsage$Friends)
Nooit <- grepl("nooit", SubsetUsage$Friends)

SubsetUsage$FriendsNum[Dag] = 5
SubsetUsage$FriendsNum[Week] = 4
SubsetUsage$FriendsNum[Maand] = 3
SubsetUsage$FriendsNum[Minder] = 2
SubsetUsage$FriendsNum[Nooit] = 1

Dag <- grepl("dagelijks", SubsetUsage$School)
Week <- grepl("wekelijks", SubsetUsage$School)
Maand <- grepl("maandelijks", SubsetUsage$School)
Minder <- grepl("minder vaak", SubsetUsage$School)
Nooit <- grepl("nooit", SubsetUsage$School)

SubsetUsage$SchoolNum[Dag] = 5
SubsetUsage$SchoolNum[Week] = 4
SubsetUsage$SchoolNum[Maand] = 3
SubsetUsage$SchoolNum[Minder] = 2
SubsetUsage$SchoolNum[Nooit] = 1

Dag <- grepl("dagelijks", SubsetUsage$Dutch.people)
Week <- grepl("wekelijks", SubsetUsage$Dutch.people)
Maand <- grepl("maandelijks", SubsetUsage$Dutch.people)
Minder <- grepl("minder vaak", SubsetUsage$Dutch.people)
Nooit <- grepl("nooit", SubsetUsage$Dutch.people)

SubsetUsage$Dutch.peopleNum[Dag] = 5
SubsetUsage$Dutch.peopleNum[Week] = 4
SubsetUsage$Dutch.peopleNum[Maand] = 3
SubsetUsage$Dutch.peopleNum[Minder] = 2
SubsetUsage$Dutch.peopleNum[Nooit] = 1

Dag <- grepl("dagelijks", SubsetUsage$Reading.Fun)
Week <- grepl("wekelijks", SubsetUsage$Reading.Fun)
Maand <- grepl("maandelijks", SubsetUsage$Reading.Fun)
Minder <- grepl("minder vaak", SubsetUsage$Reading.Fun)
Nooit <- grepl("nooit", SubsetUsage$Reading.Fun)

SubsetUsage$Reading.FunNum[Dag] = 5
SubsetUsage$Reading.FunNum[Week] = 4
SubsetUsage$Reading.FunNum[Maand] = 3
SubsetUsage$Reading.FunNum[Minder] = 2
SubsetUsage$Reading.FunNum[Nooit] = 1

Dag <- grepl("dagelijks", SubsetUsage$TV)
Week <- grepl("wekelijks", SubsetUsage$TV)
Maand <- grepl("maandelijks", SubsetUsage$TV)
Minder <- grepl("minder vaak", SubsetUsage$TV)
Nooit <- grepl("nooit", SubsetUsage$TV)

SubsetUsage$TVNum[Dag] = 5
SubsetUsage$TVNum[Week] = 4
SubsetUsage$TVNum[Maand] = 3
SubsetUsage$TVNum[Minder] = 2
SubsetUsage$TVNum[Nooit] = 1

#Adding aggregated scores for Usage and Proficiency
ScoreUsage <- mutate(SubsetUsage, Usage = TVNum + FriendsNum + SchoolNum + Reading.FunNum)
ScoreProf <- mutate(ScoreUsage, Proficiency = as.numeric(reading.proficiency) + as.numeric(writing.proficiency) + as.numeric(Listening.proficiency) + as.numeric(Speaking.proficiency))



### Participant data ###
#age#
subsetAge <- filter(data, !is.na(data$Age))
subsetAge$AgeNum <- as.numeric(subsetAge$Age)
qplot(subsetAge$AgeNum, geom="histogram")
mean(subsetAge$AgeNum)
sd(subsetAge$AgeNum)
count(subsetAge, vars = subsetAge$AgeNum)

#Hand#
count(data, vars = data$Hand)

#education#
subsetEdu <- filter(data, !is.na(data$Education))

VMBO <- grepl("VMBO", subsetEdu$Education)
MBO <- grepl("MBO", subsetEdu$Education)
HAVO <- grepl("HAVO", subsetEdu$Education)
VWO <- grepl("VWO", subsetEdu$Education)
HBO <- grepl("HBO", subsetEdu$Education)
Universiteit <- grepl("Universiteit", subsetEdu$Education)


subsetEdu$EducationNum[MBO] = 2
subsetEdu$EducationNum[VMBO] = 1
subsetEdu$EducationNum[HAVO] = 3
subsetEdu$EducationNum[VWO] = 4
subsetEdu$EducationNum[HBO] = 5
subsetEdu$EducationNum[Universiteit] = 6

qplot(subsetEdu$EducationNum, geom="histogram")
count(subsetEdu, vars = subsetEdu$Education)

#correct answers#
count(data, vars = data$Correct)

# reading proficiency level #
lees <- grepl("leesvaardigheidsniveau", data$Question)
subsetLees <- filter(data, lees)
subsetLees$LeesNum <- as.numeric(subsetLees$Answer)
qplot(subsetLees$LeesNum, geom="histogram")
mean(subsetLees$LeesNum)
sd(subsetLees$LeesNum)
count(subsetLees, vars = subsetLees$LeesNum)

# writing proficiency level  #
schrijf <- grepl("schrijfvaardigheid", data$Question)
subsetSchrijf <- filter(data, schrijf)
subsetSchrijf$SchrijfNum <- as.numeric(subsetSchrijf$Answer)

qplot(subsetSchrijf$SchrijfNum, geom="histogram")
hist(subsetSchrijf$SchrijfNum)
mean(subsetSchrijf$SchrijfNum)
sd(subsetSchrijf$SchrijfNum)
count(subsetSchrijf, vars = subsetSchrijf$SchrijfNum)

# listening proficiency level  #
luister <- grepl("luistervaardigheid", data$Question)
subsetLuister <- filter(data, luister)
subsetLuister$LuisterNum <- as.numeric(subsetLuister$Answer)
qplot(subsetLuister$LuisterNum, geom="histogram")
mean(subsetLuister$LuisterNum)
sd(subsetLuister$LuisterNum)
count(subsetLuister, vars = subsetLuister$LuisterNum)

# speaking proficiency level  #
spreek <- grepl("spreekvaaridgheid", data$Question)
subsetSpreek <- filter(data, spreek)
subsetSpreek$SpreekNum <- as.numeric(subsetSpreek$Answer)
qplot(subsetSpreek$SpreekNum, geom="histogram")
mean(subsetSpreek$SpreekNum)
sd(subsetSpreek$SpreekNum)
count(subsetLuister, vars = subsetSpreek$SpreekNum)

#How often do you talk in English with friends#
englishTalking <- grepl("Engels om te praten met v", ScoreProf$Question)
subsetEnglishTalking <- filter(ScoreProf, englishTalking)
count(subsetEnglishTalking, vars = subsetEnglishTalking$FriendsNum)
mean(subsetEnglishTalking$FriendsNum)
sd(subsetEnglishTalking$FriendsNum)

#How often do you use English on School#
englishOnSchool <- grepl("Engels op school", ScoreProf$Question)
subsetEnglishOnSchool <- filter(ScoreProf, englishOnSchool)
count(subsetEnglishOnSchool, vars = subsetEnglishOnSchool$SchoolNum)
mean(subsetEnglishOnSchool$SchoolNum)
sd(subsetEnglishOnSchool$SchoolNum)

#How often do you talk English with other Dutch people#
englishWithOtherDutch <- grepl("met andere N", ScoreProf$Question)
subsetEnglishWithOtherDutch <- filter(ScoreProf, englishWithOtherDutch)
count(subsetEnglishWithOtherDutch, vars = subsetEnglishWithOtherDutch$Dutch.peopleNum)
mean(subsetEnglishWithOtherDutch$Dutch.peopleNum)
sd(subsetEnglishWithOtherDutch$Dutch.peopleNum)

#How often do you read for fun in English#
englishReading <- grepl("Engels om te lezen", ScoreProf$Question)
subsetEnglishReading <- filter(ScoreProf, englishReading)
count(subsetEnglishReading, vars = subsetEnglishReading$Reading.FunNum)
mean(subsetEnglishReading$Reading.FunNum)
sd(subsetEnglishReading$Reading.FunNum)

#How often do you watch English tv#
englishTV <- grepl("Engelstalige TV", ScoreProf$Question)
subsetEnglishTV <- filter(ScoreProf, englishTV)
count(subsetEnglishTV, vars = subsetEnglishTV$TVNum)
mean(subsetEnglishTV$TVNum)
sd(subsetEnglishTV$TVNum)

#Aggregated score for Proficiency
count(subsetEnglishTalking, vars = subsetEnglishTalking$Proficiency)
mean(subsetEnglishTalking$Proficiency)
sd(subsetEnglishTalking$Proficiency)

#Aggregated score for Usage
count(subsetEnglishTalking, vars = subsetEnglishTalking$Usage)
mean(subsetEnglishTalking$Usage)
sd(subsetEnglishTalking$Usage)

#How long do you speak English#
howLongEnglish <- grepl("Hoe lang spreek je", data$Question)
subsetHowLongEnglish <- filter(data, howLongEnglish)
count(subsetHowLongEnglish, vars = subsetHowLongEnglish$Answer)
