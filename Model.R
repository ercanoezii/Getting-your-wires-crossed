library(dplyr)
library(ggplot2)
library(openxlsx)
library(lme4)

data <- read.xlsx("Final-Data-Cleaned.xlsx")
str(data)

### TRANSFORMING DATA ###

#Calculating accuracy per participant
ACCdata <- filter(data, Type!="intro" & Type!="info" & Type!="practice")
Group_part <- group_by(ACCdata, Results.index)
ACCdata2 <- mutate(Group_part, ACC = sum(as.numeric(Correct)/60))

## Aggregated usage score

#Transforming usage answers to numeric values
# 5 = daily
# 4 = weekly
# 3 = monthly
# 2 = less frequently
# 1 = never

Dag <- grepl("dagelijks", ACCdata2$Friends)
Week <- grepl("wekelijks", ACCdata2$Friends)
Maand <- grepl("maandelijks", ACCdata2$Friends)
Minder <- grepl("minder vaak", ACCdata2$Friends)
Nooit <- grepl("nooit", ACCdata2$Friends)

ACCdata2$FriendsNum[Dag] = 5
ACCdata2$FriendsNum[Week] = 4
ACCdata2$FriendsNum[Maand] = 3
ACCdata2$FriendsNum[Minder] = 2
ACCdata2$FriendsNum[Nooit] = 1

Dag <- grepl("dagelijks", ACCdata2$School)
Week <- grepl("wekelijks", ACCdata2$School)
Maand <- grepl("maandelijks", ACCdata2$School)
Minder <- grepl("minder vaak", ACCdata2$School)
Nooit <- grepl("nooit", ACCdata2$School)

ACCdata2$SchoolNum[Dag] = 5
ACCdata2$SchoolNum[Week] = 4
ACCdata2$SchoolNum[Maand] = 3
ACCdata2$SchoolNum[Minder] = 2
ACCdata2$SchoolNum[Nooit] = 1

Dag <- grepl("dagelijks", ACCdata2$Dutch.people)
Week <- grepl("wekelijks", ACCdata2$Dutch.people)
Maand <- grepl("maandelijks", ACCdata2$Dutch.people)
Minder <- grepl("minder vaak", ACCdata2$Dutch.people)
Nooit <- grepl("nooit", ACCdata2$Dutch.people)

ACCdata2$Dutch.peopleNum[Dag] = 5
ACCdata2$Dutch.peopleNum[Week] = 4
ACCdata2$Dutch.peopleNum[Maand] = 3
ACCdata2$Dutch.peopleNum[Minder] = 2
ACCdata2$Dutch.peopleNum[Nooit] = 1

Dag <- grepl("dagelijks", ACCdata2$Reading.Fun)
Week <- grepl("wekelijks", ACCdata2$Reading.Fun)
Maand <- grepl("maandelijks", ACCdata2$Reading.Fun)
Minder <- grepl("minder vaak", ACCdata2$Reading.Fun)
Nooit <- grepl("nooit", ACCdata2$Reading.Fun)

ACCdata2$Reading.FunNum[Dag] = 5
ACCdata2$Reading.FunNum[Week] = 4
ACCdata2$Reading.FunNum[Maand] = 3
ACCdata2$Reading.FunNum[Minder] = 2
ACCdata2$Reading.FunNum[Nooit] = 1

Dag <- grepl("dagelijks", ACCdata2$TV)
Week <- grepl("wekelijks", ACCdata2$TV)
Maand <- grepl("maandelijks", ACCdata2$TV)
Minder <- grepl("minder vaak", ACCdata2$TV)
Nooit <- grepl("nooit", ACCdata2$TV)

ACCdata2$TVNum[Dag] = 5
ACCdata2$TVNum[Week] = 4
ACCdata2$TVNum[Maand] = 3
ACCdata2$TVNum[Minder] = 2
ACCdata2$TVNum[Nooit] = 1

#Adding aggregated scores for Usage and Proficiency
ScoreUsage <- mutate(ACCdata2, Usage = TVNum + FriendsNum + SchoolNum + Reading.FunNum)
ScoreProf <- mutate(ScoreUsage, Proficiency = as.numeric(reading.proficiency) + as.numeric(writing.proficiency) + as.numeric(Listening.proficiency) + as.numeric(Speaking.proficiency))

#Filter for correctly answered items 
subsetRTQuestions <- filter(ScoreProf, Correct == 1)

#Filter for experimental items
#Removing non word items
non <- grepl("-non-", subsetRTQuestions$Type)
subsetNotNon <- subset(subsetRTQuestions, !non)

#Removing filler items
filler <- grepl("filler-", subsetNotNon$Type)
subsetIdioms <- subset(subsetNotNon, !filler)

#Getting general idea of RT data
x <- subsetIdioms$Row.id 
y <- subsetIdioms$RT 
plot(x, y, main = "Average RT's", xlab = "Question/participant", ylab = "RT", pch = 19, frame = FALSE)

## See if some participants couldn't keep up, some participants had quite large amounts of outliers, 
## but not so large that we would think they did not understand the experiment, so all participants were kept in
Group_participant <- group_by(subsetIdioms, Results.index)

part_mean <- Group_participant %>% summarize(meanRT = mean(RT), sdRT = sd(RT)) 
part_logmean <- Group_participant %>% summarize(meanRT = mean(log(RT)), sdRT = sd(RT))
part_mean
plot(part_mean$Results.index, part_mean$meanRT, main = "RT per participant", xlab = "paricipant", ylab = "mean RT")
plot(part_logmean$Results.index, part_logmean$meanRT, main = "Log RT per participant", xlab = "participant", ylab = "mean Log RT")


filter_participants <- Group_participant %>% filter(RT>3000)
totalslow <- filter_participants %>% summarize(n = n())

#Remover very long RT's (and investigate which ones were long)
length(subsetIdioms$RT)
subsetRTQuestionsAbove3000 <- filter(subsetIdioms, RT > 3000)
length(subsetRTQuestionsAbove3000$RT)
subsetRTQuestionsAbove3500 <- filter(subsetIdioms, RT > 3500)
length(subsetRTQuestionsAbove3500$RT)
subsetRTQuestionsAbove4500 <- filter(subsetIdioms, RT > 4500)
length(subsetRTQuestionsAbove4500$RT)

#Reaction times above 3000ms are removed (18 answers)
subsetIdiomsUnder3000 <- filter(subsetIdioms, RT <= 3000)

## Check experimental items, there is a learning effect/curve (esp for first 10 items)
Group_items <- group_by(subsetIdiomsUnder3000, Item.order)

part_mean <- Group_items %>% summarize(meanRT = mean(RT), sdRT = sd(RT)) 
part_logmean <- Group_items %>% summarize(meanRT = mean(log(RT)), sdRT = sd(RT))
part_mean
plot(part_mean$Item.order, part_mean$meanRT, main = "RT per item", xlab = "item order", ylab = "mean RT")
plot(part_logmean$Item.order, part_logmean$meanRT, main = "RT per item", xlab = "item order", ylab = "mean Log RT")



## log transformation of RT
#Based on filtered data (reaction time >= 3000)
datafile <- transform(subsetIdiomsUnder3000, LogRT = log(RT))

#Based on unfiltered dataset
datafile2 <- transform(subsetIdioms, LogRT = log(RT))

#Adding idiom type to dataset
EnControl <- grepl("ENidiom-control", datafile$Type)
EnWord <- grepl("ENidiom-word", datafile$Type)
NlControl <- grepl("NLidiom-control", datafile$Type)
NlWord <- grepl("NLidiom-word", datafile$Type)


datafile$IdiomType[EnControl] = "EN.Control"
datafile$IdiomType[EnWord] = "EN.Word"
datafile$IdiomType[NlControl] = "NL.Control"
datafile$IdiomType[NlWord] = "NL.Word"

EnControl2 <- grepl("ENidiom-control", datafile2$Type)
EnWord2 <- grepl("ENidiom-word", datafile2$Type)
NlControl2 <- grepl("NLidiom-control", datafile2$Type)
NlWord2 <- grepl("NLidiom-word", datafile2$Type)

datafile2$IdiomType[EnControl2] = "EN.Control"
datafile2$IdiomType[EnWord2] = "EN.Word"
datafile2$IdiomType[NlControl2] = "NL.Control"
datafile2$IdiomType[NlWord2] = "NL.Word"



#Transforming education levels to numeric values
VMBO <- grepl("VMBO", datafile$Education2)
MBO <- grepl("MBO", datafile$Education2)
HAVO <- grepl("HAVO", datafile$Education2)
VWO <- grepl("VWO", datafile$Education2)
HBO <- grepl("HBO", datafile$Education2)
Universiteit <- grepl("Universiteit", datafile$Education2)

datafile$EducationNum[MBO] = 2
datafile$EducationNum[VMBO] = 1
datafile$EducationNum[HAVO] = 3
datafile$EducationNum[VWO] = 4
datafile$EducationNum[HBO] = 5
datafile$EducationNum[Universiteit] = 6



#Getting general idea of RT data
x <- datafile$Row.id 
y <- datafile$LogRT 
z <- datafile$ACC
q <- datafile$Proficiency
plot(x, y, main = "Average RT's", xlab = "Question/participant", ylab = "LogRT", pch = 19, frame = FALSE)

#Cool images, effect of idioms can be seen in all types of data
ggplot(datafile,aes(y=LogRT,x=Row.id,color=factor(IdiomType))) + geom_point()+stat_smooth(method="lm",se=FALSE)
ggplot(datafile,aes(y=LogRT,x=ACC,color=factor(IdiomType))) + geom_point()+stat_smooth(method="lm",se=FALSE)
ggplot(datafile,aes(y=LogRT,x=Proficiency,color=factor(IdiomType))) + geom_point()+stat_smooth(method="lm",se=FALSE)
ggplot(datafile,aes(y=LogRT,x=Usage,color=factor(IdiomType))) + geom_point()+stat_smooth(method="lm",se=FALSE)
ggplot(datafile,aes(y=LogRT,x=EducationNum,color=factor(IdiomType))) + geom_point()+stat_smooth(method="lm",se=FALSE)



#### MIXED EFFECTS MODELLING ####

### Find best random effects structure 
rm1 <- lmer(LogRT ~ 1 + IdiomType + Usage + (1 + IdiomType + Usage|Results.index) + (1 + IdiomType|Item.number), data = datafile) #start with most complex structure, don't adjust usage by item since this makes no sense
rm2 <- lmer(LogRT ~ 1 + IdiomType + Usage + (1 + IdiomType + Usage|Results.index) + (1|Item.number), data = datafile) #don't adjust idiom type by itemnumber since these have a vey high correlation
rm3 <- lmer(LogRT ~ 1 + IdiomType + Usage + (1 + IdiomType|Results.index) + (1|Item.number), data = datafile) # dont adjust usage by participant
rm4 <- lmer(LogRT ~ 1 + IdiomType + Usage + (1 + IdiomType|Results.index), data = datafile)
rm5 <- lmer(LogRT ~ 1 + IdiomType + Usage + (1|Results.index), data = datafile)
rm6 <- lmer(LogRT ~ 1 + IdiomType + Usage + (1|Results.index) + (1|Item.order), data = datafile) #Because of seem learning effect, this random effects structure is best
summary(rm6)
anova(rm5, rm6) 
anova(lm7, rm6)

rm7 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1|Results.index) + (1|Group), data = datafile)
rm8 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1|Results.index), data = datafile) #no difference between with or without group
rm9 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1 + IdiomType + Usage + Item.order|Results.index) + (1 + IdiomType + Usage + Item.order|Group), data = datafile) #too complex, Item order was the smallest so this was removed
rm10 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1 + IdiomType + Usage|Results.index) + (1 + IdiomType + Usage|Group), data = datafile) #Too complex, usage smallest effect was removed
rm11 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1 + IdiomType|Results.index) + (1 + IdiomType|Group), data = datafile) #Model still to complex, try removing type in both conditions
rm12 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1|Results.index) + (1 + IdiomType|Group), data = datafile)
rm13 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1 + IdiomType|Results.index) + (1|Group), data = datafile)
rm14 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1|Results.index), data = datafile)
rm15 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1|Results.index) + (1|Group), data = datafile) #Most complex model that converges
summary(rm7)
anova(rm7, rm8)
summary(rm11)


# initial look at variables
lm1 <- lmer(LogRT ~ 1 + Proficiency + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm2 <- lmer(LogRT ~ 1 + Usage + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm3 <- lmer(LogRT ~ 1 + Usage + Proficiency + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm4 <- lmer(LogRT ~ 1 + Usage*Proficiency + Item.order + (1|Results.index) + (1|Group), data = datafile)
summary(lm4)
anova(lm3, lm2)
anova(lm3, lm1)
anova(lm4, lm2)
# Both usage and proficiency on its own in a model are significant predictors. However, usage appears to be
# a stronger predictors, since when it is included in a model with proficiency, proficiency is no longer significant

lm5 <- lmer(LogRT ~ 1 + Usage + Dutch.peopleNum + Item.order + (1|Results.index) + (1|Group), data = datafile)
summary(lm5)
anova(lm2, lm5)
# Not significant addition to model, but talking English with other dutch people makes the RT slower. 
# This could be because participants who speak a lot of English with other people may have a relatively low familiarity
# With English idioms compared to their proficiency (know less idioms than expected for someone with their English level)

lm6 <- lmer(LogRT ~ 1 + IdiomType + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm7 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm8 <- lmer(LogRT ~ 1 + IdiomType * Usage + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm8.2 <- lmer(LogRT ~ 1 + IdiomType * Usage * Item.order + (1|Results.index) + (1|Group), data = datafile)
lm8.3 <- lmer(LogRT ~ 1 + IdiomType + Usage * Item.order + (1|Results.index) + (1|Group), data = datafile)
summary(lm8)
anova(lm6, lm7) #significant addition of usage
anova(lm2, lm7) #type is significant added to usage
anova(lm7, lm8) #interaction not significant
anova(lm7, lm8.2) #no significant interaction
anova(lm7, lm8.3) #no significant interaction

### Testing individual components of proficiency
lm9 <- lmer(LogRT ~ 1 + IdiomType + Usage + Proficiency + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm10 <- lmer(LogRT ~ 1 + IdiomType + Usage + reading.proficiency + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm11 <- lmer(LogRT ~ 1 + IdiomType + Usage + writing.proficiency + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm12 <- lmer(LogRT ~ 1 + IdiomType + Usage + Listening.proficiency + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm13 <- lmer(LogRT ~ 1 + IdiomType + Usage + Speaking.proficiency + Item.order + (1|Results.index) + (1|Group), data = datafile)
anova(lm7, lm9)
anova(lm7, lm10)
anova(lm7, lm11)
anova(lm7, lm12)
anova(lm7, lm13)
# All individual components of proficiency also not significant contributors

### Individual components of usage
lm14 <- lmer(LogRT ~ 1 + IdiomType + Reading.FunNum + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm15 <- lmer(LogRT ~ 1 + IdiomType + TVNum + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm16 <- lmer(LogRT ~ 1 + IdiomType + SchoolNum + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm17 <- lmer(LogRT ~ 1 + IdiomType + FriendsNum + Item.order + (1|Results.index) + (1|Group), data = datafile)
anova(lm6, lm14)
anova(lm6, lm15)
anova(lm6, lm16)
anova(lm6, lm17)
# TV and friends are most significant contributes to compound score, then ReadingFun and then TV which was the only one which in its own was not significant
# Since all factors are relatively important we keep the compound score the same as in the original experiment

# confirm keeping the model of original research
lm18 <- lmer(LogRT ~ 1 + IdiomType + Reading.FunNum + TVNum + SchoolNum + FriendsNum + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm19 <- lmer(LogRT ~ 1 + IdiomType + Reading.FunNum + SchoolNum + FriendsNum + Item.order + (1|Results.index) + (1|Group), data = datafile)
anova(lm7, lm18) # Adding all 4 parts separately to the model is not significantly better than what the original experiment did.
anova(lm7, lm19) # Adding all 4 parts separately to the model is not significantly better than what the original experiment did.
anova(lm18, lm19) # Leaving out TVNum is not significantly better

### Frequency of use as factor
#lm7 <- lmer(LogRT ~ 1 + IdiomType + Usage + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm20 <- lmer(LogRT ~ 1 + IdiomType + Usage + How.long.speaking + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm21 <- lmer(LogRT ~ 1 + IdiomType + How.long.speaking + Item.order + (1|Results.index) + (1|Group), data = datafile)
anova(lm7, lm20)
anova(lm6, lm21)
anova(lm7, lm21)
# Unlike original experiment how long people have been speaking English is not a significant predictor, this could be because in the Netherlands everyone learns 
# English at almost the same age

### Other effects to test
lm22 <- lmer(LogRT ~ 1 + IdiomType + Usage + ACC + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm23 <- lmer(LogRT ~ 1 + IdiomType + Usage + Hand2 + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm24 <- lmer(LogRT ~ 1 + IdiomType + Usage + Age2 + Item.order + (1|Results.index) + (1|Group), data = datafile)
lm25 <- lmer(LogRT ~ 1 + IdiomType + Usage + Education2 + Item.order + (1|Results.index) + (1|Group), data = datafile)

anova(lm7, lm22)
anova(lm7, lm23)
anova(lm7, lm24)
anova(lm7, lm25)
summary(lm25)
# only relevant factor is education, where HAVO is fastest then Uni, HBO, MBO, VMBO. This is almost exactly in order of the
# educational levels in the Dutch system. HAVO was VERY fast since all other groups have slower RT, however what should be kept in 
# mind is that there were only 2 HAVO and 1 VMBO participant, (MBO 5) the vast majority of participants was from HBO and Uni

#Same model as lm25, but without removal of 3000 < RT's
lm26 <- lmer(LogRT ~ 1 + IdiomType + Usage + Education2 + Item.order + (1|Results.index) + (1|Group), data = datafile2)
summary(lm25)
anova(lm8, lm7)

# The interaction between Education and IdiomType is not significantly better
lm27 <- lmer(LogRT ~ 1 +  Usage + IdiomType * Education2 + Item.order + (1|Results.index) + (1|Group), data = datafile)
anova(lm25, lm27)
summary(lm27)

# Transforming back from LogRT to RT (based on lm25)
(RT.IdiomTypeEn.Control <- exp(7.1421779))
(RT.IdiomTypeEN.Word <-  exp(0.0893395))
(RT.IdiomTypeNL.Control <-  exp(0.0122948))
(RT.IdiomTypeNL.Word <-  exp(0.0571718))

# intercept per condition
RT.IdiomTypeEn.Control
RT.IdiomTypeEn.Control * RT.IdiomTypeEN.Word
RT.IdiomTypeEn.Control * RT.IdiomTypeNL.Control
RT.IdiomTypeEn.Control * RT.IdiomTypeNL.Word

# adjustment per condition
RT.IdiomTypeEn.Control 
(RT.IdiomTypeEn.Control * RT.IdiomTypeEN.Word ) - RT.IdiomTypeEn.Control
(RT.IdiomTypeEn.Control * RT.IdiomTypeNL.Control)- RT.IdiomTypeEn.Control
(RT.IdiomTypeEn.Control * RT.IdiomTypeNL.Word)- RT.IdiomTypeEn.Control
