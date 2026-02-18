





# Load libraries ####

library(readxl)
library(ggplot2)
library(ggdist)
library(ggthemes)
library(fmsb)
library(tidyr)
library(factoextra)
library(lavaan)
library(lavaanPlot)

# Humans ####

## Load humans data ####

bd <- read_excel('data/raw/combined_data_edited.xlsx') # This excel file was already pre-processed by Silvia

# Change names to something shorter 
names(bd) <- c('participantId', 'age', 'gender', 'yearsEducation', 'imageId', 
               'category', 'areaScore', 'liking', 'vividness', 'originality', 
               'aesthetics', 'curiosity', 'creativityScore')

summary(bd) # check how the variables are stored (character, numerics...)

## Category factor #### 

# Store the labels that we want to use for the different categories
categoryLabels <- c('Visual Artists', 'Non Artists', 'Human Inspired GenAI', 'Self-Guided GenAI')

table(bd$category, useNA = 'ifany') # check that all images are in one of the four categories
bd$category <- factor(bd$category, labels=categoryLabels) 

# We generate a new category variable that divides the sample between Humans and GenAI without considering the subgroups.
bd$category2 <- factor(ifelse(bd$category %in% categoryLabels[1:2], 'Humans', 'GenAI'), levels=c('Humans', 'GenAI'))
table(bd$category, bd$category2)

imagesToChange <- c('P29S1.png', 'P29S2.png','P29S3.png','P29S4.png','P29S5.png',
                    'P29S6.png','P29S7.png','P29S8.png','P29S9.png','P29S10.png','P29S11.png')


## Cleaning demographic variables ####

# Let's take a look at the gender of participants:
table(bd$gender) 

# We see that the contents of the variable are all over the place: participants 
# have used different ways to input their gender. We must change them in order to sort
# all results into four variables: male, female, non-binary and other 

bd$gender <- tolower(bd$gender) # switch all to lower case to remove categories

bd$gender[grepl('f', bd$gender)] <- 'Female' # All results that contain 'f' are female
bd$gender[grepl('mujer', bd$gender)] <- 'Female'

bd$gender[grepl('h', bd$gender)] <- 'Male' # All results that contain 'h' are male
bd$gender[startsWith(bd$gender, 'm')] <- 'Male' # Now, all results that start with 'm' are male (we have already classified 'Mujer')

bd$gender[bd$gender=='otro'] <- 'Other'
bd$gender[bd$gender=='non binary'] <- 'Non-binary'

table(bd$gender, useNA = 'ifany') # CHECK how we classify NAs

bd$gender <- factor(bd$gender) 

# Now, let's take a look at age:

summary(bd$age)

# There is a weird distribution of age: the max is 3737737 years old
# let's take a look:

table(bd$age)

# It looks like a participant had a typo with the age. Let's change it to NA (CHECK if we have real age)

bd$age[bd$age > 100] <- NA

# The same happens with years of education:

table(bd$yearsEducation)
bd$yearsEducation[bd$yearsEducation > 100] <- NA

summary(bd) # Now everything checks out



bd$category[bd$imageId %in% imagesToChange] <- 'Visual Artists'


## Generate a Creativity score: PCA analysis ####

# Besides using the mean between the 5 subscales to generate a unique score, we 
# will use PCA analysis to create a more accurate measure.

# keep only the variables we want for the PCA
variablesPca <- bd[,c('liking', 'curiosity', 'originality', 'aesthetics', 'vividness')]

# use prcomp() to perform the PCA
res.pca <- prcomp(variablesPca, scale = FALSE) # Generate an object containing the PCA results

bd$pcaScore <- res.pca$x[,1] # Store the PCA prediction as pcaScore in our database





## Normalize scores ####

# We will try normalizing the score values. To do this, we need the maximum and minimum value of each participant.
# NOTE: we will determine the max and min across all types of ratings

rangeParticipants <- c() # here we will store the range of values that each participant used

# For each participant, store max and min scores (of any type)

for (p in 1:length(unique(bd$participantId))) {
  
  rangeParticipants <- rbind(rangeParticipants,
                             c(unique(bd$participantId)[p],
                               max(bd[bd$participantId==unique(bd$participantId)[p],
                                      which(colnames(bd)=='liking'):which(colnames(bd)=='curiosity')]),
                               min(bd[bd$participantId==unique(bd$participantId)[p],
                                      which(colnames(bd)=='liking'):which(colnames(bd)=='curiosity')])))
  
}


rangeParticipants <- as.data.frame(rangeParticipants)
names(rangeParticipants) <- c('participantId', 'maxScore', 'minScore')
rangeParticipants$maxScore <- as.numeric(rangeParticipants$maxScore)
rangeParticipants$minScore <- as.numeric(rangeParticipants$minScore)
unique(bd$participantId) %in% unique(rangeParticipants$participantId) # Check that all participants have a range stablished

bd <- merge(bd, rangeParticipants, by='participantId', all.x=T) # add min and max scores to main database

# set range of scores (in all cases, the range was between 1 and 7)
maxTotal <- 7
minTotal <- 1

# To standardize the values, we use the formula:
# 1 + ((maxTotal - minTotal) * ( (rating - participantsMin) / (particpantsMax - participantsMin) ) )



bd$likingNorm <- 1 + ((maxTotal - minTotal) * ((bd$liking - bd$minScore)/(bd$maxScore-bd$minScore)))
bd$vividnessNorm <- 1 + ((maxTotal - minTotal) * ((bd$vividness - bd$minScore)/(bd$maxScore-bd$minScore)))
bd$originalityNorm <- 1 + ((maxTotal - minTotal) * ((bd$originality - bd$minScore)/(bd$maxScore-bd$minScore)))
bd$aestheticsNorm <- 1 + ((maxTotal - minTotal) * ((bd$aesthetics - bd$minScore)/(bd$maxScore-bd$minScore)))
bd$curiosityNorm <- 1 + ((maxTotal - minTotal) * ((bd$curiosity - bd$minScore)/(bd$maxScore-bd$minScore)))

bd$creativityNorm <- 1 + ((maxTotal - minTotal) * ((bd$creativity - bd$minScore)/(bd$maxScore-bd$minScore)))








## Demographics database ####

# Generate separate database with only demographic data. This will make describing our sample easier
dm <- unique(bd[,c('participantId', 'age', 'gender', 'yearsEducation', 'areaScore')])

summary(dm)

# we will also add the mean scores to the demographics database
meanScoresParticipant <- aggregate(cbind(pcaScore, creativityScore, liking, vividness, originality, aesthetics, curiosity) ~ participantId, data=bd, FUN=mean)
dm <- merge(dm, meanScoresParticipant, by='participantId', all.x=T)






## AREA Score: raw data ####

# Although Silvia's data file contains an overall AREA score, we want to have the individual 
# questions in case we want to make extra analysis with this information. 

# The raw data to calculate the AREA score is in the original individual files. Here we load them and extract the
# information regarding the individual questions from the AREA questionnaire. This will allow us to 
# analyze these data in depth and re-calculate the final score using PCA.

filesRaw <- list.files(path='data/raw/XLSX TOTAL UPDATED/', pattern='*.xlsx')

bdArea <- c()
missingIds <- c()

for (file in filesRaw) {
  
  xx <- read_excel(paste0('data/raw/XLSX TOTAL UPDATED/', file))
  if (sum(is.na(xx$PROLIFIC_PID)) > 0) {
    xx$PROLIFIC_PID <- unique(xx$PROLIFIC_PID)[1]
  }
  xx <- na.omit(xx[,c('PROLIFIC_PID', 'AreaSlider.response')])
  names(xx) <- c('participantId', 'areaAnswer')
  xx$areaAnswer <- as.numeric(substr(xx$areaAnswer, 1, 1))
  xx$areaQuestion <- paste0('q', formatC(1:14, width = 2, flag = "0"))
  xx <- spread(xx, areaQuestion, areaAnswer)
  
  
  bdArea <- rbind(bdArea, xx)
}


bdArea$appreciation <- (bdArea$q01 + bdArea$q02 + bdArea$q03 + 
                          bdArea$q04 + bdArea$q06 + bdArea$q09 + 
                          bdArea$q13 + bdArea$q14)/8

bdArea$experience <- (bdArea$q08 + bdArea$q11 + bdArea$q12 + bdArea$q13)/4

bdArea$behaviour <- (bdArea$q05 + bdArea$q07 + bdArea$q10)/3

dm <- merge(dm, bdArea, by='participantId', all=T)

# We will also generate a new variable dividing the sample into high and low area scores according to the sample median
dm$areaGroup <- factor(ifelse(dm$areaScore <= median(dm$areaScore), 'Low', 'High'))
bd$areaGroup <- factor(ifelse(bd$areaScore <= median(dm$areaScore), 'Low', 'High'))






## Store processed databases ####

save(bd, file = 'data/processed/database.RData')
save(dm, file = 'data/processed/demographics.RData')









# GPT DATA ####

# Toni asked to use GPT to rate the images using the same 5 subscales.



## Load data ####
# The results are stored in separate .xlsx files, one for each image, containing 30 ratings per image
# The code below gathers all these files and generates a single dataframe


filesRaw <- list.files(path='data/raw/results-gpt/', pattern='*.xlsx')

bdGpt <- c()

for (file in filesRaw) {
  
  xx <- read_excel(paste0('data/raw/results-gpt/', file))
  xx$id <- 1:nrow(xx)
  
  bdGpt <- rbind(bdGpt, xx)
}

names(bdGpt) <- c('category', 'imageId', 'liking', 'vividness', 'originality', 'aesthetics', 'curiosity', 'gptId')


## Factor category ####
bdGpt$category <- factor(bdGpt$category, labels=categoryLabels) # CHECK WITH SILVIA: ADD LABELS
bdGpt$category2 <- factor(ifelse(bdGpt$category %in% categoryLabels[1:2], 'Humans', 'GenAI'), levels=c('Humans', 'GenAI'))



## Generate PCA score from Human weights ####

bdGpt$pcaScore <- predict(res.pca, bdGpt[,c('liking', 'vividness', 'originality', 'aesthetics', 'curiosity')])[,1]
bdGpt$creativityScore <- rowSums(bdGpt[,c('liking', 'vividness', 'originality', 'aesthetics', 'curiosity')])/5


# will use PCA analysis to create a more accurate measure.

# keep only the variables we want for the PCA
variablesPcaGpt <- bdGpt[,c('liking', 'curiosity', 'originality', 'aesthetics', 'vividness')]

# use prcomp() to perform the PCA
res.pca.gpt <- prcomp(variablesPcaGpt, scale = FALSE) # Generate an object containing the PCA results

bdGpt$pcaScoreGpt <- res.pca.gpt$x[,1] # Store the PCA prediction as pcaScore in our database

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



bdGpt$category[bdGpt$imageId %in% imagesToChange] <- 'Visual Artists'


## Save final dataframe ####

save(bdGpt, file ='data/processed/gpt-ratings.RData')
load( file ='data/processed/gpt-ratings.RData')




# Whole database ####

# Finally, we will merge the human and GPT databases into one combined dataframe that will be needed for
# some of the analyses

bd_human <- bd[, names(bd) %in% names(bdGpt) | names(bd) == "participantId"] # select columns that match in both dfs
bd_gpt <- bdGpt[,names(bdGpt) %in% names(bd)]
bd_gpt$participantId <- NA
bd_human$rater <- 'Human' # generate new variable to identify where the info comes from
bd_gpt$rater <- 'GPT'
bdWhole <- rbind(bd_human, bd_gpt)
bdWhole$rater <- factor(bdWhole$rater, levels=c('Human', 'GPT'))


names(bdWhole)

# See https://lavaan.ugent.be/tutorial/groups.html
if( !file.exists("./cache/1FA.rds") )
{
  crea.1f <- "
    crea =~ liking + vividness + originality + aesthetics + curiosity;
  ";
  
  # configurational invariance:
  crea.1f.fit <- cfa(crea.1f, data=bdWhole, group="rater", se="bootstrap", test="bootstrap");
  summary(crea.1f.fit);
  fitMeasures(crea.1f.fit, c("chisq", "df", "pvalue", "cfi", "tli", "nnfi", "rfi"));
  lavaanPlot(model=crea.1f.fit, coefs=TRUE, sig=1.00, stand=TRUE, covs=TRUE, stars=c("regress", "latent", "covs"), edge_options=list(color="gray"));
  
  # weak invariance:
  crea.1f.fit.wi <- cfa(crea.1f, data=bdWhole, group="rater", group.equal="loadings", se="bootstrap", test="bootstrap");
  
  # strong invariance:
  crea.1f.fit.si <- cfa(crea.1f, data=bdWhole, group="rater", group.equal=c("intercepts", "loadings"), se="bootstrap", test="bootstrap");
  
  # model comparison tests
  lavTestLRT(crea.1f.fit, crea.1f.fit.wi, crea.1f.fit.si); # both p-values are signif. ->  factor loadings and latent means are different between human and AI judges! 
  
  # separately for humans and AI judges:
  crea.1f.fit.hum <- cfa(crea.1f, data=bdWhole[bdWhole$rater=="Human",], se="bootstrap", test="bootstrap");
  summary(crea.1f.fit.hum);
  fitMeasures(crea.1f.fit.hum, c("chisq", "df", "pvalue", "cfi", "tli", "nnfi", "rfi"));
  lavaanPlot(model=crea.1f.fit.hum, coefs=TRUE, sig=1.00, stand=TRUE, covs=TRUE, stars=c("regress", "latent", "covs"), edge_options=list(color="gray"));
  
  crea.1f.fit.ai <- cfa(crea.1f, data=bdWhole[bdWhole$rater=="GPT",], se="bootstrap", test="bootstrap");
  summary(crea.1f.fit.ai);
  fitMeasures(crea.1f.fit.ai, c("chisq", "df", "pvalue", "cfi", "tli", "nnfi", "rfi"));
  lavaanPlot(model=crea.1f.fit.ai, coefs=TRUE, sig=1.00, stand=TRUE, covs=TRUE, stars=c("regress", "latent", "covs"), edge_options=list(color="gray"));
  
  # the model for AI suggested by EFA:
  crea.2f <- "
    cai1 =~ aesthetics + vividness + liking;
    cai2 =~ originality + curiosity;
  ";
  crea.2f.fit.ai <- cfa(crea.2f, data=bdWhole[bdWhole$rater=="GPT",], se="bootstrap", test="bootstrap");
  summary(crea.2f.fit.ai, stand=TRUE);
  fitMeasures(crea.2f.fit.ai, c("chisq", "df", "pvalue", "cfi", "tli", "nnfi", "rfi"));
  lavaanPlot(model=crea.2f.fit.ai, coefs=TRUE, sig=1.00, stand=TRUE, covs=TRUE, stars=c("regress", "latent", "covs"), edge_options=list(color="gray"));
  anova(crea.1f.fit.ai, crea.2f.fit.ai); # 2FA, indeed, seems better
  
  crea1FA <- list("model"=crea.1f, "fit"=crea.1f.fit, "fit.hum"=crea.1f.fit.hum, "fit.ai"=crea.1f.fit.ai, "invariance.tests"=lavTestLRT(crea.1f.fit, crea.1f.fit.wi, crea.1f.fit.si), "fit.ai.2FA"=crea.2f.fit.ai);
  saveRDS(crea1FA, file="./cache/1FA.rds", compress="xz");
} else
{
  crea1FA <- readRDS("./cache/1FA.rds");
}



crea1FA.pred <- lavPredict(crea1FA$fit, type="lv", method="regression", transform=TRUE, append.data=TRUE, assemble=TRUE);
# check that the order is correct:
all(vapply(1:nrow(bdWhole), function(i) bdWhole$rater[i] == crea1FA.pred$rater[i] && bdWhole$liking[i] == crea1FA.pred$liking[i] && 
             bdWhole$vividness[i] == crea1FA.pred$vividness[i] && bdWhole$originality[i] == crea1FA.pred$originality[i] && bdWhole$aesthetics[i] == crea1FA.pred$aesthetics[i] && 
             bdWhole$curiosity[i] == crea1FA.pred$curiosity[i], logical(1)));
bdWhole$crea <- crea1FA.pred$crea

humansMax <- max(bdWhole$crea[bdWhole$rater=='Human'])
humansMin <- min(bdWhole$crea[bdWhole$rater=='Human'])

gptMax <- max(bdWhole$crea[bdWhole$rater=='GPT'])
gptMin <- min(bdWhole$crea[bdWhole$rater=='GPT'])



bdWhole$creaScaled <- ifelse(bdWhole$rater=='Human', 
                             (((bdWhole$crea - humansMin) * (5 - 1)) / (humansMax - humansMin)) + 1,
                             (((bdWhole$crea - gptMin) * (5 - 1)) / (gptMax - gptMin)) + 1)



bdWhole$category[bdWhole$imageId %in% imagesToChange] <- 'Visual Artists'


# load type of stimulus data

stim <- read_excel('data/raw/GENAI IMAGES-STIMULI.xlsx')

stim <- stim[,c('ImageID', 'StimulusType (1-12)')]
names(stim) <- c('imageId', 'stimId')
stim$stimType <- factor(ifelse(stim$stimId %in% c(1, 2, 3, 4, 11, 12), 'Old', 'New'), levels=c('Old', 'New'))
table(stim$stimId, stim$stimType)

bdWhole <- merge(bdWhole, stim, by='imageId', all=T)

save(bdWhole, file ='data/processed/database-whole.RData')

