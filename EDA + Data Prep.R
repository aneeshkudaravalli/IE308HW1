library(plyr)
all_data = read.csv("Chicago Medicare Data.csv", header = T)

## Exploratory Data Analysis and Clustering Preparation


count(all_data$provider_type) ## too many categories (73)

summary(all_data$hcpcs_code) ## too many categories

## turn to hspc code to understand type of practice using CPT specifications:

#Evaluation and Management: 99201 – 99499
#Anesthesia: 00100 – 01999; 99100 – 99140
#Surgery: 10021 – 69990
#Radiology: 70010 – 79999
#Pathology and Laboratory: 80047 – 89398
#Medicine: 90281 – 99199; 99500 – 99607

all_data$hcpcs_code = as.character(all_data$hcpcs_code)

x = as.numeric(all_data$hcpcs_code)

sum(is.na(x)) ## number of values that aren't classified to specifications above (< 10%)


#Create binary classification features to identify what type of practice a service is 

em = numeric(77286)
an = numeric(77286)
pl = numeric(77286)
med = numeric(77286)
rad = numeric(77286)
sur = numeric(77286)

#loop that was run multiple times to create features to classify the samples

for (i in 1:77286)
{
  if (is.na(x[i]) == 1)
  {
    sur[i] = 0;
  }
  
  else
  {
    if (findInterval(x[i], c(10021,69990)) == 1)
    {
      sur[i] = 1;
    }
    else
    {
      sur[i] = 0;
    }
  }
  
}

all_data = cbind(sur, all_data)

## Understanding the payments

hist(all_data$average_Medicare_allowed_amt) #extremely skewed
hist(log(all_data$average_Medicare_allowed_amt)) #better for clustering

hist(all_data$average_submitted_chrg_amt) #extremely skewed
hist(log(all_data$average_submitted_chrg_amt)) #better for clustering


summary(all_data$medicare_participation_indicator) #most practices participate in Medicare

#understanding location of practices and services

summary(all_data$nppes_provider_city)
zdata = strtrim(as.character(all_data$nppes_provider_zip), 3)

count(zdata)
#some samples Chicago in the city name, but aren't actually in Chicago
zipdata = cbind(zdata, all_data)
zipdata = subset(zipdata, zipdata$zdata != "660") #get rid of the samples with these zip codes (660, 463, and 617)

## putting the log of the features we want to cluster into the dataset
logama = log(zipdata$average_Medicare_allowed_amt)
logsca = log(zipdata$average_submitted_chrg_amt)

zipdata = cbind(logama, zipdata)
zipdata = cbind(logsca, zcdata)


###Plan to cluster in Python on 
#- type of practice (hcpcs code classification)
#- location (zip code classification)
#- average submitted charge amount
#- average medicare allowed amount (to understand how much medicare is willing to contribute relative to cost of service)


write.csv(zipdata, '/Users/aneeshkudaravalli/Documents/IEMS 308/zc.csv')













