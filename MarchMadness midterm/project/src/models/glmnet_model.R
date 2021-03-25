library(data.table)
library(caret)
library(Metrics)
library(glmnet)
library(plotmo)
library(lubridate)



#read in data, notice the path will always look like this because the assumed working directory is the repo level folder
train<-fread('/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/PSU_Stat_380/Lectures/11. MTG_price wrapup/project/volume/data/interim/train.csv')
test<-fread('/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/PSU_Stat_380/Lectures/11. MTG_price wrapup/project/volume/data/interim/test.csv')
example_sub<-fread('/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/MarchMadness midterm/project/volumn/data/raw/example_sub.csv')

##########################
# Prep Data for Modeling #
##########################


# subset out only the columns to model

drops<- c('team_1','team_2')

train<-train[, !drops, with = FALSE]
test<-test[, !drops, with = FALSE]



#save the response var because dummyVars will remove
train_y<-train$result


# work with dummies

dummies <- dummyVars(result ~ ., data = train)
train<-predict(dummies, newdata = train)
test<-predict(dummies, newdata = test)



#fit a logistic model
gl_model_cv<-cv.glmnet(train, train_y, alpha = 1,family="binomial")

bestlam<-gl_model_cv$lambda.min

gl_model<-glmnet(train, train_y, alpha = 1,family="binomial")

#save model
saveRDS(gl_model,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/PSU_Stat_380/Lectures/11. MTG_price wrapup/project/volume/models/gl_model.model")

test<-as.matrix(test)

#use the full model
pred<-predict(gl_model,s=bestlam, newx = test,type="response")

bestlam
predict(gl_model,s=bestlam, newx = test,type="coefficients")
gl_model



#########################
# make a submision file #
#########################


#our file needs to follow the example submission file format.
#we need the rows to be in the correct order

example_sub$result<-pred

#now we can write out a submission
fwrite(example_sub,"/Users/niuyuefeng/Desktop/Penn state course/4/Second semester/Stat 380/PSU_Stat_380/Lectures/11. MTG_price wrapup/project/volume/data/processed/submit_3.csv")

