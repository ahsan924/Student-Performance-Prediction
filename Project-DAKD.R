#rm(list = ls())
setwd("Y:/.../UPC/DAKD/Project/data")

d1=read.table("student-mat.csv",sep=";",header=TRUE)
d1$course="math" # add new column

d2=read.table("student-por.csv",sep=";",header=TRUE)
d2$course="portuguese"  # add new column

finaldata <- rbind(d1, d2)
finaldata <- finaldata[,-31] # remove G1
finaldata <- finaldata[,-31] # remove G"

######### Grade Conversation ############
finaldata[finaldata$G3 == 20, "G3"]= "A+"
finaldata[finaldata$G3 == 19, "G3"]= "A+"
finaldata[finaldata$G3 == 18, "G3"]= "A+"

finaldata[finaldata$G3 == 16, "G3"]= "A"
finaldata[finaldata$G3 == 17, "G3"]= "A"

finaldata[finaldata$G3 == 15, "G3"]= "B"
finaldata[finaldata$G3 == 14, "G3"]= "B"

finaldata[finaldata$G3 == 13, "G3"]= "C"
finaldata[finaldata$G3 == 12, "G3"]= "C"
finaldata[finaldata$G3 == 11, "G3"]= "C"
finaldata[finaldata$G3 == 10, "G3"]= "C"

finaldata[finaldata$G3 == 9, "G3"]= "F"
finaldata[finaldata$G3 == 8, "G3"]= "F"
finaldata[finaldata$G3 == 7, "G3"]= "F"

finaldata[finaldata$G3 == 6, "G3"]= "F"
finaldata[finaldata$G3 == 5, "G3"]= "F"
finaldata[finaldata$G3 == 4, "G3"]= "F"
finaldata[finaldata$G3 == 3, "G3"]= "F"
finaldata[finaldata$G3 == 2, "G3"]= "F"
finaldata[finaldata$G3 == 1, "G3"]= "F"

newdata<- data.frame (
           finaldata$age,  finaldata$Medu, finaldata$Fedu, finaldata$traveltime, finaldata$studytime,
           finaldata$failures, finaldata$famrel,finaldata$freetime, finaldata$goout,
           finaldata$Dalc,finaldata$Walc,finaldata$health,finaldata$absences,
           finaldata$school,  finaldata$sex, finaldata$address, finaldata$famsize, finaldata$Pstatus,
           finaldata$Mjob,finaldata$Fjob,finaldata$reason,finaldata$guardian, finaldata$schoolsup,finaldata$famsup,
           finaldata$paid, finaldata$activities ,finaldata$nursery,finaldata$higher,
           finaldata$internet,finaldata$romantic ,  finaldata$course,finaldata$G3    )

dataset <- setNames(newdata, c("age",  "Medu", "Fedu", "traveltime", "studytime",
                                   "failures", "famrel","freetime", "goout",
                                   "Dalc","Walc","health","absences",
                                   "school",  "sex", "address", "famsize", "Pstatus",
                                   "Mjob","Fjob","reason","guardian", "schoolsup","famsup",
                                   "paid", "activities" ,"nursery","higher",
                                   "internet","romantic" ,  "course", "G3"))


##### PCA Numeric #####
library(FactoMineR)
#install.packages("factoextra")
library(factoextra)
library(lattice)
library(lmtest)

model.pca = PCA(dataset[, 1:13], scale.unit=TRUE, ncp=5, graph=T)
model.desc <- dimdesc(model.pca, axes = c(1,2))
model.desc$Dim.1
model.desc$Dim.2
fviz_pca_contrib(model.pca, choice = "var", axes = 1)
fviz_pca_contrib(model.pca, choice = "var", axes = 2)
plot.PCA(model.pca, axes=c(1, 2), choix="var", select = "contrib 5", shadow=TRUE, cex=0.6)

# MCA 
devtools::install_github("kassambara/factoextra")
names(dataset)
model.mca= MCA(dataset[, 14:31], ncp = 5, graph = TRUE)
fviz_screeplot(model.mca)
fviz_contrib(model.mca, choice = "var", axes = 1, top = 10)
fviz_contrib(model.mca, choice = "var", axes = 2, top = 10)

# Data Sampling 
smp_size <- floor(0.70 * nrow(dataset))
set.seed(123)
train_ind <- sample(seq_len(nrow(dataset)), size = smp_size)

train <- dataset[train_ind, ]
test2<- dataset[-train_ind, ]

test <- dataset[-train_ind, ]
testNB <-dataset[-train_ind, ]
testNB[, 32] = ""

# Decisiton Tree C.4.5
attach(dataset)
#install.packages("randomForest")
library(randomForest)

names(train)
# fit model
trainfit <- randomForest(G3~school+internet+ paid+ address+ course+ higher +guardian + Fjob+ Pstatus+ Mjob
           + reason+ sex+  Medu+ Fedu+ Dalc+ Walc + goout , data=train)
# summarize the fit
summary(trainfit)
# make predictions
test[, 32] = ""
names(test)
head(test)
predictions <- predict(trainfit, test[,1:31])
# summarize accuracy
predictions
table(predictions, test2$G3)

#install.packages("party")
library(party)
library(caret)
x <- ctree(G3~school+internet+ paid+ address+ course+ higher +guardian + Fjob+ Pstatus+ Mjob
           + reason+ sex+  Medu+ Fedu+ Dalc+ Walc + goout , data=train)
plot(x)
pred <- predict(x, newdata=test)

confusionMatrix(pred, test2$G3)

library(class) 
library(e1071) 
model2<- naiveBayes(G3~school+internet+ paid+ address+ course+ higher +guardian + Fjob+ Pstatus+ Mjob
                   + reason+ sex+  Medu+ Fedu+ Dalc+ Walc + goout , data=train)

pred2 <- predict(model2, newdata=testNB)
confusionMatrix(pred2, test2$G3)



####### Analysis #######

finaldata2 <- rbind(d1, d2)

finaldata2$Dalc <- as.factor(finaldata2$Dalc)      
finaldata2$Dalc <- mapvalues(finaldata2$Dalc, 
                             from = 1:5, 
                             to = c("Very Low", "Low", "Medium", "High", "Very High"))

finaldata2$Walc <- as.factor(finaldata2$Walc)      
finaldata2$Walc <- mapvalues(finaldata2$Walc, 
                             from = 1:5, 
                             to = c("Very Low", "Low", "Medium", "High", "Very High"))

#font_import()
#install.packages("plyr")
library(plyr)
library(gridExtra)
library(alluvial)
library(waffle)
library(extrafont)
loadfonts(device="win")
windowsFonts(FontAwesome=windowsFont("FontAwesome"))

alcohol.d <- as.data.frame(table(finaldata2$Dalc))
par.d <- as.numeric(alcohol.d$Freq)
names(par.d) <- alcohol.d$Var1
par.d <- round(par.d/10)

waffle.col <- c("red","sienna","palevioletred1","royalblue2", "purple")
c1 <- waffle(par.d, rows=5, 
             use_glyph="shield", 
             size=2, 
             title = "Workday alcohol consumption among students",
             glyph_size=8,
             xlab="1 shield == 10 students",
             colors=waffle.col,
             legend_pos= "top"
)

alcohol.w <- as.data.frame(table(finaldata2$Walc))
par.w <- as.numeric(alcohol.w$Freq)
names(par.w) <- alcohol.w$Var1
par.w <- round(par.w/10)

c2 <- waffle(par.w, rows=5, 
             use_glyph="shield", 
             size=2, 
             title = "Weekend alcohol consumption among students",
             glyph_size=8,
             xlab="1 shield == 10 students",
             colors=waffle.col,
             legend_pos= "top"
)

grid.arrange(c1,c2, nrow=2)

########
c3 <- ggplot(finaldata2, aes(x=Dalc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#FF0000", "#0000FF"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("School")+
  ggtitle("Workday alcohol consumption per school and sex")

c4 <- ggplot(finaldata2, aes(x=Walc, y=school, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#FF0000", "#0000FF"))+
  theme_bw()+
  xlab("Weekend alcohol consumption")+
  ylab("School")+
  ggtitle("Weekend alcohol consumption per school and sex")

grid.arrange(c3,c4, nrow=2)

########
c5 <- ggplot(finaldata2, aes(x=Dalc, y=course, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#FF0000", "#0000FF"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Course")+
  ggtitle("Workday alcohol consumption per school and sex")

c6 <- ggplot(finaldata2, aes(x=Walc, y=course, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#FF0000", "#0000FF"))+
  theme_bw()+
  xlab("Weekend alcohol consumption")+
  ylab("Course")+
  ggtitle("Weekend alcohol consumption per course and sex")

grid.arrange(c5,c6, nrow=2)

########
c7 <- ggplot(finaldata2, aes(x=Dalc, y=Pstatus, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#FF0000", "#0000FF"))+
  theme_bw()+
  xlab("Workday alcohol consumption")+
  ylab("Parents Status")+
  ggtitle("Workday alcohol consumption per sex with respect to Pasrent's Status")

c8 <- ggplot(finaldata2, aes(x=Walc, y=Pstatus, color=sex))+
  geom_jitter(alpha=0.7)+
  scale_colour_manual(values=c("#FF0000", "#0000FF"))+
  theme_bw()+
  xlab("Weekend alcohol consumption")+
  ylab("Parents Status")+
  ggtitle("Weekend alcohol consumption per sex with respect to Pasrent's Status")

grid.arrange(c7,c8, nrow=2)

########
c9 <- ggplot(finaldata2, aes(x=Dalc, y=G3, fill=Dalc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption on weekdays")+
  ylab("Grade")+
  ggtitle("Final grade")

grid.arrange(c9, ncol=1)

c10 <- ggplot(finaldata2, aes(x=Walc, y=G3, fill=Walc))+
  geom_boxplot()+
  theme_bw()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Alcohol consumption on weekends")+
  ylab("Grade")+
  ggtitle("Final grade")

grid.arrange(c10, ncol=1)

########
c11 <- ggplot(finaldata2, aes(x=Dalc, y=absences, fill=Dalc))+
  geom_violin()+
  scale_fill_manual(values = waffle.col)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Weekdays alcohol consumption effect on Absences")+
  xlab("Alcohol consumption")+
  ylab("Number of school absences")

c12 <- ggplot(finaldata2, aes(x=Walc, y=absences, fill=Walc))+
  geom_violin()+
  scale_fill_manual(values = waffle.col)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Weekends alcohol consumption effect on Absences")+
  xlab("Alcohol consumption")

grid.arrange(c11,c12, nrow=2)

########
c13 <- ggplot(finaldata2, aes(x=age, fill=Dalc))+
  geom_histogram(binwidth=1, colour="black")+
  facet_grid(~Dalc)+
  scale_fill_manual(values= waffle.col)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Workday alcohol consumption per age")+
  xlab("Student's age")

c14 <- ggplot(finaldata2, aes(x=age, fill=Walc))+
  geom_histogram(binwidth=1, colour="black")+
  facet_grid(~Walc)+
  scale_fill_manual(values= waffle.col)+
  theme_bw()+
  theme(legend.position="none")+
  ggtitle("Weekend alcohol consumption per age")+
  xlab("Student's age")

grid.arrange(c13,c14, nrow=2)

