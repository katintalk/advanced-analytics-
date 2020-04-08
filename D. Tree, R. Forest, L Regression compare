mdf<-read.csv("grades.csv", header=T)
View(mdf)
set.seed(1234)
mdf$admitf<-factor(mdf$admit)

library(party)
tree<-ctree(admitf~gre+gpa+rank, data=mdf, controls = ctree_control(mincriterion = 0.75, minsplit = 10))
tree
plot(tree)

ptrn<-predict(tree,mdf)
cfmtrn<-table(ptrn, mdf$admitf)
print(cfmtrn)
1-sum(diag(cfmtrn))/sum(cfmtrn)

library(randomForest)
set.seed(1234)
model1<-randomForest(admitf~.,data=mdf,mtry=3, importance=TRUE)
model1
predrf <- predict(model1, mdf, type = "class")
mean(predrf==mdf$admitf)
table(predrf, mdf$admitf)

pairs(mdf[1:4])
rgmodel<-lm(admitf~gre+gpa+rank, mdf)
rgmodel
summary(rgmodel)

#predict(rgmodel, interval = 'confidence')


#removed GRE
tree1<-ctree(admitf~gpa+rank, data=mdf, controls = ctree_control(mincriterion = 0.75, minsplit = 10))
tree1
plot(tree1)
ptrn1<-predict(tree1,mdf)
cfmtrn1<-table(ptrn1, mdf$admitf)
print(cfmtrn1)
1-sum(diag(cfmtrn1))/sum(cfmtrn1)

model2<-randomForest(admitf~gpa+rank,mdf, ntree=800,  importance=TRUE)
model2
predrf2 <- predict(model2, mdf, type = "class")
mean(predrf2==mdf$admitf)
table(predrf2, mdf$admitf)


rgmodel2<-glm(admitf~gpa+rank, mdf)
rgmodel2
summary(rgmodel2)



