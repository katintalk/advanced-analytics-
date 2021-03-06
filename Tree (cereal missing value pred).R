library("RSQLite")
db<-dbConnect(SQLite(), dbname="cereal.db")
db
View(db)
dbListTables(db)
mdftrain<-data.frame(dbGetQuery(db,'select calories, protein, fat from cereal1'))
mdftrain$fatf<-factor(mdftrain$fat)
library(party)
tree<-ctree(fatf~calories+protein, data=mdftrain, controls = ctree_control(mincriterion = 0.75, minsplit = 10))
tree
plot(tree)
ptrn<-predict(tree, mdftrain)
cfmtrn<-table(ptrn, mdftrain$fatf)
print(cfmtrn)
1-sum(diag(cfmtrn))/sum(cfmtrn)
mdftest<-data.frame(dbGetQuery(db,'select calories, protein, fat from cereal2'))
ptrn2<-predict(tree, mdftest)
print(ptrn2)
