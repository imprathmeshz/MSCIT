install.packages("caTools")
install.packages("e1071")

library(caTools)
library(e1071)

ds = read.csv("Social_Network_Ads.csv")
ds

ds=ds[3:5]
ds

set.seed(123)
split = sample.split(ds$Purchased,SplitRatio = 0.75)
training_set = subset(ds,split == TRUE)
test_set = subset(ds,split==FALSE)
ds


test_set[-3] = scale(test_set[-3])
training_set[-3] = scale(training_set[-3])
test_set[-3] = scale(test_set[-3])
test_set[-3]

classifier = svm(formula = Purchased ~ .,data = training_set,type='C-classification',kernel = 'linear')
classifier

Y_pred = predict(classifier,newdata = test_set[-3])

Y_pred

cm = table(test_set[,3],Y_pred)
cm

set = training_set
X1=seq(min(set[,1])-1,max(set[,1])+1,by = 0.01)
X2=seq(min(set[,2])-1,max(set[,2])+1,by = 0.01)

grid_set = expand.grid(X1,X2)
colnames(grid_set) = c('Age','EstimatedSalary')
Y_grid= predict(classifier, newdata = grid_set)
plot(set[,-3],main = 'svm (Training set)',
xlab = 'Age', ylab = 'Estimated Salary',
xlim = range(X1),ylim=range(X2))

contour(X1,X2, matrix(as.numeric(y_grid),length(X1), length(X2)),add=TRUE)
points(grid_set,pch='.', col=ifelse(Y_grid ==1,'coral1','aquamarine'))
points(set,pch=21,bg=ifelse(set[,3] == 1,'green4','red3'))