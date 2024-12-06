#load data
data(iris)

#Calculate mean
?mean
petal_length_mean <- mean(iris$Petal.Length)
petal_length_mean

#Plot the distribution of Petal_Lenth
hist(iris$Petal.Length)
