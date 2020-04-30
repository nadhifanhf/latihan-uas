##ANALISIS DISKRIMINAN DATA IRIS MANUAL

library(dplyr) #Data processing

data("iris") #Load dataset

View(iris)


##Kategori Setosa##

setosa <- iris %>%
  filter(Species == 'setosa')
setosa

#Menghitung vektor rata-rata Setosa

setosa_mean <- sapply(setosa[, -5], mean)
setosa_mean

#Menghitung matriks kovarians Setosa
covarians_setosa <- round(cov(setosa[, -5]), 2)
covarians_setosa


##Kategori Versicolor##
versicolor <- iris %>%
  filter(Species == 'versicolor')
versicolor

#Menghitung vektor rata-rata Versicolor
versicolor_mean <- sapply(versicolor[, -5], mean)
versicolor_mean

#Menghitung matriks kovarians Versicolor
covarians_versicolor <- round(cov(versicolor[, -5]), 2)
covarians_versicolor

##Kategori Virginica##

virginica <- iris %>%
  filter(Species == 'virginica')

#Menghitung vektor rata-rata Virginica
virginica_mean <- sapply(virginica[, -5], mean)
virginica_mean

#Menghitung matriks kovarians Virginica
covarians_virginica <- round(cov(virginica[, -5]), 2)
covarians_virginica

#Menghitung matriks kovarian gabungan  (Spooled)
spooled <- (49 * covarians_setosa + 49 * covarians_versicolor + 49 * covarians_virginica)/(49 + 49 + 49)
round(spooled, 2)

#Fungsi Diskriminan

#Setosa

-0.5 * t(setosa_mean) %*% solve(spooled) %*%  setosa_mean #intercept

t(setosa_mean) %*% solve(spooled) #koeffisien

ld_setosa <- '-85.61 + 23.41 X1 + 23.63 X2 - 15.91 X3 - 18.38 X4 + ln 0.33'

#Versicolor

-0.5 * t(versicolor_mean) %*% solve(spooled) %*%  versicolor_mean #intercept

t(versicolor_mean) %*% solve(spooled) #koeffisien

ld_versicolor <- '-71.92 + 15.36 X1 + 7.63 X2 + 5.96 X3 + 4.58 X4 + ln 0.33'

#Virginica

-0.5 * t(virginica_mean) %*% solve(spooled) %*%  virginica_mean #intercept

t(virginica_mean) %*% solve(spooled) #koeffisien

ld_virginica <- '-102.89 + 11.86 X1 + 4.53 X2 + 13.81 X3 + 18.51 X4 + ln 0.33'

