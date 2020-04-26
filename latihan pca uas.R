#LATIHAN UAS

#SOAL UAS SK 2014/2015 NO 1

##Membuat Matriks
r1 <- c(1, 0.42, 0.34, -0.20) 
r2 <- c(0.42, 1, -0.06, 0.07)
r3 <- c(0.34, -0.06, 1, 0.26)
r4 <- c(-0.20, 0.07, 0.26, 1)

r <- rbind(r1, r2, r3, r4)
r

##Menghitung eigen value & eigen vector

eigR1 <- eigen(r)
eigR1

##Interpretasi Persamaan (dipilih 3 komponen berdasarkan proporsi varians yang dijelaskan)

'KU1 = 0.725166621 x1 + 0.546120364 x2 + 0.419767914 X3 + 0.004651932 x4'
'Komponen Pendidikan'

'KU2 = -0.1743220 x1 - 0.2431813 x2 + 0.6092932 x3 + 0.7343272 x4' 
'Komponen Perumahan'

'KU3 = -0.2339376 x1 + 0.6600214 x2 - 0.4607288 x3 + 0.5453200 x4'
'Komponen Literasi'

'Secara umum, kemiskinan di kab/kota dipengaruhi komponen pendidikan, perumahan, literasi'

##########################################################################################
#SOAL UAS SK 2015/2016 NO 2 

#a. Karena terdapat multikolinearitas pada data.

#b. ada di file materi

#c. 4 komponen utama karena telah menjelaskan 86,4% keragaman data
  'KU1 = 0.219 x1 + 0.581 x2 + 0.549 x3 - 0.261 x4 + 0.482 x5 + 0.093 x6 - 0.069 x7'
  'KU2 = 0.628 x1 + 0.056 x2 + 0.023 x3 - 0.175 x4 + 0.110 x5 + 0.076 x6 + 0.744 x7'
  'KU3 = -0.044 x1 - 0.268 x2 - 0.165 x3 - 0.369 x4 + 0.489 x5 - 0.711 x6 - 0.139 x7'
  'KU4 = 0.410 x1 + 0.293 x2 + 0.199 x3 + 0.714 x4 + 0.092 x5 + 0.412 x6 - 0.134 x7'
#d. 
  'KU1 = Komponen gula'
  'KU2 = Komponen usia'
  'KU3 = Komponen kolesterol HDL'
  'KU4 = Komponen kolesterol total'

########################################################################################
#SOAL UAS SK 2016/2017 NO 2
  #Asumsi = matriks covariance bkn dari data standar
  #Agak ambigu memahami soal, yang dimaksud dua komponen utama atau 2 variabel
  
b1 <- c(101.3, 63, 71)
b2 <- c(63, 80.2, 55.6)
b3 <- c(71, 55.6, 97.4)
s <- rbind(r1, r2, r3)
R <- cov2cor(s) #mengubah matriks covariance ke matriks korelasi
R
eigR <- eigen(R)

'KU1 = -0.59X1 - 0.57X2 - 0.57X3 --> tidak, karena ketiga variabel berkorelasi dengan KU1'

########################################################################################
#SOAL UAS SK 2017/2018 NO 1

#Matriks 1
'X1 = jumlah penduduk (dalam 100rb)'
'X2 = angka kematian ibu (dalam 1000000 kelahiran)'
'x3 = angka harapan hidup(tahun)'

#Matriks 2
#a

'X1 = rata-rata lama sekolah(tahun)'
'x2 = rasio jumlah sekolah terhadap anak(per 1000 anak)'
'x3 = persentase angka buta huruf(persen)'


#b
'Matriks korelasi --> bisa --> telah menghandle perbedaan satuan variabel'
'misal matriks kovarians yg diketahui --> liat kondisinya --> jika terdapat perbedaan satuan 
variabel --> standarisasi dulu --> bisa'

#c
#Matriks 1
b1 <- c(1, 0.81, -0.72)
b2 <- c(0.81, 1, -0.61)
b3 <- c(-0.72, -0.61, 1)

r1 <- rbind(b1, b2, b3)

eigen(r1)
'Memilih 1 komponen utama karena nilai eigen yang lebih dari 1'
'KU1 = -0.6038286 x1  -0.5768087 x2 + 0.5501662 x3'
'Komponen Demografi'

#Matriks 2

b1 <- c(1, 0.12, -0.23)
b2 <- c(0.12, 1, -0.05)
b3 <- c(-0.23, -0.05, 1)

r2 <- rbind(b1, b2, b3)
eigen(r2)

'Memilih 1 komponen utama karena nilai eigen yang lebih dari 1'
'KU1 = -0.6754620 x1  -0.3974129 x2 + 0.6211394 x3'
'Komponen Pendidikan'


# Cara menghitung Korelasi pada KU
'korelasi = vektor eigen * akar(eigen)'
eigR2 <- eigen(r2)
eigR2$vectors[ ,1] * sqrt(eigR2$values[1])
eigR2$vectors[ ,2] * sqrt(eigR2$values[2])



