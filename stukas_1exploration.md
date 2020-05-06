---
title: "Churn on Telcom - Exploration"
author: "Rizqi Haryastuti"
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Brief Description

Laman ini berisi **eksplorasi** mengenai "aktifitas churn" pelanggan sebuah perusahaan telekomunikasi. Studi kasus berikut diberikan pada mata kuliah STK692 (TA 19/20), program studi S2 Statistika Terapan, IPB. Data dan keterangan dapat diakses melalui <https://www.stat.ipb.ac.id/pasca/id/index.php/2019/11/19/527/>.

Oleh:
Muthia Nadhira Faladiba, Citra Komang Sari, Rizqi Haryastuti

```{r, message=FALSE}
library(reshape2)
library(ggplot2)
library(gridExtra)
library(dplyr)
library(factoextra)
library(NbClust)
library(ggpubr)
library(viridis)
library(randomForest)
library(caret)
library(Information)
```

**Input data**
```{r}
loc <- "D:/RH/2 AKADEMIK S2/Semester 3/STK692 - Studi Kasus/telco.csv"
data <- read.csv(loc, header=T, sep=";")

sum(is.na(data))
str(data)

```

Pada peubah `voice_rev`, `broadband_rev`, dan `broadband_usg` terjadi kesalahan deteksi karakter. Seharusnya desimal menggunakan `.` namun terbaca `,`, sehingga diubah terlebih dahulu.

**Arrange data**
```{r}
voice_rev2 <- gsub(",",".",data$voice_rev)
data$voice_rev = as.numeric(voice_rev2)

broadband_rev2 <- gsub(",",".",data$broadband_rev)
data$broadband_rev = as.numeric(broadband_rev2)

broadband_usg2 <- gsub(",",".",data$broadband_usg)
data$broadband_usg = as.numeric(broadband_usg2)

data$lapsed_flag = as.factor(as.character(data$lapsed_flag))

```

```{r}
prop.table(table(data$lapsed_flag))
summary(data)
```

### Visual lapsed_flag (churn)
```{r}
table(data$lapsed_flag)
cs <- data.frame(var=c("Stay","Churn"),val=c(61398,28602))
ggplot(cs, aes(x="", y=val, fill=var)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label=paste0(val)), position=position_stack(vjust=0.5)) +
  labs(x=NULL, y=NULL, fill=NULL, title="Lapsed Flag")
```

### Visualisasi peubah prediktor
```{r}
class = ifelse(rep(data$lapsed_flag,times=16)==1,"Churn","Stay")

dfm <- data.frame(class,melt(data[-c(1,18)]))
ggplot(dfm, aes(x=variable, y=value)) + 
  geom_boxplot(aes(fill=class)) +
  facet_wrap( ~ variable, scales="free")
```

Berikut adalah gambaran proporsi revenue yang ada dalam `data telco`.

### Visualisasi proporsi revenue (keseluruhan)
```{r, message=FALSE, results="hide"}
#
sum.rchg <- sum(data$rchg_rev)
sum.voice <- sum(data$voice_rev)
sum.sms <- sum(data$sms_rev)
sum.broad <- sum(data$broadband_rev)
sum.voicepack <- sum(data$voice_package_rev)
sum.sum <- sum(sum.rchg+sum.voice+sum.sms+sum.broad+sum.voicepack)

#
revenue <- c("recharge","voice","sms","broadband","voice package")
proportion <- c(sum.rchg/sum.sum, sum.voice/sum.sum, sum.sms/sum.sum,
                sum.broad/sum.sum, sum.voicepack/sum.sum)
dfrev <- data.frame(revenue,proportion)

#
ggplot(dfrev, aes(x="", y=proportion, fill=revenue)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  geom_text(aes(label=paste0(round(proportion,3))), position=position_stack(vjust=0.5)) +
  labs(x=NULL, y=NULL, fill=NULL, title="Proportion of Revenue")

```

### Visualisasi proporsi revenue (berdasarkan class "Churn" & "Stay")
```{r, message=FALSE, results="hide"}
## churn
sum.rchg1 <- sum(data[which(data$lapsed_flag=="1"),3])
sum.voice1 <- sum(data[which(data$lapsed_flag=="1"),5])
sum.sms1 <- sum(data[which(data$lapsed_flag=="1"),9])
sum.broad1 <- sum(data[which(data$lapsed_flag=="1"),12])
sum.voicepack1 <- sum(data[which(data$lapsed_flag=="1"),15])
sum.sum1 <- sum(sum.rchg1+sum.voice1+sum.sms1+sum.broad1+sum.voicepack1)

#
revenue <- c("recharge","voice","sms","broadband","voice package")
proportion1 <- c(sum.rchg1/sum.sum1, sum.voice1/sum.sum1, sum.sms1/sum.sum1,
                sum.broad1/sum.sum1, sum.voicepack1/sum.sum1)
dfrev1 <- data.frame(revenue,proportion1)

#
rev1 <- ggplot(dfrev1, aes(x="", y=proportion1, fill=revenue)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme(legend.position = "none") +
  geom_text(aes(label=paste0(round(proportion1,3))), position=position_stack(vjust=0.5)) +
  labs(x=NULL, y=NULL, fill=NULL, title="Proportion of Revenue (Churn)")

## stay
sum.rchg0 <- sum(data[which(data$lapsed_flag=="0"),3])
sum.voice0 <- sum(data[which(data$lapsed_flag=="0"),5])
sum.sms0 <- sum(data[which(data$lapsed_flag=="0"),9])
sum.broad0 <- sum(data[which(data$lapsed_flag=="0"),12])
sum.voicepack0 <- sum(data[which(data$lapsed_flag=="0"),15])
sum.sum0 <- sum(sum.rchg0+sum.voice0+sum.sms0+sum.broad0+sum.voicepack0)

#
revenue <- c("recharge","voice","sms","broadband","voice package")
proportion0 <- c(sum.rchg0/sum.sum0, sum.voice0/sum.sum0, sum.sms0/sum.sum0,
                 sum.broad0/sum.sum0, sum.voicepack0/sum.sum0)
dfrev0 <- data.frame(revenue,proportion0)

#
rev0 <- ggplot(dfrev0, aes(x="", y=proportion0, fill=revenue)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + theme(legend.position = "none") +
  geom_text(aes(label=paste0(round(proportion0,3))), position=position_stack(vjust=0.5)) +
  labs(x=NULL, y=NULL, fill=NULL, title="Proportion of Revenue (Stay)")

##
grid.arrange(rev1,rev0,ncol=2)

```

## Bagaimana dengan *length of stay*?

Tentunya kita beranggapan bahwa pelanggan yang Churn adalah pelanggan memiliki nilai length of stay atau los yang relatif lebih kecil atau sedikit dibanding pelanggan Stay.

Pada bagian `summary(data)` didapatkan adanya kejanggalan pada nilai `max(data$los)`, yakni sebesar **330788** hari atau sekitar **906** tahun. Hal tersebut menjadi tidak make sense dan perlu pertimbangan untuk melakukan analisis selanjutnya.

```{r , results="hide"}
IQR = 2619-400 #q3-q1 data all 
minor.outlier <- (2619 + (1.5*IQR)) #5947.5 hari = 16.29 tahun
mayor.outlier <- (2619 + (3*IQR)) #9276 hari = 25.41 tahun

nrow(data[which(data$los>minor.outlier),]) #12789
data <- data[-which(data$los>minor.outlier),-c(1,24)] #77211
table(data$lapsed_flag)
summary(data)

```

## Eksplorasi tiap layanan

***RECHARGE***
```{r, message=FALSE, results="hide"}
## apakah jika rchg_trx==0 maka rchg_rev==0?
nrow(data[which(data$rchg_rev==0 & data$rchg_trx!=0),]) 
nrow(data[which(data$rchg_rev!=0 & data$rchg_trx==0),]) 
nrow(data[which(data$rchg_rev==0 & data$rchg_trx==0),]) 
nrow(data)-nrow(data[which(data$rchg_rev==0 & data$rchg_trx==0),]) 
## pelanggan rchg_trx==0 dan rchg_rev==0 belum tentu yang lain 0

```

***VOICE***
```{r, message=FALSE, results="hide"}
nrow(data[which(data$voice_rev==0 & data$voice_trx!=0),]) #6333
nrow(data[which(data$voice_rev!=0 & data$voice_trx==0),]) #0
nrow(data[which(data$voice_rev==0 & data$voice_trx==0),]) #9814

nrow(data[which(data$voice_rev==0 & data$voice_trx==0 & data$voice_mou==0),]) #9814 (mou!=0 #0)
nrow(data[which(data$voice_rev==0 & data$voice_trx==0 & data$voice_mou==0 & data$voice_dou==0),]) #9814 (dou!=0 #0)

nrow(data[which(data$voice_rev==0 & data$voice_trx!=0 & data$voice_mou!=0),]) #6195
nrow(data[which(data$voice_rev==0 & data$voice_trx!=0 & data$voice_mou==0),]) #138

nrow(data[which(data$voice_rev==0 & data$voice_trx!=0 &  data$voice_mou!=0 & data$voice_dou!=0),]) #6195
nrow(data[which(data$voice_rev==0 & data$voice_trx!=0 &  data$voice_mou!=0 & data$voice_dou==0),]) #0

nrow(data[which(data$voice_rev==0 & data$voice_trx!=0 &  data$voice_mou==0 & data$voice_dou!=0),]) #138
nrow(data[which(data$voice_rev==0 & data$voice_trx!=0 &  data$voice_mou==0 & data$voice_dou==0),]) #0

nrow(data[which(data$voice_mou==0 & data$voice_dou!=0),]) #4330
nrow(data[which(data$voice_mou!=0 & data$voice_dou==0),]) #0
nrow(data[which(data$voice_mou!=0 & data$voice_dou!=0),]) #75856

nrow(data[which(data$voice_rev!=0 & data$voice_trx!=0),]) #73853

nrow(data[which(data$voice_rev!=0 & data$voice_trx!=0 &  data$voice_mou!=0 & data$voice_dou!=0),]) #69661
nrow(data[which(data$voice_rev!=0 & data$voice_trx!=0 &  data$voice_mou==0 & data$voice_dou!=0),]) #4192

```

***SMS***
```{r, message=FALSE, results="hide"}
nrow(data[which(data$sms_rev==0 & data$sms_trx==0),]) #12302
nrow(data[which(data$sms_rev==0 & data$sms_trx!=0),]) #2849
nrow(data[which(data$sms_rev!=0 & data$sms_trx==0),]) #0
nrow(data[which(data$sms_rev!=0 & data$sms_trx!=0),]) #74849

nrow(data[which(data$sms_rev==0 & data$sms_trx==0 & data$sms_dou==0),]) #12302
nrow(data[which(data$sms_rev==0 & data$sms_trx==0 & data$sms_dou!=0),]) #0

nrow(data[which(data$sms_rev==0 & data$sms_trx!=0 & data$sms_dou==0),]) #0
nrow(data[which(data$sms_rev==0 & data$sms_trx!=0 & data$sms_dou!=0),]) #2849

nrow(data[which(data$sms_rev!=0 & data$sms_trx!=0 & data$sms_dou==0),]) #
nrow(data[which(data$sms_rev!=0 & data$sms_trx!=0 & data$sms_dou!=0),]) #74849

```

***BROADBAND***
```{r, message=FALSE, results="hide"}
nrow(data[which(data$broadband_rev==0 & data$broadband_usg==0),]) #63075
nrow(data[which(data$broadband_rev==0 & data$broadband_usg!=0),]) #7660
nrow(data[which(data$broadband_rev!=0 & data$broadband_usg==0),]) #662
nrow(data[which(data$broadband_rev!=0 & data$broadband_usg!=0),]) #18603

nrow(data[which(data$broadband_rev==0 & data$broadband_usg==0 & data$broadband_dou==0),]) #63068
nrow(data[which(data$broadband_rev==0 & data$broadband_usg==0 & data$broadband_dou!=0),]) #7

nrow(data[which(data$broadband_rev!=0 & data$broadband_usg==0 &
                  data$broadband_dou==0),]) #0
nrow(data[which(data$broadband_rev!=0 & data$broadband_usg==0 &
                  data$broadband_dou!=0),]) #662

nrow(data[which(data$broadband_rev==0 & data$broadband_usg!=0 & data$broadband_dou==0),]) #0
nrow(data[which(data$broadband_rev==0 & data$broadband_usg!=0 & data$broadband_dou!=0),]) #7660

nrow(data[which(data$broadband_rev!=0 & data$broadband_usg!=0 & data$broadband_dou==0),]) #0
nrow(data[which(data$broadband_rev!=0 & data$broadband_usg!=0 & data$broadband_dou!=0),]) #18603

```

***VOICE PACKAGE***
```{r, message=FALSE, results="hide"}
nrow(data[which(data$voice_package_rev==0 & data$voice_package_trx==0),]) #0
nrow(data[which(data$voice_package_rev==0 & data$voice_package_trx!=0),]) #577
nrow(data[which(data$voice_package_rev!=0 & data$voice_package_trx==0),]) #0
nrow(data[which(data$voice_package_rev!=0 & data$voice_package_trx!=0),]) #89423

nrow(data[which(data$voice_package_rev==0 & data$voice_package_trx!=0 & data$voice_package_dou==0),]) #0
nrow(data[which(data$voice_package_rev==0 & data$voice_package_trx!=0 & data$voice_package_dou!=0),]) #577

nrow(data[which(data$voice_package_rev!=0 & data$voice_package_trx!=0 & data$voice_package_dou==0),]) #0
nrow(data[which(data$voice_package_rev!=0 & data$voice_package_trx!=0 & data$voice_package_dou!=0),]) #89423

```

Setelah menelusuri elemen-elemen yang ada pada setiap layanan, didapatkan pola-pola yang unik dari pelanggan. Sehingga selanjutnya dibuat kategori-kategori yang dapat mewakili pola pelanggan tersebut.

**Membuat peubah kategori untuk tiap layanan **

***RECHARGE***
```{r, message=FALSE}
class.rchg <- as.factor(ifelse(data[,3]==0 & data[,4]==0,"0","1")) 

```

***VOICE***
```{r, message=FALSE}
class.voice <- as.factor(ifelse(data[,5]==0 & data[,6]==0 & data[,7]==0 & data[,8]==0,"0",
                                ifelse(data[,5]==0 & data[,6]!=0 & data[,7]!=0 & data[,8]!=0,"1",
                                       ifelse(data[,5]==0 & data[,6]!=0 & data[,7]==0 & data[,8]!=0,"2",
                                              ifelse(data[,5]!=0 & data[,6]!=0 & data[,7]==0 & data[,8]!=0,"3",
                                                     "4")))))

```

***BROADBAND***
```{r, message=FALSE}
class.broad <- as.factor(ifelse(data[,12]==0 & data[,14]==0 & data[,13]==0,"0",
                              ifelse(data[,12]==0 & data[,14]!=0 & data[,13]!=0,"2",
                                     ifelse(data[,12]!=0 & data[,14]!=0 & data[,13]!=0,"3",
                                            "1"))))

```

***SMS***
```{r, message=FALSE}
class.sms <- as.factor(ifelse(data[,9]==0 & data[,10]==0 & data[,11]==0,"0",
                              ifelse(data[,9]!=0 & data[,10]!=0 & data[,11]!=0,"2",
                                     "1")))

```

***VOICE PACKAGE***
```{r, message=FALSE}
class.voice_pack <- as.factor(ifelse(data[,15]!=0 & data[,16]!=0 & data[,17]!=0,"1",
                                     "0"))

```

### Visualisasi peubah kategorik tiap layanan
```{r, message=FALSE}
class.lapsed = ifelse(data$lapsed_flag==1,"Churn","Stay")
dfclass = data.frame(class.lapsed,class.rchg,class.voice,class.broad,class.sms,class.voice_pack)

p.rchg <- ggplot(dfclass, aes(x=class.rchg, fill=class.lapsed)) +
  geom_bar(position="fill")

p.voice <- ggplot(dfclass, aes(x=class.voice, fill=class.lapsed)) +
  geom_bar(position="fill")

p.broad <- ggplot(dfclass, aes(x=class.broad, fill=class.lapsed)) +
  geom_bar(position="fill")

p.sms <- ggplot(dfclass, aes(x=class.sms, fill=class.lapsed)) +
  geom_bar(position="fill")

p.voice_pack <- ggplot(dfclass, aes(x=class.voice_pack, fill=class.lapsed)) +
  geom_bar(position="fill")

grid.arrange(p.rchg,p.voice,p.broad,p.sms,p.voice_pack,top="Churn & Stay Proportion")

```

Berdasarkan diagram batang untuk setiap peubah kategori layanan tersebut, didapatkan perbedaan tingkat Churn dan Stay yang cukup berarti pada kategori dalam layanan `rchg`, `voice`, `sms`, dan `voice_package`.

```{r, message=FALSE}
## risiko rchg 
prop.table(table(dfclass$class.lapsed,dfclass$class.rchg),margin=2)

## risiko voice
prop.table(table(dfclass$class.lapsed,dfclass$class.voice),margin=2)

## risiko broadband
prop.table(table(dfclass$class.lapsed,dfclass$class.broad),margin=2)

## risiko SMS
prop.table(table(dfclass$class.lapsed,dfclass$class.sms),margin=2)

## risiko voice_package
prop.table(table(dfclass$class.lapsed,dfclass$class.voice_pack),margin=2)

```

Untuk memperkuat gambaran tersebut, dilakukan pemodelan regresi logistik yang melibatkan 5 peubah kategorik baru tersebut sebagai prediktor serta lapsed_flag sebagai respon.

```{r, message=FALSE}
m1 <- glm(class.lapsed~., data=dfclass, family=binomial(link="logit"))
summary(m1)

```

Namun dari output tersebut kami menemukan kejanggalan. Output regresi logistik pada R Markdown tidak sama dengan output ketika kami menjalankan program pada RStudio atau R CLI. Berikut kami sertakan output dari RStudio:

![Output Regresi Logistik](../reglog.JPG)

Setiap layanan terdapat minimal satu kategori yang signifikan pada taraf nyata 5%, maka cukup bukti untuk menyatakan bahwa pengelompokkan setiap layanan signfikan.

```{r, message=FALSE}


```

```{r, message=FALSE}


```
