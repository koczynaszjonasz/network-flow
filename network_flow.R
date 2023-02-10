install.packages("ggplot2")
install.packages("RColorBrewer")
install.packages("dplyr")  
install.packages("tidyverse") 
install.packages("scales")
install.packages("installr")
install.packages("ggvis")
install.packages("ggpubr")
install.packages("corrplot")
install.packages("heatmaply")
install.packages("plotrix")

library("plotrix")
library("heatmaply")
library("corrplot")
library("ggpubr")
library('ggvis')
library("dplyr") 
library("tidyverse") 
# Biblioteka do wykresów
library("ggplot2")

#Biblioteka do kolorów w wykresach
library("RColorBrewer")

options(scipen=999) # usuwamy postać wykładniczą

setwd("C:/Users/Sebastian/Desktop/Studia/II rok 2 semestr/ASR_PROJEKT")

netflowds <- read.csv("Train_Test_Network.csv", encoding = "UTF-8")

netflowds

summary(netflowds)

(netflowds3 <-netflowds[netflowds$`dns_query`!="(empty)" & netflowds$`dns_query`!="-",]) # wykluczanie niepotrzebnych linii

sorttowanie <- (sort(table(netflowds3$'dns_query'),DECREASING=F))

paletabarw <- brewer.pal(6, "Set2")
(takiwykres <- pie(tail(sorttowanie),
                       border="white" ,
                       main = "wykres",
                       col = paletabarw))
dev.off() 

sorttowanie2 <- (sort(table(netflowds$'proto'),DECREASING=F))

paletabarw <- brewer.pal(3, "Set2")
(takiwykres <- pie(tail(sorttowanie2),
                   border="white" ,
                   main = "wykres",
                   col = paletabarw))

mean(netflowds$'duration')

mean(netflowds$'missed_bytes')

sd(netflowds$'duration')

sd(netflowds$'missed_bytes')

sum(netflowds$'duration')

sum(netflowds$'missed_bytes')


bytesperipmean <- (aggregate(netflowds$'missed_bytes', by=list(netflowds$'dst_ip'), FUN = mean))

bytesperipmeansorted <- bytesperipmean[with(bytesperipmean, order(x, Group.1, decreasing = T)), ]

bytesperipsd <- (aggregate(netflowds$'missed_bytes', by=list(netflowds$'dst_ip'), FUN = sd))

bytesperipsdsorted <- bytesperipsd[with(bytesperipsd, order(x, Group.1, decreasing = T)), ]

bytesperip <- (aggregate(netflowds$'missed_bytes', by=list(netflowds$'dst_ip'), FUN = sum))

bytesperipsorted <- bytesperip[with(bytesperip, order(x, Group.1, decreasing = T)), ]

bytesperipsorted <- bytesperipsorted %>% 
  mutate(inmb = round((bytesperipsorted$x / 1048576), 2))

bytesperipsorted$x <- NULL

TES1 <- head(bytesperipsorted, n = 10)

TES2 <- head(bytesperipsorted, n = 3)

TES3 <- tail(TES1, n = 7)

str(bytesperipsorted)

ggplot(TES1, aes(Group.1, inmb)) 
  + geom_bar(stat = 'identity') 
  + theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(TES2, aes(Group.1, inmb)) 
  + geom_bar(stat = 'identity') 
  + theme(axis.text.x = element_text(angle=65, vjust=0.6))

ggplot(TES3, aes(Group.1, inmb)) 
  + geom_bar(stat = 'identity') 
  + theme(axis.text.x = element_text(angle=65, vjust=0.6))

View(head(bytesperipsorted, n = 50))

src_pkts <- netflowds %>%
  select(src_pkts, dst_pkts, src_bytes, dst_bytes, duration,  missed_bytes, src_ip_bytes, dst_ip_bytes ) %>%
  filter(src_pkts >0 & dst_pkts >0 )

cor(src_pkts$src_pkts,src_pkts$dst_pkts, method="spearman")

length(src_pkts$src_pkts)
length(src_pkts$dst_pkts)

corrplot(corr = cor(src_pkts), method='number')

str(netflowds)

r <- cor(src_pkts)
cor.test.p <- function(x){
  FUN <- function(x, y) cor.test(x, y)[["p.value"]]
  z <- outer(
    colnames(x), 
    colnames(x), 
    Vectorize(function(i,j) FUN(x[,i], x[,j]))
  )
  dimnames(z) <- list(colnames(x), colnames(x))
  z
}
p <- cor.test.p(src_pkts)

heatmaply_cor(
  r,
  node_type = "scatter",
  point_size_mat = p,
  point_size_name = "p value",
  label_names = c("x", "y", "Correlation")
)

#----------------------------------------------

koripbytes <- cor.test(src_pkts$src_ip_bytes, src_pkts$dst_ip_bytes, method="pearson")

koripbytes

korpkts <- cor.test(src_pkts$src_pkts, src_pkts$dst_pkts, method="pearson")

korpkts

korbytes <- cor.test(src_pkts$src_bytes, src_pkts$dst_bytes, method="pearson")

korbytes

kordest <- cor.test(src_pkts$duration, src_pkts$dst_bytes, method="pearson")

kordest

type1 <- netflowds[netflowds$`type`!="normal",]

typeanalysis <- (sort(table(type1$'type'),DECREASING=F))

ggplot(data.frame(typeanalysis), aes(Var1, Freq)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle=65, vjust=0.6)) + scale_y_continuous(breaks=seq(0,20000,by=1000))

typeanalysis2 <- (sort(table(netflowds$'type'),DECREASING=F))

ggplot(data.frame(typeanalysis2), aes(Var1, Freq)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle=65, vjust=0.6)) + scale_y_continuous(breaks=seq(0,300000,by=20000))

status_polaczenia <- (sort(table(netflowds$'conn_state'),DECREASING=F))

view(status_polaczenia)

stat_data <- data.frame(status_polaczenia)

freqdata <- stat_data %>%
  select(2)

namedata <- stat_data %>%
  select(1)

proba<- stat_data[stat_data$Var1 == "S0" & stat_data$Var1 == "S1" & stat_data$Var1 == "REJ", ]

pie3D(as.numeric(unlist(freqdata)),labels = as.character(unlist(namedata)),explode = 0.1, main = "dasdsa ")
