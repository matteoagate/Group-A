library(tibble)
apple<-read.csv("C:\\Users\\matte\\OneDrive\\Desktop\\Econometrics for finance\\AAPL.csv")
Hurst_geometric<-function(apple,d)
apple$Adj.Close<-apple$Adj.Close %>%na.omit()
res<-c()
for (i in 2:length(apple$Adj.Close)) {
  res[i]<-(log(apple$Adj.Close[i]/apple$Adj.Close[i-1]))
}
res<-res[-1]

n<-length(apple)/d
m<-seq(1:d)
D<- c()

d<-4

andrea<- as.tibble(res) %>% slice(1:(length(res)/d))
alessio<- as.tibble(res) %>% slice((round((length(res)/d),0)+1):(round(2*(length(res)/d),0)))
silvia<- as.tibble(res) %>% slice((round(2*(length(res)/d),0)+1):(round(3*(length(res)/d),0)))
matteo<- as.tibble(res) %>% slice((round(3*(length(res)/d),0)+1):(round(4*(length(res)/d),0)))

data_frame_bellissimo<-data.frame(andrea,alessio[-1256,],silvia,matteo[-1256,])



for (i in 1:length(m)){
D[i]<- tail(data_frame_bellissimo[,i],1)-head(data_frame_bellissimo[,i],1)
}


data<- data.frame(D= seq(1:d),value=c(D[1],D[2],D[3],D[4])   )
myplot<-ggplot(data)+geom_point(aes(x=log2(length(res)),y=(mean(D))))
modello<-lm(y=log2(mean(D))~log2(length(res)))





