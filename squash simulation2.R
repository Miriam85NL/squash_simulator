rm(list=ls())

library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(reshape2)


#p=probability player 1 wins at own serve , q= probability player 2 wins at own serve
#player 1 always starts

#n=number of simulations

#system 1: current system. Rally point
#system 2: previous system: only point at own serve
#system 3: alternating serve
#system 4: tie break like tennis tie break

#first to N with difference of k
#system 1,3,4: N=11, k=2
#system 2: N=9, k=1


squash_sim<-function(n,p,q,system,N,k) {
#vector winner and length of game, score of player 1 and 2
winner<-rep(0,n)
length<-rep(0,n)
score1<-rep(0,n)
score2<-rep(0,n)

for (i in 1:n){
serveerder=1
length_help=1
winner_help=0
score1_help=0
score2_help=0
  while (winner_help==0){
    rnumber=runif(1, min = 0, max = 1);
    
    if(system==1){
      if(serveerder==1){
         if(rnumber<p) {
           score1_help=score1_help+1;
           serveerder=1
         } else{
          score2_help=score2_help+1;
          serveerder=2
        }  
      } else {
        
      if(rnumber>q) {
       score1_help=score1_help+1;
          serveerder=1
        } else{
          score2_help=score2_help+1;
          serveerder=2
        }  
      }
    }
    
    if(system==2){
      if(serveerder==1){
        if(rnumber<p) {
          score1_help=score1_help+1;
          serveerder=1
        } else{
          #score2_help=score2_help+1;
          serveerder=2
        }  
      } else {
        
        if(rnumber>q) {
          #score1_help=score1_help+1;
          serveerder=1
        } else{
          score2_help=score2_help+1;
          serveerder=2
        }  
      }
    }
    
    if(system==3){
      if(serveerder==1){
        if(rnumber<p) {
          score1_help=score1_help+1;
          serveerder=2
        } else{
          score2_help=score2_help+1;
          serveerder=2
        }  
      } else {
        
        if(rnumber>q) {
          score1_help=score1_help+1;
          serveerder=1
        } else{
          score2_help=score2_help+1;
          serveerder=1
        }  
      }
    }
    
    
    if(system==4){
      if(serveerder==1){
        if(rnumber<p) {
          score1_help=score1_help+1;
          if((score1_help+score2_help)%%4==1||(score1_help+score2_help)%%4==2){
          serveerder=2
          } else{
            serveerder=1
          }
        } else{
          score2_help=score2_help+1;
          if((score1_help+score2_help)%%4==1||(score1_help+score2_help)%%4==2){
            serveerder=2
          } else{
            serveerder=1
          }
        }  
      } else {
        
        if(rnumber>q) {
          score1_help=score1_help+1;
          if((score1_help+score2_help)%%4==1||(score1_help+score2_help)%%4==2){
            serveerder=2
          } else{
            serveerder=1
          }
        } else{
          score2_help=score2_help+1;
          if((score1_help+score2_help)%%4==1||(score1_help+score2_help)%%4==2){
            serveerder=2
          } else{
            serveerder=1
          }
        }  
      }
    }
      
    length_help=length_help+1 
    
    if(score1_help>=N &&(score1_help-score2_help)>=k){winner_help=1
    }
    if(score2_help>=N &&(score2_help-score1_help)>=k){winner_help=2
    }
    #print(rnumber)
    #print(score1_help)
    #print(score2_help)
  }
winner[i]=winner_help
length[i]=length_help
score1[i]=score1_help
score2[i]=score2_help

}
per_win_1=sum(winner==1)/n
av_len=mean(length)
return(list(winner,length,score1,score2,per_win_1,av_len))
}

#players with equal strength

#probability that player 1 wins when both have equal strength

probwin1<-rep(0,9)
probwin2<-rep(0,9)
probwin3<-rep(0,9)
probwin4<-rep(0,9)
len1<-rep(0,9)
len2<-rep(0,9)
len3<-rep(0,9)
len4<-rep(0,9)
#probwin1_theoretical<-rep(0,9)

for (j in 1:9) {
  sim<-squash_sim(1000000,j/10,j/10,1,11,2)
  probwin1[j]<-sim[5]
  len1[j]<-sim[6]
  #probwin1_theoretical[j]<-F2b(j/10,j/10,11)
}

for (j in 1:9) {
  sim<-squash_sim(1000000,j/10,j/10,2,9,1)
  probwin2[j]<-sim[5]
  len2[j]<-sim[6]
  
}

for (j in 1:9) {
  sim<-squash_sim(1000000,j/10,j/10,3,11,2)
  probwin3[j]<-sim[5]
  len3[j]<-sim[6]
  
}

for (j in 1:9) {
  sim<-squash_sim(1000000,j/10,j/10,4,11,2)
  probwin4[j]<-sim[5]
  len4[j]<-sim[6]
  
}

x<-c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

data_gelijkesterkte<-data.frame(x,sys1=unlist(probwin1),sys2=unlist(probwin2),sys3=unlist(probwin3),sys4=unlist(probwin4),len1=unlist(len1),len2=unlist(len2),len3=unlist(len3),len4=unlist(len4))
save(data_gelijkesterkte,file="C:\\Users\\looic\\OneDrive - HvA\\Documents\\Documents\\squash\\data_gelijkesterkte.Rda")



#players with unequal strength
probwin1<-rep(0,9)
probwin2<-rep(0,9)
probwin3<-rep(0,9)
len1<-rep(0,9)
len2<-rep(0,9)
len3<-rep(0,9)
#probwin1_theoretical<-rep(0,9)

for (j in 1:9) {
  sim<-squash_sim(1000000,j/10+0.05,j/10-0.05,1,11,2)
  probwin1[j]<-sim[5]
  len1[j]<-sim[6]
  #probwin1_theoretical[j]<-F2b(j/10,j/10,11)
}

for (j in 1:9) {
  sim<-squash_sim(1000000,j/10+0.05,j/10-0.05,2,9,1)
  probwin2[j]<-sim[5]
  len2[j]<-sim[6]
  
}

for (j in 1:9) {
  sim<-squash_sim(1000000,j/10+0.05,j/10-0.05,3,11,2)
  probwin3[j]<-sim[5]
  len3[j]<-sim[6]
  
}

for (j in 1:9) {
  sim<-squash_sim(1000000,j/10+0.05,j/10-0.05,4,11,2)
  probwin4[j]<-sim[5]
  len4[j]<-sim[6]
  
}
x2<-c("0.15/0.05","0.25/0.15","0.35/0.25","0.45/0.35","0.55/0.45","0.65/0.55","0.75/0.65","0.85/0.75","0.95/0.85")
x<-c(0.15,0.25,0.35,0.45,0.55,0.65,0.75,0.85,0.95)


oud1<-squash_sim(1000000,0.5,0.5,2,9,1)
oud2<-squash_sim(1000000,0.6,0.5,2,9,1)
huidige1<-squash_sim(1000000,0.5,0.5,1,11,2)
huidige2<-squash_sim(1000000,0.6,0.5,1,11,2)


#https://r-graph-gallery.com/connected_scatterplot_ggplot2.html
data_sterkteverschil<-data.frame(x,x2,sys1=unlist(probwin1),sys2=unlist(probwin2),sys3=unlist(probwin3),sys4=unlist(probwin4),,len1=unlist(len1),len2=unlist(len2),len3=unlist(len3),len4=unlist(len4))
save(data_sterkteverschil,file="C:\\Users\\looic\\OneDrive - HvA\\Documents\\Documents\\squash\\data_sterkteverschil.Rda")

#figuur 1
load("C:\\Users\\looic\\OneDrive - HvA\\Documents\\Documents\\squash\\data_gelijkesterkte.Rda")

data<-data.frame(x=data_gelijkesterkte$x,huidig=data_gelijkesterkte$sys1,oud=data_gelijkesterkte$sys2,tiebreak=data_gelijkesterkte$sys4)

data<-melt(data,id.vars=1)
colnames(data)[2]<-"puntentelling"


ggplot(data,aes(x=x,y=value))+
  #theme_ipsum(base_size = 13,axis_title_size = 14) +
  theme_classic(base_size = 13)+
  geom_line(aes(color=puntentelling,group=puntentelling))+
  geom_point(aes(color=puntentelling),size=4)+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  scale_y_continuous(breaks=seq(0.35,0.65,0.05))+
  ggtitle("Spelers met gelijke sterkte: winkans")+
  labs(x = "Kans dat speler een rally wint open eigen service")+
  labs(y = "Kans dat speler 1 de game wint") +
  theme(legend.position = c(0.9,0.2),legend.title = element_text(size=13, face="bold"),legend.background = element_rect(fill="white", 
                                                                                                                        size=0.5, linetype="solid"),legend.text=element_text(size=12))
#figuur 2

data<-data.frame(x=data_gelijkesterkte$x,huidig=data_gelijkesterkte$len1,oud=data_gelijkesterkte$len2,tiebreak=data_gelijkesterkte$len4)

data<-melt(data,id.vars=1)
colnames(data)[2]<-"puntentelling"


ggplot(data,aes(x=x,y=value))+
  #theme_ipsum(base_size = 13,axis_title_size = 14) +
  theme_classic(base_size = 13)+
  geom_line(aes(color=puntentelling,group=puntentelling))+
  geom_point(aes(color=puntentelling),size=4)+
  scale_x_continuous(breaks=seq(0,1,0.1))+
  #scale_y_continuous(breaks=seq(0.35,0.65,0.05))+
  ggtitle("Spelers met gelijke sterkte: lengte game")+
  ylim(0,80)+
  labs(x = "Kans dat speler rally wint op eigen service")+
  labs(y = "Gemiddeld aantal rally's per game") +
  theme(legend.position = c(0.9,0.9),legend.title = element_text(size=13, face="bold"),legend.background = element_rect(fill="white", size=0.5, linetype="solid"),legend.text=element_text(size=12))
                                                                                                                        

#figuur 3                                                                
load("C:\\Users\\looic\\OneDrive - HvA\\Documents\\Documents\\squash\\data_sterkteverschil.Rda")
data<-data.frame(x=data_sterkteverschil$x2,huidig=data_sterkteverschil$sys1,oud=data_sterkteverschil$sys2,tiebreak=data_sterkteverschil$sys4)
library(reshape2)
data<-melt(data)
colnames(data)[2]<-"puntentelling"


ggplot(data,aes(x=x,y=value))+
  #theme_ipsum(base_size = 13,axis_title_size = 14) +
  theme_classic(base_size = 13)+
  geom_line(aes(color=puntentelling,group=puntentelling))+
  geom_point(aes(color=puntentelling),size=4)+
  scale_y_continuous(breaks=seq(0.65,1,0.05))+
  ggtitle("Speler 1 is sterker: winkans")+
  labs(x = "Kans dat speler 1 resp. 2 een rally wint op eigen service")+
  labs(y = "Kans dat speler 1 de game wint") +
  theme(legend.position = c(0.9,0.85),legend.title = element_text(size=13, face="bold"),legend.background = element_rect(fill="white", 
                                                                                                                         size=0.5, linetype="solid"),legend.text=element_text(size=12))




