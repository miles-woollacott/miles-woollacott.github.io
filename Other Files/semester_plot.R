require(readxl)
require(ggplot2)
require(plotly)

setwd("~/Documents/GitHub/miles-woollacott.github.io/Other Files/")
sem.csv<-read_excel("semester_reviews.xlsx")

# Get bootstrap median and CI
sem.15<-sem.csv[,c("1", "2", "3", "4", "5")]
sem.csv$Response.Rate<-rowSums(sem.15)/sem.csv$`Total Students`
sem.csv$Mean<-as.matrix(sem.15)%*%1:5/(sem.csv$`Total Students`*sem.csv$Response.Rate)
sem.csv$no.missing<-sem.csv$`Total Students`*(1-sem.csv$Response.Rate)
sem.probs<-t(apply(sem.15+1, 1, function(x){x/sum(x)}))

sem.df<-as.matrix(sem.15)
overall.mean<-mean((sem.df%*%1:5)/rowSums(sem.df))
overall.mean

sem.response<-list()
n<-nrow(sem.csv)
for(i in 1:n){
  sem.response[[i]]<-rep(1:5, sem.15[i,])
}

B<-24000
boot.samps<-matrix(NA, nrow=B, ncol=n)
set.seed(16)
pb <- txtProgressBar(min=1, max=B, style=3)
for(i in 1:B){
  for(j in 1:n){
    i.samp<-sample(1:5, size=sem.csv$no.missing[j], prob=sem.probs[j,], replace=T)
    boot.samps[i,j]<-mean(c(sem.response[[j]], i.samp))
  }
  setTxtProgressBar(pb, i)
}
close(pb)

# Aggregate data
semesters<-sem.csv$Semester
sem.data<-data.frame(semester=factor(semesters, levels=unique(semesters)),
                     Topic=sem.csv$Question,
                     s.mean=sem.csv$Mean,
                     SE=apply(boot.samps, 2, sd),
                     ci.025=apply(boot.samps, 2, quantile, 0.025),
                     ci.975=apply(boot.samps, 2, quantile, 0.975),
                     Response.Rate=sem.csv$Response.Rate)

# Create plot

p<-ggplot(sem.data, aes(x=semester, y=s.mean, color=Topic, group=Topic,
                     fill=Topic))+
  annotate("rect", xmin=-Inf, xmax=2.5, ymin=-Inf, ymax=Inf, alpha=.2)+
  geom_smooth(aes(x=semester, y=ci.025), se=F, linetype=2, linewidth=0.5)+
  geom_smooth(aes(x=semester, y=ci.975), se=F, linetype=2, linewidth=0.5)+
  geom_vline(xintercept=2.5, color="red", linetype=2)+
  annotate(x=1.5, y=+Inf, label="Hybrid Instructor", vjust=2, geom="label")+
  annotate(x=3.1, y=+Inf, label="Seated Instructor", vjust=2, geom="label")+
  geom_smooth(se=F)+
  geom_point(size=4)+
  scale_color_manual(values=c("Course"="purple", "Instructor"="dodgerblue"))+
  labs(y="Average Rating (From 1 to 5)", x="Semester")+theme_classic()+
  scale_y_continuous(limits=c(1,5))
 
ggplotly(p)   