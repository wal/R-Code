pima <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data",header=F,sep=",")


colnames(pima)<-c("npreg","glucose","bp","triceps","insulin","bmi","diabetes","age","class")



pima$glucose[pima$glucose==0]<-NA
pima$bp[pima$bp==0]<-NA
pima$triceps[pima$triceps==0]<-NA
pima$insulin[pima$insulin==0]<-NA
pima$bmi[pima$bmi==0]<-NA


pima$class<-factor(pima$class)
levels(pima$class)<-c("neg","pos")


plot(density(pima$bp,na.rm=TRUE))
