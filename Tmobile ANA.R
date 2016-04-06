##analysis code for T-mobile
#source in the cleaning code
filepath<-"C:/Users/xji/Desktop/H2P/Data Repository/Tmobile"   ##change folder as you need to
CodeName <- "Tmobile Cleaning.R"  ##cleaning file names
source(file.path(filepath,CodeName))

###descriptives
names(t)
piefun<-function(x) {
	lbls<-paste(names(table(x)),round(prop.table(table(x))*100), "%")
	pie(table(x), label=lbls, main=deparse(substitute(x)))
}
piefun(t$JobDescription)
piefun(t$Location)
piefun(t$EmployementSAP)
piefun(t$TermReason)
hist(t$TenureMonthsAtTerm)
piefun(t$ApplicationSource)
piefun(t$Assessment.Profile)
piefun(t$AssessmentBand)
hist(t$AssessmentScore)
hist(t$Experience_Records_on_Application)
piefun(t$IsCurrentJob)
#plot outcomes pattern
boxfun<-function(x) {
	a<-paste("t$",x,1,sep="")
	for (i in 2:6) {
		a<-paste(a,paste("t$",x,i,sep=""), sep=",")
	}
	boxplot(eval(a),ylim=range(c(eval(a)),na.rm=T),names=c("mo1","mo2","mo3","mo4","mo5","mo6"),main="Quality Rating by Manager")
}
boxfun(Quality_Month)

boxplot(t$Quality_Month1,t$Quality_Month2,t$Quality_Month3,t$Quality_Month4,t$Quality_Month5,t$Quality_Month6,
	ylim =range(c(t$Quality_Month1,t$Quality_Month2,t$Quality_Month3,t$Quality_Month4,
	t$Quality_Month5,t$Quality_Month6),na.rm=T),names=c("mo1","mo2","mo3","mo4","mo5","mo6"),main="Quality Rating by Manager")
}
x<-"Quality_Month"


boxplot(t$Quality_Month1,t$Quality_Month2,t$Quality_Month3,t$Quality_Month4,t$Quality_Month5,t$Quality_Month6,
	ylim =range(c(t$Quality_Month1,t$Quality_Month2,t$Quality_Month3,t$Quality_Month4,
	t$Quality_Month5,t$Quality_Month6),na.rm=T),names=c("mo1","mo2","mo3","mo4","mo5","mo6"),main="Quality Rating by Manager")
boxplot(t$Quality_Month1,t$Quality_Month2,t$Quality_Month3,t$Quality_Month4,t$Quality_Month5,t$Quality_Month6,
	ylim =range(c(t$Quality_Month1,t$Quality_Month2,t$Quality_Month3,t$Quality_Month4,
	t$Quality_Month5,t$Quality_Month6),na.rm=T),names=c("mo1","mo2","mo3","mo4","mo5","mo6"),main="Quality Rating by Manager")

table(t$ServiceProviderName)
hist(t$daydiff_12[t$daydiff_12<0])



