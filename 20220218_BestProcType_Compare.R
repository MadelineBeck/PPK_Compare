proc.type <- c(rep('20220218_Diffs',5),rep('20220218_Diffs_OutlierRemoved',4))
proc.type <- as.factor(proc.type)
diffs <- c(0.087695,-0.008027,-0.085615,0.143506,-0.012559,
           0.087695,-0.008027,-0.085615,-0.012559)
mnvec <- tapply(diffs,proc.type,mean)
nvec <- tapply(diffs,proc.type,length)
stdvec <- tapply(diffs,proc.type,sd)          

aovdata <- data.frame(cbind(diffs,proc.type))
attach(aovdata)

# Make side-by-side boxplots
quartz()
boxplot(diffs~proc.type,main="Differences by Processing Type",
        xlab = 'Snow Depth Differences',
        ylab = 'Differences (m)'
        )
abline(h= 0, col = "red")

# ANOVA using aov function
f1 <- aov(diffs~as.factor(proc.type)) 
summary(f1)

f2 <- lm(diffs~as.factor(proc.type)) 
summary(f2)

# Means Model ANOVA using aov and lm functions 
f3 <- aov(diffs~as.factor(proc.type))    # same ANOVA table as effects model
summary(f3)
f4 <- lm(diffs~as.factor(proc.type))   # same ANOVA table as effects model
summary(f4)   

hist(c(0.087695,-0.008027,-0.085615,-0.012559))