# This code is the first step to analyzing how the different processing types perform for accurately
# capturing 
proc.type <- c(rep('1_NoPPK_NoAlign',5),rep('2_PPK_HeightAdj_NoAlign',5),rep('3_PPK_NoHeightAdj_NoAlign',5),
            rep('6_PPK_NoHeightAdj_4DAlign',5),rep('7_PPK_NoHeightAdj_4DAlign_Paired',5), rep('4_PPK_HeightAdj_4DAlign',5),
            rep('5_PPK_HeightAdj_4DAlign_Paired',5), rep('8_NoPPK_4DAlign',5), rep('9_NoPPK_4DAlign_Paired',5))
proc.type <- as.factor(proc.type)

diffs <- c(-0.172803,--0.372285,-0.469404,0.845654,-0.208848,
           -0.549023,-0.652803,-0.717939,-0.693652,-0.69957,
           0.087695,-0.008027,-0.085615,0.143506,-0.012559,
           0.070605,-0.051973,-0.196455,-0.176074,-0.19127,
           0.162402,-0.060273,-0.188154,-0.197803,-0.211533,
           -0.496533,-0.716035,-0.817793,-0.797168,-0.867051,
           -0.457959,-0.642305,-0.803145,-0.756885,-0.827744,
           -0.062939,-0.656221,-0.424971,1.250195,-0.1046,
           1.455615,1.039336,1.417803,1.417803,1.743789)

mnvec <- tapply(diffs,proc.type,mean)
nvec <- tapply(diffs,proc.type,length)
stdvec <- tapply(diffs,proc.type,sd)
# Bind the data to assign the batch number to each amount value
aovdata <- data.frame(cbind(diffs,proc.type))
attach(aovdata)

# Make side-by-side boxplots
bp <- boxplot(diffs ~ proc.type, data = aovdata, xaxt = "n",main="20220218 Differences (meters) by Processing Type",
              xlab = 'Processing Model',
              ylab = 'Difference (meters)')
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.3, bp$names, srt = 45, xpd = TRUE)
abline(h = 0, col = "red")

quartz()
boxplot(diffs~proc.type,main="Differences (meters) by Processing Type",
        xlab = 'Processing Types',
        ylab = 'Difference (meters)')
abline()

# ANOVA using aov function
f1 <- aov(diffs~as.factor(proc.type)) 
summary(f1)

f2 <- lm(diffs~as.factor(proc.type)) 
summary(f2)

# Means Model ANOVA using aov and lm functions 
f3 <- aov(diffs~as.factor(proc.type))    # same ANOVA table as effects model
summary(f3)
f4 <- lm(diffs~as.factor(proc.type))   # same ANOVA table as effects model
summary(f4)            # output below   Signif. codes:

# Model 1 is the baseline
model.1 <- aovdata[aovdata$proc.type == '1',]
# the rest are the models we wish to compare
model.2 <- aovdata[aovdata$proc.type == '2',]
model.3 <- aovdata[aovdata$proc.type == '3',]
model.4 <- aovdata[aovdata$proc.type == '4',]
model.5 <- aovdata[aovdata$proc.type == '5',]
model.6 <- aovdata[aovdata$proc.type == '6',]
model.7 <- aovdata[aovdata$proc.type == '7',]
model.8 <- aovdata[aovdata$proc.type == '8',]
model.9 <- aovdata[aovdata$proc.type == '9',]

t.test(model.1$diffs,mu=0)
t.test(model.2$diffs,mu=0)
t.test(model.3$diffs,mu=0)
t.test(model.4$diffs,mu=0)
t.test(model.5$diffs,mu=0)
t.test(model.6$diffs,mu=0)
t.test(model.7$diffs,mu=0)
t.test(model.8$diffs,mu=0)
t.test(model.9$diffs,mu=0)

# Now let's see if models 3, 6, and 7 differ from each other.
t.test(model.1,model.3,var.equal=T)
t.test(model.1,model.6,var.equal=T)
t.test(model.1,model.7,var.equal=T)
t.test(model.3,model.6,var.equal=T)
t.test(model.3,model.7,var.equal=T)
t.test(model.6,model.7,var.equal=T)

mean(model.1$diffs)
mean(model.2$diffs)
mean(model.3$diffs)
mean(model.4$diffs)
mean(model.5$diffs)
mean(model.6$diffs)
mean(model.7$diffs)
mean(model.8$diffs)
mean(model.9$diffs)

sd(model.1$diffs)
sd(model.2$diffs)
sd(model.3$diffs)
sd(model.4$diffs)
sd(model.5$diffs)
sd(model.6$diffs)
sd(model.7$diffs)
sd(model.8$diffs)
sd(model.9$diffs)

df_list <- list(model.1, model.3, model.6, model.7, model.8)      

#merge all data frames together
compare <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
boxplot(diffs ~ proc.type, data = compare,main="20220218 Differences (meters) by Processing Type",
              xlab = 'Processing Model',
              ylab = 'Difference (meters)')
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)

abline(h=0, col = 'red')


