proc.type <- c(rep('1_NoPPK_NoAlign',5),rep('2_PPK_HeightAdj_NoAlign',5),rep('3_PPK_NoHeightAdj_NoAlign',5),
  rep('6_PPK_NoHeightAdj_4DAlign',5),rep('4_PPK_HeightAdj_4DAlign',5),rep('5_PPK_HeightAdj_4DAlign_Paired',5),
  rep('7_PPK_NoHeightAdj_4DAlign_Paired',5),rep('8_NoPPK_4DAlign',5),rep('9_NoPPK_4DAlign_Paired',5))

proc.type <- as.factor(proc.type)

diffs <- c(-0.341162,-0.34582,-0.471113,-0.731689,-0.350059,
           -0.255957,-0.275508,-0.410078,-0.659668,-0.290488,
           0.061914,0.054814,-0.076826,-0.321777,0.053262,
           0.006006,-0.011348,-0.220869,-0.374268,0.002969,
           -0.024756,-0.032588,-0.400313,-0.501953,-0.115684,
           -0.264746,-0.230586,-0.552168,-0.442871,-0.322715,
           0.066797,0.086553,-0.208174,-0.169678,-0.008506,
           -3.487158,-3.49499,-3.885664,-4.164307,-3.752891,
           -1.962012,-1.820186,-1.887861,-1.983887,-1.786338)

mnvec <- tapply(diffs,proc.type,mean)
nvec <- tapply(diffs,proc.type,length)
# Bind the data to assign the batch number to each amount value
aovdata <- data.frame(cbind(diffs,proc.type))
attach(aovdata)

# Make side-by-side boxplots

# Make side-by-side boxplots
bp <- boxplot(diffs ~ proc.type, data = aovdata, xaxt = "n",main="20220212 Differences (meters) by Processing Type",
              xlab = 'Processing Model',
              ylab = 'Difference (meters)')
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)
text(tick, par("usr")[3] - 0.3, bp$names, srt = 45, xpd = TRUE)
abline(h = 0, col = "red")
quartz()
boxplot(diffs~proc.type,main="Differences by Processing Type")

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

# Compare the models that differed significantly from the baseline

# Model 1 is the baseline
model.1 <- aovdata[aovdata$proc.type == '1',]
model.2 <- aovdata[aovdata$proc.type == '2',]
model.8 <- aovdata[aovdata$proc.type == '8',]
model.9 <- aovdata[aovdata$proc.type == '9',]
# the rest are the models we wish to compare
model.3 <- aovdata[aovdata$proc.type == '3',]
model.4 <- aovdata[aovdata$proc.type == '4',]
model.5 <- aovdata[aovdata$proc.type == '5',]
model.6 <- aovdata[aovdata$proc.type == '6',]
model.7 <- aovdata[aovdata$proc.type == '7',]

t.test(model.1$diffs,mu=0)
t.test(model.3$diffs,mu=0)
t.test(model.4$diffs,mu=0)
t.test(model.5$diffs,mu=0)
t.test(model.6$diffs,mu=0)
t.test(model.7$diffs,mu=0)
t.test(model.8$diffs,mu=0)
t.test(model.9$diffs,mu=0)

# Now let's see if models 3, 6, and 7 differ from each other.
t.test(model.3,model.4,var.equal=T)
t.test(model.3,model.6,var.equal=T)
t.test(model.3,model.7,var.equal=T)
t.test(model.4,model.6,var.equal=T)
t.test(model.4,model.7,var.equal=T)
t.test(model.6,model.7,var.equal=T)

mean(model.1$diffs)
mean(model.3$diffs)
mean(model.6$diffs)
mean(model.7$diffs)

sd(model.1$diffs)
sd(model.2$diffs)
sd(model.3$diffs)
sd(model.4$diffs)
sd(model.5$diffs)
sd(model.6$diffs)
sd(model.7$diffs)
sd(model.8$diffs)
sd(model.9$diffs)

df_list <- list(model.1, model.3, model.4, model.6, model.7)      

#merge all data frames together
compare <- Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
boxplot(diffs ~ proc.type, data = compare,main="20220212 Differences (meters) by Processing Type",
        xlab = 'Processing Model',
        ylab = 'Difference (meters)')
tick <- seq_along(bp$names)
axis(1, at = tick, labels = FALSE)

abline(h=0, col = 'red')
  