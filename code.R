# preparation of data
Data = read.csv("regular_season_detailed_results.csv",header = T)
Data = Data[-which(Data$wteam == 1464),]
Data = Data[-which(Data$lteam == 1464),]
head(Data)
index.choose = c()
team = sort(unique(Data$wteam))
i = 1
while(length(team) != 0 | i == nrow(Data))
{
  if(Data$wteam[i] %in% team && Data$lteam[i] %in% team)
    {
      index.choose = c(index.choose,i)
      team = team[-which(team == Data$wteam[i])[1]]
      team = team[-which(team == Data$lteam[i])[1]]
      i = 1
    }
  else i = i + 1;
}


Data = Data[index.choose,]

head(Data)
dim(Data)
Index1 = Data$wteam > Data$lteam
Index2 = Data$wteam < Data$lteam
Data = Data[,-c(1,2,4,6,7,8)]
Data1 = Data[Data$wteam > Data$lteam,c(2,1,16:28,3:15)]
Data2 = Data[Data$wteam < Data$lteam,]
head(Data1)
head(Data2)
Data1$wol = 0
Data2$wol = 1
Data$wol = 2
Data[Index1,] = Data1
Data[Index2,] = Data2
names(Data) = c("Ateam","Bteam","Afgm","Afga","Afgm3","Afga3",
                "Aftm","Afta","Aor","Adr","Aast","Ato","Astl",
                "Ablk","Apf","Bfgm","Bfga","Bfgm3","Bfga3","Bftm",
                "Bfta","Bor","Bdr","Bast","Bto","Bstl","Bblk",
                "Bpf","wol")
head(Data,100)
test = Data[,-c(1,2)]
head(test)
ChoseData = test[,-c(1,3,5,14,16,18)]
head(ChoseData)
dim(ChoseData)
rownames(ChoseData) = c(1:175)
set.seed(999)
n = sample(x = 1:175,size = 175,replace = F)
ChoseData = ChoseData[n,]
########################################################################
# models

#GLM full model
TRY = glm(wol~.,data = ChoseData,family = "binomial")

# residual diagnostic
goodness_of_fit(TRY)
#leverage points and cook's distance
leverage_cook(TRY,ChoseData)
# within group prediction
within_pred(TRY,ChoseData)
#cross validation check
cv.check(data = ChoseData,methods = "GLM")
#Deviance Table
anova(TRY,test = "Chi")

#######################################################################

#GLM reduced model(variables selection). 

##Using cv.glmnet to Choose 
library(glmnet)
par(mfrow = c(1,1))
cvob = cv.glmnet(x = as.matrix(ChoseData[,-21]),y = ChoseData$wol,
                 intercept = TRUE,family = "binomial")
plot(cvob)
small.lambda.index = which(cvob$lambda == cvob$lambda.min)
small.lambda.betas = cvob$glmnet.fit$beta[,small.lambda.index]
glmnet.fit = glmnet(x = as.matrix(ChoseData[,-21]), y = ChoseData$wol,intercept = TRUE, 
                    lambda = cvob$lambda[small.lambda.index],family='binomial')
semifinalData = ChoseData[,c("Afta","Aor","Adr","Aast","Ato","Ablk",
                             "Apf","Bfga","Bor","Bdr","Bast","Bto",
                             "Bstl","Bpf","wol")]
# Reduced model
refit = glm(wol ~ ., data = semifinalData, family = "binomial")
# residual diagnostic
goodness_of_fit(refit)
#leverage points and cook's distance
leverage_cook(refit,semifinalData)
# within group prediction
within_pred(refit,semifinalData)
#cross validation check
cv.check(data = semifinalData,methods = "GLM")
#Deviance Table
anova(refit,test = "Chi")


## Using AIC to choose?



#########################################################################
#GAM

library(gam)
library(ggplot2)
Gam.fit = gam::gam(wol~ s(Afga) + s(Afga3) + s(Afta) + s(Aor) +
                     s(Adr) + s(Aast) + s(Ato) + s(Astl) +s(Ablk) +
                     s(Apf) + s(Bfga) + s(Bfga3) + s(Bfta) + s(Bor) + 
                     s(Bdr) + s(Bast) + s(Bto) + s(Bstl) + s(Bblk) + s(Bpf),
                   data = ChoseData, family= binomial("logit"))

summary(Gam.fit)
par(mfrow=c(4,5),mar = c(4,1,1,1))
ggplot.model(Gam.fit)



#GAPLM
Gaplm.fit = gam::gam(wol~ Afga + s(Afga3) + Afta + Aor +
                     Adr + Aast + s(Ato) + s(Astl) +s(Ablk) +
                     Apf + Bfga + s(Bfga3) + s(Bfta) + Bor + 
                     Bdr + s(Bast) + Bto + Bstl + s(Bblk) + s(Bpf),
                     data = ChoseData, family= binomial("logit"))

summary(Gaplm.fit)
ggplot.model(Gaplm.fit)
#goodness of fit
goodness_of_fit(model = Gaplm.fit)
#leverage and cook's distance
leverage_cook(model = Gaplm.fit,data = ChoseData)
#cross validation
cv.check(data = ChoseData,methods = "GAPLM")
#within group prediction
within_pred(model = Gaplm.fit,data = ChoseData)

#########################################


#####################################################################
#Function used in the model.
within_pred = function(model,data)
{
  p = ncol(data)
  pred = round(predict(model,data[,-p],type = "response"))
  confusion_matrix = table(data[,p],pred)
  return(confusion_matrix)
}


leverage_cook = function(model,data,t = 0.1)
{
  par(mfrow = c(1,2))
  leverages = hatvalues(model)
  n = nrow(data)
  p = ncol(data)
  plot(names(leverages),leverages,xlab = "Index", type = "h", main
       ="Leverage Points")
  abline(h = 2*p/n,col="red")
  points(names(leverages[leverages > 2*p/n]),leverages[leverages>2*p/n],
         col = 70 * leverages[leverages > 2*p/n],cex = 0.6)  
  cookdist = cooks.distance(model)
  thres = p/(n-p-1)
  plot(which(cookdist <= thres),cookdist[cookdist <= thres],cex = 0.6,
       main = "Cook's Distance",ylim = c(0,max(cookdist)+max(cookdist)*t),
       xlab = "Index", ylab = "Cook's distance")
  points(which(cookdist > thres),cookdist[cookdist > thres],
         col = 70 * cookdist[cookdist > thres],cex = 0.6)
  abline(h = thres,col = "red")
}


cv.check = function(data = ChoseData, fold = 10, methods = c("GLM","GAM","GAPLM"))
{
  require(MASS)
  require(gam)
  #rearrage data
  n = nrow(data)
  p = ncol(data)
  index = sample(n,n,replace = F)
  data = data[index,]
  groups = rep(1:fold, length.out = n)
  fold.index = split(1:n, groups)
  if(methods == "GLM")
  {
    tables = sapply(1:fold, function(i){
      traindata = data[-fold.index[[i]],]
      testdata = data[fold.index[[i]],]
      fit= glm(wol~. , data = traindata, family = binomial)
      pred = round(predict(fit,testdata[,-p],type = "response"))
      table(testdata[,p],pred)
    })
  }
  if(methods == "GAPLM")
  {
    tables = sapply(1:fold, function(i){
      traindata = data[-fold.index[[i]],]
      testdata = data[fold.index[[i]],]
      fit= gam::gam(wol~ Afga + s(Afga3) + Afta + Aor +
                      Adr + Aast + s(Ato) + s(Astl) +s(Ablk) +
                      Apf + Bfga + s(Bfga3) + s(Bfta) + Bor + 
                      Bdr + s(Bast) + Bto + Bstl + s(Bblk) + s(Bpf),
                    data = traindata, family= binomial("logit"))
      pred = round(predict(fit,testdata[,-p],type = "response"))
      table(testdata[,p],pred)
    })
  }
  if(methods == "GALPM")
  {
    tables = sapply(1:fold, function(i){
      traindata = data[-fold.index[[i]],]
      testdata = data[fold.index[[i]],]
      fit= gam::gam(wol~ s(Afga) + s(Afga3) + s(Afta) + s(Aor) +
                      s(Adr) + s(Aast) + s(Ato) + s(Astl) +s(Ablk) +
                      s(Apf) + s(Bfga) + s(Bfga3) + s(Bfta) + s(Bor) + 
                      s(Bdr) + s(Bast) + s(Bto) + s(Bstl) + s(Bblk) + s(Bpf),
                    data = traindata, family= binomial("logit"))
      pred = round(predict(fit,testdata[,-p],type = "response"))
      table(testdata[,p],pred)
    })
  }
  return(matrix(rowSums(tables), nrow = 2, byrow = F))
}


goodness_of_fit = function(model)
{
  require(lawstat)
  resid.P = resid(model,type = "pearson")
  resid.D = resid(model,type = "deviance")
  fitted.values = model$fitted.values
  par(mfrow = c(2,2),mar = c(4,3,4,3))
  rt = runs.test(resid.P,plot.it = TRUE)
  rp = rt$p.value
  boxplot(resid.P,resid.D,names = c("Pearson","Deviance"),
          main = "Boxplot of residual")
  plot(fitted.values,resid.P,col = "blue",
       xlab = "fitted values",ylab = "residuals",
       main = "Pearson residuals against the fitted values")
  lines(smooth.spline(fitted.values,resid.P,spar = 2.29), col = "red")
  
  plot(fitted.values,resid.D,col = "blue",
       xlab = "fitted values",ylab = "residuals",
       main = "Deviance residuals against the fitted values")
  lines(smooth.spline(fitted.values,resid.P,spar = 2.29), col = "red")
  return(rp)
}


library(ggplot2)
ggplot.model <- function(model, res=FALSE, 
                         col.line="#7fc97f", col.point="#beaed4", size.line=1, size.point=1)
{
  require(visreg)
  require(plyr)
  png(paste0(tempdir(),"/temporary_R_file.png"))
  plot <- visreg(model)
  dev.off()
  smooths <- ldply(plots, function(part)   
    data.frame(x=part$fit[,part$meta$x], smooth=part$fit$visregFit, 
               name = part$meta$x))
  points <-  ldply(plot, function(part) 
    data.frame(data.x=part$res[,part$meta$x], data.y=part$res$visregRes,
               name = part$meta$x)) 
  if (res)
    ggplot(smooths, aes(x, smooth)) + geom_line(col=col.line, size=size.line) +
    geom_point(data = points, aes(data.x, data.y), col=col.point, size=size.point) +
    facet_wrap( ~ name, scales = "free")
  else
    ggplot(smooths, aes(x, smooth)) + geom_line(col=col.line, size=size.line) +
    facet_wrap( ~ name, scales = "free")
}


