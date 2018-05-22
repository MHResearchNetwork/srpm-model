
library(glmnet)
library(doParallel)
library(foreach)
library(sqldf)
setwd("//groups/data/CTRHS/MHRN/Eric")

pc.analytic <- read.csv("analytic_pc90.csv")
pc.validation <- read.csv("valid_pc90.csv")














roc.function <- function(preds.var, outcome.var, wts.var) {
obs_data <- na.exclude(data.frame(preds=preds.var, outcome=outcome.var, wts=wts.var))
obs_data$preds <- signif(obs_data$preds, 3)



obs_col <- sqldf("select preds, outcome, sum(wts) as wts from obs_data group by preds, outcome order by preds, outcome")
vals <- matrix(0, nrow=dim(obs_col)[1]+2, ncol=10)
pred.b <- (obs_col$preds >= 0)
tp <- sum(obs_col$wts[obs_col$outcome==1 & pred.b==1])
fp <- sum(obs_col$wts[obs_col$outcome==0 & pred.b==1])
tn <- sum(obs_col$wts[obs_col$outcome==0 & pred.b==0])
fn <- sum(obs_col$wts[obs_col$outcome==1 & pred.b==0])
wts.1 <- rep(1, length(obs_col$wts))
tp.u <- sum(wts.1[obs_col$outcome==1 & pred.b==1])
fp.u <- sum(wts.1[obs_col$outcome==0 & pred.b==1])
tn.u <- sum(wts.1[obs_col$outcome==0 & pred.b==0])
fn.u <- sum(wts.1[obs_col$outcome==1 & pred.b==0])
sens <- tp / (tp+fn)
spec <- tn / (tn+fp)
speci <- 1 - spec
ppv <- tp / (tp+fp)
npv <- tn / (tn+fn)
vals[1,] <- c(sens, spec, speci, ppv, npv, tp.u, fp.u, fn.u, tn.u, 0)



for(j in 1:dim(obs_col)[1]) { #set more and more predictions = 0
if(obs_col$outcome[j]==1) { #observation now incorrect
fn <- fn + obs_col$wts[j]
tp <- tp - obs_col$wts[j]
fn.u <- fn.u + 1
tp.u <- tp.u - 1
} else { #observation remains correct
tn <- tn + obs_col$wts[j]
fp <- fp - obs_col$wts[j]
tn.u <- tn.u + 1
fp.u <- fp.u - 1
}
sens <- tp / (tp+fn)
spec <- tn / (tn+fp)
speci <- 1 - spec
ppv <- tp / (tp+fp)
npv <- tn / (tn+fn)
vals[j+1,] <- c(sens, spec, speci, ppv, npv, tp.u, fp.u, fn.u, tn.u, obs_col$preds[j])
} #end j loop

pred.b <- (obs_col$preds > 1)
tp <- sum(obs_col$wts[obs_col$outcome==1 & pred.b==1])
fp <- sum(obs_col$wts[obs_col$outcome==0 & pred.b==1])
tn <- sum(obs_col$wts[obs_col$outcome==0 & pred.b==0])
fn <- sum(obs_col$wts[obs_col$outcome==1 & pred.b==0])
wts.1 <- rep(1, length(obs_col$wts))
tp.u <- sum(wts.1[obs_col$outcome==1 & pred.b==1])
fp.u <- sum(wts.1[obs_col$outcome==0 & pred.b==1])
tn.u <- sum(wts.1[obs_col$outcome==0 & pred.b==0])
fn.u <- sum(wts.1[obs_col$outcome==1 & pred.b==0])
sens <- tp / (tp+fn)
spec <- tn / (tn+fp)
speci <- 1 - spec
ppv <- tp / (tp+fp)
npv <- tn / (tn+fn)
vals[dim(obs_col)[1]+2,] <- c(sens, spec, speci, ppv, npv, tp.u, fp.u, fn.u, tn.u, 1)
colnames(vals) <- c("Sensitivity", "Specificity", "1-Spec", "PPV", "NPV", "TP", "FP", "FN", "TN", "Cutoff")
vals <- as.data.frame(vals)
vals2 <- sqldf("select distinct * from vals group by cutoff having tn = max(tn) order by cutoff")


ma <- function(x,n=2){filter(x,rep(1/n,n), sides=1)}
auc1 <- na.omit(cbind(ma(vals2[,1]), abs(vals2[,3] - ma(vals2[,3]))*2))
auc <- signif(auc1[,1] %*% auc1[,2], 3)


#bootstrap CI time
obs_pos <- subset(obs_data, outcome==1)
obs_neg <- subset(obs_data, outcome==0)



cl <- makeCluster(detectCores())
registerDoParallel(cl)
nboot <- 1:10000
auc.b <- foreach(i=nboot, .verbose=TRUE, .combine=rbind) %dopar% {
library(sqldf)
obs_pos_r <- sample(1:nrow(obs_pos), replace = TRUE)
obs_pos_b <- obs_pos[obs_pos_r,]
obs_neg_r <- sample(1:nrow(obs_neg), replace = TRUE)
obs_neg_b <- obs_neg[obs_neg_r,]
obs_boot <- rbind(obs_pos_b, obs_neg_b)
obs_col_b <- sqldf("select preds, outcome, sum(wts) as wts from obs_boot group by preds, outcome order by preds, outcome")

vals <- matrix(0, nrow=dim(obs_col_b)[1]+2, ncol=10)
pred.b <- (obs_col_b$preds >= 0)
tp <- sum(obs_col_b$wts[obs_col_b$outcome==1 & pred.b==1])
fp <- sum(obs_col_b$wts[obs_col_b$outcome==0 & pred.b==1])
tn <- sum(obs_col_b$wts[obs_col_b$outcome==0 & pred.b==0])
fn <- sum(obs_col_b$wts[obs_col_b$outcome==1 & pred.b==0])
wts.1 <- rep(1, length(obs_col_b$wts))
tp.u <- sum(wts.1[obs_col_b$outcome==1 & pred.b==1])
fp.u <- sum(wts.1[obs_col_b$outcome==0 & pred.b==1])
tn.u <- sum(wts.1[obs_col_b$outcome==0 & pred.b==0])
fn.u <- sum(wts.1[obs_col_b$outcome==1 & pred.b==0])
sens <- tp / (tp+fn)
spec <- tn / (tn+fp)
speci <- 1 - spec
ppv <- tp / (tp+fp)
npv <- tn / (tn+fn)
vals[1,] <- c(sens, spec, speci, ppv, npv, tp.u, fp.u, fn.u, tn.u, 0)



for(j in 1:dim(obs_col_b)[1]) { #set more and more predictions = 0
if(obs_col_b$outcome[j]==1) { #observation now incorrect
fn <- fn + obs_col_b$wts[j]
tp <- tp - obs_col_b$wts[j]
fn.u <- fn.u + 1
tp.u <- tp.u - 1
} else { #observation remains correct
tn <- tn + obs_col_b$wts[j]
fp <- fp - obs_col_b$wts[j]
tn.u <- tn.u + 1
fp.u <- fp.u - 1
}
sens <- tp / (tp+fn)
spec <- tn / (tn+fp)
speci <- 1 - spec
ppv <- tp / (tp+fp)
npv <- tn / (tn+fn)
vals[j+1,] <- c(sens, spec, speci, ppv, npv, tp.u, fp.u, fn.u, tn.u, obs_col_b$preds[j])
} #end j loop

pred.b <- (obs_col_b$preds > 1)
tp <- sum(obs_col_b$wts[obs_col_b$outcome==1 & pred.b==1])
fp <- sum(obs_col_b$wts[obs_col_b$outcome==0 & pred.b==1])
tn <- sum(obs_col_b$wts[obs_col_b$outcome==0 & pred.b==0])
fn <- sum(obs_col_b$wts[obs_col_b$outcome==1 & pred.b==0])
wts.1 <- rep(1, length(obs_col_b$wts))
tp.u <- sum(wts.1[obs_col_b$outcome==1 & pred.b==1])
fp.u <- sum(wts.1[obs_col_b$outcome==0 & pred.b==1])
tn.u <- sum(wts.1[obs_col_b$outcome==0 & pred.b==0])
fn.u <- sum(wts.1[obs_col_b$outcome==1 & pred.b==0])
sens <- tp / (tp+fn)
spec <- tn / (tn+fp)
speci <- 1 - spec
ppv <- tp / (tp+fp)
npv <- tn / (tn+fn)
vals[dim(obs_col_b)[1]+2,] <- c(sens, spec, speci, ppv, npv, tp.u, fp.u, fn.u, tn.u, 1)
colnames(vals) <- c("Sensitivity", "Specificity", "1-Spec", "PPV", "NPV", "TP", "FP", "FN", "TN", "Cutoff")
vals <- as.data.frame(vals)
vals2 <- sqldf("select distinct * from vals group by cutoff having tn = max(tn) order by cutoff")
auc1 <- na.omit(cbind(ma(vals2[,1]), abs(vals2[,3] - ma(vals2[,3]))*2))
auc.boot <- signif(auc1[,1] %*% auc1[,2], 3)
auc.boot
} #end foreach
stopCluster(cl)

auc.ci.b <- quantile(auc.b, probs = c(0.025, 0.975))

auc.ci <- paste(auc, " (", auc.ci.b[1], ", ", auc.ci.b[2], ")", sep="")


results.out <- list(vals2, auc.ci)
return(results.out)
}






model.char <- function(training.data, validation.data, setting, outcome, model.num, dir.in, dir.out, extra="", reest.coef=NULL, wts.var.mc=NULL) {
setwd(dir.in)

results <- list()
if(outcome == "event30") outcome.f <- "sr_30"
if(outcome == "event90") outcome.f <- "sr_90"
if(outcome == "death30") outcome.f <- "sd_30"
if(outcome == "death90") outcome.f <- "sd_90"

results <- list()
load(paste(setting, "_", outcome.f, "_final_model_m", model.num, ".RData", sep=""))
load(paste(setting, "_", outcome.f, "_model_list_m", model.num, ".RData", sep=""))
lambdas.mh <- read.csv(paste(setting, "_", outcome.f, "_lambdas_m", model.num, ".csv", sep=""))




coef.lasso <- coef(final.model, s=subset(lambdas.mh, bic == min(lambdas.mh$bic))$lambda.vals)
coef.lasso2 <- as.matrix(cbind(coef.lasso, seq(length(coef.lasso))-1))
lasso.predictors <- subset(coef.lasso2, coef.lasso2[,1] != 0)[,2]
lasso.predictors <- subset(lasso.predictors, lasso.predictors != 0)
lasso.names <- names(lasso.predictors)
lasso.predictors <- lasso.predictors + 6

results[[1]] <- lasso.names


if(is.null(reest.coef)==0) {
log.coef <- reest.coef

setwd(dir.out)
xmat.training <- training.data[,lasso.predictors]
xmat.training <- as.matrix(cbind(rep(1, dim(xmat.training)[1]), xmat.training))
preds.training.xb <- xmat.training %*% log.coef
preds.training <- exp(preds.training.xb)/(1+exp(preds.training.xb))
results[[2]] <- preds.training
y.training <- training.data[,outcome]
results[[3]] <- y.training

xmat.validation <- validation.data[,lasso.predictors]
xmat.validation <- as.matrix(cbind(rep(1, dim(xmat.validation)[1]), xmat.validation))
preds.validation.xb <- xmat.validation %*% log.coef
preds.validation <- exp(preds.validation.xb)/(1+exp(preds.validation.xb))
results[[4]] <- preds.validation
y.validation <- validation.data[,outcome]
results[[5]] <- y.validation

cutpoints <- quantile(preds.training, probs=c(0, 0.5, 0.75, 0.90, 0.95, 0.99, 0.995, 1))

preds.training.pct <- rep("0-50", length(preds.training))
preds.training.pct[preds.training >= cutpoints[2]] <- "51-75"
preds.training.pct[preds.training >= cutpoints[3]] <- "76-90"
preds.training.pct[preds.training >= cutpoints[4]] <- "91-94"
preds.training.pct[preds.training >= cutpoints[5]] <- "95-98"
preds.training.pct[preds.training >= cutpoints[6]] <- "99.0-99.4"
preds.training.pct[preds.training >= cutpoints[7]] <- "99.5-100"

preds.validation.pct <- rep("0-50", length(preds.validation))
preds.validation.pct[preds.validation >= cutpoints[2]] <- "51-75"
preds.validation.pct[preds.validation >= cutpoints[3]] <- "76-90"
preds.validation.pct[preds.validation >= cutpoints[4]] <- "91-94"
preds.validation.pct[preds.validation >= cutpoints[5]] <- "95-98"
preds.validation.pct[preds.validation >= cutpoints[6]] <- "99.0-99.4"
preds.validation.pct[preds.validation >= cutpoints[7]] <- "99.5-100"

percentile.table <- data.frame(Training=tapply(y.training, preds.training.pct, mean, na.rm=T), Validation=tapply(y.validation, preds.validation.pct, mean, na.rm=T), ValidationP=tapply(y.validation, preds.validation.pct, sum, na.rm=T)/sum(y.validation))
results[[6]] <- percentile.table
write.csv(percentile.table, paste(setting, "_", outcome.f, "_", model.num, "_", extra, "_percentile_table.csv", sep=""))


cp.roc <- sort(c(0, 1, unique(signif(preds.training, 3))))

if(is.null(wts.var.mc)) wts.roc.t <- rep(1, length(preds.training))
if(is.null(wts.var.mc)==0) wts.roc.t <- training.data[,wts.var.mc]
if(is.null(wts.var.mc)) wts.roc.v <- rep(1, length(preds.validation))
if(is.null(wts.var.mc)==0) wts.roc.v <- validation.data[,wts.var.mc]

roc.training <- roc.function(preds.training, y.training, wts.roc.t)
results[[7]] <- roc.training[[1]]
roc.validation <- roc.function(preds.validation, y.validation, wts.roc.v)
results[[8]] <- roc.validation[[1]]

results[[9]] <- roc.training[[2]]
results[[10]] <- roc.validation[[2]]

write.csv(roc.training, paste(setting, "_", outcome.f, "_", model.num, "_", extra, "_training_cutpoints.csv", sep=""))
write.csv(roc.validation, paste(setting, "_", outcome.f, "_", model.num, "_", extra, "_validation_cutpoints.csv", sep=""))

} #end if block for supplied coefficients

return(results)
} #end function




dir.in.pc <-  "//groups/data/CTRHS/MHRN/Eric/srs2/pc_results"
dir.out.pc <- "//groups/data/CTRHS/MHRN/Eric/srs2/pc_results"


pc.sr.90.5.coefs <- c(-6.592665, -0.0177708, 0.2800567, -0.1544488, -0.169057, 0.7085663, -0.01017, 0.2015132, -0.0605431, 0.2860064, -0.1090385, -0.0003243, -0.2056298, -0.4051398, -0.0878258, -0.2380795, -0.1424411, -0.1410835, -0.1626887, -0.8148915, -0.0480855, 0.1362236, 0.0881055, 0.340881, 0.1163377, 0.0725226, 0.1072703, 0.1582301, 0.113767, 0.1660328, 0.2134675, 0.089926, 0.168969, -0.1065041, 0.2589882, 0.2320369, 0.0835463, 0.4740714, 0.4714488, 1.668482, 0.1475331, 0.1329157, 0.2464229, -0.0830589, 0.0223427, -0.0997792, 0.0018103, 0.1381357, -0.1207415, -0.1652086, -0.029999, -0.3205841, -0.0356545, 0.0055312, 0.0293979, -0.0217082, 0.1220448, -0.0625263, 0.2185043, 0.1852716, 0.4865901, -0.1041901, -0.1074305, 0.1497073, 0.1359814, 1.718887, -0.0119109, 0.3794353, 0.0013635, 0.1141705, 0.1409446, 0.005867, -0.0005691, 0.2136741, 0.0835573, 0.443662, 0.5796565, 1.295697, 0.1123629, 0.2631496, 0.1512429, -0.0022386, 0.0202408, 0.0063296, 0.0069394, 0.0004031, 0.0049376, 0.0141446, 0.0296125, 0.0542913, 0.0594748, -0.0316618, -0.218713, -0.5715746, -0.0159464, -0.0143564, -0.995897, -0.4112843, -0.2070626, 0.3432375, -0.2630062, -0.4859498, -0.1867706)
pc.sr.90.5 <- model.char(pc.analytic, pc.validation, "PC", "event90", 5, dir.in.pc, dir.out.pc, extra="", reest.coef=pc.sr.90.5.coefs)




pc.sd.90.5.coefs <- c(-9.160812, -0.4024572, -0.001533, -1.452239, 0.5385625, 0.2818673, 0.3160587, 0.6988312, 0.0190551, 0.2420071, 0.0240631, -0.0602928, -1.842608, 0.0022517, 0.8959889, 0.8135118, 0.497053, 0.0072638, 0.1735233, 0.0103087, 0.5644535, 0.0354654, 0.0782259, 0.0005279, 0.0166789, 0.0254827, 0.0455625, 0.0163483, -0.0314048, -0.0499144)
pc.sd.90.5 <- model.char(pc.analytic, pc.validation, "PC", "death90", 5, dir.in.pc, dir.out.pc, extra="", reest.coef=pc.sd.90.5.coefs)

