
library(glmnet)
library(doParallel)
library(foreach)
library(sqldf)
setwd("//groups/data/CTRHS/MHRN/Eric")

mh.analytic <- read.csv("analytic_mh90.csv")
mh.validation <- read.csv("valid_mh90.csv")




roc.function <- function(preds.var, outcome.var, wts.var) {
obs_data <- na.exclude(data.frame(preds=preds.var, outcome=outcome.var, wts=wts.var))
names(obs_data) <- c("preds", "outcome", "wts")
obs_data$preds <- signif(obs_data$preds, 3)



obs_col <- sqldf("select preds, outcome, count(*) as wts_u, sum(wts) as wts from obs_data group by preds, outcome order by preds, outcome")
vals <- matrix(0, nrow=dim(obs_col)[1]+2, ncol=10)
pred.b <- rep(1, length(obs_col$preds))
tp <- sum(obs_col$wts[obs_col$outcome==1 & pred.b==1])
fp <- sum(obs_col$wts[obs_col$outcome==0 & pred.b==1])
tn <- sum(obs_col$wts[obs_col$outcome==0 & pred.b==0])
fn <- sum(obs_col$wts[obs_col$outcome==1 & pred.b==0])
tp.u <- sum(obs_col$wts_u[obs_col$outcome==1 & pred.b==1])
fp.u <- sum(obs_col$wts_u[obs_col$outcome==0 & pred.b==1])
tn.u <- sum(obs_col$wts_u[obs_col$outcome==0 & pred.b==0])
fn.u <- sum(obs_col$wts_u[obs_col$outcome==1 & pred.b==0])
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
fn.u <- fn.u + obs_col$wts_u[j]
tp.u <- tp.u - obs_col$wts_u[j]
} else { #observation remains correct
tn <- tn + obs_col$wts[j]
fp <- fp - obs_col$wts[j]
tn.u <- tn.u + obs_col$wts_u[j]
fp.u <- fp.u - obs_col$wts_u[j]
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
tp.u <- sum(obs_col$wts_u[obs_col$outcome==1 & pred.b==1])
fp.u <- sum(obs_col$wts_u[obs_col$outcome==0 & pred.b==1])
tn.u <- sum(obs_col$wts_u[obs_col$outcome==0 & pred.b==0])
fn.u <- sum(obs_col$wts_u[obs_col$outcome==1 & pred.b==0])
sens <- tp / (tp+fn)
spec <- tn / (tn+fp)
speci <- 1 - spec
ppv <- tp / (tp+fp)
npv <- tn / (tn+fn)
vals[dim(obs_col)[1]+2,] <- c(sens, spec, speci, ppv, npv, tp.u, fp.u, fn.u, tn.u, 1)
colnames(vals) <- c("Sensitivity", "Specificity", "1-Spec", "PPV", "NPV", "TP", "FP", "FN", "TN", "Cutoff")
vals <- as.data.frame(vals)
vals2 <- sqldf("select distinct * from vals group by cutoff having fn = max(fn) order by cutoff")


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
obs_col_b <- sqldf("select preds, outcome, count(*) as wts_u, sum(wts) as wts from obs_boot group by preds, outcome order by preds, outcome")

vals <- matrix(0, nrow=dim(obs_col_b)[1]+2, ncol=10)
pred.b <- rep(1, length(obs_col_b$preds >= 0))
tp <- sum(obs_col_b$wts[obs_col_b$outcome==1 & pred.b==1])
fp <- sum(obs_col_b$wts[obs_col_b$outcome==0 & pred.b==1])
tn <- sum(obs_col_b$wts[obs_col_b$outcome==0 & pred.b==0])
fn <- sum(obs_col_b$wts[obs_col_b$outcome==1 & pred.b==0])
tp.u <- sum(obs_col_b$wts_u[obs_col_b$outcome==1 & pred.b==1])
fp.u <- sum(obs_col_b$wts_u[obs_col_b$outcome==0 & pred.b==1])
tn.u <- sum(obs_col_b$wts_u[obs_col_b$outcome==0 & pred.b==0])
fn.u <- sum(obs_col_b$wts_u[obs_col_b$outcome==1 & pred.b==0])
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
fn.u <- fn.u + obs_col_b$wts_u[j]
tp.u <- tp.u - obs_col_b$wts_u[j]
} else { #observation remains correct
tn <- tn + obs_col_b$wts[j]
fp <- fp - obs_col_b$wts[j]
tn.u <- tn.u + obs_col_b$wts_u[j]
fp.u <- fp.u - obs_col_b$wts_u[j]
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
tp.u <- sum(obs_col_b$wts_u[obs_col_b$outcome==1 & pred.b==1])
fp.u <- sum(obs_col_b$wts_u[obs_col_b$outcome==0 & pred.b==1])
tn.u <- sum(obs_col_b$wts_u[obs_col_b$outcome==0 & pred.b==0])
fn.u <- sum(obs_col_b$wts_u[obs_col_b$outcome==1 & pred.b==0])
sens <- tp / (tp+fn)
spec <- tn / (tn+fp)
speci <- 1 - spec
ppv <- tp / (tp+fp)
npv <- tn / (tn+fn)
vals[dim(obs_col_b)[1]+2,] <- c(sens, spec, speci, ppv, npv, tp.u, fp.u, fn.u, tn.u, 1)
colnames(vals) <- c("Sensitivity", "Specificity", "1-Spec", "PPV", "NPV", "TP", "FP", "FN", "TN", "Cutoff")
vals <- as.data.frame(vals)
vals2 <- sqldf("select distinct * from vals group by cutoff having fn = max(fn) order by cutoff")
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




dir.in.mh <-  "//groups/data/CTRHS/MHRN/Eric/srs2/mh_results"
dir.out.mh <- "//groups/data/CTRHS/MHRN/Eric/srs2/mh_results"

mh.sr.90.5.coefs <- c(-5.497382, -0.034902, 0.178448, -0.0358015, -0.0197698, 0.3466038, 0.5063412, -0.0128649, 0.0561249, 0.0171635, -0.0002604, -0.1706591, -0.198161, -0.0584846, 0.1840148, 0.1500438, 0.1732092, 0.3239251, 0.1700364, -0.1660521, 0.1012587, 0.0108364, 0.1058904, 0.0223592, 0.1591924, 0.1811564, 0.125625, 0.3349677, 0.0636899, -0.2080869, -0.1074946, 0.2510373, 0.1537127, 0.166392, 0.2960692, 0.2702175, 0.2485821, 0.1847974, 0.138099, 0.0439858, 0.0260373, -0.0447908, -0.149802, -0.0482917, -0.124487, -0.1624266, -0.0618083, -0.0002945, -0.0567475, 0.0355083, 0.1007399, 0.1138703, 0.0082783, 0.1329516, 0.0542167, -0.1682208, 0.0781856, 0.4336656, -0.145304, -0.0950878, 0.9937001, 0.0124627, -0.0104171, 0.079513, 0.0047955, 0.0065237, 0.0007926, 0.1572326, 0.1391769, -0.1002797, -0.3909997, 0.2835966, 0.3991401, 0.4591372, 0.0623086, 0.2768611, 0.0128594, -0.0062793, -0.0197291, -0.0280699, 0.0066678, 0.0086971, 0.0128, 0.0109181, 0.0202854, 0.0288333, -0.0682734, -0.1170131, -0.2917472, -0.2325519, -0.0160737, 0.0591954, 0.2117987, -0.146646, -0.3357564)
mh.sr.90.5 <- model.char(mh.analytic, mh.validation, "MH", "event90", 5, dir.in.mh, dir.out.mh, extra="", reest.coef=mh.sr.90.5.coefs)



mh.sd.90.5.coefs <- c(-8.946025, -1.545886, -0.5708209, -0.0005089, -0.7158009, -0.3935343, -0.5247922, 0.0776264, 0.6995959, 0.467845, 0.4699488, 0.2892493, 0.180343, 0.4432957, 0.4700143, 0.3527572, 1.36854, -0.0000514, -0.0005351, -1.465944, 0.6011625, -1.362098, 0.7672085, 0.6380826, 0.0111731, 0.0064072, 0.0042201, -0.0112257, 0.1592071, -0.5062722, 0.0241234, -0.0776751, -0.1020686, -0.1020686, -0.0564359, 0.000565, -0.0047322, 0.0125815, 0.0079878, 0.0035114, -0.0365011, 0.0551443, -0.1787875, -0.0343006, 0.0103636)
mh.sd.90.5 <- model.char(mh.analytic, mh.validation, "MH", "death90", 5, dir.in.mh, dir.out.mh, extra="", reest.coef=mh.sd.90.5.coefs)
