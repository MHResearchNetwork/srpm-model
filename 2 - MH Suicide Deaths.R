
setwd("C:/Users/Administrator/Desktop/mh")


sink("MH_sd_90_logfile.txt", split=TRUE)


library(glmnet)

set.seed(12)


srs2_analytic_subset <- read.csv("analytic_mh90.csv")
srs2_analytic_subset <- subset(srs2_analytic_subset, is.na(srs2_analytic_subset$death90)==0)



srs2_analytic_subset$age <- scale(srs2_analytic_subset$age)
srs2_analytic_subset$days_since_prev <- scale(srs2_analytic_subset$days_since_prev)
srs2_analytic_subset$charlson_score <- scale(srs2_analytic_subset$charlson_score)
srs2_analytic_subset$charlson_a <- scale(srs2_analytic_subset$charlson_a)
srs2_analytic_subset$dep_dx_pre5y_cumulative_a <- scale(srs2_analytic_subset$dep_dx_pre5y_cumulative_a)
srs2_analytic_subset$anx_dx_pre5y_cumulative_a <- scale(srs2_analytic_subset$anx_dx_pre5y_cumulative_a)
srs2_analytic_subset$bip_dx_pre5y_cumulative_a <- scale(srs2_analytic_subset$bip_dx_pre5y_cumulative_a)
srs2_analytic_subset$sch_dx_pre5y_cumulative_a <- scale(srs2_analytic_subset$sch_dx_pre5y_cumulative_a)
srs2_analytic_subset$phqNumber90 <- scale(srs2_analytic_subset$phqNumber90)
srs2_analytic_subset$phqNumber183 <- scale(srs2_analytic_subset$phqNumber183)
srs2_analytic_subset$phqNumber365 <- scale(srs2_analytic_subset$phqNumber365)
srs2_analytic_subset$phq8_index_score_calc <- scale(srs2_analytic_subset$phq8_index_score_calc)
srs2_analytic_subset$phq8_index_score_calc_f <- scale(srs2_analytic_subset$phq8_index_score_calc_f)
srs2_analytic_subset$raceAsian_8 <- scale(srs2_analytic_subset$raceAsian_8)
srs2_analytic_subset$raceBlack_8 <- scale(srs2_analytic_subset$raceBlack_8)
srs2_analytic_subset$raceHP_8 <- scale(srs2_analytic_subset$raceHP_8)
srs2_analytic_subset$raceIN_8 <- scale(srs2_analytic_subset$raceIN_8)
srs2_analytic_subset$raceMUOT_8 <- scale(srs2_analytic_subset$raceMUOT_8)
srs2_analytic_subset$raceUN_8 <- scale(srs2_analytic_subset$raceUN_8)
srs2_analytic_subset$hispanic_8 <- scale(srs2_analytic_subset$hispanic_8)
srs2_analytic_subset$age_8 <- scale(srs2_analytic_subset$age_8)
srs2_analytic_subset$q9_0_a <- scale(srs2_analytic_subset$q9_0_a)
srs2_analytic_subset$q9_1_a <- scale(srs2_analytic_subset$q9_1_a)
srs2_analytic_subset$q9_2_a <- scale(srs2_analytic_subset$q9_2_a)
srs2_analytic_subset$q9_3_a <- scale(srs2_analytic_subset$q9_3_a)
srs2_analytic_subset$q9_0_8 <- scale(srs2_analytic_subset$q9_0_8)
srs2_analytic_subset$q9_1_8 <- scale(srs2_analytic_subset$q9_1_8)
srs2_analytic_subset$q9_2_8 <- scale(srs2_analytic_subset$q9_2_8)
srs2_analytic_subset$q9_3_8 <- scale(srs2_analytic_subset$q9_3_8)
srs2_analytic_subset$q9_0_c <- scale(srs2_analytic_subset$q9_0_c)
srs2_analytic_subset$q9_1_c <- scale(srs2_analytic_subset$q9_1_c)
srs2_analytic_subset$q9_2_c <- scale(srs2_analytic_subset$q9_2_c)
srs2_analytic_subset$q9_3_c <- scale(srs2_analytic_subset$q9_3_c)
srs2_analytic_subset$any_sui_att_pre5y_cumulative_a <- scale(srs2_analytic_subset$any_sui_att_pre5y_cumulative_a)
srs2_analytic_subset$any_sui_att_pre5y_cumulative_8 <- scale(srs2_analytic_subset$any_sui_att_pre5y_cumulative_8)
srs2_analytic_subset$any_sui_att_pre5y_cumulative_c <- scale(srs2_analytic_subset$any_sui_att_pre5y_cumulative_c)


y <- srs2_analytic_subset$death90
wts <- rep(1, length(y))
cvvar <- srs2_analytic_subset$cvvar
id.vars <- cbind(srs2_analytic_subset$person_id, srs2_analytic_subset$visit_seq, srs2_analytic_subset$cvvar, y)









xnames <- c(names(srs2_analytic_subset[,7:319]))
xmat.scaled <- as.matrix(srs2_analytic_subset[,7:319])
colnames(xmat.scaled) <- xnames

lambda.vals <- sort(unique(
c(
seq(0.01, 		0.1, by=0.005),
seq(0.001, 		0.01, by=0.0005),
seq(0.0001, 	0.001, by=0.00005),
seq(0.00001, 	0.0001, by=0.000005),
seq(0.000001, 	0.00001, by=0.0000005),
seq(0.0000001, 	0.000001, by=0.00000005))
)
, decreasing=TRUE)
cv.values <- sort(unique(cvvar))

time.start.cv <- proc.time()[3]
loglik.total <- NULL
model.list <- list()
phat.out <- NULL
coef.count <- NULL

for(j in cv.values) {
training.model <- glmnet(subset(xmat.scaled, cvvar != j), subset(y, cvvar != j), lambda=lambda.vals, standardize=FALSE, weights=subset(wts, cvvar != j), family='binomial', alpha=1)
p.hat <- predict(training.model, subset(xmat.scaled, cvvar == j), type="response")
y.valid.reverse <- 1 - subset(y, cvvar == j)
likelihood.matrix <- abs(y.valid.reverse - p.hat)  #p.hat for y=1, 1-p.hat for y=0.  ignores N-choose-Y portion, but that should be OK
loglik.matrix <- log(likelihood.matrix)
loglik.sum <- apply(loglik.matrix, 2, sum)
if(is.null(loglik.total)) {
loglik.total <- loglik.sum 
} else {
loglik.total <- loglik.total + loglik.sum
}
coef.kept <- coef(training.model)!=0
coef.count <- cbind(coef.count, apply(coef.kept, 2, sum))
model.list[[j+1]] <- training.model
phat.out <- rbind(phat.out, cbind(subset(id.vars, cvvar == j), p.hat))
print(j)
print("")
flush.console()
} #end j loop
time.end.cv <- proc.time()[3]
(time.end.cv - time.start.cv)/(60 * 60)
Nbeta.count.avg <- apply(coef.count, 1, mean)
ll2 <- - 2 * loglik.total
aic <- 2 * apply(coef.count, 1, mean) - 2*loglik.total
bic <- log(dim(xmat.scaled)[1]) * apply(coef.count, 1, mean) - 2*loglik.total

lambda.results <- data.frame(lambda.vals, Nbeta.count.avg, ll2, aic, bic)
lambda.results

subset(lambda.results, ll2 == min(lambda.results$ll2))
subset(lambda.results, aic == min(lambda.results$aic))
subset(lambda.results, bic == min(lambda.results$bic))

final.model <- glmnet(xmat.scaled, y, lambda=lambda.vals, standardize=FALSE, weights=wts, family='binomial', alpha=1)
final.model.p <- predict(final.model, xmat.scaled, type="response")

write.csv(lambda.results, "MH_sd_90_Lambdas_m5.csv")
save(model.list, file="MH_sd_90_model_list_m5.RData")
save(final.model, file="MH_sd_90_final_model_m5.RData")









sink()

