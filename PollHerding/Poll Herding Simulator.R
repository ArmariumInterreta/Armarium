# Set the number of weeks before election day to "start" from
weeks <- 10

# Set the number of simulations you want the model to run
sims <- 10^5

# Set if you want sampling bias to slightly change each week
dynamic_sampling_bias <- TRUE

# Sets the actual 2pp margin
true_val <- 1.53

# Sets the number of pollsters
pollsters <- 5

# The historical average error on individual polls
std_err <- 2

# Simulated sample size per poll
sample_size <- 1000

# Calculates sampling error
sampling_err <- round(sqrt(((50+true_val)*(100-(50+true_val)))/sample_size),2)

# Calculates the average size of sampling bias
sampling_bias <- round(sqrt((std_err^2 - sampling_err^2))/2,2)
#sampling_bias <- 0

# Sets the average change from the start of the campaign (user-defined) and the final vote
avg_time_change <- 2

# Parallel-processing libraries are used so the Monte Carlo process can be done reasonably quickly
require(foreach)
require(doParallel)

# Randomly generates starting 2pp margins
init_val <- round(rnorm(sims,mean=true_val,sd=avg_time_change),2)
#init_val <- round(rnorm(sims,mean=-3,sd=sampling_err),2)

# Randomly generates a set of time series model types
# 1: linear model; popular vote goes linearly from init_val to true_val over course of campaign
# 2: logistic model; popular vote doesn't change much in the initial and final weeks but rapidly changes in the middle
# 3: saturation model; popular vote rapidly changes at start of campaign then changes very little in final weeks
# 4: exponential model; popular vote changes little at start but rapidly changes to true_val at the end

time_model <- sample.int(4,sims,replace=TRUE)
#time_model <- rep(1,sims)

# Calculates the change in vote over the entire campaign for 
delta <- true_val-init_val

# Generates parameters for each simulation by model type
loc <- init_val
loc[time_model == 2] <- runif(sum(time_model == 2),4,6)
loc[time_model == 3] <- 0
loc[time_model == 4] <- 9

scale <- delta/(weeks - 1)
scale[time_model == 2] <- runif(sum(time_model == 2),2,4)
scale[time_model == 3] <- runif(sum(time_model == 3),4,6)
scale[time_model == 4] <- runif(sum(time_model == 4),1,2)

# Generates popular vote over time simulations
PopVote <- data.frame(matrix(init_val,nrow=sims,ncol=weeks))
PopVote[time_model==1,2:10] <- round(PopVote[time_model==1,2:10] + scale[time_model == 1] * t(matrix(1:9,ncol=sum(time_model == 1),nrow=(weeks-1))),2)
PopVote[time_model==2,] <- round(PopVote[time_model==2,1:10] + delta[time_model == 2] * pnorm(t(matrix(1:10,ncol=sum(time_model == 2),nrow=(weeks))),mean=loc[time_model == 2],sd=scale[time_model == 2]),2)
PopVote[time_model==3,] <- round(PopVote[time_model==3,] +  delta[time_model == 3] * (2 * pnorm(t(matrix(0:9,ncol=sum(time_model == 3),nrow=(weeks))),mean=loc[time_model == 3],sd=scale[time_model == 3])-1),2)
PopVote[time_model==4,] <- round(PopVote[time_model==4,] +  delta[time_model == 4] * (2 * pnorm(t(matrix(0:9,ncol=sum(time_model == 4),nrow=(weeks))),mean=loc[time_model == 4],sd=scale[time_model == 4])),2)

# Generates random sample biases for each simulation
init_sample_skew <- round(rnorm(sims,0,sampling_bias),2)
#init_sample_skew <- round(-3,2)
#init_sample_skew <- round((9:0)*(-1/3),2)
sample_skew <- matrix(init_sample_skew,nrow=sims,ncol=weeks)

if(dynamic_sampling_bias==TRUE){
  sample_skew <- round(sample_skew * matrix(runif(sims*10,0.8,1.2),nrow=sims,ncol=weeks),2)
}

SampleVote <- PopVote + sample_skew

# Creates and registers parallel processing cluster
cores <- min(detectCores(),pollsters)
cluster1 <- makeCluster(cores)
registerDoParallel(cluster1)

# Simulates non-herded polls
Polls_Ind <- foreach(i=1:pollsters,.inorder=FALSE) %dopar% {
  round((SampleVote+matrix(rnorm(n=(sims*weeks),0,sampling_err),nrow=nrow(SampleVote),ncol=ncol(SampleVote)))/0.5,0)*0.5
}

# Aggregates starting polls
Init_Polls <- data.frame(foreach(i=1:pollsters,.inorder=TRUE) %dopar% {
  Polls_Ind[[i]][,1]
})

# Aggregates 2nd week of poll samples for later herding
W2_Polls_Ind <- data.frame(foreach(i=1:pollsters,.inorder=TRUE) %dopar% {
  Polls_Ind[[i]][,2]
})

# Aggregates final non-herded polls
Final_Polls_Ind <- data.frame(foreach(i=1:pollsters,.inorder=TRUE) %dopar% {
  Polls_Ind[[i]][,weeks]
})

# Renames dataframes
colnames(Init_Polls) <- paste("P",1:pollsters,sep="")
colnames(Final_Polls_Ind) <- paste("P",1:pollsters,"_Ind",sep="")
Init_PollAvgs <- round(rowMeans(Init_Polls),2)

poll_avgs <- Init_PollAvgs
Polls_Herd <- Polls_Ind

#stopCluster(cluster1)

# Register function for simulating herded polls
simulateHerdedPolls <- function(init_sample,polling_avgs,absdev_limit,p_drawer){
  # Uses the last polling average per simulation to define outliers
  last_poll_avgs <- matrix(polling_avgs,nrow=sims,ncol=pollsters)
  outlier_matrix <- abs(init_sample - last_poll_avgs) >= absdev_limit
  outliers <- sum(outlier_matrix)
  
  adj_sample <- init_sample
  
  # Goes through the herding process by removing or adjusting outliers
  # Loops 4 times; this removes most of the outliers but not all (removing all outliers is very time-consuming and is a little less realistic)
  
  if(outliers > 0 && !is.na(outliers)){
    replacement <- rep(0,outliers)
    replacement[runif(outliers,0,1) <= p_drawer] <- NA
    adj_vec <- runif(outliers,0,1)
    adj_vec[(abs(init_sample - last_poll_avgs)[outlier_matrix] >= 2*absdev_limit)] <- 0
    replacement[!is.na(replacement) & adj_vec < 0.8] <- (init_sample[outlier_matrix][!is.na(replacement) & adj_vec < 0.8] - last_poll_avgs[outlier_matrix][!is.na(replacement) & adj_vec < 0.8]) * absdev_limit/abs(init_sample[outlier_matrix][!is.na(replacement) & adj_vec < 0.8] - last_poll_avgs[outlier_matrix][!is.na(replacement) & adj_vec < 0.8])
    replacement[!is.na(replacement) & adj_vec >= 0.8] <- rnorm(sum(!is.na(replacement) & adj_vec >= 0.8),mean=(init_sample[outlier_matrix][!is.na(replacement) & adj_vec >= 0.8] - last_poll_avgs[outlier_matrix][!is.na(replacement) & adj_vec >= 0.8])/2,sd=absdev_limit/2)
    replacement[!is.na(replacement)] <- replacement[!is.na(replacement)] + last_poll_avgs[outlier_matrix][!is.na(replacement)]
    
    adj_sample[outlier_matrix] <- replacement
  }
  
  outlier_matrix <- abs(adj_sample - last_poll_avgs) >= absdev_limit
  outliers <- sum(outlier_matrix)
  
  for (n in 1:2){
    if(outliers > 0 && !is.na(outliers)){
      replacement <- rep(0,outliers)
      #replacement[runif(outliers,0,1) < p_drawer] <- NA
      adj_vec <- runif(outliers,0,1)
      adj_vec[(abs(adj_sample - last_poll_avgs)[outlier_matrix] >= 2*absdev_limit)] <- 0
      replacement[!is.na(replacement) & adj_vec < 0.7] <- (adj_sample[outlier_matrix][!is.na(replacement) & adj_vec < 0.7] - last_poll_avgs[outlier_matrix][!is.na(replacement) & adj_vec < 0.7]) * absdev_limit/abs(adj_sample[outlier_matrix][!is.na(replacement) & adj_vec < 0.7] - last_poll_avgs[outlier_matrix][!is.na(replacement) & adj_vec < 0.7])
      replacement[!is.na(replacement) & adj_vec >= 0.7] <- rnorm(sum(!is.na(replacement) & adj_vec >= 0.7),mean=(adj_sample[outlier_matrix][!is.na(replacement) & adj_vec >= 0.7] - last_poll_avgs[outlier_matrix][!is.na(replacement) & adj_vec >= 0.7])/2,sd=absdev_limit/4)
      replacement[!is.na(replacement)] <- replacement[!is.na(replacement)] + last_poll_avgs[outlier_matrix][!is.na(replacement)]
      
      adj_sample[outlier_matrix] <- replacement
    }
    
    outlier_matrix <- abs(adj_sample - last_poll_avgs) >= absdev_limit
    outliers <- sum(outlier_matrix)
  }
  
  final_sample <- adj_sample
  
  if(outliers > 0 && !is.na(outliers)){
    replacement <- rep(0,outliers)
    #replacement[runif(outliers,0,1) < p_drawer] <- NA
    adj_vec <- runif(outliers,0,1)
    adj_vec[(abs(final_sample - last_poll_avgs)[outlier_matrix] >= 2*absdev_limit)] <- 0
    replacement[!is.na(replacement) & adj_vec < 0.6] <- (final_sample[outlier_matrix][!is.na(replacement) & adj_vec < 0.6] - last_poll_avgs[outlier_matrix][!is.na(replacement) & adj_vec < 0.6]) * absdev_limit/abs(final_sample[outlier_matrix][!is.na(replacement) & adj_vec < 0.6] - last_poll_avgs[outlier_matrix][!is.na(replacement) & adj_vec < 0.6])
    replacement[!is.na(replacement) & adj_vec >= 0.6] <- rnorm(sum(!is.na(replacement) & adj_vec >= 0.6),mean=(final_sample[outlier_matrix][!is.na(replacement) & adj_vec >= 0.6] - last_poll_avgs[outlier_matrix][!is.na(replacement) & adj_vec >= 0.6])/4,sd=absdev_limit/4)
    replacement[!is.na(replacement)] <- replacement[!is.na(replacement)] + last_poll_avgs[outlier_matrix][!is.na(replacement)]
    
    final_sample[outlier_matrix] <- replacement
  }
  
  return(final_sample)
}

# Simulates herded polls
for (i in 2:weeks){
  # Calculates the limits of what is defined as an outlier
  absdev_limit <- round((2-2/(1+exp(-0.5*(i-weeks)))),2)
  
  # Calculates the probability of an outlier poll being shelved (drawer effect)
  p_drawer <- round(0.3/(1+exp(1.25*(i-0.65*weeks))),2)
  
  # Generates initial polling
  if(i == 2){
    init_sample <- as.matrix(W2_Polls_Ind)
  } else {
    init_sample <- matrix(SampleVote[,i],nrow=sims,ncol=pollsters) + matrix(rnorm(sims*pollsters,0,sampling_err),nrow=sims,ncol=pollsters)
  }
  
  # Performs herding process on initial poll sample
  final_sample <- simulateHerdedPolls(init_sample,poll_avgs,absdev_limit,p_drawer)
  
  # Rounds polls to nearest 0.5pct and generates random polls if all initial polls have been shelved in drawer (for coding convenience; in real life there are often weeks where there are no polls)
  polls <- round(final_sample/0.5,0)*0.5
  new_poll_avgs <- round(rowMeans(polls,na.rm=TRUE),2)
  no_polls <- is.nan(new_poll_avgs)
  new_poll_avgs[no_polls] <- poll_avgs[no_polls] + (SampleVote[,i][no_polls] - poll_avgs[no_polls]) * absdev_limit/abs(poll_avgs[no_polls] - SampleVote[,i][no_polls])
  poll_avgs <- round(new_poll_avgs,2)
  
  for(p in 1:pollsters){
    Polls_Herd[[p]][,i] <- polls[,p]
  }
}

# Aggregates final herded polls
Final_Polls_Herd <- data.frame(foreach(i=1:pollsters,.inorder=TRUE) %dopar% {
  Polls_Herd[[i]][,weeks]
})

# Renames dataframe columns
colnames(Final_Polls_Herd) <- paste("P",1:pollsters,"_Herd",sep="")

# Calculates polling averages for both non-herded and herded polls
ind_avgs <- round(rowMeans(Final_Polls_Ind),2)
herd_avgs <- round(rowMeans(Final_Polls_Herd),2)

# Calculates poll average error for differing numbers of polls
PollAvgErr_Ind <- data.frame(foreach(i=1:pollsters,.inorder=TRUE) %dopar% {
  abs(rowMeans(Final_Polls_Ind[1:i]) - true_val)
})

PollAvgErr_Herd <- data.frame(foreach(i=1:pollsters,.inorder=TRUE) %dopar% {
  abs(rowMeans(Final_Polls_Herd[1:i]) - true_val)
})

stopCluster(cluster1)

# Cleans up working environment and renames objects
rm(list=c('adj_sample','final_sample','init_sample','last_poll_avgs','outlier_matrix','new_poll_avgs'))

colnames(PollAvgErr_Ind) <- paste("PollAvg",1:pollsters,sep="")
colnames(PollAvgErr_Herd) <- paste("PollAvg",1:pollsters,sep="")

# Calculates average error on polling averages containing varying numbers of polls
Change_PollAvgErr_Ind <- round(colMeans(PollAvgErr_Ind),2)
Change_PollAvgErr_Herd <- round(colMeans(PollAvgErr_Herd),2)

# Calculates errors in the final polling
errs <- round(Final_Polls_Ind - true_val,2)

# Calculates deviations from polling average in the finl polling
devs <- round(Final_Polls_Ind - rowMeans(Final_Polls_Ind,na.rm=TRUE),2)

# Sets up vectors of polling errors
cov_errs1 <- errs[,rep(colnames(errs),(pollsters-1):0)]
cov_devs1 <- devs[,rep(colnames(devs),(pollsters-1):0)]
index_vec <- c()

for (p in 1:(pollsters-1)){
  index_vec <- c(index_vec,(p+1):pollsters)
}

cov_errs2 <- errs[,index_vec]
cov_devs2 <- devs[,index_vec]

colnames(cov_errs1) <- paste("P",1:choose(pollsters,2),sep='')
colnames(cov_errs2) <- paste("P",1:choose(pollsters,2),sep='')
colnames(cov_devs1) <- paste("P",1:choose(pollsters,2),sep='')
colnames(cov_devs2) <- paste("P",1:choose(pollsters,2),sep='')

# Performs the same procedure of setting up vectors of all polling for herded polls
errs_herd <- round(Final_Polls_Herd - true_val,2)
devs_herd <- round(Final_Polls_Herd - rowMeans(Final_Polls_Herd,na.rm=TRUE),2)

cov_errs1_herd <- errs_herd[,rep(colnames(errs_herd),(pollsters-1):0)]
cov_devs1_herd <- devs_herd[,rep(colnames(devs_herd),(pollsters-1):0)]
index_vec <- c()

for (p in 1:(pollsters-1)){
  index_vec <- c(index_vec,(p+1):pollsters)
}

cov_errs2_herd <- errs_herd[,index_vec]
cov_devs2_herd <- devs_herd[,index_vec]

colnames(cov_errs1_herd) <- paste("P",1:choose(pollsters,2),sep='')
colnames(cov_errs2_herd) <- paste("P",1:choose(pollsters,2),sep='')
colnames(cov_devs1_herd) <- paste("P",1:choose(pollsters,2),sep='')
colnames(cov_devs2_herd) <- paste("P",1:choose(pollsters,2),sep='')

# Sets up parallel processing cluster
cores <- min(detectCores(),sims)
cluster2 <- makeCluster(cores)
registerDoParallel(cluster2)

# Calculates polling covariance and the correlated error between polls
corrdevs_ind <- data.frame(foreach(i=1:sims,.inorder=TRUE,.combine=rbind,.export=c('cov_errs1','cov_errs2','cov_devs1','cov_devs2')) %dopar% {
  c(unname(unlist(cov_errs1[i,])) %*% unname(unlist(cov_errs2[i,])),unname(unlist(cov_devs1[i,])) %*% unname(unlist(cov_devs2[i,])))
})

corrdevs_herd <- data.frame(foreach(i=1:sims,.inorder=TRUE,.combine=rbind,.export=c('cov_errs1_herd','cov_errs2_herd','cov_devs1_herd','cov_devs2_herd')) %dopar% {
  c(unname(unlist(cov_errs1_herd[i,])) %*% unname(unlist(cov_errs2_herd[i,])),unname(unlist(cov_devs1_herd[i,])) %*% unname(unlist(cov_devs2_herd[i,])))
})

stopCluster(cluster2)

colnames(corrdevs_ind) <- c('corrdev','sample_cov')
colnames(corrdevs_herd) <- c('corrdev','sample_cov')

rownames(corrdevs_ind) <- 1:nrow(corrdevs_ind)
rownames(corrdevs_herd) <- 1:nrow(corrdevs_herd)

corrdevs_ind <- corrdevs_ind/choose(pollsters,2)
corrdevs_herd <- corrdevs_herd/choose(pollsters,2)

rm(list=c("cluster1","cluster2",'cov_devs1','cov_devs1_herd','cov_devs2','cov_devs2_herd','cov_errs1','cov_errs1_herd','cov_errs2','cov_errs2_herd'))

par(family='Lato')

# Cullen and Frey graphs for the final polls
require(fitdistrplus)
descdist(unname(unlist(Final_Polls_Ind)))
descdist(unname(unlist(Final_Polls_Herd)))

hist(as.vector(matrix(as.matrix(Final_Polls_Ind),nrow=sims*pollsters,ncol=1)),border=NA,freq=FALSE,xlim=c(-8,8),ylim=c(0,0.5),col=rgb(1,0,0,0.25),breaks=25,main='Individual polls',xlab="Coalition 2pp in simulated polls")
hist(as.vector(matrix(as.matrix(Final_Polls_Herd),nrow=sims*pollsters,ncol=1)),add=TRUE,freq=FALSE,breaks=25,col=rgb(0,0,1,0.25),border=NA)
segments(true_val,0,true_val,1,lty=5,lwd=2)
#segments(-1,0,-1,1,col=rgb(0,0.5,0,0.35),lwd=4)
#segments(-1,0,-1,1,col=rgb(0,0.5,0,0.35),lwd=4)
#segments(-1.5,0,-1.5,1,col=rgb(0,0.5,0,0.35),lwd=4)
#segments(-1.5,0,-1.5,1,col=rgb(0,0.5,0,0.35),lwd=4)
#segments(-2,0,-2,1,col=rgb(0,0.5,0,0.35),lwd=4)

hist(ind_avgs,freq=FALSE,border=NA,breaks=50,xlim=c(-8,8),ylim=c(0,0.75),col=rgb(1,0,0,0.25),main='Polling averages',xlab='Coalition 2pp in simulated polling average')
hist(herd_avgs,add=TRUE,freq=FALSE,border=NA,breaks=50,col=rgb(0,0,1,0.25))
segments(true_val,0,true_val,1,lty=5,lwd=2)
#segments(-1.4,0,-1.4,1,col='forestgreen',lwd=4)

# Cullen and Frey graphs for the final polling average
descdist(ind_avgs)
descdist(herd_avgs)

#plot(x=corrdevs_ind$sample_cov,y=corrdevs_ind$corrdev,pch=21,col=NA,bg=rgb(0,0,0,1/50))
#plot(x=corrdevs_herd$sample_cov,y=corrdevs_herd$corrdev,pch=21,col=NA,bg=rgb(0,0,0,1/50))

#summary(lm(corrdev~.,corrdevs_ind))
#summary(lm(corrdev~.,corrdevs_herd))
#corrdevs <- rbind(corrdevs_ind,corrdevs_herd)
#summary(lm(corrdev~.,corrdevs))

PollAvgStats <- data.frame(round(cbind(ind_avgs,corrdevs_ind$sample_cov,corrdevs_ind$corrdev,herd_avgs,corrdevs_herd$sample_cov,corrdevs_herd$corrdev),2))
colnames(PollAvgStats) <- c('PollAvg_Ind','EstCorrDev_Ind','Corrdev_Ind','PollAvg_Herd','EstCorrDev_Herd','Corrdev_Herd')

# Outputs dataframes
write.csv(data.frame(Final_Polls_Ind,Final_Polls_Herd),'SimulatedPollsOutput.csv',row.names=FALSE)
write.csv(PollAvgStats,'SimulatedPollAvgStats.csv',row.names=FALSE)

# Calculates covariance matrix between polls
cov_matrix_ind <- cov(Final_Polls_Ind)
cov_matrix_herd <- cov(Final_Polls_Herd)

for(i in 1:pollsters){
  cov_matrix_ind[i,i] <- NA
  cov_matrix_herd[i,i] <- NA
}

mean(cov_matrix_ind,na.rm=TRUE)
mean(cov_matrix_herd,na.rm=TRUE)

# Calculate polling average errors
ind_errs <- ind_avgs - true_val
herd_errs <- herd_avgs - true_val

ind_abserr <- abs(ind_avgs - true_val)
herd_abserr <- abs(herd_avgs - true_val)