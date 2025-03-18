rm(list=ls())

transition_matrix=rbind(c(0.5,0.5,0),
                        c(0,0.25,0.75),
                        c(0.75,0,0.25))

#1: Calculate the 4 step transition probability

P4=transition_matrix%*%transition_matrix%*%transition_matrix%*%transition_matrix
P4
#======================================================================================

#2: Calculate the marginal distribution using the following initial distribution

delta_0=c(1,0,0) # at t=0, state 1 is occupied with certainty.

# Marginal distribution at step t is given by pi^t = pi^(t-1) * P.
# where pi^t is the marginal distribution at t step and P is the transition matrix.

delta_1=delta_0%*%transition_matrix
delta_1 # at t=1, there's a 50% of being in state 1 or 2.

# plot marginal probability of state 2 from 0th step to 10th step
delta_t=delta_0

plot(0,delta_t[2],type="p",xlim=c(0,10),ylim=c(0,1),ylab="P(Xt=2)",xlab="t",pch=16)
abline(h=c(0,0.5,1),lty=2)

for(i in 1:10){
  delta_t=delta_t%*%transition_matrix
  points(i,delta_t[2],pch=16)
}

#======================================================================================

