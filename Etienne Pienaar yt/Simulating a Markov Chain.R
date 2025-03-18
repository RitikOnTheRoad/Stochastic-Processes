rm(list=ls())

# In this problem we assume three states (1,2,3).
# We define one step transition probability matrix as follows.
transition_matrix=rbind(c(0.5,0.5,0),c(0.75,0,0.25),c(0.33,0.33,0.33))
transition_matrix


transition<-function(current_state,transition_matrix){
  next_state=sample(1:3,1,prob=transition_matrix[current_state,])
  current_state=next_state
  return(current_state)
}

initial_state=2
all_states=c(initial_state)
for(i in 1:10){
  current_state=transition(initial_state,transition_matrix)
  all_states=append(all_states,current_state)
  initial_state=current_state
}
plot(all_states,xlim=c(1,11),ylim=c(0,3),xlab = "Steps",ylab = "State",type="b")


