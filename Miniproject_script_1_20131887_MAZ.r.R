###############################################################################################
## Project: MS4217 Mini-project
## Script purpose: analysis tennis data
## Date: 18th December,2020
## Author:Adams Zequi Mohammed
###############################################################################################
install.packages('DiagrammeR')
library(DiagrammeR)
library(tidyverse)
library(expm)
theme_set(cowplot::theme_cowplot())

install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)    # alternatively, this also loads %>%

# read in the data
tennis_df <- read_csv('/Users/adamszequi/Desktop/Msc.Math Modelling/4217-sto processes/Stochastic_tennis_mini_project-1/stochastic_tennis_w_states.csv')
tennis_df
# The same markov function from the lectures...
markov <- function(init, mat, n, labels){
  # one line if statement to add labels if not present
  if(missing(labels)) labels <- 1:length(init)
  
  simlist <- numeric(n+1) # where we are going to store each transition
  states <- 1:length(init) # How many states do we have?  
  simlist[1] <- sample(states,1,prob=init) # what state do we start in from the inital state?
  
  for(i in 2:(n+1)){ # from state i-1 where do we go next?
    # sample the prob vec for trans from x_{t-1} to X_{t}
    simlist[i] <- sample(states,1,prob=mat[simlist[i-1],]) 
  }
  return(labels[simlist])
}


#Introduction 
---
#We create a demonstartive transition diagram to describe movement between various states for a stochatic tennis model
DiagrammeR::mermaid("graph LR; Deuce-->Advantage-P1 ; Advantage-P1-->Deuce; Deuce--> Advantage-P2; Advantage-P2-->Deuce; Advantage-P1-->Game-P1; Advantage-P2-->Game-P2")

# generate graphical and numeric summaries ----------------------------------------------------

#We find out interesting summary statistics about our games
Games_Played_Per_Match_Summary=summary(tennis_df$game_no)
Games_Played_Per_Match_Summary

#We create a boxplot of games played per match to visualise the games played 
boxplot(Games_Played_Per_Match_Summary)

#We find out interesting summary statistics about our matches
Matches_Summary=summary(tennis_df$match_id)
Matches_Summary

#Our Analysis is going to be based on Venus Williams so  p1 should only be Venus
tennis_df$p1 %>% unique()

# We take a look at Venus's opponents 
tennis_df$p2%>% unique() 
tennis_df$p2 %>% unique() %>% length # 60 opponents in the data frame

# Which plauyer has Venus played the most number of points with 
tennis_df %>% count(p2) %>% arrange(desc (n)) # Serena is number 5

#We create a dataframe of how many points she's played against other players
dataframe_descend<-tennis_df %>% count(p2) %>% arrange(desc(n)) %>%  tail(5)

#From our earlier dataframe we can combine the top 5 players shes played against
#and bottom 5 players 
New_df <- rbind(head(dataframe_descend, 5), tail(dataframe_descend , 5))

#Lets Visualise top and bottom 5 players she's played against
New_df  %>% 
  ggplot(data=., aes(y=p2, x=n)) +
  geom_bar(stat="identity", color="blue", fill="white")

tennis_df %>% count(p2) %>% arrange(desc(n)) %>%  tail(5)%>% 
  ggplot(data=., aes(y=p2, x=n)) +
  geom_bar(stat="identity")


# We calculate the probability p of her winning a game
#and probability q of her losing a game
tennis_df %>% filter(to %in% c('game_p1', 'game_p2')) %>% 
  count(to) %>% 
  mutate(prob = n/sum(n))

#Draw a piechart of distribution of number of points won or lost
lbls <- c("Won point p=0.521", "Lost point q=0.479")
pie(slices,labels=lbls ,explode=0.1,main="Points won and Lost")

#This code finds the upper boundaries(higher than 140) of points won 
p1 <- point_count %>% filter(n >= 140) %>% ggplot(aes(y = p2, x = n)) + 
  geom_bar(stat = 'identity')
ggsave(p1, filename = './number_of_pointed_played.png')


# We look at the number of transitions between states
tennis_df %>% count(from, to)


# We determine how long a game would last 
duration_df= tennis_df %>% group_by(unique_id) %>% 
  summarise(
    game_length = max(game_duration) + 1
  ) 

#Plot duration
duration_df %>% ggplot(aes(x=game_length)) + geom_bar(fill='steelblue')

#We analyse games that venus played against her sister
tennis_vs_df <- tennis_df %>% filter(p2 == 'Williams S.')
summary(tennis_vs_df)

# We calculate the probability p of her winning an
#and probability q of Serena winning 
tennis_vs_df%>% filter(to %in% c('game_p1', 'game_p2')) %>% 
count(to) %>% 
mutate(prob = n/sum(n))

#Draw a piechart of distribution of number of points won by Venus or Serena
lbls <- c("Venus won p=0.529", "Serena won q=0.471")
pie(slices,labels=lbls ,explode=0.1,main="Points won and Lost")


# create transition matrix  -------------------------------------------------------------------

#Lets count the number of movement from one state to another with regards top Serena vs Venus
trans_count <- tennis_vs_df %>% count(from, to)

trans_count
# from   to          n
# <chr>  <chr>      <int>
# 1 adv_p1 deuce      19 # Venus losses point
# 2 adv_p1 game_p1    27 # Venus wins point
# 3 adv_p2 deuce      19 # Venus wins point
# 4 adv_p2 game_p2    24 # Venus losses point
# 5 deuce  adv_p1     46 # Venus wins point
# 6 deuce  adv_p2     43 # Venus losses point

# so we find out that the 2nd, 3rd and 5th rows are where Venus winns

# p is the total number of transtions that where Venus wins a point
#we sum all values of serenas wins and divide by total of both Serena and Venus's wins
p <- sum(trans_count$n[c(2,3,5)])/sum(trans_count$n)
q <- 1 - p
q
cat('The probability of Venus winning a point p is : ',p)
cat('The probability of Serena winning a point q is : ',q)

# populate a transition matrix with the transition probabilities
P <- matrix(0,5,5) 
state_names <- c('deuce', 'adv_p1', 'adv_p2', 'game_p1', 'game_p2')
colnames(P) <- state_names
rownames(P) <- state_names

#We populate our transition matrix with trasition probabilities 
P[1,2] <- p # from deuce to adv_p1
P[1,3] <- q # from deuce to adv_p2

P[2,4] <- p # from adv_p1 to game_p1
P[2,1] <- q # from adv_p1 to deuce

P[3,5] <- q # from adv_p2 to game_p2
P[3,1] <- p # from adv_p2 to deuce

P[4,4] <- 1 # from game_p1 to game_p1
P[5,5] <- 1 # from game_p2 to game_p2

#Our populated trasnition matrix
P

library(expm)

P %^% 2

P %^% 5

P %^% 100

cat('Starting at Deuce ,What is the probability that Venus wins across  the 2 step,5 step and 100 step probabilities?')

#We create a demonstartive transition diagram to describe movement between various states for a stochatic tennis model
DiagrammeR::mermaid("graph LR; Deuce-->Advantage-P1 ; Advantage-P1-->Deuce; Deuce--> Advantage-P2; Advantage-P2-->Deuce; Advantage-P1-->Game-P1; Advantage-P2-->Game-P2")

alpha=c(1,0,0,0,0)

t(alpha) %*% (P %^% 2 ) 

t(alpha) %*% (P %^% 5 ) 

t(alpha) %*% (P %^% 100 ) 

cat('Starting at either Adv. Venus or Adv. Serena ,What is the probability that Venus wins across  the 2 step,5 step and 100 step probabilities?')

alpha_not_told=c(0,1,1,0,0)

t(alpha_not_told) %*% (P %^% 2 ) 

t(alpha_not_told) %*% (P %^% 5 ) 

t(alpha_not_told) %*% (P %^% 100 )

# using simulation
M = 10000
n = 100

#We replicate our results from probability of wins in our earlier analysis and compare them
#using the replicate function in R
simulations <- replicate(M,markov(init=alpha,P,100,state_names))
cat('The simulated probabilities of wins when game starts from Deuce using markov chain are :')

(table(simulations)/M)/100

simulations_when_we_start_from_Adv_Venus_or_Serena <- replicate(M,markov(init=alpha_not_told,P,100,state_names))
cat('The simulated probabilities of wins when game starts from either Adv.Serena or Adv.Venus is  using markov chain are :')

(table(simulations_when_we_start_from_Adv_Venus_or_Serena )/M)/100
# extend code ---------------------------------------------------------------------------------
#We start extending our code by looking at the long term behaviour by starting with 
#limiting distribution of our model.
P_100=P %^% 100
P_1000=P %^% 1000
P_10000=P %^% 10000
P_100000=P %^% 100000
P_1000000=P %^% 1000000
P_100
P_1000
P_10000
P_100000
P_1000000



#We determine the fundamental matrix by finding the submatrices Q,R,I,O
P
Q<-P[1:3,1:3]
Q
R<-P[1:3,4:5]
R
I<-P[4:5,4:5]
I
O<-P[4:5,1:3]
O

Fundamental_matrix=solve(diag(3)-Q)
Fundamental_matrix[1:3]

# We find the player expected to hit the first point. 
tennis_vs_df%>% filter(to %in% c('adv_p1', 'adv_p2')) %>% 
  count(to) %>% 
  mutate(prob = n/sum(n))