#
rm(list=ls())

d=read.csv("C:/Users/grma8007/Documents/OPTIMIZATION/final march/Exp758B_Fin.csv", header=T,sep=";")

d$GRP=gsub(",",".",d$GRP)
d$Cost.GRP=gsub(",",".",d$Cost.GRP)
d$Cost.GRP=gsub(" ","_",d$Cost.GRP)

d$Cost.GRP=gsub(substr(d[4429,"Cost.GRP"],2,2),"",d$Cost.GRP)

d$GRP=as.numeric(d$GRP)
d$Cost.GRP=as.numeric(d$Cost.GRP)

d$Copy=NULL
d$Dayparts=NULL
d$Affinity.Index=NULL
d$Discount..=NULL
d$External_TAG.Variables=NULL
d$Gross.Price=NULL
d$Net.Price=NULL
d$Price=d$GRP*d$Cost.GRP

# ----- Building of trainng set - if training fraction ==1 than d_train==d ---------------

training_fraction=1

# Building of trainng set for Polsat channel
dpol= d[d$Channel=="Polsat",]
set.seed(657)
s_pol= sample.int(nrow(dpol), floor(training_fraction*nrow(dpol)), replace=F)
dpol_train=d[s_pol,]

# Building of trainng set for TVP1 channel 
dtvp1=d[d$Channel=="TVP1",]
set.seed(657)
s_tvp1= sample.int(nrow(dtvp1), floor(training_fraction*nrow(dtvp1)), replace=F)
dtvp1_train=dtvp1[s_tvp1,]

# Building of trainng set for TVP2 channel
dtvp2=d[d$Channel=="TVP2",]
set.seed(657)
s_tvp2= sample.int(nrow(dtvp2), floor(training_fraction*nrow(dtvp2)), replace=F)
dtvp2_train=dtvp2[s_tvp2,]

# Building of trainng set for TVN channel
dtvn=d[d$Channel=="TVN",]
set.seed(657)
s_tvn= sample.int(nrow(dtvn), floor(training_fraction*nrow(dtvn)), replace=F)
dtvn_train=dtvn[s_tvn,]

# Building of final trainng set
d_train=rbind(dpol_train,dtvp1_train, dtvp2_train, dtvn_train)


# ------------------------------------- GA -----------------------------------------
popSize=2000
client_budget=1000000
max_iter=5000
epsilon=0.2
dilute_indeks=0.9

result = list(rozwiazanie_optymalne = c(), mean_fitness = c(), budget_opt=c())
best_GRP=vector()
coordinates=vector()

# Population initialization
population=matrix(NA, nrow = popSize, ncol = nrow(d_train))
population=t(apply(population,1, function(x) round(runif(nrow(d_train))>dilute_indeks)))

# Calculating fitness of indywiduals
GRP=vector()
budget=vector()
score=vector()
fitness=vector()

GRP=apply(population,1, function(x) sum(d_train$GRP[x==T]))
budget=apply(population,1, function(x) sum(d_train$GRP[x==T]*
                                             d_train$Cost.GRP[x==T]))

score=apply(population,1, function(x) 
  sum(d_train$GRP[x==T]) / 
    (45**(abs(log10(sum(d_train$Price[x==T])/client_budget))+1)) 
)

fitness= sapply(score, function(x) (1+epsilon)*x-min(score))
result$rozwiazanie_optymalne=GRP[which.max(fitness)]
result$mean_fitness=mean(fitness)
result$budget_opt=sum(d_train$Price[population[which.max(fitness),]==T])


war_stuck_idx=0
no_iter=0
no_pen=100
ongoing=0
while (no_iter<max_iter){
  
  no_iter=no_iter+1
  
  #--------- CHECK IF GA STUCK IN LOCAL OPTIMA (for no penalty method) -----------------
  
  #  if (no_iter>800){
  #   war_stuck=sum(tail(result$budget_opt, 100))/(tail(result$budget_opt, 1)*100)
  #  }else {
  #   war_stuck=1
  #}
  
  
  #  if (ongoing==0){
  #    if (war_stuck<0.998){
  #      war_stuck_idx=1
  #    } else{
  #      war_stuck_idx=no_pen
  #    }
  #  } else{
  
  #  }
  
  # --------------------------------------------------------------------------------
  
  
  
  # --------------------- SELECTION OF INDYWIDUALS TO NEXT POPULATION -------------
  
  udz_proc=sapply(fitness, function(x) x/sum(fitness))
  udz_nar=cumsum(udz_proc)
  
  losowanie=runif(popSize, 0.0, 1.0)
  roulette=sapply(losowanie, function(x) which.min(udz_nar<x))
  
  population_next=population[roulette,]
  
  
  # -------------------------------- CROSSOVER  ----------------------
  
  pk=runif(nrow(population_next), 0.0, 1.0)
  popCross=population_next[pk>=0.4,]
  pop_not_cross=population_next[pk<0.4,]
  
  TP_cross=function(parent_1){
    parent_2=popCross[round(runif(1,1,nrow(popCross))),]
    position_i=round(runif(1,1,(length(parent_1)-100)))
    position_st_j=position_i+1
    position_j=round(runif(1,(position_st_j),(length(parent_1)-50)))
    
    offspring_1=c(parent_1[1:position_i],
                  parent_2[(position_i+1):(position_j)],
                  parent_1[(position_j+1):length(parent_1)])
    
    result=offspring_1
    return(result)
  }
  
  crossover=matrix(NA, nrow = nrow(popCross), ncol = nrow(d_train))
  crossover=apply(popCross,1, function(x) TP_cross(x))
  popPar_after_cross=rbind(t(crossover),pop_not_cross)
  
  # ----------------------------- MUTATION ------------------------------------
  
  pm=runif(nrow(popPar_after_cross), 0.0, 1.0)
  
  pop2Mut=popPar_after_cross[pm<0.2,]
  pop_not_mut=popPar_after_cross[pm>=0.2,]
  
  RSM_mutation=function(dane_rsm){
    position_i=round(runif(1,1,(length(dane_rsm)-3)))
    position_st_j=position_i+1
    position_j=round(runif(1,(position_st_j),(length(dane_rsm)-1)))
    try=c(dane_rsm[1:position_i],rev(dane_rsm[(position_i+1):(position_j)]),
          dane_rsm[(position_j+1):length(dane_rsm)])
    return(try)
  }
  
  popMut=apply(pop2Mut,1, function(x) RSM_mutation(x))
  popMut_t=t(popMut)
  population=rbind(popMut_t, pop_not_mut)
  
  
  # Calculating fitness of indywiduals
  GRP=vector()
  budget=vector()
  score=vector()
  fitness=vector()
  
  GRP=apply(population,1, function(x) sum(d_train$GRP[x==T]))
  budget=apply(population,1, function(x) sum(d_train$GRP[x==T]*
                                               d_train$Cost.GRP[x==T]))
  
  #--------------------------------JUDGEMENT DAY--------------------------------------------
  if (no_iter>700){
    war_stuck=sum(tail(result$budget_opt, 250))/(tail(result$budget_opt, 1)*250)
  }else {
    war_stuck=1
  }
  
  
  if (war_stuck<1){
    best_vector=apply(population,1, function (x) sum(d_train$GRP[x==T]))
    best_one=population[which.max(best_vector),]
    population=matrix(NA, nrow = popSize, ncol = nrow(d_train))
    population=t(apply(population,1, function(x) round(runif(nrow(d_train))>dilute_indeks)))
    population[1,]=best_one
  }
  
  score=apply(population,1, function(x) 
    sum(d_train$GRP[x==T]) / 
      (45**(abs(log10(sum(d_train$Price[x==T])/client_budget))+1)) 
  )
  
  #-------------------------------- NO PENALTY -------------------------------------------------
  
  
  # if (war_stuck_idx<no_pen){
  #    score=apply(population,1, function(x) 
  #      sum(d_train$GRP[x==T])
  #    )
  #    war_stuck_idx=war_stuck_idx+1
  #    ongoing=1
  #  }else{
  #    score=apply(population,1, function(x) 
  #      sum(d_train$GRP[x==T]) / 
  #        (45**(abs(log10(sum(d_train$Price[x==T])/client_budget))+1)) 
  #    )
  
  #   ongoing=0
  #  }
  
  #-----------------------------------usuall score ---------------------------------------
  #score=apply(population,1, function(x) 
  #  sum(d_train$GRP[x==T]) / 
  #    (45**(abs(log10(sum(d_train$Price[x==T])/client_budget))+1)) 
  #)
  
  #-------------------------------------------------------------------------------------------
  fitness= sapply(score, function(x) (1+epsilon)*x-min(score))
  
  result$rozwiazanie_optymalne=rbind(result$rozwiazanie_optymalne, GRP[which.max(fitness)])
  coordinates=rbind(coordinates, population[which.max(fitness),])
  
  result$mean_fitness=rbind(result$mean_fitness, mean(fitness))
  result$budget_opt=rbind(result$budget_opt, sum(d_train$Price[population[which.max(fitness),]==T]))
  
  print(no_iter)
  print(mean(fitness))
}


plot(result$rozwiazanie_optymalne,
     main="GRP achieved for best solution in each population",
     xlab="Iteration number",
     ylab="GRP")

plot(result$budget_opt,
     main="Budger achieved for best solution in each population",
     xlab="Iteration number",
     ylab="Budget [PLN]")

plot(result$mean_fitness,
     main="Graph of the average objective function in each population",
     xlab="Iteration number",
     ylab="Objective function values")

