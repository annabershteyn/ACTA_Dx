rm(list = ls())# clear the workspace
library(stats)
set.seed(20210512)
# 4593328	Pupils in primary education (ages 6-12), Malawi 2019	https://data.worldbank.org/indicator/SE.PRM.ENRL?locations=MW
# 989847	Pupils in secondary education (ages 13-17), Malawi 2019	https://data.worldbank.org/indicator/SE.SEC.ENRL?end=2019&locations=MW&start=1972&view=chart
# 69.51	Pupil-teacher ratio, primary education, Malawi	https://data.worldbank.org/indicator/SE.PRM.ENRL.TC.ZS?locations=MW
# 37.9	Pupil-teacher ratio, secondary education, Malawi, 2015	https://data.worldbank.org/indicator/SE.SEC.ENRL.TC.ZS?end=2019&locations=MW&start=1972&view=chart
# 6065	Number of primary schools in Malawi, 2017	https://www.unicef.org/esa/sites/unicef.org.esa/files/2019-04/UNICEF-Malawi-2018-Education-Budget-Brief.pdf
# 1411	Number of secondary schools in Malawi, 2017	https://www.unicef.org/esa/sites/unicef.org.esa/files/2019-04/UNICEF-Malawi-2018-Education-Budget-Brief.pdf
pupils_per_primary_school = 757.3500412
pupils_per_secondary_school = 701.5216159	
teachers_per_primary_school = 10.89555519
teachers_per_secondary_school = 18.50980517
total_people_per_primary_school = pupils_per_primary_school + teachers_per_primary_school
total_people_per_secondary_school = pupils_per_secondary_school + teachers_per_secondary_school
frac_days_in_school = 5/7
days_of_active_infection = 15

# 0.15%	Primary school SAR, given Rt=0.8	
# 0.16%	Secondary school SAR, given Rt=0.8	
# 0.22%	Primary school SAR, given Rt=1.2	
# 0.23%	Secondary school SAR, given Rt=1.2	
# 0.36%	Primary school SAR, given Rt=2	
# 0.39%	Secondary school SAR, given Rt=2	

# Distributed transmission from index case over 2 weeks, gamma distribution, k=2.25, theta=2.80
sim_data_columns = c("S","E",paste0("I_",seq(1,15)),"R","inc_community","inc_school","attack_rate") # building in but not using E since we have I daily for 14 days

n_days_to_simulate = 90

# will need to loop over these

community_prevalence = 0.01
community_incidence = community_prevalence/10 # infectious duration
Rt = 2

school_type = "PRIMARY"
# school type
if(school_type=="PRIMARY"){
  n_teachers = round(teachers_per_primary_school,digits=0)
  n_pupils = round(pupils_per_primary_school,digits=0)
  n_schools = 6065
  #rel_infectivity_child=0.2
}else{
  n_teachers = round(teachers_per_secondary_school,digits=0)
  n_pupils = round(pupils_per_secondary_school,digits=0)
  n_schools = 1411
  #rel_infectivity_child=1
}

pop_per_school = n_teachers + n_pupils

infectivity_over_time_NOT_NORMALIZED = dgamma(x=seq(0,days_of_active_infection,1), shape=2.25, rate = (1/2.8))
normalizing_multiplier = Rt/(frac_days_in_school*pop_per_school*sum(infectivity_over_time_NOT_NORMALIZED)) 
infectivity_over_time = infectivity_over_time_NOT_NORMALIZED*normalizing_multiplier

sim_burnin = setNames(data.frame(matrix(0, ncol = length(sim_data_columns), nrow = days_of_active_infection)), sim_data_columns)

# Draw from Binomial distribution for how many people got infected in the community
sim_burnin$I_1 = rbinom(n=days_of_active_infection,size=pop_per_school,prob=community_incidence)

# allow infected people to progress through the days of infection
for(d_iter in seq(1,days_of_active_infection-1)){  sim_burnin[[paste0("I_",d_iter+1)]][-1]=sim_burnin[[paste0("I_",d_iter)]][1:(length(sim_burnin$I_1)-1)]}
sim_burnin = sim_burnin[nrow(sim_burnin),] # only take the final day of burn-in, which will be the first day of the sim

sim  = setNames(data.frame(matrix(0, ncol = length(sim_data_columns), nrow = n_days_to_simulate-1)), sim_data_columns)
sim = rbind(sim_burnin,sim) # start the first day from the burn-in

sim$I_1[1]=rbinom(n=1,size=pop_per_school,prob=community_incidence)
sim$S[1] = pop_per_school - sim$E[1] - sim$R[1] - sum(sim[1,paste0("I_",seq(1,15,1))])

for(d_iter in seq(1,n_days_to_simulate-1)){
  
  
  if(((d_iter-1) %% 7)<(7*frac_days_in_school)){
    sim$attack_rate[d_iter] = 1-exp(-1*sum(as.vector(sim[d_iter,paste0("I_",seq(1,15,1))]*infectivity_over_time))) # convert from daily rate to prob
    if(sim$attack_rate[d_iter]>0 & sim$S[d_iter]>0){sim$inc_school[d_iter+1]=rbinom(n=1,size=sim$S[d_iter],prob = sim$attack_rate[d_iter])}
  }
  
  sim$inc_community[d_iter+1]=rbinom(n=1,size=sim$S[d_iter]-sim$inc_school[d_iter+1],prob = community_incidence) # community incidence only into S's not to be infected at school
  sim$I_1[d_iter+1]=sim$inc_school[d_iter+1]+sim$inc_community[d_iter+1]
  sim[d_iter+1,paste0("I_",seq(2,15,1))]=sim[d_iter,paste0("I_",seq(1,14,1))]
  sim$R[d_iter+1] = sim$R[d_iter]+sim$I_15[d_iter]
  sim$S[d_iter+1] = pop_per_school - sim$E[d_iter+1] - sim$R[d_iter+1] - sum(sim[d_iter+1,paste0("I_",seq(1,15,1))])
  
}

print(sim)

