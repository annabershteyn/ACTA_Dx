rm(list = ls())# clear the workspace
start_script = Sys.time()
library(stats)
curr_scenario = 'all' # <== set to specific number or to 'all'
curr_seed = 20210512
set.seed(curr_seed)

sim_size = 'SMALL'

if(sim_size == "LARGE"){
n_replicates = 100
n_boostraps = 5000 
fraction_of_Malawi_to_simulate = 1/5
}else{
  if(sim_size == "MEDIUM"){
  n_replicates = 50
  n_boostraps = 5000 
  fraction_of_Malawi_to_simulate = 1/25
}else{
  if(sim_size == "SMALL"){
    n_replicates = 2
    n_boostraps = 100 
    fraction_of_Malawi_to_simulate = 1/700}}}
  


#bsci <- function(x){bstrap <- c();for (i in 1:1000){bstrap = c(bstrap,mean(sample(x,length(x),replace=T)));return(as.vector(bstrap))}}
fn_boot_95CI_lo <- function(x){
  bstrap = c()
  for(i in 1:n_boostraps){bstrap = c(bstrap, mean(sample(x,length(x),replace=T)))}
  return(quantile(bstrap,.025))
}

fn_boot_95CI_hi <- function(x){
  bstrap = c()
  for(i in 1:n_boostraps){bstrap = c(bstrap, mean(sample(x,length(x),replace=T)))}
  return(quantile(bstrap,.975))
}


########################################################################################################
## Sources of data on number of primary and secondary schools, teachers/school, and pupils per school:##
# 4593328	Pupils in primary education (ages 6-12), Malawi 2019	https://data.worldbank.org/indicator/SE.PRM.ENRL?locations=MW
# 989847	Pupils in secondary education (ages 13-17), Malawi 2019	https://data.worldbank.org/indicator/SE.SEC.ENRL?end=2019&locations=MW&start=1972&view=chart
# 69.51	Pupil-teacher ratio, primary education, Malawi	https://data.worldbank.org/indicator/SE.PRM.ENRL.TC.ZS?locations=MW
# 37.9	Pupil-teacher ratio, secondary education, Malawi, 2015	https://data.worldbank.org/indicator/SE.SEC.ENRL.TC.ZS?end=2019&locations=MW&start=1972&view=chart
# 6065	Number of primary schools in Malawi, 2017	https://www.unicef.org/esa/sites/unicef.org.esa/files/2019-04/UNICEF-Malawi-2018-Education-Budget-Brief.pdf
# 1411	Number of secondary schools in Malawi, 2017	https://www.unicef.org/esa/sites/unicef.org.esa/files/2019-04/UNICEF-Malawi-2018-Education-Budget-Brief.pdf

# percent of enrolled pupils that are secondary school = 989847/(989847+4593328) = 17.8%
# weighted average teachers per school = 0.178*18.50980517 + (1-0.178)*10.89555519 = 12.3
# weighted avg total pop per school = 0.178*(18.50980517+701.5216159) + (1-0.178)*(10.89555519+757.3500412) = 759.66

########################################################################################################

Malawi_n_primary_schools = 6065
Malawi_n_secondary_schools = 1411
Malawi_n_primary_schools + Malawi_n_secondary_schools

model_n_primary_schools = round(Malawi_n_primary_schools*fraction_of_Malawi_to_simulate,digits=0)
model_n_secondary_schools = round(Malawi_n_secondary_schools*fraction_of_Malawi_to_simulate,digits=0)

# Assume primary and secondary schools are separate without between-school transmission (other than via community transmission)
pupils_per_primary_school = 757.3500412
pupils_per_secondary_school = 701.5216159	
teachers_per_primary_school = 10.89555519
teachers_per_secondary_school = 18.50980517

frac_days_in_school = 5/7 # Assume Day 1 of the sim to be a Monday and assume weekends are Saturdays and Sundays with no school in session
days_of_active_infection = 15 # Gaussean distribution with no infectiousness on Day 1, peak infectiousness on days 4 and 5, then long tail to Day 15. 
ENABLE_SCHOOL_BASED_TESTING = TRUE

# For each run of the entire school system of Malawi, obtain aggregate counts across schools of the following. (Re-run Malawi 100's of times to obtain mean and CI of these)
agg_data_columns = c("tests_administered","inc_inf_pupils_PRIMARY","inc_inf_pupils_SECONDARY","inc_inf_teachers") # building in but not using E since we have daily infectiousness


# Subscript t denotes teacher, p denotes pupil. bI denotes the prob of infecting any given other person, before accounting for reduced susceptibility if kids.
sim_data_columns = c("t_S","t_E",paste0("t_I_",seq(1,15)),"t_R","t_I1_comm","t_I1_schl","t_bI",
                     "p_S","p_E",paste0("p_I_",seq(1,15)),"p_R","p_I1_comm","p_I1_schl","p_bI",
                     "n_tests") # building in but not using E since we have daily infectiousness



scenario_iter = 0
# ask organizers -- are scenarios 18 and 19 identical? answer -- yes and should skip scenario 19

############################################
### Begin loop over 72 testing scenarios ###
############################################

for(testing_pop in c("All teachers","All teachers + 13-18 year olds","All teachers + 5-12 year olds","All teachers + all students")){
  for(testing_freq in c("1x/2 weeks","1x/week","2x/week")){
    for(Rt in c(0.8, 1.2, 2)){
      for(community_prevalence in c(0.001, 0.01)){
        
        scenario_iter = scenario_iter+1
        
        # run only desired scenarios. Regardless, skip scenario_iter 19 because it is a random duplicate in the template that throws off the numbering
        if(scenario_iter == 19 | (curr_scenario != "ALL" & curr_scenario != "all" & curr_scenario != scenario_iter)){next} # option of running only one scenario (for parallel computing)
        print(scenario_iter)
      }}}}
        #print(paste0("Scenario ",scenario_iter,": ",testing_pop," -- ",testing_freq," -- Rt=",Rt," -- comm prev ",community_prevalence))
        
        ## For testing purposes, just hard-set a single scenarion
        #testing_pop = "All teachers + all students"
        #testing_freq = "2x/week" 
        #Rt = 1.2
        #community_prevalence = 0.01
        
        community_incidence = community_prevalence/10 # infectious duration
        
        n_days_to_simulate = 90
        
        # Assume 2x/week testing occurs every Monday and Thursday, and other strategies occur on Mondays (the optimal day to minimize weekday exposures)
        if(testing_freq=="1x/2 weeks"){testing_days = seq(1,n_days_to_simulate,14)}
        if(testing_freq=="1x/week"){testing_days = seq(1,n_days_to_simulate,7)}
        if(testing_freq=="2x/week"){testing_days = floor(seq(1,n_days_to_simulate,3.5))}
        
        
      
        if(n_replicates>1){
          reps_to_agg = array(rep(0, n_days_to_simulate*length(agg_data_columns)*1, n_replicates), c(n_days_to_simulate, length(agg_data_columns),n_replicates))
        }
        
        ###################################################################################################################
        ### If looking for CI for multiple runs of the whole country -- begin loop over aggregate of all Malawi schools ###
        ###################################################################################################################
        
        for(replicate_number in seq(1,n_replicates)){
          
          start_1_natl_sim <- Sys.time()
          
          
          agg_natl = setNames(data.frame(matrix(0, ncol = length(agg_data_columns), nrow = n_days_to_simulate)), agg_data_columns)


          

          
          for(school_iter in seq(1,model_n_primary_schools + model_n_secondary_schools,1)){
            if(school_iter > model_n_primary_schools){school_type = "SECONDARY"}else{school_type = "PRIMARY"}
            
            ############################################################
            ### Begin setup/sim/processing of each individual school ###
            ############################################################           
            
            agg_schl = setNames(data.frame(matrix(0, ncol = length(agg_data_columns), nrow = n_days_to_simulate)), agg_data_columns)
            
            if(school_type=="PRIMARY"){
              n_teachers = round(teachers_per_primary_school,digits=0)
              n_pupils = round(pupils_per_primary_school,digits=0)
              n_schools = 6065
              
              # Datner 2021 Israeli study: children age 0-19 are 43%[31%-55%] as suscepible as adults and 63%[37%-88%] as infectious as adults
              #https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1008559
              
              # Zhu 2020 global meta-analysis: children 62% [42%-91%] as susceptible as adults and ?? as infectious as adults
              # Instead of estimating how much less infectious kids are, they estimated how much less infectious asymptomatic people are :( and they are 19% as infectious
              # In household transmission studies, children are found to be the index case only 18.5% of the time
              # "In a subset analysis where additional information was provided on the age of the pediatric contact, younger children (<10 years) were no more or less susceptible to infection compared with older children (>10 years); RR=0.69 (95% CI, 0.26-1.82) "
              # https://academic.oup.com/cid/advance-article/doi/10.1093/cid/ciaa1825/6024998
              
              rel_infectiousness_child   = 0.63
              rel_susceptibility_child = 0.43
              
            }else{
              n_teachers = round(teachers_per_secondary_school,digits=0)
              n_pupils = round(pupils_per_secondary_school,digits=0)
              n_schools = 1411
              rel_infectiousness_child   = 1
              rel_susceptibility_child = 1
            }
            
            pop_per_school = n_teachers + n_pupils
            
            infectiousness_over_time_NOT_NORMALIZED = dgamma(x=seq(0,days_of_active_infection,1), shape=2.25, rate = (1/2.8))
            normalizing_multiplier = Rt/(frac_days_in_school*pop_per_school*sum(infectiousness_over_time_NOT_NORMALIZED)) #### TO DO -- normalizing multiplier sep for teachers and pupils
            infectiousness_over_time = infectiousness_over_time_NOT_NORMALIZED*normalizing_multiplier
            
            infectiousness_over_time_child = infectiousness_over_time*rel_infectiousness_child
            
            sim_burnin = setNames(data.frame(matrix(0, ncol = length(sim_data_columns), nrow = days_of_active_infection)), sim_data_columns)
            
            # Draw from Binomial distribution for how many people got infected in the community
            sim_burnin$t_I_1 = rbinom(n=days_of_active_infection,size=n_teachers,prob=community_incidence)
            sim_burnin$p_I_1 = rbinom(n=days_of_active_infection,size=n_pupils,  prob=community_incidence)
            
            # allow infected people to progress through the days of infection
            for(d_iter in seq(1,days_of_active_infection-1)){  
              sim_burnin[[paste0("t_I_",d_iter+1)]][-1]=sim_burnin[[paste0("t_I_",d_iter)]][1:(length(sim_burnin$t_I_1)-1)]
              sim_burnin[[paste0("p_I_",d_iter+1)]][-1]=sim_burnin[[paste0("p_I_",d_iter)]][1:(length(sim_burnin$p_I_1)-1)]  
            }
            sim_burnin = sim_burnin[nrow(sim_burnin),] # only take the final day of burn-in, which will be the first day of the sim
            
            sim  = setNames(data.frame(matrix(0, ncol = length(sim_data_columns), nrow = n_days_to_simulate)), sim_data_columns)
            sim[1,] = sim_burnin[1,] # start the first day from the burn-in
            
            sim$t_I_1[1]=rbinom(n=1,size=n_teachers,prob=community_incidence)
            sim$p_I_1[1]=rbinom(n=1,size=n_pupils,  prob=community_incidence)
            sim$t_S[1] = n_teachers - sim$t_E[1] - sim$t_R[1] - sum(sim[1,paste0("t_I_",seq(1,15,1))])
            sim$p_S[1] = n_pupils -   sim$p_E[1] - sim$p_R[1] - sum(sim[1,paste0("p_I_",seq(1,15,1))])                            
            
            #######################################
            ########## begin simulation ###########
            #######################################
            
            for(d_iter in seq(1,n_days_to_simulate-1)){
              
              if(((d_iter-1) %% 7)<(7*frac_days_in_school)){
                
                if(ENABLE_SCHOOL_BASED_TESTING){
                  
                  # Check if today is a testing day
                  if(is.element(d_iter,testing_days)){
                    
                    ### NOTE, by changing seq(1,15,1) below to a narrower range, we can have an imperfect Ag test that misses the very start and end of infection when viral shedding is low
                    
                    # All testing strategies include teachers. So if it is a testing day, move infected teachers to R.
                    sim$t_R[d_iter] = sim$t_R[d_iter] + sum(sim[d_iter,paste0("t_I_",seq(1,15,1))])
                    sim[d_iter,paste0("t_I_",seq(1,15,1))]=0
                    
                    # Some testing strategies include students. Always schools if "All teachers + all students." Secondary schools if "All teachers + 13-18 year olds." Primary schools if "All teachers + 5-12 year olds."
                    if( testing_pop == "All teachers + all students" | (school_type=="SECONDARY" & testing_pop == "All teachers + 13-18 year olds") | (school_type=="PRIMARY" & testing_pop == "All teachers + 5-12 year olds")){
                      sim$p_R[d_iter] = sim$p_R[d_iter] + sum(sim[d_iter,paste0("p_I_",seq(1,15,1))])
                      sim[d_iter,paste0("p_I_",seq(1,15,1))]=0
                    }
                  }
                }
                
                # calculate how much infectiousness arises from all teachers (t_bI) and all pupils (p_bI)
                sim$t_bI[d_iter] = sum(as.vector(sim[d_iter,paste0("t_I_",seq(1,15,1))]*infectiousness_over_time))
                sim$p_bI[d_iter] = sum(as.vector(sim[d_iter,paste0("p_I_",seq(1,15,1))]*infectiousness_over_time_child))
                
                # calculate the number of teachers who end up infected by converting sum of daily rates t_bI and p_bI to probability
                ## print(paste0(rbinom(n=1,size=sim$t_S[d_iter],prob = 1-exp(-1*(sim$t_bI[d_iter]+sim$p_bI[d_iter])))," teachers infected on day ",d_iter))
                if(sim$t_S[d_iter]>0 & (sim$t_bI[d_iter] + sim$p_bI[d_iter])>0){sim$t_I1_schl[d_iter+1]=rbinom(n=1,size=sim$t_S[d_iter],prob = 1-exp(-1*(sim$t_bI[d_iter]+sim$p_bI[d_iter])))}
                
                # calculate the number of pupils  who end up infected -- this time reducing t_bI and p_bI by rel_susceptibility_child
                ## print(paste0(rbinom(n=1,size=sim$t_S[d_iter],prob = 1-exp(-1*(sim$t_bI[d_iter]+sim$p_bI[d_iter])))," pupils infected on day ",d_iter))
                if(sim$p_S[d_iter]>0 & (sim$t_bI[d_iter] + sim$p_bI[d_iter])>0){sim$p_I1_schl[d_iter+1]=rbinom(n=1,size=sim$p_S[d_iter],prob = 1-exp(-1*rel_susceptibility_child*(sim$t_bI[d_iter]+sim$p_bI[d_iter])))}
                
              }
              
              # calculate community incidence into teachers (t) and pupils (p) who are S and not about to be infected at school
              sim$t_I1_comm[d_iter+1]=rbinom(n=1,size=sim$t_S[d_iter]-sim$t_I1_schl[d_iter+1],prob = community_incidence) 
              sim$p_I1_comm[d_iter+1]=rbinom(n=1,size=sim$p_S[d_iter]-sim$p_I1_schl[d_iter+1],prob = community_incidence) 
              
              # calculate total incident cases = school transmission + community transmission 
              sim$t_I_1[d_iter+1]=sim$t_I1_schl[d_iter+1]+sim$t_I1_comm[d_iter+1]
              sim$p_I_1[d_iter+1]=sim$p_I1_schl[d_iter+1]+sim$p_I1_comm[d_iter+1]  
              
              # today's infections on days 1 thru 14 become tomorrow's day 2 thru 15
              sim[d_iter+1,paste0("t_I_",seq(2,15,1))]=sim[d_iter,paste0("t_I_",seq(1,14,1))]
              sim[d_iter+1,paste0("p_I_",seq(2,15,1))]=sim[d_iter,paste0("p_I_",seq(1,14,1))]
              
              # today's day-15 infections join tomorrow's recovered
              sim$t_R[d_iter+1] = sim$t_R[d_iter]+sim$t_I_15[d_iter]
              sim$p_R[d_iter+1] = sim$p_R[d_iter]+sim$p_I_15[d_iter]
              
              # susceptibles make up the balance of the school population
              sim$t_S[d_iter+1] = n_teachers - sim$t_E[d_iter+1] - sim$t_R[d_iter+1] - sum(sim[d_iter+1,paste0("t_I_",seq(1,15,1))])
              sim$p_S[d_iter+1] = n_pupils   - sim$p_E[d_iter+1] - sim$p_R[d_iter+1] - sum(sim[d_iter+1,paste0("p_I_",seq(1,15,1))])
            }
            
            #####################################
            ########## end simulation ###########
            #####################################
            
            agg_schl$tests_administered[testing_days] = n_teachers
            if( testing_pop == "All teachers + all students" | 
                (school_type=="SECONDARY" & testing_pop == "All teachers + 13-18 year olds") | 
                (school_type=="PRIMARY" & testing_pop == "All teachers + 5-12 year olds")){
              agg_schl$tests_administered[testing_days] = agg_schl$tests_administered[testing_days] + n_pupils
            }
            
            
            agg_schl$inc_inf_teachers = sim$t_I_1
            if(school_type=="PRIMARY"){
              agg_schl$inc_inf_pupils_PRIMARY = sim$p_I_1
              
            }
            if(school_type=="SECONDARY"){
              agg_schl$inc_inf_pupils_SECONDARY = sim$p_I_1
            }
            
            #print(paste0("Scenario ",scenario_iter,": ",school_type," school #",school_iter," had ",n_teachers-sim$t_S[90]," teachers & ", n_pupils-sim$p_S[90],"pupils infected."))
            #print(sim[seq(1,15),])
            #print(sim[seq(86,90),])
            
            ##########################################################
            ### End setup/sim/processing of each individual school ###
            ##########################################################
            
            agg_natl = agg_natl + agg_schl
            end_1_natl_sim <- Sys.time()
            
            #
            
          }
          
          print(paste0("Scenario ", scenario_iter,", rep ",replicate_number,": ",format(as.numeric(end_1_natl_sim-start_1_natl_sim,units="mins"),digits = 4)," min  -- ",
                       sum(agg_natl$tests_administered)," tests, ",
                       sum(agg_natl$inc_inf_teachers)," tchrs, ",
                       sum(agg_natl$inc_inf_pupils_PRIMARY)," 1o pupils, ",
                       sum(agg_natl$inc_inf_pupils_SECONDARY)," 2o pupils -- ",
                       testing_pop," -- ",testing_freq," -- Rt=",Rt," -- cmprv ",community_prevalence))
          
          
          if(n_replicates>1){
          reps_to_agg[,,replicate_number]=as.matrix(agg_natl) 
          }
          
        }
        
          #####################################################
          ### End loop over aggregate of all Malawi schools ###
          #####################################################
        if(n_replicates==1){write.csv(agg_natl, file=paste0("Scenario",scenario_iter,".csv"))}else{
        
        agg_mean = rowMeans(reps_to_agg, dims = 2)

        # equivalent but slower
        #agg_mean = apply(X = reps_to_agg, MARGIN = c(1,2), FUN = mean)
        
        ## Assuming normally distributed error is problematic -- causes negative incidence when sims are noisy at the single-day level
        #agg_sd = as.data.frame(apply(X = reps_to_agg, MARGIN = c(1,2), FUN = sd))
        #agg_me = qt(.975,n_replicates-1)*agg_sd/sqrt(n_replicates)
        #agg_lo95CIme = agg_mean - agg_me
        #agg_hi95CIme = agg_mean + agg_me
        
        ## Instead, will have to use non-parametric boostrapping technique to est 95% CI (albeit this is slow!)
        start_bootstrap = Sys.time()
        agg_lo95CIboot = apply(X = reps_to_agg, MARGIN = c(1,2),FUN = fn_boot_95CI_lo)
        agg_hi95CIboot = apply(X = reps_to_agg, MARGIN = c(1,2),FUN = fn_boot_95CI_hi)

        print(paste0("Scenario ", scenario_iter,": ",format(as.numeric(Sys.time()-start_bootstrap,units="mins"),digits = 4)," min spent obtaining booststrap CIs"))

        results_data_columns = c('tests_95lo','tests_mean','tests_95hi',
                                 'pupilsPRIMARY_95lo','pupilsPRIMARY-mean','pupilsPRIMARY_95hi',
                                 'pupilsSECONDARY_95lo','pupilsSECONDARY_mean','pupilsSECONDARY_95hi',
                                 'teachers_95lo','teachers_mean','teachers_hi')
        
        results_with_CI = setNames(data.frame(matrix(0, ncol = length(results_data_columns), nrow = n_days_to_simulate)), results_data_columns)
        curr_column=0
        for(output_unit_iter in seq(1,4,1)){
          for(output_err_iter in seq(1,3,1)){
            
            curr_column=curr_column+1
            curr_data_unit = agg_data_columns[output_unit_iter]
            if(output_err_iter==1){results_with_CI[,curr_column] = agg_lo95CIboot[,output_unit_iter]}
            if(output_err_iter==2){results_with_CI[,curr_column] = agg_mean[,      output_unit_iter]}
            if(output_err_iter==3){results_with_CI[,curr_column] = agg_hi95CIboot[,output_unit_iter]}
          }
        }
        
        write.csv(results_with_CI, file=paste0("Scenario",scenario_iter,".csv"))
        }
        
        }}}}

print(print(paste0("Entire script took ",format(as.numeric(Sys.time()-start_script,units="mins"),digits = 4)," minutes."))
)
##########################################
### End loop over 72 testing scenarios ###
##########################################

