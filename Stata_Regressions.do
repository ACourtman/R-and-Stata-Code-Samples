clear
capture log close
set more off


*** Data file included in the repository. Save to your device and set own global home_dir and global data_dir

global home_dir "G:\My Drive\School\Thesis"
global data_dir "$home_dir\Data"

use "G:\My Drive\School\Thesis\Data\MasterListFinal.dta"

local controls "q2_age q14_socioeconomic_status"


* Initial Regression with temperature treatment binary variable (= if mean session temp >= 30C, = 0 if otherwise) and location, gender fixed effects, clustered at site_session level

reg Altruism treatment IND USA KEN male `controls', cluster(site_session)



* Adding emotional trigger from lost competition to regression, clustered at site_session level

reg Altruism treatment IND USA KEN male trigger `controls', cluster(site_session)



* Adding interaction of temperature treatment effect and trigger, clustered at site_session level

reg Altruism treatment IND USA KEN male trigger TreatmentTrigger `controls', cluster(site_session)



* Adding interaction of temperature treatment effect and gender, clustered at site_session level

reg Altruism treatment IND USA KEN male trigger TreatmentTrigger TreatmentMale `controls', cluster(site_session)



* Adding 3 way interaction of temperature treatment effect, trigger and gender, clustered at site_session level

reg Altruism treatment IND USA KEN male trigger TreatmentTrigger TreatmentMale TreatmentTriggerMale `controls', cluster(site_session)




* Initial Regression with continuous temperature measure and location, gender fixed effects, clustered at site_session level

reg Altruism temp IND USA KEN male `controls', cluster(site_session)



* Adding emotional trigger from lost competition to regression, clustered at site_session level

reg Altruism temp IND USA KEN male trigger `controls', cluster(site_session)



* Adding interaction of temperature and trigger, clustered at site_session level

reg Altruism temp IND USA KEN male trigger TempTrigger `controls', cluster(site_session)



* Adding interaction of temperature and gender, clustered at site_session level

reg Altruism temp IND USA KEN male trigger TempTrigger TempMale `controls', cluster(site_session)



* Adding 3 way interaction of temperature, trigger and gender, clustered at site_session level

reg Altruism temp IND USA KEN male trigger TempTrigger TempMale TempTriggerMale `controls', cluster(site_session)