outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])

best <- function(state, outcome) {
        ## Read outcome data
        outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        outcome_state_only <- outcome[outcome$State == state, ]
        outcome_state_only <- outcome[outcome$State == "CA", ]
        
        head(outcome_state_only)
        
        outcome_interested_only <- outcome_state_only[, 
                c(outcome_state_only$Hospital.30.Day.Readmission.Rates.from.Heart.Attack,
                  outcome_state_only$Hospital.30.Day.Readmission.Rates.from.Heart.Failure,
                  outcome_state_only$Hospital.30.Day.Readmission.Rates.from.Pneumonia)]
        
        outcome_interested_only <- outcome_state_only[,
                "Hospital.30.Day.Readmission.Rates.from.Heart.Attack"]
        
        #oio == outcome_interested_only
        oio <- outcome_state_only[, 
              c("Hospital.Name",
                "Hospital.30.Day.Readmission.Rates.from.Heart.Attack",
                "Hospital.30.Day.Readmission.Rates.from.Heart.Failure",
                "Hospital.30.Day.Readmission.Rates.from.Pneumonia")]
        
        colnames(oio) = c("Name", "HeartAttack", "HeartFailure", "Pneumonia")
        
        outcome_interested_only <- oio[c(order(oio$"HeartAttack", oio$"Name"))]
                  
        outcome_interested_only <- oio[c(order(oio$HeartAttack, oio$Name)),]
        
        ## Return hospital name in that state with lowest 30-day death
        #\heart attack", \heart failure", or \pneumonia".
        
        #11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the 
        #risk adjusted rate (percentage) for each hospital.
        
        #17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the 
        #risk adjusted rate (percentage) for each hospital.
        
        #23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the 
        #risk adjusted rate (percentage) for each hospital.
        
        ## rate
}