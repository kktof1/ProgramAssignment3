best <- function(state, outcome) {
        ret_Value = NULL
        
        ## Read outcome data
        read_csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        if (length(state) > 2) {
                stop("invalid state")
        } else {
                if (length(grep(state, read_csv$State)) == 0 ) {
                        stop("invalid state")
                }        
        }
        
        ## Check that state and outcome are valid
        state_only <- read_csv[read_csv$State == state, ]
        #state_only <- read_csv[read_csv$State == "TX", ]
        #head(state_only)
        
        #t_io == temporary_interested_only
        t_io <- state_only[, 
                           c("Hospital.Name",
                             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
                             "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
                             "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
        #head(t_io$Hospital.Name)
                
        colnames(t_io) = c("Name", "HeartAttack", "HeartFailure", "Pneumonia")
        t_io$HeartAttack <- as.numeric(t_io$HeartAttack)
        t_io$HeartFailure <- as.numeric(t_io$HeartFailure)
        t_io$Pneumonia <- as.numeric(t_io$Pneumonia)
        
        if(outcome == "heart attack") {
                interested_only <- t_io[c(order(t_io$HeartAttack, t_io$Name)),]
                #interested_only <- t_io[c(order(t_io$HeartAttack)),]
                
        } else if (outcome == "heart failure") {
                interested_only <- t_io[c(order(t_io$HeartFailure, t_io$Name)),]
        } else if (outcome == "pneumonia") {
                interested_only <- t_io[c(order(t_io$Pneumonia, t_io$Name)),]
        } else {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with lowest 30-day death
        ## rate
        ret_Value <- interested_only[1, "Name"]
        ret_Value
        
        #\heart attack", \heart failure", or \pneumonia".
        
        #11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the 
        #risk adjusted rate (percentage) for each hospital.
        
        #17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the 
        #risk adjusted rate (percentage) for each hospital.
        
        #23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the 
        #risk adjusted rate (percentage) for each hospital.
}