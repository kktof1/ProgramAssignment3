rankall <- function(outcome, num="best") {
        ret_Value = NULL
        
        ## Read outcome data
        read_csv <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        
        ## Check that state and outcome are valid
        if(outcome == "heart attack") {
                t_io <- read_csv[, c("Hospital.Name", "State",
                                     "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack")]
        } else if (outcome == "heart failure") {
                t_io <- read_csv[, c("Hospital.Name", "State",
                                     "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure")]
        } else if (outcome == "pneumonia") {
                t_io <- read_csv[, c("Hospital.Name", "State",
                                     "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")]
        } else {
                stop("invalid outcome")
        }
        
        colnames(t_io) = c("hospital", "state", "Factor1")
        t_io$Factor1 <- as.numeric(t_io$Factor1)
        t_io <- t_io[c(order(t_io$Factor1, t_io$hospital)),]
        
        i_state <- t_io[,"state"]
        i_state <- unique(i_state)
        i_state <- i_state[c(order(i_state))]

        
        ## For each state, find the hospital of the given rank
        # df_hospital <- c(rep("",times=length(i_state)))
        # df_state <- i_state
        # df_factor <- c(rep(0, times=length(i_state)))
        # r_value <- data.frame(hospital = df_hospital, state = df_state, factor = df_factor)
        r_value1 = NULL
        r_value2 = NULL
        if (num == "best") {
                j = 1;
                for (i in i_state) {
                        interested_only <- t_io[t_io$state == i, ]
                        # r_value[j, ] <- interested_only[1, ]
                        r_value1[j] <- interested_only[1, 1]
                        r_value2[j] <- interested_only[1, 2]
                        j <- j+1
                }
        } else if (num == "worst") {
                j = 1;
                for (i in i_state) {
                        interested_only <- t_io[t_io$state == i, ]
                        nlast <- nrow(interested_only[!is.na(interested_only$Factor1),])
                        # r_value[j, ] <- interested_only[nrow(!is.na(interested_only$Factor1))]
                        r_value1[j] <- interested_only[nlast, 1]
                        r_value2[j] <- interested_only[nlast, 2]
                        j <- j+1
                        
                        # interested_only[!is.na(interested_only$HeartAttack),]
                }
        } else {
                j = 1;
                for (i in i_state) {
                        interested_only <- t_io[t_io$state == i, ]
                        if(num > nrow(interested_only)) {
                                # r_value[j, 1] <- NA
                                # r_value[j, 2] <- i
                                r_value1[j] <- NA
                                r_value2[j] <- i
                        } else {
                                # r_value[j, ] <- interested_only[num, ]
                                r_value1[j] <- interested_only[num, 1]
                                r_value2[j] <- interested_only[num, 2]
                        }
                        j <- j+1
                }
        }
        
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        ret_value <- data.frame(hospital = r_value1, state = r_value2)
        ret_value
        
        #\heart attack", \heart failure", or \pneumonia".
        
        #11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: Lists the 
        #risk adjusted rate (percentage) for each hospital.
        
        #17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: Lists the 
        #risk adjusted rate (percentage) for each hospital.
        
        #23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: Lists the 
        #risk adjusted rate (percentage) for each hospital.
}