library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(mice)
library(ggplot2)
library(tidyverse)
library(foreach)
library(doParallel)
library(rvest)
library(magrittr)



number_of_cores  = 7


(cl <- number_of_cores %>%  makeCluster) %>% registerDoParallel

clusterEvalQ(cl, {
  source("~/Home/DataScience/check-digit/algorithms.R")
  library(stringr)
})


returnComputation <-
  foreach(x=1:number_of_cores) %dopar%{
    library(stringr)
    a<-'

    

setwd("~/Home/DataScience/check-digit/")
source("algorithms.R")
fileConn<-file(paste0("output_", x, ".txt"))


#========================================================
# Data
    #========================================================
    
    #account.numbers <- read.table("~/Home/DataScience/check-digit/account-numbers.csv", comment.char="")
    account.numbers <- read.csv("~/Home/DataScience/check-digit/account-numbers-verified.csv", sep="")
    names(account.numbers)<-c("account")
    #account.numbers$account<-str_replace(account.numbers$account, "-","")
    
    account.numbers$acc_number<-substr(account.numbers$account, 1, 6) %>% as.character()
    account.numbers$check_digit<-substr(account.numbers$account, 7, 7) %>% as.character()

    #========================================================

limit.1<-1
limit<-10
mod_min = 9
mod_max = 9
ddim<-dim(account.numbers)[1]
k<-vector()

# number of cores
cores = y
number_of_core = x

num_per_core  = floor(1.*(limit-limit.1)/cores)
start = num_per_core*(number_of_core-1)+limit.1
end = (num_per_core)*(number_of_core)+limit.1
if (number_of_core == cores){
end = limit
}

    
    for (i1 in start:end){
    k[1]<-i1
    
    for (i2 in limit.1:limit){
    k[2]<-i2
    
    for (i3 in limit.1:limit){
    k[3]<-i3
    
    for (i4 in limit.1:limit){
    k[4]<-i4
    
    for (i5 in limit.1:limit){
    k[5]<-i5
    
    for (i6 in limit.1:limit){
    k[6]<-i6
  
      for (i7 in 1:limit){
        k[7]<-i7 
        
        
        # Checking mod number
        for (m in mod_min:mod_max){
          
          
          
          
          
          # Applying add_digit algorithm
          for (c in 0:1){
            
            i=1

            print(k)
            match<-TRUE

            # select account number at random
            rand_num<-sample(1:ddim, ddim, replace=FALSE) 
            
            
            while(match){ 
              
              digit<-calc_check_digit_general(as.character(account.numbers$account[rand_num[i]]),k, m, c)

              if (digit == 0 & (length(k[which(k==m)]) !=7)){
                match<-TRUE
                i=i+1
              } else{
                match<-FALSE
              }
              
              if(i > 0.99*dim(account.numbers)[1]){
               writeLines(c("Found",paste0("number of correct=", i), paste0("k=", k), paste0("c=", c), paste0("mod=", m)), fileConn)
               close(fileConn)
                stop("check")
              }
            }
            
          }
        }
      }
    }
    }
    }

    }
    }
    }


 writeLines(c("No solution found :("), fileConn)
               close(fileConn)

'
    
    
    b<-str_replace(a,"number_of_core = x", paste0("number_of_core =",x))
    c<-str_replace(b,"cores = y", paste0("cores =",number_of_cores))
    d<-str_replace(c,", x,", paste0(", ",x,","))
    #print(someList[[x]])
    eval(parse(text = d))
    #source(someList[[x]])
  }



clusterEvalQ(cl, {
})


#length(someList)
stopImplicitCluster()











#========================================================
# testing with no guarantee of correctness
#========================================================
limit<-9
mod_min = 9
mod_max = 11
ddim<-dim(account.numbers)[1]
k<-vector()

for (i1 in 1:limit){
  k[1]<-i1
  
  for (i2 in 1:limit){
    k[2]<-i2
    
    for (i3 in 1:limit){
      k[3]<-i3
      
      for (i4 in 1:limit){
        k[4]<-i4
        
        for (i5 in 1:limit){
          k[5]<-i5
          
          for (i6 in 1:limit){
            k[6]<-i6
            
            # the factor multiplying the check digit can't be the same as the mod!
            #
            for (i7 in 1:limit){
              k[7]<-i7 
              
              
              # Checking mod number
              for (m in mod_min:mod_max){
                
                
                
                
                
                # Applying add_digit algorithm
                for (c in 0:1){
                  
                  g=0
                  cat("mod", m, "\n")
                  cat("add digit", c, "\n")
                  # cat("Subtract from mod", e, "\n")
                  print(k)
                  
                  for (i in 1:dim(account.numbers)[1]){
                    
                    digit<-calc_check_digit_general(account.numbers$account[i],k, m, c)
                    #print(digit)
                    #print(as.numeric(account.numbers$check_digit[i]))
                    # now checking to see whether it is zero
                    if (digit == 0){
                      
                      #print(i)
                      #print(digit)
                      g=g+1
                    } 
                    
                    if((g > 0.75*i) & i > 10){
                      cat("number correct, ", g, "\n")
                      cat("number so far", i, "\n")
                      
                    }
                    
                    
                  }
                  
                  if(g > 0.75*dim(account.numbers)[1]){
                    cat("number correct, ", g, "\n")
                    cat("factors", k, "\n")
                    stop("check")
                    
                  }
                  
                }
                
                
              }
              
            }
          }
        }
      }
    }
  }
  
}


cat("total correct at end, ", g, "\n")
cat("percentage", (g/i), "\n")




#========================================================
# Checks
#========================================================

# 1.)

b=3 # the number to multiply odd numbers

for (i in 1:dim(account.numbers)[1]){
  digit<-calc_check_digit(account.numbers$acc_number[i],b)
  #print(digit)
  #print(as.numeric(account.numbers$check_digit[i]))
  if (digit == as.numeric(account.numbers$check_digit[i])){
    account.numbers$match[i] = 1
  } else{
    account.numbers$match[i] = 0
  }
  
}

account.numbers %>% summarise(matchrate=mean(match))

#==========================================
# 2.  Canadian Social insurance number
#==========================================
# https://stackoverflow.com/questions/4263398/how-do-i-check-the-validity-of-the-canadian-social-insurance-number-in-c
a = 1
b = 2
k=0
for (i in 1:dim(account.numbers)[1]){
  print(i)
  digit<-calc_check_digit_odd_even(account.numbers$acc_number[i],a,b)
  if (digit == as.numeric(account.numbers$check_digit[i])){
    match<-TRUE
    k=k+1
  } else{
    match<-FALSE
  }
}

print(k/dim(account.numbers)[1])

#==========================================
# 5. Generalized odd even
#==========================================
limit<-9

for (i in 1:limit){
  a<-i
  
  for (j in 1:2){
    b<-(j-1)
    
    
    g=0
    cat("factor", a, "\n")
    cat("odd/even", b, "\n")
    
    
    for (i in 1:dim(account.numbers)[1]){
      
      digit<-calc_check_digit_odd_even_general(account.numbers$account[i],a,b)
      #print(digit)
      #print(as.numeric(account.numbers$check_digit[i]))
      # now checking to see whether it is zero
      if (digit == account.numbers$check_digit[i]){
        
        #print(i)
        #print(digit)
        g=g+1
      } 
      
      if((g > 0.75*i) & i > 10){
        cat("number correct, ", g, "\n")
        cat("number so far", i, "\n")
        
      }
      
      
    }      
    
    print(g/dim(account.numbers)[1])
    
    
    
  }
}











#==========================================
# 3. MOD 10 Check Digit
#==========================================
# http://health.gov.on.ca/english/providers/pub/ohip/tech_specific/pdf/5_13.pdf

for (i in 1:dim(account.numbers)[1]){
  digit<-mod10(account.numbers$acc_number[i])
  #print(digit)
  #print(as.numeric(account.numbers$check_digit[i]))
  if (digit == as.numeric(account.numbers$check_digit[i])){
    account.numbers$match[i] = 1
  } else{
    account.numbers$match[i] = 0
  }
  
}

account.numbers %>% summarise(matchrate=mean(match))



#==========================================
# 4. Modulo-11 weighted check digit calculation
#==========================================
# http://www.pgrocer.net/Cis51/mod11.html

for (i in 1:dim(account.numbers)[1]){
  digit<-calc_check_digit3(account.numbers$acc_number[i])
  #print(digit)
  #print(as.numeric(account.numbers$check_digit[i]))
  if (digit == as.numeric(account.numbers$check_digit[i])){
    account.numbers$match[i] = 1
  } else{
    account.numbers$match[i] = 0
  }
  
}

account.numbers %>% summarise(matchrate=mean(match))

#==========================================
# 5. Generalized
#==========================================
# https://www.wikihow.com/Calculate-the-Check-Digit-of-a-Routing-Number-from-an-Illegible-Check
# https://www.codeproject.com/Articles/459507/Identification-numbers-and-check-digit-algorithms
# for all countries
#
# https://docs.oracle.com/cd/E18727_01/doc.121/e13483/T359831T498954.htm


# General mathematical traits of check digit algorithms are:
# 1. multiply each number by a factor (can be unique or repeated)
# 2. Add any digits which have become double digits
# 3. Add all together
# 4. take mod 10 or 11
# 5. Possibly subtract from the mod to get the remainder
# What if the factor multiplying the check digit is not 1? 


# Make these limits for now

#factors,  9 9 9 9 9 1 8 
limit<-9
mod_min = 9
mod_max = 11
ddim<-dim(account.numbers)[1]
k<-vector()

for (i1 in 1:limit){
  k[1]<-i1
  
  for (i2 in 1:limit){
    k[2]<-i2
    
    for (i3 in 1:limit){
      k[3]<-i3
      
      for (i4 in 1:limit){
        k[4]<-i4
        
        for (i5 in 1:limit){
          k[5]<-i5
          
          for (i6 in 1:limit){
            k[6]<-i6
            
            # the factor multiplying the check digit can't be the same as the mod!
            #
            for (i7 in 1:(mod_min-1)){
              k[7]<-i7 
              
              
              # Checking mod number
              for (m in mod_min:mod_max){
                
                
                
                
                
                # Applying add_digit algorithm
                for (c in 0:1){
                  
                  i=1
                  cat("mod", m, "\n")
                  cat("add digit", c, "\n")
                  # cat("Subtract from mod", e, "\n")
                  print(k)
                  match<-TRUE
                  # select account number at random
                  rand_num<-sample(1:ddim, ddim, replace=FALSE) 
                  
                  
                  while(match){ # dim(account.numbers)[1]){
                    
                    digit<-calc_check_digit_general(account.numbers$account[rand_num[i]],k, m, c)
                    #print(digit)
                    #print(as.numeric(account.numbers$check_digit[i]))
                    # now checking to see whether it is zero
                    if (digit == 0){
                      match<-TRUE
                      print(i)
                      print(digit)
                      i=i+1
                    } else{
                      match<-FALSE
                    }
                    
                    if(i > 0.75*dim(account.numbers)[1]){
                      cat("factors, ", k, "\n")
                      cat("number so far", i, "\n")
                      stop("check")
                    }
                  }
                  
                }
              }
            }
          }
        }
      }
      
    }
  }
}

#==========================================
# 6. LUHN
#==========================================

k=0
for (i in 1:dim(account.numbers)[1]){
  #print(i)
  digit<-luhn(account.numbers$acc_number[i])
  if (digit == as.numeric(account.numbers$check_digit[i])){
    match<-TRUE
    k=k+1
  } else{
    match<-FALSE
  }
}

print(k/dim(account.numbers)[1])














#==========================================
# Modulus 11
#==========================================
sims = 10000
for (j in 1:sims){
  
  k<-sample(1:9, 3, replace=FALSE) 
  y<-k[1]
  z<-k[2]
  q<-k[3]
  
  for (i in 1:dim(account.numbers)[1]){
    
    digit<-calc_check_digit2(account.numbers$acc_number[i],y,z,q)
    #print(digit)
    #print(as.numeric(account.numbers$check_digit[i]))
    if (digit == as.numeric(account.numbers$check_digit[i])){
      account.numbers$match2[i] = 1
    } else{
      account.numbers$match2[i] = 0
    }
    
  }
  print(k)
  rate<-account.numbers %>% summarise(matchrate=mean(match2))
  print(rate)
  if(rate$matchrate > 0.8){
    stop("check")
  }
}
