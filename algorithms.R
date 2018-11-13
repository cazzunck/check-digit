library(plyr)
library(dplyr)
library(stringr)
library(lubridate)
library(mice)
library(ggplot2)
library(tidyverse)


# Sumamry of all countries.  Includes some strange unconventional ones.  
#https://docs.oracle.com/cd/E18727_01/doc.121/e13483/T359831T498954.htm
#=====================================================
# FUNCTIONS
#=====================================================


add_digit<-function(x){
  z<-as.character(x)
  a<-as.data.frame(strsplit(z, ""))
  names(a)<-c("pos1")
  len<-dim(a)[1]
  
  sum=0
  for (i in 1:len){
    sum=sum+(as.numeric(as.character(a$pos1[i])))
  }
  return(sum)
}

luhn<-function(x){
  
  mod=10
  #https://wiki.openmrs.org/display/docs/Check+Digit+Algorithm
  a<-as.data.frame(strsplit(x, ""))
  names(a)<-c("pos1")
  
  #odd numbers
  #
  b=1
  a1<-add_digit(b*(as.numeric(as.character(a$pos1[1]))))
  a2<-add_digit(b*as.numeric(as.character(a$pos1[3])))
  a3<-add_digit(b*as.numeric(as.character(a$pos1[5])))
  # even numbers
  #
  c=2
  a4<-add_digit(c*(as.numeric(as.character(a$pos1[2]))))
  a5<-add_digit(c*as.numeric(as.character(a$pos1[4])))
  a6<-add_digit(c*as.numeric(as.character(a$pos1[6])))
  
  d1<-((a1+a2+a3+a4+a5+a6) %% mod)
  
  if (d1 > 0) {
    c1<-(mod-d1)
  } else{
    c1 = 0  
  }
  return(c1)
}

mod10<-function(x){
  
  mod=10
  a<-as.data.frame(strsplit(x, ""))
  names(a)<-c("pos1")
  
  #odd numbers
  #
  b=2
  a1<-add_digit(b*(as.numeric(as.character(a$pos1[1]))))
  a2<-add_digit(b*as.numeric(as.character(a$pos1[3])))
  a3<-add_digit(b*as.numeric(as.character(a$pos1[5])))
  # even numbers
  #
  c=1
  a4<-add_digit(c*(as.numeric(as.character(a$pos1[2]))))
  a5<-add_digit(c*as.numeric(as.character(a$pos1[4])))
  a6<-add_digit(c*as.numeric(as.character(a$pos1[6])))
  
  y<-as.character(a1+a2+a3+a4+a5+a6)
  y1<-as.data.frame(strsplit(y, ""))
  names(y1)<-c("pos1")
  d1<-mod - (as.numeric(as.character(y1$pos1[2])))
  
  return(d1)
}


calc_check_digit<-function(x,b){
  a<-as.data.frame(strsplit(x, ""))
  names(a)<-c("pos1")
  
  a1<-b*(as.numeric(as.character(a$pos1[1]))+as.numeric(as.character(a$pos1[3]))+as.numeric(as.character(a$pos1[5])))
  b1<-as.numeric(as.character(a$pos1[2]))+as.numeric(as.character(a$pos1[4]))+as.numeric(as.character(a$pos1[6]))
  d1=(a1+b1) %% 10
  if (d1 > 0) {
    c1<- (10-d1)
  } else{
    c1 = 0  
  }
  return(c1)
}

calc_check_digit_odd_even<-function(x,b,c){
  a<-as.data.frame(strsplit(x, ""))
  names(a)<-c("pos1")
  
  #odd numbers
  #
  a1<-b*(as.numeric(as.character(a$pos1[1]))+as.numeric(as.character(a$pos1[3]))+as.numeric(as.character(a$pos1[5])))
  # even numbers
  #
  b1<-(add_digit(c*as.numeric(as.character(a$pos1[2])))+add_digit(c*as.numeric(as.character(a$pos1[4])))+add_digit(c*as.numeric(as.character(a$pos1[6]))))
  
  d1<- (a1+b1) %% 10
  if (d1 > 0) {
    c1<- (10-d1)
  } else{
    c1 = 0  
  }
  return(c1)
}


calc_check_digit_odd_even_general<-function(x,c,y){
  a<-as.data.frame(strsplit(x, ""))
  names(a)<-c("pos1")
  # c is the factor multiplying either the odd or even row
  # y indicates whether c applied to odd or even
  
  #odd numbers
  #
  if( y == 0){
    a1<-(as.numeric(as.character(a$pos1[1]))+as.numeric(as.character(a$pos1[3]))+as.numeric(as.character(a$pos1[5]))) 
    # even numbers
    b1<-add_digit(c*as.numeric(as.character(a$pos1[2])))+add_digit(c*as.numeric(as.character(a$pos1[4])))+add_digit(c*as.numeric(as.character(a$pos1[6])))
  } else{
    a1<-(as.numeric(as.character(a$pos1[2]))+as.numeric(as.character(a$pos1[4]))+as.numeric(as.character(a$pos1[6]))) 
    # even numbers
    b1<-add_digit(c*as.numeric(as.character(a$pos1[1])))+add_digit(c*as.numeric(as.character(a$pos1[3])))+add_digit(c*as.numeric(as.character(a$pos1[5])))
    
  }
  
  d1<- (a1+b1) %% 10
  if (d1 > 0) {
    c1<- (10-d1)
  } else{
    c1 = 0  
  }
  return(d1)
}




calc_check_digit2<-function(x,y,z,q){
  a<-as.data.frame(strsplit(x, ""))
  names(a)<-c("pos1")
  
  a1<-y*(as.numeric(as.character(a$pos1[1]))+as.numeric(as.character(a$pos1[4])))
  b1<-z*(as.numeric(as.character(a$pos1[2]))+as.numeric(as.character(a$pos1[5])))
  c1<-q*(as.numeric(as.character(a$pos1[3]))+as.numeric(as.character(a$pos1[6])))
  
  d1<- (a1+b1+c1) %% 10
  if (d1 > 0) {
    e1<- (10-d1)
  } else{
    e1 = 0  
  }
  
  return(e1)
}

calc_check_digit3<-function(x){
  # checked
  a<-as.data.frame(strsplit(x, ""))
  names(a)<-c("pos1")
  
  a1<-7*(as.numeric(as.character(a$pos1[1]))+6*as.numeric(as.character(a$pos1[2])))+5*as.numeric(as.character(a$pos1[3]))+4*as.numeric(as.character(a$pos1[4]))+3*as.numeric(as.character(a$pos1[5]))+2*as.numeric(as.character(a$pos1[6]))
  d1<- (a1) %% 11
  if (d1 > 0) {
    c1<- (11-d1)
  } else{
    c1 = 0  
  }
  
  return(c1)
}

#Generalized
calc_check_digit_general<-function(x,z,modulous, add){
  fudge_factor = 0
  b<-as.data.frame(strsplit(x, ""))
  names(b)<-c("pos1")
  
  if (add == 1){
    a1<-add_digit(z[1]*as.numeric(as.character(b$pos1[1])))
    a2<-add_digit(z[2]*as.numeric(as.character(b$pos1[2])))
    a3<-add_digit(z[3]*as.numeric(as.character(b$pos1[3])))
    a4<-add_digit(z[4]*as.numeric(as.character(b$pos1[4])))
    a5<-add_digit(z[5]*as.numeric(as.character(b$pos1[5])))
    a6<-add_digit(z[6]*as.numeric(as.character(b$pos1[6])))
    a7<-add_digit(z[7]*as.numeric(as.character(b$pos1[7])))
  } else{
    a1<-z[1]*as.numeric(as.character(b$pos1[1]))
    a2<-z[2]*as.numeric(as.character(b$pos1[2]))
    a3<-z[3]*as.numeric(as.character(b$pos1[3]))
    a4<-z[4]*as.numeric(as.character(b$pos1[4]))
    a5<-z[5]*as.numeric(as.character(b$pos1[5]))
    a6<-z[6]*as.numeric(as.character(b$pos1[6])) 
    a7<-z[7]*as.numeric(as.character(b$pos1[7])) 
  }
  
  
  d1<-(a1+a2+a3+a4+a5+a6+a7) %% modulous
  return(d1)
  
}

