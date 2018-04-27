

#Appointments

#Aim: To look at the difference between eNoting outcomes for intended appointment date and the actual follow up date.
#Each reporting function gives the aggregate table, a ggplot and the actual patients so they can be looked up. It should also provide the table by clinic code


#Get the appointments for a specific clinic code over time from the Business Objects report
library(readxl)
library(dplyr)
library(EndoMineR)
#All the appts for the month specified in business objects
Appts<-read_excel("S:\\Gastroenterology\\Seb\\R\\Data\\Admin\\BO_OP17P_For_RetroSpecAppts.xlsx")



#' ApptCleaner
#' 
#' This function gets all the appointment dates cleaned
#' 
#' @param x the dataframe
#' @keywords Appointment cleaning
#' @export
#' @examples ApptCleaner(df)
#'
ApptCleaner<-function(x){
  names(Appts)<-gsub(" ","",names(Appts))
  Appts$AppointmentDate<-as.Date(Appts$AppointmentDate, origin="1899-12-30")
  Appts$ApptRequestReceivedDate<-as.Date(as.numeric(Appts$ApptRequestReceivedDate), origin="1899-12-30")
  return(Appts)
}


#Select the service codes you are interested in:
target <- c("7R3", "7N4")
MyClinicsOfInterest<-df[df$ClinicCode %in% target, ] 


#' ServiceAppts
#' 
#' This function filters which appointments you want by code (Service or speciality) and by new or follow up
#' 
#' @param x the dataframe
#' @param Column the dataframe
#' @param Code the dataframe
#' @keywords Appointment filtering
#' @export
#' @examples ServiceAppts(My,"ClinicCode","7N4","NeworFollowUpAppointment","New")
#'
ServiceAppts<-function(x,Column,Code,NeworFollowUpAppointmentCol,NeworFollowUpAppointment){
  x<-data.frame(x)
x<-x[grepl(Code,x[,Column]),]
x<-x[grepl(NeworFollowUpAppointment,x[,NeworFollowUpAppointmentCol]),]
  return(x)
}

AggregateAppts<-HowManyTests(My2,"ClinicCode","AppointmentDate","7N4")



#' ClinicOutcome
#' 
#' This function gives the clinic outcomes on a per clinic code basis and also provides a graphical outcome as well as a list
#' 
#' @param x the dataframe
#' @param Column the dataframe
#' @param Code the dataframe
#' @keywords Appointment filtering
#' @importFrom magrittr '%>%'
#' @importFrom dplyr arrange group_by summarise
#' @import rlang
#' @export
#' @examples ClinicOutcome(df,"AppointmentOutcomeName")
#'

ClinicOutcome<-function(x,AppointmentOutcomeName){
  
  AppointmentOutcomeNamea <- rlang::sym(AppointmentOutcomeName)
  #Group with dplyr to get aggregate table
  Outcomes<-df%>% group_by(!!AppointmentOutcomeNamea)%>% summarise(n=n())  
  Outcomes<-data.frame(Outcomes)
  
  #Get the ggplot output
  Myplot <- ggplot(data = Outcomes, aes(x = AppointmentOutcomeName, y = n)) + geom_point() +
    theme_bw()+
    xlab("Outcome") + 
    ylab("Number") +
    theme(axis.text.x=element_text(angle=-90)) +
    theme(legend.position="top")
  functionResults <- list(Myplot = Myplot, TestNumbers = Outcomes)
  return(functionResults)
}
