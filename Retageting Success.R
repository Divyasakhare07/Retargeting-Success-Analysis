# Reading abandoned and Reservation datasets
library(readr)
library(readxl)
Abandoned <- read_excel("C:/Users/divya/OneDrive/Desktop/Study/QMB/Midterm_Project/Abandoned.xlsx")
Reservation <- read_excel("C:/Users/divya/OneDrive/Desktop/Study/QMB/Midterm_Project/Reservation.xlsx")
View(Abandoned)
# Task 1.2
#Retrieving the size of Abandoned and Reservation
Abandoned_count<-table(Abandoned$Test_Control);Abandoned_count
Reservation_count<-table(Reservation$Test_Control);Reservation_count

# Creating column for Test and Control for Abandoned
Abandoned$Test <- as.numeric(Abandoned$Test_Control=='test')
Abandoned$Control <- as.numeric(Abandoned$Test_Control=='control')

# Creating column for Test and Control for Reservation
Reservation$Test <- as.numeric(Reservation$Test_Control=='test')
Reservation$Control <- as.numeric(Reservation$Test_Control=='control')

#Filtering both data which has state value
Abandoned_state<-subset(Abandoned, Address!='NA')
Reservation_state<-subset(Reservation, Address!='NA')

A_states<-unique(Abandoned_state$Address);A_states
R_states<-unique(Reservation_state$Address);R_states

# Task 1.3
#Computing summary statistics for Abandoned and Reservation for state HI
Abandoned_HI<-subset(Abandoned_state,Address == "HI")
summary(Abandoned_HI$Test)

Reservation_HI<-subset(Reservation_state,Address == "HI")
summary(Reservation_HI$Test)

#Computing summary statistics for Abandoned and Reservation for state NY
Abandoned_NY<-subset(Abandoned_state,Address == "NY")
summary(Abandoned_NY$Test)

Reservation_NY<-subset(Reservation_state,Address == "NY")
summary(Reservation_NY$Test)

#Computing summary statistics for Abandoned and Reservation for state GA
Abandoned_GA<-subset(Abandoned_state,Address == "GA")
summary(Abandoned_GA$Test)

Reservation_GA<-subset(Reservation_state,Address == "GA")
summary(Reservation_GA$Test)

#Computing summary statistics for Abandoned and Reservation for state AZ
Abandoned_AZ<-subset(Abandoned_state,Address == "AZ")
summary(Abandoned_AZ$Test)

Reservation_AZ<-subset(Reservation_state,Address == "AZ")
summary(Reservation_AZ$Test)

#Computing summary statistics for Abandoned and Reservation for state DE
Abandoned_DE<-subset(Abandoned_state,Address == "DE")
summary(Abandoned_DE$Test)

Reservation_DE<-subset(Reservation_state,Address == "DE")
summary(Reservation_DE$Test)

# Task 2.4
#Match data on Email
matched_email=Abandoned$Email[complete.cases(Abandoned$Email)] %in% Reservation$Email[complete.cases(Reservation$Email)]
Abandoned$matched_email <- 0
Abandoned$matched_email[complete.cases(Abandoned$Email)] <- 1* matched_email
sum(Abandoned$matched_email) 

#Match data on Incoming Phone
matched_Incoming_Phone=Abandoned$Incoming_Phone[complete.cases(Abandoned$Incoming_Phone)] %in% Reservation$Incoming_Phone[complete.cases(Reservation$Incoming_Phone)]
Abandoned$matched_Incoming_Phone <- 0
Abandoned$matched_Incoming_Phone[complete.cases(Abandoned$Incoming_Phone)] <- 1* matched_Incoming_Phone
sum(Abandoned$matched_Incoming_Phone) 

#Match data On Contact Phone
matched_Contact_Phone=Abandoned$Contact_Phone[complete.cases(Abandoned$Contact_Phone)] %in% Reservation$Contact_Phone[complete.cases(Reservation$Contact_Phone)]
Abandoned$matched_Contact_Phone <- 0
Abandoned$matched_Contact_Phone[complete.cases(Abandoned$Contact_Phone)] <- 1* matched_Contact_Phone
sum(Abandoned$matched_Contact_Phone) 

#Match data On Contact Phone and incoming phone
matched_Contact_incoming=Abandoned$Contact_Phone[complete.cases(Abandoned$Contact_Phone)] %in% Reservation$Incoming_Phone[complete.cases(Reservation$Incoming_Phone)]
Abandoned$matched_Contact_incoming <- 0
Abandoned$matched_Contact_incoming[complete.cases(Abandoned$Contact_Phone)] <- 1* matched_Contact_incoming
sum(Abandoned$matched_Contact_incoming) 

#Match data On incoming phone and Contact Phone 
matched_incoming_Contact=Abandoned$Incoming_Phone[complete.cases(Abandoned$Incoming_Phone)] %in% Reservation$Contact_Phone[complete.cases(Reservation$Contact_Phone)]
Abandoned$matched_incoming_Contact <- 0
Abandoned$matched_incoming_Contact[complete.cases(Abandoned$Incoming_Phone)] <- 1* matched_incoming_Contact
sum(Abandoned$matched_incoming_Contact) 

# Create one column which is any one of the above match
Abandoned$overall_Match<-1*(Abandoned$matched_email | Abandoned$matched_Incoming_Phone | Abandoned$matched_Contact_Phone | Abandoned$matched_incoming_Contact | Abandoned$matched_Contact_incoming)

# Task 2.5
#Treatment group who purchased
sum(Abandoned$overall_Match&Abandoned$Test)

#Control group who purchased
sum(Abandoned$overall_Match&Abandoned$Control)

#Treatment group who didn't purchased
sum(!Abandoned$overall_Match&Abandoned$Test)

#Control group who didn't purchased
sum(!Abandoned$overall_Match&Abandoned$Control)


sum(!Abandoned$overall_Match)

# Task 2.7
#Creating Cross Tabulation
cross_table <- xtabs(~ overall_Match + Test, data = Abandoned)
cross_table

#Task 2.8
#Creating Cross Tabulation for HI
cross_table_HI <- xtabs(~ overall_Match + Test, Abandoned$Address=='HI',data = Abandoned)
cross_table_HI

#Creating Cross Tabulation for NY
cross_table_NY <- xtabs(~ overall_Match + Test, Abandoned$Address=='NY',data = Abandoned)
cross_table_NY

#Creating Cross Tabulation for GA
cross_table_GA <- xtabs(~ overall_Match + Test, Abandoned$Address=='GA',data = Abandoned)
cross_table_GA

#Creating Cross Tabulation for AZ
cross_table_AZ <- xtabs(~ overall_Match + Test, Abandoned$Address=='AZ',data = Abandoned)
cross_table_AZ

#Creating Cross Tabulation for DE
cross_table_DE <- xtabs(~ overall_Match + Test, Abandoned$Address=='DE',data = Abandoned)
cross_table_DE

#Task 3.9
#Creating Cleaned data 
Cleaned_data <- data.frame(
  CustomerID = Abandoned$Caller_ID,
  Test_Group = Abandoned$Test_Control,
  Outcome = Abandoned$overall_Match,
  State_Available=Abandoned$Address,
  Email_Available=Abandoned$Email
  
)
View(Cleaned_data)
Cleaned_data$State_Available <- ifelse(is.na(Cleaned_data$State_Available),0,1)
Cleaned_data$Email_Available <- ifelse(is.na(Cleaned_data$Email_Available),0,1)
write.csv(Cleaned_data,"C:/Users/divya/OneDrive/Desktop/Study/QMB/Midterm_Project/Cleaned_data.csv",row.names=FALSE)

#Task 3.10
#Executing a linear regression 
Linear_reg <- lm(Cleaned_data$Outcome ~ Cleaned_data$Test_Group)
summary(Linear_reg)

#Task 3.11
#Executing Annova Test
Annova <- aov(Cleaned_data$Outcome~Cleaned_data$Test_Group)
summary(Annova)

#Task 3.13
#Executing linear regression with dummies
Linear_reg1 <- lm(Cleaned_data$Outcome~Cleaned_data$Test_Group + Cleaned_data$State_Available + Cleaned_data$Email_Available)
summary(Linear_reg1)

#Executing linear regression with dummies part 2
Linear_reg2 <- lm(Cleaned_data$Outcome~Cleaned_data$Test_Group*Cleaned_data$State_Available+ Cleaned_data$Test_Group*Cleaned_data$Email_Available)
summary(Linear_reg2)

# Generate summary table
install.packages("stargazer")
library(stargazer)
stargazer(Linear_reg, Linear_reg1, Linear_reg2, type = "html", out = "midterm.htm")
