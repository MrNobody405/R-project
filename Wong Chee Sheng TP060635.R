#Wong Chee Sheng
#TP060635

#Importing data set
emp_data = read.csv("M:\\APU\\APU degree year 2\\Sem 1\\Programming for Data Analysis\\Assignment\\employee_attrition.csv", header = TRUE)

#Installing packages
install.packages("ggplot2")
install.packages("dplyr")

#Loading package
library(ggplot2)
library(dplyr)

#_______________________________________________________________________________


#Viewing the data in table form
View(emp_data)

#Obtaining the number of rows and columns of dataset
dim(emp_data)                                                                   #total 49653 rows and 18 columns

#Viewing the names of the column  
names(emp_data)

#Obtaining the structure of the dataset
str(emp_data)

#Viewing first and last 20 lines of dataset
head(emp_data, 20)
tail(emp_data, 20)

#Viewing a summary of the dataset
summary(emp_data)

#View which class the dataset is stored
class(emp_data)


#_______________________________________________________________________________


#Data cleaning

#Removing unused columns
emp_data$gender_full = NULL                                                     #As there are a column with gender in short form, the full form is not required

#Correcting the value from "Resignaton" to "Resignation"
emp_data[emp_data$termreason_desc == "Resignaton", 13] = "Resignation"

#Correcting the city name from "New Westminister" to "New Westminster"
emp_data[emp_data$city_name == "New Westminister", 8] = "New Westminster"

#Replacing termination date of "1/1/1900" to "NA"
emp_data$terminationdate_key[emp_data$terminationdate_key == "1/1/1900"] = "NA"

#Removing old data
emp_data = 
  emp_data %>%
  group_by(EmployeeID) %>%
  arrange(EmployeeID) %>%
  slice_tail(n = 1)

#_______________________________________________________________________________


#Data pre-processing

#Renaming column headings
colnames(emp_data)[colnames(emp_data) %in% c(
  "EmployeeID", "recorddate_key", "birthdate_key", "orighiredate_key", "terminationdate_key", 
  "age", "length_of_service", "city_name", "department_name", "job_title", "store_name", "gender_short", 
  "termreason_desc", "termtype_desc", "STATUS_YEAR", "STATUS", "BUSINESS_UNIT"
)] = c(
  "EmpID", "RecDate", "DOB", "HiredDate", "TerminationDate", "Age", "YearOfService", "City", 
  "Department", "JobTitle", "StoreID", "Gender", "TerminationReason", "Voluntariness", 
  "YearStatus", "Status", "BusinessUnit"
)


#_______________________________________________________________________________


#Data transformation

#Removing time stamp from recorddate_key
emp_data$RecDate = format(as.POSIXct(emp_data$RecDate, format='%m/%d/%Y %H:%M'), format='%m/%d/%Y')                      
#As the DateTime format for the recorddate_key has a time of 0:00, removing it is better

#Change RecDate data type to Date
emp_data$RecDate = as.Date(emp_data$RecDate, "%m/%d/%Y")

emp_data$DOB = as.Date(emp_data$DOB, "%m/%d/%Y")

emp_data$HiredDate = as.Date(emp_data$HiredDate, "%m/%d/%Y")


#_______________________________________________________________________________


#Data Exploration

#Determine which column is continuous or categorical
#Continuous data   : EmpID, RecDate, DOB, HiredDate, TerminationDate, Age, YearOfService, YearStatus
#Categorical data  : City, Department, JobTitle, StoreID, Gender, GenderFull, TerminationReason, Voluntariness, Status, BusinessUnit

#Max and min value of continuous column
max(emp_data$EmpID)
min(emp_data$EmpID)
summary(emp_data$EmpID)

max(emp_data$RecDate)
min(emp_data$RecDate)
summary(emp_data$RecDate)

max(emp_data$DOB)
min(emp_data$DOB)
summary(emp_data$DOB)

max(emp_data$HiredDate)
min(emp_data$HiredDate)
summary(emp_data$HiredDate)

max(emp_data$TerminationDate)
min(emp_data$TerminationDatee)
summary(emp_data$TerminationDate)

max(emp_data$Age)
min(emp_data$Age)
summary(emp_data$Age)

max(emp_data$YearOfService)
min(emp_data$YearOfService)
summary(emp_data$YearOfService)

max(emp_data$YearStatus)
min(emp_data$YearStatus)
summary(emp_data$YearStatus)

#Categories available in Categorical column
emp_data$City
factor(emp_data$City)
nlevels(factor(emp_data$City))                                                  #39

emp_data$Department
factor(emp_data$Department)
nlevels(factor(emp_data$Department))                                            #21

semp_data$JobTitle
factor(emp_data$JobTitle)
nlevels(factor(emp_data$JobTitle))                                              #47

emp_data$StoreID
factor(emp_data$StoreID)
nlevels(factor(emp_data$StoreID))                                               #46

emp_data$Gender
factor(emp_data$Gender)
nlevels(factor(emp_data$Gender))

emp_data$TerminationReason
factor(emp_data$TerminationReason)
nlevels(factor(emp_data$TerminationReason))                                     #4

emp_data$Voluntariness
factor(emp_data$Voluntariness)
nlevels(factor(emp_data$Voluntariness))                                         #3

emp_data$Status
factor(emp_data$Status)
nlevels(factor(emp_data$Status))                                                #2

emp_data$BusinessUnit
factor(emp_data$BusinessUnit)
nlevels(factor(emp_data$BusinessUnit))                                          #2


nrow(emp_data[emp_data$Status == "TERMINATED", ])                               
#There are 1485 employees that are terminated based on Status


Terminated_emp = subset(emp_data, Status == "TERMINATED")
#Creating a variable called Terminated_emp with employee data that has terminated status


#_______________________________________________________________________________


#Categorical data
table(Terminated_emp$Department)
names(which.max(table(Terminated_emp$Department)))                              

ggplot(Terminated_emp, aes(Department, fill = Department)) + geom_bar()



table(Terminated_emp$JobTitle)
names(which.max(table(Terminated_emp$JobTitle)))

ggplot(Terminated_emp, aes(JobTitle, fill = JobTitle)) + geom_bar()



table(Terminated_emp$StoreID)
names(which.max(table(Terminated_emp$StoreID)))

ggplot(Terminated_emp, aes(StoreID, fill = StoreID)) + geom_bar()



table(Terminated_emp$City)
names(which.max(table(Terminated_emp$City)))

ggplot(Terminated_emp, aes(City, fill = City)) + geom_bar()



table(Terminated_emp$Gender)
names(which.max(table(Terminated_emp$Gender)))

ggplot(Terminated_emp, aes(Gender, fill = Gender)) + geom_bar()



table(Terminated_emp$TerminationReason)
names(which.max(table(Terminated_emp$TerminationReason)))

ggplot(Terminated_emp, aes(TerminationReason, fill = TerminationReason)) + geom_bar()



table(Terminated_emp$Voluntariness)
names(which.max(table(Terminated_empe$Voluntariness)))

ggplot(Terminated_emp, aes(Voluntariness, fill = Voluntariness)) + geom_bar()


table(Terminated_emp$Status)
names(which.max(table(Terminated_emp$Status)))

ggplot(Terminated_emp, aes(Status, fill = Status)) + geom_bar()



table(Terminated_emp$BusinessUnit)
names(which.max(table(Terminated_emp$BusinessUnit)))

ggplot(Terminated_emp, aes(BusinessUnit, fill = BusinessUnit)) + geom_bar()


Ratio = c(nrow(emp_data[emp_data$Status == "TERMINATED", ]), 
          nrow(emp_data[emp_data$Status == "ACTIVE", ]))

RatioPerc = paste0(round(100 * Ratio/sum(Ratio), 2), "%")
#Ratio percentage is 23.6% to 76.4%

pie(Ratio,
    labels = c("Terminated", "Non-terminated"),
    radius = 1,
    main = "Ratio of employees",
    col = c("pink", "lightblue"),
    density = 90,
    clockwise = TRUE
)


#_______________________________________________________________________________

#Question 1: 
# What is the general characteristic between terminated and non-terminated employees?

#Analysis 1-1: Find the difference in year of service between terminated and non-terminated employees.
Active_emp = emp_data %>%
             filter(Status == "ACTIVE")

ggplot(Active_emp,
       aes(Age, YearOfService)
       ) + geom_point(aes(size = 1)) + 
           stat_smooth(method = lm) + 
           ggtitle("Year of service for active employee for every age")

ggplot(Terminated_emp,
       aes(Age, YearOfService)
       ) + geom_point(aes(size = 1)) + 
           stat_smooth(method = lm) + 
           ggtitle("Year of service for terminated employee for every age")

#Active employees have a proportional year of service when compared to their age and some have longer year of service 
#such that employees age 45 to 60 have years of service up to 25 years.
#Terminated employees also have a proportional year of service but a slight down slope for age 20 to 50.
#Some outliers exists where older employee have short year of service which may due to joining the company late.
#_______________________________________________________________________________
#Analysis 1-2: Find the difference in city between terminated and non-terminated employees.
ggplot(Active_emp,
       aes(City)
       ) + geom_histogram(stat = "count", color = "burlywood3", fill = "blanchedalmond")+ 
           labs(x = "City", y = "Number of employees") +
           ggtitle("Cities of active employees")

ggplot(Terminated_emp,
       aes(City)
       ) + geom_histogram(stat = "count", color = "aquamarine4", fill = "aquamarine")+ 
           labs(x = "City", y = "Number of employees") +
           ggtitle("Cities of terminated employees")

#Majority of the active employee works in Vancouver city so is terminated employees.
#_______________________________________________________________________________
#Analysis 1-3: Find the difference in department between terminated and non-terminated employees.
Active_empdept = as.data.frame(table(Active_emp$Department))
Terminated_empdept = as.data.frame(table(Terminated_emp$Department))

ggplot(Active_empdept,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "darkorange4", fill = "darkorange") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Department", y = "Number of employees") + 
           ggtitle("Number of active employees in terms of department")

ggplot(Terminated_empdept,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", fill = "darkred") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Department", y = "Number of employees") + 
           ggtitle("Number of terminated employees in terms of department")

#There are only 9 department with active employees and most of them are from Bakery (786), Customer service (928),
#Dairy (842), Meats (875), Processed foods (646) and produce (707). There are only 15 employees left in
#Executive (10), Recruitment (1) and Store Management (4) department.
#Most of the department are terminated and the highest number of terminated department are Bakery (112), 
#Customer Service (262), Dairy (191), Meats (377), Processed foods (90) and produce (353).
#_______________________________________________________________________________
#Analysis 1-4: Find the difference in job title between terminated and non-terminated employees.
Active_empJ = as.data.frame(table(Active_emp$JobTitle))
Terminated_empJ = as.data.frame(table(Terminated_emp$JobTitle))

ggplot(Active_empJ,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "blue1", fill = "darkslategray2") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Job title", y = "Number of employees") + 
           ggtitle("Number of active employees in terms of job title")

ggplot(Terminated_empJ,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", fill = "darkred") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Job title", y = "Number of employees") + 
           ggtitle("Number of terminated employees in terms of job title")

#Majority of the active employees work as baker, cashier, dairy person, meat cutter, produce clerk and shelf stocker
##while other job position does not reach a minimum of 20 employees.
#Majority of terminated employees worked as the same as active employees but with more job title position eliminated.
#_______________________________________________________________________________
#Analysis 1-5: Find the difference in storeID between terminated and non-terminated employees. 
Active_empS = as.data.frame(table(Active_emp$StoreID))
Terminated_empS = as.data.frame(table(Terminated_emp$StoreID))

ggplot(Active_empS,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", fill = "chartreuse3") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "StoreID", y = "Number of employees") + 
           ggtitle("Number of active employees in terms of StoreID")

ggplot(Terminated_empS,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", fill = "darkmagenta") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "StoreID", y = "Number of employees") + 
           ggtitle("Number of terminated employees in terms of StoreID")

#The storeID with high number of active employees includes 46 (473), 18 (399), 42 (370), 21 (333).
#As for the terminated employees, storeID 35 and 37 has a high number of employees terminated.
#_______________________________________________________________________________
#Analysis 1-6: Find the difference in gender between terminated and non-terminated employees.
Active_empG = table(Active_emp$Gender)
Terminated_empG = table(Terminated_emp$Gender)

par(mfrow = c(1, 2))

pie(Active_empG,
    radius = 5,
    main = "Gender of active employees",
    labels = c("Female", "Male"),
    col = c("deeppink4", "blue4"),
    density = 60,
    clockwise = T
    )

pie(Terminated_empG,
    radius = 5,
    main = "Gender of terminated employees",
    labels = c("Female", "Male"),
    col = c("deeppink4", "blue4"),
    density = 60,
    clockwise = T
    )

Terminated_empGRatio = paste0(round(100 * Terminated_empG/sum(Terminated_empG), 2), "%")

#The number of male and female employees that are active is even while the number of terminated employees
#have higher number of females terminated compared to male at 61.62% to 38.38%.
#_______________________________________________________________________________

#_______________________________________________________________________________

#Question 2:
# At what age, employees are being terminated the most?

#Analysis 2-1: Find the age of most terminated employees.
AllA = table(emp_data$Age)
TermA = table(Terminated_emp$Age)
Acomp = rbind(AllA, TermA)
barplot(Acomp, 
        main = "Number of total and terminated employees in terms of age",
        xlab = "Age",
        ylab = "Number of employees",
        col = c("grey", "red"),
        beside = T)

#Most employees are terminated at age 60 and 65.
#_______________________________________________________________________________
#Analysis 2-2: Find the gender of terminated employees aged 60 and 65.
Term60 = table(Terminated_emp$Gender[Terminated_emp$Age == "60"])
Term65 = table(Terminated_emp$Gender[Terminated_emp$Age == "65"])

par(mfrow = c(1, 2))

pie(Term60,
    radius = 5,
    main = "Gender of terminated employees aged 60",
    labels = c("Female", "Male"),
    col = c("pink", "lightblue"),
    clockwise = T
)

pie(Term65,
    radius = 5,
    main = "Gender of terminated employees aged 65",
    labels = c("Female", "Male"),
    col = c("pink", "lightblue"),
    clockwise = T
    )

#Most employees aged 60 are male while all employees aged 65 are female.
#_______________________________________________________________________________
#Analysis 2-3: Find the exact number of 60 years old terminated male and female employee.
emp60 = Terminated_emp %>%
        filter(Age == 60 )
emp60G = as.data.frame(table(emp60$Gender))

ggplot(emp60G,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "red", fill = "pink") +
           geom_text(aes(label = Freq), vjust = 0) +
           labs(x = "Gender", y = "Number of employees") +
           ggtitle("Number of employees aged 60 in terms of gender")

#There are 2 female employees and 294 male employees aged 60.
#_______________________________________________________________________________
#Analysis 2-4: Find the termination reason for employees aged 60 and 65.
emp60M = Terminated_emp %>%
        filter(Age == 60 & Gender == "M")

emp60R = table(emp60M$TerminationReason)

emp65 = Terminated_emp %>%
        filter(Age == 65)

emp65R = table(emp65$TerminationReason)

par(mfrow = c(1, 2))

pie(emp60R,
    radius = 5,
    main = "Termination reason for male employees aged 60",
    col = "deepskyblue4",
    clockwise = T
    )

pie(emp65R,
    radius = 5,
    main = "Termination reason for female employees aged 65",
    col = "darkviolet",
    clockwise = T
    )

#All male employees aged 60 and all female employees aged 65 retires.
#_______________________________________________________________________________
#Analysis 2-5: Find the department of both male and female employees aged 60 and 65.
emp60D = as.data.frame(table(emp60M$Department))

ggplot(emp60D,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "blue", fill = "lightblue") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Department", y = "Number of employees") + 
           ggtitle("Number of male employees aged 60 in terms of department")

emp65D = as.data.frame(table(emp65$Department))

ggplot(emp65D,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "red", fill = "pink") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Department", y = "Number of employees") + 
           ggtitle("Number of female employees aged 65 in terms of department")

#The majority of 60 years old male employees worked in Meats (70) and Produce (137) department
#while the majority of 65 years old female employees worked in Meats (247) and Produce (169) department.
#_______________________________________________________________________________
#Analysis 2-6: Find the Job title of both male and female employees aged 60 and 65.
emp60J = as.data.frame(table(emp60$JobTitle))

ggplot(emp60J,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "blue", fill = "lightblue") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Job title", y = "Number of employees") + 
           ggtitle("Number of male employees aged 60 in terms of job title")

emp65J = as.data.frame(table(emp65$JobTitle))

ggplot(emp65J,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "red", fill = "pink") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Job title", y = "Number of employees") + 
           ggtitle("Number of female employees aged 65 in terms of job title")

#The majority of 60 years old male employees worked as Meat cutter (63) and Produce clerk (129) department
#while the majority of 65 years old female employees worked as Meat cutter (236) and Produce clerk (160) department.
#Their Job title is heavily related to the department.
#_______________________________________________________________________________
#Analysis 2-7: Find the StoreID of the employees aged 60 and 65.
emp60_65 = Terminated_emp %>%
           filter(Age == 60 | Age == 65 & Department == "Meats" | Department == "Produce")

emp60_65Store = as.data.frame(table(emp60_65$StoreID))

ggplot(emp60_65Store,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "green", fill = "lightgreen") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "StoreID", y = "Number of employees") + 
           ggtitle("Number of employees aged 60 and 65 in terms of storeID")

# Under the meats and produce department, most of the employees worked in storeID 35 (124) and storeID 37 (90).
#_______________________________________________________________________________
#Analysis 2-8: Find the year of service for both employees aged 60 and 65.
ggplot(emp60, 
       aes(x = Age, y = YearOfService, fill = "")
       ) + geom_violin(trim = T, draw_quantiles = c(.25, .5, .75)) +
           ggtitle("Year of service for employees aged 60")

ggplot(emp65, 
       aes(Age, YearOfService, fill = "")
       ) + geom_violin(trim = T, draw_quantiles = c(.25, .5, .75)) +
           ggtitle("Year of service for employees aged 60")

#The majority of 60 years old employees have worked at least 8 years and some worked for 19 years and maximum is 22 years.
#The majority of 65 years old employees have worked at least 13 years and some worked for more than 22 years.
#_______________________________________________________________________________
#Analysis 2-9: Find the city of majority of the employees aged 60 and 65 live in.
emp60_65 = Terminated_emp %>%
           filter(Age == 60 | Age == 65)

ggplot(emp60_65,
       aes(City)
       ) + geom_histogram(stat = "count", aes(fill =..count..), binwidth = .5)+ 
           labs(x = "City", y = "Number of employees") +
           ggtitle("Number of employees aged 60 and 65 in terms of city")

#Most employees aged 60 and 65 worked in Vancouver city (220).
#_______________________________________________________________________________
#Analysis 2-10: Find the hired date of the employees aged 60 and 65.
emp60_65$HiredDate = format(as.POSIXct(emp60_65$HiredDate, format='%m/%d/%Y'), format='%Y') 
emp60_65Hired = as.data.frame(table(emp60_65$HiredDate))

ggplot(emp60_65Hired,
       aes(Var1, y = Freq)
        ) + geom_bar(stat = "identity", col = "red", fill = "pink") +
            geom_text(aes(label = Freq), vjust = 0) + 
            labs(x = "Hired year", y = "Number of employees") + 
            ggtitle("Number of employees aged 60 and 65 in terms of hired year")

#All of the employees aged 60 and 65 are hired from the year 1989 to 2000 with a peak
#year of 1990 (149) and (118) at 1998 and 1999.
#_______________________________________________________________________________
#Analysis 2-11: Find the average retirement year for employees aged 60 and 65.
emp60_65$TerminationDate = format(as.POSIXct(emp60_65$TerminationDate, format='%m/%d/%Y'), format='%Y') 
emp60_65Retire = as.data.frame(table(emp60_65$TerminationDate))

ggplot(emp60_65Retire,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "red", fill = "pink") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Retirement year", y = "Number of employees") + 
           ggtitle("Number of employees aged 60 and 65 in terms of retirement year")

# Most of the employees retire in the late 2000s and a decline starts from 2011 to 2015.
#The number peak at 2008 (128), followed by 2007 (122) and 2009 (115).
#_______________________________________________________________________________
#Analysis 2-12: Find the percentage of retirement compared to other termination reason.
termR = c(nrow(Terminated_emp[Terminated_emp$TerminationReason == "Retirement", ]), 
          nrow(Terminated_emp[Terminated_emp$TerminationReason == "Resignation", ]), 
          nrow(Terminated_emp[Terminated_emp$TerminationReason == "Layoff", ]))

pie(termR,
    radius = 5,
    labels = c("Retirement", "Resignation", "Layoff"),
    main = "Pie chart for termination reason",
    col = c("darkred", "darkviolet", "darkblue"),
    clockwise = T
    )

termRatio = paste0(round(100 * termR/sum(termR), 2), "%")

#59.6% of the employees are retired.
#_______________________________________________________________________________

#_______________________________________________________________________________

#Question 3: 
# Why does employees aged 30 have an abnormal termination rate?

#Analysis 3-1: Find the gender of employees aged 30.
emp30 = Terminated_emp %>%
        filter(Age == 30)

emp30G = c(nrow(emp30[emp30$Gender == "F", ]),
           nrow(emp30[emp30$Gender == "M", ]))

pie(emp30G,
    radius = 5,
    labels = c("Female", "Male", "Layoff"),
    main = "Pie chart for gender in employees aged 30",
    col = c("deeppink2", "darkturquoise"),
    clockwise = T
    )
gender30Ratio = paste0(round(100 * emp30G/sum(emp30G), 2), "%")

#92.68% of the employees aged 30 are female while 7.32% is male.
#_______________________________________A________________________________________
#Analysis 3-2: Find the termination reason for employees aged 30.
ggplot(emp30,
       aes(TerminationReason)
       ) + geom_histogram(stat = "count", fill = "darkred")+ 
           labs(x = "Termination reason", y = "Number of employees") +
           ggtitle("Termination reason for employees aged 30")

nrow(emp30[emp30$TerminationReason == "Resignation", ])

#Most of the employees aged 30 are resigning (75) and some are laid off (7).
#_______________________________________________________________________________
#Analysis 3-3: Find the department for employees aged 30.
emp30dept = as.data.frame(table(emp30$Department))

ggplot(emp30dept,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "coral1", fill = "cornsilk1") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Department", y = "Number of employees") + 
           ggtitle("Number of employees aged 30 in terms of department")

#All of them worked in bakery, customer service, dairy, meats and processed foods department
#with highest number in dairy (28), second in processed foods (26) and third in customer service (18).
#_______________________________________________________________________________
#Analysis 3-4: Find the job title for employees aged 30.
emp30J = as.data.frame(table(emp30$JobTitle))

ggplot(emp30J,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "blue1", fill = "darkslategray2") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Job title", y = "Number of employees") + 
           ggtitle("Number of employees aged 30 in terms of job title")

#Following the prior analysis, 28 of them worked as dairy person, 26 as shelf stocker and 18 as cashier.
#_______________________________________________________________________________
#Analysis 3-5: Find the year of service for employees aged 30.
ggplot(emp30,
       aes(x = Age, y = YearOfService, fill ="")
       ) + geom_violin(trim = T, draw_quantiles = c(.25, .5, .75)) +
           ggtitle("Year of service for employees aged 30")

# Most of the employees have served for 5 years and some served for 6. The maximum years is 7 while the minimum is 3.
#_______________________________________________________________________________
#Analysis 3-6: Find the city for employees aged 30.
emp30C = as.data.frame(table(emp30$City))

ggplot(emp30C,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "blue1", fill = "darkslategray2") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "City", y = "Number of employees") + 
           ggtitle("Number of employees aged 30 in terms of city")

#13 of the employees are from Vancouver while 10 are from New Westminster and Victoria each.
#_______________________________________________________________________________
#Analysis 3-7: Find the hired date for employees aged 30.
emp30$HiredDate = format(as.POSIXct(emp30$HiredDate, format='%m/%d/%Y'), format='%Y') 
emp30Hired = as.data.frame(table(emp30$HiredDate))

ggplot(emp30Hired,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "chartreuse2", fill = "darkolivegreen1") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Hired year", y = "Number of employees") + 
           ggtitle("Number of employees aged 30 in terms of hired year")

#Most of the employees aged 30 are hired in the year 2007 (38), following by  2008 (25) and 2009 (13).
#_______________________________________________________________________________
#Analysis 3-8: Find the average termination year for employees aged 30.
emp30$TerminationDate = format(as.POSIXct(emp30$TerminationDate, format='%m/%d/%Y'), format='%Y') 
emp30TermDate = as.data.frame(table(emp30$TerminationDate))

ggplot(emp30TermDate,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "blue1", fill = "cornflowerblue") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Termination year", y = "Number of employees") + 
           ggtitle("Number of employees aged 30 in terms of termination year")
#Most of the employees are terminated in 2015 (30), followed by 2013 (19) and 2012 (18).
#_______________________________________________________________________________

#_______________________________________________________________________________

#Question 4: 
#What is the difference in termination rate in terms of business unit?

#Analysis 4-1: Find the termination rate for the business unit.
HOdiff = c(nrow(emp_data[emp_data$Status == "ACTIVE" & emp_data$BusinessUnit == "HEADOFFICE", ]),
           nrow(emp_data[emp_data$Status == "TERMINATED" & emp_data$BusinessUnit == "HEADOFFICE", ]))

Sdiff = c(nrow(emp_data[emp_data$Status == "ACTIVE" & emp_data$BusinessUnit == "STORES", ]),
          nrow(emp_data[emp_data$Status == "TERMINATED" & emp_data$BusinessUnit == "STORES", ]))

par(mfrow = c(1, 2))

pie(HOdiff,
    radius = 3,
    labels = c("Active", "Terminated"),
    main = "Pie chart for termination in HEAOFFICE",
    col = c("darkseagreen3", "deeppink4"),
    clockwise = T
)

pie(Sdiff,
    radius = 3,
    labels = c("Active", "Terminated"),
    main = "Pie chart for termination in STORES",
    col = c("darkseagreen3", "deeppink4"),
    clockwise = T
)

HOPerc = paste0(round(100 * HOdiff/sum(HOdiff), 2), "%")
SPerc = paste0(round(100 * Sdiff/sum(Sdiff), 2), "%")

#86.25% of HEADOFFICE business unit is terminated while 22.82% of STORES business unit is terminated.
#_______________________________________________________________________________
#Analysis 4-2: Find the departments associated with respective business unit.
headofficedept = emp_data %>%
                 filter(BusinessUnit == "HEADOFFICE")

storedept = emp_data %>%
            filter(BusinessUnit == "STORES")

unique(headofficedept$Department)
unique(storedept$Department)

ggplot(headofficedept,
       aes(Department)
       ) + geom_histogram(stat = "count", color = "burlywood3", fill = "blanchedalmond")+ 
           labs(x = "Department", y = "Number of employees") +
           ggtitle("Department in HEADOFFICE")

ggplot(storedept,
       aes(Department)
       ) + geom_histogram(stat = "count", color = "burlywood3", fill = "blanchedalmond")+ 
           labs(x = "Department", y = "Number of employees") +
           ggtitle("Department in STORES")

#Most employees in HEADOFFICE works in executive(10), HR technology (9) and recruitment (9).
#Most employees in STORES works in Meats, Customer service, dairy, bakery and processed foods.
#_______________________________________________________________________________
#Analysis 4-3: Find which department is still active and which is terminated for headoffice business unit.
headofficeActive = headofficedept %>%
                   filter(Status == "ACTIVE")

headofficeTerminated = headofficedept %>%
                       filter(Status == "TERMINATED")

unique(headofficeActive$Department)
unique(headofficeTerminated$Department)

#All department from HEADOFFICE is completely terminated while only executive and recruitment remains.
#_______________________________________________________________________________
#Analysis 4-4: Find the age of terminated headoffice departments.
ggplot(headofficeTerminated,
       aes(x = Department, y = Age, color = Department)
       ) + geom_boxplot() +
           ggtitle("Age of terminated employees in HEADOFFICCE in terms of department")

#The employees in HEADOFFIVE for each department in the range of 60 and 65 years old.
#_______________________________________________________________________________
#Analysis 4-5: Find the termination reason in headoffice business unit.
headofficedeptTermReason = table(headofficeTerminated$TerminationReason)

pie(headofficedeptTermReason,
    radius = 2,
    main = "Termination reason in headoffice",
    col = c("blue3", "darkorchid1"), 
    density = 40,
    clockwise = T
    )

HOTermPerc = paste0(round(100 * headofficedeptTermReason/sum(headofficedeptTermReason), 2), "%")

#98.55% of employees are retired while 1.45% is resigned from their position.
#_______________________________________________________________________________
#Analysis 4-6: Find the job title of terminated employees in headoffice.
HOJ = as.data.frame(table(headofficeTerminated$JobTitle))

ggplot(HOJ,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "blue1", fill = "darkslategray2") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Job title", y = "Number of employees") + 
           ggtitle("Number of terminated employees in HEADOFFICCE in terms of job title")

#The number of employees are pretty even throughout the job title for HEADOFFICE with maximum 8 holding
#the position of HRIS Analyst and recruiter.
#_______________________________________________________________________________
#Analysis 4-7: Find the StoreID of terminated employees in headoffice.
HOStoreID = as.data.frame(table(headofficeTerminated$StoreID))

ggplot(HOStoreID,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "blue1", fill = "darkslategray2") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "StoreID", y = "Number of employees") + 
           ggtitle("Number of terminated employees in HEADOFFICCE in terms of storeID")

#All of the employees worked in store 69.
#_______________________________________________________________________________
#Analysis 4-8: Find the year of service of terminated employees in headoffice.
ggplot(headofficeTerminated,
       aes(Age, YearOfService, aes(size = 1), col = Age)
       ) + geom_point() + 
           ggtitle("Year of service for terminated employees in HEADOFFICE") + 
           stat_smooth(method = lm)

#The year of service is proportional to their age where a 57 years old employees have been 
#serving the company for 17 years while 65 years old employees have been serving for around 25 years.
#_______________________________________________________________________________
#Analysis 4-9: Find the city of terminated employees in headoffice.
HOcity = as.data.frame(table(headofficeTerminated$City))

ggplot(HOcity,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "darkgray", fill = "cornsilk2") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "City", y = "Number of employees") + 
           ggtitle("Number of terminated employees in HEADOFFICE in terms of city")

#All of the 69 terminated employees worked in Vancouver.
#_______________________________________________________________________________
#Analysis 4-10: Find the average hired year of terminated employees in headoffice.
headofficeTerminated$HiredDate = format(as.POSIXct(headofficeTerminated$HiredDate, format='%m/%d/%Y'), format='%Y') 
HOhired = as.data.frame(table(headofficeTerminated$HiredDate))

ggplot(HOhired,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "chartreuse2", fill = "darkolivegreen1") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Hired year", y = "Number of employees") + 
           ggtitle("Number of terminated employees in HEADOFFICE in terms of hired year")

#The terminated employees in HEADFOFFICE are hired in the year 1989 (30) and 1990 (39).
#_______________________________________________________________________________
#Analysis 4-11: Find the average termination year of terminated employees in headoffice.
headofficeTerminated$TerminationDate = format(as.POSIXct(headofficeTerminated$TerminationDate, format='%m/%d/%Y'), format='%Y') 
HOTermDate = as.data.frame(table(headofficeTerminated$TerminationDate))

ggplot(HOTermDate,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "chartreuse2", fill = "darkolivegreen1") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Termination year", y = "Number of employees") + 
           ggtitle("Number of terminated employees in HEADOFFICE in terms of termination year")

#Most of the employees retires in year 2009 (23) and 2014 (24).
#_______________________________________________________________________________

#_______________________________________________________________________________

#Question 5: 
#Which year has the highest termination rate?

#Analysis 5-1: Find the year of highest termination after removing retirement.
year = subset(Terminated_emp, TerminationReason != "Retirement")
year$TerminationDate = format(as.POSIXct(year$TerminationDate, format='%m/%d/%Y'), format='%Y') 

ggplot(year,
      aes(TerminationDate)
      ) + geom_histogram(stat = "count", color = "red", aes(fill = ..count..)) +
          scale_fill_gradient("Count", low = "green", high = "red") +
          ggtitle("Number of employees terminated in year")

#2014 has the most termination rate at 197 employees.

#_______________________________________________________________________________
#Analysis 5-2: Find the age of employees terminated in 2014.
age2014 = year %>%
          filter(TerminationDate == 2014)

ggplot(age2014 ,
       aes(x = TerminationDate, y = Age)
       ) + geom_boxplot() +
           geom_dotplot(binaxis = "y",
                        stackdir = "center",
                        dotsize = 0.5,
                        fill = "cyan") +
           ggtitle("Age of terminated employees in 2014")

#Most of the employees terminated in 2014 are younger employees around the age of 21.
#_______________________________________________________________________________
#Analysis 5-3: Find the gender of employees terminated in 2014.
gender2014 = c(nrow(age2014[age2014$Gender == "F", ]),
               nrow(age2014[age2014$Gender == "M", ]))

pie(gender2014,
    radius = 5,
    main = "Gender of terminated employees in 2014",
    labels = c("Female", "Male"),
    col = c("brown2", "darkslategray3"), 
    density = 40,
    clockwise = T
)
gender2014Ratio = paste0(round(100 * gender2014/sum(gender2014), 2), "%")

#The distribution of gender of terminated employees in 2014 is pretty even at 48.73% female and 51.27% male.
#_______________________________________________________________________________
#Analysis 5-4: Find the termination reason of employees terminated in 2014.
ggplot(age2014,
       aes(TerminationReason)
       ) + geom_histogram(stat = "count", fill = "darkgoldenrod")+ 
           labs(x = "Termination reason", y = "Number of employees") +
           ggtitle("Termination reason for employees in 2014")

nrow(age2014[age2014$TerminationReason == "Layoff", ])
nrow(age2014[age2014$TerminationReason == "Resignation", ])

#A high number of employees are laif off in 2014 (142) and some employees resigned (55).
#_______________________________________________________________________________
#Analysis 5-5: Find the department of employees terminated in 2014.
age2014dept = as.data.frame(table(age2014$Department))

ggplot(age2014dept,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "darkorange4", fill = "darkorange") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Department", y = "Number of employees") + 
           ggtitle("Number of employees terminated in 2014 in terms of department")

#The employees are mostly from customer service department (96).
#_______________________________________________________________________________
#Analysis 5-6: Find the job title of employees terminated in 2014.
age2014J = as.data.frame(table(age2014$JobTitle))

ggplot(age2014J,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "blue1", fill = "darkslategray2") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Job title", y = "Number of employees") + 
           ggtitle("Number of employees terminated in 2014 in terms of job title")

#As per the department, a high number worked as Cashier for customer service department at 94 employees.
#_______________________________________________________________________________
#Analysis 5-7: Find the storeID of employees terminated in 2014.
age2014ID = as.data.frame(table(age2014$StoreID))

ggplot(age2014ID,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "darkslategrey", fill = "cornflowerblue") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "StoreID", y = "Number of employees") + 
           ggtitle("Number of terminated employees in 2014 in terms of storeID")

#The number of employees in 2014 are distributed evenly across the store however
#there are a peak number of terminated employees in store 11 (39), 20 (31), 13(30) and 14 (21).
#_______________________________________________________________________________
#Analysis 5-8: Find the year of service of employees terminated in 2014.
ggplot(age2014,
       aes(Age, YearOfService, aes(size = 1), col = Age)
       ) + geom_point() + 
           ggtitle("Year of service for terminated employees in HEADOFFICE") + 
           stat_smooth(method = lm)

#The year of service for terminated employees in 2014 are proportional as younger employees
#do not have longer service compared to older employees.
#_______________________________________________________________________________
#Analysis 5-9: Find the city of employees terminated in 2014.
age2014city = as.data.frame(table(age2014$City))

ggplot(age2014city,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "darkslateblue", fill = "cadetblue2") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "City", y = "Number of employees") + 
           ggtitle("Number of terminated employees in HEADOFFICE in terms of city")

#Most of the employees in 2014 worked from Fort Nelson (39), followed by New Westminster (35), Grand Forks (30) and Haney (21).
#_______________________________________________________________________________
#Analysis 5-10: Find the average hired year of employees terminated in 2014.
age2014$HiredDate = format(as.POSIXct(age2014$HiredDate, format='%m/%d/%Y'), format='%Y') 
age2014hired = as.data.frame(table(age2014$HiredDate))

ggplot(age2014hired,
       aes(Var1, y = Freq)
       ) + geom_bar(stat = "identity", col = "chartreuse2", fill = "darkolivegreen1") +
           geom_text(aes(label = Freq), vjust = 0) + 
           labs(x = "Hired year", y = "Number of employees") + 
           ggtitle("Number of terminated employees in HEADOFFICE in terms of hired year")

#Most of the employee are employed at a low rate each year from 1990
#but it peaked at 2012 with 29 employees and 2013 with 41 employees.
#_______________________________________________________________________________


