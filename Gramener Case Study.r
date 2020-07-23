#Gramener Case Study

#Reading the loan.csv into a data frame
loan<-read.csv("loan.csv",stringsAsFactors = F)

#Columns with less than 2 unique levels can be removed

loan<-loan[, sapply(loan, function(col) length(unique(col))) > 1]
colnames(loan)

#Remove columns which are not helpfull to draw conclusions

loan <- subset(loan, select = -c(id,member_id,title,collections_12_mths_ex_med, chargeoff_within_12_mths, tax_liens,url,desc) )

#Data cleaning - Formatting date fields
#Issue Date / Earlier Credit Line / Last Payment / Last credit pull

loan$issue_d <- as.Date(loan$issue_d,"%b-%d")
loan$earliest_cr_line <- as.Date(loan$earliest_cr_line,"%b-%d")
loan$last_credit_pull_d <- as.Date(loan$last_credit_pull_d,"%b-%d")
loan$last_pymnt_d <- as.Date(loan$last_pymnt_d, "%b-%d")


#Fixing outliers
x <- loan$annual_inc
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
loan$annual_inc <- x

#Using only full paid and defaulted loan data (leaving out current loans) - Data Subsetting
ncur_loan <- subset(loan,loan$loan_status!='Current')

#Filtering the loan dataset to include only 'Charged Off' loans
loan_co <- subset(loan,loan$loan_status=="Charged Off")


#Plotting for below cases :
library(ggplot2)

#Plot 1 : Annual Income - Loan Status - Box Plot
ggplot(ncur_loan, aes(ncur_loan$loan_status,ncur_loan$annual_inc)) + geom_boxplot() + ylim(0,100000) +
  labs(title = "Annual Income - Loan Status",x = "Loan Status",y = "Annual Income")

#Plot 2 : Annual Income - Loan Status - Histogram
ggplot(ncur_loan, aes(x=ncur_loan$annual_inc, fill = factor(ncur_loan$loan_status))) +  
  geom_histogram(bandwidth = 10000) +
  labs(title = "Annual Income - Loan Status", x = "Annual Income", y = "Loan Status")

#Performing Univariate Analysis on Annual Income  

ggplot(loan_co,aes(annual_inc)) + geom_histogram(binwidth = 10000,fill="steelblue")+ggtitle("Annual Income Histogram")
ggplot(ncur_loan,aes(annual_inc)) + geom_histogram(binwidth = 10000,fill="steelblue")+ggtitle("Annual Income Histogram")


#Performing Univariate Analysis on Loan Amount
ggplot(loan_co,aes(loan_amnt)) + geom_histogram(binwidth = 5000,fill="steelblue")+ggtitle("Loan Amount Histogram")
ggplot(ncur_loan,aes(loan_amnt)) + geom_histogram(binwidth = 5000,fill="steelblue")+ggtitle("Loan Amount Histogram")

#Performing Univariate Analysis on term
library(dplyr)
loan_g1 <- group_by(loan, loan$term, loan$loan_status)
loan_s1 <- summarise(loan_g1, count = n())
loan_s1 <- mutate(loan_s1,percent = (count/sum(count))*100)
# we can include above table in presentation 

ggplot(ncur_loan, aes(term)) + geom_bar(aes(fill = loan_status)) + ggtitle("Payment Tenure categorised by loan status")
ggplot(ncur_loan, aes(term)) + geom_bar(stat="count", fill="steelblue") + geom_text(stat='count',aes(label=..count..), vjust=-0.30)+xlab("Term Of Payments") + ylab("Count of Applicants") + ggtitle("Payment Tenure")

#Performing Univariate Analysis on emp_length
ggplot(ncur_loan,aes(emp_length)) + geom_bar(aes(fill = loan_status))+ggtitle("Emp Length categorised by loan status")
loan_g2 <- group_by(loan, loan$emp_length, loan$loan_status)
loan_s2 <- summarise(loan_g2, count = n())
loan_s2 <- mutate(loan_s2,percent = (count/sum(count))*100)
#we can include above table in presentation 

#Performing Univariate Analysis on Ownership
ggplot(loan_co,aes(home_ownership)) + geom_bar()+ggtitle("Home Ownership of charged off loans")

#Performing Univariate Analysis on grade
ggplot(loan_co,aes(grade)) + geom_bar(fill="steelblue")+ggtitle("Grade VS Count for charged off loans")
ggplot(loan_co, aes(grade)) + geom_bar(stat="count", fill="steelblue")+ geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.8), vjust=-0.30) + xlab("Grade") + ylab("Count of Applicants") + ggtitle("No of applicants Grade wise")+theme_minimal()

loan_g3 <- group_by(loan, loan$grade, loan$loan_status)
loan_s3 <- summarise(loan_g3, count = n())
loan_s3 <- mutate(loan_s3,percent = (count/sum(count))*100)
colnames(loan_s3) <- c("Grade", "Status", "Count", "Percentage")


loan_co1 <- subset(loan_s3, loan_s3$Status =="Charged Off")
ggplot(loan_co1,aes(x=loan_co1$Grade,y =loan_co1$Percentage)) + geom_col(fill="steelblue") + labs(x = "Grade", y = "Charge Off Percentage")

#Performing Univariate Analysis on DTI                                                                                
ggplot(loan_co,aes(dti)) + geom_histogram(binwidth = 5,fill="steelblue")+ggtitle("DTI Histogram")
ggplot(ncur_loan,aes(dti)) + geom_histogram(binwidth = 5,fill="steelblue")+ggtitle("DTI Histogram")

#Univariate analysis on Subgrade

ggplot(loan_co, aes(sub_grade)) + geom_bar(stat="count", fill="steelblue") +geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.8), vjust=-0.30)+ xlab("Sub Grade") + ylab("Count of Applicants") + ggtitle("No of applicants Sub Grade wise for charged off loans")+theme_minimal()

#Univariate analysis on Verification Status

ggplot(loan_co, aes(verification_status)) + geom_bar(stat="count", fill="steelblue") +geom_text(stat='count',aes(label=..count..),vjust=-0.35)+xlab("Verification Status of Applicant ")+ ylab("Applicants Count")+ ggtitle("Verification Status of Applicants of charged off loan status")



#Univariate analysis on Home Loan Purpose

ggplot(loan_co, aes(purpose)) + geom_bar(stat="count", fill="steelblue") + scale_y_continuous(trans='log2') +geom_text(stat='count',aes(label=..count..),vjust=-0.30)+ scale_x_discrete(labels = abbreviate)+ xlab("Purpose given by applicant") + ylab("Count of Applicants") +ggtitle("Purpose of loan")

#Univariate analysis on Open Accounts

ggplot(loan_co, aes(open_acc)) + geom_bar(stat = "count", fill="steelblue") + xlab("Open Accounts") + ylab("Count") + ggtitle("Open Accounts")

#Univariate analysis on Public Records

ggplot(loan_co, aes(pub_rec)) + geom_bar(stat = "count", fill="steelblue") + xlab("No. of Public Records") + ylab("Count") + ggtitle("Public Records")

#Univariate analysis on Public Record Bankruptcies

ggplot(loan_co, aes(pub_rec_bankruptcies)) + geom_bar(stat = "count", fill="steelblue") + xlab("No. of Public Records of Bankruptcy") + ylab("Count") + ggtitle("Public Record Bankruptcies")

#Univariate analysis on No of enquiries in last 6 months

ggplot(loan_co, aes(inq_last_6mths)) + geom_bar(stat = "count", fill="steelblue") +  geom_text(stat='count',aes(label=..count..),position=position_dodge(width=0.8), vjust=-0.30) +xlab("no of enquiries") + ylab("count") + ggtitle("No of enquiries in last 6 months") 


#Perfomring  Bivariate Analysis


#Relation between loan_amnt and home_ownership
ggplot(loan_co,aes(loan_amnt))+geom_histogram(aes(fill = home_ownership),binwidth = 5000)+ggtitle('Default Loans')


#Relation between annual_inc and home_ownership
ggplot(loan_co,aes(annual_inc))+geom_histogram(aes(fill = home_ownership),binwidth = 10000)+ggtitle('Default Loans')

#Relation between loan_amnt and grade
ggplot(loan_co,aes(loan_amnt))+geom_histogram(aes(fill = grade),binwidth = 5000)+ggtitle('Default Loans')

#Relation between annual_inc and grade
ggplot(loan_co,aes(annual_inc))+geom_histogram(aes(fill = grade),binwidth = 10000)+ggtitle('Default Loans')

#Relation between Loan status and Loan term 
ggplot(ncur_loan,aes(term,fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Loan term ")

#Relation between Loan status and Grade 
ggplot(ncur_loan,aes(grade,fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Grade")

#Relation between Loan status and Sub Grade 
ggplot(ncur_loan,aes(sub_grade,fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Sub Grade")

#Relation between Loan status and Home Ownership  
ggplot(ncur_loan,aes(home_ownership,fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Home Ownership ")

#Relation between Loan status and Verification Status 
ggplot(ncur_loan,aes(verification_status,fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Verification Status")

#Relation between Loan status and Purpose 
ggplot(ncur_loan,aes(purpose,fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Purpose ")

#Relation between Loan status and Delinquecy 2yrs 
ggplot(ncur_loan,aes(as.factor(delinq_2yrs),fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Delinquecy 2yrs")

#Relation between Loan status and Inquiry in Last 6months 
ggplot(ncur_loan,aes(as.factor(inq_last_6mths),fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Inquiry in Last 6months ")

#Relation between Loan status and Months since last delinquecy 
ggplot(ncur_loan,aes(as.factor(mths_since_last_delinq),fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Months since last delinquecy")

#Relation between Loan status and Open Account 
ggplot(ncur_loan,aes(as.factor(open_acc),fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Open Account")

#Relation between Loan status and Public Record 
ggplot(ncur_loan,aes(as.factor(pub_rec),fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Public Record ")

#Relation between Loan status and Public Record Bankruptcies 
ggplot(ncur_loan,aes(as.factor(pub_rec_bankruptcies),fill=loan_status))+geom_bar(position = 'fill')+ggtitle("Loan status vs Public Record Bankruptcies")

#Multivariate Analysis

ggplot(loan_co, aes(loan_amnt, y = grade, col = home_ownership )) + geom_point( alpha = 0.4) + geom_jitter()


