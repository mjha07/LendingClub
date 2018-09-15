df = read.csv('/Users/mayankajha/Documents/Projects/LendingClub/LendingClubApp/data/LoanStats_2016Q1.csv', skip = 1)
dim(df)
colnames(df)

df_select <- df[,c("loan_amnt", "term", "int_rate", "installment", "grade","home_ownership", "annual_inc", "verification_status", "loan_status", "pymnt_plan", "purpose", 
"zip_code", "addr_state" )]

install.packages("ggplot2")
library(ggplot2)

ggplot(data=chol, aes(chol$AGE)) + 
  geom_histogram()

#Loan Amount
ggplot(data=df_select, aes(df_select$loan_amnt)) + 
  geom_histogram(col = "red", aes(fill=..count..), alpha = 0.2) + 
  labs(title="Histogram for Loan Amount", x="Loan Amount", y="Count") + 
  scale_fill_gradient("Count", low="green", high="red") 

#Term
ggplot(data=df_select, aes(x=factor(df_select$term))) + geom_bar(stat="count", width=0.7, fill="steelblue")+ labs(title="Frequency plot for Term", x="Term", y="Count") + theme_minimal()

#Interest Rate
#df_select <- data.frame(sapply(df_select, function(x) as.numeric(gsub("%", "", x))))
df_select$new_int_rate = as.numeric(gsub("%", "", df_select$int_rate))

ggplot(data=df_select, aes(df_select$new_int_rate)) + 
  geom_histogram(col = "red", aes(fill=..count..), alpha = 0.2) + 
  labs(title="Histogram for Interest Rate", x="Interest Rate", y="Frequency") +
  scale_fill_gradient("Count", low="green", high="red") 

#Grade
ggplot(data=df_select, aes(x=factor(df_select$grade))) + geom_bar(stat="count", width=0.7, fill="magenta")+ labs(title="Frequency plot for Loan Grade", x="Grade", y="Count") + theme_minimal()

#Home Ownership
ggplot(data=df_select, aes(x=factor(df_select$home_ownership))) + geom_bar(stat="count", width=0.7, fill="seagreen")+ labs(title="Frequency plot for Home Ownership Status", x="Home Ownership", y="Count") + theme_minimal()

#Annual Income
ggplot(data=df_select, aes(df_select$annual_inc)) + geom_histogram(col = "blue", aes(fill=..count..), alpha = 0.2) + labs(title="Histogram for Annual Income", x="Annual Income", y="Count") + scale_fill_gradient("Count", low="pink", high="blue") 

#Verification Status
ggplot(data=df_select, aes(x=factor(df_select$verification_status))) + geom_bar(stat="count", width=0.7, fill="orange")+ labs(title="Frequency plot for Verification Status", x="Verification Status", y="Count") + theme_minimal()

#Loan Status
ggplot(data=df_select, aes(x=factor(df_select$loan_status))) + geom_bar(stat="count", width=0.7, fill="steelblue")+ labs(title="Frequency plot for Loan Status", x="Loan Status", y="Count") + theme_minimal()

#Payment Plan
ggplot(data=df_select, aes(x=factor(df_select$pymnt_plan))) + geom_bar(stat="count", width=0.7, fill="steelblue")+ labs(title="Frequency plot for Payment Plan", x="Payment Plan", y="Count") + theme_minimal()

#Purpose
ggplot(data=df_select, aes(x=factor(df_select$purpose), fill = df_select$purpose)) + geom_bar(stat="count", width=0.7)+ labs(title="Frequency plot for Purpose", x="Purpose", y="Count") + theme_minimal() + coord_flip()

#State
ggplot(data=df_select, aes(x=factor(df_select$state), fill =df_select$state )) + geom_bar(stat="count", width=0.7)+ labs(title="Frequency plot for State", x="State", y="Count") + theme_minimal()

##Multiple Variable Data Exploration

#Loan Status vs Loan Amount
ggplot(data=df_select, aes(x=loan_status, y=loan_amnt, fill=df$loan_status)) + 
  geom_bar(stat="identity") + 
  labs(title="Loan amount vs Loan Status")

df_select["new_loan_status"] <- NA
df_select$new_loan_status <- ifelse((df_select$loan_status == 'Current')|(df_select$loan_status == 'Fully Paid'), 'Good Loan', 'Bad Loan')

ggplot(data=df_select, aes(x=new_loan_status, y=loan_amnt, fill=new_loan_status)) + 
  geom_bar(stat="identity") + 
  labs(title="Loan amount vs Bi-Category Loan Status")

#Verification Status vs Interest Rate
ggplot(data=df_select, aes(x=verification_status, y=new_int_rate, fill=df_select$verification_status)) + 
  geom_bar(stat="identity") + 
  labs(title="Interest Rate vs Verification Status")


