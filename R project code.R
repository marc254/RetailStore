getwd() #------- GET Working Directory ---------------
setwd('C:\\Users\\Acer\\Desktop\\USB\\Marc - DSA\\Projects\\retail_project')


#============================= Importing datasets ==============================

customer = read.csv('C:\\Users\\Acer\\Desktop\\USB\\Marc - DSA\\Projects\\retail-shop-case-study-dataset\\Customer.csv',
                 header = TRUE, na.strings=c("","NA"), sep = ",")


product = read.csv('C:\\Users\\Acer\\Desktop\\USB\\Marc - DSA\\Projects\\retail-shop-case-study-dataset\\prod_cat_info.csv',
                    header = TRUE, na.strings=c("","NA"), sep = ",")


transaction = read.csv('C:\\Users\\Acer\\Desktop\\USB\\Marc - DSA\\Projects\\retail-shop-case-study-dataset\\Transactions.csv',
                   header = TRUE, na.strings=c("","NA"), sep = ",")




#========================== Merging datasets ==================================


customer1 = merge(x = customer, y = transaction, by.x = "customer_Id", by.y = "cust_id")

full_data = merge(x = customer1, y = product, by.x = c("prod_cat_code", "prod_subcat_code"),
                  by.y = c("prod_cat_code", "prod_sub_cat_code"))



#===================== Cleaning the dataset full_data ====================


full_data1 = data.frame(Customer_Id = full_data$customer_Id, DOB = as.Date(full_data$DOB,format='%m/%d/%Y'),
                        Gender = full_data$Gender, City_code = full_data$city_code, Transaction_id = full_data$transaction_id,
                        Transaction_date = as.Date(full_data$tran_date,format='%m/%d/%Y'), Quantity = full_data$Qty, Total_amt = full_data$total_amt,
                        Store_type = full_data$Store_type, Prod_cat = full_data$prod_cat, Prod_sub_cat = full_data$prod_subcat)



#----------------- find if missing values ; ----------------------------


mv = full_data1[!complete.cases(full_data1),] #---------17 rows have at least one missing values. Those can be deleted from the dataset

nmv = full_data1[complete.cases(full_data1),] #--------- Our dataset will be sliced from this data


write.table(nmv, "clean_data.csv", sep = ",", row.names = FALSE)


#======================== Slicing data: keeping data for 2013 and 2014 ==================


Rdata = nmv[nmv$Transaction_date >= as.Date('01/01/2013', format = '%m/%d/%Y') & (nmv$Total_amt>=0),]

#------- Rdata is the final dataset with 7727 rows and 11 variables

write.table(Rdata, 'final_data.csv', sep = ",", row.names = FALSE)


#========================== 1) Basic data summary analysis ===================================

attach(Rdata)

sample = Rdata[sample(nrow(Rdata),5),] #-------------data sample-------------

str(Rdata)

summary(Rdata)

max(Transaction_date) #--------- last transaction date in the dataset ---------



#----- graphs ---------------------

layout(matrix(c(1,1), nrow = 1, ncol = 1, byrow = TRUE))

hist(Rdata$Total_amt, xlab = "Total Amount", main = "Sales amount distrbution", col = "blue")



#========================= 2) Relations between variables transaction amount with age and gender =============

#--------------- a) creating age column ------------------------


Rdata_age = cbind(Rdata, Age = round((max(Transaction_date)-DOB)/365))



#----------------- b) Gender analysis with transaction amount-------------

table_gender_amt_sum = aggregate(Total_amt ~ Gender, data = Rdata_age, FUN = sum)

table_gender_amt_mean = aggregate(Total_amt ~ Gender, data = Rdata_age, FUN = mean)




#----------------- creating age group and analysis ---------------------------------------



x = as.numeric(Rdata_age$Age)

age_group= c()
i=1

while(i <= length(x)){

if(x[i]<25){

  age_group[i] = "less than 25 YRS"

}else if (x[i]<=35){

  age_group[i] = "25 to 35 YRS"
}else if (x[i]>35){

  age_group[i] = "over 35 YRS"
}

  i = i+1

}

Rdata_age_group=cbind(Rdata_age, Age_group = as.factor(age_group))





table_agegrp_sum = aggregate(Total_amt ~ Age_group, data = Rdata_age_group, FUN = sum)

table_agegrp_mean = aggregate(Total_amt ~ Age_group, data = Rdata_age_group, FUN = mean)



table_gender_agegrp_mean = tapply(Rdata_age_group$Total_amt, list(Rdata_age_group$Gender, Rdata_age_group$Age_group), mean) #---get the average amount spent by age_group


#---------- graph -----------------------

z=table(Rdata$Gender)
s=table(Rdata_age_group$Age_group)

pie(z, labels = paste(names(z), "", round(z)), main = "Gender distribution", col = c("#76448a", "#b3b6b7") )

pie(s, labels = paste(names(s), "/", round(s/sum(s)*100, digits = 0), "%"), main = "Age group distribution", col = c("blue", "yellow", "red") )



#====================== 3) Which product category is more popular? ========================



table_prodcat_sum = as.matrix(prop.table(by(Rdata$Total_amt, Rdata$Prod_cat, sum)))
table_prodcat_sum



#---------- graph -----------------

layout(matrix(1), 1, 1)

pie(table_prodcat_sum, labels =
      paste(names(table_prodcat_sum[1:4,]), "", round(table_prodcat_sum[,1]*100, digits = 1), '%'),
    main =  "% of sales for each product category", cex = 1.2,
    col = c("#b03a2e", "#76448a", "#1f618d", "#148f77", "#b7950b", "#b3b6b7"))


#======================= 4) Product category distribution within gender =====================


table_prodcat_gender = addmargins(round(xtabs(Total_amt~ Prod_cat + Gender, data = Rdata)), FUN = mean)


table_prodcat_gender

#--------------- graph ----------------------------------

install.packages("ggplot2")
library(ggplot2)


abc = xtabs(Total_amt ~ Gender + Prod_cat, data = Rdata_age_group)





layout(matrix(c(1,2), nrow = 1, ncol = 2), widths = c(0.80,0.20))


par(mar = c(5, 4, 5, 1))

barplot(abc, main = "Product category distribution within gender", beside = TRUE, 
        xlab = "Product category", ylab = "Sales", col = c("blue", "green"))


par(mar = c(5, 0, 2, 1))
plot(0,150000,col="white",axes=FALSE,ann=FALSE)
legend("topleft",c("Female","Male"),cex=1,
       fill = c("blue", "green"))






#======================= 5) customer segmentation : age group per city =====================

table_agegrp_city = table(age_group, City_code)
table_agegrp_city

#-----------Graph-------

layout(matrix(c(1,2), nrow = 1), widths = c(0.7, 0.3))
par(mar = c(5, 4, 2, 1))

barplot(table_agegrp_city, width = 1,
        xlab = "City", ylab = "Number of customers", offset = 3, col = c("#a04000", "#1e8449", "#2874a6") ) 

par(mar = c(5, 0, 2, 1))

plot(x,y,col="white",axes=FALSE,ann=FALSE)
legend(x=-2.2,y=11,c("25 to 35 YRS","Less than 25 YRS", "Over 35 YRS"),cex=.8,
       col=c("#a04000", "#1e8449", "#2874a6"),fill=c("#a04000", "#1e8449", "#2874a6"), title = "Age group")



#============================ 6) Which city is spending more? ==============================



table_city_amt = aggregate(Total_amt ~ City_code, data = Rdata_age, FUN = mean)

#-------- Graph -----------


layout(matrix(c(1,1), nrow = 1))

par(mar = c(5, 4, 2, 1))

plot(table_city_amt$City_code, table_city_amt$Total_amt, main = "Sales mean for each city", pch = 20, col="red", xlab="City", ylab="Sales mean", type = "b")



#======================== 7) Sales amount per store type ===============================


table_store_totalamt = aggregate(Total_amt ~ Store_type, data = Rdata, FUN = sum)


table_store_totalamt_prop = as.matrix(prop.table(xtabs(Total_amt ~ Store_type, data = Rdata)))




#----------- graph ---------------

qplot(Total_amt, data = Rdata, geom = "histogram", fill = Store_type)

pie(table_store_totalamt_prop, cex = 1.2, labels =
    paste(names(table_store_totalamt_prop[1:4,]), "", round(table_store_totalamt_prop[,1]*100, digits = 1), '%'),
    main =  "Percentage of sales for each store type",
    col = rainbow(length(table_store_totalamt_prop)))



#======================== 8) Relation between store type and product category ==================



table_store_prod = round(addmargins(xtabs(Quantity~ Store_type + Prod_cat, data = Rdata), FUN = mean), digits = 0)



#------------ graph --------------


layout(matrix(c(1,2), nrow = 1), widths = c(0.8,0.2))


par(mar = c(5, 4, 5, 1))
barplot(table_store_prod, beside = TRUE, main = "Product category quantity per store", 
        col = c("#8B0000", "#FF8C00", "#006400", "#008B8B", "#2F4F4F"), xlab = "Product category", ylab = "Quantity")

par(mar = c(5, 0, 2, 1))
plot(x,y,col="white",axes=FALSE,ann=FALSE)
legend("topleft",c("eshop","Flagship Store", "MBR", "Teleshop", "Mean"), cex = 0.8,
       fill = c("#8B0000", "#FF8C00", "#006400", "#008B8B", "#2F4F4F"))



#======================== 9) Quantity sold per month ===============================

install.packages("lubridate")
library(lubridate)

Rdata_month_year =cbind(Rdata_age_group,
                        trans_month = month(as.POSIXlt(Rdata_age_group$Transaction_date, format="%y-%m-%d")),
                        trans_year = year(as.POSIXlt(Rdata_age_group$Transaction_date, format="%y-%m-%d")))



y2 = xtabs(Quantity ~ trans_month, data = Rdata_month_year)



#------------- graph --------------------

layout(matrix(c(1,1), nrow = 1))


par(mar = c(5, 4, 5, 1))
barplot(y2, beside = TRUE, main = "Quantity sold per month", 
        col = 1, xlab = "Month", ylab = "Quantiy")




#======================= 10) Total sales per month for each year ================




y1 = xtabs(Total_amt ~ trans_year + trans_month, data = Rdata_month_year)



#----------- graph ---------------

layout(matrix(c(1,2), nrow = 1), widths = c(0.85,0.15))


par(mar = c(5, 4, 5, 1))
barplot(y1, beside = TRUE, main = "Sales amount per month", 
        col = 1:2, xlab = "Month", ylab = "Sales")

par(mar = c(5, 0, 2, 1))
plot(x,y,col="white",axes=FALSE,ann=FALSE)
legend("topleft",c("2013","2014"),cex=1.2,
       fill = 1:2)


