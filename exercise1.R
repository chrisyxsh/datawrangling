
library(tidyr)
library(dplyr)
refine_data <- read.table("refine_original.csv", header=TRUE, 
                          sep=",")
refine_data$company<-tolower(refine_data$company)

refine_data<-separate(refine_data, Product.code...number, c("product_code", "product_number"), sep = "-")
explain_pdcode<-function(x){
  y=c()
  for (value in x){
    if (value=="p"){
      y<-c(y,"Smartphone")
    }else if(value=="v"){
      y<-c(y,"TV")
    }else if(value=="x"){
      y<-c(y,"Laptop")
    }else if(value=="q"){
      y<-c(y,"Tablet")
    }else{
      y<-c(y,NULL)
    }
  }
  return(y)
}
dummy_gen<-function(company_name,checked_name){
  dummy=c()
  for (name in company_name){
    if (name==checked_name){
      dummy<-c(dummy,1)
    }else{
      dummy<-c(dummy,0)
    }
  }
  return(dummy)
}
c_philips="philips"
c_akzo="akzo"
c_unilever="unilever"
c_van_houten="van houten"
refine_data$product_category<-explain_pdcode(refine_data$product_code)
refine_data<-unite(refine_data, "full_address", address, city, country, sep = ",")
refine_data$company_philips <- dummy_gen(refine_data$company,c_philips) 
refine_data$company_akzo <- dummy_gen(refine_data$company,c_akzo) 
refine_data$company_unilever <- dummy_gen(refine_data$company,c_unilever)
refine_data$company_van_houten <- dummy_gen(refine_data$company,c_van_houten)

c_smartphone="Smartphone"
c_tv="TV"
c_laptop="Laptop"
c_tablet="Tablet"
refine_data$product_smartphone <- dummy_gen(refine_data$product_category,c_smartphone) 
refine_data$product_tv <- dummy_gen(refine_data$product_category,c_tv) 
refine_data$product_laptop <- dummy_gen(refine_data$product_category,c_laptop)
refine_data$product_tablet <- dummy_gen(refine_data$product_category,c_tablet)