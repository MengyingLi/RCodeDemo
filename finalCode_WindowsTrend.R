Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_71")

library(dplyr)
library(data.table)
library(zoo)
library(xlsx)
library(RJSONIO)
library(stringr)
setwd("E:/GameReport")

load("./PC_Phone_Nov_Dec.rda")

#Change the data format for Steve's data
changeDataFormat = function(x,...){
filter_crit = lazyeval::interp(~ filter_var != ""  &!is.na(filter_var) & filter_var!= 0.0 & filter_var!="n/a" ,filter_var = as.name(...))
example = select_(x, "CountDevicesScaled", "Month", "Region", "IsGamer", "FormFactor", ...) %>% 
filter_(filter_crit) %>% group_by_("Month", "Region", "IsGamer", "FormFactor", ...) %>% summarise(Value = sum(CountDevicesScaled, na.rm = TRUE)) %>% mutate(Percentage = Value/sum(Value))
example = as.data.frame(example)
colnames(example)[5] = "SubCategory"
example$Category = as.character(...)
if(as.character(...) == "OsInstallBase"){

example = group_by(example,Month,Region, IsGamer, FormFactor, Category) %>% slice(match(OsInstallBaseOrder, SubCategory))


} else if (as.character(...) == "Storage"){
example = group_by(example, Month,Region, IsGamer, FormFactor, Category) %>% slice(match(StorageOrder, SubCategory))

} else if (as.character(...) == "Memory"){
example = group_by(example, Month,Region, IsGamer, FormFactor, Category) %>% slice(match(MemoryOrder, SubCategory))
}  else if (as.character(...) == "Resolution"){
example = group_by(example,Month,Region, IsGamer, FormFactor, Category) %>% slice(match(ResolutionOrder, SubCategory))

} else  if (as.character(...) == "DirectXSupport"){
example = group_by(example, Month,Region, IsGamer, FormFactor, Category) %>% slice(match(DirectXSupportOrder, SubCategory))
}
return(example)
}









#********************************************************* Load Steve data *************************************************#
StorageOrder = c("<=64 GB", "128 GB"  , "250 GB",  "500 GB" ,"1 TB","2 TB", ">2 TB" )
MemoryOrder = c("<=2 GB", "3 GB", "4 GB",  "8 GB", "16 GB", ">16 GB")
ResolutionOrder = c("<720p", "720p", "1080p", "1440p", ">=4k" )
OsInstallBaseOrder = c( "Windows 7" , "Windows 8" ,"Windows 8.1", "Windows 10")
DirectXSupportOrder = c( "9_3 or lower", "10", "11+" )
OsInstallBaseOrder_Phone = c("Windows Phone 8", "Windows Phone 8.1")
pcCategory = c( "OsInstallBase", "Storage", "Memory", "Resolution", "DirectXSupport")
newMonth = "Feb"
newPcdataPath = paste("HWSurvey_", newMonth, "_PC.csv", sep = "") 
newPcMonth = "2/1/2016  12:00:00 AM"
newPCData = read.csv(newPcdataPath, header = TRUE, as.is = TRUE, stringsAsFactors = FALSE)
pcData_New = newPCData %>% rename(OsInstallBase = OSName, Storage = TotalDiskCapacity, Memory = Memsize, Resolution = ResolutionClass,DirectXSupport =  MaxDXLevelSupport, FormFactor = FormFactorFamily ) %>% 
mutate( DirectXSupport = ifelse(DirectXSupport <= 9.3, "9_3 or lower",ifelse((DirectXSupport == 10 |DirectXSupport == 10.1), 10,"11+")),
Resolution = ifelse(str_detect(Resolution, "4k"), ">=4k", Resolution), 
Memory = ifelse(str_detect(Memory, "2 GB"), "<=2 GB", Memory),
Storage = ifelse(str_detect(Storage,"64 GB"), "<=64 GB", Storage),
OsInstallBase = ifelse(OsInstallBase == "Windows 7 RTM"|OsInstallBase == "Windows 7 SP1", "Windows 7", ifelse(OsInstallBase == "Windows 8 RTM", "Windows 8",ifelse(OsInstallBase == "Windows Phone 8 RTM", "Windows 8", ifelse(OsInstallBase == "Windows Phone 8.1", "Windows 8.1", OsInstallBase)))),
FormFactor = ifelse(FormFactor == "Tablet","PC",FormFactor)) %>% filter(!(FormFactor=="Phone"& OsInstallBase == "Windows Phone 10"))
pcData_New_gamer = pcData_New %>% filter(IsGamer == TRUE) %>% mutate(IsGamer = "Gamers")
pcData_New_all = mutate(pcData_New, IsGamer = "All")
pcData_New = rbind(pcData_New_gamer,pcData_New_all)


pcData_New = rename(pcData_New, CountDevicesScaled = CountDevices)
pcResult_List_New = lapply(pcCategory, function(x)changeDataFormat(pcData_New,x))
pcResult_New = rbindlist(pcResult_List_New) %>% mutate(Month = as.yearmon(Month, format='%m/%d/%Y %H:%M'))
pcResult_New = mutate(pcResult_New, SubCategory = ifelse(FormFactor == "Phone"& SubCategory == "Windows 8", "Windows Phone 8",
ifelse(FormFactor == "Phone" & SubCategory == "Windows 8.1", "Windows Phone 8.1", SubCategory)))

pcResult_World = pcResult_New %>% 
  group_by(Month, IsGamer, FormFactor, SubCategory, Category) %>% 
  summarise(Value = sum(Value)) %>% group_by(Month, IsGamer, FormFactor, Category) %>% 
  mutate(Percentage = Value/sum(Value)) %>% 
  mutate(Region = "World")


#********************************************************* Load WSI Data  *************************************************#

IAP_PaidApps = read.csv("./WSI_Data/Jan.2016-Feb.2016/grossSaleNoCategory.csv")
IAP_PaidApps =IAP_PaidApps %>%  
  rename( FormFactor = Platform.Name, 
          Region = Area..A13., 
          Month = PeriodName, paidApps = Gross.Sales..DTO.,
          IAP = Gross.Sales..AddOn.) %>%
  mutate(
    Month =as.yearmon(Month,"%b'%y"),
    FormFactor = ifelse(FormFactor == "Windows.Desktop"|FormFactor == "Client", "PC", "Phone"),
    IsGamer = "All") %>% 
  filter(as.character(FormFactor)!= "Unknown"& as.character(FormFactor)!= "Unknown*" & !(paidApps ==0 & IAP == 0)) 

revenue_app_IAP = IAP_PaidApps  %>% group_by(FormFactor, Region, Month, IsGamer) %>% summarise(paidApps = sum(paidApps), IAP = sum(IAP))
library(reshape2)
revenue_app_IAP_melted = melt(revenue_app_IAP, id.vars = c("FormFactor", "Region", "Month", "IsGamer")) %>% 
  rename(SubCategory = variable, Value = value) %>% 
  mutate(Category = "Revenue mix of apps and IAPs", SubCategory = ifelse(SubCategory == "paidApps", "Paid Apps", "IAP")) %>%
  group_by(FormFactor, Region, Month, IsGamer, Category) %>% mutate(Percentage = Value/sum(Value))

revenue_app_IAP_melted_world = revenue_app_IAP_melted %>% group_by(Month, Category, SubCategory,FormFactor, IsGamer) %>% 
  summarise(Value = sum(Value)) %>% group_by(FormFactor, Month, IsGamer, Category)%>% 
  mutate(Percentage = Value/sum(Value), Region = "World")
# Gross Sales by Category

grossSales_Category = read.csv("./WSI_Data/Jan.2016-Feb.2016/grossSaleByCategory.csv", header = TRUE, as.is = TRUE, stringsAsFactors =  FALSE)

grossSales_Category = grossSales_Category %>% mutate(Product.Category = ifelse(is.na(Product.Category), "Unknown", Product.Category))
# introduce loren's table
mappingTableCategory = read.xlsx("Mapping Tables_Loren.xlsx", sheetIndex = 1)
colnames(mappingTableCategory) = c("Original", "SubCategory")
mappingTableCategory = mappingTableCategory %>% filter(!is.na(SubCategory))
mappingTableCategory$Original = tolower(mappingTableCategory$Original)
mappingTableCategory$Original = str_replace_all(mappingTableCategory$Original, "&", "")
grossSales_Category =grossSales_Category %>%  
  rename( FormFactor = Platform.Name, 
          Region = Area..A13., 
          Month = PeriodName, 
          Original = Product.Category ) %>%
  mutate(Month =as.yearmon(Month,"%b'%y"),
         FormFactor = ifelse(FormFactor == "Windows.Desktop"|FormFactor == "Client", "PC", "Phone"),
         IsGamer = "All", Original = tolower(Original)) %>% 
  filter(FormFactor!= "Unknown"& FormFactor!= "Unknown*") 

grossSales_Category = inner_join(grossSales_Category, mappingTableCategory) %>% select(-Original) # delete all the unknown

grossSales_Category = grossSales_Category %>% group_by(FormFactor, Region, SubCategory, Month, IsGamer) %>% summarise(sales = sum(Gross.Sales))
grossSales_Category_melted = melt(grossSales_Category, id.vars = c("FormFactor", "Region", "Month", "IsGamer", "SubCategory")) %>%
  rename(Category = variable, Value = value) %>% 
  mutate(Category ="App and IAP sales by category", 
         SubCategory = as.character(SubCategory))%>%
  group_by(Month, Region, Category, FormFactor, IsGamer) %>%
  arrange(desc(Value)) %>% 
  mutate(index = 1:n()) %>% 
  mutate(SubCategory = ifelse(index > 10, "Other", SubCategory)) %>% 
  select(-index) %>% group_by(FormFactor, Region, Month, IsGamer, SubCategory, Category) %>% summarise(Value = sum(Value)) %>% 
  group_by(FormFactor, Region, Month, IsGamer, Category)%>% 
  mutate(Percentage = Value/sum(Value))

grossSales_Category_melted_world = grossSales_Category_melted %>%
  group_by(Month, Category, SubCategory,FormFactor, IsGamer) %>% 
  summarise(Value = sum(Value)) %>% 
  group_by(Month, Category, FormFactor, IsGamer) %>%
  arrange(desc(Value)) %>% 
  mutate(index = 1:n()) %>% 
  mutate(SubCategory = ifelse(index > 10, "Other", SubCategory)) %>% 
  select(-index)%>%
  group_by(Month, FormFactor, IsGamer,Category, SubCategory) %>% 
  summarise(Value = sum(Value)) %>%
  group_by(FormFactor, Month, IsGamer, Category)%>% 
  mutate(Percentage = Value/sum(Value), Region = "World")



# Acquision by Category

acquisition_Category =  read.csv("./WSI_Data/Jan.2016-Feb.2016/acquisitionByCategory.csv", header = TRUE, as.is = TRUE, stringsAsFactors =  FALSE)
acquisition_Category =acquisition_Category %>%  
  rename( FormFactor = Platform.Name, 
          Region = Area..A13., 
          Month = PeriodName, 
          
          Original = Product.Category ) %>%
  mutate(Month =as.yearmon(Month,"%b'%y"),
         
         OSVersion = ifelse(FormFactor == "Windows.Desktop", "Windows 10", 
                            ifelse(FormFactor == "Client", "Windows 8.1", 
                                   ifelse(FormFactor == "Phone", "Windows 8.1", "Windows 10"))),
         FormFactor = ifelse(FormFactor == "Windows.Desktop"|FormFactor == "Client", "PC", "Phone"),
         IsGamer = "All", Original = tolower(Original)) %>% 
  filter(FormFactor!= "Unknown"& FormFactor!= "Unknown*") 

acquisition_Category_primary = inner_join(acquisition_Category, mappingTableCategory) %>% select(-Original) # delete all the unknown

acquisition_Category_primary = acquisition_Category_primary %>% group_by(FormFactor, Region, SubCategory, Month, IsGamer) %>% summarise(download = sum(Acquisitions))
acquisition_Category_melted = melt(acquisition_Category_primary, id.vars = c("FormFactor", "Region", "Month", "IsGamer", "SubCategory")) %>%
  rename(Category = variable, Value = value) %>% 
  mutate(Category ="App downloads by category", 
         SubCategory = as.character(SubCategory))%>%
  group_by(Month, Region, Category, FormFactor, IsGamer) %>%
  arrange(desc(Value)) %>% 
  mutate(index = 1:n()) %>% 
  mutate(SubCategory = ifelse(index > 10, "Other", SubCategory)) %>% 
  select(-index) %>%  %>% group_by(FormFactor, Region, Month, IsGamer, SubCategory, Category) %>% summarise(Value = sum(Value)) %>%
  group_by(FormFactor, Region, Month, IsGamer, Category)%>% 
  mutate(Percentage = Value/sum(Value))

acquisition_Category_melted_world = acquisition_Category_melted %>%
  group_by(Month, Category, SubCategory,FormFactor, IsGamer) %>% 
  summarise(Value = sum(Value)) %>% 
  group_by(Month, Category, FormFactor, IsGamer) %>%
  arrange(desc(Value)) %>% 
  mutate(index = 1:n()) %>% 
  mutate(SubCategory = ifelse(index > 10, "Other", SubCategory)) %>% 
  select(-index)%>%
  group_by(Month, FormFactor, IsGamer,Category, SubCategory) %>% 
  summarise(Value = sum(Value)) %>%
  group_by(FormFactor, Month, IsGamer, Category)%>% 
  mutate(Percentage = Value/sum(Value), Region = "World")

#Acquisition by OS Version
acquisition_byOS = acquisition_Category %>% filter(!(FormFactor == "Phone" & OSVersion == "Windows 10") )%>%
  rename(SubCategory = OSVersion)%>% mutate(Category = "App and IAP downloads by OS version", SubCategory = ifelse(FormFactor == "Phone" & SubCategory == "Windows 8.1", "Windows Phone 8.1", SubCategory) ) %>% 
  group_by(FormFactor, Region, Month, IsGamer,SubCategory, Category) %>% summarise(Value = sum(Acquisitions)) %>% as.data.frame(.) %>%
  group_by(FormFactor, Region, Month, IsGamer, Category) %>%
  mutate(Percentage = Value/sum(Value))
acquisition_byOS_world = acquisition_byOS %>%  group_by(Month, Category, SubCategory,FormFactor, IsGamer) %>% 
  summarise(Value = sum(Value)) %>% group_by(FormFactor, Month, IsGamer, Category)%>% 
  mutate(Percentage = Value/sum(Value), Region = "World")








#******************************* Combine all the files together *********************************************#


finalResult_recent = rbind(pcResult_New,pcResult_World,
                             revenue_app_IAP_melted,  revenue_app_IAP_melted_world,
                             grossSales_Category_melted, grossSales_Category_melted_world,
                           acquisition_Category_melted ,acquisition_Category_melted_world,
                             acquisition_byOS, acquisition_byOS_world)

finalResult = rbind(finalResult, finalResult_recent) %>% mutate(Region = as.character(Region))
finalResult = mutate(finalResult, Region = ifelse(Region == "Central and Eastern Europe", "CEE", as.character(Region)))
#finalResult = rbind(finalResult, finalResult_world_recent, pcResult_New )
finalResult = arrange(finalResult, Category, Month) %>% mutate(Month = as.character(Month))


finalResult = finalResult %>% group_by(Month, Region, IsGamer, FormFactor, SubCategory, Category) %>% summarise(Value = sum(Value), Percentage = sum(Percentage))
write.csv(finalResult, file = "test_for_tableau_new_new.csv", row.names = FALSE)

save(finalResult, file = "Feb2016.rda")

pcResult_Feb = rbind(pcResult_New,pcResult_World) %>%mutate(Month = as.character(Month))



finalResult = rbind(finalResult, pcResult_Feb)










finalResult_0_recent = rbind( download_By_OS, revenue_app_IAP_melted)

finalResult_world_recent = finalResult_0_recent %>% 
group_by(Month, IsGamer, FormFactor, SubCategory, Category) %>% 
summarise(Value = sum(Value)) %>% group_by(Month, IsGamer, FormFactor, Category) %>% 
mutate(Percentage = Value/sum(Value)) %>% 
mutate(Region = "World")

finalResult = rbind(finalResult, finalResult_0_recent,finalResult_world_recent, app_category_melted)
#finalResult = rbind(finalResult, finalResult_world_recent, pcResult_New )
finalResult = arrange(finalResult, Category, Month) %>% mutate(Month = as.character(Month))

finalResult = mutate(finalResult, SubCategory = ifelse(SubCategory== "IAP Revenue", "IAP", ifelse(SubCategory == "App Revenue", "Paid Apps", SubCategory)))

write.csv(finalResult, file = "test_for_tableau_new.csv", row.names = FALSE)

save(finalResult, file = "PC_Phone_Nov_Dec.rda")
load("PC_Phone_Nov_Dec.rda")

# Transform it into JSON format

# All Category strings
categoryString = unique(finalResult$Category)
finalResult = finalResult[finalResult$Region!="Unknown"&finalResult$Region!="Unknown*", ]
finalResult$Region = as.character(finalResult$Region)
finalResult = as.data.table(finalResult)

# Transform the data to JSON format 
jsonFunction = function(p){
data = filter(finalResult, Category == p)
#monthString = unique(data$Month)
regionString = unique(data$Region)
formFactorString = unique(data$FormFactor)
#gamerString = unique(data$IsGamer)
nn =  list(Name = p, 
allData = lapply(regionString, function(z){
list(Region = z, 
regionData = lapply(unique(filter(data,Region==z)$FormFactor), function(x){
list(formFactor = x, 
formFactorData = lapply(unique(filter(data, Region ==z &FormFactor == x)$IsGamer), function(y){
list(gamer = y, 
gamerData = lapply(unique(filter(data, Region ==z&FormFactor == x&IsGamer ==y)$Month),function(v){
list(month = v,
monthlyData = as.data.frame(filter(data,Category == p&Month ==v&FormFactor ==x&Region == z&IsGamer == y)) %>%select(-Month, -FormFactor,-Region, -IsGamer,-Category,-Value) %>% rename(Category = SubCategory))
}))
}))
}))


}))
}



dd = lapply(categoryString,jsonFunction)
mm = toJSON(dd, pretty= TRUE)
write(mm, file = "jsontest2.json")

