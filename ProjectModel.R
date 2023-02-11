library(data.table)
library(skimr)
library(ggplot2)
library(caret)
library(MLmetrics)
library(dplyr)
library(magrittr)
library(ggthemes)
library(cowplot)
current_date <- Sys.Date()

#Enter the IBM dataset 
HRdfIBM <- fread("WA_Fn-UseC_-HR-Employee-Attrition.csv")

#With skim() we get intuition over our data
skim(HRdfIBM)

#We gonna classify some attributes by their respective values
HRdfIBM[ , `:=`(median_compensation = 
                  median(MonthlyIncome)),by = .(JobLevel) ]
HRdfIBM[ , `:=`(CompensationRatio =     (MonthlyIncome/median_compensation)), by =. (JobLevel)]
HRdfIBM[ , `:=`(CompensationLevel = 
                  factor(fcase(CompensationRatio 
                               %between% list(0.75,1.25), "Average",
                               CompensationRatio 
                               %between%  list(0,0.74), "Below",
                               CompensationRatio 
                               %between% list(1.26,2),  "Above"),
                         levels = c("Below","Average","Above"))),
         by = .(JobLevel) ]
HRdfIBM <- na.omit(HRdfIBM)

#Here we count by attrition the turnover
turnover_rate <- HRdfIBM[, list(count = .N, rate = (.N/nrow(HRdfIBM))), by = Attrition]
turnover_rate

set.seed(123)
HRdf <- HRdfIBM[sample(.N, 500)]

#We gonna see how many employees are satisfied with their jobs
theme_custom <- function(){ 
  
  theme(
    
    # background
    strip.background = element_rect(colour = "black", fill = "lightgrey"),
    
    # x-axis already represented by legend
    axis.title.x = element_blank(),               
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank(),                
    
    # legend box
    legend.box.background = element_rect())
  
}

# specify colors
library(RColorBrewer)
myCol <- rbind(brewer.pal(8, "Blues")[c(5,7,8)],
               brewer.pal(8, "Reds")[c(4,6,8)])



# Correlation: The higher employee's job satisfaction,the lower the turnover rate.
# Boxplot
plot_jobsatisfaction <- ggplot(HRdf, aes(x = Attrition, y = JobSatisfaction,fill = Attrition))+
  geom_boxplot(width=0.1)+
  scale_fill_manual(values = myCol)+
  ylab("Job Satisfaction")+
  xlab("Employee Attrition")+
  theme_custom()+
  ggtitle("Are leavers less satisfied than stayers?")

ggsave(plot_jobsatisfaction, file=paste0(current_date,"_","Distribution_Jobsatisfaction.png"), width = 15, height = 10, units = "cm")
plot_jobsatisfaction


HRdf[, list(count = .N, rate = (.N/nrow(HRdf))), by = JobSatisfaction]

#We get some further insight from our analysis in attrition based on other variables
library(vcd)
library(vcdExtra)

mosaic(~ Attrition + EnvironmentSatisfaction, data = HRdf,
       main = "Environment Satisfaction against Turnover", shade = TRUE, legend = TRUE)
mosaic(~ Attrition + JobInvolvement, data = HRdf,
       main = "Job Involvement against Turnover", shade = TRUE, legend = TRUE)
mosaic(~ Attrition + WorkLifeBalance, data = HRdf,
       main = "Work-Life-Balance against Turnover", shade = TRUE, legend = TRUE)
mosaic(~ Attrition + RelationshipSatisfaction, data = HRdf,
       main = "Relationship Satisfaction against Turnover", shade = TRUE, legend = TRUE)
options(repr.plot.width=8, repr.plot.height=7) 

#This visualization took place with dplyr in order to be easier to read
per.sal <- HRdf %>% select(Attrition, PercentSalaryHike, MonthlyIncome) %>% 
  ggplot(aes(x=PercentSalaryHike, y=MonthlyIncome)) + geom_jitter(aes(col=Attrition), alpha=0.5) + 
  theme_economist() + theme(legend.position="none") + scale_color_manual(values=c("#58FA58", "#FA5858")) + 
  labs(title="Income and its Impact on Attrition") + theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
                                                           axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                                                           axis.title=element_text(colour="white"))

perf.inc <- HRdf %>% select(PerformanceRating, MonthlyIncome, Attrition) %>% group_by(factor(PerformanceRating), Attrition) %>% 
  ggplot(aes(x=factor(PerformanceRating), y=MonthlyIncome, fill=Attrition)) + geom_violin() + coord_flip() + facet_wrap(~Attrition) + 
  scale_fill_manual(values=c("#58FA58", "#FA5858")) + theme_economist() + 
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"), 
        legend.text=element_text(color="white")) + 
  labs(x="Performance Rating",y="Monthly Income") 


plot_grid(per.sal, perf.inc, nrow=2)

#Lets Pre-process our data 
#Look at data to find variables that probably do not have any predictive power
colnames(HRdf)

#Clean up data
HRdf<- HRdf[,-c("DailyRate","EducationField",  "EmployeeCount","EmployeeNumber","MonthlyRate","StandardHours","TotalWorkingYears","StockOptionLevel","Gender","Over18", "OverTime", "median_compensation")]
HRdf_reduced <- as.data.frame(unclass(HRdf),stringsAsFactors=TRUE)

# find numeric values
nums <- unlist(lapply(HRdf_reduced, is.numeric))
# save numeric variables for later
HRdf_nums <- HRdf_reduced[,nums]
# show numeric variables
head(HRdf_nums)
# calculate correlation matrix
correlationMatrix <- cor(HRdf_nums)
# summarize the correlation matrix
correlationMatrix
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print colnames of highly correlated attributes
colnames(HRdf_nums[,highlyCorrelated])
correlationMatrix[,highlyCorrelated]

#remove highly correlated variables to overcome multicollinearity
colnames(HRdf_nums)
highlyCorrelated <- c(1,7,11,16,17,19)
HRdf_nums <- HRdf_nums[,-highlyCorrelated]

# select factor variables to convert, but leave Attrition out
vars_to_dummy <- HRdf_reduced[,sapply(HRdf_reduced, is.factor) & colnames(HRdf_reduced) != "Attrition"]
head(vars_to_dummy)
# Create dummy variables with caret
dummies <- dummyVars( ~ ., data = vars_to_dummy)
HRdf_dummy <- predict(dummies, newdata = vars_to_dummy)
# New dataframe to work with later
HRdf_sample <- data.frame(HRdf_dummy, HRdf_nums, Attrition = HRdf_reduced$Attrition)
View(HRdf_sample)

# remove near zero variables (except for attr)
remove_cols <- nearZeroVar(HRdf_sample, names = TRUE)
remove_cols
# Get all column names 
all_cols <- names(HRdf_sample)
# Remove from data
HRdf_final<- HRdf_sample[ , setdiff(all_cols, remove_cols)]
# make sure that factor levels of attrition are correctly ordered
levels(HRdf_final$Attrition)
HRdf_final$Attrition <- factor(HRdf_final$Attrition, levels = c("Yes", "No"))
# double check
levels(HRdf_final$Attrition)


#Data folds for cross-validation
myFolds <- createFolds(HRdf_final$Attrition, k = 5)

#Control function for the model
myControl <- trainControl(
  method = "cv", 
  number= 15, 
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE,
  savePredictions = "final",
  index = myFolds
)

#Fit random forest model
model_baseline <- caret::train(
  Attrition ~ MonthlyIncome + JobSatisfaction + MonthlyIncome*JobSatisfaction,
  data = HRdf_final,
  method = "rf",
  tuneLength = 10,
  trControl = myControl
)


model_baseline$results
summary(model_baseline)



# Create confusion matrix
base_Pred <- predict.train(model_baseline, HRdf_final)
confusionMatrix(base_Pred, HRdf_final$Attrition, mode = "prec_recall")

#Get indices of employees that still work for the company
still_active <- setDT(HRdf_final)[Attrition == "No", which = TRUE]
probs <- predict.train(model_baseline, HRdf_final, type = "prob")
# Save predicted probs for still active employees
risks <- probs[still_active,]
emp_active <- HRdf[still_active,]
# Save row index
risks$index <- 1:nrow(risks)
#find employees who may be at risk leaving the company
emp_risk <-setDT(risks)[order(-Yes)]
emp_risk
# show employee data who could leave soon
emp_indices <- emp_risk$index
top_10 <- head(emp_active[emp_indices,],10)
View(top_10)
