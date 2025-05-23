
install.packages("readr")
install.packages('ggplot2')
library(ggplot2) 
install.packages('dplyr')
library(dplyr)
library(readr)

#Dataset Description:

thyroid<- read_csv("/Users/urvigandhi/Desktop/Thyroid_Diff.csv")
View(thyroid)
head(thyroid)
str(thyroid)
summary(thyroid)
dim(thyroid)
nrow(thyroid)
ncol(thyroid)
typeof(thyroid)
names(thyroid)

#missing values
thyroid$Age[is.na(thyroid$Age)] <- mean(thyroid$Age, na.rm = TRUE)

missing_values <- colSums(is.na(thyroid))
missing_values

#mutation
thyroid <- thyroid %>%
  mutate_if(is.numeric, ~ifelse(is.na(.), mean(., na.rm = TRUE), .)) %>%
  mutate_if(is.character, ~ifelse(is.na(.), as.character(mode(.)), .))
 thyroid

#Factor
smoking_history<- thyroid[thyroid$Smoking == "Yes", ]
smoking_history


adenopathy_df<- thyroid[thyroid$Adenopathy=='Right', ]
adenopathy_df

thyroid$Gender <- as.factor(ifelse(thyroid$Gender == "male", 1, 0))
thyroid$Smoking <- as.factor(ifelse(thyroid$Smoking == "yes", 1, 0))

indices <- which(thyroid$Hx_Smoking == "yes")


thyroid$Adenopathy <- as.factor(ifelse(thyroid$Adenopathy == "yes", 1, 0))
thyroid$Pathology <- as.factor(thyroid$Pathology)
thyroid$Focality <- as.factor(thyroid$Focality)
thyroid$Risk <- as.factor(thyroid$Risk)
thyroid$Response <- as.factor(thyroid$Response)
thyroid$Recurred <- as.factor(thyroid$Recurred)



unique(thyroid$Pathology)
thyroid$Pathology <- factor(thyroid$Pathology, levels = c('Micropapillary', "Papillary", "Follicular", 'Hurthel Cell'))
str(thyroid)
head(thyroid, 20)

levels(thyroid$Pathology)



#data visualisation

'''1. Histogram of age'''
ggplot(thyroid, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "yellow", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")


'''2. Gender distribution'''

ggplot(thyroid, aes(x = Gender)) +
  geom_bar(fill = "pink", color = "black") +
  labs(title = "Gender Distribution", x = "Gender", y = "Count")


'''3. Boxplot of Age by Response'''

ggplot(thyroid, aes(x = Response, y = Age, fill = Response)) +
  geom_boxplot() +
  labs(title = "Boxplot of Age by Response", x = "Response", y = "Age")


'''4. Correlation between Age and Tumor Size (T)'''

ggplot(thyroid, aes(x = Age, y = T)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Correlation between Age and Tumor Size (T)",
       x = "Age", y = "Tumor Size (T)",
       color = "Linear Regression Line")

'''5. Gender ditribution'''

ggplot(thyroid, aes(x = Gender)) +
  geom_bar(fill = "lightgreen", width = 0.7) +
  labs(title = "Distribution of Gender", x = "Gender", y = "Count")


'''6. Smoking distribution'''

ggplot(thyroid, aes(x = Smoking)) +
  geom_bar(fill = "purple", width = 0.7) +
  labs(title = "Distribution of Smoking", x = "Smoking", y = "Count")


'''7. Pathology Distribution'''

ggplot(thyroid, aes(x = Pathology)) +
  geom_bar(fill = "pink", width = 0.7) +
  labs(title = "Distribution of Pathology", x = "Pathology", y = "Count")


'''8. CDistribution of Risk'''

ggplot(thyroid, aes(x = Risk)) +
  geom_bar(fill = "green", width = 0.7) +
  labs(title = "Distribution of Risk", x = "Risk", y = "Count")


'''9. Distribution of response'''

ggplot(thyroid, aes(x = Response)) +
  geom_bar(fill = "orange", width = 0.7) +
  labs(title = "Distribution of Response", x = "Response", y = "Count")


'''10. Distribution of Recurred'''
# Recommended way (with ggplot)
ggplot(data = thyroid, aes(x = Age)) +
  geom_histogram()


#ggplot(thyroid, aes(x = Recurred)) +
 # geom_bar(fill = "yellow", width = 0.7) +
  #labs(title = "Distribution of Recurred", x = "Recurred", y = "Count")

'''11. Age vs Smoke'''
qplot(thyroid$Age,thyroid$Smoking,main="Age vs Smoking ")

'''12. AGE VS PHYSICAL EXAMINATION'''
library(ggplot2)
library(dplyr)

# Assuming `thyroid` is your dataset
#qplot(thyroid$Age,thyroid$Physical.Examination,main="Age vs Physical Examination",xlab = "Age",ylab="Physical Examination")
#ggplot(thyroid,aes(thyroid$Age,ty$Physical.Examination))+geom_boxplot()

'''13. Age vs Recurred'''
ggplot(thyroid,aes(thyroid$Age,thyroid$Recurred))+geom_boxplot()


'''14. Scatterplot of Age vs. Another Numerical Variable'''
ggplot(thyroid, aes(x=Age, y=Stage)) +
  geom_point(color="skyblue") +
  labs(title="Scatterplot of Age vs. Another Numerical Variable", x="Age", y="Another Numerical Variable")

# Statistical Analysis
'''ANOVA'''
anova_result <- aov(Age ~ Response, data = thyroid)
summary(anova_result)


mean_age <- data_4127 %>%
  group_by(Gender) %>%
  summarize(mean_age = mean(Age, na.rm = TRUE))

print(mean_age)

cross_tab <- table(thyroid$Gender, thyroid$Smoking)
print(cross_tab)

cross_tab <- table(thyroid$Adenopathy, thyroid$Gender)
print("Cross-tabulation of Adenopathy and Gender:")
print(cross_tab)

adenopathy_by_smoking <- thyroid %>%
  group_by(Smoking, Adenopathy) %>%
  summarize(count = n()) %>%
  mutate(proportion = count / sum(count))

print("Proportion of Adenopathy by Smoking status:")
print(adenopathy_by_smoking)


'''t-test'''
t_test_agegender<-t.test(Age ~ Gender, data = thyroid)
t_test_agesmoking<-t.test(thyroid$Age ~ thyroid$Smoking)


#pathology:
missing_values <- sum(is.na(thyroid$Pathology))
print(paste("Missing values for Pathology:", missing_values))

ggplot(thyroid, aes(x = Pathology, fill = Pathology)) +
  geom_bar() +
  labs(title = "Distribution of Pathology", x = "Pathology", y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Statistical Analysis

cross_tab <- table(thyroid$Pathology, thyroid$Risk)
print("Cross-tabulation of Pathology and Risk:")
print(cross_tab)

pathology_by_gender <- thyroid %>%
  group_by(Gender, Pathology) %>%
  summarize(count = n(), .groups = "drop") %>%
  mutate(proportion = count / sum(count))

print("Proportion of Pathology types by Gender:")
print(pathology_by_gender)























