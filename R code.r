**IMPORTING LIBRARY**

```{r warning=FALSE}
library(tidyverse)
```

**LOADING DATA**

```{r reading the csv file}
cvd <- read.csv("/kaggle/input/cardiovascular-diseases-risk-prediction-dataset/CVD_cleaned.csv")

```

```{r}
# Showing the first 6 rows

 head(cvd)
 
```

**Preparing the data for analysis**

```{r}
 # checking for missing values and duplicates rows
 
 sum(is.null(cvd))

 duplicates <- sum(duplicated(cvd)) 

duplicates

```



```{r}
 # removing duplicates

cvd <- unique(cvd)

# confirming theere are no more duplicates

duplicates <- sum(duplicated(cvd))
duplicates
```

**overview of the data**

```{r}
str(cvd)
```

  **Categorical Data Analysis**
  
**General Health**
```{r}
unique(pull(cvd,General_Health))

GH <- cvd %>%
  group_by(General_Health) %>% 
  select(General_Health) %>% 
  count(General_Health) %>% 
  mutate(percentage = paste0(round((n/nrow(cvd)*100),1),"%"))


ggplot(data = GH, mapping = aes(reorder(General_Health,-n),n))+
  geom_bar(stat = "identity", fill = "light blue") +
  geom_text(aes(label = percentage),vjust=0.05)+
  xlab("General Health")+
  ylab("count") +
  labs(title = "Distribution of general health among respondents")

```

**Checkups**
```{r}
unique(pull(cvd,Checkup))

 df <- cvd %>%
  group_by(Checkup) %>% 
  select(Checkup) %>% 
  count(Checkup)
 
ggplot(data = df, mapping = aes(y= reorder(Checkup,-n),x=n,fill = Checkup))+
  geom_bar(stat = "identity",width = 0.7) +
  xlab("count")+
  ylab("checkup") +
  labs(title = "Distribution of checkups among respondents")

```

**Exercise**
```{r}
unique(pull(cvd,Exercise))

df2 <- cvd %>%
  group_by(Exercise) %>% 
  select(Exercise) %>% 
  count(Exercise) %>% 
  mutate(percentage = paste0(round((n/nrow(cvd)*100),1),"%"))
 


  ggplot(data = df2, mapping =  aes(x ="",y = percentage,fill=Exercise)) +
    geom_bar(stat = "identity",width = 1) +
    geom_text(aes(label = percentage), position = position_stack(vjust = 0.5)) +
    coord_polar("y", start = 0) +
    labs( x= NULL, y = NULL, title = "Exercise participation rate of all respondents") +
    theme_void()
```


**Age category vs Arthritis**
```{r}
A1 <- cvd %>% 
    select(Age_Category, Arthritis) %>%
    filter(Arthritis == "Yes") %>% 
    group_by(Age_Category) %>% 
    count(Arthritis)
ggplot(data = A1, mapping = aes(x=reorder(Age_Category,-n),y = n,fill = Age_Category)) +
  geom_bar(stat = "identity") +
  labs(title = "Relationship between age category and Arthritis") +
  xlab("Age category") 
```


**Depression distribution among respondents**
```{r}
unique(pull(cvd,Depression))

D <- cvd %>%
  group_by(Depression) %>% 
  select(Depression) %>% 
  count(Depression) %>% 
  mutate(percentage = paste0(round((n/nrow(cvd)*100),1),"%"))

ggplot(data = D, mapping = aes(x= Depression, y = percentage,fill = Depression))+
  geom_bar(stat = "identity") +
  geom_text(aes(label = percentage),position = position_stack(vjust = 1.03))+
  xlab("Depression")+
  ylab("percentage") +
  labs(title = "General Depression Distribution among respondents")
```


**Rate of Depression based on gender(depressed respondent)**
```{r}
A2 <- cvd %>% 
    select(Sex, Depression) %>% 
    filter(Depression=="Yes") %>% 
    group_by(Sex) %>% 
    count(Depression) %>% 
    mutate(percentage = paste0(round((n/nrow(cvd)*100),1),"%"))
  
 ggplot(data = A2, mapping = aes(x="", y=n,fill = Sex)) +
   geom_bar(stat = "identity",width =1) + 
   geom_text(aes(label = percentage),position = position_stack(vjust = 0.5)) +
   coord_polar("y", start = 0) +
   theme_void() +
   labs(title = "Depression rate by gender")
```


 **Numerical Data Analysis**
 
```{r}
 num <- cvd %>% 
   select(Height_.cm.:FriedPotato_Consumption,-(Smoking_History))
head(num)
```
 
 **Statistical view of the data**
 
```{r}
summary(num)
```
 
 **correlation Analysis**
```{r}
  library(reshape2)
 
cor <- cor(num)

 melted_cor <- melt(cor) 

 head(melted_cor) 

 ggplot(data = melted_cor, mapping = aes(x= Var1, y =Var2, fill = value)) +
   geom_tile() +
   scale_fill_gradient(low = "red",high = "green") +
   geom_text(aes(label = round(value,3))) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
   
 
```
 
