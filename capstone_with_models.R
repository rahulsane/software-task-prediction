# Part 1: Exploratory Data Analysis

library(tidyverse)
library(ggpubr)

# Change path1 and path2 as required.
path1 <- '~/R/capstone/data/SiP_dataset-master/Sip-task-info.csv'
path2 <- '~/R/capstone/data/SiP_dataset-master/est-act-dates.csv'
software_tasks <- read_csv(path1)
software_dates <- read_csv(path2)

# See which priority levels are most common. We can see that 1 is most common, and 
#   levels 6-10 are given out very sparingly.
ggplot(software_tasks, aes(x = Priority)) + 
  geom_histogram() +
  theme_classic() +
  scale_x_continuous(breaks = round(seq(min(0), max(10), by=1))) +
  ggtitle('Distribution of Task Priorities')

# What are the distinct combinations of Category and Subcategory that we see:
unique_cats <- software_tasks %>% group_by(Category, SubCategory) %>% count() %>%
  arrange(desc(n))
print(unique_cats) # 26 combos

# See what the priority split is for the 4 most common tasks types.
common_tasks <- c('Enhancement', 'Bug', 'In House Support', 'Support')
for (task in common_tasks) {
  assign(task, filter(software_tasks, SubCategory == task))
}

e_plot <- ggplot(Enhancement, aes(x = Priority)) + 
  geom_histogram(bins = 10) + 
  ggtitle('Enhancement Priorities') + 
  scale_x_continuous(breaks = 1:10, labels = 1:10)

ihs_plot <- ggplot(`In House Support`, aes(x = Priority)) + 
  geom_histogram(bins = 10) + 
  ggtitle('In House Support Priorities') + 
  scale_x_continuous(breaks = 1:10, labels = 1:10)

b_plot <- ggplot(Bug, aes(x = Priority)) + 
  geom_histogram(bins = 10) + 
  ggtitle('Bug Priorities') + 
  scale_x_continuous(breaks = 1:10, labels = 1:10)

s_plot <- ggplot(Support, aes(x = Priority)) + 
  geom_histogram(bins = 10) + 
  ggtitle('Support Priorities') + 
  scale_x_continuous(breaks = 1:10, labels = 1:10)

ggarrange(e_plot, ihs_plot, b_plot, s_plot, labels=c('A', 'B', 'C', 'D'),
          ncol=2, nrow=2)

# Bug and Support tend to have a higher ratio of higher-priority tasks, e.g. they
#   both have a sugnificant number of Priority 5 tasks. Next, we can take a look at
#   some summary statistics for each category.
task_hours <- software_tasks %>%
  group_by(Category, SubCategory) %>%
  summarize(mean_priority = mean(Priority), mean_hours = mean(HoursActual), 
            median_hours = median(HoursActual)) %>%
  arrange(desc(median_hours))
ggplot(task_hours, aes(x = mean_priority, y = median_hours)) +
  geom_point() +
  theme_classic() +
  ggtitle('Mean Priority vs Median Hours per SubCategory') +
  xlab('Mean Priority') +
  ylab('Median Actual Hours')

unique_task_summary <- unique_cats %>% inner_join(task_hours)

# Let's take a step back and look at the hours for the total set.
normal_plot <- ggplot(software_tasks, aes(x = HoursEstimate, y = HoursActual)) +
  geom_point() +
  theme_classic() +
  ggtitle("Estimated vs Actual Hours") +
  xlab('Estimated') +
  ylab('Actual')

# It looks like there are a couple outliers in the dataset, where only a few hours
#   were estimated, but quite a few more were actually used. It may be prudent to 
#   ignore these points, but let's take a look at them to see what the stories are.
long_tasks <- software_tasks %>% filter(HoursActual > 1500)

# These datapoints show a large discrepancy between HoursActual and 
#   DeveloperHoursActual. Let's try a plot where we compare the two.
log_plot <- ggplot(software_tasks, aes(x = log(HoursEstimate), y = log(HoursActual))) +
  geom_point() +
  theme_classic() +
  ggtitle("Log-Estimate vs Log-Actual") +
  xlab('Log of Estimate') +
  ylab("Log of Actual")

ggarrange(normal_plot, log_plot, labels=c('A', 'B'), ncol=2)

# There seems to be an almost perfect relationship between the two for the most 
#   part, but there are a few points way outside the norm. These points seem to 
#   have the same two summaries, and all belong to the CHRONICLE status code. We 
#   don't want to throw away any points if we don't have to, so let's stick to 
#   using DeveloperHoursActual for our plot.
plot(software_tasks$HoursEstimate, software_tasks$DeveloperHoursActual)

# Based on the above plot, it looks like there are still a couple points that we
#   can safely assume are outliers. We can remove these from the dataset for now.
software_tasks <- filter(software_tasks, DeveloperHoursActual < 1500)
plot(log(software_tasks$HoursEstimate), log(software_tasks$DeveloperHoursActual))

# Let's take another look at the four most common task types.
for (task in common_tasks) {
  assign(task, filter(software_tasks, SubCategory == task))
}

e_plot <- ggplot(Enhancement, aes(x = log(HoursEstimate), y = log(HoursActual))) + 
  geom_point() + 
  theme_classic() +
  ggtitle('Enhancement Hours') + 
  geom_smooth() + 
  xlab("Log of Estimate") +
  ylab("Log of Actual")

ihs_plot <- ggplot(`In House Support`, aes(x = log(HoursEstimate), y = log(HoursActual))) + 
  geom_point() + 
  theme_classic() +
  ggtitle('In House Support Hours') + 
  geom_smooth() + 
  xlab("Log of Estimate") +
  ylab("Log of Actual")

b_plot <- ggplot(Bug, aes(x = log(HoursEstimate), y = log(HoursActual))) + 
  geom_point() + 
  theme_classic() +
  ggtitle('Bug Hours') + 
  geom_smooth() + 
  xlab("Log of Estimate") +
  ylab("Log of Actual")

s_plot <- ggplot(Support, aes(x = log(HoursEstimate), y = log(HoursActual))) + 
  geom_point() + 
  theme_classic() +
  ggtitle('Support Hours') + 
  geom_smooth() + 
  xlab("Log of Estimate") +
  ylab("Log of Actual")

ggarrange(e_plot, ihs_plot, b_plot, s_plot, labels=c('A', 'B', 'C', 'D'),
          ncol=2, nrow=2)

# We can see based on the graphs that each SubCategory has its own distribution
#   of points. Based on this, we can likely run separate analyses for each 
#   SubCategory for an initial model. We can then create a new model by 
#   introducing text-scraping components based on task summary and comparing it
#   to the first model. It will be interesting to see if the text scraping brings
#   higher accuracy.


# Part 2: Text Analysis
library(tidytext)

# Tokenizing the Summary Column to get a list of words for reach entry.
# 'word' column is the output of unnest_tokens(). 'Summary' is input.
tokened_tasks <- software_tasks %>% unnest_tokens(word, Summary)
tokened_tasks %>% count(word) %>% arrange(desc(n))

# There are multiple values we can remove from the words list.
# Appending custom stop words to built-in tidytext stop_words table:
custom_stop_words <- tribble(~word, ~lexicon, "ccc", "CUSTOM", "xxxx", "CUSTOM", 
                             "yyy", "CUSTOM", "zzz", "CUSTOM", "code", "CUSTOM")
stop_words2 <- custom_stop_words %>% bind_rows(stop_words)
stop_numbers <- tribble(~word, ~lexicon, 1:2017, 'CUSTOM') %>% unnest() %>% 
  select('word', 'lexicon')
stop_numbers$word <- as.character(stop_numbers$word)
stop_words3 <- stop_numbers %>% bind_rows(stop_words2)

# Removing stop words from tokened_tasks:
tokened_tasks <- tokened_tasks %>% anti_join(stop_words3)

# Creating word_counts table:
word_counts <- tokened_tasks %>%
  group_by(SubCategory)  %>%
  count(word) %>%
  top_n(10, n) %>%
  mutate(word2 = fct_reorder(word, n))

# Creating visualization for words_counts:
ggplot(word_counts, aes(x = word2, y = n, fill = SubCategory)) + 
  geom_col(show.legend = FALSE) + 
  coord_flip() + 
  facet_wrap(~ SubCategory, scales = 'free') + 
  ggtitle('Word Counts by Category')

# It looks like words can be a viable predictor for our purposes. Different types of
#   tasks have different words show up more often. For example, "add" shows up a lot
#   in Enhancement task types. Similarly, "scm" and "search" show up with high
#   frequency in Bug task types. Interestingly, "bug" does not seem to show up with
#   high frequency in any task type other than Bug tasks. I thought it might have
#   been a confounding variable, but it seems not. A couple words, such as "meeting"
#   show up with high frequency in many task types, so it may be worth it to remove
#   "meeting" from our list of words. However, this may adversely affect prediction
#   for task types with very low counts to begin with.

# Another type of analysis to perform would be topic modeling, in which we try to 
#   split up words based on topic, but since we have pre-defined SubCategory topics,
#   this likely wouldn't yield any actionable insights.

# Now we can prepare a data frame with dummy variables for SubCategory:
library(dummies)
top_tokens <- word_counts$word2
top_tokened_tasks <- tokened_tasks %>% filter(word %in% top_tokens)
top_tokened_tasks$SubCategory <- as.factor(top_tokened_tasks$SubCategory)
top_tokened_tasks$word <- as.factor(top_tokened_tasks$word)
dummied_tasks <- cbind(top_tokened_tasks, dummy(top_tokened_tasks$SubCategory, 
                                                sep = "_"))

new_names <- c('Board_Meeting', 'Bug', 'Bus_Spec', 'Client_Support', 'Consult', 
               'Conversion', 'Documentation', 'Enhancement', 'General_Docs', 
               'In_House_Support', 'Mgmt_Meeting', 'Marketing', 'Office_Mgmt', 
               'Prog_Meeting', 'Proj_Mgmt', 'Release', 'Research', 'Staff_Mgmt', 
               'Staff_Recruit', 'Support', 'Tech_Spec', 'Testing', 'Third_Party', 
               'Training')
names(dummied_tasks)[18:41] <- new_names

dummied_tokened_tasks <- cbind(dummied_tasks, dummy(dummied_tasks$word, sep='_'))

coalesce_by_column <- function(df) {
  return(dplyr::coalesce(!!! as.list(df)))
}
aggregate_dummied_tasks <- dummied_tokened_tasks %>% group_by(TaskNumber) %>%
  summarize_all(coalesce_by_column)

# Part 3: Modeling
# Remove columns that are not useful in our prediction:
remove_cols <- c('RaisedByID', 'AssignedToID', 'AuthorisedByID', 'StatusCode', 
                 'ProjectCode', 'ProjectBreakdownCode', 'Category', 'DeveloperID', 
                 'SubCategory', 'word', 'TaskPerformance', 'DeveloperPerformance', 
                 'TaskNumber', 'DeveloperHoursActual')

dummied_tasks2 <- aggregate_dummied_tasks %>% select(-remove_cols)

dummied_tasks3 <- dummied_tasks2 %>%
  mutate(log_estimate = log(HoursEstimate), log_actual = log(HoursActual)) %>%
  select(-c('HoursEstimate', 'HoursActual'))

# Split into testing and training data sets for the model:
library(rsample)
set.seed(24)
split <- initial_split(dummied_tasks3)
training_data <- training(split)
testing_data <- testing(split)
actuals <- exp(1)^testing_data$log_actual
estimates <- exp(1)^testing_data$log_estimate

# Base model comparing HoursEstimate to DeveloperHoursActual
base_error = estimates - actuals
base_root_mse = sqrt(mean(base_error^2))
base_srmse <- base_root_mse/sd(actuals)
base_nrmse <- base_root_mse/(max(actuals) -
                               min(actuals))

# Linear predictive model using only HoursEstimate as input variable:
model1 <- train(log_actual ~ log_estimate, training_data, method = 'lm',
                trControl=trainControl(method='cv', number=10, verboseIter=TRUE),
                preProcess=c('knnImpute', 'center', 'scale'))
p1 <- predict(model1, testing_data)
error1 <- (exp(1)^p1) - actuals
root_mse1 <- sqrt(mean(error1^2))
srmse1 <- root_mse1/sd(actuals)
nrmse1 <- root_mse1/(max(actuals) -
                       min(actuals))

# Linear predictive model using all input variables:
model2 <- train(log_actual ~ ., training_data, method = 'lm',
                trControl=trainControl(method='cv', number=10, verboseIter=TRUE),
                preProcess=c('knnImpute', 'center', 'scale'))
p2 <- predict(model2, testing_data)
error2 <- (exp(1)^p2) - actuals
root_mse2 <- sqrt(mean(error2^2))
srmse2 <- root_mse2/sd(actuals)
nrmse2 <- root_mse2/(max(actuals) -
                       min(actuals))

# Random forest model using only HoursEstimate as a predictor:
model3 <- train(log_actual ~ log_estimate, training_data, method='rf', 
                trControl=trainControl(method='cv', number=10, verboseIter=TRUE), 
                preProcess=c('knnImpute', 'center', 'scale'))
p3 <- predict(model3, testing_data)
error3 <- (exp(1)^p3) - actuals
root_mse3 <- sqrt(mean(error3^2))
srmse3 <- root_mse3/sd(actuals)
nrmse3 <- root_mse3/(max(actuals) -
                       min(actuals))

# Random forest model using all input variables:
model4 <- train(log_actual ~ ., training_data, method='rf', 
                trControl=trainControl(method='cv', number=10, verboseIter=TRUE), 
                preProcess=c('knnImpute', 'center', 'scale'))
p4 <- predict(model4, testing_data)
error4 <- (exp(1)^p4) - actuals
root_mse4 <- sqrt(mean(error4^2))
srmse4 <- root_mse4/sd(actuals)
nrmse4 <- root_mse4/(max(actuals) -
                       min(actuals))

# Here we have some interesting observations. It looks like model2 performed the
#   best out of the four models we trained. The RMSE is still in natural log form; 
#   if we convert that to base form (exp(1)^root_mse2) then we see that the root
#   mean squared error becomes about 2.17. The normalized RMSE (normalized by range)
#   is about 7.40%. These figures, although only marginally better than the next 
#   best model, show us that a simple linear model using all predictor variables 
#   (priority, SubCategory dummies, token dummies) is the best model.
