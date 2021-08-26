

## Data preprocessing and exploring

We start by reading our dataset file and storing it in a variable Cd
#{r echo=TRUE}
setwd("C:/Users/Isaac/Desktop/DS last class/Data Science Assign 3")
Cd <- read.csv("assign3.csv")

#

## Structure of dataset

#{r echo=TRUE}
str(Cd)

#

### Summary of dataset
#{r echo=TRUE}
summary(Cd)
#
##### Comment and Observation
It is observed that the file contains 60,004 observations of 20 columns, with empty columns and rows in the dataset. Our next step is to eliminate all the empty columns first, then drop all the empty rows, and finally convert the dataset into a binary incidence matrix form and then convert it nto transaction value so the apriori algorithm can take it as input.

#{r echo=TRUE}
#Eliminate all the empty columns
Cd <- Cd[,1:10]
summary(Cd)
View(Cd)
str(Cd)
#Eliminate all the empty rows
Cd<-na.omit(Cd)
str(Cd)
View(Cd)
summary(Cd)
# The empty rows and columns have now been removed, We then drop the first column seeing that we don't need Customer.Number in associating the categories
Cd.mat <- as.matrix(Cd[,-1])
View(Cd.mat)
#Convert the Matrix to Transaction Values
Cd.trans <- as(Cd.mat, "transactions")
#

#{r echo=TRUE}

library(dplyr)

summary(Cd.trans)
#
##### Observations
* After preprocessing, the dataset includes 4998 records and 9 fields as seen the output and Environment tab. The 9 fields are as follows:  Clothing.Division, Housewares.Division, Health.Products.Division, Automotive.Division, Personal.Electronics.Division, Computers.Division, Garden.Division, Novelty.Gift.Division, Jewelry.Division.
* From the total 4998 records, Health.Products.Division appears in all 4998 entries. This means that in every transaction there was a purchase from the Health.Products.Division. 

### Frequency Plot

#{r echo=TRUE}
# Display an item frequency plot ordered in descending form, with Absolute values
itemFrequencyPlot(Cd.trans,topN=9,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")
# Display an item frequency plot ordered in descending form, with Relative values
itemFrequencyPlot(Cd.trans,topN=9,type="relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot")
#

##### Observation
* From the plot, it was observed that Health.Products.Division has the highest frequency of appearance, followed by the Personal.Electronics.Division

## Association rules for Exeter, Inc.
We assumed a support and confidence and performed Association rule mining (apriori) for the dataset:
  Support: 0.01
Confidence: 0.8

An Initial support of 0.01 implies that an item should appear at least 49.98 times (1% of the entire records in the dataset) to be considered a frequent item
That is 1% of the items in the dataset, 4998.

### Apriori Simulation

#### 1-Itemsets Range
We perform ARM to get the 1-item set that meet or exceed the support and confidence
#{r  echo=TRUE}

#Understanding the min and max support for 1-itemsets range
itemsets <- apriori(Cd.trans, parameter=list(minlen=1, maxlen=1, support=0.01, target="frequent itemsets"))
summary(itemsets)

#Display the top 100 frequent 1-itemsets, sorted by their support
inspect(head(sort(itemsets, by = "support"), 100))

#
##### Observation
* This iteration is meant to display itemsets with just 1 item in the LHS. Since there are only 9 individual catalog groups, there can only be 9 individual itemsets, with the most count of 1-Itemset items, 4998 for "{Health.Products.Division}" catalogs, and the least count of 1-Itemset items, 165 for "{Clothing.Division}" catalogs.

#### 2-Itemsets Range
We perform ARM to get the 2-item set that meet or exceed the support and confidence
#{r  echo=TRUE}
#Understanding the min and max support for 2-itemsets range
itemsets2 <- apriori(Cd.trans, parameter=list(minlen=2, maxlen=2, support=0.01, target="rules"))

summary(itemsets2)

#Display the top 100 frequent 2-itemsets, sorted by their support

inspect(head(sort(itemsets2, by = "support"),100))

#
##### Observation
* This iteration is meant to display itemsets with just 2 items in the LHS. There are 33 individual itemsets, with the most count of 2-Itemset items, 2336 for "{Health.Products.Division,Personal.Electronics.Division}" catalogs, and the least count of 2-Itemset items, 56 for "{Clothing.Division,Garden.Division}" catalogs.

#### 3-Itemsets Range
We perform ARM to get the 3-item set that meet or exceed the support and confidence
#{r  echo=TRUE}
#Understanding the min and max support for 3-itemsets range
itemsets <- apriori(Cd.trans, parameter=list(minlen=3, maxlen=3, support=0.01, target="rules"))

summary(itemsets)

#Display the top 100 frequent 3-itemsets, sorted by their support
inspect(head(sort(itemsets, by = "support"), 100))
#
##### Observation
* This iteration is meant to display itemsets with just 3 items in the LHS. There are 58 individual itemsets, with the most count of 3-Itemset items, 1177 for "	{Housewares.Division,Health.Products.Division,Personal.Electronics.Division}" catalogs, and the least count of 3-Itemset items, 51 for "	{Clothing.Division,Housewares.Division,Novelty.Gift.Division}" catalogs.

#### 4-Itemsets Range
We perform ARM to get the 4-item set that meet or exceed the support and confidence
#{r  echo=TRUE}

#Understanding the min and max support for 4-itemsets range
itemsets <- apriori(Cd.trans, parameter=list(minlen=4, maxlen=4, support=0.01, target="frequent itemsets"))
summary(itemsets)

#Display the top 100 frequent 4-itemsets, sorted by their support
inspect(head(sort(itemsets, by = "support"), 100))


#
##### Observation
* This iteration is meant to display itemsets with just 4 items in the LHS. There are 51 individual itemsets, with the most count of 4-Itemset items, 660 for "{Housewares.Division,Health.Products.Division,Personal.Electronics.Division,Jewelry.Division}" catalogs, and the least count of 4-Itemset items, 51 for "{Housewares.Division,Personal.Electronics.Division,Computers.Division,Novelty.Gift.Division}" catalogs.

#### 5-Itemsets Range
We perform ARM to get the 5-item set that meet or exceed the support and confidence
#{r  echo=TRUE}

#Understanding the min and max support for 5-itemsets range
itemsets<- apriori(Cd.trans, parameter=list(minlen=5, maxlen=5, support=0.01, target="frequent itemsets"))
summary(itemsets)

#Display the top 100 frequent 5-itemsets, sorted by their support
inspect(head(sort(itemsets, by = "support"), 100))


#
##### Observation
* This iteration is meant to display itemsets with just 5 items in the LHS. There are 24 individual itemsets, with the most count of 5-Itemset items, 318 for "{Housewares.Division, Health.Products.Division, Personal.Electronics.Division, Novelty.Gift.Division, Jewelry.Division} " catalogs, and the least count of 5-Itemset items, 51 for "{Housewares.Division, Health.Products.Division, Personal.Electronics.Division, Computers.Division, Novelty.Gift.Division}" catalogs.

#### 6-Itemsets Range
We perform ARM to get the 6-item set that meet or exceed the support and confidence
#{r  echo=TRUE}

#Understanding the min and max support for 6-itemsets range
itemsets6 <- apriori(Cd.trans, parameter=list(minlen=6, maxlen=6, support=0.01, target="frequent itemsets"))
summary(itemsets6)

#Display the top 100 frequent 6-itemsets, sorted by their support
inspect(head(sort(itemsets6, by = "support"), 100))


#
##### Observation
* This iteration is meant to display itemsets with just 5 items in the LHS. There are 7 individual itemsets, with the most count of 6-Itemset items, 191 for "{Housewares.Division, Health.Products.Division, Personal.Electronics.Division, Garden.Division, Novelty.Gift.Division, Jewelry.Division}" catalogs, and the least count of 6-Itemset items, 86 for "{Housewares.Division, Automotive.Division, Personal.Electronics.Division, Garden.Division, Novelty.Gift.Division, Jewelry.Division}" catalogs.

#### 7-Itemsets Range
We perform ARM to get the 7-item set that meet or exceed the support and confidence
#{r  echo=TRUE}

#Understanding the min and max support for 7-itemsets range
itemsets <- apriori(Cd.trans, parameter=list(minlen=7, maxlen=7, support=0.01, target="frequent itemsets"))
summary(itemsets)

#Display the top 100 frequent 7-itemsets, sorted by their support
inspect(head(sort(itemsets, by = "support"), 100))
#
##### Observation
* This iteration is meant to display itemsets with just 7 items in the LHS. It was observed that there is just 1 entry that contains up to 7 items in the Itemset "{Housewares.Division, Health.Products.Division, Automotive.Division, Personal.Electronics.Division, Garden.Division, Novelty.Gift.Division, Jewelry.Division}", with a count of 86.
We can stop at this point the iteration

### Automatic Apriori Simulation
We generate all itemsets that meet or exceed the support (0.01) using the default Apriori algorithm.

#{r  echo=TRUE}
itemsets <- apriori(Cd.trans, parameter=list(minlen=1, support=0.01, target="frequent itemsets"))
inspect(head(sort(itemsets, by = "support"), 100))
summary(itemsets)
#
##### Observation
* With a minimum support value of 0.01, it was observed that there are 183 Itemsets with the 3-Itemset group having the maximum number of entries, 58, and the 7-Itemset group having the least number of entries, 1.

###### 1a) i. Interpret the first dozen rules in the output in words

We display the top 12 frequent items sorted by their support
#{r  echo=TRUE}
#Display the top 12 frequent items sorted by their support
inspect(head(sort(itemsets, by = "support"), 12))
#
##### Observations
* The "{Health.Products.Division}" entry has maximum support and count, as had been identified earlier.
* The "{Novelty.Gift.Division}" entry has the 12th highest support and count.

## Rule Creation

* We use the Apriori algorithm in arules library to mine frequent itemsets and association rules. 

* We pass supp=0.01 and conf=0.8 to return all the rules that have a support of at least 0.1% and confidence of at least 80%. 

* We sort the rules by decreasing confidence. 


#{r  echo=TRUE}

rules <- apriori(Cd.trans, parameter=list(support=0.01, confidence=0.8, target = "rules"))

rules <- sort(rules, by='lift', decreasing = TRUE)
summary(rules)
inspect(rules)
#

#### Observations
* Number of rules: 140
* Maximum lift: 3
* Minimum lift: 1

##### 1a) ii. Reviewing the first couple of dozen rules, comment on their redundancy 

#{r  echo=TRUE}


rules <- rules[!is.redundant(rules)]
summary(rules)

inspect(rules)
rules1<-rules[1:12,]
summary(rules1)
inspect(rules1)

#

#### Observations
* Initial number of rules: 144
* Number of rules after removal of redundant rules: 24
* The distribution of rules by length: Most rules were observed to be 4 items long (12 rules).


### Observation after inspecting Top 12 Rules

* 80% of customers who bought catalogs from "Automotive.Division, Personal.Electronics.Division, Novelty.Gift.Division, Jewelry.Division" also purchase from "Garden.Division" as well. 
* 80% of customers who bought catalogs from "Automotive.Division, Novelty.Gift.Division, Jewelry.Division" also purchase from "Garden.Division" as well. * {Housewares.Division, Automotive.Division, Novelty.Gift.Division, Jewelry.Division}  => {Personal.Electronics.Division} has the highest confidence value.

## We plot the top 12 rules.

#{r  echo=TRUE}
topRules <- rules[1:12]
plot(topRules)
#


#{r  echo=TRUE}
plot(topRules, method="graph")
#

###Non-Directed Map for the top 2 rules

#{r  echo=TRUE}
topRules <- rules[1:2]
plot(topRules, method="graph")
#

##### 2b. i. 2-Itemset Rule Generation and construct a non directed network graph  
This step is to generate the 2-Items Association Ruleset that is generated from the Apriori algorithm.

#{r echo=TRUE}
rules <- apriori(Cd.trans, parameter=list(minlen = 2, maxlen=2, support=0.01, confidence=0.8, target = "rules"))

#Display the top 100 frequent items sorted by their lift
inspect(head(sort(rules, by = "confidence"), 100))

#

##### Observations
* It was observed that there are only 8 rules that meet the 2-Items rules criteria.



#{r echo=TRUE}
# We export the rules as a dataframe that will serve as input for the edges
rulesList<- DATAFRAME(rules, setStart='', setEnd='', separate = TRUE)

# convert edges to edge list matrix
edges <- as.matrix(rulesList[, c(1,2)])
g <- graph.edgelist(edges,directed=FALSE)

# plot graph
# nodes' size is proportional to their eigenvector centrality
plot(g, vertex.size = eigen_centrality(g)$vector * 50)
#

##### 2b. i. Visualize the network and report degree distribution and density metric.
### Display Degree Distribution and Degree Centrality of rules
Degree

#{r echo=TRUE}

degree(g)
#

Betweenness
#{r echo=TRUE}
betweenness(g)
#
Closeness

#{r echo=TRUE}
closeness(g)
#
Eigen_Centrality

#{r echo=TRUE}
eigen_centrality(g)
#
## We the generate 2-3 Itemset rule to better observe purchases and association and construct a non directed network graph 
This step is to generate the 2-3Items Association Ruleset that is generated from the Apriori algorithm.

#{r echo=TRUE}
rules <- apriori(Cd.trans, parameter=list(minlen = 2, maxlen=3, support=0.01, confidence=0.8, target = "rules"))

#Display the top 100 frequent items sorted by their lift
inspect(head(sort(rules, by = "confidence"), 100))

#

##### Observations
* It was observed that there are 38 rules that meet the 2-3 Items rules criteria.



#{r echo=TRUE}
# Export the rules as a dataframe that will serve as input for the edges
rulesList<- DATAFRAME(rules, setStart='', setEnd='', separate = TRUE)

# convert edges to edge list matrix
edges <- as.matrix(rulesList[, c(1,2)])
g <- graph.edgelist(edges,directed=FALSE)

# plot graph
# nodes' size is proportional to their eigenvector centrality
plot(g, vertex.size = eigen_centrality(g)$vector * 10)
#

### Display Degree Distribution and Degree Centrality of rules
Degree
#{r echo=TRUE}

degree(g)
#

Betweenness

#{r echo=TRUE}
betweenness(g)
#

Closeness

#{r echo=TRUE}
closeness(g)
#

Eigen_Centrality

#{r echo=TRUE}
eigen_centrality(g)
#