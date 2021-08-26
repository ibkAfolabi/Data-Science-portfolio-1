
#SNA

library(igraph)
library(knitr)
library(ggplot2)
library(sqldf)
library(kableExtra)
library(rtweet)


# Importing The Twitter Data
connection <- read.csv("1007-1020TwitterEdgelist.csv", header = TRUE)
users <- read.csv("1007-1020TwitterNodelist.csv", header = TRUE)

# convert edges to matrix
edges <- as.matrix(connection[, c(1,2)])
nodes <- as.matrix(users[, c(1,2)])

# Converting the Twitter to Graph
g <- graph.data.frame(edges , nodes, directed = TRUE )

# Assigning edge weight attribute to the 
E(g)$weight <- connection[, c(7)]





# To calculate the "in degree" of the network

degree_in <- degree(g, mode = "in")
degree_distribution_in <- degree_distribution(g, mode= "in" )


# To calculate the "out degree" of the network

degree_out <- degree(g, mode = "out")
degree_distribution_out <- degree_distribution(g, mode= "out" )


# To calculate all degrees of the network
degree_all <- degree(g)
degree_distribution_all <- degree_distribution(g)

# Defining variables to Plot a Scatter_plot

dti <- table(degree_in)
dto <- table(degree_out)
dta <- table(degree_all)





## Recent Twitter Nework Graph



#{r Recent Network Graph Declarations, echo=TRUE, message=FALSE, warning=FALSE}
# plot graph
# nodes' size is proportional to their Eigen Vector centrality
# Defining some attributes for the vertex of the graph

V(g)$color <- "magenta" 
V(g)$shape <- "sphere"
E(g)$color <- "green"

# Making the edge color of edge weight above average 
edt <- ave(E(g)$weight)
E(g)[ weight > edt ]$color <- "red"




# Recent Network Graph ploted with the Eigen Vector of the nodes

#{r Recent IN_degree Graph, echo=TRUE, message=FALSE, warning=FALSE}
# Plotting the graph
plot.igraph(g,vertex.size= eigen_centrality(g)$vector*10, layout = layout.fruchterman.reingold(g),vertex.label= nodes, vertex.label.cex=0.5,   edge.arrow.size = 0.0005)

# Saving the graph to as a piccture
png("my-plot-recent-graph.png", width=1200, height=1200)
par(mar=c(0,0,0,0))
plot(g, vertex.size= eigen_centrality(g)$vector*10, layout = layout.fruchterman.reingold(g),vertex.label= nodes, vertex.label.cex=0.5,   edge.arrow.size = 0.0005)
dev.off()





#
## Recent Network degree distribution scatter plot

# Recent In_degree distribution scatter plot

#{r Recent In_ Degree Distribution Scatter Plot, echo=TRUE, message=FALSE, warning=FALSE}


# Converting table dti to dataframe
histogram <- as.data.frame(dti)

# Need to convert the first column to numbers
histogram[,1] <- as.numeric(paste(histogram[,1]))

# Plot the scatter plot
ggplot(histogram, aes(x = degree_in, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(users with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("In_Degree Distribution ") +
  theme_bw()






# Recent Out_degree distribution scatter plot

#{r Recent Out_Degree Distribution Scatter Plot, echo=TRUE, message=FALSE, warning=FALSE}


# Converting tabel dto to dataframe
histogramo <- as.data.frame(dto)

# Need to convert the first column to numbers 
histogramo[,1] <- as.numeric(paste(histogramo[,1]))

# Plot the scatter plot
ggplot(histogramo, aes(x = degree_out, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(users with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Out_Degree Distribution ") +
  theme_bw()




# Recent Total degree distribution scatter plot
#{r Recent Total degree distribution scatter plot, echo=TRUE, message=FALSE, warning=FALSE}

histograma <- as.data.frame(dta)

# Need to convert the first column to numbers
histograma[,1] <- as.numeric(paste(histograma[,1]))

# Plot the scatter plot
ggplot(histograma, aes(x = degree_all, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(users with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Total Degree Distribution ") +
  theme_bw()


#
## Recent Network Degree distribution table

# Recent In_degree distribution table
#{r Recent In_degree distribution table, echo=TRUE, message=FALSE, warning=FALSE}

# To out put In_degree distribution in table

ddti <- as.matrix(dti[c(1:53)]/sum(dti))
uddti <- ddti[,1]
rfi <- data.frame(dti,uddti)

# Removing the column degree_in from rfi
df=subset(rfi, select = -c(degree_in))

# To print In_degree distribution table
colnames(df) = c("Frequency", "In_Degree Distribution")
kbl(df) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))





# Recent Out_degree distribution table 

#{r Recent Out_Degree Distribution table, echo=TRUE, message=FALSE, warning=FALSE}

# To out put out_degree distribution in table

ddto <- as.matrix(dto[c(1:52)]/sum(dto))
uddto <- ddto[,1]
rfo <- data.frame(dto,uddto)

# Remove the column degree_out from table rfo 
dfo=subset(rfo, select = -c(degree_out))

# To print Out_degree distribution table
colnames(dfo) = c("Frquency", "Out_Degree Distribution")
kbl(dfo) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))






# Recent Total degree distribution table 

#{r Recent Total Degree Distribution Tale, echo=TRUE, message=FALSE, warning=FALSE}

# To out put all_degree distribution in table

ddta <- as.matrix(dta[c(1:45)]/sum(dta))
uddta <- ddta[,1]
rfa <- data.frame(dta,uddta)

# Removing the column degree_all from rfa
dfa=subset(rfa, select = -c(degree_all))

# To print all degree distribution table
colnames(dfa) = c("Frquency", "Total_Degree Distribution")
kbl(dfa) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


#
## The Density Of The Recent Twitter Network Graph 
#{r Recent Network Density, echo=TRUE, message=FALSE, warning=FALSE}

# To calculate the density of the network
edge_density(g)



#
#
#

###
#
#
#
## Previous Twitter Nework Graph
#{r Previous Network Beginning Declarations, echo=TRUE, message=FALSE, warning=FALSE}

# Importing the Previous twitter data

connection2 <- read.csv("1104-1007TwitterEdgelist.csv", header = TRUE)
users2 <- read.csv("1104-1007TwitterNodelist.csv", header = TRUE)

# convert edges to edge list matrix
edges2 <- as.matrix(connection2[, c(1,2)])
nodes2 <- as.matrix(users2[, c(1,2)])

# Converting data to graph
g2 <- graph.data.frame(edges2 , nodes2,   directed=TRUE)

# Assigning edge weight attribute to the
E(g2)$weight <- connection2[, c(7)]




# To calculate the "in degree" of the network

degree_in2 <- degree(g2, mode = "in")
degree_distribution_in2 <- degree_distribution(g2, mode= "in" )


# To calculate the "out degree" of the network

degree_out2 <- degree(g2, mode = "out")
degree_distribution_out2 <- degree_distribution(g2, mode= "out" )


# To calculate all degrees of the network
degree_all2 <- degree(g2)
degree_distribution_all2 <- degree_distribution(g2)

# Declaration of variables for scatter plot

dti2 <- table(degree_in2)
dto2 <- table(degree_out2)
dta2 <- table(degree_all2)


# Previous Network Graph

#{r Previous Network Graph Declarations, echo=TRUE, message=FALSE, warning=FALSE}
# plot graph
# nodes' size is proportional to their degree  centrality

# Declaring the attributes of the graph vertex
V(g2)$color <- "magenta" 
V(g2)$shape <- "sphere"
E(g2)$color <- "green"

# Making the edge color of edge weight above average 
edt <- ave(E(g2)$weight)
E(g2)[ weight > edt ]$color <- "red"


# Previous Network Graph Plotted with the Eigen Vector Of The Nodes

#{r Previous In_Degree Graph, echo=TRUE, message=FALSE, warning=FALSE}

# Plot graph

plot.igraph(g2, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(g2),vertex.label= nodes2, vertex.label.cex=0.7,   edge.arrow.size = 0.0005)

# Saving graph as picture
png("my-plot-previous-graph.png", width=1200, height=1200)
par(mar=c(0,0,0,0))
plot(g2, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(g2),vertex.label= nodes2, vertex.label.cex=0.7,   edge.arrow.size = 0.0005)
dev.off()



#
## Previous Network degree distribution scatter plot

# Previous In_degree distribution scatter plot

#{r Previous In_Degree Distribution Scatter Plot, echo=TRUE, message=FALSE, warning=FALSE}

# Converting table dti2 to dataframe
histogram2 <- as.data.frame(dti2)

# Need to convert the first column to numbers
histogram2[,1] <- as.numeric(paste(histogram2[,1]))

# Plot the scatter plot
ggplot(histogram2, aes(x = degree_in2, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(users with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("In_Degree Distribution ") +
  theme_bw()



# Previous Out_degree distribution scatter plot
#{r Previous Out_Degree Distribution Scatter Plot, echo=TRUE, message=FALSE, warning=FALSE}

# Converting the table dto2 to dataframe
histogramo2 <- as.data.frame(dto2)

# Need to convert the first column to numbers
histogramo2[,1] <- as.numeric(paste(histogramo2[,1]))

# Plot the scatter plot
ggplot(histogramo2, aes(x = degree_out2, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(users with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Out_Degree Distribution ") +
  theme_bw()



# Previous Total Degree Distribution Scatter Plot
#{r Previous Total Degree Distribution Scatter Plot, echo=TRUE, message=FALSE, warning=FALSE}

# Converting table dta2 to dtatframe
histograma2 <- as.data.frame(dta2)

# Need to convert the first column to numbers
histograma2[,1] <- as.numeric(paste(histograma2[,1]))

# Plot the scatter plot
ggplot(histograma2, aes(x = degree_all2, y = Freq)) +
  geom_point() +
  scale_x_continuous("Degree\n(users with this amount of connections)",
                     breaks = c(1, 3, 10, 30, 100, 300),
                     trans = "log10") +
  scale_y_continuous("Frequency\n(how many of them)",
                     breaks = c(1, 3, 10, 30, 100, 300, 1000),
                     trans = "log10") +
  ggtitle("Total Degree Distribution ") +
  theme_bw()


#
## Previous Network Degree distribution table

# Previous In_degree distribution table

#{r Previous In-degree distribution table, echo=TRUE, message=FALSE, warning=FALSE}

# To out put In_degree distribution in table

ddti2 <- as.matrix(dti2[c(1:24)]/sum(dti2))
uddti2 <- ddti2[,1]
rfi2 <- data.frame(dti2,uddti2)

# Removing the column degree_in2 from rfi2
df2=subset(rfi2, select = -c(degree_in2))

# To print In_degree distribution table
colnames(df2) = c("Frquency", "In_Degree Distribution")
kbl(df2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))




# Previous Out_degree distribution table
#{r Previous Out_degree distribution table, echo=TRUE, message=FALSE, warning=FALSE}

# To out put out_degree distribution in table

ddto2 <- as.matrix(dto2[c(1:20)]/sum(dto2))
uddto2 <- ddto2[,1]
rfo2 <- data.frame(dto2,uddto2)

# Remove the column degree_out2 from rfo2
dfo2=subset(rfo2, select = -c(degree_out2))

# To print Out_degree distribution table
colnames(dfo2) = c("Frquency", "Out_Degree Distribution")
kbl(dfo2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


#

# Previous Total degree distribution table
#{r Previous Total Distribution Table, echo=TRUE, message=FALSE, warning=FALSE}

# To out put all_degree distribution in table

ddta2 <- as.matrix(dta2[c(1:27)]/sum(dta2))
uddta2 <- ddta2[,1]
rfa2 <- data.frame(dta2,uddta2)

# Remove the column degree_all2 from rfa2
dfa2=subset(rfa2, select = -c(degree_all2))

# To print all degree distribution table
colnames(dfa2) = c("Frquency", "Total_Degree Distribution")
kbl(dfa2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


#
## The Density Of The Previous Twitter Network Graph 

#{r Previous Network Density, echo=TRUE, message=FALSE, warning=FALSE}

# To calculate the density of the network
edge_density(g2)


#
#
#
#
#
#
#
### To get The Degree, closeness, Betweeness and Eigenvector
#
#
## A Table Showing The Degree, Closeness, Betweenness and Eigenvector of Recent Twiter Network

#{r degree closeness betweenness and eigenvector declarations, echo=TRUE, message=FALSE, warning=FALSE}

# Declaring the variables for betweenness, egenvector, closeness for both data set
bet <- betweenness(g)
bet2 <- betweenness(g2)

eg <- eigen_centrality(g)$vector
eg2 <- eigen_centrality(g2)$vector

# There are different components in the graph
# The major Component was found and the closeness was calculator

maincomp = which.max(components(g, mode = "strong")$csize)
mainclose = induced.subgraph(g, which(components(g, mode = "strong")$membership == maincomp))
close <- closeness(mainclose)
maincomp2 = which.max(components(g2, mode = "strong")$csize)
mainclose2 = induced.subgraph(g2, which(components(g2, mode = "strong")$membership == maincomp2))
close2 <- closeness(mainclose2)


# Getting the attribute value of both graph vertices
name <- get.vertex.attribute(g, "Label")
name2 <- get.vertex.attribute(g2, "Label")

vertice <- get.vertex.attribute(g, "name")
vertice2 <- get.vertex.attribute(g2, "name")




#





#{r Recent network degree closeness betweenness and eigenvector table, echo=TRUE, message=FALSE, warning=FALSE}

# Printing the closeness value in a table
table <- data.frame(V(mainclose)$Label, close)
colnames(table) = c("User_name", "Closeness")
kbl(table) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# Printing the degree, betweenness and eigenvector values of the graph in a table

table <- cbind( vertice ,name, degree_in, degree_out, degree_all, bet, eg)
tablee <- data.frame(table)

colnames(tablee) = c("Vertex_Id", "User_name", "In_Degree", "Out_Degree", "Total_Degree", "Betweenness", "Eigen_Vector")
kbl(tablee) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))




## To take out the influential users based on degree 
# In_Degree
#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
kls<-ifelse(degree_in >= ave(degree_in), degree_in, NA)
klsi<-ifelse(degree_in >= ave(degree_in), V(g)$Label, NA)
klsii<-ifelse(degree_in >= ave(degree_in), V(g)$name, NA)

# Removing NA from the table
kls <- na.omit(kls)
klsi <- na.omit(klsi)
klsii <- na.omit(klsii)

# Priniting the table 

tab <- cbind(klsi, klsii ,kls)

tab1 <- data.frame(tab[,c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "In_Degree")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# Out_Degree
#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
kls<-ifelse(degree_out >= ave(degree_out), degree_out, NA)
klsi<-ifelse(degree_out >= ave(degree_out), V(g)$Label, NA)
klsii<-ifelse(degree_out >= ave(degree_out), V(g)$name, NA)

# To remove the NA from the table
kls <- na.omit(kls)
klsi <- na.omit(klsi)
klsii <- na.omit(klsii)

# To print the table
tab <- cbind(klsi, klsii ,kls)

tab2 <- data.frame(tab[,c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "Out_Degree")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# Total Degree
#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
kls<-ifelse(degree_all >= ave(degree_all), degree_all, NA)
klsi<-ifelse(degree_all >= ave(degree_all), V(g)$Label, NA)
klsii<-ifelse(degree_all >= ave(degree_all), V(g)$name, NA)

# removing the NA from the table
kls <- na.omit(kls)
klsi <- na.omit(klsi)
klsii <- na.omit(klsii)
# Printing the table
tab <- cbind(klsi, klsii ,kls)

tab3 <- data.frame(tab[,c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "Total_Degree")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


#
#
#
#

#Betweenness
#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
luk<-ifelse(bet>=ave(bet), bet, NA)
tluk<-ifelse(bet>=ave(bet), V(g)$Label, NA)
tluki<-ifelse(bet>=ave(bet), V(g)$name, NA)

# Removing the NA form the table
luk <- na.omit(luk)
tluk <- na.omit(tluk)
tluki <- na.omit(tluki)

# Print the table
tab <- cbind(tluk, tluki ,luk)

tab4 <- data.frame(tab[,c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "Betweenness")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


#Eigenvector
#{r echo=TRUE, message=FALSE, warning=FALSE}


# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
juk<-ifelse(eg>=ave(eg), eg, NA)
julk<-ifelse(eg>=ave(eg), V(g)$Label, NA)
julki<-ifelse(eg>=ave(eg), V(g)$name, NA)


# Removing the NA from the table
juk <- na.omit(juk)
julk <- na.omit(julk)
julki <- na.omit(julki)


# Print the table
tab <- cbind(julk, julki ,juk)

tab5 <- data.frame(tab[,c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "Eigen_Vector")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



#Closeness

#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
fg <- ifelse(close>=ave(close), close, NA)
kk <- ifelse(close>=ave(close), V(mainclose)$Label, NA)
kki <- ifelse(close>=ave(close), V(mainclose)$name, NA)

# Remove the NA from table
fg<- na.omit(fg)
kk<- na.omit(kk)
kki <- na.omit(kki)

# Print the table
tab <- cbind(kk, kki ,fg)
tab6 <- data.frame(tab[,c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "Closeness")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))




### List Of Influential Users in The Recent Network

#{r echo=TRUE, message=FALSE, warning=FALSE}

# Using teh sqldf package to filter the tables
a1 <- sqldf('SELECT * FROM tab1 INTERSECT SELECT * FROM tab2')
a2 <- sqldf('SELECT * FROM tab3 INTERSECT SELECT * FROM tab4')
a3 <- sqldf('SELECT * FROM tab5 INTERSECT SELECT * FROM tab6')

a2 <- sqldf('SELECT * FROM a1 INTERSECT SELECT * FROM a2')
a3 <- sqldf('SELECT * FROM a2 INTERSECT SELECT * FROM a3')

# Printing the table

colnames(a3) = c("Name", "Vertex_Id")
kbl(a3) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))





## A Table Showing The Degree, Closeness, Betweenness and Eigenvector of Previous Twiter Network

#{r Previous network degree closeness betweenness and eigenvector table, echo=TRUE, message=FALSE, warning=FALSE}

# Printing the CLoseness and other centralities to a table
table2 <- data.frame(V(mainclose2)$Label, close2)

colnames(table2) = c("User_name", "Closeness")
kbl(table2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

table2 <- cbind( vertice2 ,name2, degree_in2, degree_out2, degree_all2, bet2, eg2)
tablee2 <- data.frame(table2)

colnames(tablee2) = c("Vertice_Id" ,"User_name", "In_Degree", "Out_Degree", "Total_Degree" ,"Betweenness", "Eigenvector")
kbl(tablee2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))







## To take out the influential users based on degree 
# In_Degree
#{r echo=TRUE, message=FALSE, warning=FALSE}
# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
kls<-ifelse(degree_in2 >= ave(degree_in2), degree_in2, NA)
klsi<-ifelse(degree_in2 >= ave(degree_in2), V(g2)$Label, NA)
klsii<-ifelse(degree_in2 >= ave(degree_in2), V(g2)$name, NA)
# Remove NA
kls <- na.omit(kls)
klsi <- na.omit(klsi)
klsii <- na.omit(klsii)

# Print table
tab <- cbind(klsi, klsii, kls)

tab1 <- data.frame(tab[,c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "In_Degree")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# Out_Degree
#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
kls<-ifelse(degree_out2 >= ave(degree_out2), degree_out2, NA)
klsi<-ifelse(degree_out2 >= ave(degree_out2), V(g2)$Label, NA)
klsii<-ifelse(degree_out2 >= ave(degree_out2), V(g2)$name, NA)

# Remove NA
kls <- na.omit(kls)
klsi <- na.omit(klsi)
klsii <- na.omit(klsii)

# Print the table
tab <- cbind(klsi,klsii, kls)

tab2 <- data.frame(tab[, c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "Out_Degree")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# Total Degree
#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
kls<-ifelse(degree_all2 >= ave(degree_all2), degree_all2, NA)
klsi<-ifelse(degree_all2 >= ave(degree_all2), V(g2)$Label, NA)
klsii<-ifelse(degree_all2 >= ave(degree_all2), V(g2)$name, NA)

# Remove the NA
kls <- na.omit(kls)
klsi <- na.omit(klsi)
klsii <- na.omit(klsii)

# Print the table
tab <- cbind(klsi, klsii ,kls)

tab3 <- data.frame(tab[, c(1,2)])
colnames(tab) = c("User_name", "Vertex_Id", "Total_Degree")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))





#
#
#
#

#Betweenness
#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
luk<-ifelse(bet2>=ave(bet2), bet2, NA)
tluk<-ifelse(bet2>=ave(bet2), V(g2)$Label, NA)
tluki<-ifelse(bet2>=ave(bet2), V(g2)$name, NA)

# Remove NA
luk <- na.omit(luk)
tluk <- na.omit(tluk)
tluki <- na.omit(tluki)

# Print table
tab <- cbind(tluk, tluki ,luk)

tab4 <- data.frame(tab[, c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "Betweenness")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


#Eigenvector
#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
juk<-ifelse(eg2>=ave(eg2), eg2, NA)
julk<-ifelse(eg2>=ave(eg2), V(g2)$Label, NA)
julki<-ifelse(eg2>=ave(eg2), V(g2)$name, NA)

# Remove NA
juk <- na.omit(juk)
julk <- na.omit(julk)
julki <- na.omit(julki)

# Print the table
tab <- cbind(julk, julki ,juk)

tab5 <- data.frame(tab[, c(1,2)])

colnames(tab) = c("User_name", "Vertex_Id", "Eigen_Vector")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



#Closeness

#{r echo=TRUE, message=FALSE, warning=FALSE}

# To compute the average degree
# To filter out the veritices with a degree value lower than the average degree
fg <- ifelse(close2>=ave(close2), close2, NA)
kk <- ifelse(close2>=ave(close2), V(mainclose2)$Label, NA)
kki <- ifelse(close2>=ave(close2), V(mainclose2)$name, NA)

# Removing NA
fg<- na.omit(fg)
kk<- na.omit(kk)
kki<- na.omit(kki)

# Print table
tab <- cbind(kk, kki ,fg)
tab6 <- data.frame(tab[,c(1,2)])
colnames(tab) = c("User_name", "Vertex_Id", "Closeness")
kbl(tab) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))




### List Of Influential Users in The Previous Network

#{r echo=TRUE, message=FALSE, warning=FALSE}

# Filtering the tables using sqldf package
a1 <- sqldf('SELECT * FROM tab1 INTERSECT SELECT * FROM tab2')
a2 <- sqldf('SELECT * FROM tab3 INTERSECT SELECT * FROM tab4')
a4 <- sqldf('SELECT * FROM tab5 INTERSECT SELECT * FROM tab6')

a2 <- sqldf('SELECT * FROM a1 INTERSECT SELECT * FROM a2')
a4 <- sqldf('SELECT * FROM a2 INTERSECT SELECT * FROM a4')

# Print the table
colnames(a4) = c("Name", "Vertex_Id")
kbl(a4) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



### List Of Loyal Users On Network
#{r echo=TRUE, message=FALSE, warning=FALSE}

# Using sqldf to filter out loyal users from the influential users from both data 
a1 <- sqldf('SELECT Name FROM a3 INTERSECT SELECT Name FROM a4')


# Printing teh table
colnames(a1) = c("User_name")
kbl(a1) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

### The three most influential commenters in the Recent Network

#{r echo=TRUE, message=FALSE, warning=FALSE}

# Using sqldf to filter teh top 3 influentials in the recent data
joke <- sqldf('SELECT * FROM tablee WHERE User_name IN (SELECT User_name FROM tablee JOIN a3 on tablee.User_name = a3.Name) ORDER BY Eigen_Vector DESC LIMIT 3')

# Printing table
colnames(joke) = c("Vertex_Id", "User_name", "In_Degree", "Out_Degree", "Total_Degree", "Betweenness", "Eigen_Vector")
kbl(joke) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



### The three most influential commenters in the Previous Network

#{r echo=TRUE, message=FALSE, warning=FALSE}

# Using sqldf to filter teh top 3 influentials in the recent data
joke2 <- sqldf('SELECT * FROM tablee2 WHERE User_name IN (SELECT User_name FROM tablee2 JOIN a4 on tablee2.User_name = a4.Name) ORDER BY Eigenvector DESC LIMIT 3')

# Printing table
colnames(joke2) = c("Vertex_Id", "User_name", "In_Degree", "Out_Degree", "Total_Degree", "Betweenness", "Eigen_Vector")
kbl(joke2) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



###Twitter Information on Recent Network

#{r echo=TRUE, message=FALSE, warning=FALSE}

# Retrieving twitter information of top three influential users in the recent data
userx <- joke$User_name

usr_df <- lookup_users(userx)
usr_df <- usr_df[, c(1:6,10,12,14,31,72:73,75:77,79,81:84)]

# Printing table
colnames(usr_df) = c("User_Id", "Status_Id", "Created_At", "Screen_Name", "Text", "Source", "Reply_To_Screen_Name", "Is_Retweet", "Retweet_Count", "Mentions_Screen_Name", "Status_Url", "Name", "Description", "Url", "Protected", "Friends_Count", "Statuses_Count", "Favourites_Count", "Account_Created_at", "Verified")
kbl(usr_df) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))




###Twitter Information On Previous Network

#{r include=TRUE,echo=FALSE, message=FALSE, warning=FALSE}

# Retrieving twitter information of top three influential users in the recent data
userx <- joke2$User_name

usr_df <- lookup_users(userx)
usr_df <- usr_df[, c(1:6,10,12,14,31,72:73,75:77,79,81:84)]

# Printing table
colnames(usr_df) = c("User_Id", "Status_Id", "Created_At", "Screen_Name", "Text", "Source", "Reply_To_Screen_Name", "Is_Retweet", "Retweet_Count", "Mentions_Screen_Name", "Status_Url", "Name", "Description", "Url", "Protected", "Friends_Count", "Statuses_Count", "Favourites_Count", "Account_Created_at", "Verified")
kbl(usr_df) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



###Egocentric Network of the Recent Network

#{r echo=TRUE, message=FALSE, warning=FALSE}

# Plotting the egocentric graph for the top three influtial users in the recent data

kot <- make_ego_graph(g, order= 1, nodes= V(g)[joke$Vertex_Id])
plot(kot[[1]], vertex.label = V(kot[[1]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[1]]), vertex.label.cex=0.8,   edge.arrow.size = 0.0005)

png("my-Egocentric-1-plot-Recent-graph.png", width=1200, height=1200)
par(mar=c(0,0,0,0))
plot(kot[[1]], vertex.label = V(kot[[1]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[1]]), vertex.label.cex=0.8,   edge.arrow.size = 0.0005)
dev.off()




plot(kot[[2]], vertex.label = V(kot[[2]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[2]]), vertex.label.cex=0.8,   edge.arrow.size = 0.0005)

png("my-Egocentric-2-plot-Recent-graph.png", width=1200, height=1200)
par(mar=c(0,0,0,0))
plot(kot[[2]], vertex.label = V(kot[[2]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[2]]), vertex.label.cex=0.8,   edge.arrow.size = 0.0005)
dev.off()



plot(kot[[3]], vertex.label = V(kot[[3]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[3]]), vertex.label.cex=2,   edge.arrow.size = 0.0005)

png("my-Egocentric-3-plot-Recent-graph.png", width=1200, height=1200)
par(mar=c(0,0,0,0))
plot(kot[[3]], vertex.label = V(kot[[3]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[3]]), vertex.label.cex=2,   edge.arrow.size = 0.0005)
dev.off()





###Egocentric Network of the Previous Network
#{r echo=TRUE, message=FALSE, warning=FALSE}

## Plotting the egocentric graph for the top three influtial users in the recent data

kot <- make_ego_graph(g2, order= 1, nodes= V(g2)[joke2$Vertex_Id])

plot(kot[[1]], vertex.label = V(kot[[1]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[1]]), vertex.label.cex=0.8,   edge.arrow.size = 0.0005)

png("my-Egocentric-1-plot-Previous-graph.png", width=1200, height=1200)
par(mar=c(0,0,0,0))
plot(kot[[1]], vertex.label = V(kot[[1]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[1]]), vertex.label.cex=0.8,   edge.arrow.size = 0.0005)
dev.off()




plot(kot[[2]], vertex.label = V(kot[[2]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[2]]), vertex.label.cex=0.8,   edge.arrow.size = 0.0005)

png("my-Egocentric-2-plot-Previous-graph.png", width=1200, height=1200)
par(mar=c(0,0,0,0))
plot(kot[[2]], vertex.label = V(kot[[2]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[2]]), vertex.label.cex=0.8,   edge.arrow.size = 0.0005)
dev.off()



plot(kot[[3]], vertex.label = V(kot[[3]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[3]]), vertex.label.cex=2,   edge.arrow.size = 0.0005)

png("my-Egocentric-3-plot-Previous-graph.png", width=1200, height=1200)
par(mar=c(0,0,0,0))
plot(kot[[3]], vertex.label = V(kot[[3]])$Label, vertex.size= eigen_centrality(g2)$vector*10, layout = layout.fruchterman.reingold(kot[[3]]), vertex.label.cex=2,   edge.arrow.size = 0.0005)
dev.off()






#{r eval=FALSE, include=FALSE, message=FALSE, warning=FALSE}
#neighbors(g, V(g)["n652"])
require(linkprediction)


#ho <- (neighborhood(g,order = 1,nodes = c(V(g)[1],V(g)[2])))




#