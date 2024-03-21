library(igraph)
library(intergraph)
library(sna)
library(network)
library(dplyr)
library(RColorBrewer)
library(viridis)
library(Rcpp)
library(colourvalues)
library(ggplot2)

# Turn network edgelists into network diagrams with nodes positioned in the same location each time, nodes sized by (in)degree and colored by final grade, and edges colored by whether or not they appear in both the interaction and recognition networks for that semester

brenau_attr <- read.csv("Brenau_Attributes_Anon.csv")
brenau_attr$FinalGradeNumeric <- as.numeric(brenau_attr$FinalGradeNumeric)

colrs<-data.frame(fallgrade_rounded=c(seq(70,99,1),NA),fall_color=c(viridis(30),"white"))
brenau_attr <- brenau_attr %>% mutate(fallgrade_rounded=round(brenau_attr$FinalGradeNumeric,digits=0))
brenau_attr<-left_join(brenau_attr,colrs,by="fallgrade_rounded")

brenau_attr_spring <- read.csv("Brenau_Attributes_Anon.csv")
brenau_attr_spring <- brenau_attr_spring[c(1,2,3,5,6,8:17,19:21),]
brenau_attr_spring$FinalGradeNumeric <- as.numeric(brenau_attr_spring$FinalGradeNumeric)

colrs<-data.frame(springgrade_rounded=c(seq(70,99,1),NA),spring_color=c(viridis(30),"white"))
brenau_attr_spring <- brenau_attr_spring %>% mutate(springgrade_rounded=round(brenau_attr_spring$FinalGradeNumeric_Spring,digits=0))
brenau_attr_spring<-left_join(brenau_attr_spring,colrs,by="springgrade_rounded")

lec_interactions<-read.csv("Interaction_Edgelist_Fall_Anon.csv")

lecint_net <- graph_from_data_frame(d=lec_interactions, vertices=brenau_attr, directed=T)
lecint_net <- simplify(lecint_net, remove.multiple = F, remove.loops = T) 

lec_perceptions<-read.csv("Recognition_Edgelist_Fall_Anon.csv")

lecper_net <- graph_from_data_frame(d=lec_perceptions, vertices=brenau_attr, directed=T)
lecper_net <- simplify(lecper_net, remove.multiple = F, remove.loops = T) 


lec_interactions_spring<-read.csv("Interaction_Edgelist_Spring_Anon.csv")
lec_interactions_spring<-lec_interactions_spring[lec_interactions_spring$from!=lec_interactions_spring$to,]

lecint_net_spring <- graph_from_data_frame(d=lec_interactions_spring, vertices=brenau_attr_spring, directed=T)
lecint_net_spring <- simplify(lecint_net_spring,remove.multiple = F, remove.loops = T) 

lec_perceptions_spring<-read.csv("Recognition_Edgelist_Spring_Anon.csv")
lec_perceptions_spring<-lec_perceptions_spring[lec_perceptions_spring$from!=lec_perceptions_spring$to,]

lecper_net_spring <- graph_from_data_frame(d=lec_perceptions_spring, vertices=brenau_attr_spring, directed=T)
lecper_net_spring <- simplify(lecper_net_spring,remove.multiple = F, remove.loops = T)



# graph layouts
# g1
set.seed(1)
lecper_net<-asIgraph(lecper_net)
layg1 <- layout.fruchterman.reingold(lecper_net)

# g2
set.seed(2)
lecint_net<-asIgraph(lecint_net)
layg2 <- layout.fruchterman.reingold(lecint_net)

# g3
set.seed(3)
lecper_net_spring<-asIgraph(lecper_net_spring)
layg3 <- layout.fruchterman.reingold(lecper_net_spring)

# g4
set.seed(4)
lecint_net_spring<-asIgraph(lecint_net_spring)
layg4 <- layout.fruchterman.reingold(lecint_net_spring)



# overwrite coords for shared nodes
layg2[which(V(lecint_net)$name %in% V(lecper_net)$name), ] <- 
  layg1[which(V(lecper_net)$name %in% V(lecint_net)$name),]

layg3<-layg1[c(1,2,3,5,6,8:17,19:21),]
layg4<-layg1[c(1,2,3,5,6,8:17,19:21),]


xlim <- range(c(layg1[,1], layg2[,1]))
ylim <- range(c(layg1[,2], layg2[,2]))


E(lecper_net)$overlap <- lec_perceptions$overlap_with_lecture_interactions
E(lecint_net)$overlap <- lec_interactions$overlap_with_lecture_perceptions

E(lecper_net)$color[E(lecper_net)$overlap==1] <- 'grey5'
E(lecper_net)$color[E(lecper_net)$overlap==0] <- 'grey60'

E(lecint_net)$color[E(lecint_net)$overlap==1] <- 'grey5'
E(lecint_net)$color[E(lecint_net)$overlap==0] <- 'grey60'

E(lecper_net_spring)$overlap <- lec_perceptions_spring$overlap_with_lecture_interactions
E(lecint_net_spring)$overlap <- lec_interactions_spring$overlap_with_lecture_perceptions

E(lecper_net_spring)$color[E(lecper_net_spring)$overlap==1] <- 'grey5'
E(lecper_net_spring)$color[E(lecper_net_spring)$overlap==0] <- 'grey60'

E(lecint_net_spring)$color[E(lecint_net_spring)$overlap==1] <- 'grey5'
E(lecint_net_spring)$color[E(lecint_net_spring)$overlap==0] <- 'grey60'

V(lecper_net)$centrality.in <- igraph::degree(lecper_net, mode = 'in')
V(lecint_net)$centrality.in <- igraph::degree(lecint_net, mode = 'all')
V(lecper_net)$size <- (V(lecper_net)$centrality.in*5)
V(lecint_net)$size <- (V(lecint_net)$centrality.in*5)

V(lecper_net_spring)$centrality.in <- igraph::degree(lecper_net_spring, mode = 'in')
V(lecint_net_spring)$centrality.in <- igraph::degree(lecint_net_spring, mode = 'all')
V(lecper_net_spring)$size <- (V(lecper_net_spring)$centrality.in*5)
V(lecint_net_spring)$size <- (V(lecint_net_spring)$centrality.in*5)


plot(lecper_net, layout=layg1, xlim=xlim, ylim=ylim, rescale=FALSE,edge.arrow.size=0.3,vertex.label=NA,edge.width=1.6,vertex.color=brenau_attr$fall_color)
plot(lecint_net, layout=layg2, xlim=xlim, ylim=ylim, rescale=FALSE,edge.arrow.size=0.3,vertex.label=NA,edge.width=1.6,vertex.color=brenau_attr$fall_color)
plot(lecper_net_spring, layout=layg3, xlim=xlim, ylim=ylim, rescale=FALSE,edge.arrow.size=0.3,vertex.label=NA,edge.width=1.6,vertex.color=brenau_attr_spring$spring_color)
plot(lecint_net_spring, layout=layg4, xlim=xlim, ylim=ylim, rescale=FALSE,edge.arrow.size=0.3,vertex.label=NA,edge.width=1.6,vertex.color=brenau_attr_spring$spring_color)

# Extract descriptive statistics of each network, for example:

summary(lecper_net)
edge_density(lecper_net)
reciprocity(lecper_net)
transitivity(lecper_net)
centralization.degree(lecper_net,mode="in")$centralization

# Scatterplot comparing recognition network indegree in fall and spring for each student

noms<-as.data.frame(table(lec_perceptions$to))

brenau_attr<-brenau_attr %>% mutate(lec_noms=vector(mode = "numeric", length = length(brenau_attr$ID)))

for (i in 1:length(brenau_attr$ID)) {
  brenau_attr$lec_noms[i]<-ifelse(brenau_attr$ID[i]%in%noms$Var1==TRUE,noms$Freq[noms$Var1==brenau_attr$ID[i]],0)
}

noms_spring<-as.data.frame(table(lec_perceptions_spring$to))

brenau_attr<-brenau_attr %>% mutate(lec_noms_spring=vector(mode = "numeric", length = length(brenau_attr$ID)))

for (i in 1:length(brenau_attr$ID)) {
  brenau_attr$lec_noms_spring[i]<-ifelse(brenau_attr$ID[i]%in%noms_spring$Var1==TRUE,noms_spring$Freq[noms_spring$Var1==brenau_attr$ID[i]],0)
}

ggplot(brenau_attr[is.na(brenau_attr$FinalGradeNumeric_Spring)==FALSE,], aes(x= lec_noms, y=lec_noms_spring))+ geom_jitter(width=0.2,size=2.5) + theme_minimal() + xlab("Fall recognition network indegree") + ylab("Spring recognition network indegree") + theme(legend.position = c(0.8,0.8))+
  theme(axis.text = element_text(color="#02080b", size = 16),axis.title = element_text(color="#02080b", size = 16))+
  theme(legend.position = c(0.2,0.85),legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),legend.text = element_text(color="#02080b", size = 16),legend.title = element_text(color="#02080b", size = 16,face="bold"))+theme(legend.title = element_blank(),legend.text = element_text(size=16,color='black'),strip.background = element_rect(colour='gray20', size=1),strip.text=element_text(size=16,color='black',face="bold")) +
  theme(panel.border = element_rect(color = "gray20", fill = NA, size = 1))+
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14))+scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14))

# Scatterplot comparing recognition network indegree and grade for each student in each semester

brenau_attr<-brenau_attr[,c(1,2,3,6,7)]

grades_fall<-brenau_attr[,c(1,2,4)]
colnames(grades_fall)<-c("ID","Grade","Indegree")
grades_fall <- grades_fall %>% mutate(Semester="Fall")

grades_spring<-brenau_attr[,c(1,3,5)]
colnames(grades_spring)<-c("ID","Grade","Indegree")
grades_spring <- grades_spring %>% mutate(Semester="Spring")

grades_combined<-rbind(grades_fall,grades_spring)

ggplot(grades_combined, aes(x= Grade, y=Indegree,color=Semester))+ geom_jitter(width=0.2,size=2.5) + theme_minimal() + xlab("Final course grade (%)") + ylab("Recognition network indegree") + theme(legend.position = c(0.8,0.8))+ scale_color_manual(values=c("#8b3d01","#fea867"))+
  theme(axis.text = element_text(color="#02080b", size = 16),axis.title = element_text(color="#02080b", size = 16))+
  theme(legend.position = c(0.2,0.85),legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),legend.text = element_text(color="#02080b", size = 16),legend.title = element_text(color="#02080b", size = 18,face="bold"))+theme(legend.title = element_blank(),legend.text = element_text(size=16,color='black'),strip.background = element_rect(colour='gray20', size=1),strip.text=element_text(size=16,color='black',face="bold")) +
  theme(panel.border = element_rect(color = "gray20", fill = NA, size = 1))+
  scale_x_continuous(breaks=c(70,75,80,85,90,95,100))+scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14))


# Scatterplot comparing recognition network indegree and interaction network degree for each student in each semester

lecint_net <- intergraph::asNetwork(lecint_net)
int_degree_fall<-as.data.frame(degree(lecint_net))
brenau_attr_fall<-cbind(brenau_attr,int_degree_fall)
brenau_attr_fall<-brenau_attr_fall[is.na(brenau_attr_fall$FinalGradeNumeric)==FALSE,]
brenau_attr_fall<-brenau_attr_fall[,c(1,4,6)]
colnames(brenau_attr_fall)<-c("ID","RecognitionIndegree","InteractionDegree")
brenau_attr_fall <- brenau_attr_fall %>% mutate(Semester="Fall")

lecint_net_spring <- intergraph::asNetwork(lecint_net_spring)
int_degree_spring<-as.data.frame(degree(lecint_net_spring))
brenau_attr_spring<-cbind(brenau_attr_spring,int_degree_spring)
noms_spring<-as.data.frame(table(lec_perceptions_spring$to))
brenau_attr_spring<-brenau_attr_spring %>% mutate(lec_noms_spring=vector(mode = "numeric", length = length(brenau_attr$ID)))
for (i in 1:length(brenau_attr$ID)) {
  brenau_attr_spring$lec_noms_spring[i]<-ifelse(brenau_attr_spring$ID[i]%in%noms_spring$Var1==TRUE,noms_spring$Freq[noms_spring$Var1==brenau_attr_spring$ID[i]],0)
}
brenau_attr_spring<-brenau_attr_spring[,c(1,6,7)]
colnames(brenau_attr_spring)<-c("ID","InteractionDegree","RecognitionIndegree")
brenau_attr_spring <- brenau_attr_spring %>% mutate(Semester="Spring")

brenau_attr_all<-rbind(brenau_attr_fall,brenau_attr_spring)

ggplot(brenau_attr_all, aes(x= InteractionDegree, y=RecognitionIndegree,color=Semester))+ geom_jitter(width=0.2,size=2.5) + theme_minimal() + xlab("Interaction network degree") + ylab("Recognition network indegree") + theme(legend.position = c(0.8,0.8))+ scale_color_manual(values=c("#8b3d01","#fea867"))+
  theme(axis.text = element_text(color="#02080b", size = 16),axis.title = element_text(color="#02080b", size = 16))+
  theme(legend.position = c(0.85,0.85),legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"),legend.text = element_text(color="#02080b", size = 16),legend.title = element_text(color="#02080b", size = 16,face="bold"))+theme(legend.title = element_blank(),legend.text = element_text(size=16,color='black'),strip.background = element_rect(colour='gray20', size=1),strip.text=element_text(size=16,color='black',face="bold")) +
  theme(panel.border = element_rect(color = "gray20", fill = NA, size = 1))+
  scale_x_continuous(breaks=c(0,2,4,6,8,10,12,14,16))+scale_y_continuous(breaks=c(0,2,4,6,8,10,12,14))

