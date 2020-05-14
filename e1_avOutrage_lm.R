# Julian De Freitas, 2020

# Analysis script for De Freitas & Cikara
# Deliberately Biased Autonomous Vehicles Elicit The Most Outrage

## clear workspace
rm(list = ls()) 

options(download.file.method="libcurl")

## necessary libraries
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ggsignif)) {install.packages("ggsignif"); require(ggsignif)}
if (!require(lme4)) install.packages("lme4"); require(lme4)           
if (!require(ggforce)) {install.packages("ggforce"); require(ggsignif)}
if (!require(ggpubr)) {install.packages("ggpubr"); require(ggpubr)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
library("lmerTest")

##================================================================================================================
                                              ##IMPORT & PRE-PROCESS DATA##
##================================================================================================================

#read data
#dir <- setwd("[YOUR DATA DIRECTORY HERE]")
data <- read.csv('e1_data.csv')

#pre-process mturk data:

#define some categories for processing conditions
favored <- c("child", "woman", "athlete", "exec", "lawful")
unfavored <- c("elderly", "man", "obese", "homeless", "lawless")
categories <- list('child' = 'age', 'elderly' = 'age',
                   'man' = 'gender', 'woman' = 'gender',
                   'athlete' = 'fitness', 'obese' = 'fitness',
                   'lawful' = 'law', 'lawless' = 'law',
                   'exec' = 'status', 'homeless' = 'status')

#define new data frame that we'll extract preprocessed data into
d_subset <- array(dim=c(dim(data)[1], 21))
colnames(d_subset) <- c('full_cond_name','intention_cond','agent_cond_name','pref_cond','category_cond',
                        'blame_man','blame_av','blame_company','outrage_anger','outrage_punish','outrage_wrong',
                        'col_donate','col_volunteer','col_protest','col_socMedia','col_shareLike','col_shareNonLike',
                        'worry_self','worry_other','comp1','comp2')
d_subset <- as.data.frame(d_subset, stringsAsFactors=FALSE)

#extract the good data from the middle part of the raw data
for(i in 1:dim(data)[1]) {
  curr <- data[i,29:348][!is.na(data[i,29:348])] #for a given row, get only the non NA values
  d_subset[i,6:21] <- curr[curr!= ""] #and only the non-empty values
  d_subset[i,1] <- names(data[i,])[apply(data[i,], 1, function(j) which(j == d_subset[i,20]))][1]
  d_subset[i,2:3] <- strsplit(d_subset[i,1],"_")[[1]][3:4]
  d_subset[i,4] <- ifelse(d_subset[i,3] %in% favored, 'preferred', 'unpreferred')
  d_subset[i,5] <- categories[[d_subset[i,3]]]
}

#merge good data with first and last halves of the original data
data <- cbind(data[,1:28], d_subset, data[,349:356])

#check that we have equal numbers for each of our various conditions
table(data$intention_cond)
table(data$pref_cond)
table(data$category)
table(data$agent_cond_name)

# number of subjects before exclusions
n_original <- dim(data)[1]
n_original

##================================================================================================================
                                                ##EXCLUSIONS##
##================================================================================================================

#define some categories for processing comprehension items
favored_comp <- c("Child", "Woman", "Athlete", "Executive/Doctor", "Law-abiding person")
unfavored_comp <- c("Elderly person", "Man", "Large person", "Homeless person", "Law-breaking person")

#perform exclusions based on attention and comprehension checks
data$att1 <- ifelse( ((data$attention_check_1_3 > data$attention_check_1_2) &
                       (data$attention_check_1_2 > data$attention_check_1_1) &
                       (data$attention_check_1_2%%10 == 0) &
                       (data$attention_check_1_1 == 15)), 0, 1)
data$comp1_recode <- ifelse(data$comp1 == "Deliberately", "intent", "random")
data$comp2_recode <- ifelse(data$comp2 %in% favored_comp, 'preferred', 'unpreferred')

data <- subset(data, (data$att1 == 0 &
                      data$attention_check_2 == 0 &
                      data$comp1_recode == data$intention_cond &
                      data$comp2_recode == data$pref_cond))

#number of subjects after exclusions
n_after_exclusions <- dim(data)[1]
n_after_exclusions
percent_excluded <- (n_after_exclusions - n_original)/n_original #34%

## mean age and gender
mean(data$age,na.rm = TRUE) 
table(data$gender)[1]/sum(table(data$gender)) 

##================================================================================================================
                                                  ##DATA PREP##
##================================================================================================================

#assign simple variable names to measures of interest
#and convert them to factor or numeric
#also aggregate outrage and collective action items

#conds
intention_cond <- as.factor(data$intention_cond)
pref_cond <- as.factor(data$pref_cond)
social_cond <- as.factor(data$category)

#blame
blame_man <- as.numeric(data$blame_man)
blame_av <- as.numeric(data$blame_av)
blame_company <- as.numeric(data$blame_company)

#outrage
outrage_anger <- as.numeric(data$outrage_anger)
outrage_punish <- as.numeric(data$outrage_punish)
outrage_wrong <- as.numeric(data$outrage_wrong)

outrage_mat <- array(0,dim=c(dim(data)[1],3)) #get cronbach's alpha, then average items
outrage_mat[,1] <- outrage_anger
outrage_mat[,2] <- outrage_punish
outrage_mat[,3] <- outrage_wrong
cronbach.alpha(outrage_mat) #0.93

outrage <- rowMeans(outrage_mat) 
data$outrage <- outrage

#collective action
col_volunteer <- as.numeric(data$col_volunteer)
col_protest <- as.numeric(data$col_protest)
col_socMedia <- as.numeric(data$col_socMedia)
col_shareLike <- as.numeric(data$col_shareLike)
col_shareNonLike <- as.numeric(data$col_shareNonLike)

col_mat <- array(0,dim=c(dim(data)[1],5)) #get cronbach's alpha, then average items
col_mat[,1] <- col_volunteer
col_mat[,2] <- col_protest
col_mat[,3] <- col_socMedia
col_mat[,4] <- col_shareLike
col_mat[,5] <- col_shareNonLike
cronbach.alpha(col_mat) #0.85

col_action <- rowMeans(col_mat)
data$col_action <- col_action

#worry
worry_self <- as.numeric(data$worry_self)
worry_other <- as.numeric(data$worry_other)

##================================================================================================================
                                                      ##ANALYSIS##
##================================================================================================================

#define some labels
intention_labels <- c('intent', 'random')
pref_labels <- c('preferred', 'unpreferred')
social_labels <- unique(social_cond)

intention_cond_num <- as.numeric(intention_cond)
pref_cond_num <- as.numeric(pref_cond)
social_cond_num <- as.numeric(social_cond)

# (1.1) BLAME HUMAN
blame_man_mod <- lmer(blame_man ~ pref_cond_num*intention_cond_num + (1 | social_cond_num))
summary(blame_man_mod)

#followup to interaction: preferred v. unpreferred, intentional
var.test(blame_man[intention_cond == 'intent' & pref_cond == 'preferred'], 
         blame_man[intention_cond == 'intent' & pref_cond == 'unpreferred'])
blame_man_t1 <- t.test(blame_man[intention_cond == 'intent' & pref_cond == 'preferred'], 
                       blame_man[intention_cond == 'intent' & pref_cond == 'unpreferred'], 
                       var.equal=TRUE, paired=FALSE)
blame_man_t1

var.test(blame_man[intention_cond == 'random' & pref_cond == 'preferred'], 
         blame_man[intention_cond == 'random' & pref_cond == 'unpreferred'])
blame_man_t2 <- t.test(blame_man[intention_cond == 'random' & pref_cond == 'preferred'], 
                       blame_man[intention_cond == 'random' & pref_cond == 'unpreferred'], 
                       var.equal=TRUE, paired=FALSE)
blame_man_t2

#intentional unpreferred v. random preferred
var.test(blame_man[intention_cond == 'intent' & pref_cond == 'unpreferred'], 
         blame_man[intention_cond == 'random' & pref_cond == 'preferred'])
blame_man_t3 <- t.test(blame_man[intention_cond == 'intent' & pref_cond == 'unpreferred'],
                    blame_man[intention_cond == 'random' & pref_cond == 'preferred'],
                    var.equal=TRUE, paired=FALSE)
blame_man_t3



# (1.2) BLAME AV
blame_av_mod <- lmer(blame_av ~ pref_cond_num*intention_cond_num + (1 | social_cond_num))
summary(blame_av_mod)

#intentional unpreferred v. random preferred
var.test(blame_av[intention_cond == 'intent' & pref_cond == 'unpreferred'], 
         blame_av[intention_cond == 'random' & pref_cond == 'preferred'])
blame_av_t <- t.test(blame_av[intention_cond == 'intent' & pref_cond == 'unpreferred'],
                     blame_av[intention_cond == 'random' & pref_cond == 'preferred'],
                     var.equal=TRUE, paired=FALSE)
blame_av_t



# (1.3) BLAME COMPANY
blame_company_mod <- lmer(blame_company ~ pref_cond_num*intention_cond_num + (1 | social_cond_num))
summary(blame_company_mod)

#intentional unpreferred v. random preferred
var.test(blame_company[intention_cond == 'intent' & pref_cond == 'unpreferred'], 
         blame_company[intention_cond == 'random' & pref_cond == 'preferred'])
blame_company_t <- t.test(blame_company[intention_cond == 'intent' & pref_cond == 'unpreferred'],
                          blame_company[intention_cond == 'random' & pref_cond == 'preferred'],
                          var.equal=FALSE, paired=FALSE)
blame_company_t



# (2) OUTRAGE
outrage_mod <- lmer(outrage ~ pref_cond_num*intention_cond_num + (1 | social_cond_num))
summary(outrage_mod)

#followup: preferred v. unpreferred, intentional
var.test(outrage[intention_cond == 'intent' & pref_cond == 'preferred'], 
         outrage[intention_cond == 'intent' & pref_cond == 'unpreferred'])
outrage_t <- t.test(outrage[intention_cond == 'intent' & pref_cond == 'preferred'], 
                     blame_man[intention_cond == 'intent' & pref_cond == 'unpreferred'], 
                     var.equal=FALSE, paired=FALSE)
outrage_t

var.test(outrage[intention_cond == 'random' & pref_cond == 'preferred'], 
         outrage[intention_cond == 'random' & pref_cond == 'unpreferred'])
outrage_2 <- t.test(outrage[intention_cond == 'random' & pref_cond == 'preferred'], 
                    blame_man[intention_cond == 'random' & pref_cond == 'unpreferred'], 
                    var.equal=TRUE, paired=FALSE)
outrage_2


#intentional unpreferred v. random preferred
var.test(outrage[intention_cond == 'intent' & pref_cond == 'unpreferred'], 
         outrage[intention_cond == 'random' & pref_cond == 'preferred'])
outrage_t3 <- t.test(outrage[intention_cond == 'intent' & pref_cond == 'unpreferred'],
                    outrage[intention_cond == 'random' & pref_cond == 'preferred'],
                    var.equal=FALSE, paired=FALSE)
outrage_t3



# (3) COLLECTIVE ACTION
col_action_mod <- lmer(col_action ~ pref_cond_num*intention_cond_num + (1 | social_cond_num))
summary(col_action_mod)

#intentional unpreferred v. random preferred
var.test(col_action[intention_cond == 'intent' & pref_cond == 'unpreferred'], 
         col_action[intention_cond == 'random' & pref_cond == 'preferred'])
col_action_t <- t.test(col_action[intention_cond == 'intent' & pref_cond == 'unpreferred'],
                       col_action[intention_cond == 'random' & pref_cond == 'preferred'],
                       var.equal=TRUE, paired=FALSE)
col_action_t



# (4.1) WORRY SELF
worry_self_mod <- lmer(worry_self ~ pref_cond_num*intention_cond_num + (1 | social_cond_num))
summary(worry_self_mod)

#intentional unpreferred v. random preferred
var.test(worry_self[intention_cond == 'intent' & pref_cond == 'unpreferred'], 
         worry_self[intention_cond == 'random' & pref_cond == 'preferred'])
worry_self_t <- t.test(worry_self[intention_cond == 'intent' & pref_cond == 'unpreferred'],
                       worry_self[intention_cond == 'random' & pref_cond == 'preferred'],
                       var.equal=TRUE, paired=FALSE)
worry_self_t



# (4.2) WORRY OTHER
worry_other_mod <- lmer(worry_other ~ pref_cond_num*intention_cond_num + (1 | social_cond_num))
summary(worry_other_mod)

#intentional unpreferred v. random preferred
var.test(worry_other[intention_cond == 'intent' & pref_cond == 'unpreferred'], 
         worry_self[intention_cond == 'random' & pref_cond == 'preferred'])
worry_other_t <- t.test(worry_other[intention_cond == 'intent' & pref_cond == 'unpreferred'],
                        worry_other[intention_cond == 'random' & pref_cond == 'preferred'],
                        var.equal=TRUE, paired=FALSE)
worry_other_t

##=============================================================================================================
                                         ##FIG 1: PLOT MAIN RESULTS##
##================================================================================================================

#color pallettes: https://www.datanovia.com/en/blog/ggplot-colors-best-tricks-you-will-love/
#violin + strip plot: https://bio723-class.github.io/Bio723-book/introduction-to-ggplot2.html

## VIOLIN PLOTS ##
data$intention_cond <- as.factor(data$intention_cond)
data$pref_cond <- as.factor(data$pref_cond)
data$category_cond <- as.factor(data$category_cond)
data$blame_company <- as.numeric(data$blame_company)
data$outrage <- as.numeric(data$outrage)
data$col_action <- as.numeric(data$col_action)
pref_graph_labels <- c("Preferred", "Unpreferred")


# (1) BLAME COMPANY
p1_1<-ggplot(data,aes(x=factor(intention_cond),y=blame_company,fill=factor(pref_cond)),color=factor(pref_cond)) +  
  theme_bw()+coord_cartesian(ylim=c(1,100))
p1_1<- p1_1+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_discrete(labels=c("Deliberate", "Random"))+
  ggtitle("Blame of Manufacturer")+
  scale_fill_manual(values = c("#56B4E9", "#009E73"),name= "Target Killed:",
                    labels=pref_graph_labels)+
  ylab ("") + xlab("")+
  theme_classic()+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(plot.title = element_text(size=14, hjust=0.5))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size=16))+
  geom_violin(width=0.9, alpha=0.38, size=0.75)+  
  geom_sina(alpha=0.6, size=0.95, color = "#999999")+
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9))+
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p1_1


# (2) OUTRAGE
p2_1<-ggplot(data,aes(x=factor(intention_cond),y=outrage,fill=factor(pref_cond)),color=factor(pref_cond)) +  
  theme_bw()+coord_cartesian(ylim=c(1,100))
p2_1<- p2_1+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_discrete(labels=c("Deliberate", "Random"))+
  ggtitle("Outrage Toward Manufacturer")+
  scale_fill_manual(values = c("#56B4E9", "#009E73"),name= "Outcome:Killed",
                    labels=pref_graph_labels)+
  xlab ("") + ylab ("") +
  theme_classic()+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(plot.title = element_text(size=14, hjust=0.5))+
  geom_violin(width=0.9, alpha=0.38, size=0.75)+  
  geom_sina(alpha=0.6, size=0.95, color = "#999999")+
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9))+
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p2_1

# (3) COLLECTIVE ACTION
p3_1<-ggplot(data,aes(x=factor(intention_cond),y=col_action,fill=factor(pref_cond)),color=factor(pref_cond)) +  
  theme_bw()+coord_cartesian(ylim=c(1,100))
p3_1<- p3_1+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  scale_x_discrete(labels=c("Deliberate", "Random"))+
  ggtitle("Collective Action Against Manufacturer")+
  scale_fill_manual(values = c("#56B4E9", "#009E73"),name= "Outcome:Killed",
                    labels=pref_graph_labels)+
  xlab ("") + ylab ("") +
  theme_classic()+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(plot.title = element_text(size=14, hjust=0.5))+
  geom_violin(width=0.9, alpha=0.38, size=0.75)+  
  geom_sina(alpha=0.6, size=0.95, color = "#999999")+
  stat_summary(fun.data = "mean_se", color = "black", 
               size=0.4, fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9))+
  stat_summary(fun.data = "mean_se", color = "black", 
               fun.args = list(mult = 1), 
               position = position_dodge(width = 0.9),
               geom="errorbar", width = 0.2)
p3_1


dev.new(width=13,height=6,noRStudioGD = TRUE)
figure<-ggarrange(p1_1, p2_1, p3_1, nrow=1,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean Rating", color="black", face ="plain",size=16, rot=90),
                bottom = text_grob("AV Programming", color="black", face ="plain",size=16)) 


#================================================================================================================
                        ##PREPARE FOR PLOTTING SOME INDIVIDUAL SOCIAL CATEGORY RESULTS##
##================================================================================================================

## (1_2) blame company
blame_company_plot <- array(0,dim=c(10, 6))
colnames(blame_company_plot) <- c('intention', 'social', 'mean','sd','n','sem')
blame_company_plot <- as.data.frame(blame_company_plot, stringsAsFactors=FALSE)

counter = 1
for(i in 1:length(unique(intention_cond))) {
  for(k in 1:length(unique(social_cond))) {
    blame_company_plot[counter,] <- c(i, k, mean(blame_company[ intention_cond == intention_labels[i] & social_cond == social_labels[k] ]),
                                      sd(blame_company[ intention_cond == intention_labels[i] & social_cond == social_labels[k] ]),
                                      length(blame_company[ intention_cond == intention_labels[i] & social_cond == social_labels[k] ]),0)
    blame_company_plot[counter,6] <- blame_company_plot[counter,4]/sqrt(blame_company_plot[counter,5])
    counter = counter+1
  }
}



## (2_2) outrage
outrage_plot <- array(0,dim=c(10, 6))
colnames(outrage_plot) <- c('intention', 'social', 'mean','sd','n','sem')
outrage_plot <- as.data.frame(outrage_plot, stringsAsFactors=FALSE)

counter = 1
for(i in 1:length(unique(intention_cond))) {
  for(k in 1:length(unique(social_cond))) {
    outrage_plot[counter,] <- c(i, k, mean(outrage[ intention_cond == intention_labels[i] & social_cond == social_labels[k] ]),
                                sd(outrage[ intention_cond == intention_labels[i] & social_cond == social_labels[k] ]),
                                length(outrage[ intention_cond == intention_labels[i] & social_cond == social_labels[k] ]),0)
    outrage_plot[counter,6] <- outrage_plot[counter,4]/sqrt(outrage_plot[counter,5])
    counter = counter+1
  }
}



## (3_2) collective action
col_action_plot <- array(0,dim=c(10, 6))
colnames(col_action_plot) <- c('intention', 'social', 'mean','sd','n','sem')
col_action_plot <- as.data.frame(col_action_plot, stringsAsFactors=FALSE)

counter = 1
for(i in 1:length(unique(intention_cond))) {
  for(k in 1:length(unique(social_cond))) {
    col_action_plot[counter,] <- c(i, k, mean(col_action[ intention_cond == intention_labels[i] & social_cond == social_labels[k] ]),
                                   sd(col_action[ intention_cond == intention_labels[i] & social_cond == social_labels[k] ]),
                                   length(col_action[ intention_cond == intention_labels[i] & social_cond == social_labels[k] ]),0)
    col_action_plot[counter,6] <- col_action_plot[counter,4]/sqrt(col_action_plot[counter,5])
    counter = counter+1
  }
}

#================================================================================================================
                             ##FIG 2: PLOT SOME INDIVIDUAL SOCIAL CATEGORY RESULTS##
##================================================================================================================

social_plot_labels <- c("Fitness", "Age", "Gender", "Status", "Law")

# (1_2) BLAME COMPANY
p1_2<-ggplot(blame_company_plot,aes(x=factor(intention),y=mean,fill=factor(social)),color=factor(social)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(1,100)) 
p1_2<- p1_2+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+
  scale_x_discrete(breaks = 1:length(unique(intention_cond)), labels=c("Intentional", "Random"))+
  ggtitle("Blame of Manufacturer")+
  scale_fill_manual(values = c("#1E10AD", "#4E85C9", "#68AB6D", "#E8B433", "#DC595C"),name= "Social Category:",
                    labels=social_plot_labels, guide = guide_legend(reverse = TRUE))+
  xlab ("") + ylab ("") +
  theme_classic()+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(plot.title = element_text(size=14, hjust=0.5))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size=16))
p1_2

# (2_2) OUTRAGE
p2_2<-ggplot(outrage_plot,aes(x=factor(intention),y=mean,fill=factor(social)),color=factor(social)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(1,100)) 
p2_2<- p2_2+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+
  scale_x_discrete(breaks = 1:length(unique(intention_cond)), labels=c("Intentional", "Random"))+
  ggtitle("Outrage Toward Manufacturer")+
  scale_fill_manual(values = c("#1E10AD", "#4E85C9", "#68AB6D", "#E8B433", "#DC595C"),name= "Social Category",
                    labels=social_plot_labels, guide = guide_legend(reverse = TRUE))+
  xlab ("") + ylab ("") +
  theme_classic()+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(plot.title = element_text(size=14, hjust=0.5))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size=16))
p2_2

# (3_2) COLLECTIVE ACTION
p3_2<-ggplot(col_action_plot,aes(x=factor(intention),y=mean,fill=factor(social)),color=factor(social)) +  
  stat_summary(fun.y=mean,position=position_dodge(),geom="bar")+
  theme_bw()+coord_cartesian(ylim=c(1,100)) 
p3_2<- p3_2+theme(text = element_text(size=16),panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
  geom_errorbar(aes(ymax=mean+sem, ymin=mean-sem), position="dodge")+
  scale_x_discrete(breaks = 1:length(unique(intention_cond)), labels=c("Intentional", "Random"))+
  ggtitle("Collective Action Against Manufacturer")+
  scale_fill_manual(values = c("#1E10AD", "#4E85C9", "#68AB6D", "#E8B433", "#DC595C"),name= "Social Category",
                    labels=social_plot_labels, guide = guide_legend(reverse = TRUE))+
  xlab ("") + ylab ("") +
  theme_classic()+
  theme(axis.text.x = element_text(size=12))+
  theme(axis.text.y = element_text(size=10))+
  theme(plot.title = element_text(size=14, hjust=0.5))+
  theme(legend.text = element_text(size=15))+
  theme(legend.title = element_text(size=16))
p3_2

dev.new(width=13,height=6,noRStudioGD = TRUE)
figure<-ggarrange(p1_2, p2_2, p3_2, nrow=1,ncol=3,common.legend = TRUE, legend="top", vjust = 1.0, hjust=0.5) 
annotate_figure(figure,left = text_grob("Mean Rating", color="black", face ="plain",size=16, rot=90),
                bottom = text_grob("AV Programming", color="black", face ="plain",size=16)) 

##================================================================================================================
##END##
##================================================================================================================




















