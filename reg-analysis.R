#!/usr/bin/env Rscript

if(! "librarian" %in% rownames(installed.packages())) {
  install.packages("librarian", repos = "http://cran.us.r-project.org")
}

# libs --------------------------------------------------------------------
librarian::shelf(rstudioapi,
                 renv,
                 mltools,
                 data.table,
                 MASS,
                 ggplot2,
                 GGally,
                 ggcorrplot,
                 stringr,
                 caret,
                 olsrr,
                 leaps,
                 dplyr,
                 VIM,
                 h2o,
                 Boruta,
                 cowplot,
                 CombMSC,
                 cran_repo = 'https://cran.r-project.org')

#renv::clean()
setwd(dirname(getSourceEditorContext()$path))

student.data=read.table("student-data.csv", sep=",", header=T)

d=student.data
str(d)

# encode categoricals ------
char.df<- d %>% select_if(is.character)

fac.df<-d
fac.df <- as.data.frame(unclass(fac.df), stringsAsFactors = TRUE)


others.df<-d %>% select(!where(is.character))
others.df

enc.st.d<-cbind(one_hot(
  as.data.table(
    lapply(char.df, as.factor)
  )
  ,dropCols = T
), others.df)

# test normality ---------
plot(Hmisc::describe(enc.st.d)) 

data.table(Column=names(d),
           P=apply(enc.st.d, 2,function(x) shapiro.test(x)$p.value)
)

# cor matrix --------
corr.df=fac.df %>% select_if(is.numeric)
corr.df
corMtrx <- cor(corr.df)
print(corMtrx)
highcorr=findCorrelation(corMtrx, names=F, exact=F, cutoff=0.5)
highcorr

features<-names(corr.df[,..highcorr])
features<-features[features != "G3.portugese"]

dims=length(colnames(corMtrx))
corrDF <- expand.grid(row = 1:dims, col = 1:dims)
corrDF$correlation <- as.vector(corMtrx)
levelplot(correlation ~ row + col, corrDF)

corr = round(cor(corr.df), 2)
p.mat <- cor_pmat(corr.df)
ggcorrplot(corr,
           colors = c("white", "white", "red"),
           type = c("lower"),
           insig = c("pch", "blank"), pch = 10, pch.col = "gray", pch.cex =1,
           hc.order=F,
           # hc.method = "median",
           sig.level=.1,
           tl.srt = 90,
           tl.cex=9,
           outline.color = "white",
           ggtheme=ggplot2::theme_dark(),
           p.mat = p.mat,
           title="Correlation heatmap among student variables") 
# +
  # theme(axis.text.x = element_text(margin=margin(0,0,-15,-15)),  # Order: top, right, bottom, left
  #       axis.text.y = element_text(margin=margin(0,0,15,15)),
  #       panel.grid.major=element_blank()
  #       ) 

# var selection ------------
subset.model=regsubsets(G3.portugese~.,data=fac.df,
                        really.big=T,
                        nbest=20,method=c("exhaustive"))
summary(subset.model)

onlyf<-fac.df %>% select_if(is.factor)
for (of in names(onlyf)) {
  print(of)
  print(contrasts(onlyf[,of]))
}

# 
# plot(subset.model, scale="r2")
plot(subset.model, scale="adjr2")
# plot(subset.model, scale="bic")
# plot(subset.model, scale="Cp")
# 

reg=lm(G3.portugese ~ ., data=fac.df)
summary(reg)


# bestsubset=ols_step_best_subset(reg)
# bestsubset
# 
# allpos=ols_step_all_possible(reg)
# allpos
# plot(allpos)

Hmisc::describe(fac.df)
# 
# boruta_output <- Boruta(G3.portugese ~ ., data=fac.df, doTrace=0)  
# plot(boruta_output)
# boruta_signif <- getSelectedAttributes(boruta_output, withTentative = TRUE)
# print(boruta_signif)  
# roughFixMod <- TentativeRoughFix(boruta_output)
# boruta_signif <- getSelectedAttributes(roughFixMod)
# print(boruta_signif)
# attStats(roughFixMod)
# # plotImpHistory(roughFixMod)
# cnm<-names(roughFixMod$finalDecision[which(roughFixMod$finalDecision == 'Confirmed')])
# 
# cnm
# features<-unlist(boruta_signif)

leap.features=c(
   "school"
  ,"Mjob"
  ,"reason"
  ,"guardian"
  ,"failures.math"
  ,"failures.portugese"
  ,"G1.portugese"
  ,"G2.portugese"
)

features<-append(leap.features, "G3.portugese")

seltd<-fac.df[,features]

seltd<-as.data.frame(seltd)

features

# EDA--------------

# eda.df<-cbind(region=df.reg[,c("region")], binnedInc=df.bi[,c("binnedInc")])
# eda.df<-cbind(eda.df, fsel[features][,-c(9:10)])
# eda.df

reencode<-function(cn, df) {
  x<-str_split_fixed(names(df)[max.col(df)], '_', 2)[,2]
  return (x)
}

xx<-cbind(
  sex=reencode("sex", seltd[,c(1:2)]),
  schoolsup=reencode("schoolsup", seltd[,c(3:4)]),
  higher=reencode("higer", seltd[,c(5:6)])
)

eda.df<-cbind(xx, enc.st.d[,..features][,-c(1:6)])
eda.df
eda.df<-seltd
# rank distribution
ggplot(data=eda.df,aes(x=G3.portugese,fill=sex))+
  geom_bar(position='dodge')+
  facet_grid(. ~ schoolsup)+
  ggtitle("Bar-plot showing rank distribution, separated by gender and discipline")

#histogram
ggplot(data=eda.df,aes(x=G3.portugese,fill=higher))+
  geom_density(alpha=0.4)+
  facet_grid(. ~ sex)+
  ggtitle("Density curves showing salary distribution, separated by gender and discipline")

# boxplots
ggplot(data=eda.df,aes(x=sex,y=G3.portugese,fill=sex))+
  geom_boxplot(notch=TRUE)+
  facet_grid(. ~ higher + schoolsup)+
  ggtitle("Notched boxplots showing deathRate distribution, separated by region and discipline")

# violinplots
ggplot(data=eda.df,aes(x=sex,y=G3.portugese,fill=sex))+
  geom_violin()+
  geom_boxplot(fill='darkred',width=0.1,notch=TRUE)+
  geom_point(position='jitter',size=1)+
  facet_grid(. ~ higher + schoolsup)+
  ggtitle("Notched violinplots showing salary distribution, separated by gender and discipline")

plotpervar<-function(featrs) {
  ln=length(featrs)
  plots.elems<-list()
  for(i in seq_along(featrs[-ln])) {
    plots.elems[[i]]<-
      ggplot(eda.df,aes_string(featrs[c(ln)],featrs[c(i)],colour="sex"))+
      geom_point()+geom_rug()
  }
  
  return (plots.elems)
}

plot_grid(plotlist=plotpervar(intersect(names(eda.df), features)))

ggpairs(eda.df,mapping=ggplot2::aes(colour = school))


#Modelling --------
do_model<-function(apply_feat) {
  ln=length(apply_feat)
  seq_along(apply_feat[-ln])
  
  models<-list()
  for(i in seq_along(apply_feat[-ln])){
    models[[i]]<- lm(G3.portugese~.,data=seltd[,apply_feat[c(1:i,ln)]])
  }
  return(
    list(
      models,
      lapply(models,summary),
      do.call(anova,models),
      lapply(models, PRESS.lm)
    )
  )
}

features<-features[features %in% c(
  "sex_M",
  "schoolsup_yes",
  "higher_yes",
  "age",
  "absences.portugese",
  "G2.math",
  "G3.math",
  "G1.portugese",
  "G2.portugese"
  ) == FALSE]


features<-features[features %in% c(
  "guardian_mother"
  ,"absences.portugese"
  ,"G1.portugese"
  ,"G2.portugese"
  ,"address_R"
  ,"famsize_GT3"
  ,"Pstatus_A"
  ,"Fjob_other"
  ,"famsup_no"
  ,"higher_no"
  ,"nursery_no"
  ,"internet_no"
  ,"romantic_no"
  ,"paid.math_no"
) == FALSE]

levels(seltd$Mjob)[levels(seltd$Mjob) %in% 
                     c("at_home", "health", "services", "teacher")] <-
  "home_health_service_teacher"

levels(seltd$reason)[levels(seltd$reason) %in% 
                     c("course", "home", "other")] <-
  "course_home_other"

contrasts(seltd$guardian)

features<-features[features %in% c(
   "guardian"
  ,"reason"
) == FALSE]

leapft<-do_model(features)
leapft
extractmodel<-function(mft, modnum) {
  return(list(mft[[1]][[modnum]],
              mft[[4]][[modnum]]
  ))
}

calcr2jack<-function(pressval) {
  SSyy=(nrow(seltd)-1)*var(seltd$G3.portugese)
  return (1-(pressval/SSyy))
}

par(mfrow=c(2,2))
x<-extractmodel(leapft, 6)
x

x.selmod=x[[1]]
x.pressval=x[[2]]

x.selmod
# reg=lm(G3.portugese ~ sex_F + schoolsup_no + higher_no + studytime + 
#          Walc + failures.math + G1.math + failures.portugese, data=seltd)
# 
df6 = cbind(seltd, pred = predict(x.selmod))
seltd
as.integer(df6$school)-1

eq<-function() {
  z<-(
    0.4052 
    - 0.6745 * (as.integer(df6$school)-1)
    - 0.3593 * (as.integer(df6$Mjob)-1)
    - 0.1910 * df6$failures.math
    - 0.3610 * df6$failures.portugese
    + 0.1329 * df6$G1.portugese
    + 0.8833 * df6$G2.portugese
  )
  return(z)
}

eq()

ggplot(df6,aes(G1.portugese+G2.portugese+failures.math+failures.portugese,
               G3.portugese,
               colour=school,shape=Mjob))+
  geom_point()+geom_rug()+
  stat_function(fun = eq, col="green" )+
  # stat_function(fun = function(G1.math) -224.628+4391.953*G1.math + 4885.557*G1.math^2 )+
  ggtitle("Price as a function of G1.math, separated by school support and gender")

ols_coll_diag(x.selmod)

plot(x.selmod)

x.pressval

calcr2jack(x.pressval)

par(mfrow=c(1,1))
results<-predict(x.selmod,seltd)
data.frame(results,actual=seltd$G3.portugese) %>%
  ggplot(aes(x=results,y=actual)) + 
  geom_point()+
  stat_smooth(method="lm",show.legend = T)






