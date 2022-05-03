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

drop.known=TRUE

# encode categoricals ------
fac.df <- as.data.frame(unclass(d), stringsAsFactors = TRUE)
if (drop.known == TRUE) {
  fac.df<-fac.df[,-c(31,32,37,38)]
}
names(fac.df)

# test normality ---------
plot(Hmisc::describe(fac.df)) 

num.only.df= fac.df %>% select(where(is.numeric))
data.table(Column=names(num.only.df),
           P=apply(num.only.df, 2,function(x) shapiro.test(x)$p.value)
)

# cor matrix --------
corMtrx <- cor(num.only.df)
print(corMtrx)
highcorr=findCorrelation(corMtrx, names=F, exact=F, cutoff=0.5)
highcorr

names(num.only.df[,highcorr])

# dims=length(colnames(corMtrx))
# corrDF <- expand.grid(row = 1:dims, col = 1:dims)
# corrDF$correlation <- as.vector(corMtrx)
# levelplot(correlation ~ row + col, corrDF)

corr = round(cor(num.only.df), 2)
p.mat <- cor_pmat(num.only.df)
ggcorrplot(corr,
           colors = c("white", "white", "red"),
           type = c("lower"),
           insig = c("pch", "blank"), pch = 10, pch.col = "gray", pch.cex =1,
           hc.order=F,
           hc.method = "median",
           sig.level=.01,
           tl.srt = 90,
           tl.cex=9,
           outline.color = "white",
           ggtheme=ggplot2::theme_dark(),
           p.mat = p.mat,
           title="Correlation heatmap among student variables")  
# +
#   theme(axis.text.x = element_text(margin=margin(0,0,-5,-5)),  # Order: top, right, bottom, left
#         axis.text.y = element_text(margin=margin(0,0,5,5)),
#         panel.grid.major=element_blank()
#         )

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
plot(subset.model, scale="bic")
plot(subset.model, scale="Cp")
# 

reg=lm(G3.portugese ~ ., data=fac.df)
summary(reg)

Hmisc::describe(fac.df)

if (drop.known == FALSE) {
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
} else {
  leap.features=c(
    "school"
    ,"sex"
    ,"age"
    ,"higher"
    ,"Dalc"
    ,"absences.math"
    ,"G3.math"
    ,"failures.portugese"
  )
}
leap.features

features<-append(leap.features, "G3.portugese")

seltd<-fac.df[,features]

seltd<-as.data.frame(seltd)

features

# EDA--------------
eda.df<-seltd
names(eda.df)

if (drop.known == FALSE) {
  # rank distribution
  ggplot(data=eda.df,aes(x=G3.portugese,fill=school))+
    geom_bar(position='dodge')+
    facet_grid(. ~ Mjob)+
    ggtitle("Bar-plot showing rank distribution, separated by gender and discipline")
  
  #histogram
  ggplot(data=eda.df,aes(x=G3.portugese,fill=school))+
    geom_density(alpha=0.4)+
    facet_grid(. ~ Mjob)+
    ggtitle("Density curves showing salary distribution, separated by gender and discipline")
  
  # boxplots
  ggplot(data=eda.df,aes(x=school,y=G3.portugese,fill=school))+
    geom_boxplot(notch=TRUE)+
    facet_grid(. ~ Mjob + reason)+
    ggtitle("Notched boxplots showing deathRate distribution, separated by region and discipline")
  
  # violinplots
  ggplot(data=eda.df,aes(x=school,y=G3.portugese,fill=school))+
    geom_violin()+
    geom_boxplot(fill='darkred',width=0.1,notch=TRUE)+
    geom_point(position='jitter',size=1)+
    facet_grid(. ~ Mjob + reason)+
    ggtitle("Notched violinplots showing salary distribution, separated by gender and discipline")
  
  plotpervar<-function(featrs) {
    ln=length(featrs)
    plots.elems<-list()
    for(i in seq_along(featrs[-ln])) {
      plots.elems[[i]]<-
        ggplot(eda.df,aes_string(featrs[c(ln)],featrs[c(i)],colour="school"))+
        geom_point()+geom_rug()
    }
    
    return (plots.elems)
  }
  
  intersect(names(eda.df), features)
  plot_grid(plotlist=plotpervar(intersect(names(eda.df), features)))
  
  ln=length(names(eda.df))
  eda.df<-seltd
  eda.df<-cbind(G3.portugese=eda.df[,ln], eda.df[,-c(1:4,ln)], eda.df[,c(1:4)])
  ggpairs(eda.df,mapping=ggplot2::aes(colour = school))
  
} else {
  
  # rank distribution
  ggplot(data=eda.df,aes(x=G3.portugese,fill=school))+
    geom_bar(position='dodge')+
    facet_grid(. ~ sex)+
    ggtitle("Bar-plot showing grade distribution, separated by gender and higer edu aspiration")
  
  #histogram
  ggplot(data=eda.df,aes(x=G3.portugese,fill=school))+
    geom_density(alpha=0.4)+
    facet_grid(. ~ sex)+
    ggtitle("Density curves showing grade distribution, separated by gender and higer edu aspiration")
  
  # boxplots
  ggplot(data=eda.df,aes(x=school,y=G3.portugese,fill=sex))+
    geom_boxplot(notch=TRUE)+
    facet_grid(. ~ school + higher)+
    ggtitle("Notched boxplots showing grade distribution, separated by school and higher edu aspiration")
  
  # violinplots
  ggplot(data=eda.df,aes(x=sex,y=G3.portugese,fill=sex))+
    geom_violin()+
    geom_boxplot(fill='darkred',width=0.1,notch=TRUE)+
    geom_point(position='jitter',size=1)+
    facet_grid(. ~ school + higher)+
    ggtitle("Notched violinplots showing grade distribution, separated by school and higher edu aspiration")
  
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
  
  intersect(names(eda.df), features)
  plot_grid(plotlist=plotpervar(intersect(names(eda.df), features)))
  
  ln=length(names(eda.df))
  eda.df<-seltd
  eda.df<-cbind(G3.portugese=eda.df[,ln], eda.df[,-c(1:4,ln)], eda.df[,c(1:4)])
  ggpairs(eda.df,mapping=ggplot2::aes(colour = sex))
  
}

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

if (drop.known == FALSE) {
  
  levels(seltd$Mjob)[levels(seltd$Mjob) %in% 
                       c("at_home", "health", "services", "teacher")] <-
    "home_health_service_teacher"
  
  levels(seltd$reason)[levels(seltd$reason) %in% 
                         c("course", "home", "other")] <-
    "course_home_other"
  features<-features[features %in% c(
    "guardian"
    ,"reason"
  ) == FALSE]
  
} else {
  
  features<-features[features %in% c(
    "age"
  ) == FALSE]
  
}

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
if (drop.known == FALSE) {
  x<-extractmodel(leapft, 6)
} else {
  x<-extractmodel(leapft, 7)
}
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

if (drop.known == FALSE) {
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
  
} else {
  eq<-function() {
    z<-(
      9.84446 
      -1.34579 * (as.integer(df6$school)-1)
      -1.16412 * (as.integer(df6$sex)-1)
      +1.37463 * (as.integer(df6$higher)-1)
      -0.43644 *  (as.integer(df6$Dalc)-1)
      -0.04056 * df6$absences.math
      +0.29721 * df6$G3.math
      -1.18926 * df6$failures.portugese
    )
    return(z)
  }
  
  eq()
  
  ggplot(df6,aes(G3.math+absences.math+failures.portugese,
                 G3.portugese,
                 colour=school,shape=sex))+
    geom_point()+geom_rug()+
    stat_function(fun = eq, col="green" )+
    # stat_function(fun = function(G1.math) -224.628+4391.953*G1.math + 4885.557*G1.math^2 )+
    ggtitle("Price as a function of G1.math, separated by school support and gender")
  
}

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







