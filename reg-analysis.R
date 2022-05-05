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
                 ggplotify,
                 sjPlot,
                 lme4,
                 TMB,
                 glmmTMB,
                 effects,
                 stats,
                 AICcmodavg,
                 lessR,
                 cran_repo = 'https://cran.r-project.org')

#renv::clean()
setwd(dirname(getSourceEditorContext()$path))

student.data=read.table("student-data.csv", sep=",", header=T)

names(student.data)
d=student.data
str(d)

drop.known=TRUE

# encode categoricals ------
fac.df <- as.data.frame(unclass(d), stringsAsFactors = TRUE)
if (drop.known == TRUE) {
  fac.df<-fac.df[,-c(31,32,37,38)]
}

names(fac.df)
dim(student.data)

# test normality ---------
plot(Hmisc::describe(fac.df)) 

num.only.df= fac.df %>% select(where(is.numeric))
# dff=fac.df %>% select(where(is.factor))
# dff<-lapply(dff, as.integer)
# dff<-lapply(dff, function(x) x-1)
# xx<-cbind(num.only.df, dff)
# num.only.df<-xx[c(1:16,18:35,17)]

norm.df<-d %>% select(where(is.numeric))
data.table(Column=names(norm.df),
           P=apply(norm.df, 2,function(x) shapiro.test(x)$p.value)
)

# some EDA dataset graphs --------------

plotpervar<-function(featrs) {
  ln=length(featrs)
  plots.elems<-list()
  for(i in seq_along(featrs[-ln])) {
    plots.elems[[i]]<-
      ggplot(num.only.df,aes_string(featrs[c(ln)],featrs[c(i)],colour="school"))+
      geom_point()+geom_rug()
  }
  
  return (plots.elems)
}

intersect(names(eda.df), features)
plot_grid(plotlist=plotpervar(names(num.only.df)))

# normal distribution --------

h2<-ggplot(d, aes(x = G3.portugese)) + 
  geom_histogram(aes(y = ..density.., fill=..count..), bins=20) +
  scale_fill_gradient("Legend",low = "green", high = "blue")+
  geom_density(color="red") 
student<-d
h1<-as.grob(expression(hist(student$G3.portugese)))
grid.newpage()
grid.draw(h2)
vp = viewport(x=.3, y=.75, width=.35, height=.3)
pushViewport(vp)
grid.draw(h1)
upViewport()


# cor matrix --------
getcorrmtx<-function(.df, threshold) {
  corMtrx <- cor(.df)
  print(corMtrx)
  highcorr=findCorrelation(corMtrx, names=F, exact=F, cutoff=threshold)
  return(names(.df[,highcorr]))
}

getcorrmtx(num.only.df, .5)

names(fac.df)


# EDA DataSet-------------
p1<-ggplot(fac.df, aes(Dalc, fill = sex)) +
  geom_histogram(position = "dodge") +
  ggtitle("Students weekday alcohol consumption")

p2<-ggplot(fac.df, aes(G3.portugese, fill = sex)) +
  geom_histogram(position = "dodge") +
  ggtitle("Students Portuguese Final Grades")

cowplot::plot_grid(p1, p2)




fdf<-fac.df

gg=fdf %>%
mutate(binCounts = cut(G3.portugese, breaks = seq(0, 100, by = 5))) %>%
  group_by(binCounts) %>%
  mutate(sumVal = sum(G3.portugese)) %>%
  ungroup() %>%
  group_by(binCounts, G3.portugese) %>%
  summarise(prct = sum(G3.portugese)/mean(sumVal))

gg=fdf %>%
  mutate(binCounts = cut(G3.portugese, breaks = seq(0, 100, by = 5))) %>%
  group_by(binCounts) %>%
  mutate(sumVal = mean(G3.portugese)) %>%
  ungroup() %>%
  group_by(binCounts, school, sex) %>%
  summarise(student_percent = sum(G3.portugese)/mean(sumVal))

ggplot(gg) +
  geom_bar(aes(x=binCounts, y=student_percent, fill=sex), stat="identity") +
  theme(axis.text.x=element_text(angle = 90, hjust=1))


gg=fdf %>%
  mutate(binCounts = cut(G3.portugese, breaks = seq(0, 100, by = 5))) %>%
  group_by(binCounts) %>%
  mutate(sumVal = mean(G3.portugese)) %>%
  ungroup() %>%
  group_by(binCounts, sex, Dalc, Walc) %>%
  summarise(student_percent = sum(G3.portugese)/mean(sumVal))

gg<-fdf %>% group_by(school, sex) %>% summarise(ct=n()) %>% ungroup()

gg

ggplot(subset(fac.df),aes(x=Dalc,y=G3.portugese,colour=sex))+
  geom_point()+geom_rug()+ geom_abline(intercept = 147, slope = 2, color="blue",
                                       linetype="dotted", size=1)+
  ggtitle("Percent of Poverty and Mortality Rate by Region, separated by rank")

gg<-fdf %>% group_by(Dalc, sex) %>% summarise(ct=n()) %>% ungroup()


gg<-fdf %>% mutate(binCounts = cut(age, breaks = seq(0, 100, by = 5))) %>%
  group_by(binCounts) %>%
  mutate(avgAge = mean(age)) %>%
  ungroup() %>%
  group_by(binCounts) %>%
  summarize(mean_age=mean(avgAge))
  
pie(gg$mean_age, gg$binCounts, main="Age classification of students")

pi1<-PieChart(mean_age, hole=0, values="%", data=gg, fill=1:4, main="Age classification of students")


gg<-fdf %>% mutate(binCounts = cut(studytime, breaks = seq(0, 100, by = 5))) %>%
  group_by(binCounts) %>%
  mutate(avgStudyTime = mean(studytime)) %>%
  ungroup() %>%
  group_by(binCounts) %>%
  summarize(mean_st=mean(avgStudyTime))



pi2<-PieChart(studytime, hole=0, values="%", data=fdf, fill=1:4, main="StudyTime of students")

par(mfcol=c(2,1))
PieChart(age, hole=0, values="%", data=fdf, fill=1:4, main="Age of students")
PieChart(studytime, data=fdf)

cowplot::plot_grid(pi1, pi2)

gg

ggplot(fac.df, aes(schoolsup, fill = sex)) +
  geom_histogram(position = "dodge") +
  ggtitle("Students weekday alcohol consumption")

+
  geom_point()+geom_rug()
  
ggplot(gg) +
  geom_bar(aes(x=school, y=ct, fill=sex), stat="identity") +
  theme(axis.text.x=element_text(angle = 90, hjust=1))

geom_point()+geom_rug()+
  
names(fdf)
ggplot(data=fdf,aes(x=age,y=Dalc, col=sex))+ 
  stat_smooth(method=lm)+ 
  geom_point(shape=21)+
  # annotate(geom='text',x=40,y=125000,size=3.5,label='y = 985.3x + 91718.7 (p < 0.001)',family='Times')+
  ggtitle("Straight line fit expressing salary as a function of experience")


print(plot)

plot(fdf$age, fdf$Dalc, col=fdf$sex)


# num.only.df<-d%>% select(where(is.numeric))
corr = round(cor(num.only.df), 2)
p.mat <- cor_pmat(num.only.df)
ggcorrplot(corr,
           colors = c("white", "white", "red"),
           type = c("lower"),
           insig = c("pch", "blank"), pch = 1, pch.col = "gray", pch.cex =1,
           hc.order=F,
           hc.method = "median",
           sig.level=.1,
           tl.srt = 90,
           tl.cex=9,
           outline.color = "white",
           ggtheme=ggplot2::theme_dark(),
           p.mat = p.mat,
           lab=TRUE,
           digits=1,
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
plot(subset.model, scale="r2")
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

#reshuffle seltd cols as per corrmtx------
x<-seltd%>%select_if(is.numeric)
y<-seltd%>%select_if(is.factor)
y<-sapply(y, as.integer)-1
reshuf.features<-getcorrmtx(cbind(x,y), .01)
reshuf.features

nm=setdiff(names(seltd), reshuf.features)
nm
rb.seltd<-cbind(seltd[,reshuf.features], absences.math=seltd[,nm])
names(rb.seltd)

rb.seltd<-cbind(rb.seltd[, !names(rb.seltd) %in% c("G3.portugese")],
      G3.portugese=rb.seltd[, names(rb.seltd) %in% c("G3.portugese")])
names(rb.seltd)
seltd<-rb.seltd

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
  p1<-ggplot(data=eda.df,aes(x=school,y=G3.portugese,fill=sex))+
    geom_boxplot(notch=TRUE)+
    facet_grid(. ~ school + higher)+
    ggtitle("Boxplots showing grade distribution, separated by school and higher edu aspiration")
  
  # violinplots
  p2<-ggplot(data=eda.df,aes(x=sex,y=G3.portugese,fill=sex))+
    geom_violin()+
    geom_boxplot(fill='darkred',width=0.1,notch=TRUE)+
    geom_point(position='jitter',size=1)+
    facet_grid(. ~ school + higher)+
    ggtitle("Violinplots showing grade distribution, separated by school and higher edu aspiration")
  
  plot_grid(p1, p2, nrow=2)
  
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

  ln=length(names(eda.df))
  eda.df<-seltd
  intersect(names(eda.df), features)
  plot_grid(plotlist=plotpervar(intersect(names(eda.df), features)))
  
  eda.df<-cbind(G3.portugese=eda.df[,ln], eda.df[,-c(1:4,ln)], eda.df[,c(1:4)])
  ggpairs(eda.df,mapping=ggplot2::aes(colour = sex))

  names(eda.df)
  ggplot(eda.df,aes_string("G3.portugese","G3.math",colour="sex"))+
    geom_point()+geom_rug()
  
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
    # "age"
  ) == FALSE]
  
}

names(seltd)
leapft<-do_model(names(seltd))
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
  x<-extractmodel(leapft, 8)
}
x

x.selmod=x[[1]]
x.pressval=x[[2]]

summary(x.selmod)
anova(x.selmod)
AIC(x.selmod)

mod.names <- c("")

leapft[[1]]
aictab(cand.set=leapft[[1]])

# reg=lm(G3.portugese ~ sex_F + schoolsup_no + higher_no + studytime + 
#          Walc + failures.math + G1.math + failures.portugese, data=seltd)
# 
df6 = cbind(seltd, pred = predict(x.selmod))

# df6$school<-as.integer(df6$school)-1
# df6$sex<-as.integer(df6$sex)-1
df6$higher<-as.integer(df6$higher)-1

df6$Dalc<-as.integer(df6$Dalc)-1
names(df6)

-0.32301 + 0.31273 * G3.math 
- 1.44057 * failures.portugese 
- 2.11560 * school - 1.10659 * sex + 1.91890 * higher + 0.58411 * age - 0.46871 * Dalc - 0.05219 * absences.math

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
  eq<-function(.df) {
      -0.32301 
      +0.31273 * df6$G3.math
      -1.44057 * df6$failures.portugese
      -2.11560 * (as.integer(df6$school)-1)
      -1.10659 * (as.integer(df6$sex)-1)
      +1.91890 * (as.integer(df6$higher)-1)
      +0.58411 * df6$age
      -0.46871 * (as.integer(df6$Dalc)-1)
      -0.05219 * df6$absences.math
  }

  eq()
  
  ggplot(df6, aes(-0.32301 
                  +0.31273 * G3.math
                  -1.44057 * failures.portugese 
                  -2.11560 * (as.integer(school)-1)
                  -1.10659 * (as.integer(sex)-1)
                  +1.91890 * higher
                  +0.58411 * age
                  -0.46871 * Dalc
                  -0.05219 * absences.math
                  ,
                 G3.portugese,
                 colour=sex,shape=school))+
    geom_point()+geom_rug()+
    stat_function(fun = function(.x) {
      1 * .x
    },
    colour="green" )+
    # stat_function(fun = function(G1.math) -224.628+4391.953*G1.math + 4885.557*G1.math^2 )+
    ggtitle("G3 Portuguese grade best line fit")

}

ols_coll_diag(x.selmod)


xp<-plot_model(x.selmod, type = "eff")

plot_grid(xp)

# lm<-lmer(G3.portugese ~ Dalc + (Dalc | school), 
#         seltd)
# 
# plot_model(x.selmod, type = "re")
# 

plot_model(x.selmod)
p <- plot_model(x.selmod, type = "diag")
plot_grid(p)


xxp<-plot_model(x.selmod, type = "pred", terms = c("Dalc",
                                                   "sex",
                                                   "school",
                                                   "higher"))

plot_grid(xxp)


x.pressval

calcr2jack(x.pressval)

par(mfrow=c(1,1))
results<-predict(x.selmod,seltd)
data.frame(results,actual=seltd$G3.portugese) %>%
  ggplot(aes(x=results,y=actual)) + 
  geom_point()+
  stat_smooth(method="lm",show.legend = T)







