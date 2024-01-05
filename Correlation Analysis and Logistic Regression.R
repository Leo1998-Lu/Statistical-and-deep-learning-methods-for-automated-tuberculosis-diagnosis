set.seed(2249461)
library(tidyverse) # metapackage of all tidyverse packages
data = read_csv('CODA_TB_Clinical_Meta_Info.csv')
head(data)
dis_table = function(x,y){
  t1 = table(x,y)
  print(chisq.test(t1))
  t2 = prop.table(t1,2)
  tt = t(t(round(100*table(x)/length(x),1)))
  T1 = data.frame(cbind(
    rbind(paste0(t1[1],paste0('(',round(t2[1]*100,1),')')),
          paste0(t1[2],paste0('(',round(t2[2]*100,1),')'))),
    rbind(paste0(t1[3],paste0('(',round(t2[3]*100,1),')')),
          paste0(t1[4],paste0('(',round(t2[4]*100,1),')'))),
    t(t(paste0(t(t(table(x))),'(',t(t(round(100*table(x)/length(x),1))),')')))
  ))
  colnames(T1) = c('No','Yes','Total')
  rownames(T1) = unique(x)
  return(T1)
}

summary(data)
data = data.frame(data)
# 拟合logistic模型 
for (i in c("sex","night_sweats","smoke_lweek","hemoptysis","weight_loss","fever","tb_status")){
  print(i)
  data[, i] = factor(data[, i]) 
}

data$height[which(is.na(data$height))] = mean(na.omit(data$height))

wilcox.test(data$age~data$tb_status)
wilcox.test(data$height~data$tb_status)
wilcox.test(data$weight~data$tb_status)
wilcox.test(data$heart_rate~data$tb_status)
wilcox.test(data$temperature~data$tb_status)
wilcox.test(data$reported_cough_dur~data$tb_status)

t(data%>%summarise(age = paste0(median(age),'(',quantile(age,0.25),',',quantile(age,0.75),')'),
                   height = paste0(median(height),'(',quantile(height,0.25),',',quantile(height,0.75),')'),
                   weight = paste0(median(weight),'(',quantile(weight,0.25),',',quantile(weight,0.75),')'),
                   heart_rate = paste0(median(heart_rate),'(',quantile(heart_rate,0.25),',',quantile(heart_rate,0.75),')'),
                   temperature = paste0(median(temperature),'(',quantile(temperature,0.25),',',quantile(temperature,0.75),')'),
                   reported_cough_dur = paste0(median(reported_cough_dur),'(',quantile(reported_cough_dur,0.25),',',quantile(reported_cough_dur,0.75),')')))

t(data%>%group_by(tb_status)%>%summarise(age = paste0(median(age),'(',quantile(age,0.25),',',quantile(age,0.75),')'),
                                         height = paste0(median(height),'(',quantile(height,0.25),',',quantile(height,0.75),')'),
                                         weight = paste0(median(weight),'(',quantile(weight,0.25),',',quantile(weight,0.75),')'),
                                         heart_rate = paste0(median(heart_rate),'(',quantile(heart_rate,0.25),',',quantile(heart_rate,0.75),')'),
                                         temperature = paste0(median(temperature),'(',quantile(temperature,0.25),',',quantile(temperature,0.75),')'),
                                         reported_cough_dur = paste0(median(reported_cough_dur),'(',quantile(reported_cough_dur,0.25),',',quantile(reported_cough_dur,0.75),')')))

dis_table(data$sex,data$tb_status)
table(data$sex,data$tb_status)

dis_table(data$night_sweats,data$tb_status)
table(data$night_sweats,data$tb_status)

dis_table(data$smoke_lweek,data$tb_status)
table(data$smoke_lweek,data$tb_status)

dis_table(data$hemoptysis,data$tb_status)
table(data$hemoptysis,data$tb_status)

dis_table(data$weight_loss,data$tb_status)
table(data$weight_loss,data$tb_status)

dis_table(data$fever,data$tb_status)
table(data$fever,data$tb_status)



## barplot
p1 = ggplot(data,aes(factor(tb_status),fill=sex))+geom_bar()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Number of subjects')+xlab('Tuberculosis status (0: negative, 1: positive)')+labs(fill = "Gender")

p2 = ggplot(data,aes(factor(tb_status),fill=night_sweats))+geom_bar()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Number of subjects')+xlab('Tuberculosis status (0: negative, 1: positive)')+labs(fill = "Night sweats")

p3 = ggplot(data,aes(factor(tb_status),fill=smoke_lweek))+geom_bar()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Number of subjects')+xlab('Tuberculosis status (0: negative, 1: positive)')+labs(fill = "Smoked last week")

p4 = ggplot(data,aes(factor(tb_status),fill=hemoptysis))+geom_bar()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Number of subjects')+xlab('Tuberculosis status (0: negative, 1: positive)')+labs(fill = "Hemoptysis")

p5 = ggplot(data,aes(factor(tb_status),fill=weight_loss))+geom_bar()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Number of subjects')+xlab('Tuberculosis status (0: negative, 1: positive)')+labs(fill = "Weight loss")

p6 = ggplot(data,aes(factor(tb_status),fill=fever))+geom_bar()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Number of subjects')+xlab('Tuberculosis status (0: negative, 1: positive)')+labs(fill = "Fever")

cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow = 3)
ggsave('barplot.png',dpi=600,height=24,width=18,unit='cm')

## boxplot
p1 = ggplot(data,aes(factor(tb_status),age,fill=factor(tb_status)))+geom_boxplot()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Age')+xlab('Tuberculosis status (0: negative, 1: positive)')+theme(legend.position = "none")

p2 = ggplot(data,aes(factor(tb_status),height,fill=factor(tb_status)))+geom_boxplot()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Height')+xlab('Tuberculosis status (0: negative, 1: positive)')+theme(legend.position = "none")

p3 = ggplot(data,aes(factor(tb_status),weight, fill=factor(tb_status)))+geom_boxplot()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Weight')+xlab('Tuberculosis status (0: negative, 1: positive)')+theme(legend.position = "none")

p4 = ggplot(data,aes(factor(tb_status),reported_cough_dur, fill=factor(tb_status)))+geom_boxplot()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Duration of cough')+xlab('Tuberculosis status (0: negative, 1: positive)')+theme(legend.position = "none")

p5 = ggplot(data,aes(factor(tb_status),heart_rate, fill=factor(tb_status)))+geom_boxplot()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Heart rate')+xlab('Tuberculosis status (0: negative, 1: positive)')+theme(legend.position = "none")

p6 = ggplot(data,aes(factor(tb_status),temperature, fill=factor(tb_status)))+geom_boxplot()+theme_bw()+theme(legend.position = 'bottom')+
  ylab('Temperature')+xlab('Tuberculosis status (0: negative, 1: positive)')+theme(legend.position = "none")

cowplot::plot_grid(p1,p2,p3,p4,p5,p6, nrow = 3)
ggsave('boxplot.png',dpi=600,height=24,width=18,unit='cm')

model.1 = glm(tb_status~age+height+weight+heart_rate+temperature+reported_cough_dur+sex+night_sweats+smoke_lweek+hemoptysis+weight_loss+fever,family = "binomial",data = data)
summary(model.1)


# Relative Risk
calcRelativeRisk <- function(mymatrix,alpha=0.05,referencerow=2)
{
  numrow <- nrow(mymatrix)
  myrownames <- rownames(mymatrix)
  for (i in 1:numrow)
  {
    rowname <- myrownames[i]
    DiseaseUnexposed <- mymatrix[referencerow,2]
    ControlUnexposed <- mymatrix[referencerow,1]
    if (i != referencerow)
    {
      DiseaseExposed <- mymatrix[i,2]
      ControlExposed <- mymatrix[i,1]
      totExposed <- DiseaseExposed + ControlExposed
      totUnexposed <- DiseaseUnexposed + ControlUnexposed
      probDiseaseGivenExposed <- DiseaseExposed/totExposed
      probDiseaseGivenUnexposed <- DiseaseUnexposed/totUnexposed
      
      # calculate the relative risk
      relativeRisk <- probDiseaseGivenExposed/probDiseaseGivenUnexposed
      print(paste("category =", rowname, ", relative risk = ",relativeRisk))
      
      # calculate a confidence interval
      confidenceLevel <- (1 - alpha)*100
      sigma <- sqrt((1/DiseaseExposed) - (1/totExposed) +
                      (1/DiseaseUnexposed) - (1/totUnexposed))
      # sigma is the standard error of estimate of log of relative risk
      z <- qnorm(1-(alpha/2))
      lowervalue <- relativeRisk * exp(-z * sigma)
      uppervalue <- relativeRisk * exp( z * sigma)
      print(paste("category =", rowname, ", ", confidenceLevel,
                  "% confidence interval = [",lowervalue,",",uppervalue,"]"))
    }
  }
}
# calculate the relative risk

#calcRelativeRisk(table(x=pop$female,y=pop$y),referencerow=1)

library(survey)
library(caret)
imp_model.1 = varImp(model.1)


formatFit<-function(fit){
  #p-value
  p<-summary(fit)$coefficients[,4]
  #wald
  wald<-summary(fit)$coefficients[,3]^2
  
  #B
  valueB<-coef(fit)
  #OR
  valueOR<-exp(coef(fit))
  #OR 95%CI
  confitOR<-exp(confint(fit))
  data.frame(
    B=round(valueB,2),
    Wald=round(wald,2),
    OR_with_CI=paste(round(valueOR,2),"(",
                     round(confitOR[,1],2),"~",round(confitOR[,2],2),")",sep=""),
    P=format.pval(p,digits = 3,eps=0.001)
  )
}
formatFit(model.1)


audio_df = read.csv('audio_tabular.csv')
audio_df = audio_df%>%select(-participant,-tb_prior,-tb_prior_Pul,-tb_prior_Extrapul,-tb_prior_Unknown,-age,-height,-weight,-heart_rate,-temperature,-reported_cough_dur,-sex,-night_sweats,-smoke_lweek,-hemoptysis,-weight_loss,-fever)
dim(audio_df)



tsne_df = read.csv('tsne_result.csv')
names(tsne_df) = c('t-SNE audio feature 1','t-SNE audio feature 2','t-SNE audio feature 3','label')
tsne_df = cbind(data,tsne_df[,c(1:3)])
names(tsne_df)
model.2 = glm(tb_status~age+height+weight+heart_rate+temperature+reported_cough_dur+
                sex+night_sweats+smoke_lweek+hemoptysis+weight_loss+fever+
                `t-SNE audio feature 1`+`t-SNE audio feature 2`+`t-SNE audio feature 3`,family = "binomial",data = tsne_df)
summary(model.2)
formatFit(model.2)
imp_model.2 = varImp(model.2)


#ROC
library(ggthemes)
library(pROC)
library(ggsci)
cal_roc = function(pred,model_name){
  roc_curve <- roc(data$tb_status,pred)
  names(roc_curve)
  x <- 1-roc_curve$specificities
  y <- roc_curve$sensitivities
  
  p <- ggplot(data = NULL, mapping = aes(x= x, y = y))
  p + geom_line(col=2) +geom_abline(intercept = 0, slope = 1)+
    annotate('text', x = 0.4, y = 0.5, label =paste('AUC=',round(roc_curve$auc,4)))+
    labs(x = '1-specificities',y = 'sensitivities', title = paste('ROC Curve:',model_name))+
    theme_bw()+scale_color_lancet()
}
cal_roc(model.1$fitted.values,'Model 1')
cal_roc(model.2$fitted.values,'Model 2')


# write.csv(data.frame(label = data$tb_status,Model1 = model.1$fitted.values,Model2 = model.2$fitted.values,
#            Model3 = model.3$fitted.values,Model4 = model.4$fitted.values),'roc_comp.csv',row.names=F)

# Correlation Analysis
tsne_df
dim(audio_df)
names(audio_df)



audio_df['chroma_stft_mean'] = apply(audio_df[,6:17],1,mean)
audio_df['mfcc_mean'] = apply(audio_df[,18:37],1,mean)
audio_df['mel_mean'] = apply(audio_df[,39:166],1,mean)

audio_df
corr_df = audio_df%>%select(zcr_mean,centroid_mean,f0_mean,rms_mean,chroma_stft_mean,mfcc_mean,mel_mean)
tsne_df = read.csv('tsne_result.csv')
corr_df = cbind(corr_df,tsne_df)
names(corr_df)=c("ZCR","Centroid","F0","Energy","Chroma Vector",
                 "MFCCs","Mel-spectrogram",
                 "t-SNE 1","t-SNE 2","t-SNE 3","Status" )


library(tidyverse)
library(ggthemes)
library(magrittr)
library(linkET)

## mantel test ##
mantel_test(corr_df %>% select(1,2,3,4,5,6,7),corr_df %>% select(8,9,10),
            spec_select = list("t-SNE 1" = 1:7,
                               "t-SNE 2" = 1:7,
                               "t-SNE 3" = 1:7))

# mantel_test分析，其他领域可替换为含有p值和(或)r值和(或)其他值的数据
mantel <- mantel_test(corr_df %>% select(8,9,10),corr_df %>% select(1,2,3,4,5,6,7),
                      spec_select = list("t-SNE 1" = 1,
                                         "t-SNE 2" = 2,
                                         "t-SNE 3" = 3)) %>% 
  mutate(rd = cut(abs(r), breaks = c(-Inf, 0.1, 0.3, Inf),
                  labels = c("<0.1", "0.1-0.3", ">0.3")),
         pd = cut(p, breaks = c(-Inf, 0.001, 0.01, 0.05, Inf), #设定范围将连续变量变为分类变量
                  labels = c("< 0.001", "0.001 - 0.01", "0.01 - 0.05", ">= 0.05")))


mantel



# 画图
qcorrplot(correlate(corr_df %>% select(1,2,3,4,5,6,7)), type = "upper", diag = FALSE) + #设置左下角热图，不显示对角线
  geom_square() +
  geom_couple(aes(colour = pd, size = rd), #网络图
              data = mantel,
              curvature = nice_curvature(),
              alpha = 0.9) + #连接线弧度设置
  
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11, "RdBu"), #设置颜色
                       limits = c(-1, 1), #设置范围
                       breaks = seq(-1, 1, by = 0.2)) + #设置刻度间隔及范围
  scale_size_manual(values = c("<0.1" = 0.25,
                               "0.1-0.3" = 1.7,
                               ">0.3" = 3)) + #单独设置分类数据的线条粗细
  scale_colour_manual(values = c("< 0.001" = "#45943a",
                                 "0.001 - 0.01" = "#625b86",
                                 "0.01 - 0.05" = "#b6b427",
                                 ">= 0.05" = "#d6d3d2")) + #单独设置不同分类的线条颜色
  guides(size = guide_legend(title = "Mantel's r", #图例标题
                             override.aes = list(colour = "grey35"),  #图例项的颜色
                             order = 2), #图例顺序
         colour = guide_legend(title = "Mantel's p", 
                               override.aes = list(size = 3),  #设置图例项中标记大小
                               order = 1),#图例顺序
         fill = guide_colorbar(title = "Pearson's r", order = 3))+ #图例标题和顺序
  coord_equal()+
  theme(plot.margin = unit(c(0,0,0,0),units="cm"),
        legend.background = element_blank(),
        legend.key = element_blank())
ggsave('mantel.png',dpi=600,width = 24,height = 14,unit='cm')

## correlation - groups ##
ggpairs(corr_df,mapping=aes(col=Status))+theme_bw()+scale_color_lancet()+scale_fill_lancet()
ggsave('cor_plot.png',dpi=600,width = 42,height = 32,unit='cm')

head(corr_df)
data = cbind(data,corr_df%>%select(-Status))

model.3 = glm(tb_status~age+height+weight+heart_rate+temperature+reported_cough_dur+
                sex+night_sweats+smoke_lweek+hemoptysis+weight_loss+fever+
                ZCR+Centroid+F0+Energy+`Chroma Vector`+MFCCs+`Mel-spectrogram`+ `t-SNE 1` + `t-SNE 2` + `t-SNE 3`,family = "binomial",data = data)

summary(model.3)
formatFit(model.3)
imp_model.3 = varImp(model.3)



model.4 = glm(tb_status~ZCR+Centroid+F0+Energy+`Chroma Vector`+MFCCs+`Mel-spectrogram`,family = "binomial",data = data)


summary(model.4)
formatFit(model.4)
imp_model.4 = varImp(model.4)

imp_model.4

input_x = data%>%select(age,height,weight,heart_rate,temperature,reported_cough_dur,
  sex,night_sweats,smoke_lweek,hemoptysis,weight_loss,fever,
  ZCR,Centroid,F0,Energy,`Chroma Vector`,MFCCs,`Mel-spectrogram`, `t-SNE 1` , `t-SNE 2` , `t-SNE 3`)
input_y = factor(data$tb_status)

roc_imp <- filterVarImp(input_x,input_y)
roc_imp$Variables = rownames(roc_imp)
roc_imp$Variables[1:12] = c('Age','Height','Weight','Heart Rate','Temperature',
                            'Cough Duration','Gender','Night Sweats','Smoke Last Week',
                            'Hemoptysis','Weight Loss','Fever' )
roc_imp$AUC = roc_imp$X0
roc_imp$Group = NA
roc_imp$Group[1:12] = 'Tabular Data'
roc_imp$Group[12:nrow(roc_imp)] = 'Audio Data'

write.csv(roc_imp%>%select(Variables,AUC,Group),'roc_imp.csv',row.names = F)

cal_roc(model.3$fitted.values,'Model 3')
cal_roc(model.4$fitted.values,'Model 4')

# library(rms)
# library(survminer)
# fit.1 <- lrm(tb_status~age+height+weight+heart_rate+temperature+reported_cough_dur+sex+night_sweats+smoke_lweek+hemoptysis+weight_loss+fever, data = data)
# fit.1
# 
# ddist <- datadist(data)
# options(datadist = 'ddist')
# nom = nomogram(fit.1,fun = plogis, funlabel = "Risk")
# plot(nom)


# fea_tab = read.table('clipboard',sep='\t',header=T)
# fea_tab
# fea_tab$deviation = 100*(fea_tab$AUROC - 0.862)
# fea_tab$group = ifelse(fea_tab$deviation>0,'decrease','increase')
# fea_tab
# 
# library(ggpubr)
# ggbarplot(fea_tab, x="Excluded.Feature", y="deviation", fill = "group", color = "white",
#           palette = c("#8A2BE2","#FF4500"), 
#           sort.val = "desc", sort.by.groups = FALSE, 
#           x.text.angle=90,
#           xlab = 'Excluded Feature', ylab = 'Deviation of AUROC (%)', 
#           legend.title="Group",position = position_dodge(0.9), rotate=TRUE, ggtheme = theme_minimal())+guides(fill=FALSE)
# ggsave('dev plot.png',dpi=600,height=10,width=10,unit='cm')
# 
# fea_tab = read.table('clipboard',sep='\t',header=T)
# 
# for (i in 1:nrow(fea_tab)){
#   set.seed(2249461)
#   result <- unlist(strsplit(fea_tab$AUROC[i], "±"))
#   p = t.test(x = rnorm(5*100, as.numeric(result[1]), as.numeric(result[2])), y = rnorm(5*100, 0.862, 0.034), var.equal = TRUE)$p.value
#   print(round(p,2))
# }
