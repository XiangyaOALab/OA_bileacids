library(readr)
library(survival)
library(ggplot2)
library(survminer)
library(ggthemes)
library(gridExtra)
library(haven)

kmcurve<-read_sas("./cohort_ps3_update.sas7bdat")

fitcv <- survfit(Surv(followy_tkr2, tkr_oa) ~ treat, data = kmcurve)


ggsurvplot(fitcv, data = kmcurve,
           
           
           conf.int = T,
           censor= FALSE,
           
           
           palette =c("#9FC9DF","#EFD496"),
           pval = FALSE,
           
           
           fun = "event",
           
           risk.table = TRUE,
           risk.table.title="No. at risk",
           
           tables.y.text.col= FALSE,
           
           tables.height = 0.3, 
           
           
           size = 1.1,
           
           linetype = c("solid","solid"),
           
           axes.offset=FALSE,
           
           legend.text = TRUE,
           
           
           
           
           legend.title = "",
           
           legend.labs = c("Non-users","UDCA new users"),
           risk.table.fontsize= 4.75,
           font.legend=13,
           font.tickslab=13,
           font.x=14,
           font.y=14,
           
           
           
           break.time.by=10/5,
           
           xlim = c(0, 10.2),
           ylim = c(0, 0.03),
           
           xlab = "Years of Follow-up",
           ylab = "Cumulative Incidence of knee replacement",
           ggtheme = theme(axis.line = element_line(colour = "black",color = "black"),
                           plot.margin = margin(t=80,r=30,b=1,l=10,"pt"),
                           
                           panel.background = element_rect(fill = "white", colour = "white",color = "white")),
           tables.theme = theme(axis.text.y = element_text(size = rel(1.5), color="black"),
                                legend.title = element_text(size = 20),
                                legend.text = element_text(size = 20),
                                text = element_text(hjust = 0),
                                panel.background = element_rect(colour = "white",color = "white"),
                                axis.title = element_text(colour = "white",color = "white"),
                                axis.ticks = element_line(colour = "white",color = "white"),
                                axis.line = element_line(colour = "white",color = "white"),
                                axis.text.x = element_text(colour = "white",color = "white"),
                                panel.grid.major.y = element_line(colour = "white",color = "white")))

