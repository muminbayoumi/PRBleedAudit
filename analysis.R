source("clean data.R")


library(tableone)
library(kableExtra)


(graph.1 <- data %>%
        group_by(NOAC) %>%
        summarise(count=n()) %>%
        mutate(pct=scales::percent(count/sum(count))) %>%
        ggplot(aes(NOAC,y=count)) +
        geom_bar(stat="identity",fill="maroon")+
        geom_label(aes(label=pct))+
        geom_label(aes(label=paste("n=",count)),nudge_y = -4,fill="#008B8B",color="white")+
        labs(x="Anticoagulation Status",
             y= "Number of Patients",
             subtitle="Relatively less patients on Warfarin at 6.9% of total audit sample,\nwhen compared to NOACs and Antiplateles\nTotal Number of  Patients = 159")+
        theme(axis.text.y =element_text(hjust = 0),
                      axis.ticks.length.y = unit(1,"mm"),
                      axis.title = element_text(face = "bold"))
        )

 (graph.2 <-
        data %>%
        ggplot(aes(AdmissionHb,x=Age))+
        geom_point() +
        geom_smooth(method = "lm")+
        facet_wrap(~NOAC)+
        labs(y="Admission Haemoglobin in g/dL",
        subtitle="Common Trend across all subgroups, initial Hb decreases as age increases.\nThere seemse to be a sharper trend with warfarin
however not enough patients to get confident interpretation ")+
                 theme(axis.text.y =element_text(hjust = 0),
                       axis.ticks.length.y = unit(1,"mm"),
                       axis.title = element_text(face = "bold"))
 )

(graph1 <-
                data %>%
                ggplot(aes(y=AdmissionHb,x=NOAC,fill=NOAC))+
                stat_summary(fun = median,fun.min = min,fun.max = max,geom = "crossbar",show.legend = F,alpha=.4)+
                geom_boxplot(show.legend = T)+
                geom_jitter(width=0.1,color="Red",alpha=0.5,shape=20,size=3,show.legend = F)+
                scale_fill_viridis_d(option = "cividis")+
                labs(subtitle="25% of patients with antiplatelets had Hb less than 80g/dL Approx. ",
                     caption = "Edges of bars represent max and min value while central black line represents median.
             A boxplot and jitter plot are both superimposed for those interested",
                     y="Haemoglobin on Admission g/dL",
                     x="Anticoagulation Status",
                     fill="Anticoagulation Status")+
                scale_y_continuous(breaks = c(50,55,60,65,70,75,80,90,100,110,120,140,160,180))+
                theme(axis.text.y =element_text(hjust = 0),
                      axis.ticks.length.y = unit(1,"mm"),
                      axis.title = element_text(face = "bold"),
                      axis.text.x = element_blank(),
                      legend.position = "bottom",
                      legend.text = element_text(size=13),
                      legend.title = element_blank())
)



(graph2 <- subset(data,data$NOAC=="NOAC"& Anticoagulant=="Rivaroxaban"| Anticoagulant=="Apixaban") %>%
                ggplot(aes(y=AdmissionHb,x=Anticoagulant))+
                stat_summary(fun = median,fun.min = min,fun.max = max,geom = "crossbar",)+
                geom_boxplot()+
                geom_jitter(width=0.1,color="Red",alpha=0.5,shape=20,size=3)+
                labs( subtitle = "3 patients on Dabigatran - one with Hb of 80, 2 with admission Hb of 120" ,
                      caption = "Edges of bars represent max and min value while central black line represents median.
             A boxplot and jitter plot are both superimposed for those interested",
                      y="Haemoglobin on Admission g/dL",
                      x="Specific NOAC")+
                scale_y_continuous(breaks = c(50,55,60,65,70,75,80,90,100,110,120,140,160,180))+
                theme(axis.text.y =element_text(hjust = 0),
                      axis.ticks.length.y = unit(1,"mm"),
                      axis.title = element_text(face = "bold"))
)




(graph3 <- data %>%
                ggplot(aes(fct_rev(fct_infreq(DiagnosisGrouped))))+
                geom_bar(fill="maroon")+coord_flip()+
                labs(y="Number of Patients",
                     x="Diagnosis")+
                theme(axis.text.y =element_text(hjust = 0),
                      axis.ticks.length.y = unit(4,"mm"),
                      axis.title = element_text(face = "bold"))
)

wilnilinves <- wilcox.test(data$AdmissionHb~fct_collapse(data$Investigations1ry,Nil="Nil",other_level = "inves"))

graph4.1 <-
        data %>%
        ggplot(aes(x=fct_infreq(Investigations1ry)))+
        geom_bar(fill="maroon")+
        coord_flip()+
        stat_count(geom = "label",
                   aes(label = stat(count)),
                   fill="#008B8B",color='white')+
        stat_count(geom = "label",
                   aes(y=..count..-10,
                       label = scales::percent(stat(count)/sum(stat(count)))))+
        labs(y="Number of Patients",
             x= "Primary Investigation",
             subtitle="Within LGI Endoscopy group 8 had CT abdomen as well,
while 4 went on to have CT Colonscopy after being deemed \nnot fit for colonscopy or refusing it",
             caption = "Large number of of patients not investigated \nhowever no significant difference in mean Hb on admission")+
        theme(axis.text.y =element_text(hjust = 0),
              axis.ticks.length.y = unit(4,"mm"),
              axis.title = element_text(face = "bold"))


graph6 <- data %>%  ggplot(aes(LOS,x=NOAC,fill=NOAC))+
                              stat_summary(fun=median,fun.max = max,fun.min = min,geom="crossbar",show.legend = F,alpha=0.5)+
                              geom_boxplot(show.legend = F,color="maroon")+
                              scale_fill_viridis_d(option = "cividis")+
                              labs(x="",
                                   fill="Anticoagulation Status",
                                   y="Length of Stay in days",
                                   caption="Edges of bars represent max and min value while central black line represents median.
             A boxplot and jitter plot are both superimposed for those interested",
                                   subtitle="Patients on Antiplatelets seem to have a longer length of stay. \nDespite that the median all 3 groups is the same at 3
Median Length of stay in patients with no anticoagulants prescribed is 2 days")+
                              scale_y_continuous(breaks = c(1:10,20,30))+
                              theme(axis.text.y =element_text(hjust = 0),
                                    axis.ticks.length.y = unit(1,"mm"),
                                    axis.title = element_text(face = "bold"))




(graph7 <- data %>% filter(
        NOAC !="No Anticoagulant Prescribed"&
        IndicationGroup!="Not Indicated"&
        IndicationGroup!="No Indication in Records") %>%
        ggplot(aes(x=fct_inorder(IndicationGroup))) +
        geom_bar(aes(fill = fct_inorder(IndicationGroup))) +
        stat_count(
                geom = "label",
                aes(y = ..count.. + 1, label = stat(count)),
                size = 3,
                fill = "#008B8B",
                color = "white") +
        scale_fill_viridis_d(option = "viridis") +
        guides(color=F)+
        facet_wrap( ~ NOAC, scales = "free",ncol = 2,nrow=2) +
        theme(plot.subtitle = element_text(face = "bold"),
              legend.title = element_text(face = "bold"),
              legend.position = "bottom",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())+
        labs(subtitle = "This graph demonstrates number  of  patients with various indications for
the 3 groups of medications  used ",
             caption="Total of 6 patients with No Indications in Records- Bar Highlighted in Red",
                x ="",
                fill = "Indication") )

(graph8 <- data %>% filter(
         Antiplatelet!="Other") %>%
        ggplot(aes(x=fct_inorder(IndicationGroup))) +
        geom_bar(aes(fill = fct_inorder(IndicationGroup))) +
        stat_count(
                geom = "label",
                aes(y = ..count.. + 0.1, label = stat(count)),
                size = 3,
                fill = "#008B8B",
                color = "white") +
        scale_fill_viridis_d(option = "cividis") +
        guides(color=F)+
        facet_wrap( ~Antiplatelet, scales = "free") +
        theme(plot.subtitle = element_text(face = "bold"),
              legend.title = element_text(face = "bold"),
              legend.position = "bottom",
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())+
        labs(subtitle = "This graph demonstrates number  of  patients with various indications for
the 3 antiplatelet groups",
         x ="",
                fill = "Indication") )







(graph9 <- data %>% ggplot(aes(y=Age,x=NOAC,fill=NOAC))+
        scale_fill_viridis_d(option = "cividis")+
        geom_boxplot()+
        labs(x="",
             subtitle="Patients with no Anticoagulant prescribed covered a younger age range")+

        theme(legend.position = "bottom",
              legend.title = element_blank(),
              legend.text = element_text(size=13),
              axis.text.x = element_blank())
        )




# graph.1 %>% ggsave("Anticoag Status",plot = .,dpi = 450,device = "png")
# graph.2 %>% ggsave("Hb vs Age vs Anti",plot = .,dpi = 450,device = "png")
# graph1 %>% ggsave("Hb vs Anti",plot = .,dpi = 450,device = "png")
# graph2 %>% ggsave("Hb vs NOAC",plot = .,dpi = 450,device = "png")
# graph3 %>% ggsave("Diagnosis",plot = .,dpi = 450,device = "png")
# graph4.1 %>% ggsave("Inv",plot=.,dpi=450,device = "png")
# graph6 %>% ggsave("LOS",plot = .,dpi = 450,device = "png")
# graph7 %>% ggsave("antivs ind",plot = .,dpi = 450,device = "png")
# graph7 %>% ggsave("antivs indv2",plot = .,dpi = 450,device = "png")
# graph8 %>% ggsave("antiplat",plot = .,dpi = 450,device = "png")
# graph9 %>% ggsave("antivs indv2",plot = .,dpi = 450,device = "png")


table1<- CreateTableOne(data = data, vars = c("LOS","AdmissionHb"),strata = "Gender",factorVars = "Gender",addOverall = T ,argsNonNormal = c("LOS","AdmissionHb") )



