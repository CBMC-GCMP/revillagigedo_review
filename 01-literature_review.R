
# Load libraries ----------------------------------------------------------



library(tidyverse)
library(ggplot2)
library(ggthemes)


# Load data ---------------------------------------------------------------



#Database detailing Revillagigedo's thematic studies

db <- readxl::read_xlsx("data/literature_revilla.xlsx")|> 
  rename(grupo_estudio= `study group`) 
  
unique(db$grupo_estudio)



# Number of studies per discipline ----------------------------------------


db |> 
  filter(grupo_estudio!= "Mamiferos terrestre")|>
  mutate(grupo_estudio=ifelse(grupo_estudio=="Peces", "Peces_ambos", grupo_estudio),
          category=grupo_estudio,
         category= case_when(grupo_estudio=="Peces_oseos"~"Peces",
                             grupo_estudio=="Peces_cartilaginosos"~"Peces",
                             grupo_estudio=="Peces_ambos"~"Peces",
                             TRUE~grupo_estudio
                             ),
         category=factor(category),
    grupo_estudio=factor(grupo_estudio)) |> 
  group_by(category, grupo_estudio) |> 
  summarise(total=length(grupo_estudio)) |> 

  
   ggplot(aes(x=forcats::fct_reorder(category, total), y=total, fill=grupo_estudio)) +
    guides(fill = guide_legend(title = "Grupo o disciplina de estudio")) +
   geom_bar(stat="identity", position="stack")+
  labs(x="Tipo de Estudio", y="Total")+
  theme(axis.text.x = element_text(angle=0),
        axis.title =element_text(size=10, face="bold"))+
  # scale_fill_hue(labels = c("G1", "G2"))
  scale_y_continuous(breaks=seq(0,50, 5))+
   coord_flip()+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.title = element_text(size=15,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(face="bold"))

  

ggsave("figs/tipo_de_estudio_revilla.png", dpi = 300, height = 7, width = 15)




# Revillagigedo's specific assessments ------------------------------------

#ESTUDIOS ENFOCADOS DIRECTAMENTE EN REVILLAGIGEDO


db |> 
  filter(grupo_estudio!= "Mamiferos terrestre")|>
  filter(study=="directo") |> 
  mutate(grupo_estudio=ifelse(grupo_estudio=="Peces", "Peces_ambos", grupo_estudio),
         category=grupo_estudio,
         category= case_when(grupo_estudio=="Peces_oseos"~"Peces",
                             grupo_estudio=="Peces_cartilaginosos"~"Peces",
                             grupo_estudio=="Peces_ambos"~"Peces",
                             TRUE~grupo_estudio),
         category=factor(category),
         grupo_estudio=factor(grupo_estudio)) |> 
  group_by(category, grupo_estudio) |> 
  summarise(total=length(grupo_estudio)) |> 
  
  ggplot(aes(x=forcats::fct_reorder(category, total), y=total, fill=grupo_estudio)) +
  guides(fill = guide_legend(title = "Grupo o disciplina de estudio")) +
  geom_bar(stat="identity", position="stack")+
  labs(x="Tipo de Estudio", y="Total")+
  theme(axis.text.x = element_text(angle=0),
        axis.title =element_text(size=10, face="bold"))+
  # scale_fill_manual(values=c("#998639","#385c3a","#073561", "#5e214a"))+
# scale_fill_hue(labels = c("G1", "G2"))
scale_y_continuous(breaks=seq(0,50, 5))+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.title = element_text(size=15,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(face="bold"))



ggsave("figs/tipo_de_estudio_enfocadosenrevilla.png", dpi = 300, height = 7, width = 15)



# Study type graph --------------------------------------------------------



db %>% 
  filter(study!="NA") |> 
  
  filter(grupo_estudio!= "Mamiferos terrestre")|>
  mutate(grupo_estudio=ifelse(grupo_estudio=="Peces", "Peces_ambos", grupo_estudio),
         category=grupo_estudio,
         category= case_when(grupo_estudio=="Peces_oseos"~"Peces",
                             grupo_estudio=="Peces_cartilaginosos"~"Peces",
                             grupo_estudio=="Peces_ambos"~"Peces",
                             TRUE~grupo_estudio),
         category=factor(category),
         grupo_estudio=factor(grupo_estudio),
         study= factor(study, levels=c( "mención","comparación","directo"), 
                       labels=c("Mención", "Comparación","Directo"  ))) |>
  group_by(category, study, type, grupo_estudio) |> 
  #group_by(category, grupo_estudio) |> 
  summarise(total=length(grupo_estudio)) |> 
  
  ggplot(aes(x=forcats::fct_reorder(category, total), y=total, fill=study))+
  scale_y_continuous(breaks=seq(0, 40, 5))+
  coord_flip()+
  scale_fill_manual(values=c("#998639","#385c3a","#073561"))+
  theme_classic()+
  geom_bar(stat="identity", position = "stack") +
  theme_classic()+
  labs(x="Grupo de estudio", y="Total", fill="Estudio")+
  theme(legend.position = "bottom",
        axis.title = element_text(size=15,face="bold"),
        axis.text = element_text(size=12),
        legend.title = element_text(face="bold"))




# Number of publication types ---------------------------------------------
#Graficos por tipo de información

unique(db$type)

db |>  
  mutate(type=ifelse(type=="Tesis", "Tesis de maestría", type),
          category=type,
          category= case_when(type=="Tesis de licenciatura"~"Tesis",
                              type=="Tesis de maestría"~"Tesis",
                              type=="Tesis de doctorado"~"Tesis",
                              TRUE~type),
         category=factor(category),
         category=str_replace_all(category, "Información CONANP", "CONANP"),
         type=factor(type)) |> 
  group_by(category,type) |> 
  summarise(total=length(type)) |> 
  
  ggplot(aes(x=forcats::fct_reorder(category, total), y=total, fill=type))+
  guides(fill = guide_legend(title = "Tipo de publicación")) +
  geom_bar(stat="identity", position="stack")+
  labs(x="", y="Total")+
  theme(axis.text.x = element_text(angle=0),
        axis.title =element_text(size=10, face="bold"))+
  scale_y_continuous(breaks=seq(0,120, 10))+
  # coord_flip()+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.title = element_text(size=15,face="bold"),
        axis.text = element_text(size=12, angle=0),
        legend.title = element_text(face="bold"))


ggsave("figs/type_revilla.png", dpi = 300, height = 7, width = 15)



#Estudios por año


#OPCIÓN 1

range(db$year)

db %>%
  mutate(date= as.Date(paste0(year,"-01", "-01"), "%Y-%m-%d"),
         # year=as.character(factor(year))
  ) %>%
  group_by(year) %>%
  summarise(total=length(grupo_estudio)) %>%
  filter(total>=1) |> 
  mutate(year=factor(year)) |> 
  ggplot(aes(x=year, y=total))+
  scale_y_continuous(breaks=seq(0, 15, 1))+
  
  geom_bar(stat="identity", fill="#073561", alpha=0.75, col="black")+
  geom_text(aes(y=total+0.3, label=total))+
  # geom_point(colour = "#458B74", size=3)+
  theme_classic()+
  labs(y="No. de Estudios", x="Año")+
  theme(axis.text.x = element_text(angle=90),
        axis.title =element_text(size=15, face="bold"))

ggsave("figs/estudios_por_año.png", dpi = 300, height = 7, width = 13)


