
### Développement du script par Gudule ###

# setwd("~/Data/Sync_Mega/Enseignement/Master/M1/Systèmes_complexes/USV_data")


# /!\ Aller dans Session -> Set working directory -> Choose directory 


fichiers<-list.files() #les fichiers du répertoire courant.

list.files(pattern="*_0.csv")

fichiers[1]

file.info(fichiers[1]) # renvoie un dataframe avec une ligne par fichier et une colonne par attribut de fichier (dates, owner, group, permissions, taille, ...)

fichiers[1]

# nom du fichier contient différentes informations "B1_1_4b_plus_60kHz_0.csv"
# B = condition
# B1 = parents
# _1_ = portée
# 4b = individu
# plus_60kHz = fréquences de vocalisations supérieures à 60 kHz
# 0 = time de 0 à 60 secondes

# nchar("toto") # longueur de la chaîne de caractères (ou vecteur des longueurs si vecteur de chaînes).
nchar("toto")

# substr("abcdef", 2, 4) # extraction de sous chaîne. Ici donne "bcd" (début et fin en comptant à partir de 1 et positions incluses.
substr(fichiers[1],1,5)

# strsplit # permet de splitter des chaînes de caractères en fonction d'une sous-chaîne ou d'une expression régulière 
strsplit(fichiers[1],"_")

# gregexpr(x, vect) # x est le pattern à rechercher dans chaque élément du vecteur vect. 
# Retourne une liste avec un élément par élément du vecteur vect, chaque élément de la liste étant soit -1 si pas de match, soit le vecteur des positions de début de match. 

gregexpr("4b_",fichiers[1])

gregexpr("4b_",fichiers[1])[[1]][1]

# "condition" est toujours la première lettre du nom
condition <- substr(fichiers[1],1,1)

parents <- strsplit(fichiers[1],"_")[[1]][1]

portée <- strsplit(fichiers[1],"_")[[1]][2]

individu <- paste(parents,strsplit(fichiers[1],"_")[[1]][3],sep="_")
phase <- substr(individu, nchar(individu), nchar(individu)) 
individu <- substr(individu, 1, nchar(individu)-1)

frequence <- paste(strsplit(fichiers[1],"_")[[1]][4], strsplit(fichiers[1],"_")[[1]][5], sep="")

time <- strsplit(fichiers[1],"_")[[1]][6]

time<- strsplit(strsplit(fichiers[1],"_")[[1]][6],".", fixed=T)[[1]][1]

# ou

time<- substr(strsplit(fichiers[1],"_")[[1]][6], 1, nchar(strsplit(fichiers[1],"_")[[1]][6])-4)


data_fichier_n <- read.csv(fichiers[1])

str(data_fichier_n)

data_fichier_n<-data.frame(condition, parents, portée, individu, frequence, time, data_fichier_n)


# Boucle for

for(n in 1 : 10){
  print(c("coucou",n))
}


data_tous_fichiers<-NULL

for(n in 1:length(fichiers)){
  data_fichier_n<-NULL
  
  condition <- substr(fichiers[n],1,1)
  parents <- strsplit(fichiers[n],"_")[[1]][1]
  portée <- strsplit(fichiers[n],"_")[[1]][2]
  individu <- paste(parents,strsplit(fichiers[n],"_")[[1]][3],sep="_")
  phase <- substr(individu, nchar(individu), nchar(individu)) 
  individu <- substr(individu, 1, nchar(individu)-1)
  frequence <- paste(strsplit(fichiers[n],"_")[[1]][4], strsplit(fichiers[n],"_")[[1]][5], sep="")
  time<- substr(strsplit(fichiers[n],"_")[[1]][6], 1, nchar(strsplit(fichiers[n],"_")[[1]][6])-4)
  data_fichier_n <- read.csv(fichiers[n])
  data_fichier_n<-data.frame(condition, parents, portée, individu, phase, frequence, time, data_fichier_n)
  
  data_tous_fichiers<-rbind(data_tous_fichiers,data_fichier_n)
  
}

str(data_tous_fichiers)

data_tous_fichiers$time<-as.integer(data_tous_fichiers$time)
#suppression de la colonne "X"
data_tous_fichiers$X <-NULL

#transformation des "chr" en facteur
for(n in 1:length(data_tous_fichiers)){
  if(is.character(data_tous_fichiers[,n])) { data_tous_fichiers[,n] <- as.factor(data_tous_fichiers[,n]) } 
}

str(data_tous_fichiers)

# calcul du nombre de vocalisations par minute

subset(data_tous_fichiers, time==0)

unique(data_tous_fichiers$file)
which(is.na(subset(data_tous_fichiers,  file=="B3_1_1b.wav")$temp_detect_deb)==F, arr.ind=F)

nb_usv_per_min<-NULL
for(n in 1:length(unique(data_tous_fichiers$file))){
  for(t in seq(0, 240, 60)){
    condition<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$condition[1]
    parents<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$parents[1]
    portée<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$portée[1]
    individu<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$individu[1]
    phase<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$phase[1]
    frequence<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$frequence[1]
    time<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$time[1]
    nb_usv<-length(which(is.na(subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$temp_detect_deb)==F, arr.ind=T))

    nb_usv_per_min<-rbind(nb_usv_per_min, data.frame(condition,parents,portée,individu,phase,time,frequence,nb_usv))
  }
}
head(nb_usv_per_min)
str(nb_usv_per_min)
#nb_usv_per_min$time<-as.numeric(nb_usv_per_min$time)

nb_usv_per_min$ord_condition<- ordered(nb_usv_per_min$condition, levels=c("M","B"))
library(ggplot2)
p<-ggplot(subset(nb_usv_per_min, phase=="a"), aes(x=as.factor(time), y=
         nb_usv, fill=ord_condition))+
         geom_boxplot()
  p<-p+labs(x="Time (s)", y="Number of USV") 
  p+scale_fill_discrete(name = "Condition", palette="Paired")


# Fonctions avec R
  
  plus2 <- function(x) {
    res <- x + 2
    return(res)
  }

plus2(4)

figure_fonction1<-function(jeu_couleur){
  p<-ggplot(nb_usv_per_min, aes(x=as.factor(time), y=
                                  nb_usv, fill=ord_condition))+
    geom_boxplot()
  p<-p+labs(x="Time (s)", y="Number of USV") 
  p<-p+scale_fill_discrete(name = "Condition", palette=jeu_couleur)
  return(p)
}

figure_fonction1("Set1")
figure_fonction1("Set2")
figure_fonction1("Set3")
figure_fonction1("Greens")

p<-ggplot(nb_usv_per_min, aes(x=as.factor(time), y=
                                nb_usv, fill=ord_condition))+
  geom_boxplot()
p<-p+labs(x="Time (s)", y="Number of USV") 
p+scale_fill_discrete(name = "Condition", palette="Paired")

  
  fonction_extraction_data<-function(param){
  
  out_object<-NULL
  for(n in 1:length(unique(data_tous_fichiers$file))){
    for(t in seq(0, 240, 60)){
      condition<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$condition[1]
      parents<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$parents[1]
      portée<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$portée[1]
      individu<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$individu[1]
      phase<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$phase[1]
      frequence<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$frequence[1]
      time<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$time[1]
      variable<-length(
        which(
          is.na(subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])[,param])==FALSE)
        )
      
      out_object<-rbind(out_object, data.frame(condition,parents,portée,individu,time,frequence,variable))
      
    }
  }
  return(out_object)
}

fonction_extraction_data(param="temp_detect_deb")


names(data_tous_fichiers)



fonction_extraction_data<-function(param, min_dur){
  
  out_object<-NULL
  for(n in 1:length(unique(data_tous_fichiers$file))){
    for(t in seq(0, 240, 60)){
      condition<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$condition[1]
      parents<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$parents[1]
      portée<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$portée[1]
      individu<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$individu[1]
      phase<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$phase[1]
      frequence<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$frequence[1]
      time<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$time[1]
      variable<-length(
        which(
          is.na(subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n] & duration_detect>min_dur)[,param])==FALSE)
      )
      
      out_object<-rbind(out_object, data.frame(condition,parents,portée,individu,phase,time,frequence,variable))
      
    }
  }
  return(out_object)
}


temp<-fonction_extraction_data(param="temp_detect_deb", min_dur = 0.01)

p<-ggplot(temp, aes(x=as.factor(time), y=
                                variable, fill=condition))+
  geom_boxplot()
p<-p+labs(x="Time (s)", y="Number of USV") 
p+scale_fill_discrete(name = "Condition", palette="Paired")






fonction_extraction_data<-function(param){
  
  out_object<-NULL
  for(n in 1:length(unique(data_tous_fichiers$file))){
    for(t in seq(0, 240, 60)){
      condition<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$condition[1]
      parents<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$parents[1]
      portée<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$portée[1]
      individu<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$individu[1]
      frequence<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$frequence[1]
      time<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$time[1]
      variable<-mean(subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])[,param], na.rm=T)

      out_object<-rbind(out_object, data.frame(condition,parents,portée,individu,time,frequence,variable))
      
    }
  }
  return(out_object)
}

fonction_extraction_data(param="temp_detect_deb")
fonction_extraction_data(param="freq_detect_mean")
fonction_extraction_data(param="freq_detect_max")




fonction_extraction_data<-function(colonne){
  param<-which(names(data_tous_fichiers)==colonne)
  out_object<-NULL
  for(n in 1:length(unique(data_tous_fichiers$file))){
    for(t in seq(0, 240, 60)){
      condition<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$condition[1]
      parents<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$parents[1]
      portée<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$portée[1]
      individu<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$individu[1]
      frequence<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$frequence[1]
      time<-subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])$time[1]
      variable<-mean(subset(data_tous_fichiers, time==t & file== unique(data_tous_fichiers$file)[n])[,param], na.rm=T)

      out_object<-rbind(out_object, data.frame(condition,parents,portée,individu,time,frequence,variable))
     }
  }
  return(out_object)
}

fonction_extraction_data(colonne="duration_detect")
fonction_extraction_data(colonne="freq_detect_mean")
fonction_extraction_data(colonne="freq_detect_max")


str(data_tous_fichiers)


str(nb_usv_per_min)
library(glmmTMB)
library(car)
a1=glmmTMB(nb_usv ~ condition*
             time+
             (1|individu) +
             (1|parents),
           data=subset(nb_usv_per_min, phase=="a"),
           family=poisson(link="log"))

a1<- glmmTMB(nb_usv ~ condition*time +
                   (1|individu) + (1|parents),
                 data = subset(nb_usv_per_min, phase == "a"),
                 family = nbinom2(link = "log"))


a1 <- glmmTMB(nb_usv ~ condition*time +
                    (1|individu) + (1|parents),
                  data = subset(nb_usv_per_min, phase == "a"),
                  family = genpois(link = "log"),
                  control = glmmTMBControl(
                    optimizer = optim,
                    optArgs = list(method = "BFGS")
                  ))

Anova(a1)