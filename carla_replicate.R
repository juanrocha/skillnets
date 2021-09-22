## R prueba skills para el articulo "Unpacking pthe polarization of workplace skills"
## Carla Lanyon
## Import libraries

library(tidyr)
library(dplyr)
library(gtools)
library(ggplot2)

## Import dataset

setwd("C:/Users/Owner/OneDrive/Documentos/MARAT/R_prueba_alb")
df <- read.csv("skill_ONET.csv", header = TRUE)

## Change variable data type (e.g., chr~factor)

df <- df %>% mutate(O.NET.SOC.Code = as.factor(O.NET.SOC.Code),
                    Element.ID = as.factor(Element.ID),
                    Scale.ID = as.factor(Scale.ID))

## Filtering data set: selecting Scale ID for importance and relevant variable (e.g., occupation, skills, importance)

df.2 <- df %>% filter(Scale.ID == "IM") %>%
  select(O.NET.SOC.Code, Element.ID, Data.Value) %>%
  spread(Element.ID, Data.Value)


## Mapping skills complementarity

### Creating an empty data frame for saving rca

df.skills <- data.frame(matrix(nrow = length(unique(df$O.NET.SOC.Code)),
                               ncol = length(unique(df$Element.ID))))

colnames(df.skills) <- unique(df$Element.ID)
df.skills <- df.skills %>%  mutate(O.NET.SOC.Code = df.2$O.NET.SOC.Code) %>%
  relocate(O.NET.SOC.Code, "2.A.1.a")

### Calculating rca and effective use of (j,s)

for(j in 1:length(df.skills$O.NET.SOC.Code)) {

  for(s in 1:(length(colnames(df.skills))-1)) {


    ## Calculating rca for each (j, s)

    rca <- (df.2[j, s + 1]/sum(df.2[j, -1]))/(sum(df.2[, s+1])/sum(df.2[,-1]))

    ## Calculating the effective use for each (j,s)

    if(rca>1.){

      df.skills[j, s+1] <- 1

    } else {

      df.skills[j, s+1] <- 0


    }
  }

}


## Calculating theta (probability of skill complementarity)
## El valor x de debe ir de 1:595


df.compl <- data.frame(matrix(nrow=595, ncol = 3))
colnames(df.compl) <- c("Skill.1", "Skill.2", "Prob")


for(s in 1:length(unique(df$Element.ID))){

  for(i in 1:length(unique(df$Element.ID))){


    df.compl[x,1] <- colnames(df.skills[s+1])


    if(s+i > length(unique(df$Element.ID))) {

      next

    }

    df.compl[x,2] <- colnames(df.skills[s+i+1])
    df.compl[x,3] <- sum(df.skills[,s+1]*df.skills[,s+i+1])/max(sum(df.skills[,s+1]), sum(df.skills[s+i+1]))

  }

}





