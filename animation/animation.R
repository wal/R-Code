#####################################################################################
## Load libraries ##
#####################################################################################
library(tidyquant)
library(tibbletime)
library(tweenr)
library(tidyverse)

#####################################################################################
## Get housing starts data ##
#####################################################################################
df <- tq_get("HOUST1F",get="economic.data",from="1959-01-01")

#####################################################################################
## function for rolling windows ##
#####################################################################################
mys <- function(win=12){
  rolling_mean <- rollify(mean, window = win)  #function creates rolling average based on win
  df %>%mutate(dy=rolling_mean(price),
               w=as.character(win)) %>% map_if(is.character,as.factor) %>% 
    as.data.frame  -> df
  return(df)
}


#####################################################################################
## animate ##
#####################################################################################
my.list<-purrr::map(c(1,seq(12,240,12),1),mys)


tf <- tween_states(my.list, tweenlength= 2, statelength=3, 
                   ease=rep('cubic-in-out',3),
                   nframes=120)

# Max frames 
N<-max(tf$.frame)

mydir<- "animation/plots" # change to your directory number

myf <- function(i)
{
  file_path = paste0(mydir, "/plot-",5000+i ,".png")
  g<-
    ggplot(data=filter(tf,.frame==i),aes(x=date,y=price)) +
    geom_point(size=0.5,alpha=0.5)+
    geom_line(color="royalblue",aes(y=dy))+
    theme_minimal()+
    labs(x="",y="",
         title="U.S. single-family housing starts low relative to history",
         subtitle=paste0("Starts (1000s seasonally adjusted annual rate) on 1-unit structures",
                         "\ndots monthly estimates (1000s SAAR) and line ",
                         head(filter(tf,.frame==i),1)$w,
                         "-month moving average."),
         caption="@lenkiefer Source: Source: U.S. Census Bureau and Department of Housing and Urban Development") +
    theme(legend.position="none",
          plot.title=element_text(face="bold",size=18),
          plot.subtitle=element_text(face="italic",size=14),
          plot.caption=element_text(hjust=0))+
    scale_y_continuous(labels=scales::comma,sec.axis=dup_axis()) +
    scale_x_date(date_breaks="5 years",date_labels="%Y")
  
  # Save to image directory  
  ggsave(filename=paste0(mydir, "/plot-",5000+i ,".png"),
         plot=g  ,
         width=6,height=4,scale=1.5)
}

# Save images with purrr
purrr::map(1:N,myf)