function5 <- function(inputfile){
df_final_heat <- NULL
site_subsytem_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/Site_Subsystem.csv"
phase_naming_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_SAs.csv"
phase_directions_file <- "https://raw.githubusercontent.com/abdallahshabarek/ITS_NJDOT/main/SCATS_Phasing.csv"
df <- read.delim2(inputfile,header = FALSE)
df_final <- NULL
df_tmp1 <- df
library(dplyr)
for (i in (1:nrow(df_tmp1))){
  print(paste0(i,"row"))
  if(!is.na(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])[1])[1])){
    df_tmp11 <- as.data.frame(df_tmp1[1:i-1,])
    colnames(df_tmp11) <- c("V1")
    df_tmp2 <- as.data.frame(df_tmp1[i+1:nrow(df_tmp1),])
    colnames(df_tmp2) <- c("V1")
    df_tmp3 <- NULL
    df_tmp3$V1 <- NULL
    for (k in ((1:(1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)))){
      print(k)
      if( k == 1) {st <- 1} else {st <- 1+unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k-1]}
      if( k == (1+length(unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,])))/2)) {end <- nchar(df_tmp1[i,])} else {end <- unlist(stringr::str_locate_all(pattern = paste0("\n"), df_tmp1[i,]))[k]-1}
      tmp3 <- as.data.frame(substr(df_tmp1[i,],st,end))
      colnames(tmp3) <- c("V1")
      df_tmp3 <- as.data.frame(rbind(df_tmp3,tmp3))
      colnames(df_tmp3) <- c("V1")
    }
    df_final <- as.data.frame(rbind(df_final,df_tmp3))
    colnames(df_final) <- c("V1")
  } else {
    df_final <- as.data.frame(rbind(df_final,df_tmp1[i,]))
    colnames(df_final) <- c("V1")
  }
}

df <- df_final

stringr::str_match(df$V1, "CT\\s*(.*?)\\s* ")[2,][2]
gsub("^.*?_","_","ATGAS_1121")
Date <- as.Date(paste0(substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],5,6),"-",substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],7,8)))
Year <- substr(stringr::str_match(df$V1[1], "_\\s*(.*?)\\s*.sm")[1,][2],1,4)
subsystem <- as.integer(substr(df$V1[2],unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+1,unlist(stringr::str_locate_all(pattern = paste0("SS  "), df$V1[2])[1])[2]+2))
RT_name <- as.character(stringr::str_match(df$V1[1], "filename:\\s*(.*?)\\s*_")[1,][2])

substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}

substr(df[950,],18,19)
substr(df[950,],nchar(df[950,])-3,nchar(df[950,]))


if(is.na(as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),4)))==TRUE){
  as.integer(substrRight(as.character(stringr::str_match(df$V1, "CT \\s*(.*?)\\s* RL")[2,][2]),3))
}
Day_name <- as.character(stringr::str_match(df$V1[1], "Strategic Monitor On \\s*(.*?)\\s* ")[1,][2])
Date_name <- as.character(stringr::str_match(df$V1[1], paste0(Day_name, " \\s*(.*?)\\s* "))[1,][2])
Subsystem_number <- as.integer(substr(df$V1[2],nchar(Day_name)+1+nchar(Date_name)+1+6+4+1,nchar(Day_name)+1+nchar(Date_name)+1+6+4+2))
Main_site <- as.integer(as.data.frame(readr::read_csv(site_subsytem_file) %>% dplyr::filter(readr::read_csv(site_subsytem_file)$Subsystem == Subsystem_number & readr::read_csv(site_subsytem_file)$Route_name == RT_name))$Site)
line_leng <- nchar(df[2,])
df$Degree_Saturation <- ""
df$time <- ""
df$type <- ""
df$subsite <- ""
df$strategic_approach <- ""
df$notation <- ""
df$phase <- ""
df$phase_time <- ""
df$Avg_DS_Phase <-""
df$phase_A_GT <- ""
df$phase_B_GT <- ""
df$phase_C_GT <- ""
df$phase_D_GT <- ""
df$phase_E_GT <- ""
df$phase_F_GT <- ""
df$phase_G_GT <- ""
df$phase_H_GT <- ""
df$phase_I_GT <- ""
for (i in 1:nrow(df)){
  if(substr(df$V1[i],1,6)=="Friday" | substr(df$V1[i],1,6)=="Saturd" | substr(df$V1[i],1,6)=="Sunday" | substr(df$V1[i],1,6)=="Monday" | substr(df$V1[i],1,6)=="Tuesda" | substr(df$V1[i],1,6)=="Wednes" | substr(df$V1[i],1,6)=="Thursd"){
    print(paste0(i,":",nchar(df[i,])))
    df$Degree_Saturation[i] <- as.integer(sub("^.+DS ", "", df$V1[i]))
    df$time[i] <- hms::as_hms(as.POSIXct(substr(df[i,],nchar(Day_name)+1+nchar(Date_name)+2,nchar(Day_name)+1+nchar(Date_name)+1+5),format="%H:%M"))[1]
  } else if (!is.na(as.numeric(substr(df[i,],1,2)[1]))==TRUE) {
    type <- as.character(stringr::str_match(substr(df$V1[i],2,nchar(df$V1[i])-2), " \\s*(.*?)\\s* ")[1,][2])
    df$type[i] <- type
    df$subsite[i] <- as.integer(stringr::str_match(df$V1[i], "\\s*(.*?)\\s* ")[1,][2])
    strategic_approach <- as.integer(stringr::str_match(df$V1[i], paste0(type," \\s*(.*?)\\s* "))[1,][2])
    df$strategic_approach[i] <- strategic_approach
    stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1]
    if(type == "SA"){
      end_str <- unlist(stringr::str_locate_all(pattern = paste0(type," ",strategic_approach," "), df[i,])[1])[2]
    } else if (type == "LK"){
      end_str <- unlist(stringr::str_locate_all(pattern = paste0(type,"  ",strategic_approach," "), df[i,])[1])[2]
    }
    df$notation[i] <- substr(df[i,],end_str+1,end_str+2)[1]
    end_str_phase <- unlist(stringr::str_locate_all(pattern = paste0("!"), df[i,])[1])[1]
    df$phase[i] <- as.character(substr(df$V1[i],17,19))
    df$phase_time[i] <- as.integer(substr(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2],nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])-3,nchar(stringr::str_match(df$V1[i], " \\s*(.*?)\\s*!")[1,][2])))
    df$Avg_DS_Phase[i] <- as.integer(substr(df[i,],nchar(df[i,])-4,nchar(df[i,]))[1])
  } else if (substr(df$V1[i],1,3)=="A=<"){
    df$phase_A_GT[i] <- as.integer(stringr::str_match(df$V1[i], "A=<\\s*(.*?)\\s*>")[1,][2])
    df$phase_B_GT[i] <- 
      if(is.na(unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1])==TRUE){
        ""  
      } else {
        as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("B="), df$V1[i])[1])[1]+4))
      }
    df$phase_C_GT[i] <- 
      if(is.na(unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1])==TRUE){
        ""  
      } else {
        as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("C="), df$V1[i])[1])[1]+4))
      }
    df$phase_D_GT[i] <- 
      if(is.na(unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1])==TRUE){
        ""  
      } else {
        as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("D="), df$V1[i])[1])[1]+4))
      }
    df$phase_E_GT[i] <- 
      if(is.na(unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1])==TRUE){
        ""  
      } else {
        as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("E="), df$V1[i])[1])[1]+4))
      }
    df$phase_F_GT[i] <- 
      if(is.na(unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1])==TRUE){
        ""  
      } else {
        as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("F="), df$V1[i])[1])[1]+4))
      }
    df$phase_G_GT[i] <- 
      if(is.na(unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1])==TRUE){
        ""  
      } else {
        as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("G="), df$V1[i])[1])[1]+4))
      }
    df$phase_H_GT[i] <- 
      if(is.na(unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1])==TRUE){
        ""  
      } else {
        as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("H="), df$V1[i])[1])[1]+4))
      }
    df$phase_I_GT[i] <- 
      if(is.na(unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1])==TRUE){
        ""  
      } else {
        as.integer(substr(df$V1[i],unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+2,unlist(stringr::str_locate_all(pattern = paste0("I="), df$V1[i])[1])[1]+4))
      }
  }
}
df$Day <- Day_name
df$Date <- Date
df$Date_name <- Date_name
df$SS <- subsystem
df$phase <- gsub('\\s+', '',df$phase)
df <- df %>% dplyr::filter(phase != "AB" & phase != "BC" & phase != "BA" & phase != "CB" & phase != "AC" & phase != "CA" & phase != "AD" & phase != "DA" & phase != "BD" & phase != "DB" & phase != "DC" & phase != "CD")
df1 <- readr::read_csv(phase_naming_file)
df1$ss <- NULL
df1 <- df1 %>% dplyr::filter(Site == Main_site)
colnames(df1) <- c("site","phase_name","phase_number")
df22 <- sqldf::sqldf("select a.*,
case when (a.Type = 'SA' or a.Type='LK') and (a.phase <> 'A' and  a.phase <> 'B' and a.phase <> 'C' and a.phase <> 'D' and a.phase <> 'E' and a.phase <> 'F' and a.phase <> 'G' and a.phase <> 'H' and a.phase <> 'I') then b.Phase_name
when (a.Type = 'SA' or a.Type='LK') and (a.phase = 'A' or  a.phase = 'B' or a.phase = 'C' or a.phase = 'D' or a.phase = 'E' or a.phase = 'F' or a.phase = 'G' or a.phase = 'H' or a.phase = 'I') then a.Phase
when (a.Type <> 'SA' and a.Type <> 'LK') then ''
end as Phase_name
from df as a
left join df1 as b
on a.phase = b.phase_number", drv ="SQLite")
df22 <- df22 %>% naniar::replace_with_na(replace = list(time = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(Degree_Saturation = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_A_GT = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_B_GT = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_C_GT = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_D_GT = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_E_GT = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_F_GT = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_G_GT = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_H_GT = c("")))
df22 <- df22 %>% naniar::replace_with_na(replace = list(phase_I_GT = c("")))
df22 <- df22 %>% tidyr::fill(time, .direction = "down")
df22 <- df22 %>% tidyr::fill(phase_A_GT, .direction = "up")
df22 <- df22 %>% tidyr::fill(phase_B_GT, .direction = "up")
df22 <- df22 %>% tidyr::fill(phase_C_GT, .direction = "up")
df22 <- df22 %>% tidyr::fill(phase_D_GT, .direction = "up")
df22 <- df22 %>% tidyr::fill(phase_E_GT, .direction = "up")
df22 <- df22 %>% tidyr::fill(phase_F_GT, .direction = "up")
df22 <- df22 %>% tidyr::fill(phase_G_GT, .direction = "up")
df22 <- df22 %>% tidyr::fill(phase_H_GT, .direction = "up")
df22 <- df22 %>% tidyr::fill(phase_I_GT, .direction = "up")
df22 <- df22[!grepl(" Int",substr(df22$V1,1,4)),]
df22 <- df22[!grepl("Strategic Monitor On",substr(df22$V1,1,20)),]
df22 <- df22[!grepl("A=<",substr(df22$V1,1,3)),]
df_phase <- df22 %>% dplyr::filter(type == "SA" & subsite == Main_site)
df_phase$time <- hms::as_hms(as.POSIXct(paste0(sprintf("%02d",as.integer(as.integer(df_phase$time)/3600)),":",sprintf("%02d",as.integer(as.integer((as.integer(df_phase$time))%%3600)/60))),format="%H:%M"))
df_phase$time <- as.character(df_phase$time)
df_phase$Avg_DS_Phase <- as.double(df_phase$Avg_DS_Phase)
df_phase <- sqldf::sqldf("select a.time, a.Phase_name, a.Avg_DS_by_Phase, 
case when (a.Phase_name= 'A') then cast(a.phase_A_GT as int)
when (a.Phase_name= 'B') then cast(a.phase_B_GT as int)
when (a.Phase_name= 'C') then cast(a.phase_C_GT as int)
when (a.Phase_name= 'D') then cast(a.phase_D_GT as int)
when (a.Phase_name= 'E') then cast(a.phase_E_GT as int)
when (a.Phase_name= 'F') then cast(a.phase_F_GT as int)
when (a.Phase_name= 'G') then cast(a.phase_G_GT as int)
when (a.Phase_name= 'H') then cast(a.phase_H_GT as int)
when (a.Phase_name= 'I') then cast(a.phase_I_GT as int)
End as Phase_time
from (select k.time, k.Phase_name, 
Avg(k.Avg_DS_Phase) as [Avg_DS_by_Phase], phase_A_GT, phase_B_GT, phase_C_GT, phase_D_GT, 
phase_E_GT, phase_F_GT, phase_G_GT, phase_H_GT, phase_I_GT
from df_phase as k
group by k.time, k.Phase_name) as a", drv="SQLite")
return(df_phase)
}