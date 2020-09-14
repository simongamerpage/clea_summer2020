# R-script to build CLEA's Party Nationalization Measures (PNM) datasets
# Created by Kirill Kalini
# University of Michigan
# November, 2016
# Updated by Fabricio Vasselai in August, 2018 and July 2020


# buildPNMdatasets(dataSource="", ineq.ind="Gini",  filterIndependents=FALSE,  filterSingleConstituencies=FALSE)
#
# Parameters:
#    dataSource                 - path and name of CLEA file (in .rdata format).
#    outputFolder               - optional path to folder where output files should be saved, ending with "/" in Unix systems or "\" in Windows
#    ineq.ind                   - by default computes indices based on "Gini" (other inequality indices, which can be tried include 
#                                 "RS", "Atkinson", "Theil", "Kolm", "var", "square.var", "entropy" -- see package "ineq" for more detail).
#    filterIndependents         - by default includes independents.
#    filterSingleConstituencies - by default includes single constituencies.

# Output consists of three files "party.level.csv", "national.level.csv", "cst.level.csv"

# Steps to take to run the script:
# Step 1  Highlight the function, and press the "Run" button.
# Step 2  Run the function:  buildPNMdatasets(), defining any parameters of interest.

buildPNMdatasets <- function(dataSource, outputFolder, ineq.ind="Gini", 
                               filterIndependents=FALSE,
                               filterSingleConstituencies=FALSE)
{
  usePackage <- function(required.package) {
    if (!is.element(required.package, installed.packages()[,1]))
      install.packages(required.package, dep = TRUE)
    require(required.package, character.only = TRUE)
  }

  usePackage("foreign")
  usePackage("readstata13")
  usePackage("plyr")
  usePackage("magrittr")
  usePackage("dplyr")
  usePackage("ineq")
  usePackage("XML")

#Supplementary script
if (dataSource == "" | is.na(dataSource))
{
	print("ERROR: no CLEA dataset path/file.rdata informed.")
} else
{
    if(is.character(dataSource))
    {
        if(file.exists(dataSource))
        {
            print(paste0("Loading CLEA data from ", dataSource))
            load(dataSource)
            file.name <- strsplit(tail(strsplit(dataSource, "/")[[1]], 1), ".rdata")[[1]][1]
            data <- get(file.name)
            print(paste0("Finished loading CLEA data from ", dataSource))
        } else
        {
            print("ERROR: informed path/file.rdata does not exist.")
        }
    } else
    {
        data <- dataSource
    }
}

if (ineq.ind!="Gini" & ineq.ind%in%c("Gini", "RS", "Atkinson", "Theil", "Kolm", "var", "square.var", "entropy")==FALSE)
{
	ineq.ind <- "Gini"
}

data.grid<-data %>% 
  select(id, ctr_n, ctr, yr, mn, cst,  cst_n, pty, pty_n)

data<-data %>%select(ctr_n, ctr, yr, mn, cst,  cst_n, pty, pty_n, vv1, pv1, seat)

data.a<-data %>%
  do((function(x) {
    x$can<-0
    x$vv1<-ifelse(x$vv1<0,NA,x$vv1)
    x$pv1<-ifelse(x$pv1<0,NA,x$pv1)
    na_cand <- ifelse(is.na(x$vv1) & is.na(x$pv1 ),1,0)
    data.frame(x, na_cand)})(.)) %>%
  distinct(.keep_all = TRUE)

data.b<-data %>%
  do((function(x) {
    x$can<-0
    x$vv1<-ifelse(x$vv1<0,NA,x$vv1)
    x$pv1<-ifelse(x$pv1<=0,NA,x$pv1)
    na_cand <- ifelse(is.na(x$vv1) | is.na(x$pv1 ),1,0)
    data.frame(x, na_cand)})(.)) %>%
  distinct(.keep_all = TRUE) %>%
  filter(na_cand==0)


data.c<-data %>%
  do((function(x) {
    x$can<-0
    x$vv1<-ifelse(x$vv1<0,NA,x$vv1)
    x$pv1<-ifelse(x$pv1<=0,NA,x$pv1)
    x$seat<-ifelse(x$seat<=0,NA,x$seat)
    na_cand <- ifelse(is.na(x$vv1) & is.na(x$pv1) & is.na(x$seat),1,0)
    data.frame(x, na_cand)})(.)) %>%
  distinct(.keep_all = TRUE) %>%
  filter(na_cand==0)

data.e<-data %>%
  do((function(x) {
    x$can<-0
    x$vv1<-ifelse(x$vv1<0,NA,x$vv1)
    x$pv1<-ifelse(x$pv1<=0,NA,x$pv1)
    na_cand <- ifelse(is.na(x$vv1) & is.na(x$pv1 ),1,0)
    data.frame(x, na_cand)})(.)) %>%
  distinct(.keep_all = TRUE) %>%
  filter(na_cand==0)


if(filterSingleConstituencies==TRUE){  
  if (filterIndependents==TRUE){
      data.d<-data.a %>%
      group_by(ctr, yr, mn)%>%
      arrange(cst)%>%
      do((function(x) {
        sin_const <- ifelse(x$cst[1] == 1 & x$cst[length(x$cst)]==1, 1, 0)
        data.frame(x, sin_const)})(.)) %>%
        filter(sin_const==0,  pty!= 6000)}else
          {
      data.d<-data.a %>%
      group_by(ctr, yr, mn)%>%
      arrange(cst)%>%
      do((function(x) {
        sin_const <- ifelse(x$cst[1] == 1 & x$cst[length(x$cst)]==1, 1, 0)
        data.frame(x, sin_const)})(.)) %>%
        filter(sin_const==0)}}
  
if(filterSingleConstituencies==FALSE){  
    if (filterIndependents==TRUE){
      data.d<-data.a %>%
        filter(pty!= 6000)}else
        {data.d<-data.a}}  


#Main Script 
print("Computing inequality measures...", quote=FALSE)
  
start.timer <- proc.time()  
gini<-data.a%>%
  distinct(ctr, yr, mn, cst, pty, .keep_all = TRUE)%>%   #distinct(ctr, yr, mn, cst, pty, .keep_all = TRUE)%>% 
  group_by(ctr, yr, mn, cst, pty) %>% 
  mutate(
    vv1=sum(vv1,na.rm=TRUE),
    pv1=sum(pv1,na.rm=TRUE)) %>%
  group_by(ctr, yr, mn) %>% 
  mutate(
    nat_vv1 = sum(pv1, na.rm=TRUE)) %>%
  group_by(ctr, yr, mn, pty) %>% 
  mutate(
    pty_nat_vv1 = sum(pv1, na.rm=TRUE)) %>%
  mutate(
    nat_pvs = pty_nat_vv1/nat_vv1) %>%
  filter(nat_pvs>0.05) %>%
  mutate(
    pvs=pv1/vv1,
    pvs=ifelse(is.infinite(pvs), NA, pvs),
    pvs=ifelse(is.na(pvs), 0, pvs))%>% 
  group_by(ctr, yr, mn, pty) %>%
  do((function(x) {
    giniI<-ineq(x$pvs, NULL,  type = ineq.ind, na.rm=TRUE)
    data.frame(x, giniI)})(.)) %>%
  mutate(
    giniI=replace(giniI, giniI<0, 0)) %>%
  select(ctr_n, ctr, yr, mn, cst, pty, giniI, pv1, vv1) %>%
  arrange(ctr_n, ctr, yr, mn, cst, pty) %>%
  as.data.frame()
write.csv(gini, paste0(outputFolder, "gini.values.csv"))


#******************************************
#*****Party Nationalization Measures*******
#*********Party Level Dataset**************
#******************************************
print("Computing Party Level Dataset...", quote=FALSE)

party.level<-data.a %>%
  distinct(.keep_all = TRUE) %>%
  left_join(gini) %>%
  mutate(
    PNS= 1 - giniI)%>%
  group_by(ctr, yr)%>%
  mutate(
  district_size=length(unique(cst)),
    PNS_s = PNS^(1/(log(district_size))))%>%
  distinct(ctr, yr, mn, cst, pty, .keep_all = TRUE)%>%
  group_by(ctr, yr, mn)%>%
  mutate(
    nat_vv1=sum(pv1, na.rm=TRUE))%>%
  group_by(ctr, yr, mn, pty)%>%
  mutate(
    pty_vv1=sum(pv1, na.rm=TRUE),
    denominator = nat_vv1 * pty_vv1,
    cst_vv1=vv1,
    vote_share=pv1/cst_vv1)%>%
  arrange(ctr, yr, mn, pty, vote_share) %>%
  group_by(ctr, yr, mn, pty)%>%
  mutate(
    p_j=cumsum(pv1),
    inside = cst_vv1 * (p_j - (pv1/2)),
    numerator = sum(inside, na.rm=TRUE),
    PNS_w = (2 * numerator)/ denominator)%>%
  group_by(ctr, yr, mn, cst)%>%
  mutate(
    cst_vv1_new = sum(pv1, na.rm=TRUE),
    vote_share_new = pv1/cst_vv1_new) %>% 
  group_by(ctr, yr, mn, pty)%>%
  mutate(
    p_j_new=cumsum(pv1),
    inside_new = cst_vv1_new * (p_j_new - (pv1/2)),
    numerator_new = sum(inside_new, na.rm=TRUE),
    PNS_w_new = (2 * numerator_new)/ denominator,
    alt_vv1 = 0,
    alt_vv1 = replace(alt_vv1, PNS_w>=1, 1), 
    PNS_w = ifelse(PNS_w>=1, PNS_w_new, PNS_w),
    PNS_w = ifelse(PNS_w==0, NA, PNS_w),
    PNS_w = ifelse(PNS_w>=1, PNS_w_new, PNS_w),
    PNS_s = ifelse(PNS_s==0, NA, PNS_s),
    PNS_w = replace(PNS_w, pty == 3999|pty >= 6000|ctr == 292 | 
                             ctr ==376|ctr == 674|
                            (ctr == 840 & yr == 1959 & cst ==435), NA),
    alt_vv1 = replace(alt_vv1, is.na(PNS_w), 0))%>%
    select(ctr_n, ctr, yr, mn, cst, pty_n, pty, cst_vv1, nat_vv1, PNS, PNS_s, PNS_w, alt_vv1)%>%    
  mutate(
    top = nat_vv1^2, 
    square = cst_vv1^2)%>%
  group_by(ctr, yr, mn, cst)%>%
  mutate(
    pid=1:length(nat_vv1),
    helper = rep(NA,length(nat_vv1)),
    helper = replace(helper, pid==1, square)) %>% group_by(ctr, yr)%>%
  mutate(
    cst_tot = length(unique(cst)),
    bottom=sum(helper, na.rm=TRUE),
    power_E = top/bottom,
    PNS_sw = (PNS_w)^(1/(log10(power_E))),
    PNS_sw= replace(PNS_sw, is.na(PNS_sw), NA),
    PNS_sw= replace(PNS_sw, PNS_sw>1, NA),
    PNS=replace(PNS, PNS==1, NA),
    PNS_s=replace(PNS_s,pty >= 3996 & pty < 5000, NA),
    PNS_w=replace(PNS_w, pty >= 3996 & pty < 5000, NA),
    PNS_sw=replace(PNS_sw, pty >= 3996 & pty < 5000, NA))%>%
  right_join(data.grid)%>%
  select(id, ctr_n, ctr, yr, mn, pty_n, pty, PNS, PNS_s, PNS_w, PNS_sw, cst_tot)%>%
  arrange(ctr, yr, mn, pty) %>%
  distinct(ctr, yr, mn, pty, .keep_all = TRUE)
write.csv(party.level, paste0(outputFolder, "party.level.csv"))
print(paste("Party Level Dataset has been sucessfully saved to the folder ", getwd(), sep=""), quote=FALSE)
#sum(unique(paste(party.level2$ctr, party.level2$yr, party.level2$mn, sep=":")))



#******************************************
#*****Party Nationalization Measures*******
#********National Level Dataset************
#******************************************
print("Computing National Level Dataset...", quote=FALSE)

national.level.nat<-data.b %>%
  distinct(ctr, yr, mn, cst, pty, .keep_all = TRUE)%>% 
  group_by(ctr, yr, mn, pty) %>% 
  mutate(
    pv1=sum(pv1, na.rm=TRUE)) %>%
  distinct(ctr, yr, mn, pty, .keep_all = TRUE)%>% 
  group_by(ctr, yr, mn) %>% 
  mutate(
    nat_vv1 = sum(pv1, na.rm=TRUE),
    party_prop_nat2 = (pv1/nat_vv1)^2,
    denom = sum(party_prop_nat2),
    ENEP_nat = 1/denom) %>%
  select(ctr_n, ctr, yr, mn, ENEP_nat)%>%
  mutate(
    ENEP_nat= replace(ENEP_nat, yr < 1834 & ctr == 840, NA),
    ENEP_nat= replace(ENEP_nat, yr == 1950 & ctr == 410, NA))%>%
  distinct(ctr_n, ctr, yr, mn, .keep_all = TRUE)%>% 
  arrange(ctr_n, ctr, yr, mn)

national.level.nat2<-data.b %>%
  distinct(.keep_all = TRUE)%>% 
  group_by(ctr, yr, mn, cst)%>%
  mutate(
    new_vv1=sum(pv1),
    share2 = (pv1/new_vv1)^2,
    denom = sum(share2),
    ENEP_cst = 1/denom,
    indicator = 0,
    indicator= ifelse(new_vv1 != vv1, 1,  indicator))%>%
  distinct(ctr, yr, mn, cst, .keep_all = TRUE)%>%
  group_by(ctr, yr, mn)%>%
  mutate(
    indicator = ifelse(sum(indicator, na.rm=TRUE)>0,1,indicator),
    nat_vv1 = sum(new_vv1, na.rm=TRUE),
    cst_wght =  new_vv1/nat_vv1,
    weighted =  cst_wght * ENEP_cst,
    ENEP_wght = sum(weighted),
    ENEP_avg = mean(ENEP_cst))%>%
  left_join(national.level.nat)%>%
  select(ctr_n, ctr, yr, mn, ENEP_nat, ENEP_avg, ENEP_wght, indicator)%>%
  mutate(
    ENEP_avg= replace(ENEP_avg, yr < 1834 & ctr == 840, NA),
    ENEP_avg= replace(ENEP_avg, yr == 1950 & ctr == 410, NA),
    ENEP_wght= replace(ENEP_wght, yr < 1834 & ctr == 840, NA),
    ENEP_wght= replace(ENEP_wght, yr == 1950 & ctr == 410, NA))%>%
  distinct(ctr_n, ctr, yr, mn, .keep_all = TRUE)%>%
  mutate(
    Cox =  (ENEP_nat - ENEP_avg)/ ENEP_nat,
    MK_I = (ENEP_nat -  ENEP_avg)/ ENEP_avg,
    MK_I_w = (ENEP_nat - ENEP_wght)/ ENEP_wght)
    
national.level.mk_n<- data.e %>%
  distinct(ctr, yr, mn, pty, cst, .keep_all = TRUE)%>% 
  group_by(ctr, yr, mn, cst)%>%
  mutate(
    new_vv1=sum(pv1, na.rm=TRUE),
    share2 = (pv1/new_vv1) * (pv1/new_vv1),
    denom = sum(share2, na.rm=TRUE),
    ENEP_cst = 1/denom)%>%
  left_join(national.level.nat2)%>%
  mutate(
    I_i = (( ENEP_nat-ENEP_cst)/ ENEP_cst) * 100)%>%
  group_by(ctr, yr, mn)%>%
  mutate(
    alpha = .5,
    beta = .25,
    gamma = .5,
    cst_tot = length(unique(cst)),
    nat_vote = sum(new_vv1, na.rm=TRUE),
    cst_vote_proportion = new_vv1/nat_vote,
    product =  ENEP_cst *  cst_vote_proportion,
    sum_cst = sum(product, na.rm=TRUE),
    denominator = cst_tot * sum_cst,
    W_tilde = ENEP_cst/ denominator,
    I_w = ((ENEP_nat - sum_cst)/sum_cst)*100,
    numerator = (I_i -  I_w)^2 * W_tilde,
    sum_numerator = sum(numerator, na.rm=TRUE),
    coeff_var_I_i = sqrt(sum_numerator)/I_w,
    numerator2 = ((I_i - I_w)^4) * W_tilde,
    sum_numerator2 = sum(numerator2, na.rm=TRUE),
    denominator2 = ((I_i - I_w)^2) * W_tilde,
    sum_denominator2 = sum(denominator2, na.rm=TRUE),
    sq_sum_denominator2 = (sum_denominator2)^2,
    kurtosis_I_i = sum_numerator2/sq_sum_denominator2,
    D =  (coeff_var_I_i)^gamma * (kurtosis_I_i)^(1 - gamma),
    MK_N = ((I_w)^alpha) * (D^(1 - alpha)),
    MK_N_two = ((I_w)^alpha) *  ((coeff_var_I_i)^beta) * ((kurtosis_I_i)^(1 - alpha - beta)))%>%
    group_by(ctr, yr, mn)%>%
  mutate(
    nat_vv1 = sum(pv1, na.rm=TRUE))%>%
  group_by(ctr, yr, mn, pty)%>%
  mutate(
    pty_vv1 = sum(pv1, na.rm=TRUE))%>%
  distinct(ctr, yr, mn, pty, .keep_all = TRUE)%>%
  right_join(party.level)%>%
  arrange(ctr, ctr_n, yr, mn, pty, pty_n)%>%
  mutate(
    weight = pty_vv1/nat_vv1)%>%
  group_by(ctr, yr, mn)%>%
  mutate(        
    PSNS = sum(PNS * weight, na.rm=TRUE),
    PSNS_s = sum(PNS_s * weight, na.rm=TRUE),
    PSNS_w = sum(PNS_w * weight, na.rm=TRUE),
    PSNS_sw = sum(PNS_sw *weight, na.rm=TRUE),
    PSNS = replace(PSNS,PSNS == 0 | PSNS > 1, NA),
    PSNS_s= replace(PSNS_s,PSNS_s == 0 | PSNS_s > 1, NA),
    PSNS_w = replace(PSNS_w,PSNS_w == 0 | PSNS_w > 1, NA),
    PSNS_sw = replace(PSNS_sw,PSNS_sw == 0 | PSNS_sw > 1, NA))%>%
  select(id, ctr, ctr_n, yr, mn, PSNS, PSNS_s, PSNS_w, PSNS_sw, MK_N, MK_N_two, cst_tot)%>%
  distinct(ctr_n, ctr, yr, mn, .keep_all = TRUE)        
           
national.level.psns<-data.c %>%
  distinct(.keep_all = TRUE)%>% 
  group_by(ctr, yr, mn)%>%
  mutate(
    nat_vote = sum(pv1, na.rm=TRUE))%>%
  group_by(ctr, yr, mn, cst)%>%
  mutate(
    seat_cst = sum(seat, na.rm=TRUE),
    party_vote_proportion =  pv1/nat_vote)%>%
  arrange(ctr, yr, mn, cst, pty)%>%
  group_by(ctr, yr, mn)%>%
  mutate(
    seat_total = sum(seat, na.rm=TRUE))%>%
  group_by(ctr, yr, mn, pty)%>%
  mutate(
    seat_contest = sum(seat_cst, na.rm=TRUE),
    seat_proportion = seat_contest/seat_total,
    local_E =  party_vote_proportion * seat_proportion)%>%
  group_by(ctr, yr, mn)%>%
  mutate(
    local_E = sum(local_E, na.rm=TRUE))%>%
  select(ctr, ctr_n, yr, mn, local_E)%>%
  distinct(ctr_n, ctr, yr, mn, .keep_all = TRUE)     
    
national.level<-national.level.nat2 %>% 
  left_join(national.level.nat)%>%
  right_join(national.level.mk_n)%>%
  left_join(national.level.psns)%>%
  rename(inflation1=Cox, inflation2=MK_I, inflation3=MK_I_w, inflation4=MK_N, nvvi=indicator,
           ENP_nat=ENEP_nat, ENP_avg=ENEP_avg, ENP_wght=ENEP_wght)%>%
  mutate(
    nvvi = ifelse(is.na(nvvi), 0, nvvi),
    inflation1 = ifelse(inflation1==0, NA, inflation1),
    inflation2 = ifelse(inflation2==0, NA, inflation2),
    inflation3 = ifelse(inflation3==0, NA, inflation3),
    inflation4 = ifelse(inflation4==0, NA, inflation4),
    local_E = ifelse(local_E==0|local_E>1, NA, local_E),
    ENP_nat = ifelse(ctr == 144 & yr == 1947 | ctr == 144 & yr == 1952 | ctr == 144 & yr == 1956 | ctr == 144 & yr == 1960 | ctr == 144 & yr == 1965 | ctr == 144 & yr == 1970 | ctr == 144 & yr == 1977, NA, ENP_nat),
    ENP_avg = ifelse(ctr == 144 & yr == 1947 | ctr == 144 & yr == 1952 | ctr == 144 & yr == 1956 | ctr == 144 & yr == 1960 | ctr == 144 & yr == 1965 | ctr == 144 & yr == 1970 | ctr == 144 & yr == 1977, NA, ENP_avg),
    ENP_wght = ifelse(ctr == 144 & yr == 1947 | ctr == 144 & yr == 1952 | ctr == 144 & yr == 1956 | ctr == 144 & yr == 1960 | ctr == 144 & yr == 1965 | ctr == 144 & yr == 1970 | ctr == 144 & yr == 1977, NA, ENP_wght))%>%
    right_join(data.grid)%>%
    distinct(ctr_n, ctr, yr, mn, .keep_all = TRUE)%>% 
    select(id, ctr_n, ctr, yr, mn, nvvi, ENP_nat, ENP_avg, ENP_wght, inflation1, inflation2, inflation3, 
          inflation4, PSNS, PSNS_s, PSNS_w, PSNS_sw, local_E, cst_tot)%>%
    arrange(ctr_n, ctr, yr, mn, nvvi, ENP_nat, ENP_avg, ENP_wght, inflation1, inflation2, inflation3, 
            inflation4, PSNS, PSNS_s, PSNS_w, PSNS_sw, local_E)
write.csv(national.level, paste0(outputFolder, "national.level.csv"))
print(paste("National Level Dataset has been sucessfully saved to the folder ", getwd(), sep=""), quote=FALSE)


#******************************************
#*****Party Nationalization Measures*******
#*******Constituency Level Dataset*********
#******************************************
print("Computing Constituency Level Dataset...", quote=FALSE)

cst.level<-data.b %>%
  distinct(ctr, yr, mn, cst, pty, .keep_all = TRUE)%>%
  group_by(ctr, yr, mn, cst) %>% 
  mutate(
    cst_tot = length(unique(cst)),
    new_vv1 = sum(pv1,na.rm=TRUE),
    share2 = (pv1/new_vv1)^2,
    denom = sum(share2),
    ENP_cst = 1/denom,
    cvvi = 0,
    cvvi = ifelse(vv1!= new_vv1, 1, cvvi))%>%
  select(ctr, yr, mn, cst, cst_n,  ENP_cst, cvvi)%>%
  distinct(ctr, yr, mn, cst, .keep_all = TRUE)%>%
  left_join(national.level)%>%
  mutate(
    inflation5 = (ENP_nat - ENP_cst)/ENP_cst,
    ENP_cst=replace(ENP_cst, yr < 1834 & ctr == 840, NA))%>%
  right_join(data.grid)%>%
  select(id, ctr_n, ctr, yr, mn, cst_n, cst, nvvi, cvvi, ENP_cst, ENP_nat, ENP_avg,
          ENP_wght, inflation1, inflation2, inflation3, inflation4, inflation5, 
          PSNS, PSNS_s, PSNS_w, PSNS_sw, local_E, cst_tot)%>%
  arrange(ctr_n, ctr, yr, mn, cst_n, cst, nvvi, cvvi, ENP_cst, ENP_nat, ENP_avg,
            ENP_wght, inflation1, inflation2, inflation3, inflation4, inflation5, 
            PSNS, PSNS_s, PSNS_w, PSNS_sw, local_E)%>%
  distinct(ctr, yr, mn, cst, .keep_all = TRUE)
write.csv(cst.level, paste0(outputFolder, "cst.level.csv"))
print(paste("Constituency Level Dataset has been sucessfully saved to the folder ", getwd(), sep=""), quote=FALSE)
cat("The entire computation took ", proc.time()[1]-start.timer[1], "secs \n")
print("Done!", quote=FALSE)
}


