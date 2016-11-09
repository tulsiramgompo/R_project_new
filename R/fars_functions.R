
per_cis<-function(x,n){
  p1<-x/n
  se<-sqrt(p1*(1-p1)/n)
  lb <- p1 - 1.96 * se
  ub <- p1 + 1.96 * se
  p2<-p1*100
  p3<-round(p2,1)
  lb1<-lb*100
  lb2<-round(lb1,1)
  ub1<-ub*100
  ub2<-round(ub1,1)
  out<-paste0(p3,"%","(",lb2,"%",",",ub2, "%",") " )
  return(out)
} 


test_trend_ca<-function(drug, data=clean_fars){
  if (drug %in% c("Stimulant", "Alcohol", "Cannabinoid", "Depressant", "Narcotic", "Other")){
    to_test <- clean_fars %>%
      filter(drug_type == drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol <- prop.trend.test(x = to_test$positive,
                                  n = to_test$trials)
    Z <- round(sqrt(as.double(ca_alcohol$statistic)), digits = 2)
    p.value <- round((ca_alcohol$p.value), digits = 3)
    data.frame(Z, p.value)
  } else {
    drug="Alcohol"
    to_test <- clean_fars%>%
      filter(drug_type != drug) %>%
      group_by(year) %>%
      summarize(positive = sum(positive_for_drug, na.rm = TRUE),
                trials = sum(!is.na(positive_for_drug)))
    ca_alcohol2 <- prop.trend.test(x = to_test$positive,
                                   n = to_test$trials)
    Z <- round(sqrt((ca_alcohol2$statistic)), digits =2)
    p.value <- round((ca_alcohol2$p.value),digits = 3)
    data.frame(Z, p.value)
  }
}


test_trend_log_reg <- function(drug, data = clean_vars){ 
  if(drug %in% c("Stimulant", "Alcohol", "Cannabinoid", "Depressant", "Narcotic", "Other"))
  {
    to_test <- clean_fars %>%
      filter(drug_type == drug)
    log_reg <- glm(positive_for_drug ~ year, data = to_test,family = binomial(link = "logit"))
    ab <- summary(log_reg)$coefficients
    Z= round(ab[2,3], digits =2)
    p.value = round(ab[2,4], digits = 3)
    data.frame(Z, p.value)
    
  }else{
    to_test <- clean_fars %>%
      filter(drug_type != "Alcohol")
    log_reg <- glm(positive_for_drug ~ year, data = to_test,
                   family = binomial(link = "logit"))
    summary(log_reg)$coefficients
    ab <- summary(log_reg)$coefficients
    Z= round(ab[2,3], digits = 2)
    p.value = round(ab[2,4], digits = 3)
    data.frame(Z, p.value)
  }
}
