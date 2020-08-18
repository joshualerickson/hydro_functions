lpearsonIII <- function(data,x, y, plot = TRUE){
  max.x <- tibble(x,y) 
  max.x <- na.omit(max.x)
  max.x <- max.x %>% group_by(y) %>% 
    slice_max(x) %>% 
    distinct(x) %>% arrange(desc(x)) %>% ungroup() %>%  slice_max(x, n = 10) #this removes any potential operator error
  
  if (!nrow(max.x) >= 10) {stop("Warning: Need more data (e.g. years).")}
  
  mean.x <- mean(log10(max.x$x)) #log mean of max Q
  
  sd.x <- sd(log10(max.x$x)) #log standard deviation of max Q
  
  skewness <- format(round(kurtosi(log10(max.x$x)), 1)) #finds the skewness of distribution
  
  lp.table <- logPearson_table # need the log pearson skewness table, z-values won't work
  
  lp.df <- merge(skewness, lp.table) %>% filter(skewness == Skew) 
  lp.df <- t(lp.df)
  lp.df <- lp.df[c(4:10),] #data fitting
  lp.df <- as.numeric(lp.df)
  log.pearson <- mean.x + sd.x*lp.df # equation for log pearson type III
  
  ReOccurence <- c(2,5,10,25,50,100,200)
  Flood.Freq <- data.frame(log.pearson, ReOccurence)
  
  Flood.Freq <- Flood.Freq %>% mutate(Anti_log = 10^log.pearson,#need antilog to read back data
                                      Log_ROI = log10(ReOccurence)) 
  df <- tibble(ReOccurence = seq(2,200,1))
  lm.fit <- glm(Anti_log~ns(log(ReOccurence),4), data = Flood.Freq) 
  tidy_fit <- glance(lm.fit)
  aug <- augment(lm.fit, newdata = df, type.predict = "response")
  
  g1 <- ggplot(data = Flood.Freq, aes(x= ReOccurence, y = Anti_log)) + 
    geom_point(size = 3) + 
    geom_smooth(method = "glm", formula = y ~ ns(log(x), 4)) + 
    expand_limits(y = 0) + 
    scale_x_continuous(breaks = c(2,5,10,25,50,100,200)) + 
    labs(x = "Recoccurrence Intervals", y = "Discharge (cfs)", title = "Log Pearson Type III Flood Frequency") +
    theme_light() + geom_label_repel(aes(label = round(Anti_log, 0)), force = 12)
  
  if (plot == FALSE) {} else {print(g1)}
  return(list(aug, Flood.Freq))
}