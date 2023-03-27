library(ggplot2)
dat <- as.data.frame(
  cbind(c(126,159,147,241,322,591,661,852,699),
        c(paste("B",1:9,sep=""))))
colnames(dat) <- c("no","fraction")
dat$no <- as.numeric(dat$no)

dat_v002 <- within(dat,
                   fraction <- factor(fraction, 
                                                  levels=names(sort(table(fraction), 
                                                                    decreasing=TRUE))))

ggplot(data=dat_v002, aes(x=fraction,y=no))+
  geom_bar(stat="identity") +
  coord_flip() +
  geom_text(aes(label=no), hjust=1.5,vjust=0.6, color="white", size=4) +
  theme(axis.title.y = element_blank(),axis.text.y = element_text(size=14)) + 
  scale_y_continuous(breaks = seq(0, 1000, by = 100)) +
  labs(y="Number of proteins per fraction")


#