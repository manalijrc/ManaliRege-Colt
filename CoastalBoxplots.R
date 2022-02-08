# test for norm distribution 
qqnorm(CoastData44$Minimum.Frequency..kHz.)
qqline(CoastData44$Minimum.Frequency..kHz.)
shapiro.test(CoastData44$Maximum.Frequency..kHz.)

qqnorm(CoastData44$Maximum.Frequency..kHz.)
qqline(CoastData44$Maximum.Frequency..kHz.)
shapiro.test(CoastData44$Minimum.Frequency..kHz.)

qqnorm(CoastData44$Peak.Frequency..kHz.)
qqline(CoastData44$Peak.Frequency..kHz.)
shapiro.test(CoastData44$Peak.Frequency..kHz.)

qqnorm(CoastData44$Delta.Frequency..kHz.)
qqline(CoastData44$Delta.Frequency..kHz.)
shapiro.test(CoastData44$Delta.Frequency..kHz.)

qqnorm(CoastData44$Fundamental.frequency.Start..kHz.)
qqline(CoastData44$Fundamental.frequency.Start..kHz.)
shapiro.test(CoastData44$Fundamental.frequency.Start..kHz.)

qqnorm(CoastData44$Fundamental.frequency.End..kHz.)
qqline(CoastData44$Fundamental.frequency.End..kHz.)
shapiro.test(CoastData44$Fundamental.frequency.End..kHz.)

qqnorm(CoastData44$Duration..s.)
qqline(CoastData44$Duration..s.)
shapiro.test(CoastData44$Duration..s.)

CoastData44$Recording<- NULL

CoastData96$Recording<- NULL



# Compare Nic and ES parameters in box plots
# Max Freq
MaxFreqBox.plot<-ggplot(data=CoastData44, 
                        mapping=aes(x=Population,y=Maximum.Frequency..kHz.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(MaxFreqBox.plot)

# Mann whitney u test
wilcox.test(CoastData44$Maximum.Frequency..kHz.~CoastData44$Population)

# Min Freq
MinFreqBox.plot<-ggplot(data=CoastData44, 
                        mapping=aes(x=Population,y=Minimum.Frequency..kHz.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(MinFreqBox.plot)

wilcox.test(CoastData44$Minimum.Frequency..kHz.~CoastData44$Population)

# Peak Freq
PFreqBox.plot<-ggplot(data=CoastData44, 
                      mapping=aes(x=Population,y=Peak.Frequency..kHz.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(PFreqBox.plot)

wilcox.test(CoastData44$Peak.Frequency..kHz.~CoastData44$Population)

# Duration
DurBox.plot<-ggplot(data=CoastData44, 
                    mapping=aes(x=Population,y=Duration..s.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(DurBox.plot)

wilcox.test(CoastData44$Duration..s.~CoastData44$Population)

# Delta frequency 
DFBox.plot<-ggplot(data=CoastData44, 
                   mapping=aes(x=Population,y=Delta.Frequency..kHz.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(DFBox.plot)
wilcox.test(CoastData44$Delta.Frequency..kHz.~CoastData44$Population)
# Start freq
SFBox.plot<-ggplot(data=CoastData44, 
                   mapping=aes(x=Population,y=Fundamental.frequency.Start..kHz.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(SFBox.plot)
wilcox.test(CoastData44$Fundamental.frequency.Start..kHz.~CoastData44$Population)
# end freq
EFBox.plot<-ggplot(data=CoastData44, 
                   mapping=aes(x=Population,y=Fundamental.frequency.End..kHz.,fill=Population)) + 
  geom_boxplot()+
  theme(legend.position="none")

print(EFBox.plot)
wilcox.test(CoastData44$Fundamental.frequency.End..kHz.~CoastData44$Population)