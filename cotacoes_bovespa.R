#Packages
library(tidyverse)
library(scales)
library(BatchGetSymbols)
library(ggthemes)
library(ggpubr)

#Dados
stock1 = BatchGetSymbols('ABEV3.SA', first.date=as.Date('2019-01-12'), last.date=as.Date('2019-10-24'))
stock1 = na.omit(stock1$df.tickers)

ibov = BatchGetSymbols('^BVSP', first.date=as.Date('2019-01-12'), last.date=as.Date('2019-09-20'))
stock2 = na.omit(ibov$df.tickers)

stock3 = BatchGetSymbols('VALE3.SA', first.date=as.Date('2019-01-12'), last.date=as.Date('2019-09-20'))
stock3 = na.omit(stock3$df.tickers)

stock4 = BatchGetSymbols('ELET3.SA', first.date=as.Date('2019-01-12'), last.date=as.Date('2019-09-20'))
stock4 = na.omit(stock4$df.tickers)

#Gráfico dos preços 
p1 =  ggplot(stock1, aes(ref.date, price.close))+
  geom_line(col='blue', lwd=1)+
  #scale_y_discrete(limits=c(10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000))+
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%m"))+
  xlab('')+ 
  ylab('Preço')+
  labs(title = 'ABEV3.SA',
       subtitle = 'Período - 2019/01/12 a 2019/09/20',
       caption = "Elaboração: própria \n Fonte: B3")+
  theme_economist_white()

p2 = ggplot(stock2, aes(ref.date, price.close))+
  geom_line(col='blue', lwd=1)+
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%m"))+
  xlab('')+ 
  ylab('Pontos')+
  labs(title = 'Índice Bovespa',
       subtitle = 'Período - 2019/01/12 a 2019/09/20')+
  theme_fivethirtyeight()

p3 = ggplot(stock3, aes(ref.date, price.close))+
  geom_line(col='blue', lwd=1 )+
  #scale_y_discrete(limits=c(10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000))+
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%m"))+
  xlab('')+ 
  ylab('Preço')+
  labs(title = 'VALE3',
       subtitle = 'Período - 2019/01/12 a 2019/09/20')+
  theme_fivethirtyeight()

p4 = ggplot(stock4, aes(ref.date, price.close))+
  geom_line(col='blue', lwd=1 )+
  #scale_y_discrete(limits=c(10000,20000,30000,40000,50000,60000,70000,80000,90000,100000,110000))+
  scale_x_date(breaks = date_breaks("1 month"),labels = date_format("%m"))+
  xlab('')+ 
  ylab('Preço')+
  labs(title = 'ELET3',
       subtitle = 'Período - 2019/01/12 a 2019/09/20',
       caption = '')+
  theme_fivethirtyeight()

#Unir gráfico em um mesmo quadro
figure1 = ggarrange(p1,p3,p4,p2,
                    labels = c("","","",""))
annotate_figure(figure1,
                top = text_grob("Evolução no Preço das Ações", face = "bold", size = 14),
                bottom = text_grob("Fonte: B3. \n Elaboração própria.",
                                   hjust = 1, x = 1, face = "italic", size = 10))




p <- ggplot(
  stock1, 
  aes(x = stock1$ref.date, y= stock1$price.close)
) +
  geom_line() +
  scale_color_viridis_d() +
  labs(x = "Data", y = "Preço de fechamento")
p1

teste <- p + geom_point() + 
  transition_reveal(stock1$ref.date)
  
