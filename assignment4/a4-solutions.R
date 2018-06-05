library('tidyverse')

set_wdir <- function() {
  library(rstudioapi) 
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

set_wdir()

# FLUXO DE TRÂNSITO EM ESTRADAS NA CALIFÓRINA

# 1
# Carregam os dados. [Coloca o arquivo fluxo-lot.Rdata no diretório de trabalho e executar o comando `load("fluxo-lot.Rdata")`. 
# Verifica que o data.frame `fluxo_lot` aparece no ambiente global.]
load("fluxo-lot.Rdata")
fluxo_lot

# 2
# Existe uma faixa em particular para qual o fluxo de trânsito seja consistentemente maior que nas outras faixas?
ggplot(fluxo_lot, mapping = aes(x = data_hora, y = fluxo, colour = as.factor(faixa)))+
  geom_line()

ggplot(fluxo_lot, mapping = aes(x = as.factor(faixa), y = fluxo)) +
  geom_boxplot()

# Como é possível verificar observando os gráficos plotados, o fluxo de trânsito é maior na faixa 2 apesar da faixa 1 ter maiores picos do que a faixa 1. Isso fica nítido ao oservar o boxplot.

# 3
# Existe uma relação entre fluxo e taxa de lotação?
ggplot(fluxo_lot, mapping = aes(x = lotacao, y = fluxo, colour = as.factor(faixa)))+
  geom_jitter()

ggplot(fluxo_lot, mapping = aes(x = lotacao, y = fluxo, colour = as.factor(faixa)))+
  geom_line()

# Sim, existe uma relação. A medida que o fluxo vai crescendo a taxa de lotação também vai crescendo (na maioria dos dados) até chegar em um ponto que esse crescimento de lotação para e o fluxo diminui para valores de lotação acima de 0.2 aproximadamente.

# 4
# Essa relação varia de acordo com horário do dia ou dia da semana?
ggplot(fluxo_lot, mapping = aes(x = lotacao, y = fluxo, colour = as.factor(faixa)))+
  geom_jitter() +
  facet_wrap(~ dia_semana, nrow = 2)

ggplot(fluxo_lot, mapping = aes(x = lotacao, y = fluxo, colour = as.factor(faixa)))+
  geom_line() +
  facet_wrap(~ dia_semana, nrow = 2)

ggplot(fluxo_lot, mapping = aes(x = lotacao, y = fluxo, colour = as.factor(faixa)))+
  geom_line() +
  facet_wrap(~ dia_semana, nrow = 2)

# 5
# O que está acontecendo?
# Dias de semana tem mais trânsito e consequentemente os carros passam mais tempo em cima do detector)
# Estranho na quinta feira dia 20 de março -> A US-led coalition launches an invasion of Iraq, beginning the Iraq War
# http://edition.cnn.com/2003/WORLD/meast/03/19/sprj.irq.main/


# TEMPERATURAS EM BERKELEY EM JANEIRO
load("janeiro.Rdata")
janeiro_temps

# 1
# Nos últimos seis anos, qual ano teve o janeiro mais quente? Qual ano teve o mais frio?
ggplot(janeiro_temps, mapping = aes(x = dia, y = media, colour = as.factor(ano)))+
  geom_line()

ggplot(janeiro_temps, mapping = aes(y = media, x = as.factor(ano)))+
  geom_boxplot()

# É fácil verificar que o ano de 2009 teve o janeiro mais quente e 2008 o mais frio.


# 2
# O computador ao qual o sensor é conectado estava quebrado para alguns dias em janeiro de um ano. 
# Qual ano foi? Explica como você encontrou a resposta – simplesmente examinando os dados visualmente não é aceitável.

ggplot(janeiro_temps, mapping = aes(x = dia, y = media)) + 
  geom_point() +
  facet_wrap(~ ano, nrow = 2)

# Fazendo uma analise exploratória para agilizar a procura encontramos qu eé no ano de 2008 entre os dias 11 e 18 mais ou menos 




