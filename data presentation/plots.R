library(dplyr)
library(ggplot2)
library(stringr)
library(ggthemes)

df <- read.csv('assignment2_data/population_trends.csv', stringsAsFactors = T)

# rename levels
levels(df$region) <- c( "АР Крим","Вінницька", "Волинська",  "Дніпропетровська",  
                       "Донецька",  "Житомирська" , "Закарпатська" ,"Запорізька",
                       "Івано-Франківська",  "Київська",  "Кіровоградська", "Луганська",
                       "Львівська",  "Миколаївська" ,  "м.Київ",  "м.Севастополь", "Одеська", 
                       "Полтавська", "Рівненська", "Сумська",  "Тернопільська", "Харківська", 
                       "Херсонська", "Хмельницька" ,"Черкаська", "Чернівецька","Чернігівська" )
#reorder levels
df$region <- factor(df$region,
                    levels = c( "Вінницька", "Волинська",  "Дніпропетровська",  
                                "Донецька",  "Житомирська" , "Закарпатська" ,"Запорізька",
                                "Івано-Франківська",  "Київська", "м.Київ", "Кіровоградська", "Луганська",
                                "Львівська",  "Миколаївська" , "Одеська", 
                                "Полтавська", "Рівненська", "Сумська",  "Тернопільська", "Харківська", 
                                "Херсонська", "Хмельницька" ,"Черкаська", "Чернівецька","Чернігівська","АР Крим","м.Севастополь" ))

# remove NA
df <- df[!is.na(df$rate),]


# ========== Statistics for 2016 year ==========

# prepare dataset
df_2016 <-df[df$year==2016,]
df_2016$sign <- ifelse(df_2016$rate>=0,1,-1)


ggplot() + geom_bar(data = df_2016,  aes(x = reorder(region, -rate), y = rate, fill = rate), stat = "identity")+
  scale_fill_continuous(low = "#0077FF", high = "#FFA300", breaks = c(-10,0,2),  labels = c(-10,0,2))+
  scale_y_continuous(breaks = seq(-10, 2, 4), minor_breaks = NULL)+
  labs(title = "Рівень приросту/скорочення населення у 2016 році ", 
       subtitle = str_wrap("Приріст обчислено у порівнянні з попереднім 2015 роком на 1000 осіб наявного населення", 80),
       caption = "Дані: Державна служба статистики України")+
  theme_fivethirtyeight()+
  theme(
    text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
    legend.title = element_blank(),
    plot.title = element_text(face = 'bold', size = 18, color = 'grey20'),
    plot.subtitle = element_text(size = 14, face = 'plain', margin = margin(b = 20)),
    line = element_line(color = '#9CAD91', linetype = 'dotted', size = 0.6),
    axis.text.x = element_text(angle = 45, hjust = 1, size=12),
    plot.margin = unit(rep(1,4), 'cm')
  )

ggsave("rate_2016.png", plot = last_plot(), width = 15, height = 10, units = "in")


# ========== All regions dynamics ==========

ggplot(data = df) + 
  geom_hline(yintercept = 0, size = 0.3, color = '#3A3F4A') +
  geom_area(aes(x = year, y = rate,  alpha = 0.4)) +
  geom_line(aes(x = year, y = rate)) +
  facet_wrap(~region)+
  scale_x_continuous(breaks = seq(1990, 2016, 5), labels = c("'90", "'95", "'00", "'05", "'10", "'15"), 
                     minor_breaks = NULL)+
  scale_y_continuous(breaks = seq(-15, 5, 10), minor_breaks = NULL)+
  labs(title = "Динаміка приросту/скорочення населення у 1990 - 2016 роках",
       subtitle = str_wrap("На 1000 осіб наявного населення", 80),
       caption = "Дані: Державна служба статистики України")+
  theme(
    rect = element_rect(fill = '#EAEAEA'),
    text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
    legend.position = 'none',
    axis.text = element_text(size = 12),
    strip.text.x = element_text(size = 16),
    strip.background = element_rect(fill = '#EAEAEA'),
    plot.title = element_text(face = 'bold', size = 20, color = 'grey20'),
    plot.subtitle = element_text(size = 16, face = 'plain', margin = margin(b = 20)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(size = 0.35, linetype = 'dotted', color = '#5D646F'),
    panel.grid.minor = element_blank(),
    plot.margin = unit(rep(1.3,4), 'cm')
    # plot.background = element_rect(fill = 'grey90')
  )

ggsave("rates_history.png", plot = last_plot(), width = 15, height = 10, units = "in")


# ========== Compare my native region with others ==========
df_non_Don <- df[df$region!="Донецька",]
df_Don <- df[df$region=="Донецька",]
df2 <- merge(x = df_non_Don, y = df_Don, by = "year", all.x = T)
df2 <- df2[df2$year<=2013,]

ggplot() + 
  geom_hline(yintercept = 0, size = 0.3, color = '#3A3F4A') +
  geom_area(data = df2, aes(x = year, y = rate.y,  alpha = 0.4, fill = 'red')) +
  geom_line(data = df2, aes(x = year, y = rate.y), col = 'red') +
  geom_area(data = df2, aes(x = year, y = rate.x,  alpha = 0.4)) +
  geom_line(data = df2, aes(x = year, y = rate.x)) +
  facet_wrap(~region.x)+
  scale_x_continuous(breaks = seq(1990, 2016, 5), labels = c("'90", "'95", "'00", "'05", "'10", "'15"), 
                     minor_breaks = NULL)+
  scale_y_continuous(breaks = seq(-15, 5, 10), minor_breaks = NULL)+
  labs(title = str_wrap("Порівняння динаміки приросту/скорочення населення в Донецькій області в 1990 - 2013 роках з іншими областями України", 70),
       subtitle = str_wrap("Донецька область зображена червоним. Приріст обчислено на 1000 осіб наявного населення.", 80),
       caption = "Дані: Державна служба статистики України")+
  theme(
    rect = element_rect(fill = '#EAEAEA'),
    text = element_text(family = 'Ubuntu Condensed', face = 'plain', color = '#3A3F4A'),
    axis.title = element_blank(),
    axis.text = element_text(size = 12),
    strip.text.x = element_text(size = 16),
    strip.background = element_rect(fill = '#EAEAEA'),
    legend.position = 'none',
    panel.grid.major = element_line(size = 0.35, linetype = 'dotted', color = '#5D646F'),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = 'bold', size = 20, color = 'grey20'),
    plot.subtitle = element_text(size = 16, face = 'plain', margin = margin(b = 15)),
    plot.margin = unit(c(1, 1, 1, 1), 'cm')
  )

ggsave("rates_compare_with_native_region.png", plot = last_plot(), width = 15, height = 10, units = "in")
