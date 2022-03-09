#### Figures for CAnD3 presentation 

library(ggplot2) 
library(ggforce)
library(scales)
library(ggthemes)
library(viridis)
library(viridisLite)
library(dplyr)
library(forcats)
library(showtext)
library(magick)


font_add_google("Montserrat", "Montserrat")
showtext_auto()



#### Venn diagram ----

venn_ds <- data.frame(x = c(0, 2, -2),
                      y = c(2.5, -0.5, -0.5),
                      labels = c('Math & Stats', 'Domain \nKnowledge', 'Programming')) 

ds_fig <- ggplot(data = venn_ds, aes(x0 = x, y0 = y, r = 2.5, fill = labels)) +
  geom_circle(alpha = .3, size = 1, colour = 'grey') +
  coord_fixed() +
  theme_void() + 
  theme(legend.position = 'bottom')+
  labs(fill = NULL) +
  theme(legend.position = "none") + 
  scale_fill_viridis(option = "D", discrete = TRUE) +
  annotate("text", x = venn_ds$x, y = venn_ds$y, label = venn_ds$labels, size = 8)

ggsave(ds_fig, filename = "figs/ds_fig.png")


#### Respondent map ----

cols <- viridis(3)
cols <- substr(cols, 0, 7)

respondent_fig <- highcharter::highchart(type = "map") %>%
  highcharter::hc_add_series_map(highcharter::worldgeojson,
                                 df %>% 
                                   filter(country != "I do not wish to disclose my location" |
                                            country != "Other") %>% 
                                   group_by(country) %>% 
                                   summarise(n = n()) %>%
                                   mutate(freq = round(n / sum(n),2)) %>% 
                                   ungroup() %>%
                                   mutate(iso2 = countrycode::countrycode(country, origin="country.name", destination="iso2c")),
                                 value = "freq", joinBy = "iso2") %>%
  highcharter::hc_title(text = "Respondents by country") %>%
  highcharter::hc_colors(cols)

# saved manually for simplicity


#### Demographics fig ----

demographics_fig <- df %>% 
  select(gender, age) %>% 
  droplevels() %>% 
  ggplot(aes(x = age, fill = gender)) + 
  geom_bar(position = "fill") + 
  labs(x = "age categories", 
       y = "Percentage", 
       title = "Gender groups by age categories") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1)) +
  theme_minimal() +
  #theme(text = element_text(family = "Monserrat")) +
  scale_fill_viridis(option = "D", discrete = TRUE)

ggsave(demographics_fig, filename = "figs/demographics_fig.png", width = 6, height = 4)


#### Position fig ----

position_factor = 
  forcats::fct_collapse(df$position, 
                        "Business Analyst"= "Business Analyst", 
                        "Data Analyst" = "Data Analyst",
                        "Data Engineer" = "Data Engineer", 
                        "Data Scientist" = "Data Scientist",
                        "Database Engineer" = "DBA/Database Engineer", 
                        "Machine Learning Engineer" = "Machine Learning Engineer",
                        "Product Manager" = "Product Manager",
                        "Not employed" = "Currently not employed", 
                        "Other" = "Other", 
                        "Program/Project Manager" = "Program/Project Manager",
                        "Research Scientist" = "Research Scientist",
                        "Software Engineer" = "Software Engineer",
                        "Statistician" = "Statistician",
                        "Student" = "Student",
                        "Other" = "Other"
  )


position_fig <- data.frame(fct_count(position_factor)) %>% 
  mutate(prop = round(n/sum(n),2)) %>% 
  ggplot(aes(x = reorder(f, -prop), y = prop, fill = f)) + geom_bar(stat = "identity") + 
  theme_minimal() + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 1)) + 
  ggtitle("Type of position") + 
  labs(x = "Position", 
       y = "Percentage") + 
  theme(legend.position = "none") + 
  scale_fill_viridis(option = "D", discrete = TRUE) +
  geom_text(aes(x = f, 
                y = prop + 0.05, label = round(prop, 2))) + coord_flip()


ggsave(position_fig, filename = "figs/position_fig.png")


#### PhD fig ----

new_factor = 
  forcats::fct_collapse(df$edu, 
                        "Bachelor's"= "Bachelor’s degree", 
                        "Master's" = "Master’s degree",
                        PhD = "Doctoral degree", 
                        Highschool = "No formal education past high school", 
                        "Professional doctorate" = "Professional doctorate", 
                        "Some college/uni" = "Some college/university study without earning a bachelor’s degree",
                        Missing = c("I prefer not to answer"))

phdtxt = "Only 11% of self-identified data scientists hold a PhD!"

education_fig <- data.frame(fct_count(new_factor)) %>% 
  mutate(prop = round(n/sum(n),2), 
         adv = ifelse(f == "PhD", TRUE, FALSE)) %>% 
  ggplot(aes(x = reorder(f, -prop), y = prop, fill = adv)) + geom_bar(stat = "identity") + 
  theme_bw() + 
  geom_label(data = data.frame(x = 1.5, y = .5, label = phdtxt), 
             aes(x = x, y = y, label = phdtxt), 
             hjust = 0, 
             lineheight = 8, 
             inherit.aes = FALSE,  
             label.size = NA) +
  geom_curve(data = data.frame(x = 3, y = .45, xend = 3, yend = .25), 
             mapping = aes(x = x, y = y, xend = xend, yend = yend), 
             colour = "#2D708EFF", 
             size = 1.5, 
             curvature = -0.1, 
             arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
             inherit.aes = FALSE) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1)) + 
  ggtitle("Highest level of education") + 
  theme_minimal() +
  labs(x = "Highest level of education", 
       y = "Percentage") +
  theme(legend.position = "none") + 
  scale_fill_viridis(option = "D", discrete = TRUE) +
  geom_text(aes(x = f, 
                y = prop + 0.05, label = round(prop, 2)))

ggsave(education_fig, filename = "figs/education_fig.png")

#### Experience figs ---- 

coding_factor = 
  forcats::fct_collapse(df$code_yrs, 
                        "< 1 year"= "< 1 years", 
                        "1-3 years" = "1-3 years",
                        "3-5 years" = "3-5 years", 
                        "5-10 years" = "5-10 years", 
                        "10-20 years" = "10-20 years", 
                        "20+ years" = "20+ years", 
                        "No coding experience" = "I have never written code") 


coding_text = "Most people have been coding for only 1-3 years!"

coding_fig <- data.frame(fct_count(coding_factor)) %>% 
  mutate(prop = round(n/sum(n),2), 
         yr13 = ifelse(f == "1-3 years", TRUE, FALSE), 
         f = factor(f, levels = c("< 1 year", "1-3 years", "3-5 years", "5-10 years", "10-20 years", "20+ years", "No coding experience"))) %>%
  filter(f != "Missing" & f != "I have never written code") %>% 
  ggplot(aes(x = f, y = prop, fill = yr13)) + geom_bar(stat = "identity") + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 1)) + 
  ggtitle("Number of years coding") + 
  labs(x = "Number of years", 
       y = "Percentage") +
  theme_minimal() + 
  theme(legend.position = "none") + 
  geom_text(aes(x = f, 
                y = prop + 0.05, label = round(prop, 2))) +
  geom_curve(data = data.frame(x = 3, y = .60, xend = 2, yend = .4), 
             mapping = aes(x = x, y = y, xend = xend, yend = yend), 
             colour = "#2D708EFF",
             size = 1.5, 
             curvature = 0.1, 
             arrow = ggplot2::arrow(length = unit(0.01, "npc"), type = "closed"),
             inherit.aes = FALSE) + 
  geom_label(data = data.frame(x = 2, y = .65, label = coding_text), 
             aes(x = x, y = y, label = coding_text), 
             hjust = 0, 
             lineheight = 8, 
             inherit.aes = FALSE,  
             label.size = NA) + 
  scale_fill_viridis(option = "D", discrete = TRUE) 

ggsave(coding_fig, filename = "figs/coding_fig.png")


#### Programming language fig ----

box = "Basic statistical software: Excel, Google sheets \n
Development environments: RStudio, JupyterLab \n
Statistical software: Python, R, SPSS, SAS \n
Business intelligence: Salesforce, Tableau, Spotfire \n
Cloud based: AWS, Azure \n" 

tool_fig <- df %>% 
  mutate(tool = fct_recode(primary_tool, 
                           "Statistical software" = "Advanced statistical software (SPSS, SAS, etc.)",
                           "Basic software" = "Basic statistical software (Microsoft Excel, Google Sheets, etc.)",
                           "BI software" = "Business intelligence software (Salesforce, Tableau, Spotfire, etc.)", 
                           Cloud = "Cloud-based data software & APIs (AWS, GCP, Azure, etc.)",
                           "Dev Environment" = "Local development environments (RStudio, JupyterLab, etc.)",
                           Other = "Other",
                           Missing = "")) %>% 
  count(tool) %>% 
  mutate(prop = round(n/sum(n),2))  %>% 
  ggplot(aes(x = reorder(tool, -prop), y = prop, fill = tool)) + geom_bar(stat = "identity") + 
  theme_minimal() + 
  geom_label(data = data.frame(x = 4.5, y = .30, label = box), 
             aes(x = x, y = y, label = label), 
             hjust  = 0, 
             lineheight = .7, 
             inherit.aes = FALSE, 
             label.size = 0) + 
  geom_text(aes(label = round(prop,2)), hjust = -0.5, position = position_dodge(0.9)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 1)) + 
  ggtitle("Primary tool used at work or school", 
          subtitle = "Multiple choice categories") + 
  labs(x = "Tool", 
       y = "Percentage") +
  theme(legend.position = "none") +
  coord_flip() + 
  scale_fill_viridis(option = "D", discrete = TRUE) 

ggsave(tool_fig, filename = "figs/tool_fig.png")


#### Year machine Learning fig -----

mlyears_fig <- df %>% 
  count(yrs_ML) %>% 
  mutate(prop = round(n/sum(n),2),
         ml = ifelse(yrs_ML == "Under 1 year", TRUE, FALSE)) %>% 
  ggplot(aes(x = reorder(yrs_ML, -prop), y = prop, fill = ml)) + geom_bar(stat = "identity") + 
  theme_minimal() + 
  geom_text(aes(label = round(prop,2)), vjust = -0.5, position = position_dodge(0.9)) + 
  scale_y_continuous(expand = c(0,0), limits = c(0, 1)) + 
  ggtitle("For how many years have you used machine learning methods?") + 
  labs(x = "", 
       y = "Percentage") + 
  theme(legend.position = "none") + 
  annotate("text", x = 5, y = .75, label = "35% of respondents have only been using ML for under a year") + 
  geom_curve(data = data.frame(x = 4, y = .68, xend = 1.5, yend = .40), 
             mapping = aes(x = x, y = y, xend = xend, yend = yend), 
             colour = "#2D708EFF",
             size = 1.5, 
             curvature = -0.1, 
             arrow = ggplot2::arrow(length = unit(0.01, "npc"), type = "closed"),
             inherit.aes = FALSE) + 
  scale_fill_viridis(option = "D", discrete = TRUE) 

ggsave(mlyears_fig, filename = "figs/mlyears_fig.png")


#### Data science transition venn diagram ----

DS = c(0, 0, 1)
Academia = c(0, 1, 1)
x = c(2.7, -2.7, 0)
y = c(-0.6, -0.6, -0.6)
skill = c("GPA, Poster presentation\n Published papers\n Teaching \n Teaching/research assistant", 
          "Portfolio\n Work experience \nTechnical skills \nTeam management \nBusiness accumen", 
          "")

intersection = "Critical thinking \ncommunication \ndomain knowledge \nproblem solving \nintellectual curiosity"

 dt= data.frame(DS, Academia, x, y, skill)

# define circle parameters
df.venn2 = data.frame(x = c(-2.7, 2.7), 
                      y = c(-0.5, -0.5), 
                      labels = c("Data Science", "Academia"))

intersect_fig <- ggplot(df.venn2) +
  geom_circle(aes(x0 = x, y0 = y, r = 3.5, fill = labels), alpha = .2, size = 1, colour = 'grey') +
  coord_fixed() +
  theme_void() +
  scale_y_continuous(limits = c(-6, 6)) +
  labs(fill = NULL) +
  annotate("text", x = dt$x, y = dt$y, label = dt$skill, size = 4.5) +
  annotate("text", x = 0, y = 4.5, label = "Critical thinking communication, domain knowledge, 
           \nproblem solving and intellectual curiosity", size = 5) + 
  geom_curve(data = data.frame(x = 0, y = 3.5, xend = 0, yend = -0.5), 
             mapping = aes(x = x, y = y, xend = xend, yend = yend), 
             colour = "#2D708EFF", 
             size = 1.5, 
             curvature = 0, 
             arrow = ggplot2::arrow(length = unit(0.01, "npc"), type = "closed"),
             inherit.aes = FALSE) +
  scale_fill_viridis(option = "D", discrete = TRUE)

ggsave(intersect_fig, filename = "figs/intersect_fig.png")


#### Conclusion venn diagram ----


venn_ds = data.frame(x = c(0, 2, -2),
                     y = c(2.5, -0.5, -0.5),
                     labels = c('Math & Stat', 'Domain \nKnowledge', 'Programming')) 

dsvenn_fig <- ggplot(data = venn_ds, aes(x0 = x, y0 = y, r = 2.5, fill = labels)) +
  geom_circle(alpha = .3, size = 1, colour = 'grey') +
  coord_fixed() +
  theme_void() + 
  theme(legend.position = 'bottom') +
  scale_fill_manual(values = c('cornflowerblue', 'firebrick',  'gold')) +
  scale_colour_manual(values = c('cornflowerblue', 'firebrick', 'gold'), guide = FALSE) +
  labs(fill = NULL) +
  theme(legend.position = "none") + 
  scale_fill_viridis(option = "D", discrete = TRUE) +
  annotate("text", x = venn_ds$x, y = venn_ds$y, label = venn_ds$labels, size = 8)

ggsave(dsvenn_fig, filename = "figs/dsvenn_fig.png")

#### Employer fears ----

cbind("Undergrad & Masters" = c("No revelant experience", 
                                "Will expect a lot of  hand holding", 
                                "Training is expensive and time consuming", 
                                "New grads tend to get trained and then leave"),
      "PhD" = c("Experience is hyperfocused", 
                "Unaccustomed to working collaboratively",
                "Both under- and over-qualified", 
                "Understimulated by day-to-day")) %>%
  kable(format = "html") %>% 
  kable_styling(bootstrap_options = c("striped", "hover", position = "float_right", 
                                      font_size = 30)) %>% 
  row_spec(0, color = "#453781FF") %>% 
  save_kable(file = "figs/employerfears_fig.png", 
             zoom = 1.5)
