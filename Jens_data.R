
set.seed(1)

data1 <- simMD()
data2 <- Jones %>%
  mutate(subject = 1:nrow(.)) %>%
  gather(reader, value, -subject)


length(unique(data2$reader))

LOAM(data1)
LOAM(data2)

plot(LOAM(data1))
plot(LOAM(data2), CI = "sym")


LOAM(data2, CIpct = 0.95) %>% plot()
LOAM(data2, CIpct = 0.90) %>% plot()


ITI <- read_csv("data/raw/csvITI.csv", col_names = FALSE)
LTL <- read_csv("data/raw/csvLTL.csv", col_names = FALSE)
OTO <- read_csv("data/raw/csvOTO.csv", col_names = FALSE)


ITI <- ITI %>%
  mutate(subject = 1:nrow(.)) %>%
  gather(reader, value, -subject)

LTL <- LTL %>%
  mutate(subject = 1:nrow(.)) %>%
  gather(reader, value, -subject)

OTO <- OTO %>%
  mutate(subject = 1:nrow(.)) %>%
  gather(reader, value, -subject)


LOAM(ITI)
plot(LOAM(ITI), CI = "asym")

plot(LOAM(LTL), CI = "asym")

plot(LOAM(OTO), CI = "asym")




LOAM(OTO)



























