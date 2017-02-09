library(stm)
library(tm)
library(SnowballC)

# set the random number generator seed for reproducibility
set.seed(1)

# read in data (data are not provided to protect privacy of participants)
reviews <- read_csv(...)

# define stop words, "irene", "david" were characters in one of the case studies
rm <- c("irene", "david")

# K is the number of topics to fit the model to
K <- 12


# pre-process text using tools provided in stm package
processed <- textProcessor(reviews$X1, metadata = reviews, onlycharacter = T, striphtml = T, customstopwords = rm, verbose = F)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

# fit the stm, `year` is a categorical variable defining the source of the text as described in the paper
fit <- stm(out$documents, out$vocab, K = K,
           prevalence =~ year, max.em.its = 150,
           data = out$meta, init.type = "Spectral", verbose = F)

# summarise the topics
labelTopics(fit, c(1:K))

# plot topic prevalence across the corpus
plot.STM(fit, type = "summary")

# plot mean difference in topic proportions by `year` (with 95% CIs), such that topics that form a higher proportion of
# Year 2/Year 1 responses are plotted to the right/left of the x-axis; and those evenly distributed are near zero
fx <- estimateEffect(c(1:K) ~ year, fit, meta = out$meta, uncertainty = "Global")
plot.estimateEffect(fx, covariate = "year", topics = c(1:K), model = fit,  
                       method = "difference", cov.value1 = "Year_2", cov.value2 = "Year_1",
                         main = "Year_2 compared to Year_1", verbose.labels = F)

# some words appear more frequently because students tend to restate the question in their response, so these
# were excluded. Care was taken to ensure that the same themes were still represented in roughly the same 
# proportions.

# define stop words
rm <- c("irene", "david", "dementia", "attitude", "dignity",
        "care", "acts", "undermine", "preserve", "autonomy")

# K is the number of topics to fit the model to
K <- 12


processed <- textProcessor(reviews$X1, metadata = reviews, onlycharacter = T, striphtml = T,
                           customstopwords = rm, verbose = FALSE)
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <-out$meta

fit <- stm(out$documents, out$vocab, K = K,
           prevalence =~ y, max.em.its = 150,
           data = out$meta, init.type = "Spectral", verbose = F)



labelTopics(fit, c(1:K))

plot.STM(fit, type = "summary")

fx <- estimateEffect(c(1:K) ~ year, fit, meta = out$meta, uncertainty = "Global")

# labels were added to represent themes identified by investigators
labels <- c("1. Personhood", "2. Dignity", "3. Autonomy", "4. Awakening", "5. Person-centred care", 
            "6. New knowledge", "7. Awareness", "8. Enablement", "9. Ethical Issues", "10. Identity",
            "11. Reflection", "12. Translation of knowledge")

plot.estimateEffect(fx, covariate = "year", topics = c(1:K), model = fit,  
                       method = "difference", cov.value1 = "Year_2", cov.value2 = "Year_1",
                         main = "", labeltype = "custom",
                    custom.labels = labels, width = 23, xlim = c(-0.33, 0.33), ci.level = 0.95,
                    ylab = "Topic number & Theme", xlab = "Difference in topic proportions")

m <- plot.estimateEffect(fx, covariate = "year", topics = c(1:K), model = fit,  
                       method = "pointestimate", cov.value1 = "Year_2", cov.value2 = "Year_1",
                         main = "", verbose.labels = T)

# `m.ci` extracts the means and CIs from the above plot (it is not a generic function)
m.ci <- function(x){
  mean1 <- round(m$means[[x]][2],2)
  mean2 <- round(m$means[[x]][1],2)
  cis1 <- round(m$cis[[x]][c(3,4)], 3)
  cis2 <- round(m$cis[[x]][c(1,2)], 3)
  y <- c(mean1, cis1, mean2, cis2)
  return(y)
}

g <- sapply(1:12, m.ci)
g <- as.data.frame(t(g)) 
g <- cbind(1:12, g)
names(g) <- c("Topic", "Year 1 mean", "Year 1 lower", "Year 1 upper", "Year 2 mean","Year 2 lower", "Year 1 upper")
