



percentage.table <- function(x, digits = 2){
  tab <- table(x)
  percentage.tab <- 100*tab/(sum(tab))
  rounded.tab <- round(x = percentage.tab, digits = digits)
  return(rounded.tab)
}


plot_barchart <- function(data, x_var, y_var, x_label, y_label, plot_title, reorder_x = TRUE) {
  if (reorder_x) {
    data <- data %>%
      mutate({{ x_var }} := reorder({{ x_var }}, -{{ y_var }}))
  }
  
  ggplot(data, aes(x = {{ x_var }}, y = {{ y_var }})) +
    geom_bar(stat = "identity", fill = "lightblue") +
    geom_text(aes(label = round({{ y_var }}, 2)), vjust = -0.5, size = 3) +
    labs(title = plot_title,
         x = x_label,
         y = y_label) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid = element_blank())
}



engagement.model <- function(dt, outcome.name, input.names, model.type){
  res <- fit.model(dt = dt, outcome.name = outcome.name, input.names = input.names, model.type = model.type)
  return(res)
}

fit.model <- function(dt, outcome.name, input.names, model.type, digits = 3){
  library(formulaic)
  the.formula <- create.formula(outcome.name = outcome.name, input.names = input.names, dat = dt, reduce = T)
  
  if(model.type == "logistic"){
    mod <- glm(formula = the.formula, family = "binomial", data = dt)
    mod.summary <- logistic.regression.summary(glm.mod = mod, digits = digits)
  }
  if(model.type == "linear"){
    mod <- lm(formula = the.formula, data = dt)
    mod.summary <- linear.regression.summary(lm.mod = mod, digits = digits)
  }
  mod.summary.rounded <- mod.summary[, lapply(X = .SD, FUN = "round.numerics", digits = digits)]
  return(mod.summary.rounded)
}

logistic.regression.summary <- function(glm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  glm.coefs <- as.data.table(summary(glm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = glm.coefs, old = "rn", new = "Variable")
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  glm.coefs[, Odds.Ratio := exp(Estimate)]
  glm.coefs[, OR.Lower.95 := exp(Estimate - z * `Std. Error`)]
  glm.coefs[, OR.Upper.95 := exp(Estimate + z * `Std. Error`)]
  
  return(glm.coefs[])
}



linear.regression.summary <- function(lm.mod, digits = 3, alpha = 0.05){
  library(data.table)
  lm.coefs <- as.data.table(summary(lm.mod)$coefficients, keep.rownames = TRUE)
  setnames(x = lm.coefs, old = "rn", new = "Variable")
  
  z <- qnorm(p = 1-alpha/2, mean = 0, sd = 1)
  lm.coefs[, Coef.Lower.95 := Estimate - z * `Std. Error`]
  lm.coefs[, Coef.Upper.95 := Estimate + z * `Std. Error`]
  return(lm.coefs)
}

round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}



# load afinn dictionary
afinn = read.table('https://raw.githubusercontent.com/pseudorational/data/master/AFINN-111.txt',
                   header = F,
                   quote="",
                   sep = '\t',
                   col.names = c('word','value'), 
                   encoding='UTF-8',
                   stringsAsFactors = F)
afinn <- as.data.table(afinn)

afinn.score <- function(review){
  rev.token <- review[, .( word = trimws(unlist(strsplit(tolower(review), "\\W+")))), by = review.id]
  rev.token <- rev.token[!word %in% stop_words$word]
  
  review_tokens <- merge(rev.token, afinn, by.x = "word", by.y = "word", all.x = TRUE)
  review_tokens <- review_tokens[!is.na(value)]
  review_scores <- review_tokens[, .(score = sum(value, na.rm = TRUE)), by = review.id]
  return(review_scores)
}

word.freq <- function(review_scores, data, positive){
  if (positive == TRUE) {
    reviews_id <- review_scores[score > 0, "review.id"]
  } 
  else if (positive != TRUE) {
    reviews_id <- review_scores[score < 0, "review.id"]
  }
  
  # select for only positive reviews. join with original table with reviews
  reviews <- subset(data, review.id %in% reviews_id$review.id)
  
  # Tokenize positive reviews and count word frequencies
  review.word <- reviews[ , .(word = unlist(strsplit(tolower(review), "\\W+")))]
  review.word <- review.word[!word %in% stop_words$word]
  
  # Sort the word frequencies in descending order
  word.freq <- review.word[, .N, by=word][order(-N)]
  return(word.freq)
}

skills_vector <- function(data){
  skill_list <- unlist(strsplit(data$skills, ", "))
  return(skill_list)
}

count_skills <- function(data){
  skill_freq <- tibble(skill = data) %>%
    count(skill, sort = TRUE)
  return(skill_freq)
}

freq_by_skill <- function(data){
  skill_counts_tab <- tibble(skill = data) %>%
    count(skill, sort = TRUE)
  skill_counts_tab <- as.data.table(skill_counts_tab)
  skill_counts_tab <- skill_counts_tab[1:10, .(Skill = skill, Frequency = n)]
  
  return(skill_counts_tab)
}