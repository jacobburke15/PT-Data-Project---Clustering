library(ggplot2)
library(keras)
library(tidyverse)
library(dplyr)

## need all columns to be of the same type? 
## numeric? 

## need to deal with the missing values (delete, or mean?)
## 3700 approx missing in Visit ID, nothing larger than 
## 600 missing after that - either need to remove, or potentially
## not use the Visit ID's in clustering (think that's better option)


## getting variables of interest for clustering:


ROMSdata1 <- select(ROMSdata, Age, `Sex (1=male, 2=female)`, 
                             `Body Region`, Surgical, 
                              Visits, `Pain Change Scores`, `Outcome Change Scores`,
                              `Chronic Pain (Yes/No)`)

## checking how many missing values in each column 

nas <- function(data) {
  
  h <- array(data = NA, dim = 7)
  for(i in 1:7) {
    a <- sum(is.na(data[,i]))
    h[i] <- a
  }
  return(h)
}

nas(ROMSdata1)

## we have 696 missing for payer Category, but not much else 

ROMSdata1 <- na.omit(ROMSdata1)

## a little over 600 observations are deleted 

## data preprocessing #### 

## need to get all data in the same type 

## now need all categorical variables into matrix of dummy variables

library(fastDummies)

result <- dummy_cols(ROMSdata1)

## removing the character columns for clustering 

result <- select(result, -`Sex (1=male, 2=female)`, -`Body Region`, 
                      -Surgical, - `Chronic Pain (Yes/No)`)


## May want to scale the values towards their respective 
## z-scores (says this is good for k-means)

## did for numerical values and this really lowered 
## within and between sum of squares

rescale_df <- result %>%
  mutate(Age_scal = scale(Age),
         Visits_scal = scale(Visits),
         PCh_scal = scale(`Pain Change Scores`),
         OCh_scal = scale(`Outcome Change Scores`)) %>%
  select(-c(Age, Visits, `Pain Change Scores`, 
                `Outcome Change Scores`))

## this sets each new column into a type matrix, so 
## setting back to type numeric: 

rescale_df$Age_scal <- as.numeric(rescale_df$Age_scal)

rescale_df$Visits_scal <- as.numeric(rescale_df$Visits_scal)

rescale_df$PCh_scal <- as.numeric(rescale_df$PCh_scal)

rescale_df$OCh_scal <- as.numeric(rescale_df$OCh_scal)

############ clustering attempt

set.seed(2020)

clusters <- kmeans(rescale_df, 20)

result <- mutate(result, cluster = as.factor(clusters$cluster))

str(clusters)

## elbow method to get optimal k clusters

kmean_withinss <- function(k) {
  cluster <- kmeans(rescale_df, k)
  return (cluster$tot.withinss)
}

# Set maximum cluster 

max_k <-20

# Run algorithm over a range of k 

wss <- sapply(2:max_k, kmean_withinss)

# Create a data frame to plot the graph

elbow <-data.frame(2:max_k, wss)

# Plot the graph with gglop
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  geom_point() +
  geom_line() +
  labs(x = "K Cluster Number", y = "WSS") +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

## just about after 6 clusters, the drop is fairly low dropping, so let's set to 11 for now 

opt_clusters <- kmeans(rescale_df, 14)


## visualizing attempt

print(opt_clusters)
str(opt_clusters)
library(factoextra)


fviz_cluster(opt_clusters, data = rescale_df)

## way to much overlap

## need to reduce dimensions with autoencoders


#################################### 

K <- keras::backend()

# Parameters --------------------------------------------------------------

batch_size <- 100L
original_dim <- 784L
latent_dim <- 2L
intermediate_dim <- 256L
epochs <- 50L
epsilon_std <- 1.0

# Model definition --------------------------------------------------------

x <- layer_input(shape = c(original_dim))
h <- layer_dense(x, intermediate_dim, activation = "relu")
z_mean <- layer_dense(h, latent_dim)
z_log_var <- layer_dense(h, latent_dim)

sampling <- function(arg){
  z_mean <- arg[, 1:(latent_dim)]
  z_log_var <- arg[, (latent_dim + 1):(2 * latent_dim)]
  
  epsilon <- k_random_normal(
    shape = c(k_shape(z_mean)[[1]]), 
    mean=0.,
    stddev=epsilon_std
  )
  
  z_mean + k_exp(z_log_var/2)*epsilon
}

# note that "output_shape" isn't necessary with the TensorFlow backend
z <- layer_concatenate(list(z_mean, z_log_var)) %>% 
  layer_lambda(sampling)

# we instantiate these layers separately so as to reuse them later
decoder_h <- layer_dense(units = intermediate_dim, activation = "relu")
decoder_mean <- layer_dense(units = original_dim, activation = "sigmoid")
h_decoded <- decoder_h(z)
x_decoded_mean <- decoder_mean(h_decoded)

# end-to-end autoencoder
vae <- keras_model(x, x_decoded_mean)

# encoder, from inputs to latent space
encoder <- keras_model(x, z_mean)

# generator, from latent space to reconstructed inputs
decoder_input <- layer_input(shape = latent_dim)
h_decoded_2 <- decoder_h(decoder_input)
x_decoded_mean_2 <- decoder_mean(h_decoded_2)
generator <- keras_model(decoder_input, x_decoded_mean_2)


vae_loss <- function(x, x_decoded_mean){
  xent_loss <- (original_dim/1.0)*loss_binary_crossentropy(x, x_decoded_mean)
  kl_loss <- -0.5*k_mean(1 + z_log_var - k_square(z_mean) - k_exp(z_log_var), axis = -1L)
  xent_loss + kl_loss
}

vae %>% compile(optimizer = "rmsprop", loss = vae_loss)
