# Load libraries

# e1071 contains the 'svm' function
library(e1071)
# With clusterSim we can create the moons dataset
library(clusterSim)
# To export files
library(svglite)
# Tidyverse for data wrangling
library(tidyverse)

source('functions.R')

# Define a boolean, which will create plots if set to TRUE
# NB: plots are created in a `plots` subdirectory, errors will be thrown your
# way if this directory does not exist!
e <- TRUE

# Load the iris dataset with two species: versicolor and setosa.
# These species are easily linearly seperable on their petal characteristics.

df <- iris %>%
  filter(Species %in% c("versicolor", "setosa")) %>%
  select(Petal.Length, Petal.Width, Species)

# Declare the formula we are going to use many times in this script

f <- as.formula(Species ~ Petal.Length + Petal.Width)



# Plot 1: an overview of our dataset

plot <- ggplot(df, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point(size = 4) +
  coord_fixed(ratio = 1) + expand_limits(x = 0, y = 0)
plot
if (e) { savesvg(file = '1_dataset', plot = plot); }



# Plot 2: one of the many possible separating lines

plot <- ggplot(df, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point(size = 4) +
  coord_fixed(ratio = 1) + expand_limits(x = 0, y = 0) +
  geom_abline(intercept = 1, slope = -.05)
plot
if (e) { savesvg(file = '2_one_line', plot = plot); }



# Plot 3: more possible separating lines

plot <- ggplot(df, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point(size = 4) +
  coord_fixed(ratio = 1) + expand_limits(x = 0, y = 0) +
  geom_abline(intercept = 2.1, slope = -.8) +
  geom_abline(intercept = 1, slope = -.05) +
  geom_abline(intercept = 12, slope = -5)
plot
if (e) { savesvg(file = '3_several_lines', plot = plot); }



# Plot 4: the SVM model

# Let's train an SVM model
svm.model <- svm(f, data = df,
                 type = "C-classification", cost = 100, 
                 scale = F, kernel = "linear")
# Mark the columns that are Supporting Vectors
df$is_sv <- rownames(df) %in% rownames(svm.model$SV)

# Generate a plot of this SVM model
plot <- ggplot(df, aes(Petal.Length, Petal.Width, col = Species, shape = is_sv)) +
  geom_point(size = 4) +
  boundary_equation(svm.model) +
  guides(shape = F) +
  coord_fixed(ratio = 1) + expand_limits(x = 0, y = 0)
plot
if (e) { savesvg('4_linear_svm', plot); }



# Plot 5: the same SVM model, based on just the SV's

# Generate an SVM model only with the data points that were Supporting Vectors
# in the previous model
df_lim <- df[df$is_sv, 1:3]
svm.model <- svm(f, data = df_lim,
                 type = "C-classification", cost = 100, 
                 scale = F, kernel = "linear")
# Mark the columns that are Supporting Vectors
df_lim$is_sv <- rownames(df_lim) %in% rownames(svm.model$SV)

# Generate a plot of this SVM model
plot <- ggplot(df_lim, aes(Petal.Length, Petal.Width, col = Species, shape = is_sv)) +
  geom_point(size = 4) +
  boundary_equation(svm.model) +
  guides(shape = F) +
  coord_fixed(ratio = 1) +
  expand_limits(x = c(0, df$Petal.Length), y = c(0, df$Petal.Width))
plot
if (e) { savesvg('5_linear_svm_with_two_vectors', plot); }



# Plot 6 and 7: SVM susceptiblity to outliers

# Generate an outlier to screw up the model
df_outlier <- rbind(df[,1:3], list(4.75, .5, 'setosa'))

# Train an SVM with C = 100
svm.model <- svm(f, data = df_outlier,
                 type = "C-classification", cost = 100, 
                 scale = F, kernel = "linear")
# Mark the columns that are Supporting Vectors
df_outlier$is_sv <- rownames(df_outlier) %in% rownames(svm.model$SV)
# Generate a plot of this SVM model
plot <- ggplot(df_outlier, aes(Petal.Length, Petal.Width, col = Species)) +
  geom_point(size = 4) +
  boundary_equation(svm.model) +
  guides(shape = F) +
  coord_fixed(ratio = 1) + expand_limits(x = 0, y = 0)
plot
if (e) { savesvg('6_outlier_hard_margin', plot); }

# Train an SVM with C = 1
svm.model <- svm(f, data = df_outlier,
                 type = "C-classification", cost = 1, 
                 scale = F, kernel = "linear")
# Mark the columns that are Supporting Vectors
df_outlier$is_sv <- rownames(df_outlier) %in% rownames(svm.model$SV)
# Generate a plot of this SVM model
plot <- ggplot(df_outlier, aes(Petal.Length, Petal.Width, col = Species, shape = is_sv)) +
  geom_point(size = 4) +
  boundary_equation(svm.model) +
  guides(shape = F) +
  coord_fixed(ratio = 1) + expand_limits(x = 0, y = 0)
plot
if (e) { savesvg('7_outlier_soft_margin', plot); }



# Plot 8: A properly scaled dataset

# Scale the data
df_scaled <- data.frame(scale(df[,1:2]), df[3])
# Train the model
svm.model <- svm(f, data = df_scaled,
                 type = "C-classification", cost = 100, 
                 scale = F, kernel = "linear")
# Mark the columns that are Supporting Vectors
df_scaled$is_sv <- rownames(df_scaled) %in% rownames(svm.model$SV)
# Generate a plot of this SVM model
plot <- ggplot(df_scaled, aes(Petal.Length, Petal.Width, col = Species, shape = is_sv)) +
  geom_point(size = 4) +
  boundary_equation(svm.model) +
  guides(shape = F) +
  coord_fixed(ratio = 1) + expand_limits(x = 0, y = 0)
plot
if (e) { savesvg('8_scaled_data', plot); }



# Plot 9: An improperly scaled dataset

# Mess up the scale
df_bad_scale <- mutate(df_scaled, Petal.Length = Petal.Length * 10)
# Train a model
svm.model <- svm(f, data = df_bad_scale,
                 type = "C-classification", cost = 100, 
                 scale = F, kernel = "linear")
# Mark the columns that are Supporting Vectors
df_bad_scale$is_sv <- rownames(df_bad_scale) %in% rownames(svm.model$SV)
# Generate a plot of this SVM model
plot <- ggplot(df_bad_scale, aes(Petal.Length, Petal.Width, col = Species, shape = is_sv)) +
  geom_point(size = 4) +
  boundary_equation(svm.model) +
  guides(shape = F) +
  coord_fixed(ratio = 10) + expand_limits(x = 0, y = 0)
plot
if (e) { savesvg('9_badly_scaled', plot); }



###################
# Next dataset: one-dimensional dataset, polynomial kernel
#

one_dim <- data.frame(x = seq(-4,4,1), y = 0, class = c(rep('a',2), rep('b',5), rep('a',2)))


# Plot 10: overview of this dataset

plot <- ggplot(one_dim, aes(x, y, colour = class)) +
  geom_point(size = 4) + ylim(-1, 16) + xlim(-5,5) + coord_fixed(ratio = .5)
plot
if (e) { savesvg('10_one_dim', plot); }



# Plot 11: add the x^2 transformation

one_dim$y <- one_dim$x^2

plot <- ggplot(one_dim, aes(x, y, colour = class)) +
  geom_point(size = 4) + ylim(-1,16) + xlim(-5,5) + coord_fixed(ratio = .5)
plot
if (e) { savesvg('11_two_dims', plot); }



# Plot 12: an SVM model on the transformed dataset

# Train SVM
svm.model <- svm(class ~ x + y, data = one_dim,
                 type = "C-classification", cost = 100, 
                 scale = F, kernel = "linear")
# Mark SV's
one_dim$is_sv <- rownames(one_dim) %in% rownames(svm.model$SV)
# Plot
plot <- ggplot(one_dim, aes(x, y, col = class, shape = is_sv)) +
  geom_point(size = 4) +
  boundary_equation(svm.model) +
  guides(shape = F) +
  coord_fixed(ratio = .5) + ylim(-1,16) + xlim(-5,5)
plot
if (e) { savesvg('12_two_dims_svm', plot); }



# Plot 13: the same data, but a new type of plot

decisionplot(svm.model, one_dim, "class")
if (e) { svg('plots/13_two_dims_decision.svg'); decisionplot(svm.model, one_dim, "class"); dev.off(); }



# Plot 14: plot the SVM with a polynomial kernel

# Define the dataset with y = 0
one_dim <- data.frame(x = seq(-4,4,1), y = 0, class = c(rep('a',2), rep('b',5), rep('a',2)))
# Train SVM
svm.model <- svm(class ~ x + y, data = one_dim,
                 type = "C-classification", cost = 100, 
                 scale = F, kernel = "polynomial", degree = 2)
# Plot
decisionplot(svm.model, one_dim, "class")
if (e) { svg('plots/14_one_dim_kernel.svg'); decisionplot(svm.model, one_dim, "class"); dev.off(); }



###################
# Next dataset: moons
#

# Generate data
stm <- shapes.two.moon(180)
moons <- data.frame(stm$data, stm$clusters)
names(moons) <- c("x", "y", "class")



# Plot 15: overview of the dataset

with(moons, plot(x,y,col=class+1))
if (e) { svg('plots/15_moons.svg'); with(moons, plot(x,y,col=class+1)); dev.off(); }



# Plot 16: polynomial kernel

svm.model <- svm(class ~ x + y, data = moons,
                 type = "C-classification", cost = 1, gamma = 1,
                 scale = F, kernel = "polynomial", degree= 6, coef0 = 100)
decisionplot(svm.model, moons, "class")
if (e) { svg('plots/16_moons_poly.svg'); decisionplot(svm.model, moons, "class"); dev.off(); }



# Plot 17: radial kernel

svm.model <- svm(class ~ x + y, data = moons,
                 type = "C-classification", cost = 1, gamma = 1,
                 scale = F, kernel = "radial")
decisionplot(svm.model, moons, "class")
if (e) { svg('plots/17_moons_radial.svg'); decisionplot(svm.model, moons, "class"); dev.off(); }



# Plot 18: gamma = 50

svm.model <- svm(class ~ x + y, data = moons,
                 type = "C-classification", cost = 1, gamma = 50,
                 scale = F, kernel = "radial")
decisionplot(svm.model, moons, "class")
if (e) { svg('plots/18_moons_radial_high_gamma.svg'); decisionplot(svm.model, moons, "class"); dev.off(); }



# Plot 19: gamma = 1/10

svm.model <- svm(class ~ x + y, data = moons,
                 type = "C-classification", cost = 1, gamma = .01,
                 scale = F, kernel = "radial")
decisionplot(svm.model, moons, "class")
if (e) { svg('plots/19_moons_radial_low_gamma.svg'); decisionplot(svm.model, moons, "class"); dev.off(); }
