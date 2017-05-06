library(readxl)
library(gpairs)
library(psych)
library(ggplot2)

file <- list.files(pattern = ".xlsx")


data <- read_excel(file, sheet = 2)
df <- data[,-c(1:2)]

#Getting eigen values
df <- scale(df, center = TRUE, scale = TRUE)
df_cor <- cor(df)
eig <- eigen(df_cor)
eig$values

#Try to extract PCs from scratch
eig.t <- t(eig$vectors)
df.scaled.t <- t(df)
df.new <- eig.t %*% df.scaled.t
df.new.t <- t(df.new)

principal <- princomp(df, cor = TRUE)

pca_components<- -principal$loadings[,1:41]

p <- ggplot(pca_components, aes(x = pca_components[,1], y = pca_components[,2], label = row.names(pca_components)))+geom_point()
p + coord_cartesian(xlim = c(-0.5,0.5), ylim = c(-0.5,0.5)) + geom_text()

p

principal2 <- principal(df_cor, nfactors = 4)


fa_base <- fa(df_cor, fm = "mle", n.obs = 78, nfactors = 4)

fa.diagram(fa_base)

fa_rotated <- fa(df_cor, fm = "mle", n.obs = 78, nfactors = 4, rotate = "varimax")

fa.diagram(fa_rotated)
plot(fa_rotated)

fa.parallel(df)

vss(df)

eig <- eigen(df_cor)
summary(eig)