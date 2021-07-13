library(TSrepr)
library(ggplot2)
library(data.table)
library(cluster)
library(clusterCrit)

library(fda)


data_gam <- repr_matrix(t(weekly.region[,-1]), func = repr_dwt, #
                        args = list(filter = 'haar', level = 2), ##
                        normalise = TRUE, func_norm = norm_z)


data_gam <- repr_matrix(t(weekly.region), func = repr_dwt, #
                        args = list(level = 2), # the filter name (default is "d6")
                        normalise = TRUE, func_norm = norm_z)

data_gam <- repr_matrix(t(weekly.region), func = repr_gam, #args = list(freq = c(48, 48*7)),
                        normalise = TRUE, func_norm = norm_z)

dim(data_gam)


clusterings <- lapply(c(2:7), function(x)
  pam(data_gam, x))

DB_values <- sapply(seq_along(clusterings), function(x) 
  intCriteria(data_gam, as.integer(clusterings[[x]]$clustering),
              c("Davies_Bouldin")))

ggplot(data.table(Clusters = 2:7, DBindex = unlist(DB_values)),
       aes(Clusters, DBindex)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  theme_bw()


# prepare data for plotting
data_plot <- data.table(melt(data.table(class = as.factor(clusterings[[3]]$clustering),
                                        data_gam)))
data_plot[, Time := rep(1:ncol(data_gam), each = nrow(data_gam))]
data_plot[, ID := rep(1:nrow(data_gam), ncol(data_gam))]

# prepare medoids
centers <- data.table(melt(clusterings[[3]]$medoids))
setnames(centers, c("Var1", "Var2"), c("class", "Time"))
#centers$class <- rep(1:4, dim(data_gam)[2])
centers[, ID := class]

# plot the results
ggplot(data_plot, aes(Time, value, group = ID)) +
  facet_wrap(~class, ncol = 2) +
  scale_x_continuous(breaks = c(1:4)*6) +
  geom_line(color = "grey10", alpha = 0.65) +
  geom_line(data = centers, aes(Time, value),
            color = "firebrick1", alpha = 0.80, size = 1.2) +
  #geom_vline(xintercept = 13, color = "dodgerblue2",
  #           size = 0.8, linetype = 5, alpha = 0.8) +
  labs(x = "Duration", y = "Load (normalised)") +
  theme_bw()

## 
weekly.cluster <- as.data.frame(matrix(0,137,2))
                                
weekly.cluster[,1] <- rownames(t(weekly.region[,-1]))
weekly.cluster[,2] <- as.factor(clusterings[[3]]$clustering)
colnames(weekly.cluster) <- c('region', 'cluster')

weekly.summary <- weekly.cluster %>%              
  separate(region, c('region1','region2'), sep = ', ', remove = FALSE) %>% 
  group_by(region2, cluster) %>% 
  summarise(n = n())

weekly.cluster %>% 
  group_by(cluster) %>% 
  summarise(n = n())


## fda

daytime <- 1:23

tempmat <- t(data_gam[which(weekly.cluster$cluster==1),])
tempmat <- t(data_gam[which(weekly.cluster$cluster==2),])
tempmat <- t(data_gam[which(weekly.cluster$cluster==3),])
tempmat <- t(data_gam[which(weekly.cluster$cluster==4),])

tempbasis = create.fourier.basis(c(0,23), 6)
#tempbasis = create.bspline.basis(c(0,23), 9)

tempfd = smooth.basis(daytime, tempmat, tempbasis)$fd

tempfd$fdnames = list("Duration",
                      "Weather Station",
                      "Mean (normalized)")

plot(tempfd, col=1, lty=1)


