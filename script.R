# зчитуємо дані
data <- read.table('C:\\Users\\Razor\\Desktop\\дистанційне навчання\\статистичний аналіз багатовимірних даних\\lab4\\F9.txt')

# зобразимо діаграму розсіювання пар
pairs(data, cex = 0.1)

# застосуємо метод головних компонент
res <- princomp(data)
plot(res)
summary(res)

# виведемо діаграму розсіювання перших двох головних компонент
plot(res$scores[,1:2])

# виведемо попарні діаграми розсіювання перших трьох головних компонент
plot(res$scores[,2:3])
plot(res$scores[,c(1,3)])

# виведемо тривимірну діаграму розсіювання
library(rgl)
plot3d(res$scores[,1:3])

plot3d(res$scores[,2:4])

# ієрархічний кластерний аналіз початкових даних
d <- dist(data, method = 'euclidean')
# дендрограма на евклідових відстанях і методі одного зв'язку
plot(as.dendrogram(hclust(d, method = 'single')), leaflab = 'none', 
     main = 'Euclidean metrics, single linkage')
# дендрограма на евклідових відстанях і методі повного зв'язку
plot(as.dendrogram(hclust(d, method = 'complete')), leaflab = 'none', 
     main = 'Euclidean metrics, complete linkage')
# дендрограма на евклідових відстанях і методі середнього зв'язку
plot(as.dendrogram(hclust(d, method = 'average')), leaflab = 'none', 
     main = 'Euclidean metrics, average linkage')

# відповідна кластеризація для методу повного зв'язку
groups1 <- cutree(hclust(d, method = 'complete'), k = 4)
plot(res$scores[,c(3,4)], col = c('red', 'green', 'blue', 'purple')[groups1])
# відповідна кластеризація для методу середнього зв'язку
groups2 <- cutree(hclust(d, method = 'average'), k = 4)
plot(res$scores[,c(3,4)], col = c('red', 'green', 'blue', 'purple')[groups2])
# відповідна кластеризація для методу одного зв'язку
groups3 <- cutree(hclust(d, method = 'single'), k = 4)
plot(res$scores[,c(3,4)], col = c('red', 'green', 'blue', 'purple')[groups3])

# ієрархічна кластеризація на даних методу головних компонент (2-4 компоненти)

# евклідова відстань
pca_d_e <- dist(res$scores[,2:4], method = 'euclidean')
# дендрограма, метод повного зв'язку
plot(as.dendrogram(hclust(pca_d_e, method = 'complete')), leaflab = 'none', 
     main = 'Euclidean metrics, complete linkage')
# дендрограма, метод одного зв'язку
plot(as.dendrogram(hclust(pca_d_e, method = 'single')), leaflab = 'none', 
     main = 'Euclidean metrics, single linkage')

# розфарбована діаграма розсіювання для 4 кластерів методу повного зв'язку
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue', 'purple')[cutree(hclust(pca_d_e, 
                                                             method = 'complete'), k = 4)],
     main = 'k = 4')
# розфарбована діаграма розсіювання для 3 кластерів методу повного зв'язку
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue')[cutree(hclust(pca_d_e, 
                                                             method = 'complete'), k = 3)],
     main = 'k = 3')
# розфарбована діаграма розсіювання для 4 кластерів методу одного зв'язку
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue', 'purple')[cutree(hclust(pca_d_e, 
                                                   method = 'single'), k = 4)],
     main = 'k = 4')

# відстань Махаланобіса

library(StatMatch)
pca_d_m <- mahalanobis.dist(res$scores[,2:4])
pca_d_m <- as.dist(pca_d_m)

# дендрограма, метод повного зв'язку
plot(as.dendrogram(hclust(pca_d_m, method = 'complete')), leaflab = 'none', 
     main = 'Mahalanobis metrics, complete linkage')

# дендрограма, метод одного зв'язку
plot(as.dendrogram(hclust(pca_d_m, method = 'single')), leaflab = 'none', 
     main = 'Mahalanobis metrics, single linkage')
# відповідна їй діаграма розсіювання у просторі 3 і 4 компонент із розфарбуванням на 2 кластери
plot(res$scores[,c(3,4)], 
     col = c('red', 'green','blue')[cutree(hclust(pca_d_m, method = 'single'), k = 3)],
     main = 'k = 3')
# і відповідна тривимірна діаграма
plot3d(res$scores[,2:4], 
       col = c('red', 'green')[cutree(hclust(pca_d_m, method = 'single'), k = 2)])

# розфарбована діаграма розсіювання для 3 кластерів методу повного зв'язку
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue')[cutree(hclust(pca_d_m, 
                                                             method = 'complete'), k = 3)],
     main = 'k = 3')
# розфарбована діаграма розсіювання для 4 кластерів методу повного зв'язку
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue', 'purple')[cutree(hclust(pca_d_m, 
                                                   method = 'complete'), k = 4)],
     main = 'k = 4')
# розфарбована діаграма розсіювання для 2 кластерів методу повного зв'язку
plot(res$scores[,c(3,4)], 
     col = c('red', 'green')[cutree(hclust(pca_d_m, method = 'complete'), k = 2)],
     main = 'k = 2')
# тривимірна діаграма для методу повного зв'язку і 3 кластерів
plot3d(res$scores[,2:4], 
       col = c('red', 'green', 'blue')[cutree(hclust(pca_d_m, method = 'complete'), k = 3)])

# відстань сіті-блок

pca_d_c <- dist(res$scores[,2:4], method = 'manhattan') # відстані
# дендрограма, метод повного зв'язку
plot(as.dendrogram(hclust(pca_d_c, method = 'complete')), leaflab = 'none', 
     main = 'City-block metrics, complete linkage')
# розфарбована діаграма розсіювання для 2 кластерів методу повного зв'язку
plot(res$scores[,c(3,4)], 
     col = c('red', 'green')[cutree(hclust(pca_d_c, 
                                                             method = 'complete'), k = 2)],
     main = 'k = 2')
# дендрограма, метод одного зв'язку
plot(as.dendrogram(hclust(pca_d_c, method = 'single')), leaflab = 'none', 
     main = 'City-block metrics, single linkage')

# спектральна кластеризація

library(kernlab)
sk <- specc(data, centers = 4)
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue', 'purple')[sk])

plot3d(res$scores[,2:4], col = c('red', 'green', 'blue', 'purple')[sk])

sk1 <- specc(res$scores[,2:4], centers = 4, kernel = 'rbfdot')
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue', 'purple')[sk1])

plot3d(res$scores[,2:4], col = c('red', 'green', 'blue', 'purple')[sk1])
plot3d(res$scores[,1:3], col = c('red', 'green', 'blue', 'purple')[sk1])

plot3d(res$scores[,1:3], col = c('red', 'green', 'blue', 'purple')[sk])

plot3d(res$scores[,1:3], 
         col = c('red', 'green', 'blue', 'purple')[cutree(hclust(pca_d_m, 
                                                                 method = 'complete'), k = 4)])

plot3d(res$scores[,1:3], 
       col = c('red', 'green')[cutree(hclust(pca_d_m, method = 'single'), k = 2)])
pairs(data, col = c('red', 'green')[cutree(hclust(pca_d_m, method = 'single'), k = 2)])


pca_d_canb <- dist(res$scores[,2:4], method = 'canberra')
# дендрограма, метод одного зв'язку
plot(as.dendrogram(hclust(pca_d_canb, method = 'complete')), leaflab = 'none', 
     main = 'Canberra metrics, complete linkage')
# відповідна їй діаграма розсіювання у просторі 3 і 4 компонент із розфарбуванням на 2 кластери
plot(res$scores[,c(3,4)], 
     col = c('red', 'green','blue')[cutree(hclust(pca_d_canb, method = 'complete'), k = 3)],
     main = 'k = 3')

plot3d(res$scores[,2:4], col = c('red', 'green','blue')[cutree(hclust(pca_d_canb, method = 'average'), k = 3)],
       main = 'k = 3')

plot(res$scores[,c(3,4)], 
     col = c('red', 'green','blue')[cutree(hclust(pca_d_m, method = 'average'), k = 3)],
     main = 'k = 3')
plot3d(res$scores[,2:4], col = c('red', 'green','blue')[cutree(hclust(pca_d_m, method = 'average'), k = 3)],
       main = 'k = 3')

plot(as.dendrogram(hclust(pca_d_m, method = 'average')), leaflab = 'none', 
     main = 'Mahalanobis metrics, average linkage')
plot(res$scores[,c(3,4)], 
     col = c('red', 'green','blue')[cutree(hclust(pca_d_m, method = 'average'), k = 3)],
     main = 'k = 3')
plot3d(res$scores[,2:4], col = c('red', 'green','blue')[cutree(hclust(pca_d_m, method = 'average'), k = 3)],
       main = 'k = 3')
pairs(data, col = c('red', 'green', 'blue')[cutree(hclust(pca_d_m, method = 'average'), k = 3)])
