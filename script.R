# ������� ����
data <- read.table('C:\\Users\\Razor\\Desktop\\����������� ��������\\������������ ����� ������������� �����\\lab4\\F9.txt')

# ��������� ������� ���������� ���
pairs(data, cex = 0.1)

# ��������� ����� �������� ���������
res <- princomp(data)
plot(res)
summary(res)

# �������� ������� ���������� ������ ���� �������� ���������
plot(res$scores[,1:2])

# �������� ������� ������� ���������� ������ ����� �������� ���������
plot(res$scores[,2:3])
plot(res$scores[,c(1,3)])

# �������� ��������� ������� ����������
library(rgl)
plot3d(res$scores[,1:3])

plot3d(res$scores[,2:4])

# ����������� ���������� ����� ���������� �����
d <- dist(data, method = 'euclidean')
# ����������� �� ��������� �������� � ����� ������ ��'����
plot(as.dendrogram(hclust(d, method = 'single')), leaflab = 'none', 
     main = 'Euclidean metrics, single linkage')
# ����������� �� ��������� �������� � ����� ������� ��'����
plot(as.dendrogram(hclust(d, method = 'complete')), leaflab = 'none', 
     main = 'Euclidean metrics, complete linkage')
# ����������� �� ��������� �������� � ����� ���������� ��'����
plot(as.dendrogram(hclust(d, method = 'average')), leaflab = 'none', 
     main = 'Euclidean metrics, average linkage')

# �������� ������������� ��� ������ ������� ��'����
groups1 <- cutree(hclust(d, method = 'complete'), k = 4)
plot(res$scores[,c(3,4)], col = c('red', 'green', 'blue', 'purple')[groups1])
# �������� ������������� ��� ������ ���������� ��'����
groups2 <- cutree(hclust(d, method = 'average'), k = 4)
plot(res$scores[,c(3,4)], col = c('red', 'green', 'blue', 'purple')[groups2])
# �������� ������������� ��� ������ ������ ��'����
groups3 <- cutree(hclust(d, method = 'single'), k = 4)
plot(res$scores[,c(3,4)], col = c('red', 'green', 'blue', 'purple')[groups3])

# ���������� ������������� �� ����� ������ �������� ��������� (2-4 ����������)

# �������� �������
pca_d_e <- dist(res$scores[,2:4], method = 'euclidean')
# �����������, ����� ������� ��'����
plot(as.dendrogram(hclust(pca_d_e, method = 'complete')), leaflab = 'none', 
     main = 'Euclidean metrics, complete linkage')
# �����������, ����� ������ ��'����
plot(as.dendrogram(hclust(pca_d_e, method = 'single')), leaflab = 'none', 
     main = 'Euclidean metrics, single linkage')

# ������������ ������� ���������� ��� 4 �������� ������ ������� ��'����
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue', 'purple')[cutree(hclust(pca_d_e, 
                                                             method = 'complete'), k = 4)],
     main = 'k = 4')
# ������������ ������� ���������� ��� 3 �������� ������ ������� ��'����
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue')[cutree(hclust(pca_d_e, 
                                                             method = 'complete'), k = 3)],
     main = 'k = 3')
# ������������ ������� ���������� ��� 4 �������� ������ ������ ��'����
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue', 'purple')[cutree(hclust(pca_d_e, 
                                                   method = 'single'), k = 4)],
     main = 'k = 4')

# ������� �����������

library(StatMatch)
pca_d_m <- mahalanobis.dist(res$scores[,2:4])
pca_d_m <- as.dist(pca_d_m)

# �����������, ����� ������� ��'����
plot(as.dendrogram(hclust(pca_d_m, method = 'complete')), leaflab = 'none', 
     main = 'Mahalanobis metrics, complete linkage')

# �����������, ����� ������ ��'����
plot(as.dendrogram(hclust(pca_d_m, method = 'single')), leaflab = 'none', 
     main = 'Mahalanobis metrics, single linkage')
# �������� �� ������� ���������� � ������� 3 � 4 ��������� �� �������������� �� 2 ��������
plot(res$scores[,c(3,4)], 
     col = c('red', 'green','blue')[cutree(hclust(pca_d_m, method = 'single'), k = 3)],
     main = 'k = 3')
# � �������� ��������� �������
plot3d(res$scores[,2:4], 
       col = c('red', 'green')[cutree(hclust(pca_d_m, method = 'single'), k = 2)])

# ������������ ������� ���������� ��� 3 �������� ������ ������� ��'����
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue')[cutree(hclust(pca_d_m, 
                                                             method = 'complete'), k = 3)],
     main = 'k = 3')
# ������������ ������� ���������� ��� 4 �������� ������ ������� ��'����
plot(res$scores[,c(3,4)], 
     col = c('red', 'green', 'blue', 'purple')[cutree(hclust(pca_d_m, 
                                                   method = 'complete'), k = 4)],
     main = 'k = 4')
# ������������ ������� ���������� ��� 2 �������� ������ ������� ��'����
plot(res$scores[,c(3,4)], 
     col = c('red', 'green')[cutree(hclust(pca_d_m, method = 'complete'), k = 2)],
     main = 'k = 2')
# ��������� ������� ��� ������ ������� ��'���� � 3 ��������
plot3d(res$scores[,2:4], 
       col = c('red', 'green', 'blue')[cutree(hclust(pca_d_m, method = 'complete'), k = 3)])

# ������� ��-����

pca_d_c <- dist(res$scores[,2:4], method = 'manhattan') # �������
# �����������, ����� ������� ��'����
plot(as.dendrogram(hclust(pca_d_c, method = 'complete')), leaflab = 'none', 
     main = 'City-block metrics, complete linkage')
# ������������ ������� ���������� ��� 2 �������� ������ ������� ��'����
plot(res$scores[,c(3,4)], 
     col = c('red', 'green')[cutree(hclust(pca_d_c, 
                                                             method = 'complete'), k = 2)],
     main = 'k = 2')
# �����������, ����� ������ ��'����
plot(as.dendrogram(hclust(pca_d_c, method = 'single')), leaflab = 'none', 
     main = 'City-block metrics, single linkage')

# ����������� �������������

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
# �����������, ����� ������ ��'����
plot(as.dendrogram(hclust(pca_d_canb, method = 'complete')), leaflab = 'none', 
     main = 'Canberra metrics, complete linkage')
# �������� �� ������� ���������� � ������� 3 � 4 ��������� �� �������������� �� 2 ��������
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
