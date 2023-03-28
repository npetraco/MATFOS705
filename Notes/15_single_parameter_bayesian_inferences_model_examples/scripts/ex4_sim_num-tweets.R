library(dafs)
data(wong.df)
wong.df

# sample from a narrow-ish group of lambda with replacement
# for a lambda generate a poisson rv
# break up by time of day (morning, noon, night)

# for  labmda for each time domain and pooled. Look like there may be a difference??

cnts.mat <- t(rbind(
  rpois(30, lambda = sample(seq(from=1100, to=1190, by=10), size = 30, replace = T)), # vary lam in a tp per day a little??
  rpois(30, lambda = sample(seq(from=1300, to=1390, by=10), size = 30, replace = T)),
  rpois(30, lambda = sample(seq(from=1200, to=1290, by=10), size = 30, replace = T))
))
cnts.mat
colnames(cnts.mat) <- c("M","A","N")
rownames(cnts.mat) <- 1:30
t(cnts.mat)

sample(seq(from=1100, to=1190, by=10), size = 30, replace = T)

# Morning, afternoon, night counts for 30 days
c(#M     A     N
1154, 1326, 1251,
1062, 1362, 1234,
1203, 1297, 1337,
1125, 1350, 1235,
1091, 1324, 1189,
1120, 1384, 1289,
1202, 1343, 1318,
1129, 1373, 1190,
1103, 1345, 1307,
1098, 1399, 1224,
1169, 1364, 1279,
1142, 1380, 1331,
1174, 1303, 1310,
1111, 1232, 1244,
1148, 1330, 1246,
1134, 1306, 1168,
1146, 1309, 1267,
1179, 1336, 1274,
1165, 1367, 1262,
1076, 1291, 1254,
1152, 1325, 1139,
1209, 1348, 1236,
1205, 1318, 1310,
1139, 1351, 1227,
1227, 1382, 1310,
1145, 1340, 1255,
1140, 1305, 1230,
1220, 1306, 1327,
1059, 1333, 1242,
1165, 1337, 1269)


c(1154, 1062, 1203, 1125, 1091, 1120, 1202, 1129, 1103, 1098, 1169, 1142, 1174, 1111, 1148, 1134, 1146, 1179, 1165, 1076, 1152, 1209, 1205, 1139, 1227, 1145, 1140, 1220, 1059, 1165)
c(1326, 1362, 1297, 1350, 1324, 1384, 1343, 1373, 1345, 1399, 1364, 1380, 1303, 1232, 1330, 1306, 1309, 1336, 1367, 1291, 1325, 1348, 1318, 1351, 1382, 1340, 1305, 1306, 1333, 1337)
c(1251, 1234, 1337, 1235, 1189, 1289, 1318, 1190, 1307, 1224, 1279, 1331, 1310, 1244, 1246, 1168, 1267, 1274, 1262, 1254, 1139, 1236, 1310, 1227, 1310, 1255, 1230 ,1327, 1242, 1269)

# Gamma prior
xx <- seq(from=0, to=80000, length.out=5000)
yy <- dgamma(xx, shape = 25/16, rate = 1/16000)
plot(xx,yy)
