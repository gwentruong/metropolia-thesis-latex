library(tidyr)
library(ggplot2)
# Read data from csv files
data = read.csv("Data.csv")
for (i in 1:18) {
  assign(paste("list",i,sep = ""), data[,i])
}

# Create 3 dataframes of each type of Voxers (None, 30 degree, 40 degree)
none_data = do.call(rbind, Map(data.frame, Pb = c(list1, list15, list16), Pab = c(list2, list17, list18)))
degree30 = do.call(rbind, Map(data.frame, Pb = c(list3, list6, list7, list8), Pab = c(list4,list5, list9, list10)))
degree40 = do.call(rbind, Map(data.frame, Pb = c(list12, list13), Pab = c(list11, list14)))

# Plot histogram and density frequency of total pressure of each type
hist(none_data$Pab, main = paste("Histogram of total pressure of empty pipe"), xlab = "Pab (mbar)",probability = TRUE, col = "gray87")
lines(density(none_data$Pab), col = "dodgerblue2", lwd =2 )
lines(density(none_data$Pab, adjust=2), lty="dotted", col="darkgreen", lwd=2)

hist(degree30$Pab, main = paste("Histogram of total pressure with 2 Voxers 30 degree"), xlab = "Pab (mbar)",probability = TRUE, col = "gray87")
lines(density(degree30$Pab), col = "dodgerblue2", lwd =2 )
lines(density(degree30$Pab, adjust=2), lty="dotted", col="darkgreen", lwd=2)

hist(degree40$Pab, main = paste("Histogram of total pressure with 2 Voxers 40 degree"), xlab = "Pab (mbar)",probability = TRUE, col = "gray87")
lines(density(degree40$Pab), col = "dodgerblue2", lwd =2 )
lines(density(degree40$Pab, adjust=2), lty="dotted", col="darkgreen", lwd=2) # smoothen the line

# Join Pb values of 3 types of Voxers in a dataframe
P_b =do.call(rbind, Map(data.frame,none_pb = c(list15, list16), degree30_pb = c(list7, list8), 
                        degre40_pb = degree40$Pb))
P_b$times = seq.int(nrow(P_b))

# Join Pab values of 3 types of Voxers in a dataframe
P_ab =do.call(rbind,Map(data.frame,none_pab = c(list17, list18), degree30_pab = c(list9, list10), degre40_pab = c(degree40$Pab))) 
P_ab$times = seq.int(nrow(P_ab))

# Reshape data frame from wide to long
rs_Pb = gather(data = P_b, key = "type", value = "Pb", -times)
rs_Pab = gather(data = P_ab, key = "type", value = "Pab", -times)

# Plot lines of Pb and Pab from different groups of Voxers type
Pb_plot = ggplot(data = rs_Pb, aes(x = times, y = Pb, colour = type)) + geom_point() + geom_smooth()
Pb_plot + ggtitle("Plot of values of Pb from each type of Voxers by times") + ylab("Pb (mbar)")
Pab_plot =ggplot(data = rs_Pab, aes(x = times, y = Pab, colour = type)) + geom_point() + geom_smooth() 
Pab_plot + ggtitle("Plot of values of Pab from each type of Voxers by times") + ylab("Pab (mbar)")

# Compare density line of Pb and Pab from different groups of Voxers type
Pb_density = ggplot(data= rs_Pb, aes(Pb, colour = type)) + geom_density()
Pb_density + ggtitle("Density of Pb values from each type of Voxers") + xlab("Pb (mbar)")
Pab_density = ggplot(data= rs_Pab, aes(Pab, colour = type)) + geom_density()
Pab_density + ggtitle("Density of Pab values from each type of Voxers") + xlab("Pab (mbar)")
