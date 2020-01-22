machine = read.csv(file.choose())

head(machine)
str(machine)
summary(machine)

#drive utilization coln

machine$utilization = 1 - machine$Percent.Idle

#handling date time in R

as.POSIXct(machine$Timestamp, format = "%d/%m/%Y %H:%M")



# add new coln in dataset

machine$PosixTime = as.POSIXct(machine$Timestamp, format = "%d/%m/%Y %H:%M")

head(machine)

# reaarange coln in datset

machine$Timestamp = NULL

machine = machine[,c(4,1,2,3)]

#lists are usefull in subsetting like below


RL1 = machine[machine$Machine =="RL1",]
summary(RL1)

RL1$Machine = factor(RL1$Machine)


#CONSTRUCT THE LIST

machine_stats_rl1 = c(min(RL1$utilization, na.rm = T),
                      mean(RL1$utilization, na.rm = T),
                      max(RL1$utilization, na.rm = T))



machine_stats_rl1

# utilization under 90%

length(which(RL1$utilization < 0.90))

machine_under_90_flag = length(which(RL1$utilization < 0.90))

list_rl1 = list("RL1", machine_stats_rl1,machine_under_90_flag)

list_rl1


#naming component of lists

names(list_rl1) #checking names

names(list_rl1) = c("machinetype", "stats","lowthreshold")



list_rl1


list_rl1$machinetype


#vector : all hours where utilization in unknown
list_rl1$unknownhrs = RL1[is.na(RL1$utilization),"PosixTime"]

list_rl1$unknownhrs

#remove a component

list_rl1[4] = NULL
list_rl1[5] = NULL
list_rl1[6] = NULL
list_rl1[7] = NULL
#dataframe for the machine


list_rl1$data = RL1


list_rl1

summary(list_rl1)

#subsetting list



list_rl1[[4]][1]
list_rl1$unknownhrs[1]

list_rl1[1:3]
list_rl1[c(1,4)]

sublist_rl1 = list_rl1[c("machinename","stats")]
sublist_rl1 
sublist_rl1[[2]][1]


#building the time series plot

p = ggplot(data = machine)
p+ geom_line(aes(x = PosixTime,y = utilization, colour = Machine), size = 1.2) + 
  facet_grid(Machine~.) + 
  geom_hline(yintercept = 0.90, colour ="Gray", size =1.2, linetype =3)

machineplot = p+ geom_line(aes(x = PosixTime,y = utilization, colour = Machine), size = 1.2) + 
  facet_grid(Machine~.) + 
  geom_hline(yintercept = 0.90, colour ="Gray", size =1.2, linetype =3)

list_rl1$plot = machineplot

list_rl1
