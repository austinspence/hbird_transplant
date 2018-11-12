#### Experimental Design Figure --------------------
# Austin Spence
# October 12th, 2017

par(mfrow=c(1, 2))
#par(mfrow=c(1, 1))

### Range Plot

plot(c(1:5), 1:5, type = "n", ylab = "Elevation (m)", xlab = NA,
     axes = FALSE, 
main="Hummingbird Range 
Along Sierra Nevada Mountain")
Axis(side=2, at = 1:5, labels=c("0", "1000", "2000", 
    "3000", "4000"), tick = TRUE)
polygon(1:5, c(1, 3, 5, 3, 1)) #mountain
polygon(1:5, c(1, 3, 3, 3, 1), density = c(10, 20)) #Anna's Range
legend("topright", title="Species",
       c("Anna","Calliope"), cex = 0.8, density = c(30, 0) )

### Historic Range Plot

plot(c(1:5), 1:5, type = "n", ylab = "Elevation (m)", xlab = NA,
     axes = FALSE, 
     main="Hummingbird Range 
     Along Sierra Nevada Mountain")
Axis(side=2, at = 1:5, labels=c("0", "1000", "2000", 
                                "3000", "4000"), tick = TRUE)
polygon(1:5, c(1, 3, 5, 3, 1)) #mountain
polygon(c(1, 1.5, 2.5, 4.5, 5), c(1, 2, 2, 2, 1), 
        density = c(10, 20)) #Anna's Range
legend("topright", title="Species",
       c("Anna","Calliope"), cex = 0.8, density = c(30, 0) )

## Experiment Plot
plot(c(1:5), 1:5, type = "n", ylab = "Elevation (m)", xlab = NA,
     axes = FALSE, 
     main="Acclimatization Experiment")

Axis(side=1, at = c(1,3,5), labels=c("Capture", 
                                "Acclimatization", "End"), tick = TRUE)
Axis(side=2, at = 1:5, labels=c("0", "1000", "2000", 
                                "3000", "4000"), tick = TRUE)
segments(x0 = 1, y0 = 2, x1 = 4.5, y1 = 2, col = "red")
segments(x0 = 4.5, y0 = 2, x1 = 4.5, y1 = 4.98, col = "red")
segments(x0 = 4.5, y0 = 4.98, x1 = 5, y1 = 4.98, col = "red")

segments(x0 = 1, y0 = 2.01, x1 = 1.5, y1 = 2.01, col = "black")
segments(x0 = 1.5, y0 = 2.01, x1 = 1.5, y1 = 5, col = "black")
segments(x0 = 1.5, y0 = 5, x1 = 5, y1 = 5, col = "black")

points(x = 1.3, y = 2, pch = 15)
points(x = 4.7, y = 5, pch = 15)
points(x = 1.7, y = 5, pch = 15)
points(x = 5, y = 5, pch = 16)
text(1.1, 5, "B")

### Current Range Plot with Overlapping Ranges

plot(c(1:5), 1:5, type = "n", ylab = "Elevation (m)", xlab = NA,
     axes = FALSE, 
     main=
"Anna's and Calliope Hummingbird Range 
Along Sierra Nevada Mountain")
Axis(side=2, at = 1:5, labels=c("0", "1000", "2000", 
                                "3000", "4000"), tick = TRUE)
polygon(1:5, c(1, 3, 5, 3, 1), col = "darkviolet", border = NA) #mountain
polygon(c(1, 2.25, 3, 3.76, 5), 
        c(1, 3.5, 3.5, 3.5, 1), col = "pink",
        border = NA) #Anna's Range
polygon(c(1.5, 2.24, 3, 3.76, 4.5), 
        c(2, 3.5, 3.5, 3.5, 2), col = "darkviolet", 
        border = NA, density = (3.5), lwd = 5)
legend("topright", title="Species",
       c("Calliope", "Anna"), cex = .8,
       fill = c("darkviolet", "pink"))

text(1.1, 5, "A")


### Current Range Plot with Anna's Only

plot(c(1:5), 1:5, type = "n", ylab = "Elevation (m)", xlab = NA,
     axes = FALSE, 
     main=
"Anna's Hummingbird Range 
Along Sierra Nevada Mountain")
Axis(side=2, at = 1:5, labels=c("0", "1000", "2000", 
                                "3000", "4000"), tick = TRUE)
polygon(1:5, c(1, 3, 5, 3, 1)) #mountain
polygon(c(1, 2.25, 3, 3.76, 5), 
        c(1, 3.5, 3.5, 3.5, 1), col = "pink") #Anna's Range


