

  cubedraw <- function(res3d, min = 0, max = 10, cex = 2, text. = FALSE)
       {
         ## Purpose: Draw nice cube with corners
         cube01 <- rbind(c(0,0,1), 0, c(1,0,0), c(1,1,0), 1, c(0,1,1), # < 6 outer
                         c(1,0,1), c(0,1,0)) # <- "inner": fore- & back-ground
         cub <- min + (max-min)* cube01
         ## visibile corners + lines:
         res3d$points3d(cub[c(1:6,1,7,3,7,5) ,], cex = cex, type = 'l', lty = 1)
         ## hidden corner + lines
         res3d$points3d(cub[c(2,8,4,8,6),     ], cex = cex, type = 'l', lty = 3)
         if(text.)## debug
             text(res3d$xyz.convert(cub), labels=1:nrow(cub), col='tomato', cex=2)
       }
       ## 6 a) The named colors in R, i.e. colors()
       crgb <- mat1
       par(xpd = TRUE)
       rr <- scatterplot3d(mat1, box = FALSE, angle = 25,
           xlim = c(0, 10), ylim = c(0, 10), zlim = c(0, 10),type="p",xlab="item",ylab="time",zlab="patient",highlight.3d=T)
       cubedraw(rr)
       rr$points3d(mat1[mat1$y==1 | mat1$y==10,])

       for(i in c(1,5,10))
       {
       #rr <- scatterplot3d(mat1, box = FALSE, angle = 24,
       #xlim = c(0, 10), ylim = c(0, 10), zlim = c(0, 10),type="n",xlab="item",ylab="time",zlab="patient")
       rr$points3d(mat1[mat1$y==i,],col="red")
       }
       
       for(j in 1:10)
       {
       rr <- scatterplot3d(mat1, box = FALSE, angle = 24,
       xlim = c(0, 10), ylim = c(0, 10), zlim = c(0, 10),type="n",xlab="item",ylab="time",zlab="patient")
       rr$points3d(mat1[mat1$y==1,])
       rr$points3d(mat1[mat1$y==1 & mat1$x==1 & mat1$z==j,],col="red",pch=20,cex=1.5)
       }
       
        for(j in 1:10)
       {
       rr <- scatterplot3d(mat1, box = FALSE, angle = 24,
       xlim = c(0, 10), ylim = c(0, 10), zlim = c(0, 10),type="n",xlab="item",ylab="time",zlab="patient")
       rr$points3d(mat1[mat1$y==1,])
       rr$points3d(mat1[mat1$y==1 & mat1$z==j,],col="red",pch=20,cex=1.5)
       }
       
       for(j in 1:10)
       {
       rr <- scatterplot3d(mat1, box = FALSE, angle = 24,
       xlim = c(0, 10), ylim = c(0, 10), zlim = c(0, 10),type="n",xlab="item",ylab="time",zlab="patient")
       rr$points3d(mat1[mat1$y==1,])
       rr$points3d(mat1[mat1$y==1 & mat1$x==j,],col="red",pch=20,cex=1.5)
       }

        for(i in 1:10)
       {
       rr <- scatterplot3d(mat1, box = FALSE, angle = 24,
       xlim = c(0, 10), ylim = c(0, 10), zlim = c(0, 10),type="n",xlab="item",ylab="time",zlab="patient")
       rr$points3d(mat1[mat1$y==i,])
       }


       cubedraw(rr)
       ## 6 b) The rainbow colors from rainbow(201)
       rbc <- rainbow(201)
       Rrb <- t(col2rgb(rbc))
       rR <- scatterplot3d(Rrb, color = rbc, box = FALSE, angle = 24,
           xlim = c(-50, 300), ylim = c(-50, 300), zlim = c(-50, 300))
       cubedraw(rR)
       rR$pland3d(Rrb)


       library(rgl)
 open3d()

 #points3d(x[,1],x[,2],x[,3],size=2,col='grey')

 #axes3d(c('x','y','z'))
 g1<-mat1
 #g2<-x2
 #plot3d(g1[,1],g1[,2],g1[,3],xlab="x1",ylab="x2",zlab="x3",size=3,col='red',type="s",radius=0.05)
 spheres3d(g1[,1],g1[,2],g1[,3], col='red', size=3, type="s",radius=0.05,add=T)
# spheres3d(g2[,1],g2[,2],g2[,3], col='blue', size=3, type="s",radius=0.05,add=T)
decorate3d(c(0,10),c(0,10),c(0,10),xlab="item",ylab="patient",zlab="time",box = TRUE, axes = TRUE)
 play3d(spin3d(axis=c(0,0,1), rpm=5), duration=10)

 rgl.snapshot("name2.png",fmt="png")


   ###########add a surface ############

  open3d()

 y<-g1[,2]
 x<-g1[,1]
 z<-g1[,3]

 #surfacedat<-as.data.frame(cbind(x,y,z))
 surfacedat<-as.data.frame(g1[g1$z==1,])

  surface.loess<-loess(z~x*y,data=surfacedat)
  surface.mar<-with(surfacedat,list(x=seq(from=min(x),to=max(x),by=(max(x)-min(x))/100),y=seq(from=min(y),to=max(y),by=(max(y)-min(y))/100)))
  surface.lo<-predict(surface.loess,expand.grid(surface.mar))

 surface3d(surface.mar$x,surface.mar$y,surface.lo,col="red")
 
 
 surfacedat<-as.data.frame(g1[g1$z==5,])

  surface.loess<-loess(z~x*y,data=surfacedat)
  surface.mar<-with(surfacedat,list(x=seq(from=min(x),to=max(x),by=(max(x)-min(x))/100),y=seq(from=min(y),to=max(y),by=(max(y)-min(y))/100)))
  surface.lo<-predict(surface.loess,expand.grid(surface.mar))

 surface3d(surface.mar$x,surface.mar$y,surface.lo,col="orange")
 
 
 surfacedat<-as.data.frame(g1[g1$z==10,])

  surface.loess<-loess(z~x*y,data=surfacedat)
  surface.mar<-with(surfacedat,list(x=seq(from=min(x),to=max(x),by=(max(x)-min(x))/100),y=seq(from=min(y),to=max(y),by=(max(y)-min(y))/100)))
  surface.lo<-predict(surface.loess,expand.grid(surface.mar))

 surface3d(surface.mar$x,surface.mar$y,surface.lo,col="yellow")
 
  rgl.snapshot("name.png",fmt="png")
 
 
 
 
 points3d(g1[,1],g1[,2],g1[,3],size=2.5,col='black')
 decorate3d(g1[,1],g1[,2],g1[,3], xlab = "x1", ylab = "x2", zlab = "x3", box = TRUE, axes = TRUE)
  play3d(spin3d(axis=c(0,0,1), rpm=5), duration=10)