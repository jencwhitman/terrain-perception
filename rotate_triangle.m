

remove(list=ls())

# We define the coordinate space as ranging from -1 to +1 in each of the
# four dimensions. Drawing three vectors corresponding to 3 possible
# choices, equidistant because they're at the points of an equilateral
# triangle (vectors 120 degrees from each other).

# We start with an equilateral triangle drawn just in a 2D plane,
# simply because that's easy to draw and check.

ra_deg <- 240; #120; #desired rotation angle, in degrees
rarad <- ra_deg*pi/180; # desired rotation angle in radians

# the trig is just simpler to check if our vectors have a length of one
v1 <- matrix(c(1.0, 0.0, 0.0, 0.0),4,1); 

# get dim1 and dim2 coordinates for rotating our vector 120 then 240 degrees
#dim1coord <- cos(rarad) # -0.500
#dim2coord <- sin(rarad) # -0.866

v2 <- matrix(c(-0.5,0.866,0,0),4,1)
v3 <- matrix(c(-0.5,-0.866,0,0),4,1)

# Now, we take these three vectors indicating the points of an equilateral
# triangle in the plane defined by dim1 & dim 2, then rotate approx 45 degrees
# in that plane, then the plane defined by dim1 & dim3, then that defined by
# dim2 & dim3, then dim4 & dim3, etc...

# try a bunch of iterations with rotation angle randomly selected for each 2D plane
maxiterations <- 10; #200000; #2000000; # takes a bit less than 1 minute to run 100,000 itns on my faster laptop

###################################
# initializing a few vars  

dimcombos <- matrix(c(1,2, 1,3, 1,4, 2,3, 2,4, 3,4),2,6)
dimcombos <- t(dimcombos)

twoDplanes <- matrix(0,maxiterations,nrow(dimcombos))

twoDplanes_byitn <- matrix(0,maxiterations,6); # will contain one rotation angle per 2D plane per iteration
vectorsrotated_byitn <- array(0,dim=c(maxiterations,4,3));
vartominvalues_byitn <- matrix(0,maxiterations,1);   
uniquenessvalues_byitn <- matrix(0,maxiterations,4);
forsorting_byitn <- matrix(0,maxiterations,2);
    
twoDvectortorotate <- matrix(0,2,1);
twoDrotatedvector <- matrix(0,2,1);
vectorsrotated <- matrix(0,2,ncol(vectorstorotate))
fullunrotatedvector <- matrix(0,4,1);

diffs <- matrix(0,1,3);
forsorting <- matrix(0,maxiterations,3);
###################################

t0 <- proc.time()

for(i in 1:maxiterations) { 
    
    print(paste("iteration ", i, "of", maxiterations))

    vectorstorotate <- cbind(v1,v2,v3); 
    vectorsrotated <- vectorstorotate; # the loop below will edit some cells of this matrix each time

    for(c in 1:nrow(dimcombos)) {     
    
     		dim1 <- dimcombos[c,1];
        	dim2 <- dimcombos[c,2];
     
        # ra_deg <- 45;
        # try a different random angle of rotation in each plane
         ra_deg <- 30+runif(1)*30; # randomly picks an angle between 30 & 60 degrees
	  # ra_deg <- 30+15; # set to 45 degrees for debugging / checking
        # there's no really good reason why I picked 30 and 60 as my min
        # and max. Feel free to try other options.
        
        rarad <- ra_deg*pi/180;   
	  R <- matrix(c(cos(rarad),sin(rarad),-sin(rarad),cos(rarad)),2,2)   
	  twoDplanes[i,c]<-ra_deg;

        for(v in 1:ncol(vectorstorotate)) {             
            fullunrotatedvector[,1] <- vectorstorotate[,v];
            twoDvectortorotate[1,1] <- fullunrotatedvector[dim1,1];
            twoDvectortorotate[2,1] <- fullunrotatedvector[dim2,1];
            twoDrotatedvector <- R%*%twoDvectortorotate;
            vectorsrotated[dim1,v] <- twoDrotatedvector[1,1];
            vectorsrotated[dim2,v] <- twoDrotatedvector[2,1];
        } # end vectorstorotate loop
        vectorstorotate <- vectorsrotated;
        
    } # end dimcombos loop

    vectorsrotated_byitn[i,,] <- vectorsrotated  
    
    # So, is the variability spread fairly equally across the 4 dimensions?
    forvarcheck <- t(vectorsrotated); 

    # minimizing this means that the std dev is pretty similar across all 4 dimensions
    tmp <- matrix(c(sd(forvarcheck[,1]),sd(forvarcheck[,2]),sd(forvarcheck[,3])),nrow=3)
    vartominvalues_byitn[i,1] <- sd(tmp)
    # for ensuring that no vector is very unique on a single dimension
    # goal is to force participants to pay attention to more than one dimension
    uniquenesscheck <- matrix(0,4,1);
    for(row in 1:4) { 
        diffs[1,1] <- vectorsrotated[row,1]-vectorsrotated[row,2];
        diffs[1,2] <- vectorsrotated[row,1]-vectorsrotated[row,3];
        diffs[1,3] <- vectorsrotated[row,2]-vectorsrotated[row,3];
        uniquenesscheck[row,1] <- min(abs(diffs)); # within that dimension, the distance between the two closest categories.
    } #end row loop
    maxuniquenessvalue <- max(uniquenesscheck); # will want to reject solutions where the smallest distance between two categories is still big on any one dimension.
    uniquenessvalues_byitn[i,] <- t(uniquenesscheck);
    
    forsorting[i,1] <- i;
    forsorting[i,2] <- vartominvalues_byitn[i,1]; 
    forsorting[i,3] <- maxuniquenessvalue;
} # end loop of i up to maxiterations

sorted <- forsorting[order(forsorting[,2]),] # sort so that the solution with the smallest variability across dimensions is at the top
bestiteration <- sorted[1,1];

chosensolution <- vectorsrotated_byitn[bestiteration,,]
write.table(chosensolution, file="chosensolution.txt", row.names=FALSE, col.names=FALSE)
print(paste(sd(t(vectorsrotated_byitn[bestiteration,,]))))
print(paste(vartominvalues_byitn[bestiteration,1]))

t1 <- proc.time()-t0
t1


# Here's an optimal solution from 100,000 iterations. One column per
# vector, one row per dimension.

# ans  
#
#     0.5650   -0.6246    0.0596
#    -0.3277    0.7451   -0.4174
#     0.5667   -0.0336   -0.5331
#     0.5022    0.2312   -0.7335

# Then, for each vector, just randomly jitter around each of the 4 numbers
# to pick a point somewhere within a 4D 'hypersphere' drawn around that point.

# We could set a criterion where a vector can't be near zero in any of the
# 4 dimensions, but I don't think there's anything problematic about such a
# solution, so let's let it be for now.

# Eventually, for the sake of real-world validity, we'll probably need to
# draw non-spherical spaces (which is not that hard - just use multiple
# vectors to define one space). If we want to get fancy, we can draw
# ellipses or adjacent trapezoids rather than spheres.

# We'll probably also need to allow partial overlap between regions
# (because, in real-life situations, there may be conditions when choice A
# and choice B are both good while choice C is disastrous).

#####################################################################
#####################################################################
# solution for making sure that, on each dimension, two category prototypes
# are pretty similar to each other.


##################################################################
# next step: draw a hypersphere of category exemplars around each category
# prototype.

minradius <- 0.25;
maxradius <- 0.50;
for(category in 1:3) {
    
    numexemplars_per_category <- 50;
    exemplarvectors <- matrix(0,numexemplars_per_category,4);
    
    for (e in 1:numexemplars_per_category) {
        
        r <- minradius+runif(1)*(maxradius-minradius);
	  #r <- minradius+0.5*(maxradius-minradius);
	  v_exemplar <- matrix(c(sqrt(r*r/4), sqrt(r*r/4), sqrt(r*r/4), sqrt(r*r/4)),nrow=4);
        v_prototype_row <- vectorsrotated_byitn[i,,category]; 
        v_prototype <- as.matrix(v_prototype_row); 
        vrotated <- v_exemplar;
        for (c in 1:nrow(dimcombos)) {
            
            dim1 <- dimcombos[c,1];
            dim2 <- dimcombos[c,2];
            
            ra_deg <- 0+runif(1)*90; # randomly picks an angle between 0 & 90 degrees
		#ra_deg <- 0+60;            

            rarad <- ra_deg*pi/180;

		R <- matrix(c(cos(rarad),sin(rarad),-sin(rarad),cos(rarad)),2,2)
            
            fullunrotatedvector <- v_exemplar;
            twoDvectortorotate[1,1] <- fullunrotatedvector[dim1,1];
            twoDvectortorotate[2,1] <- fullunrotatedvector[dim2,1];
            twoDrotatedvector <- R%*%twoDvectortorotate;
            vrotated[dim1,1] <- twoDrotatedvector[1,1];
            vrotated[dim2,1] <- twoDrotatedvector[2,1];
        }
        
        v <- vrotated+v_prototype;

        exemplarvectors[e,] <- t(v);
    }

    fname=paste("exemplars category",category,".txt");
    write.table(exemplarvectors, file=fname, row.names=FALSE, col.names=FALSE)
}


