
clear

% We define the coordinate space as ranging from -1 to +1 in each of the
% four dimensions. Drawing three vectors corresponding to 3 possible
% choices, equidistant because they're at the points of an equilateral
% triangle (vectors 120 degrees from each other).

% We start with an equilateral triangle drawn just in a 2D plane,
% simply because that's easy to draw and check.

ra_deg=240; %120; %desired rotation angle, in degrees
rarad=ra_deg.*pi./180; % desired rotation angle in radians

% the trig is just simpler to check if our vectors have a length of one
v1=[ 1.0;  0.0;   0.0;   0.0];

% get dim1 and dim2 coordinates for rotating our vector 120 then 240 degrees
dim1coord=cos(rarad)
dim2coord=sin(rarad)

v2=[    -0.5000;
    0.8660;
    0.0000;
    0.0000];

v3=[    -0.5000;
    -0.8660;
    0.0000;
    0.0000];


% Now, we take these three vectors indicating the points of an equilateral
% triangle in the plane defined by dim1 & dim 2, then rotate approx 45 degrees
% in that plane, then the plane defined by dim1 & dim3, then that defined by
% dim2 & dim3, then dim4 & dim3, etc...

% try a bunch of iterations with rotation angle randomly selected for each 2D plane
maxiterations=200000; %2000000; % takes a bit less than 1 minute to run 100,000 itns

tic

forsorting=zeros(maxiterations,2);
for i=1:maxiterations
    
    fprintf(['iteration ' int2str(i) ' of ' int2str(maxiterations) '\n'])
    
    dimcombos=[ % all the two-D planes in which we'll rotate our triangle
        1 2;
        1 3;
        1 4;
        2 3;
        2 4;
        3 4];
    
    vectorstorotate=[v1 v2 v3];
    vectorsrotated=vectorstorotate; % the loop below will edit some cells of this matrix each time
    
    for c=1:size(dimcombos,1)
        
        dim1=dimcombos(c,1);
        dim2=dimcombos(c,2);
        
        % ra_deg=45;
        % try a different random angle of rotation in each plane
        ra_deg=30+rand*30; % randomly picks an angle between 30 & 60 degrees
        % there's no really good reason why I picked 30 and 60 as my min
        % and max. Feel free to try other options.
        
        rarad=ra_deg.*pi./180;
        R=[ cos(rarad) -sin(rarad);
            sin(rarad)  cos(rarad)];
        
        iterationresults{i}.twoDplanes{c}.angle=ra_deg;
        
        for v=1:size(vectorstorotate,2)
            
            fullunrotatedvector=vectorstorotate(:,v);
            twoDvectortorotate(1,1)=fullunrotatedvector(dim1,1);
            twoDvectortorotate(2,1)=fullunrotatedvector(dim2,1);
            twoDrotatedvector=R*twoDvectortorotate;
            vectorsrotated(dim1,v)=twoDrotatedvector(1,1);
            vectorsrotated(dim2,v)=twoDrotatedvector(2,1);
        end
        vectorstorotate=vectorsrotated;
        
    end
    iterationresults{i}.vectorsrotated=vectorsrotated;
    
    % % check that the lengths are all still one, as they should be:
    % sqrt(v1(1,1)^2+v1(2,1)^2+v1(3,1)^2+v1(4,1)^2)
    % sqrt(v2(1,1)^2+v2(2,1)^2+v2(3,1)^2+v2(4,1)^2)
    % sqrt(v3(1,1)^2+v3(2,1)^2+v3(3,1)^2+v3(4,1)^2)
    % % yup, all still the correct length
    
    % So, is the variability spread fairly equally across the 4 dimensions?
    forvarcheck=vectorsrotated';
    % mean(forvarcheck) % means all near zero: good
    std(forvarcheck);  % variance differs across dimensions - hmmm.... try multiple iterations? try varying the angle of rotation a bit randomly?
    % min(forvarcheck)
    % max(forvarcheck)
    iterationresults{i}.vartomin=std(std(forvarcheck)); % minimizing this means that the std dev is pretty similar across all 4 dimensions
    
    
    % for ensuring that no vector is very unique on a single dimension
    % goal is to force participants to pay attention to more than one dimension
    uniquenesscheck=zeros(4,1);
    for row=1:4
        diffs(1,1)=vectorsrotated(row,1)-vectorsrotated(row,2);
        diffs(1,2)=vectorsrotated(row,1)-vectorsrotated(row,3);
        diffs(1,3)=vectorsrotated(row,2)-vectorsrotated(row,3);
        uniquenesscheck(row,1)=min(abs(diffs)); % within that dimension, the distance between the two closest categories.
    end
    maxuniquenessvalue=max(uniquenesscheck); % will want to reject solutions where the smallest distance between two categories is still big on any one dimension.
    iterationresults{i}.uniquenessvalues=uniquenesscheck;
    
    forsorting(i,1)=i;
    forsorting(i,2)=iterationresults{i}.vartomin;
    forsorting(i,3)=maxuniquenessvalue;
end

sorted=sortrows(forsorting,2);
% sorted=sortrows(forsorting,3);
% 
% for cutoffind=1:5
% % cutoffind=round(.000005*size(sorted,1))
% shortlist=sorted(1:cutoffind,:);
% doublesorted=sortrows(shortlist,2);
% 
% % bestiteration=sorted(1,1);
% bestiteration=doublesorted(1,1);
% 
% iterationresults{bestiteration}.vectorsrotated
% end

bestiteration=sorted(1,1);
chosensolution=iterationresults{bestiteration}.vectorsrotated
save -ascii chosensolution.txt chosensolution
std(transpose(iterationresults{bestiteration}.vectorsrotated))
iterationresults{bestiteration}.vartomin

toc
% save -v7.3 workspace_rotate_triangle2.mat

% Here's the optimal solution from 100,000 iterations. One column per
% vector, one row per dimension.

% ans =
%
%     0.5650   -0.6246    0.0596
%    -0.3277    0.7451   -0.4174
%     0.5667   -0.0336   -0.5331
%     0.5022    0.2312   -0.7335

% Then, for each vector, just randomly jitter around each of the 4 numbers
% to pick a point somewhere within a 4D 'hypersphere' drawn around that point.

% We could set a criterion where a vector can't be near zero in any of the
% 4 dimensions, but I don't think there's anything problematic about such a
% solution, so let's let it be for now.

% Eventually, for the sake of real-world validity, we'll probably need to
% draw non-spherical spaces (which is not that hard - just use multiple
% vectors to define one space). If we want to get fancy, we can draw
% ellipses or adjacent trapezoids rather than spheres.

% We'll probably also need to allow partial overlap between regions
% (because, in real-life situations, there may be conditions when choice A
% and choice B are both good while choice C is disastrous).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% solution for making sure that, on each dimension, two category prototypes
% are pretty similar to each other.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% next step: draw a hypersphere of category exemplars around each category
% prototype.

minradius=0.25;
maxradius=0.50;
for category=1:3
    
    numexemplars_per_category=50;
    exemplarvectors=zeros(numexemplars_per_category,4);
    
    for e=1:numexemplars_per_category
        
        r=minradius+rand*(maxradius-minradius);
        
        v_exemplar=[sqrt(r*r/4); sqrt(r*r/4); sqrt(r*r/4); sqrt(r*r/4);];
        v_prototype=iterationresults{bestiteration}.vectorsrotated(:,category);
        
        
        vrotated=v_exemplar;
        for c=1:size(dimcombos,1)
            
            dim1=dimcombos(c,1);
            dim2=dimcombos(c,2);
            
            ra_deg=0+rand*90; % randomly picks an angle between 0 & 90 degrees
            
            rarad=ra_deg.*pi./180;
            R=[ cos(rarad) -sin(rarad);
                sin(rarad)  cos(rarad)];
            
            fullunrotatedvector=v_exemplar;
            twoDvectortorotate(1,1)=fullunrotatedvector(dim1,1);
            twoDvectortorotate(2,1)=fullunrotatedvector(dim2,1);
            twoDrotatedvector=R*twoDvectortorotate;
            vrotated(dim1,1)=twoDrotatedvector(1,1);
            vrotated(dim2,1)=twoDrotatedvector(2,1);
        end
        
        v=vrotated+v_prototype;
%         [v v_prototype] % should look similar, since vrotated should be a way shorter vector - yup, that's fine

        exemplarvectors(e,:)=v';
    end
    eval(['save -ascii exemplars_category' int2str(category) '.txt exemplarvectors']);
end

