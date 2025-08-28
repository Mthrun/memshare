mutualinfo = function(x, y, isXDiscrete = FALSE, isYDiscrete = FALSE,eps=.Machine$double.eps*1000, useMPMI=FALSE,na.rm=FALSE) {
  #mutualinfo(x,y)
  #INPUT
  # x (numeric[1:n]) Input Vector to the mutual information
  # y (numeric[1:n]) Input Vector to the mutual information
  #
  #OPTIONAL
  # isXDiscrete (bool) Whether x is a discrete or continuous vector
  # isYDiscrete (bool) Whether y is a discrete or continuous vector
  # eps (numeric) The value of density estimate at which it should be disregarded for the mutual info calculation (theoretically 0 should be left out)
  # useMPMI (bool) If True uses mpmi for the calculation
  # na.rm     bool, if TRUE, uses only complete observations
  #OUTPUT
  # (numeric) the mutual info value of the pair (x,y). 0 = stochastically independent variables, the higher the more dependent they are.
  #
  #Author: Julian Maerte, Michael Thrun
  
  if (!is.vector(x) || !is.vector(y)) stop("mutualinfo: Both x and y must be vectors!")
  if (!is.numeric(x) || !is.numeric(y)) stop("mutualinfo: Both x and y must be numeric!")
  if (length(x) != length(y)) stop("mutualinfo: x and y have to be of the same length")
  
  if(!requireNamespace("ScatterDensity",quietly = T)){
    warning("Please install package ScatterDensity.")
    return(NULL)
  }else{
    if(packageVersion("ScatterDensity")<"0.1.1"){
      warning("Please update package ScatterDensity to version 0.1.1 or higher.")
      return(NULL)
    }
  }
  if(!requireNamespace("DataVisualizations",quietly = T)){
    warning("Please install package DataVisualizations.")
    return(NULL)
  }else{
    if(packageVersion("DataVisualizations")<"1.1.5"){
      warning("Please update package DataVisualizations to version 1.1.5 or higher.")
      return(NULL)
    }
  }
  
  if(isTRUE(na.rm)){
    bb=is.finite(x)&is.finite(y)
    y=y[bb]
    x=x[bb]
  }
  
  if(length(x)<3){
    return(0)
  }
  
  if (sd(x,na.rm = T) < eps || sd(y,na.rm = T) < eps) {
    return(0)
  }
  
  if (isTRUE(useMPMI)) {
    if(!requireNamespace("mpmi",quietly = T)){
		warning("Please install package mpmi.")
		return(NULL)
	}
    return(mpmi::cmi(cbind(x,y))$mi[1,2])
  } else {
    if (!isXDiscrete && !isYDiscrete) {
      ParetoRadiusX=DataVisualizations::ParetoRadius_fast(x)
      ParetoRadiusY=DataVisualizations::ParetoRadius_fast(y)
      
      if (is.nan(ParetoRadiusX) || is.nan(ParetoRadiusY)) {
        warning("One of the Pareto Radii was NaN! Returning 0.0 mutual information which might not be accurate.")
        return(0)
      }
      
      # Both variables are continuous; thus perform 2d density estimate
      xs = seq(min(x) - ParetoRadiusX, max(x) + ParetoRadiusX, length.out=512)
      ys = seq(min(y) - ParetoRadiusY, max(y) + ParetoRadiusY, length.out=512)
      
      jointV=ScatterDensity::SmoothedDensitiesXY(x,y,Xkernels=xs,Ykernels=ys,lambda = 4,Compute = "Parallel")
      joint=jointV$GridDensity #512x512
      #xs     <- jointV$Xkernels
      #ys     <- jointV$Ykernels
      dx = mean(diff(xs))
      dy = mean(diff(ys))
      
      mx = length(xs)
      my = length(ys)
      
      
      # Construct the gaussian kernel to 2d-convolve the estimate with
      relx = (seq_len(mx) - ceiling(mx / 2)) * dx
      rely = (seq_len(my) - ceiling(my / 2)) * dy
      
      # input to the gaussian kernel is the distance from origin (mean)
      distMat = outer(relx, rely, function(r1, r2) sqrt(r1^2 / (2 * ParetoRadiusX^2) + r2^2 / (2 * ParetoRadiusY^2)))
      # Apply 2d normal density to the distance matrix:
      kFFT2D = (1 / (2 * pi * ParetoRadiusX * ParetoRadiusY)) * exp(- (distMat^2))
      # important! Normalize it so that the convolution has volume 1
      kFFT2D = kFFT2D / sum(kFFT2D)
      Lx = 2^ceiling(log2(2 * mx - 1))
      Ly = 2^ceiling(log2(2 * my - 1))
      
      pad_density = matrix(0, nrow=Lx, ncol=Ly)
      pad_kernel = matrix(0, nrow=Lx, ncol=Ly)
      
      pad_density[1:mx, 1:my] = joint
      pad_kernel[1:mx, 1:my] = kFFT2D
      fft_density = fft(pad_density)
      fft_kernel = fft(pad_kernel)
      # convolve by the common inverse fft of the product of ffts
      conv = fft(fft_density * fft_kernel, inverse=T) / (Lx * Ly)
      
      # Take real part of the convolution where the center of the inverse fft corresponds to the input (top-left corner of input)
      smoothedJoint = Re(conv[((Lx-mx)/2):(ceiling((2*mx+Lx-mx)/2)-1), ((Ly-my)/2):(ceiling((2*my+Ly-my)/2)-1)])
      
      # calculate marginals
      pdf_x = rowSums(smoothedJoint) * dy
      pdf_y = colSums(smoothedJoint) * dx
      
      # denominator of fraction in MI is the pairwise product of the marginals
      pxpy <- outer(pdf_x, pdf_y)
      
      NonZeroInd <- (smoothedJoint>eps*2)
      #Compute MI via Riemann sum
      mutual <- sum( smoothedJoint[NonZeroInd] * log( smoothedJoint[NonZeroInd] / pxpy[NonZeroInd] ) ) * dx * dy
      return(mutual)
    } else {
      if (isXDiscrete && isYDiscrete) {
        # if both are discrete simply tabularize the unique values and calculate the discrete MI.
        edges_x=sort(unique(x),decreasing = F)
        edges_y=sort(unique(y),decreasing = F)
        joint_table=ScatterDensity::fast_table_num(x,y,edges_x,edges_y,redefine=F,na.rm = TRUE)
        
        n = length(x)
        return(c_mutualinfo(joint_table, n))
      } else if (isXDiscrete) {
        # always make x the continuous variable
        t = x
        x = y
        y = t
      }
      # x is continuous, y is discrete
      ParetoRadiusX=DataVisualizations::ParetoRadius_fast(x)
      if (is.nan(ParetoRadiusX)) {
        warning("Pareto Radius of the continuous variable was NaN! Return 0.0 mutual information which might not be accurate!")
        return(0)
      }
      xs = seq(min(x) - ParetoRadiusX, max(x) + ParetoRadiusX, length.out=512)
      
      jointV=ScatterDensity::SmoothedDensitiesXY(x, y, Xkernels=xs, lambda = 4, isYDiscrete=T, Compute = "Cpp")
      density = jointV$GridDensity
      
      xs = jointV$Xkernels
      dx = mean(diff(xs))
      
      p = rowSums(density)
      
      density <- sweep(density, 2, colSums(density) * dx, "/")
      
      inner = apply(density, 2, function(col) sum(col[col > eps] * log(col[col > eps] / p[col > eps])) * dx)
      
      n = length(x)
      weights = table(y) / n
      mi = sum(inner * weights)
      
      return(mi)
    } 
  }
}

