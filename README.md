# slimmr: SLiM Models in R

## Development of slimmr is discontinued
slimmr is currently not in development. A future end-of-life update may tidy up the code.

## Third-party software requirements
slimmr requires a working installation of SLiM3, which can be downloaded from: https://messerlab.org/slim/. NOTE: the contributor(s) to this package have no affiliation with, and are not endorsed by, the developers of SLiM. Please cite the developers of SLiM as per their preference if you find this package useful, in addition to citing slimmr.

## Example
```r
library(slimmr)

# Load an example model from a file:

modelpath <- "/Users/user/model.slim"
model <- newSlimModel(description = "My model", scriptfile = modelpath)

# Overview and inspect the model

model
#> -/ slimmr SLiM model /----------------------------------------------------
#>
#>
#> My model
#>
#> Lines:  17 
#> Blocks: [ 1 ]  initialize() 
#>         [ 2 ]  1 
#>         [ 3 ]  1000 late() 
#>         [ 4 ]  2000 late() 
#>
#> File:  /Users/user/model_slimmr.slim
#>
#>
#>-/ slimmr v 0.2.0 /-------------------------------------------------------

model$inspect()
#> 1   initialize() {
#> 2     initializeMutationRate(1e-7);
#> 3     initializeMutationType("m1", 0.5, "f", 0.0);
#> 4     initializeGenomicElementType("g1", m1, 1.0);
#> 5     initializeGenomicElement(g1, 0, 99999);
#> 6     initializeRecombinationRate(1e-8);
#> 7   }
#> 8   1 {
#> 9     sim.addSubpop("p1", 500);
#> 10  }
#> 11  1000 late() {
#> 12    p1.outputSample(10);
#> 13  }
#> 14  2000 late() {
#> 15    p1.outputSample(10);
#> 16    sim.outputFixedMutations();
#> 17  }

# Run model once, and then 1000 times in parallel across four nodes

result <- model$run()
results <- model$replicate(replicates = 1000, nodes = 4, feedbackinterval = 100)
```

## References
Haller, B.C., & Messer, P.W. (2019). SLiM 3: Forward genetic simulations beyond the Wright–Fisher model. Molecular Biology and Evolution 36(3), 632–637.

The SLiM-extras repository on GitHub: https://github.com/MesserLab/SLiM-Extras
