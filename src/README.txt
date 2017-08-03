
-------
Outline
-------

1. Prepare indicator data
    a. Convert country names to ISO code. Countries w/o codes are dropped
    b. Preen data, taking countries with some data w/in the last 20 years.
    c. Normalize data (currently on range [-1,1] but [0,1] would be more
       typical. More positive indicates the positive end of the statistic,
       e.g. highest life expectancy
    d. Merge data sets for each indicator into a master data frame. Countries
       that do not have available data in each indicator are dropped because
       we cannot deal with missing dimensions.

2. Construct adjaceny matrix from border data
    a. Create a subset of the master border matrix (data/border_matrix.csv)
       representing only those countries in the final data set from (1).
    b. Transform border matrix into adjacency matrix by replacing 0's with
       large number (beyond max filtration) and 1's with the Euclidean
       distance between the respective countries.

3. Compute persisent homology with R TDA package

4. (PROPOSAL) Recompute homology with a sampling of the adjaceny matrix to
   get statistics on the significance of homology classes - make a "confidence
   band" for the persistence diagram.
