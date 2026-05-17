# Cochran *C* Test

Detects and removes replicate outliers in data series based on the
Cochran *C* test for homogeneity in variance.

## Usage

``` r
cochranTest(X, id, fun = 'sum', alpha = 0.05)
```

## Arguments

- X:

  a numeric matrix (optionally a data frame that can be coerced to a
  numerical matrix).

- id:

  factor of the replicate identifiers.

- fun:

  function to aggregate data: 'sum' (default), 'mean', 'PC1' or 'PC2'.

- alpha:

  *p*-value of the Cochran *C* test.

## Value

a list with components:

- '`X`': input matrix from which outlying observations (rows) have been
  removed

- '`outliers`': numeric vector giving the row indices of the input data
  that have been flagged as outliers

## Details

The Cochran *C* test tests whether a single estimate of variance is
significantly larger than a a group of variances. It can be computed as:

\\RMSD = \sqrt{\frac{1}{n} \sum\_{i=1}^n {(y_i - \ddot{y}\_i)^2}}\\

where \\y_i\\ is the value of the side variable of the \\i\\th sample,
\\\ddot{y}\_i\\ is the value of the side variable of the nearest
neighbor of the \\i\\th sample and \\n\\ is the total number of
observations.

For multivariate data, the variance \\S_i^2\\ can be computed on
aggregated data, using a summary function (`fun` argument) such as
`sum`, `mean`, or first principal components ('PC1' and 'PC2').

An observation is considered to have an outlying variance if the Cochran
*C* statistic is higher than an upper limit critical value \\C\_{UL}\\
which can be evaluated with ('t Lam, 2010):

\\C\_{UL}(\alpha, n, N) = 1 +
\[\frac{N-1}{F\_{c}(\alpha/N,(n-1),(N-1)(n-1))}\]^{-1} \\

where \\\alpha\\ is the *p*-value of the test, \\n\\ is the (average)
number of replicates and \\F_c\\ is the critical value of the Fisher's
\\F\\ ratio.

The replicates with outlying variance are removed and the test can be
applied iteratively until no outlying variance is detected under the
given *p*-value. Such iterative procedure is implemented in
`cochranTest`, allowing the user to specify whether a set of replicates
must be removed or not from the dataset by graphical inspection of the
outlying replicates. The user has then the possibility to (i) remove all
replicates at once, (ii) remove one or more replicates by giving their
indices or (iii) remove nothing.

## Note

The test assumes a balanced design (i.e. data series have the same
number of replicates).

## References

Centner, V., Massart, D.L., and De Noord, O.E., 1996. Detection of
inhomogeneities in sets of NIR spectra. Analytica Chimica Acta 330,
1-17.

R.U.E. 't Lam (2010). Scrutiny of variance results for outliers:
Cochran's test optimized. Analytica Chimica Acta 659, 68-84.

[https://en.wikipedia.org/wiki/Cochran's_C_test](https://en.wikipedia.org/wiki/Cochran)

## Author

Antoine Stevens
