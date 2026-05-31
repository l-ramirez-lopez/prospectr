# Contributing to prospectr

Thanks for your interest in contributing to prospectr! Whether you’re
fixing a bug, adding a feature, or improving documentation… your help is
appreciated.

This package exists to make spectroscopic methods more accessible and
operational. Every contribution helps push chemometrics forward and
brings these tools closer to real-world applications in soil science,
agriculture, food science, and beyond.

## Reporting bugs

Found something broken? Open an issue on
[GitHub](https://github.com/l-ramirez-lopez/prospectr/issues) with:

- A minimal reproducible example
- Output from
  [`sessionInfo()`](https://rdrr.io/r/utils/sessionInfo.html)
- What you expected vs what happened

## Suggesting features

Have an idea? Open an issue and tell us:

- What problem you’re trying to solve
- How you’d approach it
- Any alternatives you’ve considered

## Contributing code

### Getting started

1.  Fork the repository
2.  Clone your fork:
    `git clone https://github.com/YOUR-USERNAME/prospectr.git`
3.  Create a branch: `git checkout -b feature/your-feature-name`
4.  Install dependencies: `devtools::install_deps(dependencies = TRUE)`

### Code style

- Follow the existing style
- Use roxygen2 for documentation
- Add tests for new functionality (testthat)

### Before submitting

``` r

devtools::test()
devtools::check()
```

Make sure `R CMD check` passes without errors or warnings.

### Pull request process

1.  Update documentation if needed (`devtools::document()`)
2.  Add a note to NEWS.md
3.  Submit a PR to the main branch

## Code of Conduct

This project has a Code of Conduct (see in the GitHub repo). Please be
kind and respectful.

## Questions

Open an issue or email <ramirez.lopez.leo@gmail.com>.
