# pocketsizefund

> Open source capital management  

[![Python code checks](https://github.com/pocketsizefund/pocketsizefund/actions/workflows/run_python_code_checks.yaml/badge.svg)](https://github.com/pocketsizefund/pocketsizefund/actions/workflows/run_python_code_checks.yaml) [![Rust code checks](https://github.com/pocketsizefund/pocketsizefund/actions/workflows/run_rust_code_checks.yaml/badge.svg)](https://github.com/pocketsizefund/pocketsizefund/actions/workflows/run_rust_code_checks.yaml)  

## About

**Pocket Size Fund** is an open source quantitative hedge fund.  

This repository holds the resources for the fund platform.

The project is actively a work-in-progress.  

## Project

### Setup

Run the commands below in the root directory.  

```sh
brew install flox # https://flox.dev/ for more information
flox activate
uv venv
source .venv/bin/activate
mask --help # see all available Mask tasks
mask setup
mask development python install
```

### Principles

An unordered and non-exhaustive list we work towards:  

> Test in production  
> Always roll forward  
> Systems over process  
> No code is good code  
> Never write documentation  
> Git is truth  

### Links

Check out [our tasks](https://github.com/orgs/pocketsizefund/projects/11) to see what we're working on or ping [either](https://x.com/forstmeier) of [us](https://x.com/hyperpriorai) for anything else.  
