# Triangular DQ Coefficients and High-Order Mesh

#### Introduction  
This project provides a web application that can (1) assist users in generating high-order triangular meshes, and (2) compute key coefficient matrices required for generating triangular differential quadrature method (DQM) weight coefficients.

#### Software Architecture  
The backend is developed in Go, while HTML and JavaScript are used for the frontend. Scientific computation functionalities are implemented in Mathematica script files.

#### Installation Guide  

1. Mathematica 13.0 or higher must be installed.  
2. Add the directory containing Mathematica’s `math.exe` to the system's `PATH` environment variable.

#### Instructions for Use  

1. After completing the configuration, run `main.exe` via the command line.  
2. Access the application at `http://localhost:18085`.  
3. In the "DQM Calculation" tab, the computed `c10` and `c01` represent the coefficient matrices for the first-order partial derivatives with respect to ξ and η, respectively; `cL` denotes the correction matrix accounting for integration points.
