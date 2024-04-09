# MySat

![GitHub Workflow Status (with event)](https://img.shields.io/github/actions/workflow/status/PolinaSavelyeva/MySat/ci.yml)

Simple console SAT-solver that utilizes DPLL algorithm and DIMACS input/output format.

## Requirements

- dotnet SDK (recommended version 7.0) for build and run
- picosat tool for testing

## Simple Usage

1. Navigate to the directory:

    ```
    cd MySat/
    ```

2. Provide the relative or absolute path to input data in DIMACS format and run as follows:

    ```
    dotnet run examples/example.cnf
    ```

The result of the above command:
    
```
s SATISFIABLE
v 1 -2 3 0
```

## Building

1. Navigate to the directory:

    ```
    cd MySat/
    ```

2. Build the library using dotnet tools:

    ```
    dotnet build
    ```
   
   or build in release configuration

   ```
   dotnet build --configuration Release
   ```

## Testing via Bash

1. Navigate to the directory:

    ```
    cd MySat/
    ```

2. Make test script executable:

    ```
    chmod +x ./test.sh
    ```

3. Provide the relative or absolute path to input data in DIMACS format and run script as follows:

    ```
    ./test.sh examples/example.cnf
    ```
