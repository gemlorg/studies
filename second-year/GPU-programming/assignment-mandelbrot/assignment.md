# Mandelbrot Set in CUDA

Write a program in CUDA implementing two different versions of the algorithm for creating a graphical representation of the Mandelbrot set and its immediate surroundings.

## Tasks

1. **Compare Execution Time**: Compare the execution time of the image in the CPU version and the CUDA version.
2. **Kernel Configuration**: For each version, examine the dependence of the program's execution time on the kernel configuration, according to the attached tables.

## Provided Files

In the directory `~w.rudnicki/Public/Mandelbrot`, there are two files `Mandelbrot_cuda.h` and `Mandelbrot_new_skel.cu`, which can serve as a starting point for your own implementation.

- **Mandelbrot_cuda.h**: Contains declarations of the necessary functions and the definition of real numbers.
- **Mandelbrot.cu**: Contains the `main()` function, auxiliary functions for generating images, functions generating the Mandelbrot set for CPU, and skeletons of two versions of the kernel generating the Mandelbrot set on GPU, as well as an auxiliary function for comparing results.

## Implementation

1. **Complete the Code**: Fill in the code in the kernels and the `main` function to create a functional program that generates the Mandelbrot set and its graphical representation. Note: It is acceptable and encouraged to write the program independently or to deeply modify the provided code.
2. **Run and Compare Performance**: Run the code and compare the performance of the CPU and GPU versions. Perform at least 11 runs for each version of the code. In the report, discuss the sources of time fluctuations for GPU and CPU results.
3. **Parameter Variation**: Compare the performance of different versions of the code with various parameter values (number of threads in a block in the 1D kernel, block dimensions in the 2D version) for the following section of the complex plane: {-2.0, -1.25} to {0.5, 1.25}.
4. **Analyze Differences**: Consider the reasons for differences (or lack thereof) in GPU versions. Use the tables below in your report.
5. **Kernel Execution Time**: Use the execution time of the kernel alone for performance comparisons. For comparing execution times of different GPU variants, use a resolution of 10,000 points.
6. **Reference CPU Time**: Use the time obtained when generating the set with a 10 times smaller resolution (1000x1000 instead of 10000x10000), multiplied by 100, as the reference CPU time.
7. **Clock Frequency Management**: Note that the GPU card in cloud nodes lowers its clock frequency when not in use to save energy. The management system increases the clock frequency to the maximum value only after several seconds of full load. Additionally, the virtual machine has significant fluctuations in allocated resources. Therefore, performance comparisons should follow this procedure:
   1. Run the compared calls of different code versions and configurations within a single invocation.
   2. Include the reference version call with time measurement multiple times within a single program invocation, e.g., following this pattern:
      ```
      CPU version
      GPU1D_32
      GPU1D_64
      …
      GPU1D_1024
      CPU version
      GPU2D_par1
      GPU2D_par2
      …
      GPU2D_par4
      CPU version
      ```
   3. Analyze only those runs for which the CPU version's time is stable (time differences less than 1%).

## Results Presentation

1. **Average Execution Time**: For each variant, calculate the average execution time \( \overline{T} \), where \( N \) is the number of measurements (e.g., 11), variance \( V \), and standard deviation \( S \). Calculate the standard deviation of the mean value \( S_m \).
2. **Rounding**: Round \( S_m \) up to two significant digits. Examples: 0.783456 → 0.79; 11.456 → 12; 99.01 → 100; 100.01 → 110; 0.00879345 → 0.0088.
3. **Result Format**: Present the average result with the precision resulting from rounding \( S_m \). Round the result according to standard rules. Examples: (1159.9090, 13.1609): 1160 ± 14; (1160.9510, 10.6509): 1161 ± 14; (1476.9220, 0.4992): 1476.92 ± 0.50.
4. **Acceleration Calculation**: Similarly, calculate the speedup relative to the CPU and present it as an average value with the standard deviation of the mean.
5. **Typical and Minimal Execution Time**: Provide the typical (median) and minimal execution time, rounded with the same precision as the average execution time.

## Reporting

Prepare a calculation scheme that calls several subsequent versions/configurations of the kernel within one function call, saves the results to an auxiliary table, and then calculates all values needed for the report, ready for presentation, leaving only the rounding to be corrected manually.

Reports for subsequent tasks will be prepared according to this same scheme.

## Report Structure

1. **Title Section**: Title of the report, author, student ID.
2. **Introduction**: Brief information about the problem (e.g., what is the Mandelbrot set) and the subject of the study (differences in performance depending on kernel configuration and type).
3. **Implementation Description**: Short description, include/cite the core kernel.
4. **Results**: Tables with discussion.
5. **Conclusion**: Findings.

---

### Tables for Result Summary

#### Table 1: 1D Version Summary

| Number of Threads in Block | Typical Execution Time (ms) | Minimal Execution Time (ms) | Average Execution Time (ms) | Speedup Relative to Reference |
| -------------------------- | --------------------------- | --------------------------- | --------------------------- | ----------------------------- |
| CPU 1                      |                             |                             |                             |                               |
| 32                         |                             |                             |                             |                               |
| 64                         |                             |                             |                             |                               |
| 128                        |                             |                             |                             |                               |
| 256                        |                             |                             |                             |                               |
| 512                        |                             |                             |                             |                               |
| 1024                       |                             |                             |                             |                               |

#### Table 2: 2D Version Summary (256 threads)

| Kernel Configuration | Typical Execution Time (ms) | Minimal Execution Time (ms) | Average Execution Time (ms) | Speedup Relative to Reference |
| -------------------- | --------------------------- | --------------------------- | --------------------------- | ----------------------------- |
| CPU 1                |                             |                             |                             |                               |
| 256 1                |                             |                             |                             |                               |
| 128 2                |                             |                             |                             |                               |
| 64 4                 |                             |                             |                             |                               |
| 32 8                 |                             |                             |                             |                               |
| 16 16                |                             |                             |                             |                               |
| 8 32                 |                             |                             |                             |                               |
| 4 64                 |                             |                             |                             |                               |
| 2 128                |                             |                             |                             |                               |
| 1 256                |                             |                             |                             |                               |

#### Table 3: 2D Version Summary (1024 threads)

| Kernel Configuration | Typical Execution Time (ms) | Minimal Execution Time (ms) | Average Execution Time (ms) | Speedup Relative to Reference |
| -------------------- | --------------------------- | --------------------------- | --------------------------- | ----------------------------- |
| CPU 1                |                             |                             |                             |                               |
| 1024 1               |                             |                             |                             |                               |
| 512 2                |                             |                             |                             |                               |
| 256 4                |                             |                             |                             |                               |
| 128 8                |                             |                             |                             |                               |
| 64 16                |                             |                             |                             |                               |
| 32 32                |                             |                             |                             |                               |
| 16 64                |                             |                             |                             |                               |
| 8 128                |                             |                             |                             |                               |
| 4 256                |                             |                             |                             |                               |
| 2 512                |                             |                             |                             |                               |
| 1 1024               |                             |                             |                             |                               |

#### Table 4: 2D Version Summary (Other Cases)

| Kernel Configuration | Typical Execution Time (ms) | Minimal Execution Time (ms) | Average Execution Time (ms) | Speedup Relative to Reference |
| -------------------- | --------------------------- | --------------------------- | --------------------------- | ----------------------------- |
| CPU 1                |                             |                             |                             |                               |
| 32 32                |                             |                             |                             |                               |
| 16 16                |                             |                             |                             |                               |
| 8 8                  |                             |                             |                             |                               |
| 32 16                |                             |                             |                             |                               |
| 64 8                 |                             |                             |                             |                               |
| 8 64                 |                             |                             |                             |                               |
| 16 32                |                             |                             |                             |                               |
