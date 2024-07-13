# Matrix Multiplication Assignment

## Objective

The goal of this task is to write, test, and compare the performance of several different versions of a CUDA kernel that calculates the product of two matrices. For simplicity, these should be square matrices of size NxN.

If matrix C is obtained as the product of matrices A and B, then each element of matrix C is given by the formula:

\[ c*{ij} = \sum_k a*{ik} \cdot b\_{kj} \]

where the first index is the row index and the second is the column index. Thus, the element of matrix C at index \{i,j\} is obtained as the dot product of the i-th row of matrix A and the j-th column of matrix B. The same formula can also be interpreted as the sum of N consecutive matrices \(C_k\) obtained as the outer products of consecutive columns of matrix A and columns of matrix B:

\[ C_k = A_k \otimes B_k \]

## Required Versions

1. **Reference CPU Version**: Implement the formula in C/C++.
2. **CUDA Kernel #1**: Directly translate the CPU code, replacing the double loop through the result array with thread indexing.
3. **CUDA Kernel #2**: Implement matrix multiplication as the outer product of vectors. Each block of size K threads calculates a square piece of the result matrix of size KxK, implementing formula (2). The kernel uses shared memory to store the relevant column vectors from matrix A and matrix B, as well as the relevant fragment of the result array.
4. **CUDA Kernel #3**: Modify Kernel #2 to eliminate bank conflicts in shared memory.
5. **CUDA Kernel #4**: Modify Kernel #3 to store both column vectors in registers while keeping the result array in shared memory. Use shuffle instructions to exchange register contents. Use blocks consisting of 32 threads.
6. **CUDA Kernel #5**: Further modify Kernel #4 to also store the fragment of the result matrix in registers. Each thread stores one fragment of a row of the result matrix.

## Validation

For each version, check the results against the CPU version, providing two metrics:

1. Number of differing elements.
2. If this number is non-zero, also provide the sum of absolute deviations between matrices.

## Reporting

In the report, provide the average speedup of each version compared to the reference CPU implementation, obtained from at least 10 repetitions of the code execution. To ensure comparable results, measure the execution time of the CPU version and all tested kernel versions in each repetition. For selected code versions, also examine the performance dependency on kernel configuration. Present the results in a tabular format as shown below.

### Table 1. Performance Comparison of Different Kernel Versions

| Code Version | Kernel Configuration | Average Execution Time (ms) | Average Speedup Relative to CPU |
| ------------ | -------------------- | --------------------------- | ------------------------------- |
| CPU          | —                    | —                           | —                               |
| Kernel #1    | 8 x 8                |                             |                                 |
| Kernel #1    | 16 x 16              |                             |                                 |
| Kernel #1    | 32 x 32              |                             |                                 |
| Kernel #2    | 32 x 1               |                             |                                 |
| Kernel #2    | 64 x 1               |                             |                                 |
| Kernel #2    | 96 x 1               |                             |                                 |
| Kernel #2    | 128 x 1              |                             |                                 |
| Kernel #3    | 32 x 1               |                             |                                 |
| Kernel #3    | 64 x 1               |                             |                                 |
| Kernel #4    | 32 x 1               |                             |                                 |
| Kernel #5    | 32 x 1               |                             |                                 |

## Discussion

Discuss the results in the report, explaining the observed differences in performance between the various code versions.
