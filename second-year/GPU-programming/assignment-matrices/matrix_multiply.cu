#include <stdio.h>
#include <stdlib.h>

#include <chrono>
#include <functional>
#include <iostream>
#define real float
using namespace std;
#define DIM 32
#define ROW 33
#define DIM32 32
#define DIM64 64
#define DIM96 96
#define DIM128 128
#define ROW33 33
#define ROW65 65
#define ROW97 97
#define ROW129 129

void Transpose(real* A, real* At, int N) {

  for (int i = 0; i < N; i++) {
    int loc = i * N;
    int loc_t;
    for (int j = 0; j < N; j++) {
      loc_t = j * N + i;
      At[loc_t] = A[loc];
      loc++;
    }
  }
}

void Multiply(real* A, real* B, real* C, int N) {
  // B is transposed
  int loc_a, loc_b, loc_c;
  //printf("In Multiply\n");
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      loc_c = i * N + j;
      loc_a = i * N;
      loc_b = j * N;
      C[loc_c] = 0.0f;
      for (int k = 0; k < N; k++) {
        C[loc_c] += A[loc_a] * B[loc_b];
        loc_a++;
        loc_b++;
      }
    }
  }
}

void PrintMat(real* A, int row, int ext_row, int col, int ext_col, int N) {
  int cur_row;
  int loc;
  cur_row = row;
  for (int i = 0; i < ext_row; i++) {
    loc = cur_row * N + col;
    for (int j = 0; j < ext_col; j++) {
      printf("%f  ", A[loc + j]);
    }
    printf("\n");
    cur_row++;
  }
}

void CompareMatrices(real* A, real* B, int N) {
  int count = 0;
  real Sum = 0.0f;
  int loc = 0;

  for (int i = 0; i < N; i++) {
    for (int j = 0; j < N; j++) {
      if (A[loc] != B[loc]) {
        Sum += fabs(A[loc] - B[loc]);
        count++;
      }
      loc++;
    }
  }
  printf("Difference: %f\n", Sum);
  printf("Count: %d\n", count);
}

__global__ void MatrixMultGPUv1(real* A, real* B, real* C, int N) {
  int column = blockIdx.x * blockDim.x + threadIdx.x;
  int row = blockIdx.y * blockDim.y + threadIdx.y;
  int locA, locB;
  int k;
  // We assume that B is transposed
  real c = 0.0;
  locA = N * row;
  locB = N * column;
  if (column < N && row < N) {
    for (k = 0; k < N; k++) {
      c += A[row * N + k] * B[k * N + column];
    }
    C[row * N + column] = c;
  }
}

template <uint32_t dim>
__global__ void mult_gpu2(real* A, real* B, real* C, int N) {

// Zakładamy, ze macierz A jest transponowana
// (to oznacza, ze w pamieci jest ulozona kolumnami a nie wierszami)
// Wątki są ułozone wzdłuz kolumn macierzy A
// To oznacza, ze kazdy wątek operuje ma przypisany wiersz z macierzy A
// Kazdy blok wylicza fragment macierzy C rozmiaru dimxdim
// Lokalizacja fragmentu jest dana przez adres dwuwymiarowy bloku
//
// ZACZYNAMY KOD
// ustal adres początkowy w macierzy C

// ustal start_x jako początkową kolumnę na podstawie numeru bloku i wymiaru bloku
int start_x = blockIdx.x*dim;
// ustal start_y jako początkowy wiersz na podstawie numeru bloku i wymiaru bloku
int start_y = blockIdx.y*dim;
// ustaw my_row jako numer wiersza, którym się zajmuje wątek
int row = start_y + threadIdx.y;
//
// Teraz sprawdzamy czy nasz blok zajmuje się pełnymi kwadratowymi fragmentami, czy jest
// zlokalizowany na dole macierzy C lub na koncu wiersza.
// Dla bloków zlokalizowanych na dole częśc wątków nie będzie robiła obliczen.
// Dla bloków zlokalizowanych na koncach wierszy trzeba zrobic mniej iteracji pętli po kolumnach
// (i wczytac mniej wartosci z macierzy B do bufora)
int iter = blockIdx.x < (gridDim.x-1) ? dim : dim - (dim*gridDim.x-N);
int locA, locB, locC; // zmienne nadmiarowe - mozna je usunac upraszczając kod
int k; // licznik w zewnętrznej pętli - numer kolumny i wiersza
int n; // licznik w wewnetrznej pętli
real a; // wartośc z kolumny macierzy A, właściwa dla wątku
//__shared__ real OUT[dim*dim];
__shared__ real OUT[dim][dim]; // bufor w pamięci shared dla macierzy wynikowej
__shared__ real rowB[dim]; //  fragment wiersza z macierzy B
unsigned i = threadIdx.y; // numer wątku w bloku -> czyli lokalny
// unsigned glob_block = blockIdx.y*gridDim.x+blockIdx.x;
// unsigned glob_i = glob_block*blockDim.y+i;

//    if (glob_i==0) {
//        printf("blockIdx.x %d\n",blockIdx.x);
//        printf("gridDim.x %d\n",gridDim.x);
//        printf("blockIdx.x < gridDim.x %d\n",blockIdx.x < gridDim.x);
//        printf("iter %d\n",iter);
//    }

    // zerowanie tablicy OUT
    for (n=0;n<dim;n++) OUT[i][n]=0.0;
    __syncthreads();
    for (k=0;k<N;k++) { // zewnętrzna pętla
        // czytamy wartości z k-tej kolumny macierzy A
        // sprawdzamy czy nasz wątek ma cos do zrobienia
        //if (glob_i==0) printf("iteracja %d\n",k);
        if (row <N) {
            // jeśli tak to ustalamy skąd ma wątek ma przeczytac swoją wartośc kolumny
            // k-ta kolumna zaczyna się w lokalizacji k*N
            // wątek zajmuje się jej elementem takim jaki jest jego globalny numer  -> musi być row inaczej nie przejdzie dalej: utknie w pierwszym wierszu.
            locA = k * N + row;
            a = A[locA];
            // printf("numer wątku %d, row %d locA %d",i,row,locA);
            // printf("numer wątku %d, row %d locA %d a %lf\n",i,row,locA,a);
        }
        __syncthreads();
        // czytamy wartości z k-tego wiersza macierzy B
        // sprawdzamy czy nasz wątek ma coś do zrobienia
        if (i < iter) {
            locB = start_x + i + k*N;
            rowB[i] = B[locB];
            // k-ty wiersz zaczyna się w lokalizacji k*N
            // wątek ma wczytac z niej element o numerze start_x + lokalny numer wątku
            //printf("numer wątku %d, start_x %d locB %d rowB %lf\n",i,start_x,locB,rowB[i]);

        }
        __syncthreads();

        if (row <N) { //
            // wewnetrzna petla
            for (int n=0;n<iter;n++) {
                OUT[i][n] += a * rowB[n];
                // dodaj wartosc iloczynu kolumna z A razy wiersz z B do bufora wynikowego
                // kazdy wątek ma wartośc z kolumny w swojej zmiennej
                // wszystkie wątki operują na tym samym elemencie z wektora B
                // wszystkie wątki pracują nad tą samą kolumną w buforze
            }
        }
        __syncthreads();

    }
    __syncthreads();
    // store the final result in the global memory
    if (row < N ) { // if current thread is inside the solution
        for (int n=0; n< iter; n++ ) {
            locC = row*N + n + start_x;
            C[locC] = OUT[i][n];
            // skopiuj bufor do własciwej lokalizacji w pamieci globalnej
            // kazdy wątek kopiuje "swój" wiersz.
        }
    }
    __syncthreads();

}

template <uint32_t dim>
__global__ void mult_gpu3(real* A, real* B, real* C, int N) {
  int start_x = blockIdx.x * dim;
  int start_y = blockIdx.y * dim;

  int iter = blockIdx.x < gridDim.x ? dim : dim - (dim * gridDim.x - N);

  __shared__ real OUT[dim][dim + 1];
  __shared__ real rowB[dim];

  int i = threadIdx.y;

  for (int n = 0; n < dim; n++)
    OUT[n][i] = 0;
  __syncthreads();

  int row = start_y + i;
  for (int k = 0; k < N; ++k) {
    if (i < iter)
      rowB[i] = B[k * N + start_x + i];
    __syncthreads();

    real colA = row < N ? A[row * N + k] : 0;
    __syncthreads();

    for (int n = 0; n < dim; n++) {
      OUT[i][n] += colA * rowB[n];
    }
    __syncthreads();
  }
  __syncthreads();
  if (row < N) {
    for (int n = 0; n < dim; n++) {
      C[row * N + start_x + n] = OUT[i][n];
    }
  }
  __syncthreads();
}









template <uint32_t dim>
__global__ void mult_gpu4(real* A, real* B, real* C, int N) {
  int start_x = blockIdx.x * dim;
  int start_y = blockIdx.y * dim;

  int iter = blockIdx.x < gridDim.x ? DIM : DIM - (DIM * gridDim.x - N);

  __shared__ real OUT[dim][dim + 1];
  real rowB;

  int i = threadIdx.y;

  for (int n = 0; n < dim; n++)
    OUT[n][i] = 0;
  __syncthreads();

  int row = start_y + i;
  for (int k = 0; k < N; ++k) {
    if (i < iter)
      rowB = B[k * N + start_x + i];
    __syncthreads();

    real colA = row < N ? A[row * N + k] : 0;
    __syncthreads();

    for (int n = 0; n < dim; n++) {
      OUT[i][n] += colA * __shfl_sync(0xffffffff, rowB, n);
    }
    __syncthreads();
  }
  __syncthreads();
  if (row < N) {
    for (int n = 0; n < dim; n++) {
      C[row * N + start_x + n] = OUT[i][n];
    }
  }
  __syncthreads();
}

template <uint32_t dim>
__global__ void mult_gpu5(real* A, real* B, real* C, int N) {
  int start_x = blockIdx.x * dim;
  int start_y = blockIdx.y * dim;

  int iter = blockIdx.x < gridDim.x ? dim : dim - (dim * gridDim.x - N);

  real OUT[dim];
  real rowB;

  int i = threadIdx.y;

  for (int n = 0; n < dim; n++)
    OUT[n] = 0;
  __syncthreads();

  int row = start_y + i;
  for (int k = 0; k < N; ++k) {
    if (i < iter)
      rowB = B[k * N + start_x + i];
    __syncthreads();

    real colA = row < N ? A[row * N + k] : 0;
    __syncthreads();

    for (int n = 0; n < dim; n++) {
      OUT[n] += colA * __shfl_sync(0xffffffff, rowB, n);
    }
    __syncthreads();
  }
  __syncthreads();
  if (row < N) {
    for (int n = 0; n < dim; n++) {
      C[row * N + start_x + n] = OUT[n];
    }
  }
  __syncthreads();
}

template <int dim>
void Multiply_gpu_1(real* Agpu, real* Bgpu, real* Cgpu, int N) {
  dim3 threads(dim, dim, 1);
  dim3 blocks((N - 1) / dim + 1, (N - 1) / dim + 1, 1);

  MatrixMultGPUv1<<<blocks, threads>>>(Agpu, Bgpu, Cgpu, N);

  auto status = cudaDeviceSynchronize();

  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  };
}

template <int dim>
void Multiply_gpu_2(real* Agpu, real* Bgpu, real* Cgpu, int N) {
  dim3 block_size(1, dim, 1);
  dim3 grid_size((N + dim - 1) /dim ,
                 (N + dim - 1) /dim, 1 );

  mult_gpu2<dim><<<grid_size, block_size>>>(Agpu, Bgpu, Cgpu, N);

  auto status = cudaDeviceSynchronize();

  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  };
}
template <int dim>

void Multiply_gpu_3(real* Agpu, real* Bgpu, real* Cgpu, int N) {
  dim3 block_size(1, dim, 1);
  dim3 grid_size((N + dim - 1) / dim,
                 (N + dim - 1) / dim, 1);

  mult_gpu3<dim><<<grid_size, block_size>>>(Agpu, Bgpu, Cgpu, N);

  auto status = cudaDeviceSynchronize();

  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  };
}
template <int dim>

void Multiply_gpu_4(real* Agpu, real* Bgpu, real* Cgpu, int N) {
  dim3 block_size(1, dim, 1);
  dim3 grid_size((N + dim - 1) / dim,
                 (N + dim - 1) / dim, 1);

  mult_gpu4<dim><<<grid_size, block_size>>>(Agpu, Bgpu, Cgpu, N);

  auto status = cudaDeviceSynchronize();

  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  };
}
template <int dim>

void Multiply_gpu_5(real* Agpu, real* Bgpu, real* Cgpu, int N) {
  dim3 block_size(1, dim, 1);
  dim3 grid_size((N + dim - 1) / dim,
                 (N + dim - 1) / dim, 1);

  mult_gpu5<dim><<<grid_size, block_size>>>(Agpu, Bgpu, Cgpu, N);

  auto status = cudaDeviceSynchronize();

  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  };
}

double get_avg_time_cpu(double* t, double* r, uint32_t times, real* A, real* B,
                        real* C, int N) {
  double sum = 0;
  double* result = (double*)malloc(times * sizeof(double));
  for (int i = 0; i < times; i++) {
    for (int i = 0; i < (N * N); i++) {
      A[i] = rand() * 1.0 / RAND_MAX;
      B[i] = rand() * 1.0 / RAND_MAX;
    }
    auto start = chrono::steady_clock::now();
    Multiply(A, B, C, N);
    auto stop = chrono::steady_clock::now();
    auto diff = stop - start;
    double dif = chrono::duration<double, milli>(diff).count();
    sum += dif;
    result[i] = dif;
  }
  sum /= times;
  double ssd = 0;
  for (int j = 0; j < times; j++) {
    ssd += pow((sum - result[j]), 2) / (times - 1);
  }
  ssd = pow(ssd, 0.5) / pow(times, 0.5);
  *t = sum;
  *r = ssd;
  std::cout << "time is: " << *t << "+-" << *r << std::endl;
  return sum;
}

template <int dim, class F>
double get_avg_time_gpu(uint32_t times, F mult, double cpu_t, double cpu_err,
                        real* A, real* B, real* Agpu, real* Bgpu, real* Cgpu,
                        int N) {
  double sum = 0;

  double* result = (double*)malloc(times * sizeof(double));

  for (int i = 0; i < times; i++) {

    for (int i = 0; i < (N * N); i++) {
      A[i] = rand() * 1.0 / RAND_MAX;
      B[i] = rand() * 1.0 / RAND_MAX;
    }
    //    cout << "A is: " << endl;
    //    PrintMat(A, 0, 3, 0, 3, N);
    //    cout << "B is: " << endl;
    //
    //    PrintMat(B, 0, 3, 0, 3, N);
    auto status =
        cudaMemcpy(Agpu, A, N * N * sizeof(real), cudaMemcpyHostToDevice);
    if (status != cudaSuccess) {
      cout << cudaGetErrorString(status) << endl;
    };
    status = cudaMemcpy(Bgpu, B, N * N * sizeof(real), cudaMemcpyHostToDevice);
    if (status != cudaSuccess) {
      cout << cudaGetErrorString(status) << endl;
    };
    auto start = chrono::steady_clock::now();
    mult(Agpu, Bgpu, Cgpu, N);
    auto stop = chrono::steady_clock::now();
    auto diff = stop - start;
    double dif = chrono::duration<double, milli>(diff).count();
    sum += dif;
    result[i] = dif;
    status = cudaMemcpy(B, Cgpu, N * N * sizeof(real), cudaMemcpyDeviceToHost);
    if (status != cudaSuccess) {
      cout << cudaGetErrorString(status) << endl;
    };
    //    cout << "C is: " << endl;
    //
    //    PrintMat(B, 0, 3, 0, 3, N);
  }
  sum /= times;

  double ssd = 0;
  for (int j = 0; j < times; j++) {
    ssd += pow((sum - result[j]), 2) / (times - 1);
  }
  ssd = pow(ssd, 0.5) / pow(times, 0.5);

    printf("Kernel $\\#$ &%d x 1 &%.2f+-%.2f &%.2f+-%.2f\n", dim, sum, ssd, cpu_t/sum, abs(cpu_t / sum - (cpu_t+cpu_err)/(sum-ssd)));

  return sum;
}

//printf("Matrix A:\n\n");
//PrintMat(A,0,N,0,N,N);
//printf("Matrix B\n\n");
//PrintMat(B,0,N,0,N,N);
//printf("OUT:\n\n");
//  PrintMat(C, 0, 7, 0, 7, N);

//void gpu_1(real* A, real* B, real* C, real* AT, real* BT, real* Cres,
//           real* Agpu, real* Bgpu, real* ATgpu, real* BTgpu, real* Cgpu,
//           int N) {
//  // we start business here
//
//  auto start = chrono::steady_clock::now();
//
//  auto status =
//      cudaMemcpy(Agpu, A, N * N * sizeof(real), cudaMemcpyHostToDevice);
//  if (status != cudaSuccess) {
//    cout << cudaGetErrorString(status) << endl;
//  };
//  printf("7\n");
//  status = cudaMemcpy(BTgpu, BT, N * N * sizeof(real), cudaMemcpyHostToDevice);
//  if (status != cudaSuccess) {
//    cout << cudaGetErrorString(status) << endl;
//  };
//  printf("8\n");
//  dim3 threads(DIM, DIM, 1);
//  dim3 blocks((N - 1) / DIM + 1, (N - 1) / DIM + 1, 1);
//
//  auto start2 = chrono::steady_clock::now();
//  // MatrixMultGPUv1<<<blocks, threads>>>(Agpu, BTgpu, Cgpu, N);
//
//  status = cudaDeviceSynchronize();
//  if (status != cudaSuccess) {
//    cout << cudaGetErrorString(status) << endl;
//  };
//  auto stop2 = chrono::steady_clock::now();
//  printf("10\n");
//
//  status = cudaMemcpy(Cres, Cgpu, N * N * sizeof(real), cudaMemcpyDeviceToHost);
//  if (status != cudaSuccess) {
//    cout << cudaGetErrorString(status) << endl;
//  };
//  printf("11\n");
//  auto stop = chrono::steady_clock::now();
//  auto diff = start2 - start;
//  cout << "cudaMemcpy -> GPU:  "
//       << chrono::duration<double, milli>(diff).count() << " ms" << endl;
//  diff = stop2 - start2;
//  cout << "Multiply GPU: " << chrono::duration<double, milli>(diff).count()
//       << " ms" << endl;
//  diff = stop - stop2;
//  //Multiply_gpu_1(32, 32, Agpu, BTgpu, Cgpu, N);
//  std::cout << "Averaged Multiply GPU: "
//            << get_avg_time_gpu(10, 32, 32, Multiply_gpu_1, A, BT, Agpu, BTgpu,
//                                Cgpu, N)
//            << endl;
//  cout << "cudaMemcpy -> host " << chrono::duration<double, milli>(diff).count()
//       << " ms" << endl;
//  diff = stop - start;
//  cout << "All GPU: " << chrono::duration<double, milli>(diff).count() << " ms"
//       << endl;
//
////  PrintMat(Cres, 0, 7, 0, 7, N);
//}

int main(int argc, char** argv) {

  int N;
  real *A, *AT;
  real *B, *BT;
  real *C, *Cres;
  cudaError_t status;
  real* Agpu;
  real* ATgpu;
  real* Bgpu;
  real* BTgpu;
  real* Cgpu;

  if (argc != 2) {
    printf("Usage %s  N, where N is size of the square matrix\n", argv[0]);
  } else
    printf("%i\n", atoi(argv[1]));
  N = atoi(argv[1]);
  srand(777);

  A = (real*)malloc(sizeof(real) * N * N);
  AT = (real*)malloc(sizeof(real) * N * N);
  B = (real*)malloc(sizeof(real) * N * N);
  BT = (real*)malloc(sizeof(real) * N * N);
  C = (real*)malloc(sizeof(real) * N * N);
  Cres = (real*)malloc(sizeof(real) * N * N);
  status = cudaMalloc((void**)&Agpu, N * N * sizeof(real));
  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  }
  status = cudaMalloc((void**)&ATgpu, N * N * sizeof(real));
  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  }
  status = cudaMalloc((void**)&Bgpu, N * N * sizeof(real));
  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  }
  status = cudaMalloc((void**)&BTgpu, N * N * sizeof(real));
  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  }
  status = cudaMalloc((void**)&Cgpu, N * N * sizeof(real));
  if (status != cudaSuccess) {
    cout << cudaGetErrorString(status) << endl;
  }
  int times = 10;

  //cpu(A, B, C, AT, BT, Cres, N);

  // we set a seed for the random number generator to obain reproducible OUTs

  //gpu_1(A, B, C, AT, BT, Cres, Agpu, Bgpu, ATgpu, BTgpu, Cgpu, N);

  //  std::cout << "Averaged Multiply GPU_2: "
  //            << get_avg_time_gpu(10, 32, 32, Multiply_gpu_2, A, BT, Agpu, BTgpu,
  //                                Cgpu, N)
  //            << endl;
  std::cout << " CPU: " << std::endl;
  double cpu_t;
  double cpu_err;
  get_avg_time_cpu(&cpu_t, &cpu_err, times, A, B, C, N);
  get_avg_time_cpu(&cpu_t, &cpu_err, times, A, B, C, N);
  get_avg_time_cpu(&cpu_t, &cpu_err, times, A, B, C, N);
  std::cout << " GPU1: 8X8 " << std::endl;
  get_avg_time_gpu<8>(times * 3, Multiply_gpu_1<8>, cpu_t, cpu_err, A, BT,
                      Agpu, BTgpu, Cgpu, N);
  std::cout << " GPU1: 8X8 " << std::endl;
  get_avg_time_gpu<8>(times, Multiply_gpu_1<8>, cpu_t, cpu_err, A, BT,
                      Agpu, BTgpu, Cgpu, N);
  std::cout << " GPU1: 16X16 " << std::endl;

  get_avg_time_gpu<16>(times, Multiply_gpu_1<16>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);

  std::cout << " GPU1: 32X32" << std::endl;
  get_avg_time_gpu<32>(times, Multiply_gpu_1<32>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);

  std::cout << " GPU2: 32X1 " << std::endl;
  get_avg_time_gpu<32>(times, Multiply_gpu_2<32>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);

  std::cout << " GPU2: 64X1" << std::endl;
  get_avg_time_gpu<64>(times, Multiply_gpu_2<64>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);

  std::cout << " GPU2: 96X1" << std::endl;
  get_avg_time_gpu<96>(times, Multiply_gpu_2<96>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);
  
  std::cout << " GPU2: 128X1" << std::endl;
  get_avg_time_gpu<128>(times, Multiply_gpu_2<96>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);



  std::cout << " GPU3: 32X1" << std::endl;
  get_avg_time_gpu<32>(times, Multiply_gpu_3<32>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);

  std::cout << " GPU3: 64X1" << std::endl;
  get_avg_time_gpu<64>(times, Multiply_gpu_3<64>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);

  std::cout << " GPU4: 32X1" << std::endl;

  get_avg_time_gpu<32>(times, Multiply_gpu_4<32>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);
 std::cout << " GPU5: 32X1" << std::endl;
  get_avg_time_gpu<32>(times, Multiply_gpu_5<32>, cpu_t, cpu_err, A, BT, Agpu,
                       BTgpu, Cgpu, N);
}
