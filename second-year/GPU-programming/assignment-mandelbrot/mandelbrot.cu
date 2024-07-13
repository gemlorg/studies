//
//  Mandelbrot.cpp
//
//
//  Created by Witold Rudnicki
//
// Kompilacja
// c++ -o mandel_cpu Mandelbrot.cpp -L /usr/local/lib -l PNGwriter -l png
//

#define NO_FREETYPE

#include <math.h>
#include <pngwriter.h>

#include <chrono>
#include <iostream>

#include "Mandelbrot.h"
using namespace std;
#include <sys/time.h>

#include <iterator>
#include <numeric>

__global__ void computeMandelbrotDD(real X0, real Y0, real X1, real Y1, int POZ,
                                    int PION, int ITER, int* Mandel) {
  double dX = (X1 - X0) / (POZ - 1);
  double dY = (Y1 - Y0) / (PION - 1);
  double x, y, Zx, Zy, tZx;
  int SUM = 0;
  int SIZE = POZ * PION;
  //change places???
  int pion = blockIdx.x * blockDim.x + threadIdx.x;
  int poz = blockIdx.y * blockDim.y + threadIdx.y;
  int iter = 0;

  Zx = dX * poz + X0;
  Zy = dY * pion + Y0;
  x = 0;
  y = 0;
  if (pion * POZ + poz < POZ * PION) {
    while (x * x + y * y < 4 && iter < ITER) {
      double a = x * x - y * y + Zx;
      double b = x * y * 2 + Zy;
      x = a;
      y = b;
      iter++;
      SUM++;
    }
    Mandel[pion * POZ + poz] = iter;
  }
}

__global__ void computeMandelbrotD(real X0, real Y0, real X1, real Y1, int POZ,
                                   int PION, int ITER, int* Mandel) {
  double dX = (X1 - X0) / (POZ - 1);
  double dY = (Y1 - Y0) / (PION - 1);
  double x, y, Zx, Zy, tZx;
  int SUM = 0;
  int i = blockIdx.x * blockDim.x + threadIdx.x;
  int SIZE = POZ * PION;
  int pion = i % POZ;
  int poz = i / POZ;
  int iter = 0;

  Zx = dX * poz + X0;
  Zy = dY * pion + Y0;
  x = 0;
  y = 0;
  while (x * x + y * y < 4 && iter < ITER) {
    double a = x * x - y * y + Zx;
    double b = x * y * 2 + Zy;
    x = a;
    y = b;
    iter++;
    SUM++;
  }
  Mandel[pion * POZ + poz] = iter;
}

int compare(const void* a, const void* b) {
  return (*(double*)a - *(double*)b);
}

double get_time_for_creation(real X0, real Y0, real X1, real Y1, int POZ,
                             int PION, int ITER, int* Iters) {
  auto start2 = chrono::steady_clock::now();
  int SUM = computeMandelbrot(X0, Y0, X1, Y1, POZ, PION, ITER, Iters);
  auto stop = chrono::steady_clock::now();
  auto diff = stop - start2;
  double dif = chrono::duration<double, milli>(diff).count();
  return (double)dif;
}

int computeMandelbrot(real X0, real Y0, real X1, real Y1, int POZ, int PION,
                      int ITER, int* Mandel) {
  double dX = (X1 - X0) / (POZ - 1);
  double dY = (Y1 - Y0) / (PION - 1);
  double x, y, Zx, Zy, tZx;
  int SUM = 0;
  int i;
  int SIZE = POZ * PION;
  int pion, poz;
  int index = 0;
  for (pion = 0; pion < PION; pion++) {
    for (poz = 0; poz < POZ; poz++) {
      int i = 0;
      Zx = dX * poz + X0;
      Zy = dY * pion + Y0;
      x = 0;
      y = 0;
      while (x * x + y * y < 4 && i < ITER) {
        double a = x * x - y * y + Zx;
        double b = x * y * 2 + Zy;
        x = a;
        y = b;
        i++;
        SUM++;
      }
      Mandel[pion * POZ + poz] = i;
    }
  }
  return SUM;
}

void compute_cpu(real X0, real Y0, real X1, real Y1, int POZ, int PION,
                 int ITER, int out, double* a) {
  int* Iters = (int*)malloc(sizeof(int) * POZ * PION);

  //table for multiple resulrs
  double* result = (double*)malloc(20 * sizeof(double));
  double t = 0;
  double v = 0;
  double s = 0;

  double minn = 10000000;
  double maxx = 0;

  for (int i = 0; i < 20; i++) {
    result[i] =
        get_time_for_creation(X0, Y0, X1, Y1, POZ, PION, ITER, Iters) * 100;
    maxx = max(maxx, result[i]);
    minn = min(minn, result[i]);
    t += result[i];
    //     std::cout << "result #" << i << " time: " << result[i] << std::endl;
  }
  t /= 20;
  for (int i = 0; i < 20; i++) {
    v += pow((t - result[i]), 2);
  }
  v /= 19;
  s = sqrt(v) / sqrt(20);
  qsort(result, 20, sizeof(double), compare);
  if (out == 1) {
    std::cout << "average result: " << t << " +/- " << s << std::endl;
    std::cout << "min: " << minn << std::endl;
    std::cout << "mean: " << result[9] << std::endl;
  }
  a[0] = t;
  a[1] = s;
}

void compute_gpu(real X0, real Y0, real X1, real Y1, int POZ, int PION,
                 int ITER, int TIMES, int* Iters_gpu, double ORIG, double FRAC,
                 int d1, int d2) {
  double * result = (double*) malloc(TIMES * sizeof(double));
 //printf("calculating for %d D %d \n", d1, d2);
    dim3 blockSize{d1, d2, 1};
    dim3 blockCount{POZ / blockSize.x + 1, PION / blockSize.y + 1, 1};
    double sum = 0;
    for (int j = 0; j < TIMES; j++) {
      auto start2 = chrono::steady_clock::now();
      computeMandelbrotDD<<<blockCount, blockSize>>>(X0, Y0, X1, Y1, POZ, PION,
                                                     ITER, Iters_gpu);
      cudaDeviceSynchronize();
      auto stop = chrono::steady_clock::now();
      auto diff = stop - start2;
      double dif = chrono::duration<double, milli>(diff).count();
      // printf("iteration #%d for %d D %d took %.2f ms\n", j,TOTAL/i, i, dif);
      result[j] = dif;
      sum += dif;
    }
    qsort(result, TIMES, sizeof(double), compare);
    sum /= TIMES;
    double ssd = 0;
    for (int j = 0; j < TIMES; j++) {
      ssd += pow((sum - result[j]), 2) / (TIMES - 1);
    }
    ssd = pow(ssd, 0.5) / pow(TIMES, 0.5);

    /* printf("result for %d D %d: \n", d1, d2); */
    /* printf("median: %.2f\n", result[TIMES / 2]); */
    /* printf("min: %.2f\n", result[0]); */
    /* printf("average: %.2f +/- %.2f\n", sum, ssd); */
   /* printf("speedup: %.2f +/- %.2f\n", ORIG / sum,  (ORIG + FRAC) / (sum - ssd) -  ORIG / sum); */
    printf("%d& %d& %.2f& %.2f& %.2f +/- %.2f& %.2f +/- %.2f \\\n", d1, d2, result[TIMES / 2], result[0], sum, ssd, ORIG/sum,  (ORIG + FRAC) / (sum - ssd) -  ORIG / sum);
 }

int main(int argc, char** argv) {
  struct timeval T0, T1;
  time_t start, end;
  struct timeval t0, t1;

  gettimeofday(&T0, NULL);
  if (argc != 9) {
    printf(
        "Wywołanie %s LD_Re, LD_Im, PG_Re, PG_Im, Poziom, Pion, Iteracje, "
        "TIMES\n ",
        argv[0]);
    exit(1);
  }
  //Ustaw obszar obliczeń {X0,Y0} - lewy dolny róg
  double X0 = atof(argv[1]);
  double Y0 = atof(argv[2]);
  //{X1,Y1} - prawy górny róg
  double X1 = atof(argv[3]);
  double Y1 = atof(argv[4]);
  //Ustal rozmiar w pikselach {POZ,PION}
  int POZ = atoi(argv[5]);
  int PION = atoi(argv[6]);
  //Ustal liczbę iteracji próbkowania {ITER}
  int ITER = atoi(argv[7]);
  int TIMES = atoi(argv[8]);

  //Zaalokuj tablicę do przechowywania wyniku

  int* Iters_gpu = nullptr;
  cudaError_t status;
  status = cudaMalloc((void**)&Iters_gpu, sizeof(int) * POZ * PION);
  if (status != cudaSuccess) {
    std::cout << cudaGetErrorString(status) << std::endl;
    return 1;
  }
for(int a = 0; a < 2; a++){
  double* result = (double*)calloc(TIMES, sizeof(double));
  double* arr = (double*)malloc(2 * sizeof(double));
  for (int k = 0; k < 12; k++) {
    compute_cpu(X0, Y0, X1, Y1, POZ / 10, PION / 10, ITER, 1, arr);
  }
  double ORIG = arr[0];
  double FRAC = arr[1];
  while(FRAC > 50) {
    compute_cpu(X0, Y0, X1, Y1, POZ / 10, PION / 10, ITER, 1, arr);
    ORIG = arr[0];
    FRAC = arr[1];
  }
  // do computations
  printf("Computations for rectangle { (%lf %lf), (%lf %lf) }\n", X0, Y0, X1,
         Y1);

  printf("1d calculations: \n");
  for (int i = 32; i <= 1024; i *= 2) {
    compute_gpu(X0, Y0, X1, Y1, POZ, PION, ITER, TIMES, Iters_gpu, ORIG, FRAC, i,
              1);


  }

  int TOTAL = 256;
  printf("2d calculations: \n");
  for (int i = 1; i <= TOTAL; i *= 2) {
    compute_gpu(X0, Y0, X1, Y1, POZ, PION, ITER, TIMES, Iters_gpu, ORIG, FRAC, TOTAL / i,
              i);

  }

  TOTAL = 1024;
  printf("2d calculations: \n");
  for (int i = 1; i <= TOTAL; i *= 2) {
  compute_gpu(X0, Y0, X1, Y1, POZ, PION, ITER, TIMES, Iters_gpu, ORIG, FRAC, TOTAL / i,
              i);
  }

  for (int k = 32; k >= 8; k /= 2) {
    compute_gpu(X0, Y0, X1, Y1, POZ, PION, ITER, TIMES, Iters_gpu, ORIG, FRAC,
                k, k);
  }

  compute_gpu(X0, Y0, X1, Y1, POZ, PION, ITER, TIMES, Iters_gpu, ORIG, FRAC, 32,
              16);

  compute_gpu(X0, Y0, X1, Y1, POZ, PION, ITER, TIMES, Iters_gpu, ORIG, FRAC, 64,
              8);

  compute_gpu(X0, Y0, X1, Y1, POZ, PION, ITER, TIMES, Iters_gpu, ORIG, FRAC, 8,
              64);

  compute_gpu(X0, Y0, X1, Y1, POZ, PION, ITER, TIMES, Iters_gpu, ORIG, FRAC, 16,
              32);

  /**/
  /*        auto start2 = chrono::steady_clock::now();  */
  /* gettimeofday(&t0,NULL); */
  /* dim3 blockSize{32, 1, 1}; */
  /* dim3 blockCount{POZ * PION / (blockSize.x * blockSize.y) + 1, 1, 1}; */
  /**/
  /* computeMandelbrot<<<blockCount, blockSize>>>(X0, Y0, X1, Y1, POZ, PION, ITER, Iters_gpu); */
  /* gettimeofday(&t1,NULL); */
  /*     end=clock(); */
  /* auto diff = stop - start2; */
  /**/
  /* cout << chrono::duration <double, milli> (diff).count() << " ms" << endl; */
  /* cout << chrono::duration <double, micro> (diff).count() << " us" << endl; */
  /* cout << chrono::duration <double, nano> (diff).count() << " ns" << endl; */
  /**/
  /* printf("\nTotal %d iterations took %lf s\n\n",1,1.0*(end-start)/CLOCKS_PER_SEC); */
  /* printf("Elapsed time %12.6lf s\n\n",(t1.tv_sec-t0.tv_sec)+1e-6*(t1.tv_usec-t0.tv_usec)); */

}
}

