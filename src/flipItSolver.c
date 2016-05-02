#include "flipItSolver.h"

int di[5] = {0, 1, 0, -1,  0};
int dj[5] = {0, 0, 1,  0, -1};

// Ax = b (mod q) を x について解く
// ガウスの消去法を使用する
// A は FlipIt の隣接行列
// 0 = 解無し， 1 = 解有り
int solve(int *x, int *b, int n, int q) {

  int i, j, k, **a = (int**)malloc(sizeof(int*) * n * n);

  for (i = 0; i < n * n; ++i) {
    a[i] = (int*)malloc(sizeof(int) * n * n + 1);
  }

  for (i = 0; i < n * n; ++i)
    for (j = 0; j < n * n + 1; ++j)
      a[i][j] = 0;

  for (i = 0; i < n; ++i)
    for (j = 0; j < n; ++j)
      for (k = 0; k < 5; ++k)
        if (0 <= i + di[k] && i + di[k] < n && 0 <= j + dj[k] && j + dj[k] < n) {
          a[n * i + j][n * (i + di[k]) + (j + dj[k])] = 1;
        }

  for (i = 0; i < n * n; ++i)
    a[i][n * n] = b[i];

  return gauss(a, x, n * n, n * n + 1, q);
}
