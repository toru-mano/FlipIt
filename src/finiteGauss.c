#include "finiteGauss.h"

// input : a, b
// output : x, y  s.t. ax + by = （符号付き）gcd(a, b)
int extGcd(int a, int b, int *x, int *y) {
  if (b == 0) {
    *x = 1; *y = 0; return a;
  }
  int g = extGcd(b, a % b, y, x);
  (*y) -= (a / b) * (*x);
  return g;
}

// xn = 1 (mod p)
int invMod(int n, int p) {
  int x, y, g = extGcd (n, p, &x, &y);
  if (g == 1) return x;
  else if (g == -1) return -x;
  else return 0; // gcd(n, p) != 1，解なし
}

// 有限体上の線型方程式系 Ax = b (mod q)を解く
// a = [A | b]: m × n の係数行列
// x: 解を記録するベクトル
// 計算量： O(min(m, n) * m * n)
int gauss(int **a, int *x, int m, int n, int q) {

  int rank = 0, i, j, k, l, *pivot = (int*)malloc(sizeof(int) * n);

  // 前進消去
  for (i = 0, j = 0; i < m && j < n-1; ++j) {

    int p = -1, tmp = 0;


    // ピボットを探す
    for (k = i; p < 0 && k < m; ++k) {
      if (a[k][j] != 0) p = k;  // 有限体上なので非零で十分
    }


    // ランク落ち対策
    if (p == -1) continue;


    // 第i行と第p行を入れ替える
    for (k = j; k < n; ++k)
      tmp = a[i][k], a[i][k] = a[p][k], a[p][k] = tmp;


    // 第i行を使って掃き出す
    for (k = i+1; k < m; ++k) {
      tmp = - a[k][j] * invMod(a[i][j], q) % q;
      for (l = j; l < n; ++l)
        a[k][l] = (a[k][l] + tmp * a[i][l]) % q;
    }


    // 第i行を正規化： a[i][j] = 1 にする
    tmp = invMod(a[i][j], q);
    for (k = j; k < n; ++k)
      a[i][k] = a[i][k] * tmp % q;

    pivot[i++] = j, rank++;
  }

  // 解の存在のチェック
  for (i = rank; i < m; ++i)
    if (a[i][n-1] != 0) {
      free(pivot);
      return 0;
    }

  // 解をxに代入（後退代入）
  for (i = 0; i < rank; ++i)
    x[i] = a[i][n-1];
  for (i = rank; i < n-1; ++i)
    x[i] = 0;
  for (i = rank-1; i >= 0; --i) {
    for (j = pivot[i] + 1; j < n-1; ++j)
      x[i] -= a[i][j] * x[j];
    x[i] -= x[i] / q * q, x[i] = (x[i] + q) % q;  // 0 <= x[i] < q に調整
  }

  free(pivot);
  return 1;
}
