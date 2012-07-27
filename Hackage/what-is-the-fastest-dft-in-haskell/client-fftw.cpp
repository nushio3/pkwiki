#include <cmath>
#include <complex>
#include <fftw3.h>
#include <iostream>
#include <vector>

using namespace std;

int main () {
  int wisdom_flag;
  double x1,x2,y1,y2;
  int n, iteration;
  cin >> wisdom_flag >> x1 >> x2 >> y1 >> y2 >> n >> iteration;

  fftw_complex *fft_work_in = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * n);
  fftw_complex *fft_work_out = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * n);

  fftw_plan fft_plan_fwd = fftw_plan_dft_1d(n, fft_work_in, fft_work_out, FFTW_FORWARD, FFTW_ESTIMATE);
  fftw_plan fft_plan_bwd = fftw_plan_dft_1d(n, fft_work_out, fft_work_in, FFTW_BACKWARD, FFTW_ESTIMATE);

  for (int i = 0; i < n; ++i) {
    double r = double(i)/n;
    fft_work_in[i][0] = (x1<r && r<x2) ? 1 : 0;
    fft_work_in[i][1] = r*(r-y1)*(r-y2)*(r-1);
  }
  for (int ctr = 0; ctr < iteration; ++ctr) {
    fftw_execute(fft_plan_fwd);
    fftw_execute(fft_plan_bwd);
    for (int i = 0; i < n; ++i) {
      fft_work_in[i][0] /= n;
      fft_work_in[i][1] /= n;
    }
  }
  fftw_execute(fft_plan_fwd);
  double sum = 0;
  for (int i = 0; i < n; ++i) {
    sum += pow(fft_work_out[i][0],2) + pow(fft_work_out[i][1],2);
  }
  cout << sum/n/n << endl;

  fftw_execute(fft_plan_bwd);
  sum = 0;
  for (int i = 0; i < n; ++i) {
    sum += pow(fft_work_in[i][0],2) + pow(fft_work_in[i][1],2);
  }
  cout << sum/n/n/n << endl;
}
