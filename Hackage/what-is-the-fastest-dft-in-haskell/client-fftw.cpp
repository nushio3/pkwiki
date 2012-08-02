#include <cmath>
#include <complex>
#include <fftw3.h>
#include <iomanip>
#include <iostream>
#include <vector>

using namespace std;

const char* wisdom_filename = "fftw-cpp.wisdom";

int main () {
  int wisdom_flag;
  double x1,x2,y1,y2;
  int n, iteration;
  cin >> wisdom_flag >> x1 >> x2 >> y1 >> y2 >> n >> iteration;

  if (wisdom_flag) {
    fftw_import_wisdom_from_filename(wisdom_filename);
  }

  fftw_complex *fft_work_in = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * n);
  fftw_complex *fft_work_out = (fftw_complex*)fftw_malloc(sizeof(fftw_complex) * n);

  unsigned flags = (wisdom_flag ? FFTW_MEASURE : FFTW_ESTIMATE);
  fftw_plan fft_plan_fwd = fftw_plan_dft_1d(n, fft_work_in, fft_work_out, FFTW_FORWARD, flags);
  fftw_plan fft_plan_bwd = fftw_plan_dft_1d(n, fft_work_out, fft_work_in, FFTW_BACKWARD, flags);



  for (int i = 0; i < n; ++i) {
    double r = (i+0.5)/n;
    fft_work_in[i][0] = (x1<r && r<x2) ? 1 : 0;
    fft_work_in[i][1] = (r-y1)*(r-y2);
  }
  for (int ctr = 0; ctr < iteration; ++ctr) {
    fftw_execute(fft_plan_fwd);
    fftw_execute(fft_plan_bwd);
    for (int i = 0; i < n; ++i) {
      fft_work_in[i][0] /= n;
      fft_work_in[i][1] /= n;
    }
  }

  cout << setprecision(20);
  double sum = 0;
  for (int i = 0; i < n; ++i) {
    sum += pow(fft_work_in[i][0],2) + pow(fft_work_in[i][1],2);
  }
  cout << sum/n << endl;

  fftw_execute(fft_plan_fwd);

  sum = 0;
  for (int i = 0; i < n; ++i) {
    sum += pow(fft_work_out[i][0],2) + pow(fft_work_out[i][1],2);
  }
  cout << sum/n/n << endl;

  if (wisdom_flag) {
    fftw_export_wisdom_to_filename(wisdom_filename);
  }

}
