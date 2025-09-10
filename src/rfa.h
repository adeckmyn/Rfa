#include<stdlib.h>
#include<stdio.h>
#include<math.h>
#include<string.h>
#include<inttypes.h>
#include<R.h>

// Some utilities

#define MAX(x,y) (x>y ? x : y)

// a simple test for big/little endian platforms
//#define LITTLE_ENDIAN ( *(uint16_t*)"a" < 255)

// macros for reading big-endian integers from a bit stream
// platform-independent (32bit int)
// up to four bytes, we can rely on automatic integer promotion
#define INT2(x) ( ((x)[0]<<8) + (x)[1])
#define INT3(x) ( ((x)[0]<<16) + ((x)[1]<<8) + (x)[2])
#define INT4(x) ( ((x)[0]<<24) + ((x)[1]<<16) + ((x)[2]<<8) + (x)[3])

// a few prototypes
int64_t INT8(unsigned char* x);
double DBL8(unsigned char * x);
void byteswap(void* data,int size,int n);
void fa_byteswap(void* data, int* size, int* n);
int fa_mmax(int n,int nsmax,int nmsmax);
void fa_rawreorder(double* fieldin, double* fieldout,int* nmsmax, int* nsmax);
void fa_countval_spec(int* nmsmax,int *nsmax,int* sptrunc,int*result);
double fast_pow(double x, int n);
double IBMfloat(unsigned char*x);

// main function prototypes
// FILE PARSING
void fa_fastfind_name(char **filename, double *tar_offset,char **fnm, char **fname,
                      double *foffset, int *flen, int *findex,int *err);
void fa_fastfind_mem(unsigned char *membuffer, int *bufsize,
                     char **fnm, char **fname,
                     double *foffset, int *flen, int *findex,int *err);
void fa_fastfind(FILE* fafile,  double *tar_offset,char **fnm, char **fname,
                      double *foffset, int *flen, int *findex,int *err);

void fa_parse_name(char** filename, double* tar_offset,
		   int* ninfields,
                   char** fnames, double* foffset, int* flen, int*findex,
                   int* spectral, int* ngrib, int* nbits,int* sptrunc, int* sppow,
                   double* hoffset, int* hlen, int*hindex, int* lparse, int* err);
void fa_parse_mem(unsigned char* membuffer, int* bufsize,
                  int* ninfields,
                  char** fnames, double* foffset, int* flen, int*findex,
                  int* spectral, int* ngrib, int* nbits, int* sptrunc, int* sppow,
                  double* hoffset, int* hlen, int*hindex, int* lparse, int* err);
void fa_parse(FILE* fafile, double* tar_offset,
	      int* ninfields,
              char** fnames, double* foffset,int* flen, int*findex,
              int* spectral, int* ngrib, int* nbits, int* sptrunc, int* sppow,
              double* hoffset, int* hlen, int*hindex, int* lparse, int* err);

// DECODING
void fa_grib_expand(unsigned char*inbuf,int nbits, int nval, double *fieldout,
                    double minval, double maxval, double scale);
void fa_spectral_combine(double* data1,double* data2,double* data,
                         int nsmax,int nmsmax,
                         int sptrunc,int sppow,int* ERR);
void fa_spectral_order(double* data,int* nmsmax,int* nsmax,
                       int* nx,int* ny, Rcomplex* fftdata);
void fa_grib0(unsigned char* grib,int griblen,int nval,double* values,
              double minval, double maxval,int* ERR);
void fa_decode(unsigned char* ibuf,int* buflen,double*data,int* ndata,
               int* nsmax,int* nmsmax,int* ERR);

// LINEAR SMOOTHING
void smooth_extension(double* data, int* nx, int* ny, int* maxx, int* maxy);

// ENCODING
void INT8w(unsigned char* x, int64_t ll);
void DBL8w(unsigned char * x, double val);
void fa_message_length_spectral(int *nmsmax, int *nsmax,
                                int *nbits, int *sptrunc, int* result);
void fa_grib_squeeze(unsigned char*bitstream,int streamlen,int nbits,
                     double*data, int nval, double minval, double maxval);
void fa_spectral_split(double* data1,double* data2,double* data,
                       int nsmax,int nmsmax,
                       int sptrunc,int sppow,int* ERR);
void fa_spectral_order_inv(double* data, int* nmsmax, int* nsmax,
                           int* nx, int* ny, Rcomplex* fftdata);
void fa_grib0_write(unsigned char* grib,int griblen,double* values,int nval,
                    int nbits, double minval, double maxval, int* ERR);
void fa_encode(unsigned char* obuf,int* buflen,double*data,int* ndata,
               int* nbits,int* sptrunc,int *spectral,int* lgrib,int*pow,
               int* nsmax,int* nmsmax,int*ndgl,int*ndlon,int* ERR);



