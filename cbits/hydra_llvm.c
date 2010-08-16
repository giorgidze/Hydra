#include <nvector/nvector_serial.h>

double* hydra_nv_data(void* v)
{
  return (NV_DATA_S( ((N_Vector) v)));
}

double hydra_sgn (double d)
{
  return (d > 0.0) ? 1.0 : ((d < 0.0) ? (-1.0) : 0.0);
}
