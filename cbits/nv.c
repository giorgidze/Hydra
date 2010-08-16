#include <nvector/nvector_serial.h>
#include <kinsol/kinsol.h>
#include <ida/ida.h>

N_Vector nvCreate (int n)
{
  return N_VNew_Serial(n);
}

void nvDestroy (N_Vector v)
{
  N_VDestroy_Serial(v);
  return;
}

int nvLength (N_Vector v)
{
  return NV_LENGTH_S(v);
}

double nvGet (N_Vector v, int i)
{
  return NV_Ith_S(v,i);
}

void nvSet (N_Vector v, int i, double d)
{
  NV_Ith_S(v,i) = d;
  return;
}

void nvConstant (N_Vector v, double d)
{
  N_VConst_Serial(d,v);
  return;
}

void nvCopy (N_Vector v1, N_Vector v2)
{
  int n = (NV_LENGTH_S(v1) < NV_LENGTH_S(v2)) ? (NV_LENGTH_S(v1)) : (NV_LENGTH_S(v2));
  int i;
  for (i = 0; i < n; i++) NV_Ith_S(v2,i) = NV_Ith_S(v1,i);
  return;
}

void idaFree (void* ida_mem)
{
  IDAFree(&ida_mem);
  return;
}
