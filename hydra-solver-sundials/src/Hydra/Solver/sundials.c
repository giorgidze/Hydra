#include <stdlib.h>
#include <math.h>

#include <ida/ida.h>
#include <ida/ida_dense.h>
#include <nvector/nvector_serial.h>

double reltol = 0;
double abstol = 1.0E-6;

struct Solver_Handle
{
    void* ida_mem;
    double* time;
    N_Vector yy;
    N_Vector yp;
    N_Vector id;
    int event_number;
    int* events;
};

void (*residual_init_global)  (double, double*, double*, double*);
void (*residual_main_global)  (double, double*, double*, double*);
void (*residual_event_global) (double, double*, double*, double*);

int residual_init_local (double tt, N_Vector yy, N_Vector yp, N_Vector rr, void* user_data)
{
    residual_init_global (tt, NV_DATA_S(yy), NV_DATA_S(yp), NV_DATA_S(rr));
    return 0;
}


int residual_main_local (double tt, N_Vector yy, N_Vector yp, N_Vector rr, void* user_data)
{
    residual_main_global (tt, NV_DATA_S(yy), NV_DATA_S(yp), NV_DATA_S(rr));
    return 0;
}


int residual_event_local (double tt, N_Vector yy, N_Vector yp, double* rr, void* user_data)
{
    residual_event_global (tt, NV_DATA_S(yy), NV_DATA_S(yp), rr);
    return 0;
}


void adjust_zero_crossings (N_Vector v)
{
    int i;
    for (i = 0; i < NV_LENGTH_S(v); i++)
        if (fabs(NV_Ith_S(v,i)) < abstol) NV_Ith_S(v,i) = 0;

    return;
}


struct Solver_Handle* create_solver
( double time_start
, double time_stop
, double* time

, int variable_number
, double* variables
, double* differentials
, int* alg_or_diff

, int event_number
, int* events

, void (*residual_init)  (double, double*, double*, double*)
, void (*residual_main)  (double, double*, double*, double*)
, void (*residual_event) (double, double*, double*, double*)
)
{
    residual_init_global = residual_init;
    residual_main_global = residual_main;
    residual_event_global = residual_event;

    struct Solver_Handle* solver_handle = malloc (sizeof(struct Solver_Handle));
    solver_handle->time = time;
    solver_handle->event_number = event_number;
    solver_handle->events = events;

    int flag = 0;
    int i = 0;

    solver_handle->yy = N_VMake_Serial(variable_number, variables);
    solver_handle->yp = N_VMake_Serial(variable_number, differentials);

    solver_handle->id = N_VNew_Serial(variable_number);
    for (i = 0; i < variable_number; i++)
        NV_Ith_S(solver_handle->id,i) = (alg_or_diff[i] == 0) ? 0.0 : 1.0;

    solver_handle->ida_mem = IDACreate();
    if (solver_handle->ida_mem == NULL) exit(1);

    flag = IDAInit (solver_handle->ida_mem
        , residual_main_local
        , time_start, solver_handle->yy
        , solver_handle->yp);
    if (flag != IDA_SUCCESS) exit (1);

    flag = IDASStolerances(solver_handle->ida_mem, reltol, abstol);
    if (flag != IDA_SUCCESS) exit (1);

    flag = IDASetId(solver_handle->ida_mem, solver_handle->id);
    if (flag != IDA_SUCCESS) exit (1);

    flag = IDASetSuppressAlg(solver_handle->ida_mem, FALSE);
    if (flag != IDA_SUCCESS) exit (1);

    flag = IDADense(solver_handle->ida_mem, variable_number);
    if (flag != IDADLS_SUCCESS) exit (1);

    flag = IDACalcIC(solver_handle->ida_mem, IDA_YA_YDP_INIT, time_start + abstol);
    if (flag != IDA_SUCCESS) exit (1);

    flag = IDARootInit(solver_handle->ida_mem, event_number, residual_event_local);
    if (flag != IDA_SUCCESS) exit (1);

    flag = IDASetStopTime(solver_handle->ida_mem, time_stop);
    if (flag != IDA_SUCCESS) exit (1);

    return solver_handle;
}


void destroy_solver (struct Solver_Handle* solver_handle)
{
    IDAFree(&(solver_handle->ida_mem));
    N_VDestroy_Serial(solver_handle->yy);
    N_VDestroy_Serial(solver_handle->yp);
    N_VDestroy_Serial(solver_handle->id);
    free(solver_handle);
    return;
}


int solve ( struct Solver_Handle* solver_handle)
{
    int flag;

    if (solver_handle->event_number > 0)
    {
        long int ngevals;
        flag = IDAGetNumGEvals(solver_handle->ida_mem, &ngevals);
        if (flag != IDA_SUCCESS) exit (1);

        if (ngevals == 0)
        {
            flag = IDASolve (solver_handle->ida_mem
                , *(solver_handle->time)
                , solver_handle->time
                , solver_handle->yy
                , solver_handle->yp
                , IDA_ONE_STEP);

            switch (flag)
            {
                case IDA_SUCCESS:      return 0;
                case IDA_ROOT_RETURN:  return 0;
                case IDA_TSTOP_RETURN: return 2;
                default:               exit(1);
            }
        }
    }

    flag = IDASolve (solver_handle->ida_mem
        , *(solver_handle->time)
        , solver_handle->time
        , solver_handle->yy
        , solver_handle->yp
        , IDA_NORMAL);

    switch (flag)
    {
        case IDA_SUCCESS:
            return 0;

        case IDA_ROOT_RETURN:
            flag = IDAGetRootInfo(solver_handle->ida_mem, solver_handle->events);
            if (flag != IDA_SUCCESS) exit (1);
            adjust_zero_crossings(solver_handle->yy);
            adjust_zero_crossings(solver_handle->yp);
            return 1;

        case IDA_TSTOP_RETURN:
            return 2;

        default: exit(1);
    }
}
