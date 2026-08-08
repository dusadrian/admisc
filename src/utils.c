#include <R.h>
#include <Rinternals.h>
#include <limits.h>
#include <pthread.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
    #include <windows.h>
#else
    #include <unistd.h>
#endif
#include "utils.h"


#define MIN_TASKS_PER_THREAD 1024


static inline unsigned long long int nchoosek(int n, int k) {
    if (k > n) return 0;
    if (k == 0 || k == n) return 1;

    unsigned long long int result = 1;

    if (k > n - k) {
        k = n - k;
    }

    for (int i = 0; i < k; i++) {
        if (result > ULLONG_MAX / (unsigned long long int) (n - i)) {
            return 0;
        }

        result *= (unsigned long long int) (n - i);

        if (result % (unsigned long long int) (i + 1) != 0) {
            return 0;
        }

        result /= (unsigned long long int) (i + 1);
    }

    return result;
}


typedef struct {
    int id;
    int nthreads;
    int nconds;
    int k;
    int nck;
    int ogte;
    int zerobased;
    int *out;
    int found;
} combination_job;


static int available_threads(void) {
    long nthreads = 1;

    #ifdef _WIN32
        SYSTEM_INFO info;
        GetSystemInfo(&info);
        nthreads = (long) info.dwNumberOfProcessors;
    #else
        long detected = sysconf(_SC_NPROCESSORS_ONLN);
        if (detected > 0) {
            nthreads = detected;
        }
    #endif

    if (nthreads > INT_MAX) {
        nthreads = INT_MAX;
    }
    if (nthreads < 1) {
        nthreads = 1;
    }

    return (int) nthreads;
}


static void *generate_combinations(void *data) {
    combination_job *job = (combination_job *) data;
    int tempk[job->k > 0 ? job->k : 1];

    job->found = 0;

    for (int task = job->id; task < job->nck; task += job->nthreads) {
        unsigned long long int combination = (unsigned long long int) task;
        int x = 0;

        for (int i = 0; i < job->k; i++) {
            while (1) {
                unsigned long long int cval = nchoosek(
                    job->nconds - (x + 1),
                    job->k - (i + 1)
                );
                if (cval == 0 || cval > combination) {
                    break;
                }
                combination -= cval;
                x++;
            }

            if (x < 0) {
                x = 0;
            }
            if (x >= job->nconds) {
                x = job->nconds - 1;
            }

            tempk[i] = x;
            x++;
        }

        int keep = (job->ogte <= 0) || (tempk[job->k - 1] >= job->ogte);
        if (job->ogte > 0) {
            job->found += keep;
        }

        for (int i = 0; i < job->k; i++) {
            job->out[task * job->k + i] = tempk[i] + 1 - job->zerobased;
        }
    }

    return NULL;
}


void increment(
    int k,
    int *e,
    int *h,
    int nconds,
    int *tempk,
    int minval
) {

    if (k == 1) {
        tempk[0] += 1;
    }
    else {
        if (*e < nconds - *h) {
            *h = 1;
            tempk[k - 1] += 1;
            *e = tempk[k - 1];

            if (tempk[k - 1] < minval) {
                tempk[k - 1] = minval;
                *e = minval;
            }
        }
        else {
            *e = tempk[k - *h - 1] + 1;
            ++*h;

            Rboolean under = TRUE;
            for (int j = 0; j < *h; j++) {
                under = under && (*e + j < minval);
                tempk[k - *h + j] = *e + j;
            }

            if (under) {
                *h = 1;
                tempk[k - *h] = minval;
                *e = minval;
            }
        }
    }
}


SEXP C_ombnk(SEXP list) {
    // ogte = at least one value greater than or equal to
    int nconds, k, ogte, zerobased;

    nconds = INTEGER(VECTOR_ELT(list, 0))[0];
    k = INTEGER(VECTOR_ELT(list, 1))[0];
    ogte = INTEGER(VECTOR_ELT(list, 2))[0] - 1;
    zerobased = INTEGER(VECTOR_ELT(list, 3))[0];

    int nck = 1;
    for (int i = 1; i <= k; i++) {
        nck *= nconds - (k - i);
        nck /=  i;
    }

    SEXP out;
    out = PROTECT(allocMatrix(INTSXP, k, nck));
    int *p_out = INTEGER(out);

    int found = nck;

    if (ogte > 0) {
        found = 0;
    }

    int nthreads = available_threads();
    int useful_threads = nck / MIN_TASKS_PER_THREAD;
    if (useful_threads < 1) {
        useful_threads = 1;
    }
    if (nthreads > useful_threads) {
        nthreads = useful_threads;
    }

    combination_job *jobs = (combination_job *) calloc(
        (size_t) nthreads,
        sizeof(combination_job)
    );
    pthread_t *threads = (pthread_t *) calloc(
        (size_t) nthreads,
        sizeof(pthread_t)
    );
    int *started = (int *) calloc((size_t) nthreads, sizeof(int));

    if (jobs == NULL || threads == NULL || started == NULL) {
        free(started);
        free(threads);
        free(jobs);
        UNPROTECT(1);
        Rf_error("Unable to allocate pthread worker data.");
    }

    for (int i = 0; i < nthreads; i++) {
        jobs[i].id = i;
        jobs[i].nthreads = nthreads;
        jobs[i].nconds = nconds;
        jobs[i].k = k;
        jobs[i].nck = nck;
        jobs[i].ogte = ogte;
        jobs[i].zerobased = zerobased;
        jobs[i].out = p_out;

        if (i > 0 && pthread_create(
            &threads[i],
            NULL,
            generate_combinations,
            &jobs[i]
        ) == 0) {
            started[i] = 1;
        }
    }

    generate_combinations(&jobs[0]);

    for (int i = 1; i < nthreads; i++) {
        if (started[i]) {
            pthread_join(threads[i], NULL);
        }
        else {
            generate_combinations(&jobs[i]);
        }

        if (ogte > 0) {
            found += jobs[i].found;
        }
    }

    if (ogte > 0) {
        found += jobs[0].found;
    }

    free(started);
    free(threads);
    free(jobs);

    R_CheckUserInterrupt();

    if (ogte > 0 && found < nck) {
        SEXP copy = PROTECT(duplicate(out));
        int *p_copy = INTEGER(copy);

        out = PROTECT(allocMatrix(INTSXP, k, found));
        p_out = INTEGER(out);

        int col = 0;
        for (int task = 0; task < nck; task++) {
            if (task > 0 && task % 1024 == 0) {
                R_CheckUserInterrupt();
            }
            int last = p_copy[task * k + k - 1] + zerobased - 1;
            if (last >= ogte) {
                memcpy(&p_out[col * k], &p_copy[task * k], (size_t) k * sizeof(int));
                col++;
            }
        }

        UNPROTECT(3);
        return(out);
    }

    UNPROTECT(1);
    return(out);

}
