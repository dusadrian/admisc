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


/* R matrix dimensions are ints. Saturate counts beyond the largest rank. */
#define COUNT_LIMIT ((long long) INT_MAX + 1)

static long long choose_count(int n, int k) {
    if (k < 0 || k > n) return 0;
    if (k > n - k) k = n - k;
    long long count = 1;
    for (int i = 1; i <= k; ++i) {
        count = count * (n - k + i) / i;
        if (count >= COUNT_LIMIT) return COUNT_LIMIT;
    }
    return count;
}

/* Count k-subsets with at least one member among the largest high values. */
static long long eligible_count(int n, int k, int high) {
    if (high >= n || k > n - high) return choose_count(n, k);
    int limit = k < high ? k : high;
    long long total = 0;
    for (int j = 1; j <= limit; ++j) {
        total += choose_count(high, j) * choose_count(n - high, k - j);
        if (total >= COUNT_LIMIT) return COUNT_LIMIT;
    }
    return total;
}

typedef struct {
    int n, k, minimum, offset;
    int start, end;
    int *out;
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
    combination_job *job = data;
    int k = job->k, n = job->n;
    int *column = job->out + (R_xlen_t) job->start * k;
    long long rank = job->start;
    int next = 0;
    /* Unrank only the first column of this contiguous chunk. */
    for (int i = 0; i < k - 1; ++i) {
        while (rank > 0) {
            long long block = eligible_count(n - next - 1, k - i - 1,
                                              n - job->minimum);
            if (rank < block) break;
            rank -= block;
            ++next;
        }
        column[i] = next++ + job->offset;
    }
    column[k - 1] = (next > job->minimum ? next : job->minimum) +
                    (int) rank + job->offset;

    for (int task = job->start + 1; task < job->end; ++task) {
        int *previous = column;
        column += k;
        int i = k - 1;
        while (previous[i] - job->offset == n - (k - i)) --i;
        memcpy(column, previous, (size_t) i * sizeof(int));
        column[i] = previous[i] + 1;
        for (int j = i + 1; j < k; ++j) column[j] = column[j - 1] + 1;
        if (column[k - 1] - job->offset < job->minimum) {
            column[k - 1] = job->minimum + job->offset;
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
    int n = asInteger(VECTOR_ELT(list, 0));
    int k = asInteger(VECTOR_ELT(list, 1));
    int threshold = asInteger(VECTOR_ELT(list, 2));
    int zero = asInteger(VECTOR_ELT(list, 3));
    if (n < 0 || k < 0 || k > n || threshold == NA_INTEGER ||
        (zero != 0 && zero != 1)) Rf_error("Invalid combination parameters.");

    int minimum = threshold > 1 ? threshold - 1 : 0;
    long long count = k == 0 ? (minimum == 0) :
        eligible_count(n, k, minimum < n ? n - minimum : 0);
    if (count > INT_MAX) Rf_error("Too many combinations for an R matrix.");
    SEXP out = PROTECT(allocMatrix(INTSXP, k, (int) count));
    if (k == 1) {
        int *output = INTEGER(out);
        for (int i = 0; i < count; ++i) output[i] = minimum + i + (1 - zero);
    }
    else if (k > 0 && count > 0) {
        int workers = available_threads();
        int useful = (int) count / MIN_TASKS_PER_THREAD;
        if (useful < 1) useful = 1;
        if (workers > useful) workers = useful;

        combination_job *jobs = (combination_job *) R_alloc(workers, sizeof(*jobs));
        pthread_t *threads = (pthread_t *) R_alloc(workers, sizeof(*threads));
        int *started = (int *) R_alloc(workers, sizeof(*started));
        int *output = INTEGER(out);
        for (int i = 0; i < workers; ++i) {
            jobs[i] = (combination_job) {n, k, minimum, 1 - zero,
                (int) (count * i / workers), (int) (count * (i + 1) / workers), output};
            started[i] = i > 0 &&
                pthread_create(&threads[i], NULL, generate_combinations, &jobs[i]) == 0;
        }
        for (int i = 0; i < workers; ++i) {
            if (started[i]) pthread_join(threads[i], NULL);
            else generate_combinations(&jobs[i]);
        }
        R_CheckUserInterrupt();
    }
    UNPROTECT(1);
    return out;
}
