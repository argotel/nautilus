/*
 * The author of this software is William Dorsey.
 * Copyright (c) 1993, 1994, 1995 by William Dorsey.  All rights reserved.
 *
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, THE AUTHOR DOES NOT MAKE ANY CLAIM OR
 * WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY OF THIS SOFTWARE OR
 * ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */

/* upsample.c -- multirate upsampling code (4/3)
 *
 * SCCS ID: @(#)upsample.c 1.2 96/03/01
 *
 * REVISION HISTORY
 *
 * DATE      RESPONSIBLE PARTY  DESCRIPTION
 * -------------------------------------------------------------------------
 * 93/11/12  B. Dorsey		Initial creation
 * 94/11/18  B. Dorsey		Integrated into Nautilus 0.2
 */

/*
 * Bandpass Filter:                        
 *           Freq.   Value   Weight  Lower Lim. Upper Lim.  Deviation  Dev. dB
 * Band  1: 0.0000   1.00      1.0       ----       ----     2.40E-02    0.21
 *          0.1125   1.00      1.0       ----       ----     2.40E-02    0.21
 * Band  2: 0.1250  0.500      1.0       ----       ----     2.40E-02    0.41
 * Band  3: 0.1375     0.      1.0       ----       ----     2.40E-02  -32.39
 *          0.5000     0.      1.0       ----       ----     2.40E-02  -32.39
 */

/* Filter constants */
#define	NTAPS	60
#define	L	4
#define	M	3

/* Filter coefficients */
static float fir[] = {
  -1.191814E-02,  1.654449E-03,  5.025864E-03,  8.001282E-03,  8.120284E-03,
   4.390523E-03, -1.788580E-03, -7.101940E-03, -8.095658E-03, -3.380242E-03,
   4.924052E-03,  1.206826E-02,  1.309712E-02,  6.089257E-03, -6.067766E-03,
  -1.659746E-02, -1.837332E-02, -8.384119E-03,  9.492135E-03,  2.548390E-02,
   2.867585E-02,  1.354027E-02, -1.538157E-02, -4.355295E-02, -5.205796E-02,
  -2.659891E-02,  3.442831E-02,  1.170044E-01,  1.956996E-01,  2.436154E-01,
   2.436154E-01,  1.956996E-01,  1.170044E-01,  3.442831E-02, -2.659891E-02,
  -5.205796E-02, -4.355295E-02, -1.538157E-02,  1.354027E-02,  2.867585E-02,
   2.548390E-02,  9.492135E-03, -8.384119E-03, -1.837332E-02, -1.659746E-02,
  -6.067766E-03,  6.089257E-03,  1.309712E-02,  1.206826E-02,  4.924052E-03,
  -3.380242E-03, -8.095658E-03, -7.101940E-03, -1.788580E-03,  4.390523E-03,
   8.120284E-03,  8.001282E-03,  5.025864E-03,  1.654449E-03, -1.191814E-02,
};

/* Queue Macros */
#define	QSIZE		(NTAPS/L)
#define qpush(x) \
{ \
	q[qidx++] = x; \
	if (qidx == QSIZE) \
		qidx = 0; \
}

/* Local variables */
static float	q[QSIZE];	/* should be >= NTAPS/L */
static int	qidx;

/* Local functions */
static float qindex(int i);

void
upsample(float *x, int nx, float *y)
{
	int	m, n, ny, phase, start, start_old;
	float	sum;

	ny = (nx * L) / M;	/* compute size of output */
	start_old = -1;
	for (m=0; m<ny; m++) {	/* output loop */
		phase = (m * M) % L;
		start = (m * M) / L;
		while (start_old < start)
			qpush(x[++start_old]);	/* push next input on queue */
		sum = 0.0;
		for (n=0; n<QSIZE; n++) {
			/* fir filter coefficients are assume to be reflected
			 * about the middle coefficient, simplifying indexing
			 * somewhat.
			 */
			sum += qindex(n) * fir[n*L + phase];
		}
		y[m] = (float) L * sum;
	}
	start = nx-1;
	while (start_old < start)
		qpush(x[++start_old]);
}

static float
qindex(int i)
{
	int	idx;

	idx = qidx - i - 1;
	if (idx < 0)
		idx += QSIZE;

	return q[idx];
}
