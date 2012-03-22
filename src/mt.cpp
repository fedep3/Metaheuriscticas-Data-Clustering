#include "mt.h"
#include  <stdio.h>

static __m128i  MTUpperMask;
static __m128i  MTLowerMask;
static __m128i  MTMatrixA;
static __m128i  One;
static __m128   Scale;	    // default gives random numbers in [0..1)

void
MTStore::mtRandomInit(MTStore *st, unsigned int s, float scale) {
    int mti;
    st->mtState[0]= s & 0xffffffffUL;
    for (mti=1; mti<MT_N; mti++) {
        st->mtState[mti] = 
	    (1812433253UL * (st->mtState[mti-1] ^ (st->mtState[mti-1] >> 30)) + mti); 
        /* See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. */
        /* In the previous versions, MSBs of the seed affect   */
        /* only MSBs of the array mt[].                        */
        /* 2002/01/09 modified by Makoto Matsumoto             */
        st->mtState[mti] &= 0xffffffffUL;
        /* for >32 bit machines */
    }
    for (mti=MT_N; mti<MT_N+8; mti++)	    // initialize fetch-ahead words to
	st->mtState[mti] = 0;		    // prevent memory checker complaints

    st->mtFloatCounter = MT_N;
    MTUpperMask = _mm_set1_epi32(MT_UPPER_MASK);
    MTLowerMask = _mm_set1_epi32(MT_LOWER_MASK);
    MTMatrixA   = _mm_set1_epi32(MT_MATRIX_A);
    One         = _mm_set1_epi32(1);
    Scale       = _mm_set_ps1(scale);
}

/******************************************************************************

    SIMD Mersenne Twister new state compute function.

    Loren Carpenter
    Pixar Anmimation Studios
    June, 2006

    The state consists of MT_N = 624 32-bit words.  It is computed in a single pass.
    Element [i] is made from 3 elements: [i], [(i+1)%MT_N], and [(i+m)%MT_N].
    The mathematics of the process is explained on the home web site:
	http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html

    This implementation uses the gcc SSE builtins to compute 4 adjacent
    state elements in parallel.  In fact, the loop computes 2 sets of 4 state
    words per iteration.  The subsets (0-1 and 1-2) are identified by the 01
    and 12 variable name suffixes.

    For one subset, e.g. 0-1, the process is as follows:

	We need to get 4 SIMD int variables containing words
	    [0,1,2,3], [1,2,3,4] and [m,m+1,m+2,m+3] 

	The first is simple: ws0 = sp[i].
	The second (ws01) comes from ws0 and ws1 = sp[i+1] using a double
	    quadword shift.
	Since MT_M = 397 = 99*4+1, we do a similar 1-word double quadword
	    shift to extract and align wm01.  We could do an unaligned load, but
	    that is invariably slower, plus it increases the wrap complications.
    
	The single bit merge is implemented by 2 AND's and an OR.

	The conditional xor word is extracted using the instruction _mm_cmpeq_epi32
	    to test an lsb in order to generate a mask.

	The float output is made from 31 msb's of the state.  SSE-2 does not
	    have an unsigned-int-to-float converter.  Shifting right 1 bit
	    and converting is the simplest alternative.

    The second subset (1-2) is interleaved (coarsely) with the first.  We let the
	compiler do the fancy detailed scheduling.  The result is that, with a
	decent compiler, the delays from reading memory and computing multiplies
	are filled by executing instructions from the other subset.  Adding
	the secomd subset made the speedup over serial go from 2.4X to 3.2X.

    The state array is padded by 2 quadwords to make the [i+1] fetch work
	without any runtime cost.  To compute the last element in the state
	vector we need the first [0] one, but somewhere in the middle of the
	loop we also need to reset the [m] index back to zero.  When the [m]
	index gets to the end of the array, we reset it, plus copy the valid
	[0] element to the [MT_N] position so that on the last iteration
	through the loop the low word of the [i+1] quadword is correct.
	The second pad quadword is fetched on the last iteration, but not
	used.  The padding is there to prevent memory faults.

******************************************************************************/

void
MTStore::mtNewRandomState (unsigned int *state, float *output)
{
    register int i, m;
    register __m128i  *sp, y01, y12, mag01, mag12;
    register __m128i   ws0, ws1, ws2, ws01, ws12;
    register __m128i   wm0, wm1, wm2, wm01, wm12;
    register __m128i   t1, t2, t3, t4, t5, t6;
    register __m128   *tjp, yf01, yf12;

    sp  = (__m128i *)state;
    tjp = (__m128  *)output;

    m = MT_M/4;

    ws0 = sp[0];
    wm0 = sp[m];

    // 0-1 part, first half (startup only)

    ws1 = sp[1];
    wm1 = sp[m+1];

    ws01 = _mm_or_si128 (((__m128i)_mm_srli_si128 (ws0, 4)),
                         ((__m128i)_mm_slli_si128 (ws1, 12)));

    t1  = _mm_and_si128 (ws0, MTUpperMask);	    // shift 1 bit
    t2  = _mm_and_si128 (ws01, MTLowerMask);
    y01 = _mm_or_si128 (t1, t2);

    mag01 = _mm_and_si128 (y01, One);	    // conditional xor
    mag01 = _mm_cmpeq_epi32 (mag01, One);
    mag01 = _mm_and_si128 (mag01, MTMatrixA);

    for (i = 0; i < MT_N/4; i+=2, m+=2) {	    // MT_N (==624) is a multiple of 4

    // 1-2 part, first half

        ws2 = sp[i+2];				    // 1-2 part, first half
        wm2 = sp[m+2];
						    // extract [1234] from [0123] and [4567]
        ws12 = _mm_or_si128 (((__m128i)_mm_srli_si128 (ws1, 4)),
                             ((__m128i)_mm_slli_si128 (ws2, 12)));

        t4   = _mm_and_si128 (ws1,  MTUpperMask);	// shift 1 bit
        t5   = _mm_and_si128 (ws12, MTLowerMask);
        y12  = _mm_or_si128 (t4, t5);

        mag12 = _mm_and_si128 (y12, One);    // conditional xor
        mag12 = _mm_cmpeq_epi32 (mag12, One);
        mag12 = _mm_and_si128 (mag12, MTMatrixA);

    // 0-1 part, second half
						    // extract [1234] from [0123] and [4567]
        wm01 = _mm_or_si128 (((__m128i)_mm_srli_si128 (wm0, 4)),
                             ((__m128i)_mm_slli_si128 (wm1, 12)));
	t1 = _mm_srli_epi32 (y01, 1);
        t2 = _mm_xor_si128 (t1, mag01);
        t3 = _mm_xor_si128 (wm01, t2);

        sp[i] = t3;				    // new 0-1 state

        t3     = _mm_srli_epi32 (t3, 1);
        yf01   = _mm_cvtepi32_ps (t3);
        tjp[i] = _mm_mul_ps (yf01, Scale);

    // 1-2 part, second half
						    // extract [1234] from [0123] and [4567]
        wm12 = _mm_or_si128 (((__m128i)_mm_srli_si128 (wm1, 4)),
			     ((__m128i)_mm_slli_si128 (wm2, 12)));
        t4 = _mm_srli_epi32 (y12, 1);
        t5 = _mm_xor_si128 (t4, mag12);
        t6 = _mm_xor_si128 (wm12, t5);

        sp[i+1] = t6;				    // new 1-2 state

        t6       = _mm_srli_epi32 (t6, 1);	    // convert upper 31 bits to float
        yf12     = _mm_cvtepi32_ps (t6);
        tjp[i+1] = _mm_mul_ps (yf12, Scale);	    // scale to [0..1) and save

        if (m == MT_N/4-3)  {	    // did we just fetch the last m?
	   m = -3;		    // the next fetched sp[m+3] needs to come from sp[0]
	   state[MT_N] = state[0];  // SIMD wrap padding to compute state[MT_N-1]
        }

        ws0 = ws2;				    // scroll forward by 8
        wm0 = wm2;

    // 0-1 part, first half

        ws1 = sp[i+3];
        wm1 = sp[m+3];
						    // shift 1 word left
        ws01 = _mm_or_si128 (((__m128i)_mm_srli_si128 (ws0, 4)),
                             ((__m128i)_mm_slli_si128 (ws1, 12)));
        t1 = _mm_and_si128 (ws0, MTUpperMask);	    // shift 1 bit
        t2 = _mm_and_si128 (ws01, MTLowerMask);
        y01 = _mm_or_si128 (t1, t2);

        mag01 = _mm_and_si128 (y01, One);	    // conditional xor
        mag01 = _mm_cmpeq_epi32 (mag01, One);
        mag01 = _mm_and_si128 (mag01, MTMatrixA);
    }
}

void
MTStore::mtInit(MTStore *st)
{
    mtRandomInit(st, 5489UL, K_2M31); // Init the tables
}

void
MTStore::mtInitScale(MTStore *st, float scale)
{
    mtRandomInit(st, 5489UL, scale*K_2M31); // Init the tables
}

//////////// MTStore Constructor/Destructor

MTStore::MTStore(void) 
{
	// 16-byte alignment and padding added for SIMD
	// assumes malloc() result is at least 8-byte aligned.

    mtStateBase = (unsigned int *)malloc (sizeof(unsigned int) * (MT_N+10));
    if ((unsigned long)mtStateBase & 0xF)	// 16 byte aligned?
	mtState = mtStateBase + 2;		// 8 byte aligned
    else
	mtState = mtStateBase;

    mtFloatBase = (float *)malloc (sizeof(float) * (MT_N+2));
    if ((unsigned long)mtFloatBase & 0xF)	// 16 byte aligned?
	mtFloat = mtFloatBase + 2;		// 8 byte aligned
    else
	mtFloat = mtFloatBase;

    mtFloatCounter = MT_N;
}

MTStore::~MTStore(void) 
{
    if (mtStateBase)
        free ((void *)mtStateBase);

    if (mtFloatBase)
        free ((void *)mtFloatBase);
}
