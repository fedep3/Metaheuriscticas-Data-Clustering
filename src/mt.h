#include  <emmintrin.h>
#include  <stdlib.h>

#ifndef _MT_
#define _MT_
// Mersene Twister constants
//
//  Loren Carpenter
//  Pixar Animation Studios
//  June, 2006
//

/* Period parameters */

#define MT_N 624
#define MT_M 397
#define MT_MATRIX_A   0x9908b0dfUL   /* constant vector a */
#define MT_UPPER_MASK 0x80000000UL   /* most significant bit */
#define MT_LOWER_MASK 0x7fffffffUL   /* least significant bits */

#define  K_2M31  (2.0/4294967296.0)	// default scale => [0..1)

class MTStore {
public:
      MTStore ();  // Constructor
     ~MTStore ();  // Destructor

     void mtInit (MTStore *st);
     void mtInitScale (MTStore *st, float scale);
     void mtRandomInit (MTStore *st, unsigned int s, float scale);
     void mtNewRandomState (unsigned int *state, float *output);

     unsigned int   *mtStateBase, *mtState;
     float          *mtFloatBase, *mtFloat;
     unsigned short  mtFloatCounter;
};

//  Returns the next float from the Mersenne Twister table
inline static float
mtGetRandomFloat (MTStore *st)
{
    float  *rn = (float *)st->mtFloat;
    unsigned short  c = st->mtFloatCounter;

    if (++c >= MT_N)  {
        st->mtNewRandomState (st->mtState, rn);
        st->mtFloatCounter = 0;
	return rn[0];
    }  else  {
	st->mtFloatCounter = c;
	return rn[c];
    }
}

inline static int randomInteger(MTStore* drand, int start, int end) {
    return (end-start) * mtGetRandomFloat(drand) + start;
}
#endif
