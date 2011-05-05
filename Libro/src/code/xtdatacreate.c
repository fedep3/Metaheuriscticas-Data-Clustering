// Reset j for next cycle.
float j = 0.0;
// Counter to know in which delta_time section we are on the xtdata
// array.
int arrcount = 0;
// Temporal storage for the number of packets seen.
int numpktseen = 0;

// While the added time is less than the total time and we haven't
// run out of packets, run this cycle to see how many packets we see
// per delta_time section.
while (arrcount < datapoints && timecnt < packets &&
        j <= time_int ) {

    // Add the next timedata to j and pass to the next packet time
    // diff.
    j += timedata[timecnt++];

    // We are seeing one new packet, so add it to our temporal
    // storage.
    numpktseen++;

    if (j >= (arrcount+1)*(delta_time)) {
        // If we've surpassed one of the delta_times, fill the
        // information for the delta_time section.
        xtdata[arrcount++] = numpktseen -1;

        // Boolean variables
        short int passed = 0;
        short int carry = 1;

        // Since we're not sure if the packet jumps through too
        // many sections, we check for it, filling each
        // sectioned passed with 0, except the first one which has
        // at least one.
        while (arrcount < datapoints &&
                j > (arrcount)*(delta_time)) {
            if (passed == 0) {
                xtdata[arrcount++] = 1;
                passed = 1;
                carry = 0;
            } else {
                xtdata[arrcount++] = 0;
            }
        }

        // Finally we reset the temporal storage for the next cycle.
        if (carry == 1) {
            numpktseen = 1;
        } else {
            numpktseen = 0;
        }
    }
}
