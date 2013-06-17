# 
# Fri Jun 23 00:58:24 CDT 1995    Andy Fingerhut (jaf@arl.wustl.edu)
# 
# SCCS ID: @(#)lin2ulaw.awk 1.1 96/02/29
# 
# Print a conversion table from linearly encoded samples with
# linear_bits bits each, to u-law samples with ulaw_bits each.
# 
# The linear samples may be either SIGNED or UNSIGNED, chosen by
# the setting of the variable linear_encoding to one of the defined
# "constants" SIGNED or UNSIGNED.  The u-law samples may be either
# SIGNED, UNSIGNED, or SUN, by setting the variable ulaw_encoding
# to one of these values.
# 
# The format of the output may be chosen by setting the variable
# output_format to one of the following values:
# 
# C_ARRAY_INITIALIZATION:
# Produces a table that may be inserted into a C program to specify
# an initial value of an array.
# 
# CONVERTED_VALUES_ONLY:
# Produces the mu-law values, one per line, with nothing else.
# 
# VERBOSE:
# Prints out the linear sample value, its scaled floating point
# value in the range [-1,+1], the floating point mu-law value in the
# range [-1,+1], the floating point mu-law value in the range
# [-2^(ulaw_bits-1), +2^(ulaw_bits-1)] (before truncation or rounding
# off), truncated and rounded versions of this value, and finally
# the Sun encoded value.
# 

BEGIN {

# 
# Don't change these.
# 
    SIGNED = 0;
    UNSIGNED = 1;
    SUN = 2;

    C_ARRAY_INITIALIZATION = 0;
    CONVERTED_VALUES_ONLY = 1;
    VERBOSE = 2;

# 
# mu=255 is a fairly standard value to use in mu-law coding.
# 
    mu = 255;

# 
# Change these as desired.
# 
    linear_bits = 8;
    linear_encoding = UNSIGNED;
    ulaw_bits = 8;
    ulaw_encoding = SUN;
    output_format = VERBOSE;

    debug = 0;

# 
# Compute 2^(linear_bits-1) and 2^(ulaw_bits-1)
# 
    linear_max = 1;
    for (i = 1; i <= linear_bits-1; i++) {
	linear_max = 2*linear_max;
    }

    ulaw_max = 1;
    for (i = 1; i <= ulaw_bits-1; i++) {
	ulaw_max = 2*ulaw_max;
    }

    if (debug > 0) {
        printf "linear_bits=%d linear_max=%d ulaw_bits=%d ulaw_max=%d output_format=%d\n", linear_bits, linear_max, ulaw_bits, ulaw_max, output_format;
    }

    for (i = 0; i <= 2*linear_max-1; i++) {
        if (debug > 0) {
            printf "i=%d\n", i;
        }

        real_linear_sample = (i-linear_max)/linear_max;

        # 
        # Convert it to a real mu-law value in the range -1.0 to +1.0.
        # 
        if (real_linear_sample < 0.0) {
            real_ulaw_sample = -log(1+mu*(-real_linear_sample))/log(1+mu);
        } else {
            real_ulaw_sample = log(1+mu*real_linear_sample)/log(1+mu);
        }

        # 
        # Convert the [-1,+1] mu-law sample to all possible
        # desired scaled real values.
        # 
        signed_ulaw_sample = ulaw_max*real_ulaw_sample;
        unsigned_ulaw_sample = ulaw_max*(1+real_ulaw_sample);

        if (real_ulaw_sample < 0.0) {
            sun_ulaw_sample = ulaw_max*(1+real_ulaw_sample);
        } else {
# 
# Here is the code that handles Sun's strange ordering of the samples.
# 
            sun_ulaw_sample = ulaw_max*(1+(1-real_ulaw_sample));
        }

# 
# Choose the desired value for printing out
# 
        if (ulaw_encoding == SIGNED) {
	    integer_ulaw_sample = signed_ulaw_sample;
        } else if (ulaw_encoding == UNSIGNED) {
	    integer_ulaw_sample = unsigned_ulaw_sample;
        } else if (ulaw_encoding == SUN) {
	    integer_ulaw_sample = sun_ulaw_sample;
        }

# 
# Round off to nearest integer
# 
        if (integer_ulaw_sample < 0.0) {
	    integer_ulaw_sample = int(-0.5 + integer_ulaw_sample);
	} else {
	    integer_ulaw_sample = int(0.5 + integer_ulaw_sample);
	}

        # 
        # This is only necessary for one or two points at the extremes.
        # 
        if (ulaw_encoding == SIGNED) {
	    if (integer_ulaw_sample < -ulaw_max) {
                integer_ulaw_sample = -ulaw_max;
            } else if (integer_ulaw_sample > ulaw_max - 1) {
                integer_ulaw_sample = ulaw_max - 1;
            }
        } else if (ulaw_encoding == UNSIGNED || ulaw_encoding == SUN) {
	    if (integer_ulaw_sample < 0) {
                integer_ulaw_sample = 0;
            } else if (integer_ulaw_sample > 2*ulaw_max - 1) {
                integer_ulaw_sample = 2*ulaw_max - 1;
            }
        }

        if (debug > 0) {
            printf "    output_format=%d VERBOSE=%d\n", output_format, VERBOSE;
        }
        if (output_format == C_ARRAY_INITIALIZATION) {
            if (number_printed_on_line == 8) {
                printf "\n";
                number_printed_on_line = 0;
            }
            printf " %6d", integer_ulaw_sample;
# 
# Print a comma, unless this is the last time through the loop.
# If it is the last time, print a newline.
# 
	    if (i == 2*linear_max - 1) {
		printf "\n";
	    } else {
		printf ",";
	    }
            ++number_printed_on_line;
        } else if (output_format == CONVERTED_VALUES_ONLY) {
            printf "%d\n", integer_ulaw_sample;
        } else if (output_format == VERBOSE) {
# Prints out the linear sample value, its scaled floating point
# value in the range [-1,+1], the floating point mu-law value in the
# range [-1,+1], the floating point mu-law value in the range
# [-2^(ulaw_bits-1), +2^(ulaw_bits-1)] (before truncation or rounding
# off), truncated and rounded versions of this value, and finally
# the Sun encoded value.
	    if (linear_encoding == UNSIGNED) {
		integer_linear_sample = i;
	    } else if (linear_encoding == SIGNED) {
		integer_linear_sample = i - linear_max;
	    }
	    scaled_real_ulaw_sample = ulaw_max * real_ulaw_sample;
# 
# GNU awk's int() truncates towards 0, even for negative numbers.
# Hence, the code for rounding a value to the nearest integer looks
# as it does.
# 
	    if (scaled_real_ulaw_sample < 0) {
		rounded_ulaw_sample = int(-0.5 + scaled_real_ulaw_sample);
	    } else {
		rounded_ulaw_sample = int(0.5 + scaled_real_ulaw_sample);
	    }

            printf "%6d %7.4f %7.4f %10.2f %7d %7d %10.2f %7d\n", integer_linear_sample, real_linear_sample, real_ulaw_sample, scaled_real_ulaw_sample, int(scaled_real_ulaw_sample), rounded_ulaw_sample, sun_ulaw_sample, integer_ulaw_sample;
        }

    }
}
