# SRANDOM

Generate random numbers in a cryptographically-acceptable way.

## with-random-octet-stream (stream-var) &body forms

Evaluate FORMS with STREAM-VAR bound to a stream representing a
cryptographically-acceptable source of random octets.

## random-octets n &optional stream

Return an octet vector filled with N random octets.

## random-integer max &optional stream

Return a random integer in the range [0, max).

# License

MIT
