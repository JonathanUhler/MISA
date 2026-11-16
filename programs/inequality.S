        .section text

// 0x01 and 0xFF order differently under the two signednesses, so one CMP of the pair exercises
// every ordered comparison flag. Signed, 0x01 is +1 and 0xFF is -1, so RA is the greater operand.
// Unsigned, 0x01 is 1 and 0xFF is 255, so RA is the lesser operand.
//
// Halts with 0x00 on success, or the number of the group of checks that failed.

        set ra 0x01
        set rb 0xFF

// Operands as written. The four comparison flags decide: signed less false, signed greater or
// equal true, unsigned below true, unsigned above or equal false. The branch macros only SET the
// scratch pair, so the flags from the one CMP reach every branch in the group.

        cmp ra rb

        jmpi less          _fail_forward
        jmpi above_equal   _fail_forward
        jmpi equal         _fail_forward

        jmpi greater_equal _forward_ge
        goto _fail_forward
_forward_ge:
        jmpi below         _forward_below
        goto _fail_forward
_forward_below:

// Operands reversed. This is how an assembler reaches the four relations which have no flag of
// their own, and it flips every result of the group above.

        cmp rb ra

        jmpi greater_equal _fail_reversed
        jmpi below         _fail_reversed
        jmpi equal         _fail_reversed

        jmpi less          _reversed_less
        goto _fail_reversed
_reversed_less:
        jmpi above_equal   _reversed_ae
        goto _fail_reversed
_reversed_ae:

// Equal operands. Both non-strict comparisons hold and both strict ones do not, under either
// signedness.

        cmp ra ra

        jmpi less          _fail_equal
        jmpi below         _fail_equal
        jmpi not_equal     _fail_equal

        jmpi equal         _equal_eq
        goto _fail_equal
_equal_eq:
        jmpi greater_equal _equal_ge
        goto _fail_equal
_equal_ge:
        jmpi above_equal   _equal_ae
        goto _fail_equal
_equal_ae:

        set rt 0x00
        halt rt

_fail_forward:
        set rt 0x01
        halt rt

_fail_reversed:
        set rt 0x02
        halt rt

_fail_equal:
        set rt 0x03
        halt rt
