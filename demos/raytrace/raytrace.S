// raytrace.S
//
// Renders a 128x128 scene with a sphere under a single light and streams
// the raw PPM (P6) image over the UART. Capture with:
//
//     make run > out.ppm      (or: misa-run raytrace.bin > out.ppm)
//
// Three fixed point formats are used: U = Q1.7, B = Q2.6, and S = Q4.4.
//
//   CAM_B = {0, 19, -122} = {00, 13, 86}   Q2.6 camera position
//   CAM_S = {0,  5,  -30} = {00, 05, E2}   Q4.4 camera position
//   LIGHT = {-54, 108, -43} = {CA, 6C, D5} Q1.7 unit vector to the light
//
// UART registers follow demos/echo (UART_BASE = 0x8010):
//   0x8010 TX_READY  read   1 when the transmitter can accept a byte
//   0x8011 TX_DATA   write  byte to transmit

        #macro WH 128 #endm         // image is WH x WH. loops break at WH - 1 = 127

        .section text
_start:
        set2 rscratch 0x01FF        // initialize the stack pointer
        wsr saddr rscratch
        // PPM header "P6\n128 128\n255\n"
        set ra 0x50
        call putc                   // 'P'
        set ra 0x36
        call putc                   // '6'
        set ra 0x0A
        call putc                   // '\n'
        set ra 0x31
        call putc                   // '1'
        set ra 0x32
        call putc                   // '2'
        set ra 0x38
        call putc                   // '8'
        set ra 0x20
        call putc                   // ' '
        set ra 0x31
        call putc                   // '1'
        set ra 0x32
        call putc                   // '2'
        set ra 0x38
        call putc                   // '8'
        set ra 0x0A
        call putc                   // '\n'
        set ra 0x32
        call putc                   // '2'
        set ra 0x35
        call putc                   // '5'
        set ra 0x35
        call putc                   // '5'
        set ra 0x0A
        call putc                   // '\n'
        // for (y = 0; ; y++) { for (x = 0; ; x++) { ... if (x==127) break; } if (y==127) break; }
        set ry 0x00                 // y
_main_y:
        set rz 0x00                 // x
_main_x:
        mov ra rz
        mov rb ry
        call render_pixel
        set ra 0x7F                 // WH - 1
        cmp rz ra
        jmpi equal _main_xend
        set ra 0x01
        add rz rz ra
        goto _main_x
_main_xend:
        set ra 0x7F
        cmp ry ra
        jmpi equal _main_yend
        set ra 0x01
        add ry ry ra
        goto _main_y
_main_yend:
        halt r0

//============================== math primitives (rtlib) ===============================
        .section text

//------------------------------------------------------------------------------
// smul16: signed 8x8 -> 16 multiply.  in ra=a, rb=b.  out rab = (int16_t)(a*b).
// Leaf. Clobbers ra..rf. Method: multiply magnitudes (unsigned), reapply sign.
//------------------------------------------------------------------------------
smul16:
        set rf 0x00                 // rf = sign accumulator
        cmp ra r0
        jmpi greater_equal _smul_apos
        sub ra r0 ra                // ra = |a| (0x80 stays 0x80 = unsigned 128)
        set rd 0x01
        xor rf rf rd
_smul_apos:
        mov re ra                   // re = |a| (multiplicand)
        cmp rb r0
        jmpi greater_equal _smul_bpos
        sub rb r0 rb                // rb = |b|
        set rd 0x01
        xor rf rf rd
_smul_bpos:
        set rc 0x00                 // rc = HI = 0
        mov rd rb                   // rd = LO = multiplier
        set ra 0x08                 // ra = loop counter
_smul_loop:
        set rb 0x01
        and rb rb rd                // rb = LO & 1
        cmp rb r0
        jmpi equal _smul_skip
        add rc rc re                // HI += |a|  (C = carry out)
        jmpi always _smul_shift
_smul_skip:
        add rc rc r0                // HI unchanged, C = 0
_smul_shift:
        rrc2 rc rd rc rd            // {C,HI,LO} >>= 1
        set rb 0x01
        sub ra ra rb
        cmp ra r0
        jmpi not_equal _smul_loop
        set rb 0x01
        and rb rb rf                // sign parity
        cmp rb r0
        jmpi equal _smul_done
        sub2 rc rd r0 r0 rc rd      // negate 16-bit product
_smul_done:
        mov2 rab rcd
        ret

//------------------------------------------------------------------------------
// clamp16to8: clamp signed 16-bit rab to int8_t.  in ra=hi, rb=lo.  out rt.
// Leaf. Clobbers rt.
//------------------------------------------------------------------------------
clamp16to8:
        cmp ra r0
        jmpi less _clamp_neg        // hi < 0 -> value negative
        cmp ra r0
        jmpi not_equal _clamp_hip   // hi > 0 -> value >= 256 -> +127
        set rt 0x80
        and rt rt rb                // hi == 0: lo & 0x80
        cmp rt r0
        jmpi not_equal _clamp_hip   // lo >= 128 -> value 128..255 -> +127
        mov rt rb                   // value 0..127
        ret
_clamp_hip:
        set rt 0x7F
        ret
_clamp_neg:
        set rt 0xFF
        cmp ra rt
        jmpi not_equal _clamp_lom   // hi != 0xFF -> value <= -256 -> -128
        set rt 0x80
        and rt rt rb                // hi == 0xFF: lo & 0x80
        cmp rt r0
        jmpi equal _clamp_lom       // lo < 128 -> value -256..-129 -> -128
        mov rt rb                   // value -128..-1
        ret
_clamp_lom:
        set rt 0x80                 // -128
        ret

//------------------------------------------------------------------------------
// udiv16_8: unsigned 16 / 8 division.  in rab=dividend, rc=divisor (1..128).
// out rab = quotient (16-bit). remainder discarded. Leaf. Clobbers ra..rf,rt.
// Restoring shift-subtract; partial remainder stays < 256 because divisor <=128.
//------------------------------------------------------------------------------
udiv16_8:
        set rd 0x00                 // rd = R (remainder) = 0
        set rt 0x10                 // 16 iterations
_udiv_loop:
        add rb rb rb                // Q <<= 1 (low)
        adc ra ra ra                // Q high, C = MSB of Q
        adc rd rd rd                // R = (R<<1) | C
        sub re rd rc                // re = R - D, C = 1 iff R >= D (unsigned)
        adc rf r0 r0                // rf = C (materialize carry)
        cmp rf r0
        jmpi equal _udiv_skip
        mov rd re                   // R -= D
        set rf 0x01
        or rb rb rf                 // set quotient bit 0
_udiv_skip:
        set rf 0x01
        sub rt rt rf
        cmp rt r0
        jmpi not_equal _udiv_loop
        ret

//------------------------------------------------------------------------------
// mul_core: clamp8((a*b) >> shift) with truncate-toward-zero division by 2^shift.
// in ra=a, rb=b, rc=shift.  out rt.  Non-leaf (calls smul16, clamp16to8).
//------------------------------------------------------------------------------
mul_core:
        rsr raddr re rf
        push2 re rf
        push2 ru rv
        mov ru rc                   // ru = shift (survives smul16)
        call smul16                 // rab = a*b (signed 16-bit)
        cmp ra r0
        jmpi greater_equal _mc_pos
        sub2 ra rb r0 r0 ra rb      // negate to magnitude
        mov rv ru
_mc_nloop:
        cmp rv r0
        jmpi equal _mc_ndone
        add r0 r0 r0                // C = 0
        rrc2 ra rb ra rb            // logical >> 1
        set re 0x01
        sub rv rv re
        jmpi always _mc_nloop
_mc_ndone:
        sub2 ra rb r0 r0 ra rb      // negate back
        jmpi always _mc_clamp
_mc_pos:
        mov rv ru
_mc_ploop:
        cmp rv r0
        jmpi equal _mc_clamp
        add r0 r0 r0
        rrc2 ra rb ra rb
        set re 0x01
        sub rv rv re
        jmpi always _mc_ploop
_mc_clamp:
        call clamp16to8             // rt = clamp(rab)
        pop2 ru rv
        pop2 re rf
        wsr raddr re rf
        ret

// mul_uu / mul_bu / mul_su : clamp8((a*b)/128).  in ra=a, rb=b. out rt.
mul_uu:
        set rc 0x07
        goto mul_core
// mul_ss : clamp8((a*b)/16).  in ra=a, rb=b. out rt.
mul_ss:
        set rc 0x04
        goto mul_core

//------------------------------------------------------------------------------
// div128: clamp8((a*128)/b) with truncate-toward-zero. b==0 -> a<0?-128:127.
// in ra=a, rb=b. out rt. Non-leaf (calls udiv16_8, clamp16to8).
//------------------------------------------------------------------------------
div128:
        cmp rb r0
        jmpi not_equal _div_ok
        cmp ra r0
        jmpi less _div_bzneg
        set rt 0x7F
        ret
_div_bzneg:
        set rt 0x80
        ret
_div_ok:
        rsr raddr re rf
        push2 re rf
        push2 ru rv
        set ru 0x00                 // sign accumulator
        cmp ra r0
        jmpi greater_equal _div_apos
        sub ra r0 ra                // |a|
        set rv 0x01
        xor ru ru rv
_div_apos:
        cmp rb r0
        jmpi greater_equal _div_bpos
        sub rb r0 rb                // |b|
        set rv 0x01
        xor ru ru rv
_div_bpos:
        mov rc rb                   // rc = |b| (divisor)
        mov rb ra                   // build dividend |a|<<7 in rab
        set ra 0x00
        add rb rb rb
        adc ra ra ra
        add rb rb rb
        adc ra ra ra
        add rb rb rb
        adc ra ra ra
        add rb rb rb
        adc ra ra ra
        add rb rb rb
        adc ra ra ra
        add rb rb rb
        adc ra ra ra
        add rb rb rb
        adc ra ra ra                // rab = |a| << 7
        call udiv16_8               // rab = quotient
        set rv 0x01
        and rv rv ru
        cmp rv r0
        jmpi equal _div_clamp
        sub2 ra rb r0 r0 ra rb      // negate
_div_clamp:
        call clamp16to8             // rt
        pop2 ru rv
        pop2 re rf
        wsr raddr re rf
        ret

//------------------------------------------------------------------------------
// isqrt8: floor(sqrt(x)) for x < 128.  in ra=x. out rt. Leaf.
//------------------------------------------------------------------------------
isqrt8:
        cmp ra r0
        jmpi greater _isqrt_go
        set rt 0x00
        ret
_isqrt_go:
        set rb 0x40                 // bit = 64
        set rt 0x00                 // res = 0
_isqrt_w1:
        cmp rb ra                   // while (bit > x)
        jmpi less_equal _isqrt_w1e
        add r0 r0 r0
        rrc rb rb
        add r0 r0 r0
        rrc rb rb                   // bit >>= 2
        goto _isqrt_w1
_isqrt_w1e:
        cmp rb r0                   // while (bit)
        jmpi equal _isqrt_done
        add rc rb rt                // t = res + bit
        cmp ra rc                   // if (x >= t)
        jmpi less _isqrt_else
        sub ra ra rc                // x -= t
        add r0 r0 r0
        rrc rt rt                   // res >>= 1
        add rt rt rb                // res += bit
        goto _isqrt_after
_isqrt_else:
        add r0 r0 r0
        rrc rt rt                   // res >>= 1
_isqrt_after:
        add r0 r0 r0
        rrc rb rb
        add r0 r0 r0
        rrc rb rb                   // bit >>= 2
        goto _isqrt_w1e
_isqrt_done:
        ret

//------------------------------------------------------------------------------
// sqrt_u: Q1.7 sqrt.  in ra=x. out rt. Non-leaf (isqrt8, smul16, div128).
//------------------------------------------------------------------------------
sqrt_u:
        cmp ra r0
        jmpi greater _sqrt_go
        set rt 0x00
        ret
_sqrt_go:
        rsr raddr re rf
        push2 re rf
        push2 ru rv
        push2 rw rx
        mov ru ra                   // ru = x
        call isqrt8                 // rt = isqrt8(x)
        mov ra rt
        set rb 0x0B                 // 11
        call smul16                 // rab = isqrt8(x)*11
        mov rv rb                   // rv = r (low byte, <=121)
        cmp rv r0
        jmpi greater _sqrt_rok
        set rv 0x01                 // if (r < 1) r = 1
_sqrt_rok:
        set rw 0x03                 // i = 3
_sqrt_loop:
        cmp rw r0
        jmpi equal _sqrt_end
        mov ra ru                   // div_uu(x, r)
        mov rb rv
        call div128                 // rt = div_uu(x,r)
        add rc rv rt                // sum = r + div  (both 0..127, no carry)
        add r0 r0 r0
        rrc rv rc                   // r = sum >> 1 (logical)
        set rx 0x01
        sub rw rw rx
        goto _sqrt_loop
_sqrt_end:
        mov rt rv                   // return r (before restoring rv)
        pop2 rw rx
        pop2 ru rv
        pop2 re rf
        wsr raddr re rf
        ret

//============================== scene routines (rthi) =================================
        .section text

//------------------------------------------------------------------------------
// sext8: sign-extend int8 ra into 16-bit rab (ra=hi, rb=lo). Leaf.
//------------------------------------------------------------------------------
sext8:
        mov rb ra
        set ra 0x00
        cmp rb r0
        jmpi greater_equal _sext_done
        set ra 0xFF
_sext_done:
        ret

//------------------------------------------------------------------------------
// clamp8_sum2 / clamp8_diff: clamp8(a +/- b) using sign-extended 16-bit math.
// in ra=a, rb=b. out rt. Tail-call clamp16to8. Leaf w.r.t. RADDR.
//------------------------------------------------------------------------------
clamp8_sum2:
        mov rc r0
        cmp ra r0
        jmpi greater_equal _csum_a
        set rc 0xFF
_csum_a:
        mov rd r0
        cmp rb r0
        jmpi greater_equal _csum_b
        set rd 0xFF
_csum_b:
        add2 rc ra rc ra rd rb      // {rc:ra} = a + b
        mov rb ra
        mov ra rc
        goto clamp16to8

clamp8_diff:
        mov rc r0
        cmp ra r0
        jmpi greater_equal _cdif_a
        set rc 0xFF
_cdif_a:
        mov rd r0
        cmp rb r0
        jmpi greater_equal _cdif_b
        set rd 0xFF
_cdif_b:
        sub2 rc ra rc ra rd rb      // {rc:ra} = a - b
        mov rb ra
        mov ra rc
        goto clamp16to8

//------------------------------------------------------------------------------
// halfdiff / hsum: (int8_t)((A -/+ B) >> 1) with arithmetic (floor) shift.
// A,B promoted to 16-bit signed first, exactly like C's int promotion. Leaf.
// in ra=A, rb=B. out rt.
//------------------------------------------------------------------------------
halfdiff:
        mov rc r0
        cmp ra r0
        jmpi greater_equal _hd_a
        set rc 0xFF
_hd_a:
        mov rd r0
        cmp rb r0
        jmpi greater_equal _hd_b
        set rd 0xFF
_hd_b:
        sub2 rc ra rc ra rd rb      // {rc:ra} = A - B
        add r0 rc rc                // C = sign bit of high byte
        rrc2 rc ra rc ra            // arithmetic >> 1
        mov rt ra
        ret

hsum:
        mov rc r0
        cmp ra r0
        jmpi greater_equal _hs_a
        set rc 0xFF
_hs_a:
        mov rd r0
        cmp rb r0
        jmpi greater_equal _hs_b
        set rd 0xFF
_hs_b:
        add2 rc ra rc ra rd rb      // {rc:ra} = A + B
        add r0 rc rc
        rrc2 rc ra rc ra            // arithmetic >> 1
        mov rt ra
        ret

//------------------------------------------------------------------------------
// asr4: (int8_t)(ra >> 4) arithmetic. Leaf. in ra. out rt.
//------------------------------------------------------------------------------
asr4:
        add r0 ra ra
        rrc ra ra
        add r0 ra ra
        rrc ra ra
        add r0 ra ra
        rrc ra ra
        add r0 ra ra
        rrc ra ra
        mov rt ra
        ret

//------------------------------------------------------------------------------
// putc: poll TX_READY then write ra to UART TX_DATA. Leaf. Clobbers rc,re,rf.
//------------------------------------------------------------------------------
putc:
_putc_wait:
        set2 re rf 0x8010           // TX_READY
        ld rc re rf
        cmp rc r0
        jmpi equal _putc_wait
        set2 re rf 0x8011           // TX_DATA
        st ra re rf
        ret

//------------------------------------------------------------------------------
// emit: rt8 emit(). c<0 -> 0; c = sqrt_u(c); putchar(c*2). in ra=c. Non-leaf.
//------------------------------------------------------------------------------
emit:
        rsr raddr re rf
        push2 re rf
        cmp ra r0
        jmpi greater_equal _emit_pos
        set ra 0x00
_emit_pos:
        call sqrt_u                 // rt = sqrt_u(c) (gamma 2.0)
        add rt rt rt                // c * 2 (mod 256)
        mov ra rt
        call putc
        pop2 re rf
        wsr raddr re rf
        ret

//------------------------------------------------------------------------------
// hit_sphere: primary hit test. Reads d_x,d_y,d_z. On hit writes n_x,n_y,n_z
// and returns rt=1, else rt=0. Non-leaf. Accumulator in ry:rz.
//------------------------------------------------------------------------------
hit_sphere:
        rsr raddr re rf
        push2 re rf
        push2 ry rz
        // b = clamp8(mul_uu(CAM_B.y,d.y) + mul_uu(CAM_B.z,d.z))  [CAM_B.x=0 term omitted]
        set ry 0x00
        set rz 0x00
        set ra 0x13                 // CAM_B.y = 19
        set2 re rf d_y
        ld rb re rf
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set ra 0x86                 // CAM_B.z = -122
        set2 re rf d_z
        ld rb re rf
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        mov ra ry
        mov rb rz
        call clamp16to8             // rt = b
        set2 re rf hs_b
        st rt re rf
        cmp rt r0
        jmpi less _hs_bneg          // b < 0 -> in front
        set rt 0x00
        goto _hs_ret                // b >= 0 -> behind us
_hs_bneg:
        // px = CAM_B.x - mul_uu(b,d.x) = -mul_uu(b,d.x)   (CAM_B.x = 0)
        set2 re rf hs_b
        ld ra re rf
        set2 re rf d_x
        ld rb re rf
        call mul_uu
        set rc 0x00
        sub rc rc rt
        set2 re rf hs_px
        st rc re rf
        // py = CAM_B.y(19) - mul_uu(b,d.y)
        set2 re rf hs_b
        ld ra re rf
        set2 re rf d_y
        ld rb re rf
        call mul_uu
        set rc 0x13
        sub rc rc rt
        set2 re rf hs_py
        st rc re rf
        // pz = CAM_B.z(-122) - mul_uu(b,d.z)
        set2 re rf hs_b
        ld ra re rf
        set2 re rf d_z
        ld rb re rf
        call mul_uu
        set rc 0x86
        sub rc rc rt
        set2 re rf hs_pz
        st rc re rf
        // range: any |component| > 63 -> miss
        set2 re rf hs_px
        ld ra re rf
        set rc 0x3F
        cmp ra rc
        jmpi greater _hs_miss
        set rc 0xC1                 // -63
        cmp ra rc
        jmpi less _hs_miss
        set2 re rf hs_py
        ld ra re rf
        set rc 0x3F
        cmp ra rc
        jmpi greater _hs_miss
        set rc 0xC1
        cmp ra rc
        jmpi less _hs_miss
        set2 re rf hs_pz
        ld ra re rf
        set rc 0x3F
        cmp ra rc
        jmpi greater _hs_miss
        set rc 0xC1
        cmp ra rc
        jmpi less _hs_miss
        // px,py,pz <<= 1  (Q2.6 -> Q1.7)
        set2 re rf hs_px
        ld ra re rf
        add ra ra ra
        st ra re rf
        set2 re rf hs_py
        ld ra re rf
        add ra ra ra
        st ra re rf
        set2 re rf hs_pz
        ld ra re rf
        add ra ra ra
        st ra re rf
        // h2 = 0 ; accumulate squares with early miss if > 127
        set2 re rf hs_h2
        st r0 re rf
        // q = mul_uu(px,px)
        set2 re rf hs_px
        ld ra re rf
        mov rb ra
        call mul_uu
        set2 re rf hs_h2
        ld rc re rf
        set rd 0x7F
        sub rd rd rc                // 127 - h2
        cmp rt rd
        jmpi greater _hs_miss
        add rc rc rt
        set2 re rf hs_h2
        st rc re rf
        // q = mul_uu(py,py)
        set2 re rf hs_py
        ld ra re rf
        mov rb ra
        call mul_uu
        set2 re rf hs_h2
        ld rc re rf
        set rd 0x7F
        sub rd rd rc
        cmp rt rd
        jmpi greater _hs_miss
        add rc rc rt
        set2 re rf hs_h2
        st rc re rf
        // q = mul_uu(pz,pz)
        set2 re rf hs_pz
        ld ra re rf
        mov rb ra
        call mul_uu
        set2 re rf hs_h2
        ld rc re rf
        set rd 0x7F
        sub rd rd rc
        cmp rt rd
        jmpi greater _hs_miss
        add rc rc rt
        set2 re rf hs_h2
        st rc re rf
        // s = sqrt_u(127 - h2)
        set2 re rf hs_h2
        ld rc re rf
        set ra 0x7F
        sub ra ra rc
        call sqrt_u
        set2 re rf hs_s
        st rt re rf
        // n.x = clamp8(px - mul_uu(s,d.x))
        set2 re rf hs_s
        ld ra re rf
        set2 re rf d_x
        ld rb re rf
        call mul_uu
        set2 re rf hs_px
        ld ra re rf
        mov rb rt
        call clamp8_diff
        set2 re rf n_x
        st rt re rf
        // n.y = clamp8(py - mul_uu(s,d.y))
        set2 re rf hs_s
        ld ra re rf
        set2 re rf d_y
        ld rb re rf
        call mul_uu
        set2 re rf hs_py
        ld ra re rf
        mov rb rt
        call clamp8_diff
        set2 re rf n_y
        st rt re rf
        // n.z = clamp8(pz - mul_uu(s,d.z))
        set2 re rf hs_s
        ld ra re rf
        set2 re rf d_z
        ld rb re rf
        call mul_uu
        set2 re rf hs_pz
        ld ra re rf
        mov rb rt
        call clamp8_diff
        set2 re rf n_z
        st rt re rf
        set rt 0x01
        goto _hs_ret
_hs_miss:
        set rt 0x00
_hs_ret:
        pop2 ry rz
        pop2 re rf
        wsr raddr re rf
        ret

//------------------------------------------------------------------------------
// shadowed: Q4.4 shadow-ray test from a floor point. in ra=px, rb=py, rc=pz.
// out rt (0/1). Non-leaf. Accumulator in ry:rz.
//------------------------------------------------------------------------------
shadowed:
        rsr raddr re rf
        push2 re rf
        push2 ry rz
        set2 re rf shd_px
        st ra re rf
        set2 re rf shd_py
        st rb re rf
        set2 re rf shd_pz
        st rc re rf
        // b = clamp8(mul_su(px,LIGHT.x)+mul_su(py,LIGHT.y)+mul_su(pz,LIGHT.z))
        set ry 0x00
        set rz 0x00
        set2 re rf shd_px
        ld ra re rf
        set rb 0xCA                 // LIGHT.x = -54
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf shd_py
        ld ra re rf
        set rb 0x6C                 // LIGHT.y = 108
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf shd_pz
        ld ra re rf
        set rb 0xD5                 // LIGHT.z = -43
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        mov ra ry
        mov rb rz
        call clamp16to8             // rt = b
        set2 re rf shd_b
        st rt re rf
        cmp rt r0
        jmpi greater _shd_bpos
        set rt 0x00
        goto _shd_ret               // b <= 0 -> not shadowed
_shd_bpos:
        // qx = px - mul_su(b,LIGHT.x)   (8-bit wrap, matches (int8_t) cast)
        set2 re rf shd_b
        ld ra re rf
        set rb 0xCA
        call mul_uu
        set2 re rf shd_px
        ld rc re rf
        sub rc rc rt
        set2 re rf shd_qx
        st rc re rf
        set2 re rf shd_b
        ld ra re rf
        set rb 0x6C
        call mul_uu
        set2 re rf shd_py
        ld rc re rf
        sub rc rc rt
        set2 re rf shd_qy
        st rc re rf
        set2 re rf shd_b
        ld ra re rf
        set rb 0xD5
        call mul_uu
        set2 re rf shd_pz
        ld rc re rf
        sub rc rc rt
        set2 re rf shd_qz
        st rc re rf
        // range: any |q| > 16 -> not shadowed
        set2 re rf shd_qx
        ld ra re rf
        set rc 0x10
        cmp ra rc
        jmpi greater _shd_miss
        set rc 0xF0                 // -16
        cmp ra rc
        jmpi less _shd_miss
        set2 re rf shd_qy
        ld ra re rf
        set rc 0x10
        cmp ra rc
        jmpi greater _shd_miss
        set rc 0xF0
        cmp ra rc
        jmpi less _shd_miss
        set2 re rf shd_qz
        ld ra re rf
        set rc 0x10
        cmp ra rc
        jmpi greater _shd_miss
        set rc 0xF0
        cmp ra rc
        jmpi less _shd_miss
        // h2 = clamp8(mul_ss(qx,qx)+mul_ss(qy,qy)+mul_ss(qz,qz))
        set ry 0x00
        set rz 0x00
        set2 re rf shd_qx
        ld ra re rf
        mov rb ra
        call mul_ss
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf shd_qy
        ld ra re rf
        mov rb ra
        call mul_ss
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf shd_qz
        ld ra re rf
        mov rb ra
        call mul_ss
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        mov ra ry
        mov rb rz
        call clamp16to8             // rt = h2
        set rc 0x10
        cmp rt rc
        jmpi less _shd_yes          // h2 < 16 -> shadowed
        set rt 0x00
        goto _shd_ret
_shd_yes:
        set rt 0x01
        goto _shd_ret
_shd_miss:
        set rt 0x00
_shd_ret:
        pop2 ry rz
        pop2 re rf
        wsr raddr re rf
        ret

//------------------------------------------------------------------------------
// shade: rt8 shade(). Reads d_x,d_y,d_z, writes col_x,col_y,col_z. Non-leaf.
// Live across calls: ndl->ru, spec->rv, k->rw, chk->rw, ndl(floor)->rx.
//------------------------------------------------------------------------------
shade:
        rsr raddr re rf
        push2 re rf
        push2 ru rv
        push2 rw rx
        push2 ry rz
        call hit_sphere
        cmp rt r0
        jmpi not_equal _shade_hit
        set2 re rf d_y
        ld ra re rf
        set rc 0xEA                 // -22
        cmp ra rc
        jmpi less _shade_floor      // d.y < -22 -> floor
        goto _shade_sky
//---- sphere ----
_shade_hit:
        // ndl = clamp8(mul_uu(n.x,LIGHT.x)+mul_uu(n.y,LIGHT.y)+mul_uu(n.z,LIGHT.z))
        set ry 0x00
        set rz 0x00
        set2 re rf n_x
        ld ra re rf
        set rb 0xCA
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf n_y
        ld ra re rf
        set rb 0x6C
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf n_z
        ld ra re rf
        set rb 0xD5
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        mov ra ry
        mov rb rz
        call clamp16to8             // rt = ndl
        cmp rt r0
        jmpi greater_equal _sh_ndlok
        set rt 0x00
_sh_ndlok:
        mov ru rt                   // ru = ndl
        // hx=(LIGHT.x-d.x)>>1 ; hy=(LIGHT.y-d.y)>>1 ; hz=(LIGHT.z-d.z)>>1
        set ra 0xCA
        set2 re rf d_x
        ld rb re rf
        call halfdiff
        set2 re rf sh_hx
        st rt re rf
        set ra 0x6C
        set2 re rf d_y
        ld rb re rf
        call halfdiff
        set2 re rf sh_hy
        st rt re rf
        set ra 0xD5
        set2 re rf d_z
        ld rb re rf
        call halfdiff
        set2 re rf sh_hz
        st rt re rf
        // hl = sqrt_u(clamp8(hx*hx + hy*hy + hz*hz))
        set ry 0x00
        set rz 0x00
        set2 re rf sh_hx
        ld ra re rf
        mov rb ra
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf sh_hy
        ld ra re rf
        mov rb ra
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf sh_hz
        ld ra re rf
        mov rb ra
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        mov ra ry
        mov rb rz
        call clamp16to8
        mov ra rt
        call sqrt_u                 // rt = hl
        set2 re rf sh_hl
        st rt re rf
        // spec = clamp8(n.x*div_uu(hx,hl) + n.y*div_uu(hy,hl) + n.z*div_uu(hz,hl))
        set ry 0x00
        set rz 0x00
        set2 re rf sh_hx
        ld ra re rf
        set2 re rf sh_hl
        ld rb re rf
        call div128
        set2 re rf n_x
        ld ra re rf
        mov rb rt
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf sh_hy
        ld ra re rf
        set2 re rf sh_hl
        ld rb re rf
        call div128
        set2 re rf n_y
        ld ra re rf
        mov rb rt
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf sh_hz
        ld ra re rf
        set2 re rf sh_hl
        ld rb re rf
        call div128
        set2 re rf n_z
        ld ra re rf
        mov rb rt
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        mov ra ry
        mov rb rz
        call clamp16to8             // rt = spec
        cmp rt r0
        jmpi greater_equal _sh_specok
        set rt 0x00
_sh_specok:
        // spec = spec^8
        mov ra rt
        mov rb rt
        call mul_uu
        mov ra rt
        mov rb rt
        call mul_uu
        mov ra rt
        mov rb rt
        call mul_uu
        mov rv rt                   // rv = spec
        // k = 18 + mul_uu(109,ndl)
        set ra 0x6D                 // 109
        mov rb ru
        call mul_uu
        set rc 0x12                 // 18
        add rc rc rt
        mov rw rc                   // rw = k
        // col.x = clamp8(k + spec)
        mov ra rw
        mov rb rv
        call clamp8_sum2
        set2 re rf col_x
        st rt re rf
        // col.y = clamp8(mul_uu(k,38) + spec)
        mov ra rw
        set rb 0x26                 // 38
        call mul_uu
        mov ra rt
        mov rb rv
        call clamp8_sum2
        set2 re rf col_y
        st rt re rf
        // col.z = clamp8(mul_uu(k,28) + spec)
        mov ra rw
        set rb 0x1C                 // 28
        call mul_uu
        mov ra rt
        mov rb rv
        call clamp8_sum2
        set2 re rf col_z
        st rt re rf
        goto _shade_done
//---- floor ----
_shade_floor:
        // t = div_su((-16 - CAM_S.y), d.y) = div_uu(-21, d.y)
        set ra 0xEB                 // -21
        set2 re rf d_y
        ld rb re rf
        call div128
        mov ru rt                   // ru = t
        // px = CAM_S.x(0) + mul_su(t,d.x)
        mov ra ru
        set2 re rf d_x
        ld rb re rf
        call mul_uu
        set rc 0x00
        add rc rc rt
        set2 re rf fpx
        st rc re rf
        // py = -16
        set rc 0xF0
        set2 re rf fpy
        st rc re rf
        // pz = CAM_S.z(-30) + mul_su(t,d.z)
        mov ra ru
        set2 re rf d_z
        ld rb re rf
        call mul_uu
        set rc 0xE2                 // -30
        add rc rc rt
        set2 re rf fpz
        st rc re rf
        // chk = (((px>>4)+(pz>>4)) & 1) ? 30 : 115
        set2 re rf fpx
        ld ra re rf
        call asr4
        mov rv rt
        set2 re rf fpz
        ld ra re rf
        call asr4
        add rv rv rt
        set rc 0x01
        and rv rv rc
        cmp rv r0
        jmpi equal _fl_light
        set rw 0x1E                 // 30
        goto _fl_chkdone
_fl_light:
        set rw 0x73                 // 115
_fl_chkdone:
        // ndl = LIGHT.y(108) ; if shadowed(px,py,pz) ndl = 0
        set rx 0x6C
        set2 re rf fpx
        ld ra re rf
        set2 re rf fpy
        ld rb re rf
        set2 re rf fpz
        ld rc re rf
        call shadowed
        cmp rt r0
        jmpi equal _fl_noshadow
        set rx 0x00
_fl_noshadow:
        // k = 24 + mul_uu(103,ndl)
        set ra 0x67                 // 103
        mov rb rx
        call mul_uu
        set rc 0x18                 // 24
        add rc rc rt
        // col.x = col.y = col.z = mul_uu(chk,k)
        mov ra rw
        mov rb rc
        call mul_uu
        set2 re rf col_x
        st rt re rf
        set2 re rf col_y
        st rt re rf
        set2 re rf col_z
        st rt re rf
        goto _shade_done
//---- sky ----
_shade_sky:
        // k = (d.y + 127) >> 1
        set2 re rf d_y
        ld ra re rf
        set rb 0x7F
        call hsum
        mov rw rt                   // rw = k
        // col.x = 127 - mul_uu(k,38)
        mov ra rw
        set rb 0x26                 // 38
        call mul_uu
        set rc 0x7F
        sub rc rc rt
        set2 re rf col_x
        st rc re rf
        // col.y = 127 - mul_uu(k,23)
        mov ra rw
        set rb 0x17                 // 23
        call mul_uu
        set rc 0x7F
        sub rc rc rt
        set2 re rf col_y
        st rc re rf
        // col.z = 127
        set rc 0x7F
        set2 re rf col_z
        st rc re rf
_shade_done:
        pop2 ry rz
        pop2 rw rx
        pop2 ru rv
        pop2 re rf
        wsr raddr re rf
        ret

//------------------------------------------------------------------------------
// render_pixel: rt8 main() inner body for one pixel. in ra=x, rb=y.
// Emits three gamma-corrected colour bytes over UART. Non-leaf.
//------------------------------------------------------------------------------
render_pixel:
        rsr raddr re rf
        push2 re rf
        push2 ry rz
        // v.x = x - 64
        set rc 0x40
        sub ra ra rc
        set2 re rf vx
        st ra re rf
        // v.y = 64 - y
        set rc 0x40
        sub rb rc rb
        set2 re rf vy
        st rb re rf
        // v.z = 64
        set ra 0x40
        set2 re rf vz
        st ra re rf
        // len = sqrt_u(clamp8(vx*vx + vy*vy + vz*vz))
        set ry 0x00
        set rz 0x00
        set2 re rf vx
        ld ra re rf
        mov rb ra
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf vy
        ld ra re rf
        mov rb ra
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        set2 re rf vz
        ld ra re rf
        mov rb ra
        call mul_uu
        mov ra rt
        call sext8
        add2 ry rz ry rz ra rb
        mov ra ry
        mov rb rz
        call clamp16to8
        mov ra rt
        call sqrt_u                 // rt = len
        set2 re rf dlen
        st rt re rf
        // d.x = div_uu(vx,len)
        set2 re rf vx
        ld ra re rf
        set2 re rf dlen
        ld rb re rf
        call div128
        set2 re rf d_x
        st rt re rf
        // d.y = div_uu(vy,len)
        set2 re rf vy
        ld ra re rf
        set2 re rf dlen
        ld rb re rf
        call div128
        set2 re rf d_y
        st rt re rf
        // d.z = div_uu(vz,len)
        set2 re rf vz
        ld ra re rf
        set2 re rf dlen
        ld rb re rf
        call div128
        set2 re rf d_z
        st rt re rf
        // col = shade(d)
        call shade
        // emit col.x ; emit col.y ; emit col.z
        set2 re rf col_x
        ld ra re rf
        call emit
        set2 re rf col_y
        ld ra re rf
        call emit
        set2 re rf col_z
        ld ra re rf
        call emit
        pop2 ry rz
        pop2 re rf
        wsr raddr re rf
        ret

        .section bss
// interface vectors
d_x:    .space 1
d_y:    .space 1
d_z:    .space 1
n_x:    .space 1
n_y:    .space 1
n_z:    .space 1
col_x:  .space 1
col_y:  .space 1
col_z:  .space 1
// render_pixel
vx:     .space 1
vy:     .space 1
vz:     .space 1
dlen:   .space 1
// hit_sphere
hs_b:   .space 1
hs_px:  .space 1
hs_py:  .space 1
hs_pz:  .space 1
hs_h2:  .space 1
hs_s:   .space 1
// shade sphere half-vector
sh_hx:  .space 1
sh_hy:  .space 1
sh_hz:  .space 1
sh_hl:  .space 1
// shade floor point
fpx:    .space 1
fpy:    .space 1
fpz:    .space 1
// shadowed
shd_px: .space 1
shd_py: .space 1
shd_pz: .space 1
shd_b:  .space 1
shd_qx: .space 1
shd_qy: .space 1
shd_qz: .space 1

        .section vectors
        .space 14                   // no hardware vectors are used by this demo
        .addr _start                // reset vector at 0xFFFE
