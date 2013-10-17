unit agg_pixfmt_bitbuf;

{$mode objfpc}
{$rangechecks on}

interface

uses
  Classes,
  SysUtils,
  agg_basics,
  agg_pixfmt,
  agg_color,
  agg_rendering_buffer;

const
  min_true_val = 20; // max is 255, so less than 10% is treated as transparent

procedure pixfmt_bitbuf (var pixf : pixel_formats; rb : rendering_buffer_ptr);

// calculate how long a row should be in bytes
function bitlen_to_bytelen_8bit  (x : longword) : longword;
function bitlen_to_bytelen_16bit (x : longword) : longword;
function bitlen_to_bytelen_32bit (x : longword) : longword;
function bitlen_to_bytelen_64bit (x : longword) : longword;

// dump the contents so we can examine them in the console
procedure dump_bits (rb : rendering_buffer_ptr);



implementation


type
  byte_arr = array of byte;

var
  bb_colors : array [false..true] of aggclr;

const
  {$ifdef ENDIAN_LITTLE}
    bit0=128; bit1=64; bit2=32; bit3=16;
    bit4=8;   bit5=4;  bit6=2;  bit7=1;
  {$else ENDIAN_BIG}
    // not tested. I don't have a big endian machine.
    bit0=1;  bit1=2;  bit2=4;  bit3=8;
    bit4=16; bit5=32; bit6=64; bit7=128;
  {$endif}
  ibit0=255-bit0; ibit1=255-bit1; ibit2=255-bit2; ibit3=255-bit3;
  ibit4=255-bit4; ibit5=255-bit5; ibit6=255-bit6; ibit7=255-bit7;

  bits : array[0..7] of byte =
    (bit0,  bit1,  bit2,  bit3,  bit4,  bit5,  bit6,  bit7);
  inv_bits : array[0..7] of byte =
    (ibit0, ibit1, ibit2, ibit3, ibit4, ibit5, ibit6, ibit7);

  fillval : array[boolean] of byte = (0, 255);

  bytes : array [0..7,0..7] of byte =
    ((bit0, bit0 or bit1, bit0 or bit1 or bit2, bit0 or bit1 or bit2 or bit3,
        bit0 or bit1 or bit2 or bit3 or bit4,
        bit0 or bit1 or bit2 or bit3 or bit4 or bit5,
        bit0 or bit1 or bit2 or bit3 or bit4 or bit5 or bit6,
        255),
    (0, bit1, bit1 or bit2, bit1 or bit2 or bit3,
        bit1 or bit2 or bit3 or bit4, bit1 or bit2 or bit3 or bit4 or bit5,
        bit1 or bit2 or bit3 or bit4 or bit5 or bit6,
        bit1 or bit2 or bit3 or bit4 or bit5 or bit6 or bit7),
    (0, 0, bit2, bit2 or bit3, bit2 or bit3 or bit4,
        bit2 or bit3 or bit4 or bit5, bit2 or bit3 or bit4 or bit5 or bit6,
        bit2 or bit3 or bit4 or bit5 or bit6 or bit7),
    (0, 0, 0, bit3, bit3 or bit4, bit3 or bit4 or bit5,
        bit3 or bit4 or bit5 or bit6, bit3 or bit4 or bit5 or bit6 or bit7),
    (0, 0, 0, 0, bit4, bit4 or bit5, bit4 or bit5 or bit6,
        bit4 or bit5 or bit6 or bit7),
    (0, 0, 0, 0, 0, bit5, bit5 or bit6, bit5 or bit6 or bit7),
    (0, 0, 0, 0, 0, 0, bit6, bit6 or bit7),
    (0, 0, 0, 0, 0, 0, 0, bit7)
    );

  inv_bytes : array [0..7,0..7] of byte =
    ((ibit0, ibit0 and ibit1, ibit0 and ibit1 and ibit2,
        ibit0 and ibit1 and ibit2 and ibit3,
        ibit0 and ibit1 and ibit2 and ibit3 and ibit4,
        ibit0 and ibit1 and ibit2 and ibit3 and ibit4 and ibit5,
        ibit0 and ibit1 and ibit2 and ibit3 and ibit4 and ibit5 and ibit6,
        0),
    (255, ibit1, ibit1 and ibit2, ibit1 and ibit2 and ibit3,
        ibit1 and ibit2 and ibit3 and ibit4,
        ibit1 and ibit2 and ibit3 and ibit4 and ibit5,
        ibit1 and ibit2 and ibit3 and ibit4 and ibit5 and ibit6,
        ibit1 and ibit2 and ibit3 and ibit4 and ibit5 and ibit6 and ibit7),
    (255, 255, ibit2, ibit2 and ibit3, ibit2 and ibit3 and ibit4,
        ibit2 and ibit3 and ibit4 and ibit5,
        ibit2 and ibit3 and ibit4 and ibit5 and ibit6,
        ibit2 and ibit3 and ibit4 and ibit5 and ibit6 and ibit7),
    (255, 255, 255, ibit3, ibit3 and ibit4, ibit3 and ibit4 and ibit5,
        ibit3 and ibit4 and ibit5 and ibit6,
        ibit3 and ibit4 and ibit5 and ibit6 and ibit7),
    (255, 255, 255, 255, ibit4, ibit4 and ibit5, ibit4 and ibit5 and ibit6,
        ibit4 and ibit5 and ibit6 and ibit7),
    (255, 255, 255, 255, 255, ibit5, ibit5 and ibit6, ibit5 and ibit6 and ibit7),
    (255, 255, 255, 255, 255, 255, ibit6, ibit6 and ibit7),
    (255, 255, 255, 255, 255, 255, 255, ibit7)
    );

{ num_1_bits_in_byte : array [0..255] of byte = (
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,
    4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8); }

{  byte_strings : array [0..255] of string[8] = (
    '00000000', '00000001', '00000010', '00000011',  '00000100', '00000101', '00000110', '00000111',
    '00001000', '00001001', '00001010', '00001011',  '00001100', '00001101', '00001110', '00001111',
    '00010000', '00010001', '00010010', '00010011',  '00010100', '00010101', '00010110', '00010111',
    '00011000', '00011001', '00011010', '00011011',  '00011100', '00011101', '00011110', '00011111',
    '00100000', '00100001', '00100010', '00100011',  '00100100', '00100101', '00100110', '00100111',
    '00101000', '00101001', '00101010', '00101011',  '00101100', '00101101', '00101110', '00101111',
    '00110000', '00110001', '00110010', '00110011',  '00110100', '00110101', '00110110', '00110111',
    '00111000', '00111001', '00111010', '00111011',  '00111100', '00111101', '00111110', '00111111',
    '01000000', '01000001', '01000010', '01000011',  '01000100', '01000101', '01000110', '01000111',
    '01001000', '01001001', '01001010', '01001011',  '01001100', '01001101', '01001110', '01001111',
    '01010000', '01010001', '01010010', '01010011',  '01010100', '01010101', '01010110', '01010111',
    '01011000', '01011001', '01011010', '01011011',  '01011100', '01011101', '01011110', '01011111',
    '01100000', '01100001', '01100010', '01100011',  '01100100', '01100101', '01100110', '01100111',
    '01101000', '01101001', '01101010', '01101011',  '01101100', '01101101', '01101110', '01101111',
    '01110000', '01110001', '01110010', '01110011',  '01110100', '01110101', '01110110', '01110111',
    '01111000', '01111001', '01111010', '01111011',  '01111100', '01111101', '01111110', '01111111',
    '10000000', '10000001', '10000010', '10000011',  '10000100', '10000101', '10000110', '10000111',
    '10001000', '10001001', '10001010', '10001011',  '10001100', '10001101', '10001110', '10001111',
    '10010000', '10010001', '10010010', '10010011',  '10010100', '10010101', '10010110', '10010111',
    '10011000', '10011001', '10011010', '10011011',  '10011100', '10011101', '10011110', '10011111',
    '10100000', '10100001', '10100010', '10100011',  '10100100', '10100101', '10100110', '10100111',
    '10101000', '10101001', '10101010', '10101011',  '10101100', '10101101', '10101110', '10101111',
    '10110000', '10110001', '10110010', '10110011',  '10110100', '10110101', '10110110', '10110111',
    '10111000', '10111001', '10111010', '10111011',  '10111100', '10111101', '10111110', '10111111',
    '11000000', '11000001', '11000010', '11000011',  '11000100', '11000101', '11000110', '11000111',
    '11001000', '11001001', '11001010', '11001011',  '11001100', '11001101', '11001110', '11001111',
    '11010000', '11010001', '11010010', '11010011',  '11010100', '11010101', '11010110', '11010111',
    '11011000', '11011001', '11011010', '11011011',  '11011100', '11011101', '11011110', '11011111',
    '11100000', '11100001', '11100010', '11100011',  '11100100', '11100101', '11100110', '11100111',
    '11101000', '11101001', '11101010', '11101011',  '11101100', '11101101', '11101110', '11101111',
    '11110000', '11110001', '11110010', '11110011',  '11110100', '11110101', '11110110', '11110111',
    '11111000', '11111001', '11111010', '11111011',  '11111100', '11111101', '11111110', '11111111'); }


{ calculate how long a row should be in bytes }

function bitlen_to_bytelen_8bit  (x : longword) : longword;
  begin
    result := (x + 7) div 8;
  end;

function bitlen_to_bytelen_16bit (x : longword) : longword;
  begin
    result := ((x + 15) div 16) shl 1;
  end;

function bitlen_to_bytelen_32bit (x : longword) : longword;
  begin
    result := ((x + 31) div 32) shl 2;
  end;

function bitlen_to_bytelen_64bit (x : longword) : longword;
  begin
    result := ((x + 63) div 64) shl 3;
  end;

{ functions that work on bits one at a time }

// Get a byte with this bit set to 1, e.g. x=5, result = 00000100
function focus_bit (x : longword) : byte; inline;
  begin
    result := bits[x mod 8];
  end;

// Get a byte with this bit set to 0, e.g. x=5, result = 11111011
function focus_inv_bit (x : longword) : byte; inline;
  begin
    result := inv_bits[x mod 8];
  end;

// get a pointer to the byte that contains the wanted bit x,y
function pbyte_of_bit (rb : rendering_buffer_ptr; x, y : int) : pbyte;
  begin
    result := rb^.row (y);
    inc (result, x shr 3);
  end;

// get the value of bit x within the byte, as a boolean. x mod 8 is performed.
function pbyte_get_bool (p : pbyte; x : int) : boolean; inline;
  begin
    result := (p^ and focus_bit(x)) >0;
  end;

// as above, but return a number rather than a boolean
function pbyte_get_bit  (p : pbyte; x : int) : byte; inline;
  begin
    if pbyte_get_bool (p, x)
      then result := 1
      else result := 0;
  end;

// as above, with a rendering_buffer param
function bitbuf_get_bool (rb : rendering_buffer_ptr; x, y : int) : boolean;
  begin
    result := (pbyte_of_bit(rb,x,y)^ and focus_bit(x)) >0;
  end;

// as above
function bitbuf_get_bit  (rb : rendering_buffer_ptr; x, y : int) : byte;
  begin
    result := pbyte_get_bit (pbyte_of_bit(rb,x,y), x);
  end;

// set the value of bit x within the byte. x mod 8 is performed.
procedure pbyte_set_bool (p : pbyte; x : int; b : boolean); inline;
  begin
    if b
      then p^ := p^ or focus_bit(x)
      else p^ := p^ and focus_inv_bit(x);
  end;

// as above, with a rendering_buffer param
procedure bitbuf_set_bool (rb : rendering_buffer_ptr; x, y : int; b : boolean);
  begin
    pbyte_set_bool (pbyte_of_bit (rb,x,y), x, b);
  end;

{ functions that work with groups of bits }

procedure dump_ba (ba : byte_arr);
  var
    i : longint;
    p : pbyte;
  begin
    p := @ba[0];
    for i := 0 to length(ba) shl 3 -1 do begin
      write (pbyte_get_bit(p, i));
      if (i mod 8 = 7) then begin
        write (' ');
        inc(p);
        end;
      end;
    writeln ('=');
  end;

// fill a column within a bit buffer with a particular byte - true/1/or version
procedure fill_col_1 (p_top, p_bot : pbyte; the_byte : byte; pass_stride: int);
  var
    pp : pbyte;
  begin
    pp := p_top;
    while pp<=p_bot do begin
      pp^ := pp^ or the_byte;
      inc(pp, pass_stride);
      end;
  end;

// fill a column within a bit buffer with a particular byte - false/0/and version
procedure fill_col_0 (p_top, p_bot : pbyte; the_byte : byte; pass_stride: int);
  var
    pp : pbyte;
  begin
    pp := p_top;
    while pp<=p_bot do begin
      pp^ := pp^ and the_byte;
      inc(pp, pass_stride);
      end;
  end;

// each bit (0 or 1) is shown as a letter
// abcdefgh ijklmnop pad_left 3
// 000abcde fghijklm nop00000
// first byte: abcde goes on the right, left is nil
// second byte: ijklm goes on right, fgh goes on left
// third byte: right is nil, nop goes on left
// ANOTHER EXAMPLE - this time result has same num bytes as ba_in
// abcdefgh ijklmnop qrs00000 pad_left 3
// 000abcde fghijklm nopqrs00
function pad_bit_arr (ba_in : byte_arr; old_len, pad_amount : int) : byte_arr;
  var
    new_high_bit : int;
    i : longint;
    l_shift, r_shift : byte;
    left_p, right_p, new_p : pbyte;
  begin
    pad_amount := pad_amount mod 8;
    //writeln ('pad ', pad_amount);
    //dump_ba (ba_in);
    if pad_amount=0 then begin
      result := ba_in;
      //dump_ba (result);
      exit;
      end;
    new_high_bit := (old_len-1) + pad_amount;
    setlength (result, bitlen_to_bytelen_8bit(new_high_bit+1));
    r_shift := pad_amount and 7;
    l_shift := 8-r_shift;
    // first byte:
    new_p := @result[0];
    right_p := @ba_in[0];
    new_p^ := right_p^ shr r_shift;
    // second byte through last original byte:
    for i := 1 to (old_len-1) shr 3 do begin
      left_p := right_p;
      inc (right_p);
      inc (new_p);
      new_p^ := (right_p^ shr r_shift) or byte(left_p^ shl l_shift);
      end;
    // last new byte, if needed:
    if (length(result) > length(ba_in)) then begin
      left_p := right_p;
      inc (new_p);
      new_p^ := byte (left_p^ shl l_shift);
      end;
    //dump_ba (result);
  end;

{ functions to convert between aggclr and bit (boolean) }

function bool_from_color (c : aggclr_ptr) : boolean; //inline;
  begin
    result := (c^.a >= min_true_val)
  end;

function bool_from_color_and_cover (c : aggclr_ptr; cover : byte) : boolean; //inline;
  begin
    result := ( (c^.a+1) * (cover+1) -1 ) shr 8 > min_true_val;
  end;

function bitbytes_from_colors (colors : aggclr_ptr; len : unsigned) : byte_arr;
  var
    i : int;
    p : pbyte;
  begin
    setlength (result, bitlen_to_bytelen_8bit(len));
    p := @result[0];
    i := 0;
    while i<len do begin
      if bool_from_color (colors)
        then p^ := p^ or focus_bit(i);
      inc(i);
      inc (colors, sizeof (aggclr));
      if (i mod 8) = 0 then inc(p);
      end;
  end;

function bitbytes_from_1color_and_covers (c : aggclr_ptr; covers : int8u_ptr; len : unsigned) : byte_arr;
  var
    i, nlen : int;
    p : pbyte;
  begin
    nlen := bitlen_to_bytelen_8bit(len);
    setlength (result, nlen);
    p := @result[0];
    i := 0;
    while i<len do begin
      if bool_from_color_and_cover (c, covers^)
        then p^ := p^ or focus_bit(i);
      inc(i);
      inc (covers);
      if (i mod 8) = 0 then inc(p);
      end;
  end;

function bitbytes_from_colors_and_covers (colors : aggclr_ptr; covers : int8u_ptr; len : unsigned) : byte_arr;
  var
    i : int;
    p : pbyte;
  begin
    setlength (result, bitlen_to_bytelen_8bit(len));
    p := @result[0];
    i := 0;
    while i<len do begin
      if bool_from_color_and_cover (colors, covers^)
        then p^ := p^ or focus_bit(i);
      inc(i);
      inc (colors, sizeof (aggclr));
      inc (covers);
      if (i mod 8) = 0 then inc(p);
      end;
  end;

{ pixel_format functions }

function bitbuf_pixel (this : pixel_formats_ptr; x, y : int ) : aggclr;
  begin
    result := bb_colors[bitbuf_get_bool (this^.m_rbuf, x, y)];
  end;

// function bitbuf_row   (this : pixel_formats_ptr; x, y : int ) : row_data_type;

procedure bitbuf_copy_pixel  (this : pixel_formats_ptr; x, y : int; c : aggclr_ptr );
  begin
    bitbuf_set_bool (this^.m_rbuf, x, y, bool_from_color (c));
  end;

procedure bitbuf_blend_pixel (this : pixel_formats_ptr; x, y : int; c : aggclr_ptr; cover : int8u );
  begin
    if bool_from_color(c)
      then bitbuf_set_bool (this^.m_rbuf, x, y, true);
  end;

procedure bitbuf_copy_hline  (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr );
  var
    x2, fill_count : int;
    p, q : pbyte;
    bv : boolean;
  begin
    if len=0 then exit;
    bv := bool_from_color(c);
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    if len=1 then begin
      pbyte_set_bool(p, x, bv);
      exit;
      end;
    x2 := x+len-1;
    q := pbyte_of_bit (this^.m_rbuf, x2, y);
    if p=q then
      if bv
        then p^ := p^ or bytes[x mod 8, x2 mod 8]  // 0s,1s,0s eg  00111000
        else p^ := p^ and inv_bytes[x mod 8, x2 mod 8]  // inv eg 11000111
    else begin // q>p
      // first get the mixed spots
      if bv then begin
        p^ := p^ or bytes[x mod 8, 7]; // 0s then 1s eg 00011111 or 00000001
        q^ := q^ or bytes[0, x2 mod 8]; // 1s then 0s eg 11000000 or 11111100
        end
      else {not bv} begin
        p^ := p^ and inv_bytes[x mod 8, 7];  // inv of above p e.g. 11100000
        q^ := q^ and inv_bytes[0, x2 mod 8];
        end;
      inc(p);
      // now fill the area in-between
      if (p<q) then begin
        fill_count := (x2 shr 3) - (x shr 3) -1;
        fillbyte (p^, fill_count, fillval[bv]);
        end;
      end
  end;

procedure bitbuf_copy_vline  (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr );
  var
    y2, stride : int;
    p, q : pbyte;
    bv : boolean;
  begin
    if len=0 then exit;
    bv := bool_from_color(c);
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    if len=1 then begin
      pbyte_set_bool(p, x, bv);
      exit;
      end;
    y2 := y+len-1;
    q := pbyte_of_bit (this^.m_rbuf, x, y2);
    if bv
      then fill_col_1 (p, q, focus_bit(x),     this^.m_rbuf^._stride)
      else fill_col_0 (p, q, focus_inv_bit(x), this^.m_rbuf^._stride);
  end;

procedure bitbuf_blend_hline (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
  begin
    if bool_from_color(c)
      then bitbuf_copy_hline (this, x, y, len, c);
  end;

procedure bitbuf_blend_vline (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
  begin
    if bool_from_color(c)
      then bitbuf_copy_vline (this, x, y, len, c);
  end;

procedure bitbuf_blend_solid_hspan (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
  var
    bb : byte_arr;
    pa, pb, palast, pblast : pbyte;
    i : longint;
  begin
    if len=0 then exit;
    if len=1 then begin
      if bool_from_color(c) then bitbuf_set_bool (this^.m_rbuf, x, y, true);
      exit;
      end;
    pa     := pbyte_of_bit (this^.m_rbuf, x, y);
    palast := pbyte_of_bit (this^.m_rbuf, x+len-1, y);
    bb := bitbytes_from_1color_and_covers (c, covers, len);
    bb := pad_bit_arr (bb, len, x);
    pb     := @bb[0];
    pblast := @bb[high(bb)];
    while (pa<=palast) and (pb<=pblast) do begin
      pa^ := pa^ or pb^;
      inc(pa);
      inc(pb);
      end;
  end;

procedure bitbuf_blend_solid_vspan (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
  var
    p, q : pbyte;
  begin
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    q := pbyte_of_bit (this^.m_rbuf, x, y+len-1);
    while p<=q do begin
      if bool_from_color_and_cover (c, covers^)
        then pbyte_set_bool(p, x, true);
      inc (c, sizeof(aggclr));
      inc (covers);
      end;
  end;

//procedure bitbuf_copy_color_hspan (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr);
//  var
//    bb : byte_arr;
//  begin
//    if len=0 then exit;
//    if len=1 then begin
//      bitbuf_set_bool (this^.m_rbuf, x, y, bool_from_color(c));
//      exit;
//      end;
//    bb := bitbytes_from_colors (colors, len);
//    bb := pad_bit_arr (bb, len, x);
//    
//  end;

(*
procedure bitbuf_blender   (this : pixel_formats_ptr; op : unsigned; p : int8u_ptr; cr, cg, cb, ca, cover : unsigned );
procedure bitbuf_blend_pix (this : pixel_formats_ptr; p : int8u_ptr; cr, cg, cb, alpha, cover : unsigned );


procedure bitbuf_copy_color_vspan (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr );

procedure bitbuf_blend_color_hspan (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
procedure bitbuf_blend_color_vspan (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );

procedure bitbuf_copy_from  (this : pixel_formats_ptr; from : rendering_buffer_ptr; xdst, ydst, xsrc, ysrc : int; len : unsigned );
procedure bitbuf_blend_from (this : pixel_formats_ptr; from : pixel_formats_ptr; psrc_ : int8u_ptr; xdst, ydst, xsrc, ysrc : int; len : unsigned; cover : int8u );

procedure bitbuf_blend_from_color (this : pixel_formats_ptr; from : pixel_formats_ptr; color : aggclr_ptr; xdst, ydst, xsrc, ysrc : int; len : unsigned; cover : int8u );
procedure bitbuf_blend_from_lut   (this : pixel_formats_ptr; from : pixel_formats_ptr; color_lut : aggclr_ptr; xdst, ydst, xsrc, ysrc : int; len : unsigned; cover : int8u );

procedure bitbuf_apply_gamma    (this : pixel_formats_ptr; p : int8u_ptr );
procedure bitbuf_for_each_pixel (this : pixel_formats_ptr; f : func_apply_gamma );


blender : func_blender;

copy_hline : func_copy_hline;
copy_vline : func_copy_vline;

blend_hline : func_blend_hline;
blend_vline : func_blend_vline;

blend_solid_hspan : func_blend_solid_hspan;
blend_solid_vspan : func_blend_solid_vspan;

copy_color_hspan : func_copy_color_hspan;
copy_color_vspan : func_copy_color_vspan;

blend_color_hspan : func_blend_color_hspan;
blend_color_vspan : func_blend_color_vspan;

copy_from  : func_copy_from;
blend_from : func_blend_from;

blend_from_color : func_blend_from_color;
blend_from_lut   : func_blend_from_lut;

for_each_pixel  : func_for_each_pixel;
gamma_dir_apply ,
gamma_inv_apply : func_apply_gamma;

pixel_premultiply ,
pixel_demultiply  : func_apply_gamma;

*)

procedure pixfmt_bitbuf (var pixf : pixel_formats; rb : rendering_buffer_ptr);
  begin
    pixf.Construct(rb, 1, 0);
    pixf.m_pix_width:=1;

    pixf.copy_pixel  := @bitbuf_copy_pixel;
    pixf.blend_pixel := @bitbuf_blend_pixel;

    pixf.pixel:= @bitbuf_pixel;
    //pixf.row  := @bitbuf_row;

    pixf.copy_hline  := @bitbuf_copy_hline;
    pixf.copy_vline  := @bitbuf_copy_vline;
    pixf.blend_hline := @bitbuf_blend_hline;
    pixf.blend_vline := @bitbuf_blend_vline;

    pixf.blend_solid_hspan  := @bitbuf_blend_solid_hspan;
    pixf.blend_solid_vspan  := @bitbuf_blend_solid_vspan;
 
    //pixf.copy_color_hspan:=@bitbuf_copy_color_hspan;
    //pixf.copy_color_vspan:=@bitbuf_copy_color_vspan;
    //pixf.blend_color_hspan:=@gray8_blend_color_hspan;
    //pixf.blend_color_vspan:=@gray8_blend_color_vspan;

    //pixf.copy_from :=@gray8_copy_from;
    //pixf.blend_from:=NIL; // not defined in agg_pixfmt_gray.h
    //
    //pixf.blend_from_color:=@gray8_blend_from_color;
    //pixf.blend_from_lut  :=@gray8_blend_from_lut;
    //
    //pixf.for_each_pixel :=@gray_for_each_pixel;
    //pixf.gamma_dir_apply:=@gray_gamma_dir_apply;
    //pixf.gamma_inv_apply:=@gray_gamma_inv_apply;
    
  end;

procedure dump_bits (rb : rendering_buffer_ptr);
  var
    i,j : longint;
  begin with rb^ do begin
    writeln (_width, ' x ', _height);
    for j := 0 to _height-1 do begin
      for i := 0 to _width-1 do begin
        if bitbuf_get_bool(rb, i, j)
          then write ('#')
          else write ('+');
        if (i mod 8 = 7) then write (' ');
        end;
      writeln;
      //if (j mod 4 = 3) then writeln;
      end;
    writeln;
  end; end;

initialization
  bb_colors[true].ConstrInt(high(int8u), high(int8u));
  bb_colors[false].ConstrInt(0,0);

end.

