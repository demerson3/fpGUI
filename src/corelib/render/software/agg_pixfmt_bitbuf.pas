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

var
  // minimum alpha value needed to mask a pixel. Perhaps it should be 1
  min_value_to_add_mask : byte = 51;

// When used as a transparency mask, nothing we draw should ever erase parts of the mask.
// So even if a "copy" pixfmt func gets called, it will blend, keeping all 1's
procedure pixfmt_bitbuf_noerase (var pixf : pixel_formats; rb : rendering_buffer_ptr);

// However, this pixfmt does a straight copy, which may set 1 bits to 0
procedure pixfmt_bitbuf_copyerase (var pixf : pixel_formats; rb : rendering_buffer_ptr);

// I can't imagine what it would mean to erase on blend, so I'm not implementing it


// calculate how long a row should be in bytes
function bitlen_to_bytelen_8bit  (x : longword) : longword;
function bitlen_to_bytelen_16bit (x : longword) : longword;
function bitlen_to_bytelen_32bit (x : longword) : longword;
function bitlen_to_bytelen_64bit (x : longword) : longword;

// dump the contents so we can examine them in the console
procedure dump_bits (rb : rendering_buffer_ptr; c0 : char = '+'; c1 : char = '#');



implementation


type
  byte_arr = array of byte;


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

function byte_diff_from_left_right (x1, x2 : unsigned) : unsigned;
  begin
    result := (x2 shr 3) - (x1 shr 3);
  end;

function byte_diff_from_left_length (x, len : unsigned) : unsigned;
  begin
    result := byte_diff_from_left_right (x, x + len -1);
  end;

{ functions that work on bits one at a time }

// Get a byte with this bit set to 1, e.g. x=5, result = 00000100
function focus_bit (x : longword) : byte; inline;
  begin
    result := bits[x and 7];  // equivalent to x mod 8
  end;

// Get a byte with this bit set to 0, e.g. x=5, result = 11111011
function focus_inv_bit (x : longword) : byte; inline;
  begin
    result := inv_bits[x and 7];  // equivalent to x mod 8
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

// as above, with a rendering_buffer param
function bitbuf_get_bool (rb : rendering_buffer_ptr; x, y : int) : boolean;
  begin
    result := (pbyte_of_bit(rb,x,y)^ and focus_bit(x)) >0;
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

procedure dump_ba (ba : byte_arr; c0 : char = '0'; c1 : char = '1');
  var
    i : longint;
    p : pbyte;
  begin
    p := @ba[0];
    for i := 0 to length(ba) shl 3 -1 do begin
      if pbyte_get_bool (p, i)
        then write (c1)
        else write (c0);
      if (i and 7 = 7) then begin // x and 7 is equivalent to x mod 8
        write (' ');
        inc(p);
        end;
      end;
    writeln ('=');
  end;

procedure dump_bits (rb : rendering_buffer_ptr; c0 : char = '+'; c1 : char = '#');
  var
    i,j : longint;
  begin with rb^ do begin
    writeln (_width, ' x ', _height);
    for j := 0 to _height-1 do begin
      for i := 0 to _width-1 do begin
        if bitbuf_get_bool(rb, i, j)
          then write (c1)
          else write (c0);
        if (i and 7 = 7) then write (' ');
        end;
      writeln;
      //if (j mod 4 = 3) then writeln;
      end;
    writeln;
  end; end;

// fill a column within a bit buffer with a particular byte - true/1/or version
procedure fill_col_or (p_top, p_bot : pbyte; the_byte : byte; pass_stride: int);
  begin
    while p_top<=p_bot do begin
      p_top^ := p_top^ or the_byte;
      inc(p_top, pass_stride);
      end;
  end;

// fill a column within a bit buffer with a particular byte - false/0/and version
procedure fill_col_and (p_top, p_bot : pbyte; the_byte : byte; pass_stride: int);
  begin
    while p_top<=p_bot do begin
      p_top^ := p_top^ and the_byte;
      inc(p_top, pass_stride);
      end;
  end;

procedure copy_vline_using_bool (this : pixel_formats_ptr; x, y : int; len : unsigned; bv : boolean);
  // overwrite every pixel in the range with color c
  var
    p, q : pbyte;
  begin
    if len=0 then exit;
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    if len=1 then begin
      pbyte_set_bool(p, x, bv);
      exit;
      end;
    q := pbyte_of_bit (this^.m_rbuf, x, y+len-1);
    if bv
      then fill_col_or  (p, q, focus_bit(x),     this^.m_rbuf^._stride)
      else fill_col_and (p, q, focus_inv_bit(x), this^.m_rbuf^._stride);
  end;

procedure fill_row_1 (p : pbyte; x, len: unsigned);
  // left_x lives in byte p, so definitely left_x < 7, and right_x > left_x
  var
    q : pbyte;
    byte_diff : longint;
    x2 : unsigned;
  begin
    x2 := x + len -1;
    byte_diff := byte_diff_from_left_right (x, x2);
    if byte_diff=0 then begin
      p^ := p^ or bytes[x and 7, x2 and 7];  // 0s,1s,0s eg  00111000
      exit;
      end;
    q := p + byte_diff;
    p^ := p^ or bytes[x and 7, 7];  // 0s then 1s eg 00011111 or 00000001
    q^ := q^ or bytes[0, x2 and 7]; // 1s then 0s eg 11000000 or 11111100
    // if p+1=q then there is still no in-between area to fill
    if byte_diff>1 then begin
      inc(p);
      fillbyte (p^, byte_diff-1, 255);
      end;
  end;

procedure fill_row_0 (p : pbyte; x, len: unsigned);
  // left_x lives in byte p, so definitely left_x < 7, and right_x > left_x
  var
    q : pbyte;
    byte_diff : longint;
    x2 : unsigned;
  begin
    x2 := x + len -1;
    byte_diff := byte_diff_from_left_right (x, x2);
    if byte_diff=0 then begin
      p^ := p^ and inv_bytes[x and 7, x2 and 7];  // inv eg 11000111
      exit;
      end;
    q := p + byte_diff;
    p^ := p^ and inv_bytes[x and 7, 7];  // 0s then 1s eg 00011111 or 00000001
    q^ := q^ and inv_bytes[0, x2 and 7]; // 1s then 0s eg 11000000 or 11111100
    // if p+1=q then there is still no in-between area to fill
    if byte_diff>1 then begin
      inc(p);
      fillbyte (p^, byte_diff-1, 0);
      end;
  end;

procedure copy_hline_using_bool (this : pixel_formats_ptr; x, y : int; len : unsigned; bv : boolean);
  // overwrite every pixel in the range with color c
  var
    p : pbyte;
  begin
    if len=0 then exit;
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    if len=1 then
      pbyte_set_bool (p, x, bv)
    else if bv then
      fill_row_1 (p, x, len)
    else
      fill_row_0 (p, x, len);
  end;

{ functions to convert between aggclr and bit (boolean) }

function bool_from_one_alpha (alpha : byte) : boolean; inline;
  begin
    result := alpha >= min_value_to_add_mask;
  end;

function bool_from_color (c : aggclr_ptr) : boolean; inline;
  begin
    result := bool_from_one_alpha (c^.a);
  end;

function bool_from_two_alphas (a, b : byte) : boolean; inline;
  begin
    result := bool_from_one_alpha ( ( (a+1) * (b+1) -1 ) shr 8);
    // the above was tested and works well: ( (a+1)*(b+1) -1 ) shr 8
    // aggpas ( a*(b+1) ) shr 8 is very skeptical i.e. (1,254) gives 0
  end;

function bool_from_color_and_cover (c : aggclr_ptr; cover : byte) : boolean; inline;
  begin
    result := bool_from_two_alphas (c^.a, cover);
  end;

function bitbytes_from_1color_and_covers (c : aggclr_ptr; covers : int8u_ptr;
    len : unsigned; pad : unsigned = 0) : byte_arr;
  var
    i : int;
    p : pbyte;
    alpha : byte;
  begin
    inc (len, pad);
    setlength (result, bitlen_to_bytelen_8bit(len));
    p := @result[pad shr 3];
    alpha := c^.a;
    for i := pad to len-1 do begin
      if bool_from_two_alphas (alpha, covers^)
        then p^ := p^ or focus_bit(i);
      inc (covers);
      if i and 7 = 0 then inc(p);
      end;
  end;

function bitbytes_from_colors (colors : aggclr_ptr;
    len : unsigned; pad : unsigned = 0) : byte_arr;
  var
    i : int;
    p : pbyte;
  begin
    inc (len, pad);
    setlength (result, bitlen_to_bytelen_8bit(len));
    p := @result[pad shr 3];
    for i := pad to len-1 do begin
      if bool_from_color (colors)
        then p^ := p^ or focus_bit(i);
      inc (colors, sizeof (aggclr));
      if i and 7 = 0 then inc(p);
      end;
  end;

function bitbytes_from_colors_and_1cover (colors : aggclr_ptr; cover : int8u;
    len : unsigned; pad : unsigned = 0) : byte_arr;
  var
    i : int;
    p : pbyte;
  begin
    if cover = 255 then begin
      result := bitbytes_from_colors (colors, len, pad);
      exit;
      end;
    inc (len, pad);
    setlength (result, bitlen_to_bytelen_8bit(len));
    p := @result[pad shr 3];
    for i := pad to len-1 do begin
      if bool_from_color_and_cover (colors, cover)
        then p^ := p^ or focus_bit(i);
      inc (colors, sizeof (aggclr));
      if i and 7 = 0 then inc(p);
      end;
  end;

function bitbytes_from_colors_and_covers (colors : aggclr_ptr; covers : int8u_ptr;
    len : unsigned; pad : unsigned = 0) : byte_arr;
  var
    i : int;
    p : pbyte;
  begin
    inc (len, pad);
    setlength (result, bitlen_to_bytelen_8bit(len));
    p := @result[pad shr 3];
    for i := pad to len-1 do begin
      if bool_from_color_and_cover (colors, covers^)
        then p^ := p^ or focus_bit(i);
      inc (colors, sizeof (aggclr));
      inc (covers);
      if i and 7 = 0 then inc(p);
      end;
  end;

procedure apply_bitbytes (this : pixel_formats_ptr; bb : byte_arr; x, y : int; len : unsigned);
  var
    pa, pb : pbyte;
    byte_diff : unsigned;
    i : longint;
  begin
    pa := pbyte_of_bit (this^.m_rbuf, x, y);
    pb := @bb[0];
    byte_diff := byte_diff_from_left_length (x, len);
    for i := 0 to byte_diff do begin
      pa^ := pa^ or pb^;
      inc(pa);
      inc(pb);
      end;
  end;

{ pixel_format functions }

procedure bitbuf_copy_pixel  (this : pixel_formats_ptr; x, y : int; c : aggclr_ptr );
  // overwrite this pixel with color c
  begin
    bitbuf_set_bool (this^.m_rbuf, x, y, bool_from_color (c));
  end;

procedure bitbuf_copy_pixel_noerase  (this : pixel_formats_ptr; x, y : int; c : aggclr_ptr );
  // overwrite this pixel with color c
  begin
    if bool_from_color (c)
      then bitbuf_set_bool (this^.m_rbuf, x, y, true);
  end;

procedure bitbuf_blend_pixel (this : pixel_formats_ptr; x, y : int; c : aggclr_ptr; cover : int8u );
  // blend this pixel with color c (for bitbuf, blend can only add not subtract)
  begin
    if bool_from_color_and_cover (c, cover)
      then bitbuf_set_bool (this^.m_rbuf, x, y, true);
  end;

procedure bitbuf_copy_hline  (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr );
  // overwrite every pixel in the range with color c
  begin
    copy_hline_using_bool (this, x, y, len, bool_from_color(c));
  end;

procedure bitbuf_copy_vline  (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr );
  // overwrite every pixel in the range with color c
  begin
    copy_vline_using_bool (this, x, y, len, bool_from_color(c));
  end;

procedure bitbuf_copy_hline_noerase  (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr );
  begin
    if bool_from_color(c)
      then copy_hline_using_bool (this, x, y, len, true);
    // else do nothing, no erasing a zero color
  end;

procedure bitbuf_copy_vline_noerase  (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr );
  begin
    if bool_from_color(c)
      then copy_vline_using_bool (this, x, y, len, true);
    // else do nothing, no erasing a zero color
  end;

procedure bitbuf_blend_hline (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
  // blend every pixel in the range with color c and singular cover
  begin
    if bool_from_color_and_cover (c, cover)
      then copy_hline_using_bool (this, x, y, len, true);
  end;

procedure bitbuf_blend_vline (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr; cover : int8u );
  // blend every pixel in the range with color c and singular cover
  begin
    if bool_from_color_and_cover (c, cover)
      then copy_vline_using_bool (this, x, y, len, true);
  end;

procedure bitbuf_blend_solid_hspan (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
  // blend every pixel in the range with color c, using covers array
  var
    bb : byte_arr;
  begin
    if len=0 then exit;
    if len=1 then begin
      if bool_from_color_and_cover(c, covers^)
        then bitbuf_set_bool (this^.m_rbuf, x, y, true);
      exit;
      end;
    bb := bitbytes_from_1color_and_covers (c, covers, len, x and 7);
    apply_bitbytes (this, bb, x, y, len);
  end;

procedure bitbuf_blend_solid_vspan (this : pixel_formats_ptr; x, y : int; len : unsigned; c : aggclr_ptr; covers : int8u_ptr );
  // blend every pixel in the range with color c, using covers array
  var
    p : pbyte;
    i, stride : longint;
    alpha : byte;
  begin
    if len=0 then exit;
    alpha := c^.a;
    if len=1 then begin
      if bool_from_two_alphas (alpha, covers^)
        then bitbuf_set_bool (this^.m_rbuf, x, y, true);
      exit;
      end;
    stride := this^.m_rbuf^._stride;
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    for i := 0 to len-1 do begin
      if bool_from_two_alphas (alpha, covers^)
        then pbyte_set_bool(p, x, true);
      inc (p, stride);
      inc (covers);
      end;
  end;

procedure bitbuf_copy_color_hspan (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr);
  // overwrite every pixel in the range with color from the colors array
  var
    p : pbyte;
    i : longint;
  begin
    if len=0 then exit;
    if len=1 then begin
      bitbuf_set_bool (this^.m_rbuf, x, y, bool_from_color(colors));
      exit;
      end;
    // I could use bitbytes_from_colors here, but how to deal with the edges?
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    for i := x to x+len-1 do begin
      if bool_from_color (colors)
        then pbyte_set_bool (p, x, true)
        else pbyte_set_bool (p, x, false);
      inc (colors, sizeof (aggclr));
      if i and 7 = 0 then inc(p);
      end;
  end;

procedure bitbuf_copy_color_vspan (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr );
  // overwrite every pixel in the range with color from the colors array
  var
    p : pbyte;
    i, stride : longint;
  begin
    if len=0 then exit;
    if len=1 then begin
      bitbuf_set_bool (this^.m_rbuf, x, y, bool_from_color(colors));
      exit;
      end;
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    stride := this^.m_rbuf^._stride;
    for i := 0 to len-1 do begin
      if bool_from_color (colors)
        then pbyte_set_bool (p, x, true)
        else pbyte_set_bool (p, x, false);
      inc (colors, sizeof (aggclr));
      inc (p, stride);
      end;
  end;

procedure bitbuf_copy_color_hspan_noerase (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr);
  // overwrite every 0 pixel in the range with color from the colors array (no 1->0)
  var
    bb : byte_arr;
  begin
    if len=0 then exit;
    if len=1 then begin
      if bool_from_color(colors)
        then bitbuf_set_bool (this^.m_rbuf, x, y, true);
      exit;
      end;
    bb := bitbytes_from_colors (colors, len, x and 7);
    apply_bitbytes (this, bb, x, y, len);
  end;

procedure bitbuf_copy_color_vspan_noerase (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr );
  // overwrite every 0 pixel in the range with color from the colors array (no 1->0)
  var
    p : pbyte;
    i, stride : longint;
  begin
    if len=0 then exit;
    if len=1 then begin
      if bool_from_color(colors)
        then bitbuf_set_bool (this^.m_rbuf, x, y, true);
      exit;
      end;
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    stride := this^.m_rbuf^._stride;
    for i := 0 to len-1 do begin
      if bool_from_color (colors)
        then pbyte_set_bool (p, x, true);
      inc (colors, sizeof (aggclr));
      inc (p, stride);
      end;
  end;

procedure bitbuf_blend_color_hspan (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
  var
    bb : byte_arr;
  begin
    if len=0 then exit;
    if assigned (covers) then
      bb := bitbytes_from_colors_and_covers (colors, covers, len, x and 7)
    else if bool_from_one_alpha(cover) then
      if cover < 255 then
        bb := bitbytes_from_colors_and_1cover (colors, cover, len, x and 7)
      else // cover = 255
        bb := bitbytes_from_colors (colors, len, x and 7)
    else exit; // cover is too small, no color will show
    apply_bitbytes (this, bb, x, y, len);
  end;

procedure bitbuf_blend_color_vspan (this : pixel_formats_ptr; x, y : int; len : unsigned; colors : aggclr_ptr; covers : int8u_ptr; cover : int8u );
  var
    p : pbyte;
    i, stride : longint;
  begin
    if len=0 then exit;
    p := pbyte_of_bit (this^.m_rbuf, x, y);
    stride := this^.m_rbuf^._stride;
    if assigned (covers) then
      for i := 0 to len-1 do begin
        if bool_from_color_and_cover (colors, covers^)
          then pbyte_set_bool (p, i, true);
        inc (colors, sizeof(aggclr));
        inc (covers);
        inc (p, stride);
        end
    else if cover = 255 then
      for i := 0 to len-1 do begin
        if bool_from_color (colors)
          then pbyte_set_bool (p, i, true);
        inc (colors, sizeof(aggclr));
        inc (p, stride);
        end
    else if bool_from_one_alpha(cover) then
      for i := 0 to len-1 do begin
        if bool_from_color_and_cover (colors, cover)
          then pbyte_set_bool (p, i, true);
        inc (colors, sizeof(aggclr));
        inc (p, stride);
        end;
    // else do nothing: cover is too small, no color will show
  end;

procedure pixfmt_bitbuf_noerase (var pixf : pixel_formats; rb : rendering_buffer_ptr);
  begin
    pixf.Construct(rb, 1, 0);
    pixf.m_pix_width:=1;

    pixf.copy_pixel  := @bitbuf_copy_pixel_noerase;
    pixf.blend_pixel := @bitbuf_blend_pixel;

    pixf.copy_hline  := @bitbuf_copy_hline_noerase;
    pixf.copy_vline  := @bitbuf_copy_vline_noerase;
    pixf.blend_hline := @bitbuf_blend_hline;
    pixf.blend_vline := @bitbuf_blend_vline;

    pixf.blend_solid_hspan  := @bitbuf_blend_solid_hspan;
    pixf.blend_solid_vspan  := @bitbuf_blend_solid_vspan;
 
    pixf.copy_color_hspan:=@bitbuf_copy_color_hspan_noerase;
    pixf.copy_color_vspan:=@bitbuf_copy_color_vspan_noerase;
    pixf.blend_color_hspan:=@bitbuf_blend_color_hspan;
    pixf.blend_color_vspan:=@bitbuf_blend_color_vspan;
  end;

procedure pixfmt_bitbuf_copyerase (var pixf : pixel_formats; rb : rendering_buffer_ptr);
  begin
    pixf.Construct(rb, 1, 0);
    pixf.m_pix_width:=1;

    pixf.copy_pixel  := @bitbuf_copy_pixel;
    pixf.blend_pixel := @bitbuf_blend_pixel;

    pixf.copy_hline  := @bitbuf_copy_hline;
    pixf.copy_vline  := @bitbuf_copy_vline;
    pixf.blend_hline := @bitbuf_blend_hline;
    pixf.blend_vline := @bitbuf_blend_vline;

    pixf.blend_solid_hspan  := @bitbuf_blend_solid_hspan;
    pixf.blend_solid_vspan  := @bitbuf_blend_solid_vspan;
 
    pixf.copy_color_hspan:=@bitbuf_copy_color_hspan;
    pixf.copy_color_vspan:=@bitbuf_copy_color_vspan;
    pixf.blend_color_hspan:=@bitbuf_blend_color_hspan;
    pixf.blend_color_vspan:=@bitbuf_blend_color_vspan;
  end;

end.

