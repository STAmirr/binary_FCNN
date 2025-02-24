library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.numeric_std.all;
use IEEE.STD_LOGIC_ARITH.all;

package custom_package is

    constant clk_period : time := 10 ns;
    constant image_size_x : integer := 100; -- input array size
    constant image_size_y : integer := 100; -- input array size
    constant layer1_size_x : integer := 10; -- layer1 node size    
    constant layer1_size_y : integer := 10; -- layer1 node size
    constant layer2_size_x : integer := 4; -- layer2 node size (Red - Green - Blue - mono)
    constant layer2_size_y : integer := 3; -- layer2 node size (Circle - Square - Triange)
    constant layer1_count_x : integer := 10; -- image_size_x ; -- number of neurons in x of layer 1 equals 10
    constant layer1_count_y : integer := 10; -- image_size_y / layer1_size_y; -- number of neurons in y of layer 1 equals 10

    type string_array is array (natural range <>) of string(1 to 22);

    ----- types for pixels
    type byte is array (7 downto 0) of std_logic;
    type rgb is array (2 downto 0) of byte; -- 2nd Byte is Red, 1th Byte is Green, 0th Byte is blue.
    type pixel_array is array (natural range <>, natural range <>) of rgb;
    type bit_array is array (natural range <>, natural range <>) of std_logic;
    type bit_pack is array (natural range <>, natural range <>) of bit_array(1 to layer1_size_x, 1 to layer1_size_y);
    type pix_array_pack1 is array (natural range <>, natural range <>) of pixel_array(1 to layer1_size_x, 1 to layer1_size_y); ------ set by layer1_size_x and layer1_size_y
    type pix_pack_all is array (natural range <>, natural range <>) of pix_array_pack1(1 to layer1_size_x, 1 to layer1_size_y); ------ set by layer1_size_x and layer1_size_y


    ----- types for 32bit pixels
    type byte32 is array (31 downto 0) of std_logic;
    type rgb32 is array (2 downto 0) of byte32;
    type pixel_array32 is array (natural range <>, natural range <>) of rgb32;
    type out_state is (none, training, Red_Circle, Red_Square, Red_Triangle,
        Green_Circle, Green_Square, Green_Triangle,
        Blue_Circle, Blue_Square, Blue_Triangle,
        Mono_Circle, Mono_Square, Mono_Triangle);
    type pix_array_pack2 is array (natural range <>, natural range <>) of pixel_array32(1 to layer2_size_x, 1 to layer2_size_y); ------ set by layer2_size_x and layer2_size_y
    type pix_array_pack1_32 is array (natural range <>, natural range <>) of pixel_array32(1 to layer1_size_x, 1 to layer1_size_y); ------ set by layer2_size_x and layer2_size_y

    ----- types for weight
    type rgb_w is array (2 downto 0) of std_logic; -- 2nd Byte is Red, 1th Byte is Green, 0th Byte is blue ('0' disable - '1' enabled)
    type weight_array is array (natural range <>, natural range <>) of rgb_w;
    type weight_array_pack1 is array (natural range <>, natural range <>) of weight_array(1 to layer1_size_x, 1 to layer1_size_y); ------ set by layer1_size_x and layer1_size_y
    type weight_pack_all is array (natural range <>, natural range <>) of weight_array_pack1(1 to layer1_size_x, 1 to layer1_size_y); ------ set by layer1_size_x and layer1_size_y
    type training_state is (start, inc, comp, done);

    type done_array is array (natural range <>, natural range <>) of std_logic_vector(5 downto 0);

    ----- overloaded functions                   
    function "=" (L : pixel_array(natural range <>, natural range <>); R : pixel_array(natural range <>, natural range <>)) return boolean;
    function ">" (L : byte; R : byte) return boolean;
    function "<" (L : byte; R : byte) return boolean;

    function "AND" (L, R : byte) return byte;
    function "AND" (L, R : byte32) return byte32;
    function "NOT" (a : byte) return byte;
    function "+" (L : byte32; Rin : byte) return byte32;
    function "+" (L, R : byte32) return byte32;
    function "+" (L, R : byte) return byte;
    function "-" (L : byte; R : byte32) return byte32;
    function "-" (L : byte; R : byte) return byte;
    function "&"(a : in std_logic_vector(3 downto 0); b : in std_logic_vector(3 downto 0)) return byte;

    ----- project functions
    function to_32 (a : byte) return byte32;
    function rgb_to_32 (a : rgb) return rgb32;
    function pixel_array_to_32 (b : pixel_array(natural range <>, natural range <>)) return pixel_array32;
    function image_selector (a : integer) return string;
    function string_to_nibble(input_char : character) return std_logic_vector;

    procedure array_all_zero(signal arr : in bit_array; signal outp : out std_logic);
    procedure index_finder(signal a : in bit_array(natural range <>, natural range <>); signal outp : out out_state);
    procedure greatest_node(signal a : in pixel_array32(natural range <>, natural range <>); signal b : out out_state);
    procedure dicing(signal L : in pixel_array(natural range <>, natural range <>); signal R : out pix_array_pack1(natural range <>, natural range <>));

end package custom_package;

package body custom_package is

    function "=" (L : pixel_array(natural range <>, natural range <>); R : pixel_array(natural range <>, natural range <>)) return boolean is
        variable resault : boolean := true;
    begin
        for i in L'range(2) loop
            for j in L'range(1) loop
                if (l(j, i) = r(j, i)) then
                    resault := (resault and true);
                else
                    resault := (resault and false);
                end if;
            end loop;
        end loop;
        return resault;
    end function;

    function ">" (L : byte; R : byte) return boolean is
        variable resault : boolean := true;
    begin
        for i in L'range loop
            if (L(i) > R(i)) then
                resault := (resault and true);
            else
                resault := (resault and false);
            end if;
        end loop;
        return resault;
    end function;

    function "<" (L : byte; R : byte) return boolean is
        variable resault : boolean := true;
    begin
        for i in L'range loop
            if (L(i) < R(i)) then
                resault := (resault and true);
            else
                resault := (resault and false);
            end if;
        end loop;
        return resault;
    end function;

    function "AND" (L, R : byte) return byte is
        variable resault : byte;
    begin
        for i in L'range loop
            resault(i) := L(i) and R(i);
        end loop;
        return resault;
    end function;

    function "AND" (L, R : byte32) return byte32 is
        variable resault : byte32;
    begin
        for i in L'range loop
            resault(i) := L(i) and R(i);
        end loop;
        return resault;
    end function;

    function "NOT" (a : byte) return byte is
        variable resault : byte;
    begin
        for i in a'range loop
            if (a(i) = '1') then
                resault(i) := '0';
            else
                resault(i) := '1';
            end if;
        end loop;
        return resault;
    end function;

    function "+" (L : byte32; Rin : byte) return byte32 is -- adding of 8bit with 32 bit
        variable resault : byte32 := (others => '0');
        constant R : byte32 := "000000000000000000000000" & Rin(7) & Rin(6) & Rin(5) & Rin(4) & Rin(3) & Rin(2) & Rin(1) & Rin(0);
    begin

        for i in 0 to L'length - 1 loop
            if (L(i) = '1' and R(i) = '1' and resault(i) = '0') then
                resault(i + 1) := '1';
            elsif (L(i) = '1' and R(i) = '1' and resault(i) = '1') then
                resault(i + 1) := '1';
                resault(i) := '1';
            else
                resault(i) := L(i) or R(i);
            end if;
        end loop;
        return resault;
    end function;

    function "+" (L, R : byte32) return byte32 is -- 32bit adding
        variable resault : byte32 := (others => '0');
    begin
        for i in 0 to L'length - 1 loop
            if (L(i) = '1' and R(i) = '1' and resault(i) = '0') then
                resault(i + 1) := '1';
            elsif (L(i) = '1' and R(i) = '1' and resault(i) = '1') then
                resault(i + 1) := '1';
                resault(i) := '1';
            else
                resault(i) := L(i) or R(i);
            end if;
        end loop;
        return resault;
    end function;

    function "+" (L, R : byte) return byte is -- 32bit adding
        variable resault : byte := (others => '0');
    begin
        for i in 0 to L'length - 1 loop
            if (L(i) = '1' and R(i) = '1' and resault(i) = '0' and i <= 6) then
                resault(i + 1) := '1';
                resault(i) := '0';
            elsif (L(i) = '1' and R(i) = '1' and resault(i) = '1' and i <= 6) then
                resault(i + 1) := '1';
                resault(i) := '1';
            else
                resault(i) := L(i) or R(i);
            end if;
        end loop;
        return resault;
    end function;

    function "-" (L : byte; R : byte32) return byte32 is
        variable resault : byte32;
        variable Left : byte32 := to_32(L);
    begin
        for i in 0 to Left'length - 1 loop

            if (Left(i) = '1' and R(i) = '0' and resault(i) = '0') then
                resault(i) := '1';
            elsif (Left(i) = '0' and R(i) = '1' and resault(i) = '1' and i <= 30) then
                resault(i) := '1';
                resault(i + 1) := '0';
            else
                resault(i) := '0';
            end if;

        end loop;
        return resault;
    end function;

    function "-" (L : byte; R : byte) return byte is
        variable resault : byte;
    begin
        for i in 0 to L'length - 1 loop

            if (L(i) = '1' and R(i) = '0' and resault(i) = '0') then
                resault(i) := '1';
            elsif (L(i) = '0' and R(i) = '1' and resault(i) = '1' and i <= L'length - 1) then
                resault(i) := '1';
                resault(i + 1) := '0';
            else
                resault(i) := '0';
            end if;

        end loop;
        return resault;
    end function;

    function "&"(a : in std_logic_vector(3 downto 0); b : in std_logic_vector(3 downto 0)) return byte is
        variable byte_out : std_logic_vector(7 downto 0);
    begin
        byte_out(7 downto 4) := a;
        byte_out(3 downto 0) := b;
    
        return byte(byte_out);
    end function;

    function to_32 (a : byte) return byte32 is
    begin
        return ("000000000000000000000000" & a(7) & a(6) & a(5) & a(4) & a(3) & a(2) & a(1) & a(0));
    end function;

    function rgb_to_32 (a : rgb) return rgb32 is
    begin
        return (("000000000000000000000000" & a(2)(7) & a(2)(6) & a(2)(5) & a(2)(4) & a(2)(3) & a(2)(2) & a(2)(1) & a(2)(0)),
        ("000000000000000000000000" & a(1)(7) & a(1)(6) & a(1)(5) & a(1)(4) & a(1)(3) & a(1)(2) & a(1)(1) & a(1)(0)),
        ("000000000000000000000000" & a(0)(7) & a(0)(6) & a(0)(5) & a(0)(4) & a(0)(3) & a(0)(2) & a(0)(1) & a(0)(0)));
    end function;

    function pixel_array_to_32 (b : pixel_array(natural range <>, natural range <>)) return pixel_array32 is
        variable tmp : pixel_array32(b'range(1), b'range(2));
    begin
        for i in b'range(2) loop
            for j in b'range(1) loop
                tmp(j, i) := rgb_to_32(b(j, i));
            end loop;
        end loop;
        return tmp;
    end function;

    function image_selector (a : integer) return string is
        constant pack : string_array(1 to 2) := (("/data_set/image001.txt"), ("/data_set/image002.txt"));
    begin
        return pack(a);
    end function;

    function string_to_nibble(input_char : character) return std_logic_vector is
        variable hex_representation : std_logic_vector(3 downto 0);
    begin
        case input_char is
            when '0' =>
                hex_representation := "0000";
            when '1' =>
                hex_representation := "0001";
            when '2' =>
                hex_representation := "0010";
            when '3' =>
                hex_representation := "0011";
            when '4' =>
                hex_representation := "0100";
            when '5' =>
                hex_representation := "0101";
            when '6' =>
                hex_representation := "0110";
            when '7' =>
                hex_representation := "0111";
            when '8' =>
                hex_representation := "1000";
            when '9' =>
                hex_representation := "1001";
            when 'A' | 'a' =>
                hex_representation := "1010";
            when 'B' | 'b' =>
                hex_representation := "1011";
            when 'C' | 'c' =>
                hex_representation := "1100";
            when 'D' | 'd' =>
                hex_representation := "1101";
            when 'E' | 'e' =>
                hex_representation := "1110";
            when 'F' | 'f' =>
                hex_representation := "1111";
            when others =>
                hex_representation := "0000"; -- Default to 0 for characters not in the hexadecimal range
        end case;

        return hex_representation;
    end string_to_nibble;

    procedure array_all_zero(signal arr : in bit_array; signal outp : out std_logic) is
        constant a : integer := arr'length(1);
        constant b : integer := arr'length(2);
        variable acc : std_logic := '0';
    begin

        for j in 1 to b loop
            for i in 1 to a loop
                    acc := acc OR arr(i,j);
            end loop;
        end loop;
        outp <= acc;
end procedure;

procedure index_finder(signal a : in bit_array(natural range <>, natural range <>); signal outp : out out_state) is
    variable x_index, y_index : integer;
begin
    for i in a'range(2) loop
        for j in a'range(1) loop
            if (a(j, i) = '1') then
                x_index := j;
                y_index := i;
            end if;
        end loop;
    end loop;
end procedure;

procedure greatest_node(signal a : in pixel_array32(natural range <>, natural range <>); signal b : out out_state) is
    variable max_Red, max_Green, max_blue, max_Mono : byte32 := (others => '0');
    variable index_Red, index_Green, index_blue, index_Mono : integer := 0;
begin
    for j in a'range(2) loop -- shape range(3)
        for i in a'range(1) loop -- color range (4)
            if ((a(i, j)(2)) > max_Red) then
                max_Red := a(i, j)(2);
                index_Red := j;
            end if;
            if (a(i, j)(1) > max_Green) then
                max_Green := a(i, j)(1);
                index_Green := j;
            end if;
            if (a(i, j)(0) > max_blue) then
                max_blue := a(i, j)(0);
                index_blue := j;
            end if;
        end loop;
    end loop;

    if ((max_Red = max_Green and max_Green = max_blue and max_Red = max_blue) and -- Mono
        (index_Red = index_Green and index_Green = index_blue and index_Red = index_blue)) then
        index_Mono := index_Red;
        case index_Mono is
            when 3 =>
                b <= Mono_Triangle;
            when 2 =>
                b <= Mono_Square;
            when 1 =>
                b <= Mono_Circle;
            when others =>
        end case;
    elsif (max_Red >= max_Green and max_Red >= max_blue) then -- Red
        case index_Red is
            when 3 =>
                b <= Red_Triangle;
            when 2 =>
                b <= Red_Square;
            when 1 =>
                b <= Red_Circle;
            when others =>
        end case;
    elsif (max_Green >= max_blue and max_Green >= max_Red) then -- Green
        case index_Green is
            when 3 =>
                b <= Green_Triangle;
            when 2 =>
                b <= Green_Square;
            when 1 =>
                b <= Green_Circle;
            when others =>
        end case;
    elsif (max_blue >= max_Red and max_blue >= max_Green) then -- blue
        case index_Mono is
            when 3 =>
                b <= Blue_Triangle;
            when 2 =>
                b <= Blue_Square;
            when 1 =>
                b <= Blue_Circle;
            when others =>
        end case;
    else
        b <= none;
    end if;
end procedure;

procedure dicing(signal L : in pixel_array(natural range <>, natural range <>); signal R : out pix_array_pack1(natural range <>, natural range <>)) is
    constant inx : integer := L'length(1); -- input image size
    constant iny : integer := L'length(2);
    constant outx : integer := R'length(1); -- diced output image size
    constant outy : integer := R'length(2);
    constant xc : integer := outx / inx;-- horizontal number of output images 
    constant yc : integer := outy / iny;-- vertical number of output images 
begin
    for y in 1 to yc loop -- Yth vertical output images
        for x in 1 to xc loop -- Xth horizontal output images
            for b in 1 to outy loop -- b th vertical pixel of diced image
                for a in 1 to outx loop -- a th vertical pixel of diced image
                    R(x, y)(a, b) <= L((a + ((x - 1) * outx)), ((b + (y - 1)) * outy));
                end loop;
            end loop;
        end loop;
    end loop;
end procedure;

end package body custom_package;