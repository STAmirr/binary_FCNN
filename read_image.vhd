library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.custom_package.all;
use work.image_data.all;

library work;
use work.custom_package.all;

entity read_image is
    generic (
        width : integer := 100; -- Width of the image
        height : integer := 100; -- Height of the image
        folder_path : string(positive range <>) := "/data_set"
    );
    port (
        index : integer := 0;
        out_data : out pixel_array(1 to height, 1 to width) ; -- Output pixel array data
        en : in std_logic ;
        done : out std_logic ;
        clk : in std_logic 
    );
end read_image;

architecture Behavioral of read_image is

    signal pixel_data : pixel_array(1 to height, 1 to width) ;

begin

    main : process
        variable char_pos : integer := 0;
        variable temp_r, temp_g, temp_b : byte;
        constant input_size : integer := 7;

    begin
        wait until en = '1';
        done <= '0';

        for b in 1 to height loop -- Y axis image loop
            for a in 1 to width loop -- X axis image loop

                char_pos := ((a + ((b - 1) * width)) * input_size) - input_size + 1;
                pixel_data(a, b)(2) <= string_to_nibble(images(index)(char_pos)) & string_to_nibble(images(index)(char_pos + 1));
                pixel_data(a, b)(1) <= string_to_nibble(images(index)(char_pos + 2)) & string_to_nibble(images(index)(char_pos + 3));
                pixel_data(a, b)(0) <= string_to_nibble(images(index)(char_pos + 4)) & string_to_nibble(images(index)(char_pos + 5));
            end loop;
        end loop;        

        char_pos := 0;
        wait for 0ns;
        out_data <= pixel_data;
        done <= '1';
        wait for 0ns;
    end process;
   

end Behavioral;