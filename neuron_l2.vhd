library work;
use work.custom_package.all;

library IEEE;
use IEEE.STD_LOGIC_1164.all;
entity neuron_l2 is
    generic (
        in_size_x : integer;
        in_size_y : integer
    );
    port (
        out_node : out rgb32 := (others => (others => '0'));
        in_node : in pixel_array32(1 to in_size_x, 1 to in_size_y) := (others => (others => (others => (others => '0'))));
        weight : out weight_array(1 to in_size_x, 1 to in_size_y) := (others => (others => (others => '0'))); -- for training module
        new_weight : in weight_array(1 to in_size_x, 1 to in_size_y) := (others => (others => (others => '0'))); -- for training module
        bias : out rgb := (others => (others => '0')); -- for training module
        new_bias : in rgb := (others => (others => '0')); -- for training module
        busy : out std_logic ;
        clk : in std_logic 
    );
end neuron_l2;

architecture behavioral of neuron_l2 is

    signal weight_reg : weight_array(1 to in_size_x, 1 to in_size_y) := (others => (others => (others => '0')));
    signal bias_reg : rgb := (others => (others => '0'));
    signal busy_weight, busy_bias, busy_reg : std_logic;

begin

    weight <= weight_reg;
    busy <= busy_reg;
    bias <= bias_reg;

    main : process
        variable red, green, blue : byte32 := (others => '0');
    begin
        for j in 1 to in_size_y loop
            for i in 1 to in_size_x loop
            
                wait until busy_reg = '0';
                if (weight_reg(i, j)(2) = '1') then -- for RED pixel
                    red := red + in_node(i, j)(2); -- if weight_reg is '1' then node is added to summation
                end if;
                if (weight_reg(i, j)(1) = '1') then -- for GREEN pixel
                    green := red + in_node(i, j)(1); -- if weight_reg is '1' then node is added to summation
                end if;
                if (weight_reg(i, j)(0) = '1') then -- for BLUE pixel
                    red := red + in_node(i, j)(0); -- if weight_reg is '1' then node is added to summation
                end if;
            end loop;
        end loop;
 
        wait until busy_bias = '0';
        red := red + bias_reg(2);
        green := green + bias_reg(1);
        blue := blue + bias_reg(0);
        
        wait for 0ns;
        out_node(2) <= red;
        out_node(1) <= green;
        out_node(0) <= blue;
    end process;

    busy_set : process (new_weight, new_bias)
    begin
        if (not(new_weight = weight_reg)) then
            busy_weight <= '1';
        else
            busy_weight <= '0';
        end if;

        if (new_bias /= bias_reg) then
            busy_bias <= '1';
        else
            busy_bias <= '0';
        end if;
    end process;

    apply_data : process (clk)
    begin
        if (busy_weight = '1' ) then -- apply on rising edge
            weight_reg <= new_weight;
            busy_weight <= '0';
        end if;
        if (busy_bias = '1' ) then -- apply on rising edge
            bias_reg <= new_bias;
            busy_bias <= '0';
        end if;
    end process;

end behavioral;