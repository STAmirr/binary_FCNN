library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.custom_package.all;

entity training_l2 is
    generic (
        in_size_x : integer;
        in_size_y : integer
    );
    port (
        in_sample : in pixel_array(1 to image_size_x, 1 to image_size_y); -- input data of sample
        sample_class_in : in pixel_array(1 to layer2_size_x, 1 to layer2_size_y);-- selected class of sample data
        out_nodes : in pixel_array32(1 to layer2_size_x, 1 to layer2_size_y); -- output of final layer neuron (COMPARED WITH SAMPLE DATA)
        weights : in weight_array_pack1(1 to in_size_x, 1 to in_size_y); -- bias of neuron 
        new_weights : out weight_array_pack1(1 to in_size_x, 1 to in_size_y); -- new bias to be sent to neuron
        biases : in pixel_array(1 to in_size_x, 1 to in_size_y); -- bias of neuron module
        new_biases : out pixel_array(1 to in_size_x, 1 to in_size_y); -- new bias to be sent to neuron module
        neuron_selector_out : out weight_array(1 to in_size_x, 1 to in_size_y);
        neuron_busy : in std_logic := '0';
        en_in : in std_logic := '0';
        sample_done : in std_logic := '0';
        done_out : out std_logic := '0';
        clk : in std_logic := '0'
    );
end training_l2;

architecture Behavioral of training_l2 is

    signal ns_reg : weight_array(1 to in_size_x, 1 to in_size_y);
    signal done_r, done_g, done_b : std_logic := '0';

begin
    neuron_selector_out <= ns_reg;

    red_main : process
        variable state_red, state_bias : training_state := start;
        variable out_nodes_prev : pixel_array32(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => '0'))));
        variable idle_count : integer := 0;
        constant high_amp: byte := (others => 'Z');

    begin
        wait until en_in = '1';
        neuron_y : for l in 1 to layer2_size_y loop -- loops through different neurons
            neuron_x : for k in 1 to layer2_size_x loop

                if (sample_class_in(k, l)(2) = high_amp) then
                    if (k = layer2_size_x) then
                        exit neuron_x;
                    end if;
                    next;
                end if;

                wait until sample_done = '1'; -- enable before sending sample
                done_r <= '0';
                ns_reg(k, l)(2) <= '1'; --selects appropriate neuron

                weight_optimizer : for j in 1 to in_size_y loop -- loops through different input nodes of a single neuron
                    for i in 1 to in_size_x loop

                        RED_optimizer : loop
                            case state_red is
                                when start =>
                                    if (sample_done = '1') then
                                        state_red := inc;
                                    else
                                        state_red := start;
                                    end if;

                                when inc =>
                                    out_nodes_prev := out_nodes;
                                    new_weights(k, l)(i, j)(2) <= '1';
                                    wait until (neuron_busy = '0');
                                    state_red := comp;

                                when comp =>
                                    if ((sample_class_in(k, l)(2) - out_nodes(k, l)(2)) > (sample_class_in(k, l)(2) - out_nodes_prev(k, l)(2))) then -- bad change, set zero.
                                        new_weights(k, l)(i, j)(2) <= '0';
                                        state_red := done;
                                    elsif ((sample_class_in(k, l)(2) - out_nodes(k, l)(2)) = (sample_class_in(k, l)(2) - out_nodes_prev(k, l)(2))) then -- no change, set zero.
                                        new_weights(k, l)(i, j)(2) <= '0';

                                        state_red := done;
                                    elsif ((sample_class_in(k, l)(2) - out_nodes(k, l)(2)) < (sample_class_in(k, l)(2) - out_nodes_prev(k, l)(2))) then -- positive change, set one.
                                        new_weights(k, l)(i, j)(2) <= '1';
                                        state_red := done;
                                    end if;

                                when done =>
                                    state_red := start;
                                    exit RED_optimizer;
                            end case;
                        end loop;

                    end loop;
                end loop;

                bias_optimizer : loop
                    case state_bias is
                        when start =>
                            if (sample_done = '1') then
                                state_bias := inc;
                            else
                                state_bias := start;
                            end if;
                        when inc =>
                            out_nodes_prev := out_nodes;
                            new_biases(k, l)(2) <= biases(k, l)(2) + "00000001";
                            wait until (neuron_busy = '0');
                            state_bias := comp;

                        when comp =>
                            if ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) > (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- bad change, dec
                                new_biases(k, l)(2) <= biases(k, l)(2) - "00000001";
                                state_bias := done;
                            elsif ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) = (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- no change, 
                                idle_count := idle_count + 1;
                                if (idle_count >= 5) then
                                    state_bias := done;
                                end if;
                            elsif ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) < (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- positive change, keep inc.
                                state_bias := inc;
                            end if;

                        when done =>
                            idle_count := 0;
                            state_bias := start;
                            exit bias_optimizer;
                        when others =>
                    end case;
                end loop;
                ns_reg(k, l)(2) <= '0';
                done_r <= '1';

            end loop;
        end loop;
    end process;

    green_main : process
        variable state_green, state_bias : training_state := start;
        variable out_nodes_prev : pixel_array32(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => '0'))));
        variable idle_count : integer := 0;
        constant high_amp: byte := (others => 'Z');

    begin
        wait until en_in = '1';
        neuron_y : for l in 1 to layer2_size_y loop -- loops through different neurons
            neuron_x : for k in 1 to layer2_size_x loop

                if (sample_class_in(k, l)(1) = high_amp) then
                    if (k = layer2_size_x) then
                        exit neuron_x;
                    end if;
                    next;
                end if;

                wait until sample_done = '1';
                done_g <= '0';
                ns_reg(k, l)(1) <= '1'; --selects appropriate neuron

                weight_optimizer : for j in 1 to in_size_y loop -- loops through different input nodes of a single neuron
                    for i in 1 to in_size_x loop

                        GREEN_optimizer : loop
                            case state_green is
                                when start =>
                                    if (sample_done = '1') then
                                        state_green := inc;
                                    else
                                        state_green := start;
                                    end if;

                                when inc =>
                                    out_nodes_prev := out_nodes;
                                    new_weights(k, l)(i, j)(1) <= '1';
                                    wait until (neuron_busy = '0');
                                    state_green := comp;

                                when comp =>
                                    if ((sample_class_in(k, l)(1) - out_nodes(k, l)(1)) > (sample_class_in(k, l)(1) - out_nodes_prev(k, l)(1))) then -- bad change, set zero.
                                        new_weights(k, l)(i, j)(1) <= '0';
                                        state_green := done;
                                    elsif ((sample_class_in(k, l)(1) - out_nodes(k, l)(1)) = (sample_class_in(k, l)(1) - out_nodes_prev(k, l)(1))) then -- no change, set zero.
                                        new_weights(k, l)(i, j)(1) <= '0';
                                        state_green := done;
                                    elsif ((sample_class_in(k, l)(1) - out_nodes(k, l)(1)) < (sample_class_in(k, l)(1) - out_nodes_prev(k, l)(1))) then -- positive change, set one.
                                        new_weights(k, l)(i, j)(1) <= '1';
                                        state_green := done;
                                    end if;

                                when done =>
                                    state_green := start;
                                    exit GREEN_optimizer;
                            end case;
                        end loop;

                    end loop;
                end loop;

                bias_optimizer : loop
                    case state_bias is
                        when start =>
                            if (sample_done = '1') then
                                state_bias := inc;
                            else
                                state_bias := start;
                            end if;
                        when inc =>
                            out_nodes_prev := out_nodes;
                            new_biases(k, l)(1) <= biases(k, l)(1) + "00000001";
                            wait until (neuron_busy = '0');
                            state_bias := comp;

                        when comp =>
                            if ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) > (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- bad change, dec
                                new_biases(k, l)(1) <= biases(k, l)(1) - "00000001";
                                state_bias := done;
                            elsif ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) = (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- no change, 
                                idle_count := idle_count + 1;
                                if (idle_count >= 5) then
                                    state_bias := done;
                                end if;
                            elsif ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) < (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- positive change, keep inc.
                                state_bias := inc;
                            end if;

                        when done =>
                            idle_count := 0;
                            state_bias := start;
                            exit bias_optimizer;
                        when others =>
                    end case;
                end loop;

                ns_reg(k, l)(1) <= '0';
                done_g <= '1';

            end loop;
        end loop;
    end process;

    blue_main : process
        variable state_blue, state_bias : training_state := start;
        variable out_nodes_prev : pixel_array32(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => '0'))));
        variable idle_count : integer := 0;
        constant high_amp: byte := (others => 'Z');

    begin
        wait until en_in = '1';
        neuron_y : for l in 1 to layer2_size_y loop -- loops through different neurons
            neuron_x : for k in 1 to layer2_size_x loop

                if (sample_class_in(k, l)(0) = high_amp) then
                    if (k = layer2_size_x) then
                        exit neuron_x;
                    end if;
                    next;
                end if;

                wait until sample_done = '1';
                done_b <= '0';
                ns_reg(k, l)(0) <= '1'; --selects appropriate neuron

                weight_optimizer : for j in 1 to in_size_y loop -- loops through different input nodes of a single neuron
                    for i in 1 to in_size_x loop

                        BLUE_optimizer : loop
                            case state_blue is
                                when start =>
                                    if (sample_done = '1') then
                                        state_blue := inc;
                                    else
                                        state_blue := start;
                                    end if;

                                when inc =>
                                    out_nodes_prev := out_nodes;
                                    new_weights(k, l)(i, j)(0) <= '1';
                                    wait until (neuron_busy = '0');
                                    state_blue := comp;

                                when comp =>
                                    if ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) > (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- bad change, set zero.
                                        new_weights(k, l)(i, j)(0) <= '0';
                                        state_blue := done;
                                    elsif ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) = (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- no change, set zero.
                                        new_weights(k, l)(i, j)(0) <= '0';
                                        state_blue := done;
                                    elsif ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) < (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- positive change, set one.
                                        new_weights(k, l)(i, j)(0) <= '1';
                                        state_blue := done;
                                    end if;

                                when done =>
                                    state_blue := start;
                                    exit BLUE_optimizer;
                            end case;
                        end loop;

                    end loop;
                end loop;

                bias_optimizer : loop
                    case state_bias is
                        when start =>
                            if (sample_done = '1') then
                                state_bias := inc;
                            else
                                state_bias := start;
                            end if;
                        when inc =>
                            out_nodes_prev := out_nodes;
                            new_biases(k, l)(0) <= biases(k, l)(0) + "00000001";
                            wait until (neuron_busy = '0');
                            state_bias := comp;

                        when comp =>
                            if ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) > (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- bad change, dec
                                new_biases(k, l)(0) <= biases(k, l)(0) - "00000001";
                                state_bias := done;
                            elsif ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) = (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- no change, 
                                idle_count := idle_count + 1;
                                if (idle_count >= 5) then
                                    state_bias := done;
                                end if;
                            elsif ((sample_class_in(k, l)(0) - out_nodes(k, l)(0)) < (sample_class_in(k, l)(0) - out_nodes_prev(k, l)(0))) then -- positive change, keep inc.
                                state_bias := inc;
                            end if;

                        when done =>
                            idle_count := 0;
                            state_bias := start;
                            exit bias_optimizer;
                        when others =>
                    end case;
                end loop;

                ns_reg(k, l)(0) <= '0';
                done_b <= '1';

            end loop;
        end loop;
    end process;

    done_generate : process (done_r, done_g, done_b)
    begin
        if (done_r = '1' and done_g = '1' and done_b = '1') then
            done_out <= '1';
        else
            done_out <= '0';
        end if;
    end process;
end Behavioral;