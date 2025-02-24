library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.custom_package.all;

entity training is
    generic (
        in_size_x : integer;
        in_size_y : integer;
        color_selector : integer
    );
    port (
        in_sample : in pixel_array(1 to image_size_x, 1 to image_size_y); -- input data of sample
        sample_class_in : in pixel_array(1 to layer2_size_x, 1 to layer2_size_y);-- selected class of sample data
        sample_select_x : in integer;
        sample_select_y : in integer;
        sample_done : in std_logic;
        neuron_busy : in bit_array(1 to in_size_x, 1 to in_size_y);
        out_nodes : in pixel_array32(1 to layer2_size_x, 1 to layer2_size_y); -- output of final layer neuron (COMPARED WITH SAMPLE DATA)
        weights : in weight_array_pack1(1 to in_size_x, 1 to in_size_y); -- bias of neuron 
        new_weights : out weight_array_pack1(1 to in_size_x, 1 to in_size_y); -- new bias to be sent to neuron
        biases : in pixel_array(1 to in_size_x, 1 to in_size_y); -- bias of neuron module
        new_biases : out pixel_array(1 to in_size_x, 1 to in_size_y); -- new bias to be sent to neuron module
        done_out1 : out std_logic;
        done_out2 : out std_logic;
        done_all : in std_logic;
        en_in : in std_logic;
        clk : in std_logic
    );
end training;

architecture Behavioral of training is
begin

    red_main : process
        variable state, state_bias : training_state := start;
        variable out_nodes_prev : pixel_array32(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => '0'))));
        variable idle_count : integer := 0;
        constant high_amp : byte := (others => 'Z');
    begin

        wait until en_in = '1';
        done_out1 <= '0';
        done_out2 <= '0';
        neuron_y : for b in 1 to in_size_y loop -- loops through different sliced images pixels of sliced image
            neuron_x : for a in 1 to in_size_x loop

                node_y : for d in 1 to layer1_size_y loop -- loops through pixels of sliced image
                    node_x : for c in 1 to layer1_size_x loop

                        optimizer : loop
                            case state is
                                when start =>
                                    state := inc;

                                when inc =>
                                    out_nodes_prev := out_nodes;
                                    new_weights(a, b)(c, d)(color_selector) <= '1';
                                    wait until (neuron_busy(a, b) = '0');
                                    state := comp;

                                when comp =>
                                    if ((sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes(a, b)(color_selector)) > (sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes_prev(a, b)(color_selector))) then -- bad change, set zero.
                                        new_weights(a, b)(c, d)(color_selector) <= '0';
                                        state := done;
                                    elsif ((sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes(a, b)(color_selector)) = (sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes_prev(a, b)(color_selector))) then -- bad change, set zero.
                                        new_weights(a, b)(c, d)(color_selector) <= '0';
                                        state := done;
                                    elsif ((sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes(a,b)(color_selector)) < (sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes_prev(a, b)(color_selector))) then -- bad change, set zero.
                                        new_weights(a, b)(c, d)(color_selector) <= '1';
                                        state := done;
                                    end if;

                                when done =>
                                    state := start;
                                    exit optimizer;

                                when others =>
                            end case;
                        end loop;

                    end loop;
                end loop;
                done_out1 <= '1';

                bias_optimizer : loop
                    case state_bias is
                        when start =>
                            if (sample_class_in(sample_select_x, sample_select_y)(color_selector) = high_amp) then
                                exit bias_optimizer;
                            else
                                state_bias := inc;
                            end if;

                        when inc =>
                            out_nodes_prev := out_nodes;
                            new_biases(a, b)(color_selector) <= byte(biases(a, b)(color_selector)) + "00000001";
                            wait for clk_period;
                            wait until (neuron_busy(a, b) = '0');
                            state_bias := comp;

                        when comp =>
                            if ((sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes(a, b)(color_selector)) > (sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes_prev(a, b)(color_selector))) then -- bad change, dec
                                new_biases(a, b)(color_selector) <= biases(a, b)(color_selector) - "00000001";
                                state_bias := done;
                            elsif ((sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes(a, b)(color_selector)) = (sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes_prev(a, b)(color_selector))) then -- no change, 
                                idle_count := idle_count + 1;
                                if (idle_count >= 5) then
                                    state_bias := done;
                                end if;
                            elsif ((sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes(a, b)(color_selector)) < (sample_class_in(sample_select_x, sample_select_y)(color_selector) - out_nodes_prev(a, b)(color_selector))) then -- positive change, keep inc.
                                state_bias := inc;
                            end if;

                        when done =>
                            idle_count := 0;
                            state_bias := start;
                            exit bias_optimizer;
                    end case;
                end loop;
                done_out2 <= '1';

            end loop;
        end loop;

        wait until done_all = '1';

    end process;

end Behavioral;