library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;

library work;
use work.custom_package.all;

entity neural_network is
  port (
    class_x : in integer;
    class_y : in integer;
    sample_index : in integer;
    out_class : out out_state := none;
    in_image : in pixel_array(1 to image_size_x, 1 to image_size_y);
    done_out : out std_logic;
    en_in : in std_logic;
    clk : in std_logic
  );
end neural_network;

architecture Behavioral of neural_network is

  -------- buffer
  signal in_node : pixel_array(1 to image_size_x, 1 to image_size_y);
  -------- sample_reciever data
  signal sample_image : pixel_array(1 to image_size_x, 1 to image_size_y) := (others => (others => (others => (others => '0')))); -- input data of sample
  signal sample_class : pixel_array(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => 'Z'))));-- selected class of sample data
  signal sample_en, sample_done : std_logic;

  -------- training signals
  signal training1, training_done1, neuron_busy1 : bit_array(1 to layer2_size_x, 1 to layer2_size_y);
  signal training2, training_done2, neuron_busy2 : std_logic;
  signal neuron_selector1 : weight_array(1 to layer1_size_x, 1 to layer1_size_y);
  signal neuron_selector2 : weight_array(1 to layer2_size_x, 1 to layer2_size_y);
  signal done_array1 : done_array(1 to layer2_size_x, 1 to layer2_size_y);
  signal done_vector2 : std_logic_vector(5 downto 0);

  -------- layer 1 data
  signal image_diced : pix_array_pack1(1 to layer1_size_x, 1 to layer1_size_y) := (others => (others => (others => (others => (others => (others => '0'))))));
  signal out_node_array1 : pix_array_pack1_32(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => (others => (others => '0'))))));
  signal weights_array1, new_weights_array1 : weight_pack_all(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => (others => (others => (others => '0')))))));
  signal biases_array1, new_biases_array1 : pix_array_pack1(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => (others => (others => '0'))))));
  signal busy_pack1 : bit_pack(1 to layer2_size_x, 1 to layer2_size_y);

  -------- layer 2 data
  signal out_nodes2 : pixel_array32(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => '0'))));
  signal weights2, new_weights2 : weight_array_pack1(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => (others => '0')))));
  signal biases2, new_biases2 : pixel_array(1 to layer2_size_x, 1 to layer2_size_y) := (others => (others => (others => (others => '0'))));
  signal busy_array2 : bit_array(1 to layer2_size_x, 1 to layer2_size_y);

begin

  sample_class(1, 3)(2) <= X"ED"; -- initialise class value of red triangle class
  sample_class(1, 3)(1) <= X"1C";
  sample_class(1, 3)(0) <= X"24";
  sample_class(2, 2)(2) <= X"4B"; -- initialise value of green square class
  sample_class(2, 2)(1) <= X"FF";
  sample_class(2, 2)(0) <= X"4F";

  main_training : process
  begin

    wait until en_in = '1';
    sample_en <= '1'; -- enableing training1 module waits for sample to be received
    wait until sample_done = '1';
    sample_en <= '0'; -- disable to avoid repetition

    training1(class_x, class_y) <= '1'; -- enableing training1 module 
    wait until training_done1(class_x, class_y) = '1';
    training1(class_x, class_y) <= '0'; -- disable to avoid repetition

    training2 <= '1'; -- enableing training2 module 
    wait until training_done2 = '1';
    training2 <= '0';

    done_out <= '1';

  end process;

  dice_image : process (in_node) -- cut down input image into array of smaller images
  begin
    dicing(in_node, image_diced);
  end process;

  output_selector : process (out_nodes2, training1, training2)
    variable enable : std_logic;
  begin
    for b in 1 to layer2_size_y loop -- for each Y output node
      for a in 1 to layer2_size_x loop
        enable := enable or training1(a, b);
      end loop;
    end loop;

    if ((enable = '1') or (training2 = '1')) then
      out_class <= training;
      in_node <= sample_image;
    else
      in_node <= in_image;
      greatest_node(out_nodes2, out_class); -- sets the appropriate state depending on 
    end if;
  end process;

  done_generate : process (done_array1, done_vector2)
  begin
    for b in 1 to layer2_size_y loop -- for each Y output node
      for a in 1 to layer2_size_x loop
        if (done_array1(a, b) = "111111") then
          training_done1(a, b) <= '1';
        else
          training_done1(a, b) <= '0';
        end if;
      end loop;
    end loop;

    if (done_vector2 = "111111") then
      training_done2 <= '1';
    else
      training_done2 <= '0';
    end if;
  end process;

  ------------------------------------------------------ instantiations ------------------------------------------------------

  layer1_class_y : for b in 1 to layer2_size_y generate -- for each Y output node
    layer1_class_x : for a in 1 to layer2_size_x generate -- for each X output node

      layer1_y : for d in 1 to layer1_count_y generate -- 100/layer1_size_y
        layer1_x : for c in 1 to layer1_count_x generate -- 1 to 100/layer1_size_x

          neuronL1 : entity work.neuron(behavioral)
            generic map(
              in_size_x => layer1_size_x, -- equals 10
              in_size_y => layer1_size_y -- equals 10
            )
            port map(
              in_node => image_diced(c, d),
              out_node => out_node_array1(a, b)(c, d),
              weight => weights_array1(a, b)(c, d),
              new_weight => new_weights_array1(a, b)(c, d),
              bias => biases_array1(a, b)(c, d),
              new_bias => new_biases_array1(a, b)(c, d),
              busy => busy_pack1(a, b)(c, d),
              clk => clk
            );
        end generate;
      end generate;

    end generate;
  end generate;

  layer2_y : for b in 1 to layer2_size_y generate -- for each Y output node
    layer2_x : for a in 1 to layer2_size_x generate -- for each X output node
      neuronL2 : entity work.neuron_l2(behavioral)
        generic map(
          in_size_x => layer1_size_x, -- equals 10
          in_size_y => layer1_size_y -- equals 10
        )
        port map(
          in_node => out_node_array1(a, b),
          out_node => out_nodes2(a, b),
          weight => weights2(a, b),
          new_weight => new_weights2(a, b),
          bias => biases2(a, b),
          new_bias => new_biases2(a, b),
          busy => busy_array2(a, b),
          clk => clk
        );
    end generate;
  end generate;

  training1_y : for b in 1 to layer2_size_y generate -- for each Y output node
    training1_x : for a in 1 to layer2_size_x generate -- for each X output node
      rgb : for i in 2 downto 0 generate -- Layer1 training (three instances for each color RGB)

        Layer1_training : entity work.training(behavioral)
          generic map(
            in_size_x => layer1_size_x,
            in_size_y => layer1_size_y,
            color_selector => i
          )
          port map(
            in_sample => sample_image,
            sample_class_in => sample_class,
            sample_select_x => class_x,
            sample_select_y => class_y,
            out_nodes => out_nodes2,
            weights => weights_array1(a, b),
            new_weights => new_weights_array1(a, b),
            biases => biases_array1(a, b),
            new_biases => new_biases_array1(a, b),
            neuron_busy => busy_pack1(a, b),
            en_in => training1(a, b),
            sample_done => sample_done,
            done_out1 => done_array1(a, b)((i * 2) + 1),
            done_out2 => done_array1(a, b)((i * 2)),
            done_all => training_done1(a, b),
            clk => clk
          );
      end generate;
    end generate;
  end generate;

  training2_rgb : for i in 2 downto 0 generate -- Layer2 training (three instances for each color RGB)
    Layer2_training : entity work.training(behavioral)
      generic map(
        in_size_x => layer2_size_x,
        in_size_y => layer2_size_y,
        color_selector => i
      )
      port map(
        in_sample => sample_image,
        sample_class_in => sample_class,
        sample_select_x => class_x,
        sample_select_y => class_y,
        out_nodes => out_nodes2,
        weights => weights2,
        new_weights => new_weights2,
        biases => biases2,
        new_biases => new_biases2,
        neuron_busy => busy_array2,
        en_in => training2,
        sample_done => '1', , -- always high since sample is already complete after layer 1
        done_out1 => done_vector2((i * 2) + 1),
        done_out2 => done_vector2((i * 2)),
        done_all => training_done2,
        clk => clk
      );
  end generate;

  dataset_receiver : entity work.read_image (behavioral)
    generic map(
      width => image_size_x, -- Width of the image
      height => image_size_y, -- Height of the image
      folder_path => data_set_path
    )
    port map(
      index => sample_index,
      out_data => sample_image,
      en => sample_en,
      done => sample_done,
      clk => clk
    );

end Behavioral;