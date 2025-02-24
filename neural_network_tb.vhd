library IEEE;
use IEEE.STD_LOGIC_1164.all;
use IEEE.NUMERIC_STD.all;
library work;
use work.custom_package.all;

entity neural_network_tb is
  --  Port ( );
end neural_network_tb;

architecture Behavioral of neural_network_tb is

  signal sample_index, class_x, class_y : integer;
  signal out_class : out_state := none;
  signal in_image : pixel_array(1 to image_size_x, 1 to image_size_y);
  signal en, done, clk : std_logic;

  component NN
    port (
      class_x : in integer;
      class_y : in integer;
      sample_index : in integer;
      out_class : out out_state := none;
      in_image : in pixel_array(1 to image_size_x, 1 to image_size_y);
      en_in : in std_logic;
      done_out : out std_logic;
      clk : in std_logic
    );
  end component;
  for all : NN use entity work.neural_network(Behavioral);

begin

  uut : NN
  generic map(
    data_set_path => "/data_set"
  )
  port map(
    class_x => class_x,
    class_y => class_y,
    sample_index => sample_index,
    out_class => out_class,
    in_image => in_image,
    en_in => en,
    done_out => done,
    clk => clk
  );

  main : process
  begin
    wait for 100ns;

    class_x <= 1;
    class_3 <= 3;
    sample_index <= 1;
    en <= '1';
    wait until done = '1';
    en <= '0';

    wait for general_delay;

    class_x <= 2;
    class_3 <= 2;
    sample_index <= 2;
    en <= '1';
    wait until done = '1';
    en <= '0';
    wait;

  end process;

  clk_process : process
  begin
    clk <= '0';
    wait for clk_period/2;
    clk <= '1';
    wait for clk_period/2;
  end process;

end Behavioral;