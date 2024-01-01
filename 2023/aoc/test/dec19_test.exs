defmodule Dec19Test do
  use ExUnit.Case
  doctest Dec19

  test "Dec 19" do
    input = File.read!("../../inputs/2023/19.txt")

    assert Dec19.a(input) == 401_674
    assert Dec19.b(input) == 134_906_204_068_564
  end
end
