defmodule Dec22Test do
  use ExUnit.Case
  doctest Dec22

  test "Dec 22" do
    input = File.read!("../../inputs/2023/22.txt")

    assert Dec22.a(input) == 492
    assert Dec22.b(input) == 86556
  end
end
