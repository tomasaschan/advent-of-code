defmodule Dec13Test do
  use ExUnit.Case
  doctest Dec13

  test "Dec 13" do
    input = File.read!("../../inputs/2023/13.txt")

    assert Dec13.a(input) == 40006
  end
end
