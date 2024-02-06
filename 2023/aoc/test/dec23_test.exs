defmodule Dec23Test do
  use ExUnit.Case
  doctest Dec23

  @tag timeout: :infinity
  test "Dec 23" do
    input = File.read!("../../inputs/2023/23.txt")

    assert Dec23.a(input) == 2050
    assert Dec23.b(input) == 6262
  end
end
