defmodule Dec21Test do
  use ExUnit.Case
  doctest Dec21
  doctest Dec21.InfiniteMap

  @tag timeout: :infinity
  test "Dec 21" do
    input = File.read!("../../inputs/2023/21.txt")

    assert Dec21.a(input) == 3637
    assert Dec21.b(input) == 601_113_643_448_699
  end
end
