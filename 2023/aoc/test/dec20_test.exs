defmodule Dec20Test do
  use ExUnit.Case
  doctest Dec20
  doctest Dec20.Modules
  doctest Dec20.Parse
  doctest Dec20.Run

  test "Dec 20" do
    input = File.read!("../../inputs/2023/20.txt")

    assert Dec20.a(input) == 737_679_780
  end
end
