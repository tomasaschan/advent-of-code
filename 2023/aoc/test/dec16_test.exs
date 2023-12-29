defmodule Dec16Test do
  use ExUnit.Case
  doctest Dec16

  import ExUnit.CaptureIO

  test "Dec 16" do
    input = File.read!("../../inputs/2023/16.txt")

    assert Dec16.a(input) == 7798
    assert Dec16.b(input) == 8026
  end

  test "Dec 16 parsing" do
    input = File.read!("../../inputs/2023/16.txt")

    parsed = Dec16.Mirrors.parse(input)

    assert capture_io(fn -> IO.puts(Dec16.Mirrors.show(parsed)) end) == input
  end
end
