{ lib
, gcc15
, cmake
, ninja
}:
gcc15.stdenv.mkDerivation {
  pname = "AoC 2025 C++";
  version = "0.1.0";

  src = ./.;

  nativeBuildInputs = [
    cmake
  ];
  buildInputs = [
    gcc15
  ];

  installPhase = ''
    mkdir -p $out/bin
    mv ../bin/* $out/bin
    cp compile_commands.json $out/
  '';

  CC = "${gcc15}/bin/gcc";
  CXX = "${gcc15}/bin/g++";
}