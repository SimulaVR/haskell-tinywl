{ pkgs ? import <nixpkgs> {}, ghc ? pkgs.ghc }:

pkgs.haskell.lib.buildStackProject {
  name = "haskell-tinywl";
  inherit ghc;
  buildInputs = with pkgs; [ 
                             libinput
                             (callPackage ./nix/wayland.nix { } )
                             (callPackage ./nix/wayland-protocols.nix { } )
                             (callPackage ./nix/wlroots.nix { } )
                             libGL
                             xorg.pixman
                             libxkbcommon
                             zlib
                             git
                             xorg.libX11
                             udev
                             cabal-install
                             pkgconfig
                             pixman
                             (callPackage ./nix/libdrm.nix { } )
                             # Failed attempts to fix EGL driver issues
                             # See: https://www.reddit.com/r/NixOS/comments/apnzla/using_egl_drivers_in_haskellstacknonnixos_project/
                             # cairo
                             # mesa_glu
                          ];

  LANG = "en_US.UTF-8";
  TMPDIR = "/tmp";
  XDG_RUNTIME_DIR = builtins.getEnv "XDG_RUNTIME_DIR";
}