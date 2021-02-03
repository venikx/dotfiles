{ options, config, lib, pkgs, ... }:

with lib;

let
  cfg = config.modules.hardware.nvidia;
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec -a "$0" "$@"
  '';
in {
  options.modules.hardware.nvidia = with types; {
    enable = mkOption {
      type = bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    services.xserver.videoDrivers = mkDefault [ "nvidia" ];
    hardware = {
      opengl.enable = true;
      nvidia.prime = {
        offload.enable = lib.mkDefault true;
        # Hardware should specify the bus ID for intel/nvidia devices
      };
    };

    environment.systemPackages = with pkgs; [
      nvidia-offload
      # Respect XDG conventions, damn it!
      (writeScriptBin "nvidia-settings" ''
        #!${stdenv.shell}
        mkdir -p "$XDG_CONFIG_HOME/nvidia"
        exec ${config.boot.kernelPackages.nvidia_x11.settings}/bin/nvidia-settings --config="$XDG_CONFIG_HOME/nvidia/settings"
      '')
    ];
  };
}
