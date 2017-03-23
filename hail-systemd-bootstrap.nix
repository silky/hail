# | Build a profile usable by 'hail' to spin up 'hail' instances for
# a set of services on systemd-based systems.
#
# The profiles generated live in the 'hail-profiles' nix profile
# subdirectory
#
# Example: Create a hydra jobset 'infra.staging' with job 'foo-bar-box'
# defined by:
#   let hail-systemd-bootstrap = pkgs.callPackage ./hail-systemd-bootstrap.nix {};
#   in
#     hail-systemd-bootstrap
#       { services =
#           { foo = "https://hydra.example.com/job/foo/staging/foo";
#             bar = "https://hydra.example.com/job/foo/staging/bar";
#             hail-services = "https://hydra.example.com/job/infra/staging/foo-bar-box";
#           };
#       }
# On first init, if you don't already have a 'hail-hail-services' service
# that points to https://hydra.example.com/job/infra/staging/foo-bar-box,
# you will need to manually pull down a build of that job and run the
# 'activate' script.
#
# Then, your box will always have the 'staging' versions of foo and bar
# running, at least until you update your 'foo-bar-box' job to change
# the service set.
let pkgs = import <nixpkgs> {};
in
  { writeScriptBin ? pkgs.writeScriptBin
  , writeTextDir ? pkgs.writeTextDir
  , hail ? null # : Nullable Package
                # ^ The package containing the 'hail' program, or
                # 'null' to use the hail program this nix expression
                # is distributed with
  , nix ? pkgs.nix
  , bash ? pkgs.bash
  , coreutils ? pkgs.coreutils
  , systemd ? pkgs.systemd
  , runCommand ? pkgs.runCommand
  , setupSystemdUnits ? pkgs.setupSystemdUnits
  , lib ? pkgs.lib
  }:
    { services                             # : AttrSet String URI
                                           # ^ A set whose names are
                                           # profile names and values
                                           # are hydra job URIs
    , target ? "multi-user"                # : String
                                           # ^ The systemd target that
                                           # the hail services should
                                           # be Wanted-By, or null for
                                           # none.
    , namespace ? "hail-systemd-bootstrap" # : String
                                           # ^ The namespace the
                                           # services managed by hail
                                           # should live in. See the
                                           # analogous argument to
                                           # setupSystemdUnits in
                                           # nixpkgs.
    }:
      let # Haaaaacky
          this-hail-bin =
            if builtins.pathExists ./hail.cabal
              then "${toString ./.}/dist/build/hail/hail"
              else
                let found = lib.locateDominatingFile "bin" ./.;
                in if found == null
                  then throw "Could not find hail executable"
                  else builtins.storePath
                         "${toString found.path}/bin/hail";
          hail-bin = if hail == null
                       then this-hail-bin
                       else "${hail}/bin/hail";
          mk-unit = profile: uri:
            writeTextDir "hail-${profile}.service"
              ''
                [Unit]
                Description=Hail continuous self-deployment service for ${profile}
                Wants=network-online.target

                [Service]
                Environment="PATH=${nix}/bin"
                Environment="HOME=/var/lib/empty"
                ExecStart=@${hail-bin} hail --profile hail-profiles/${profile} --job-uri ${uri}
              '';
          setup-systemd-units = setupSystemdUnits
            { units = lib.mapAttrs' (profile: uri:
                rec { name = "hail-${profile}.service";
                      value =
                        { path = "${mk-unit profile uri}/${name}";
                          wanted-by = if target == null
                                        then []
                                        else [ "${target}.target" ];
                        };
                    }
              ) services;
              inherit namespace;
            };
          activate = writeScriptBin "activate"
            ''
              #!${bash}/bin/bash -e
              # We use systemd-run because the program calling this
              # script may be one of the services being updated.
              exec -a systemd-run ${systemd}/bin/systemd-run \
                --description="Update hail services" \
                ${setup-systemd-units}/bin/setup-systemd-units
            '';
      in runCommand "hail-profile" {} ''
        mkdir -p $out/bin
        ln -sv ${activate}/bin/activate $out/bin
        ln -sv ${hail-bin} $out/bin/hail
      ''
