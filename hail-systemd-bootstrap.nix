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
  , lib ? pkgs.lib
  }:
    { services              # : AttrSet String URI
                            # ^ A set whose names are profile names
                            # and values are hydra job URIs
    , target ? "multi-user" # : String
                            # ^ The systemd target that the hail
                            # services should be Wanted-By, or null
                            # for none.
                            #
                            # Should this default to 'basic' instead?
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
                ExecStart=@${hail-bin} hail --profile hail-profiles/${profile} --job-uri ${uri}
              '';
          # TODO This could be more atomic (by analogy to NixOS's /etc/static)
          install-unit-snippet = profile: uri:
            let unit-nm = "hail-${profile}.service";
            in ''
                 if [ -f "$unitDir/${unit-nm}" ]; then
                   unitsToStop+=("${unit-nm}")
                 fi
                 ln -sf "${mk-unit profile uri}/${unit-nm}" \
                   "$unitDir/${unit-nm}"
                 unitsToStart+=("${unit-nm}")
                 ${lib.optionalString (target != null)
                     ''
                       ln -sf "../${unit-nm}" \
                         "$unitDir/${target}.target.wants/${unit-nm}"
                     ''
                  }
            '';
       in writeScriptBin "activate"
         ''
           #!${bash}/bin/bash -e
           export PATH=${coreutils}/bin:${systemd}/bin:$PATH
           unitDir=/etc/systemd/system
           if [ ! -w "$unitDir" ]; then
             unitDir=/etc/systemd-mutable/system
             mkdir -p "$unitDir"
           fi
           ${lib.optionalString (target != null)
               "mkdir -p \"$unitDir\"/${target}.target.wants"
            }
           declare -a unitsToStop unitsToStart

           echo "Installing hail service units" >&2
           ${lib.concatStringsSep "\n\n"
               (lib.mapAttrsToList install-unit-snippet services)
            }

           if [ ''${#unitsToStop[@]} -ne 0 ]; then
             echo "Stopping old hail services" >&2
             systemctl stop "''${unitsToStop[@]}"
           fi
           systemctl daemon-reload
           echo "Starting hail services" >&2
           systemctl start "''${unitsToStart[@]}"
         ''
