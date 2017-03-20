hail
==========

A service for pull-based continuous deployment based on hydra

Usage
------

`hail --profile PROFILE --job-uri HYDRA_JOB_URI`

Optional flags:
  * `--netrc-file NETRC_FILE` The path to a [netrc](https://linux.die.net/man/5/netrc) file specifying credentials for the hydra HTTP access.
    Defaults to `/etc/netrc` if it exists.
  * `--poll-period PERIOD` The period with which to poll the job, in minutes (default: 5)

Operation
----------

`hail` will regularly poll the latest successful build of the provided `HYDRA_JOB_URI`. When it changes, it will update `/nix/var/nix/profiles/PROFILE` to point to the new build and run `/nix/var/nix/profiles/PROFILE/bin/activate`

If there is already a path at `/nix/var/nix/profiles/PROFILE` when `hail` starts up, it will run the `activate` program immediately.

Step-by-step
-------------

1. Create a hydra jobset with a job that creates the profile for the service in question. The profile should have a program at `bin/activate` that:
     * Sets up everything needed for the service to run (e.g. adding systemd/init.d services, initializing dbs if needed, etc)
     * Is idempotent
     * Can detect if there is a previous version of the service running and switch as atomically as possible
2. Set up your nix config to pull from your hydra's binary cache.
3. Run `hail` on all machines that should run the latest version of that service (see [Bootstrapping](#bootstrapping))
4. Enjoy

Bootstrapping
--------------

If there are multiple services each managed independently with `hail`, you probably want to have those `hail` instances themselves managed by a manager `hail` instance. See `hail-systemd-bootstrap.nix` for a way to achieve this on systemd-based systems.

Future work
------------

* Error reporting in a monitorable way
* Handle the case where we don't want to update until all of a jobset succeeds
* Native haskell client to talk to the nix daemon
* Proper handling of manually-initiated rollbacks etc.
* Libraries/templates for easily deploying new services
* A good blue-greening story
* Handle service removal
* Incorporate with distro-agnostic service runner
