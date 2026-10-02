# SPDX-License-Identifier: BSD-3-Clause
# What `oligarchy-companion enroll` writes next to install.json.
{
  archibald.companion.commander = {
    address = "10.77.0.1";
    sshKeys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFixtureCommanderKeyOnlyForTests000000000000 oligarchy" ];
    wireguard = {
      enable = true;
      address = "10.77.0.2/24";
      peerPublicKey = "fixtureCommanderWireGuardPublicKey0000000000=";
      endpoint = "192.168.1.10:51877";
    };
  };
}
