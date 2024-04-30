# Package

version       = "0.1.0"
author        = "Kimberly Wilber"
description   = "A schema-aware d-Doc database"
license       = "LGPL-2.1-or-later"
srcDir        = "src"
installExt    = @["nim"]
#bin           = @["ddb"]


# Dependencies

requires "nim >= 1.6.8"
requires "fusion >= 1.2.0"
requires "npeg >= 1.2.1"
requires "cligen >= 1.6.18"
requires "db_connector >= 0.1.0"
