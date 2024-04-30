# This is just an example to get you started. A typical hybrid package
# uses this file as the main entry point of the application.

import ddb/docEssentials
import ddb/universeBackends/sqliteLibrary
import cligen

when isMainModule:
  var u = openSqliteLibrary "/tmp/dbtest.sqlite"
  u.add: newDoc "abc-def": title "Foobar"
