# This is just an example to get you started. A typical hybrid package
# uses this file as the main entry point of the application.

import ddb/refSchema
import ddb/universeBackends/sqliteUniverse
import cligen

when isMainModule:
  var u = openSqliteUniverse "/tmp/dbtest.sqlite"
  u.add: newExpression "abc-def": title "Foobar"
