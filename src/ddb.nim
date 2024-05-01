# This is just an example to get you started. A typical hybrid package
# uses this file as the main entry point of the application.

import ddb/essentials/docs
import ddb/libraries/sqliteLibrary
import cligen

when isMainModule:
  var u = openSqliteLibrary()
  u.add: newDoc "abc-def": title "Foobar"
