import ../essentials/[docs, parsing, repr, vocabulary]
import db_connector/db_sqlite
import fusion/matching
import std/[envvars, appdirs, paths, strformat, options, sequtils, strutils, dirs]

proc getLibraryPath*(path = ""): Path =
    if path.len > 0:
        return Path(path)
    let envPath = getEnv("INKWELL_UNIVERSE_CONFIG")
    if envPath != "":
        return Path(envPath).expandTilde
    result = getConfigDir() / Path("ddb") / Path("ddb.sqlite")
    result.parentDir.createDir()


type
    SqliteLibrary* = ref object of Library
        db: DbConn
        path: string
        vocabCache: Option[Vocabulary]

proc addWithoutVersion(library: SqliteLibrary, docs: varargs[Doc]): Doc {.discardable.}=
    for doc in docs:
        library.db.exec(sql"""
        insert into Docs(id, val, lastModified) values(?, ?, unixepoch('subsec'))
        on conflict(id) do update set val=excluded.val, lastModified=unixepoch('subsec');
        """, doc.key, doc.children.mapIt(it.reprFull).join(" "))
        library.db.exec(sql"delete from subexpr_cache where parent_id = ?;", doc.key)
        for expr in doc:
            library.db.exec(sql"""
            insert or ignore into subexpr_cache(parent_id, child_id, val) values(?, ?, ?)
            """, doc.key, expr.kind, expr.val)
    return docs[0]

proc openSqliteLibrary*(maybePath = ""): SqliteLibrary =
    let actualPath = getLibraryPath(maybePath).string
    result = SqliteLibrary(path: actualPath)
    result.db = open(actualPath, "","","")
    result.db.exec sql"pragma busy_timeout=1000"
    result.db.exec sql"pragma synchronous=NORMAL"
    result.db.exec sql"pragma journal_mode=TRUNCATE"
    result.db.exec sql"""
    create table if not exists docs(
        id TEXT primary key not null unique,
        val BLOB,
        lastModified REAL
    );
    """
    result.db.exec sql"""
    create table if not exists versions(
        id TEXT,
        val BLOB,
        timestamp REAL
    );
    """
    result.db.exec sql"create index if not exists versions_id on versions(id)"
    result.db.exec sql"create index if not exists versions_time on versions(timestamp)"
    result.db.exec sql"""
    create table if not exists subexpr_cache(
        parent_id TEXT,
        child_id TEXT,
        val TEXT,
        unique(parent_id, child_id, val)
    );
    """
    result.db.exec sql"create index if not exists subexpr_cache_child on subexpr_cache(child_id)"

    for builtinDoc in allBuiltins():
        result.addWithoutVersion builtinDoc

method close*(library: SqliteLibrary) =
    library.db.close()

method lookup*(library: SqliteLibrary, id: ID): Option[Doc] =
    let row = library.db.getRow(sql"select id, val from docs where id=?", id)
    if row[0] == "":
        return none(Doc)
    var parser = ParseState(input: row[1])
    parser.parse
    if parser.kind != parseOk:
        raise newException(ValueError, parser.message)
    return some Doc(key: row[0],
                           children: parser.results)



method add*(library: SqliteLibrary, docs: varargs[Doc]): Doc {.discardable.} =
    result = library.addWithoutVersion(docs)
    for doc in docs:
        library.db.exec(sql"""
        insert into versions(id,val,timestamp)
          select id, val, lastModified from Docs where id=?
        """, doc.key)

method contains*(library: SqliteLibrary, key: ID): bool =
    let row = library.db.getRow(sql"select id, val from docs where id=?", key)
    if row[0] != "":
        return true

method searchFor*(library: SqliteLibrary, kind: ID): seq[Doc] =
    for row in library.db.rows(sql"select parent_id from subexpr_cache where child_id = ?", kind):
        if Some(@doc) ?= library.lookup(row[0]):
            result.add doc