import ../essentials/[docs, parsing, repr, vocabulary]
import db_connector/db_sqlite
import options
import sequtils
import strutils
import fusion/matching


type
    SqliteLibrary* = ref object of Library
        db: DbConn
        path: string
        vocabCache: Option[Vocabulary]

proc openSqliteLibrary*(path: string): SqliteLibrary =
    result = SqliteLibrary(path: path)
    result.db = open(path, "","","")
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
        id TEXT primary key not null,
        val BLOB,
        timestamp REAL
    );
    """
    result.db.exec sql"create index if not exists versions_time on versions(timestamp)"
    result.db.exec sql"""
    create table if not exists subexpr_cache(
        parent_id TEXT,
        child_id TEXT,
        unique(parent_id, child_id)
    );
    """
    result.db.exec sql"create index if not exists subexpr_cache_child on subexpr_cache(child_id)"


method lookup*(library: SqliteLibrary, id: ID): Option[Doc] =
    let row = library.db.getRow(sql"select id, val from docs where id=?", id)
    if row[0] == "":
        return none(Doc)
    let parseResult = parseRaw(row[1])
    if parseResult.kind != parseOk:
        raise newException(ValueError, parseResult.message)
    return some Doc(key: row[0],
                           children: parseResult.results)


method add*(library: SqliteLibrary, docs: varargs[Doc]): Doc {.discardable.} =
    for doc in docs:
        library.db.exec(sql"""
        insert into Docs(id, val, lastModified) values(?, ?, unixepoch('subsec'))
        on conflict(id) do update set val=excluded.val, lastModified=unixepoch('subsec');
        """, doc.key, doc.children.mapIt(it.reprFull).join(" "))
        library.db.exec(sql"""
        insert into versions(id,val,timestamp)
          select id, val, lastModified from Docs where id=?
        """, doc.key)
        library.db.exec(sql"delete from annot_cache where parent_id = ?;", doc.key)
        for expr in doc.children:
            library.db.exec(sql"""
            insert or ignore into annot_cache(parent_id, child_id) values(?, ?)
            """, doc.key, expr.kind)

method contains*(library: SqliteLibrary, key: ID): bool =
    let row = library.db.getRow(sql"select id, val from doc where id=?", key)
    if row[0] != "":
        return true

method searchFor*(library: SqliteLibrary, kind: ID): seq[Doc] =
    for row in library.db.rows(sql"select parent_id from annot_cache where child_id = ?", kind):
        if Some(@doc) ?= library.lookup(row[0]):
            result.add doc