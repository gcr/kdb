import ../refSchema
import ../dexpParsing
import ../annotRepr
import db_connector/db_sqlite
import options
import sequtils
import strutils
import fusion/matching


type
    SqliteUniverse* = ref object of Universe
        db: DbConn
        path: string
        schemaCache: Option[Schema]

proc openSqliteUniverse*(path: string): SqliteUniverse =
    result = SqliteUniverse(path: path)
    result.db = open(path, "","","")
    result.db.exec sql"pragma busy_timeout=1000"
    result.db.exec sql"pragma synchronous=NORMAL"
    result.db.exec sql"pragma journal_mode=TRUNCATE"
    result.db.exec sql"""
    create table if not exists expressions(
        id TEXT primary key not null unique,
        val BLOB
    );
    """
    result.db.exec sql"""
    create table if not exists annot_cache(
        parent_id TEXT,
        child_id TEXT,
        unique(parent_id, child_id)
    );
    """


method lookup*(universe: SqliteUniverse, id: ID): Option[Expression] =
    let row = universe.db.getRow(sql"select id, val from expressions where id=?", id)
    if row[0] == "":
        return none(Expression)
    let parseResult = parseRaw(row[1])
    if parseResult.kind != parseOk:
        raise newException(ValueError, parseResult.message)
    return some Expression(key: row[0],
                           children: parseResult.results)


method add*(universe: SqliteUniverse, exprs: varargs[Expression]): Expression {.discardable.} =
    for expr in exprs:
        universe.db.exec(sql"""
            delete from annot_cache where parent_id = ?;
        """, expr.key)
        universe.db.exec(sql"""
        insert into expressions(id, val) values(?, ?)
        on conflict(id) do update set val=excluded.val;
        """, expr.key, expr.children.mapIt(it.reprFull).join(" "))
        for annot in expr.children:
            universe.db.exec(sql"""
            insert or ignore into annot_cache(parent_id, child_id) values(?, ?)
            """, expr.key, annot.kind)

method contains*(universe: SqliteUniverse, key: ID): bool =
    let row = universe.db.getRow(sql"select id, val from expressions where id=?", key)
    if row[0] != "":
        return true

method search*(universe: SqliteUniverse, kind: ID): seq[Expression] =
    for row in universe.db.rows(sql"select parent_id from annot_cache where child_id = ?", kind):
        if Some(@expr) ?= universe.lookup(row[0]):
            result.add expr