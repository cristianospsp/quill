package io.getquill.context.sql

import io.getquill.ast._
import io.getquill.context.sql.idiom.SqlIdiom
import io.getquill.context.sql.norm.{ ExpandJoin, ExpandNestedQueries, MergeSecondaryJoin, RemoveReturning }
import io.getquill.NamingStrategy
import io.getquill.norm._
import io.getquill.util.Messages.fail
import io.getquill.util.Show.Shower

object Prepare {

  def apply(ast: Ast, params: List[Ident])(implicit d: SqlIdiom, n: NamingStrategy) = {
    val returning = CollectAst(ast) {
      case Returning(_, property) => property
    }.headOption
    val (bindedAst, idents) = BindVariables(normalize(ast), params)
    import d._
    val sqlString =
      bindedAst match {
        case q: Query =>
          val sql = SqlQuery(q)
          VerifySqlQuery(sql).map(fail)
          ExpandNestedQueries(sql, collection.Set.empty).show
        case other =>
          other.show
      }

    (sqlString, idents, returning.map(prop => returningColumn(ast, prop)))
  }

  private def returningColumn(ast: Ast, prop: String)(implicit n: NamingStrategy) = {
    val entity = CollectAst(ast) {
      case Insert(e: Entity) => e
    }.head
    val propertyAlias = entity.properties.map(p => p.property -> p.alias).toMap
    propertyAlias.getOrElse(prop, n.column(prop))
  }

  private[this] val normalize =
    (identity[Ast] _)
      .andThen(RemoveReturning.apply _)
      .andThen(Normalize.apply _)
      .andThen(ExpandJoin.apply _)
      .andThen(Normalize.apply _)
      .andThen(MergeSecondaryJoin.apply _)
      .andThen(FlattenOptionOperation.apply _)
      .andThen(RenameAssignments.apply _)
      .andThen(RenameProperties.apply _)

}
