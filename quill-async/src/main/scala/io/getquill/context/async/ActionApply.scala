package io.getquill.context.async

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

class ActionApply[T, O](f: List[T] => Future[List[O]])(implicit ec: ExecutionContext)
  extends Function1[List[T], Future[List[O]]] {
  def apply(params: List[T]) = f(params)
  def apply(param: T) = f(List(param)).map(_.head)
}
