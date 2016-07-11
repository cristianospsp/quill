package io.getquill.context.jdbc

class ActionApply[T, O](f: List[T] => List[O]) extends Function1[List[T], List[O]] {
  def apply(params: List[T]) = f(params)
  def apply(param: T) = f(List(param)).head
}