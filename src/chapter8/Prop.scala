package chapter8

trait Prop {
  def check(): Boolean

  def &&(p: Prop): Prop = () => check() && p.check()
}


