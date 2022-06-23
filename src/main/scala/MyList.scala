package com.Namchuk.Maksym.lab1
import scala.collection.mutable.StringBuilder
import scala.annotation.tailrec
import MyList.*
enum MyList[+A]:
  case MyCons(h: A, t: MyList[A])
  case MyNil
  override def toString: String = {
    @tailrec
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyNil => sb.result
        case MyCons(h, t) => go(sb.append(h).append(if t == MyNil then "]" else ", "), t)
      }
    }
    go(new StringBuilder("["), this)
  }

object MyList:
  def apply[A](xs: A*) = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }

  def foldLeft[A,B](xs: MyList[A],z:B )(f:(A,B)=>B): B= {
    @tailrec
    def go(xs: MyList[A],acc:B)(f:(A,B)=>B): B=  {
      xs match{
        case MyNil => acc
        case MyCons(hd, tl) => go(tl,f(hd, acc))(f)
      }
    }
    go(xs, z)(f)
  }

  def reverse[A](xs:MyList[A]): MyList[A] ={
    @tailrec
    def go[A](xs: MyList[A], result: MyList[A] = MyNil): MyList[A] = {
      xs match{
        case MyNil=> result
        case MyCons(hd, tl) => go(tl, MyCons(hd, result))
      }
    }
    go(xs)
  }

  def fill[A](n: Int, a: A): MyList[A] = {tabulate(n, _ => a)}


  def splitAt[A](xs: MyList[A], n: Int): ( MyList[A],  MyList[A]) = {
    @tailrec
    def go[A](left: MyList[A], right: MyList[A], n: Int): (MyList[A], MyList[A]) = {
      if n <= 0 then {
        (reverse(left), right)
      }
      else {
        right match {
          case MyNil => (reverse(left), right)
          case MyCons(hd, tl) => go(MyCons(hd, left), tl, n - 1)
        }
      }
    }
    go(MyNil, xs, n);
  }


  def generate[A](z: A, f: A => Option[A]): MyList[A] = {
    @tailrec
    def go(b: A, f: A => Option[A], acc: MyList[A]=MyNil): MyList[A] = {
      f(b) match{
        case None => acc
        case Some(b) => go(b, f, MyCons(b,acc))
      }
    }
    go(z,f)
  }


  def tabulate[A](n: Int, f: Int => A): MyList[A]={
    @tailrec
    def go(n: Int, f: Int => A, acc: MyList[A] = MyNil): MyList[A] = {
      n match {
        case 0 => acc
        case _ => go(n - 1, f, MyCons(f(n - 1), acc))
      }
    }
    go(n, f)
  }
  
  def unique[A](xs: MyList[A]) = {
    def go(set: Set[A], acc: MyList[A]): MyList[A] = {
      acc match {
        case MyNil => MyNil
        case MyCons(hd,tail) if set contains hd => go(set, tail)
        case MyCons(hd,tail) => MyCons(hd, go(set + hd, tail))
      }
    }
    go(Set(), xs)
  }
@main def run(): Unit = {
}
