package com.Namchuk.Maksym.lab1
import MyList.*
import munit.FunSuite
import com.Namchuk.Maksym.lab1
class MyListSuite extends FunSuite {
  test("fill for (3, h)") {
    val expected = MyList('h', 'h', 'h')
    val actual = fill(3, 'h')
    assertEquals(actual, expected)
  }
  test("fill for (5, hello) ") {
    val expected = MyList("hello","hello","hello","hello","hello")
    val actual = fill(5,"hello")
    assertEquals(actual, expected)
  }
  test("fill for (3, true) ") {
    val expected = MyList(true,true,true)
    val actual = fill(3,true)
    assertEquals(actual, expected)
  }
  test("splitAt for ([123456], 3) ") {
    val expected = (MyList(1,2,3),MyList(4,5,6))
    val actual = splitAt(MyList(1,2,3,4,5,6), 3)
    assertEquals(actual, expected)
  }
  test("splitAt for ([1234], 2) ") {
    val expected = (MyList(1,2),MyList(3,4))
    val actual = splitAt(MyList(1,2,3,4), 2)
    assertEquals(actual, expected)
  }
  test("splitAt for ([12345689], 4) ") {
    val expected = (MyList(1,2,3,4),MyList(5,6,7,8,9))
    val actual = splitAt(MyList(1,2,3,4,5,6,7,8,9), 4)
    assertEquals(actual, expected)
  }
  test("generate for (a, str => if (str.length < 5) Some(str + a) else None)") {
    val expected = MyList("aaaaa", "aaaa", "aaa", "aa")
    val actual = generate("a", str => if (str.length < 5) Some(str + "a") else None)
    assertEquals(actual, expected)
  }
  test("generate for (b, str => if (str.length < 4) Some(str + b) else None)") {
    val expected = MyList("bbbb", "bbb", "bb")
    val actual = generate("b", str => if (str.length < 4) Some(str + "b") else None)
    assertEquals(actual, expected)
  }
  test("tabulate for (2, x=>x) ") {
    val expected = MyList(0,1)
    val actual = tabulate(2, x=>x)
    assertEquals(actual, expected)
  }
  test("tabulate for (3, x=>x+10) ") {
    val expected = MyList(10,11,12)
    val actual = tabulate(3, x=>x+10)
    assertEquals(actual, expected)
  }
  test("tabulate for (3, x=>x*10) ") {
    val expected = MyList(0,10,20)
    val actual = tabulate(3, x=>x*10)
    assertEquals(actual, expected)
  }
}
