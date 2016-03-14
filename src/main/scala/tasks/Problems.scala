package tasks

import java.util.UUID

import com.typesafe.config.ConfigFactory

import scala.collection.mutable.Builder
import scala.collection.{mutable, immutable}

object Problems extends App {
  //val myCfg =  ConfigFactory.parseFile(new File("worker/src/main/resources/reference.conf"))

  val config = ConfigFactory.load("application.conf")
  //"worker/src/main/resources")
  val config2 = ConfigFactory.load("fu.conf")
  val root2 = config2.getString("akka.actor.provider")
  val root = config.getString("akka.actor.provider")
  val httpConfig = config.getConfig("akka.actor").getString("provider")

  //  val interface = httpConfig.getString("interface")
  //  val port = httpConfig.getInt("port")
  println(root + httpConfig + root2)
}

object CovariantContravariantTest extends App {

  class First()

  class Second() extends First

  case class Covariant[+A]()

  val a: Covariant[First] = Covariant[Second]()

  //TODO: val a2: Covariant[Second] = Covariant[First]()
  /*error: type mismatch;
  found   : Covariant[AnyRef]
  required: Covariant[String]
  */
  case class Contravariant[-A]()

  val b: Contravariant[Second] = Contravariant[First]()

  //TODO: val b2:Contravariant[First] = Contravariant[Second]()
  /*error: type mismatch;
  found   : Contravariant[String]
  required: Contravariant[AnyRef]
  */

  //Contravariant
  def test_1[T](d: List[T]) = d.size

  def test_2(d: List[T forSome {type T}]) = d.size

  def test_3(d: List[_]) = d.size

}

object ImplicitTest extends App {

  case class A(int: Int)

  implicit def objConvert(valur: A) = s"${valur.int} + 0_0"

  implicit def intConvert(valur: String) =  valur.toInt + 2

  println(A(25) + "df")
  val res2: Int =  "5"
  println(res2)
}

object Help {
  def newUUID = UUID.randomUUID().toString
}

object TestMap extends App {
  val m1: Map[String, Int] = Map.empty
  val m2: String Map Int = Map.empty
  val tp1: (String, Int) = null

  class A(value: Int) {
    require(value > 10, "Value must be > 10")
  }

}

object SprayJsonTest extends App {

  import spray.json._

  case class NamedList[A](name: String, items: List[A])

  case class Item(t: String)

  object MyJsonProtocol extends DefaultJsonProtocol {
    implicit def namedListFormat[A: JsonFormat] = jsonFormat2(NamedList.apply[A])

    implicit val ItemDTO = jsonFormat1(Item.apply)
  }

  import MyJsonProtocol._

  val list = NamedList[Item](name = "Alex", items = Item("Moran") :: Item("Sem") :: Nil)
  val res = list.toJson.toString()
  val parse = res.parseJson.convertTo[NamedList[Item]]

  implicit class Switcher[A <: Item](data: NamedList[A]) {
    def getEr = data.name match {
      case "Jone" => ("It`s Jone", data)
      case "Alex" => ("It`s Alex", data)
    }

    def getErFunc[T](func: (NamedList[A], String) => T) = data.name match {
      case "Jone" => ("It`s Jone", func(data, "Param"))
      case "Alex" => ("It`s Alex", func(data, "Not Param"))
    }
  }

  val res2 = parse.getEr
  val res3 = parse.getErFunc((f, s) => (f.items.size, s.toUpperCase))
}

object Kovariance extends App {

  trait Friend[-T] {
    def pointToBe: Int

    def beFriend(someone: T): Boolean
  }

  class Person extends Friend[Person] {
    def beFriend(someone: Person): Boolean = this.pointToBe == someone.pointToBe

    def pointToBe: Int = 25
  }

  class Student extends Person {
    override def pointToBe: Int = 10
  }

  def makeFriend(s: Student, f: Friend[Student]): Boolean = {
    f.beFriend(s)
  }

  val Alex = new Student
  val Susan = new Person

  println(makeFriend(Alex, Susan))

}

object LazyPowTest extends App {

  case class GroupMember(userId: String, groupId: String, someDate: String)

  implicit class GroupMemberListOpts(list: List[GroupMember]) {
    def groupByUsers = list.groupBy(_.userId).mapValues(_.map(qm => qm.groupId -> qm.someDate).toMap)

    def groupByUsersPow = list.groupBy(_.userId).mapValues(_.map(qm => qm.groupId -> qm.someDate).toMap)
  }

  val data = (4 to 10) flatMap { n => GroupMember(Help.newUUID, Help.newUUID, n.toString) :: Nil }
  println(data)
}

object P23 extends App {
  //  scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  //  res0: List[Symbol] = List('e, 'd, 'a)
  def getRandom[A](n: Int, list: List[A]): List[A] = {
    def item = scala.util.Random.nextInt(list.size)
    (1 to n).toList.foldLeft(List.empty[A]) {
      case (k, v) => list(item) :: k
    }
  }

  println(getRandom(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)))

}

object P22 extends App {
  //  scala> range(4, 9)
  //  res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  def rangeFlatMap[T <: Int](f: T, s: T) = (f to s flatMap {
    _ :: Nil
  }).toList

  println(rangeFlatMap(4, 9))

  def rangeRec() = {}

}

//https://localhost:8443/0.1/users/name/Harry Potter

object URLEncoderTest extends App {

  import java.net.URLEncoder

  val str = "https://localhost:8443/0.1/users/name/Harry Potter"
  val res = URLEncoder.encode(str, "UTF-8")
  println(res)
}

object TaggetType2 extends App {
  type Tagged[U] = {type Tag = U}
  type @@[T, U] = T with Tagged[U]

  trait USER_ID

  trait GROUP_ID

  type UserId = String @@ USER_ID
  type GroupId = String @@ GROUP_ID

  //@@[String, GROUP_ID]

  case class User(id: UserId, name: String)

  case class Group(id: GroupId, name: String)

  object Implicits {
    private def userId(id: String): UserId = id.asInstanceOf[UserId]

    private def groupId(id: String): GroupId = id.asInstanceOf[GroupId]

    implicit class StringOpts(s: String) {
      def toUserId = userId(s)

      def toGroupId = groupId(s)
    }

  }

  import Implicits._

  def getUser(id: UserId): User = User(id, "user name")

  def getGroup(id: GroupId): Group = Group(id, "group name")

  val userId = "abc".toUserId
  val groupId = "def".toGroupId

  getUser(userId)
  //  getUser(groupId)
  //  type mismatch;
  //  [error]  found   : RunIt.<refinement>.type (with underlying type RunIt.GroupId)
  //  [error]  required: RunIt.UserId
  //  [error]     (which expands to)  String with AnyRef{type Tag = RunIt.USER_ID}
  getGroup(groupId)

  getUser(userId) match {
    //  case User(id: String, name) => println(userId)
    //  [error] ...: pattern type is incompatible with expected type;
    //  [error]  found   : String
    //  [error]  required: String with Object{type Tag = RunIt.USER_ID}
    case User(`userId`, name) => println(s"matched $name")
    case _ => println("can't match")
  }
  println(userId.split("").mkString("|"))
}

object TaggedType extends App {
  var d: AnyVal = {
    println(234)
  }

  //  def sameTest(list: List[GroupMember]): Unit ={
  //    list.map{
  //      case GroupMember(c,v,b) => ???
  //    }
  //    val ff = list.groupBy(_.userId).mapValues(_.map(qm => qm.userId -> qm.joinDate).toMap)
  //    val ff2 = list.groupBy(_.userId).mapValuesSafe(_.map(qm => qm.userId -> qm.joinDate).toMap)
  //    //list.map(gm => gm.userId -> Map(gm.groupId -> gm.joinDate)).toMap
  //  }
  sealed trait CacheKeyStatus

  case object NotProcessed extends CacheKeyStatus

  case object Initializing extends CacheKeyStatus

  case object Cached extends CacheKeyStatus

  case object Unknown extends CacheKeyStatus

  //
  type Tagged[U] = {type Tag = U}
  type @@[T, U] = T with Tagged[U]

  sealed trait MAP

  sealed trait VALUES extends MAP

  sealed trait HITS extends MAP

  sealed trait FAILED_KEYS extends MAP

  type Values = mutable.Map[Int, String] @@ VALUES
  type Hits = mutable.Map[Int, String] @@ HITS
  type FailedKeys = mutable.Map[Int, String] @@ FAILED_KEYS

  val values2 = mutable.Map.empty.asInstanceOf[Values]
  val hits2 = mutable.Map.empty.asInstanceOf[Hits]
  val failedKeys2 = mutable.Map.empty.asInstanceOf[FailedKeys]

  values2 ++= Map(24 -> "dd", 258 -> "ddd")
  hits2 ++= Map(25 -> "dd", 259 -> "ddd")
  failedKeys2 ++= Map(26 -> "dd", 260 -> "ddd")

  val lstMap2 = List(values2, hits2, failedKeys2)

  def f[T <: mutable.Map[Int, String] @@ _](v: T) = v match {
    case _: Values => "Values"
    case _: Hits => "Hits"
    case _: FailedKeys => "FailedKeys"
    case _ => "Unknown"
  }

  //  f(x)
  println(f(failedKeys2))

  val values: mutable.Map[Int, String] = mutable.Map.empty
  val hits: mutable.Map[Int, String] = mutable.Map.empty
  val failedKeys: mutable.Map[Int, String] = mutable.Map.empty

  values ++= Map(24 -> "dd", 258 -> "ddd")
  hits ++= Map(25 -> "dd", 259 -> "ddd")
  failedKeys ++= Map(26 -> "dd", 260 -> "ddd")

  val lstMap = values :: hits :: failedKeys :: Nil
  val shaga = lstMap.find(_.contains(259)).map(_ match {
    case k@dd if k == values => Unknown
    case k@dd if k == hits => Initializing
    case dd if dd == failedKeys => Cached
  }).getOrElse(NotProcessed)
  println(shaga)
  //
  //  val ddd = lstMap.map(gg => (gg, gg.contains(26)) match {
  //    case (dd: Values, true) => Unknown
  //    case (dd: Hits, true) => Initializing
  //    case (dd: FailedKeys, true) => Cached
  //    case (_, false) => NotProcessed
  //  })

  //  val ddd2 = lstMap.filter(_.contains(26)) match {
  //    case Nil => NotProcessed
  //    case list => list map {
  //      case dd  if dd.isInstanceOf[Values] => Unknown
  //      case dd  if dd.isInstanceOf[Hits] => Initializing
  //      case dd  if dd.isInstanceOf[FailedKeys] => Cached
  //      case _ => NotProcessed
  //    }
  //  }
  //

}

object P21 extends App {
  //  scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  //  res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  def insertAt(symbol: AnyRef, position: Int, list: List[AnyRef]): List[AnyRef] = {
    list.splitAt(position) match {
      case (listA, listB) => listA ::: symbol :: listB
    }
  }

  def insertAt2(symbol: AnyVal, position: Int, list: List[AnyVal]): List[AnyVal] = {
    list.splitAt(position) match {
      case (listA, listB) => listA ::: symbol :: listB
    }
  }

  implicit class istOPS[A](list: List[A]) {
    def insertAt(symbol: A, position: Int): List[A] = {
      list.splitAt(position) match {
        case (listA, listB) => listA ::: symbol :: listB
      }
    }
  }

  println(insertAt(User(4), 1, List('a, 'b, 'c, 'd)))
  println(insertAt2(4, 1, List(3, 3, 3, 3)))
  println(List('a, 'b, 'c, 'd).insertAt('new, 1))
}


case class User(idx: Int) {
  def >(user: User): Boolean = {
    idx > user.idx
  }
}

case class Emoji(user: User, idxE: Int) {
  def >(emoji: User): Boolean = {
    emoji.idx > user.idx
  }
}

object P20_1 extends App {

  implicit class istOPS[A <: Emoji](list: List[A]) {
    def groupBy2[K <: User](f: A => K): Map[K, List[A]] = {
      list.foldLeft(Map[K, List[A]]()) {
        case (m, a) =>
          m.get(f(a)) match {
            case Some(listA) => m.updated(f(a), listA match {
              case Nil => a :: list
              case _ =>
                listA.span(_ > f(a)) match {
                  case (left, right) if right.headOption.contains(f(a)) => listA
                  case (left, right) => left ::: a :: right
                }
            })
            case None => m + (f(a) -> List(a))
          }
      }
    }
  }

  println(List(Emoji(User(3), 5), Emoji(User(1), 1), Emoji(User(1), 1), Emoji(User(3), 1), Emoji(User(1), 4), Emoji(User(1), 2)).groupBy2((i: Emoji) => i.user))
  println(List(Emoji(User(3), 5), Emoji(User(1), 1), Emoji(User(1), 1), Emoji(User(3), 1), Emoji(User(1), 4), Emoji(User(1), 2)).sortWith((d1: Emoji, d2: Emoji) => d1.idxE > d2.idxE).groupBy2((i: Emoji) => i.user))
  println("lahefouhaseu")
  println(List(Emoji(User(3), 5), Emoji(User(1), 1), Emoji(User(1), 1), Emoji(User(3), 1), Emoji(User(1), 4), Emoji(User(1), 2)).groupBy((i: Emoji) => i.user))
  println(List(Emoji(User(3), 5), Emoji(User(1), 1), Emoji(User(1), 1), Emoji(User(3), 1), Emoji(User(1), 4), Emoji(User(1), 2)).sortWith((d1: Emoji, d2: Emoji) => d1.idxE < d2.idxE).groupBy((i: Emoji) => i.user))
}


object P18 extends App {
  //  scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //  res0: List[Symbol] = List('d, 'e, 'f, 'g)
  def slice[T](f: Int, e: Int, list: List[T]): List[T] = {
    if (f > e) throw new IllegalArgumentException
    list.slice(f, f + e - f)
  }

  def slice_2[T](f: Int, e: Int, list: List[T]): List[T] = (f, e, list) match {
    case (_, _, Nil) => Nil
    case (_, `e`, _) if e <= 0 => Nil
    case (`f`, `e`, h :: tail) if f <= 0 => h :: slice_2(0, e - 1, tail)
    case (`f`, `e`, h :: tail) => slice_2(f - 1, e - 1, tail)
  }

  println(slice_2(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}

object P17 extends App {
  //scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  def split[T](n: Int)(list: List[T]): (List[T], List[T]) = {
    list.splitAt(n)
  }

  def split_2[T](n: Int)(list: List[T]): (List[T], List[T]) = {
    (list.take(n), list.drop(n))
  }

  //Not My
  def splitTailRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) = {
    def splitR(curN: Int, curL: List[A], pre: List[A]): (List[A], List[A]) =
      (curN, curL) match {
        case (_, Nil) => (pre.reverse, Nil)
        case (0, list) => (pre.reverse, list)
        case (n, h :: tail) => splitR(n - 1, tail, h :: pre)
      }
    splitR(n, ls, Nil)
  }

  println(split_2(3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))

}

object P16 extends App {
  //  scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  //  res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  def drop[T](n: Int)(list: List[T]): List[T] = {
    Nil
    //    case Nil => List.empty
    //    case x :: xs => x ::  drop(n)(list.drop(n))
  }

  println(drop(3)(List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
}

object P15 extends App {
  private[this] def duplicate_3[T](n: Int)(list: List[T]): List[Any] = {
    list.flatMap(x => List.fill(n)(x))
  }

  def dupl = duplicate_3(2) _

  println(dupl(List('a, 'b, 'c, 'c, 'd)))
}

object P14 extends App {
  //  scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  //  res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  def duplicate_2[T](list: List[T]): List[T] = {
    list.flatMap(x => List(x, x))
  }

  private[this] def duplicate_3[T](n: Int)(list: List[T]): List[Any] = {
    list.flatMap(x => List.fill(n)(x))
  }

  val dupl = duplicate_3(2) _
  println(duplicate_2(List('a, 'b, 'c, 'c, 'd)))
  println(dupl(List('a, 'b, 'c, 'c, 'd)))
}

object P12 extends App {
  //  scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  //  res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  def decode[T](list: List[(Int, T)]) = {
    list.flatMap { e => List.fill(e._1)(e._2) }
  }

  def make[T](a: (Int, T)): List[T] = a match {
    case (0, _) => List.empty
    case _ => {
      a._2 :: make((a._1 - 1, a._2))
    }
  }

  def decode_2[T](list: List[(Int, T)]) = {
    list.flatMap { e => make(e) }
  }

  println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
  println(decode_2(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))

}

object P11 extends App {
  //  scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //  res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  import P10.encode

  def encodeModified[T](list: List[T]): List[Any] = {
    encode(list).map(t => if (t._1 == 1) t._2 else t)
  }

  def encodeModified_2[T](list: List[T]): List[Either[T, (Int, T)]] = {
    encode(list).map(t => if (t._1 == 1) Left(t._2) else Right(t))
  }

  println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  println(encodeModified_2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))

}

object P10 extends App {
  //  scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //  res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encode[T](list: List[T]): List[(Int, T)] = {
    pack_2(list).map(x => (x.length, x.head))
  }

  def pack_2[T](ls: List[T]): List[List[T]] = ls match {
    case Nil => List(List.empty[T])
    case _ => {
      type A = (List[T], List[T])
      val a: A = ls.span(_ == ls.head)
      if (a._2 == Nil) List(a._1)
      else a._1 :: pack_2(a._2)
    }
  }

  println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
}

object P09 extends App {
  //     Example:
  //     scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //     res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List.empty[A])
    else {
      val (packed, tail) = ls.span(_ == ls.head)
      if (tail == Nil) List(packed)
      else packed :: pack(tail)
    }
  }

  def pack_2[A](ls: List[A]): List[List[A]] = ls match {
    case Nil => List(List.empty[A])
    case _ => {
      type a = (List[A], List[A])
      val a = ls.span(_ == ls.head)
      if (a._2 == Nil) List(a._1)
      else a._1 :: pack_2(a._2)
    }
  }

  println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)).toString())
  println(pack_2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)).toString())
}

object P08 extends App {
  //  scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  //  res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)

  def compress[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: rest => head :: compress(list.dropWhile(x => x == head))
  }

  def compress_2[T](list: List[T]): List[T] = list match {
    case Nil => Nil
    case head :: rest => head :: compress(list.filterNot(x => x == head))
  }

  def compress_3_teil[T](list: List[T]): List[T] = {

    def rec(list: List[T], resList: List[T]): List[T] = list match {
      case head :: tail => rec(tail.dropWhile(x => x == head), head :: resList)
      case Nil => resList.reverse
    }
    rec(list, List.empty[T])
  }

  println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)).toString())
  println(compress_2(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)).toString())
  println(compress_3_teil(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)).toString())
}

object P07 extends App {
  //  scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  //  res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  def flatten(list: List[Any]): List[Any] = list.flatMap {
    case m: List[_] => flatten(m)
    case value => List(value)

  }

  println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))

  //  val nestedNumbers = List(List(1, 1), List(2), List(3, List(5, 8)))
  //  def oplisp[T <: Float](xs: List[List[T]])(implicit o: Numeric)= {
  //    xs.flatMap(x => x.map(_ * 2))
  //  }
}
