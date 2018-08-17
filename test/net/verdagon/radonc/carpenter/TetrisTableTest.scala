package net.verdagon.radonc.carpenter

object TetrisTableTest {
  def main(args: Array[String]): Unit = {
    val map = 0.until(1000).map(i => i -> i * 1337).toMap
    val table = new TetrisTableGenerator[Int, Int]().generateTetrisTable(map, key => key);



    assert(table.get(10) == Some(10 * 1337))
    assert(table.get(1001) == None)
    assert(table.get(15) == Some(15 * 1337))
  }
}
