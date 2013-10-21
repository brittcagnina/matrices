package worksheet
import scala.collection.mutable.ListBuffer

object matrices {
  
  // Vector Builder
	def >(elem: Double*): Vector = {
		val elems = (for(i <- 0 to elem.length - 1) yield elem(i)).toList
		new Vector(elems)
	}                                         //> > : (elem: Double*)worksheet.Vector
	
	def rowMatrix(row: Vector*): RowMatrix = {
		val rows = (for(i <- 0 to row.length - 1) yield row(i)).toList
		new RowMatrix(rows)
	}                                         //> rowMatrix: (row: worksheet.Vector*)worksheet.RowMatrix
	
	def columnMatrix(column: Vector*): ColumnMatrix = {
		val columns = (for(i <- 0 to column.length - 1) yield column(i)).toList
		new ColumnMatrix(columns)
	}                                         //> columnMatrix: (column: worksheet.Vector*)worksheet.ColumnMatrix
	
	val m1 = rowMatrix(
		>(1, 0, 0),
		>(0, 1, 0),
		>(0, 0, 1)
	)                                         //> m1  : worksheet.RowMatrix = worksheet.RowMatrix@7ddc5f7
	m1.view                                   //> 
                                                  //| 
                                                  //| |1.0, 0.0, 0.0|
                                                  //| |0.0, 1.0, 0.0|
                                                  //| |0.0, 0.0, 1.0|
                                                  //| res0: Any = ()
  m1.getNthColumn(1).dot(m1.getNthRow(0))         //> res1: Double = 0.0
  
 val m2 = rowMatrix(
		>(1, 4, 7),
		>(3, 0, 5),
		>(-1, 9, 11)
	)                                         //> m2  : worksheet.RowMatrix = worksheet.RowMatrix@26767bef
  m2.view                                         //> 
                                                  //| 
                                                  //| |1.0, 4.0, 7.0|
                                                  //| |3.0, 0.0, 5.0|
                                                  //| |-1.0, 9.0, 11.0|
                                                  //| res2: Any = ()
  m2.cofactor.view                                //> 
                                                  //| 
                                                  //| |-45.0, -38.0, 27.0|
                                                  //| |19.0, 18.0, -13.0|
                                                  //| |20.0, 16.0, -12.0|
                                                  //| res3: Any = ()
  m2.det                                          //> res4: Double = -8.0
  (m2 * m2.adj.scale(1/m2.det)).view              //> 
                                                  //| 
                                                  //| |1.0, 0.0, 0.0|
                                                  //| |0.0, 1.0, 0.0|
                                                  //| |0.0, 0.0, 1.0|
                                                  //| res5: Any = ()
  m2.getNthColumn(2).view                         //> res6: String = <7.0, 5.0, 11.0>
  val m4 = m1 * m2                                //> m4  : worksheet.Matrix = worksheet.ColumnMatrix@c5ec9da
  m4.view                                         //> 
                                                  //| 
                                                  //| |1.0, 4.0, 7.0|
                                                  //| |3.0, 0.0, 5.0|
                                                  //| |-1.0, 9.0, 11.0|
                                                  //| res7: Any = ()
  val m5 = m4.trans                               //> m5  : worksheet.Matrix = worksheet.RowMatrix@23160d8e
  m5.view                                         //> 
                                                  //| 
                                                  //| |1.0, 3.0, -1.0|
                                                  //| |4.0, 0.0, 9.0|
                                                  //| |7.0, 5.0, 11.0|
                                                  //| res8: Any = ()
  m5.isReducedRowEchelon                          //> res9: Boolean = false
  m5.getMinor(0, 2).view                          //> 
                                                  //| 
                                                  //| |4.0, 0.0|
                                                  //| |7.0, 5.0|
                                                  //| res10: Any = ()
  
  
  m4.add(m5).view                                 //> 
                                                  //| 
                                                  //| |2.0, 7.0, 6.0|
                                                  //| |7.0, 0.0, 14.0|
                                                  //| |6.0, 14.0, 22.0|
                                                  //| res11: Any = ()
  val m8 = m4.add(m5)                             //> m8  : worksheet.Matrix = worksheet.RowMatrix@f530ff3
  m4.add(m5).det                                  //> res12: Double = -294.0
  val m9 = m4.add(m5).inverse                     //> m9  : worksheet.Matrix = worksheet.RowMatrix@163c2261
 (m8 * m9).view                                   //> 
                                                  //| 
                                                  //| |1.0, 0.0, 0.0|
                                                  //| |0.0, 0.9999999999999999, 0.0|
                                                  //| |0.0, 0.0, 1.0|
                                                  //| res13: Any = ()
 
  m4.isReducedRowEchelon                          //> res14: Boolean = false
  
  rowMatrix(null).view                            //> res15: Any = | E |
  
  val v1 = >(1, 0, 0)                             //> v1  : worksheet.Vector = worksheet.Vector@45f519ae
  v1.view                                         //> res16: String = <1.0, 0.0, 0.0>
  
  val v2 = v1.add(>(2,5,6))                       //> v2  : worksheet.Vector = worksheet.Vector@5e691263
  v2.view                                         //> res17: String = <3.0, 5.0, 6.0>
}

abstract class Matrix {

	def get(r: Int, c: Int): Double
	val numRows: Int
	val numColumns: Int
	
	def add(that: Matrix): Matrix = {
		if(this.numColumns != that.numColumns && this.numRows != that.numRows) new RowMatrix(null)
		else {
			val rows: List[Vector] = (
				for(i <- 0 to this.numRows - 1)
				yield this.getNthRow(i).add(that.getNthRow(i))
			).toList
			new RowMatrix(rows)
		}
	}
	
	def *(that: Matrix): Matrix = {
		if(this.numColumns != that.numRows) new RowMatrix(null)
		else {
			var rows: List[Vector] = Nil
			for(i <- 0 to that.numColumns - 1) {
				def iter(count: Int, vector: Vector): Vector = {
					if(count > this.numColumns - 1) vector
					else {
						iter(count + 1, vector.append(this.getNthRow(count).dot(that.getNthColumn(i))))
					}
				}
				//Try ListBuffer
				rows = rows ::: List(iter(0, new Vector(List())))
			}
			new ColumnMatrix(rows)
		}

	}
	
	// Multiply matrix by constant x
	def scale(x: Double): Matrix = {
			val rows = (
				for(i <- 0 to numRows - 1)
				yield new Vector(
					(for(j <- 0 to numColumns - 1) yield x * this.getNthRow(i).get(j)).toList
				)
			)
			new RowMatrix(rows.toList)
	}
	
	// ListBuffer Example
	def trans: Matrix = {
		val columns = (for(i <- 0 to this.numColumns - 1) yield this.getNthColumn(i)).toList
		new RowMatrix(columns.toList)
	}
	
	def inverse: Matrix = {
		if(!this.isInvertible) throw new IllegalArgumentException
		else this.adj.scale(1/this.det)
	}
	
	def adj: Matrix = {
		this.cofactor.trans
	}
	
	def cofactor: Matrix = {
			val rows = (
				for(i <- 0 to numRows - 1)
				yield new Vector(
					(for(j <- 0 to numColumns - 1) yield signFactor(i,j) * this.getMinor(i,j).det).toList
				)
			)
			new RowMatrix(rows.toList)
	}
	
	def isInvertible: Boolean = this.det != 0
	
	def isReducedRowEchelon: Boolean = {
		if(this.getNthRow(0).get(0) != 1 && this.getNthRow(0).get(0) != 0) false
		for(i <- 1 to this.numRows - 1) {
			for(j <- 0 to i - 1) {
				if(this.getNthRow(i).get(j) != 0) return false
			}
			if(this.getNthRow(i).get(i) != 1 && this.getNthRow(i).get(i) != 0) return false
		}
		true
	}
	
	def getNthRow(n: Int) = {
		val row: List[Double] = (for(i <- 0 to numColumns - 1) yield get(n, i)).toList
		new Vector(row)
	}
	
	def getNthColumn(n: Int) = {
		val column: List[Double] = (for(i <- 0 to numRows - 1) yield get(i, n)).toList
		new Vector(column)
	}
	
	// Returns Minor Matrix
	def getMinor(nthRow: Int, nthColumn: Int): Matrix = {
			var rows = ListBuffer[Vector]()
			for(i <- 0 to numRows - 1) {
				if(i != nthRow) {
					rows += this.getNthRow(i).remove(nthColumn)
				}
			}
			new RowMatrix(rows.toList)
	}
	
	// Laplace Expansion
	def det: Double = {
			def iter(count: Int): Double = {
					if(this.numColumns == 1 && this.numRows == 1) this.getNthRow(0).get(0)
					else if(count > numColumns - 1) 0
					else signFactor(0, count) * this.getNthRow(0).get(count) * this.getMinor(0, count).det + iter(count + 1)
			}
			iter(0)
	}
	
	// (-1) ^ (i + j)
	def signFactor(i: Int, j: Int) = if((i + j) % 2 == 0) 1 else -1
	
	def view = {
		if(numRows == 0 && numColumns == 0) "| E |"
		else if(numRows == 1 && numColumns == 1) "|" + get(0,0) + "|"
		else {
			for(i <- 0 to numRows - 1) {
			if(i == 0) println("\n")
				def iter(count: Int, row: String): String = {
					if(count == 0) iter(count + 1, row + get(i, count))
					else if(count == numColumns - 1) row + ", " + get(i, count)
					else iter(count + 1, row + ", " + get(i, count))
				}
				println("|" + iter(0, "") + "|")
			}
		}
	}
}

class ColumnMatrix(column: List[Vector]) extends Matrix {
	def get(r: Int, c: Int) = if(column.contains(null)) 0 else column(c).get(r)
	val numRows = if(column.contains(null)) 0 else column(0).length
	val numColumns = if(column.contains(null)) 0 else column.length
}

class RowMatrix(row: List[Vector]) extends Matrix {
	def get(r: Int, c: Int) = if(row.contains(null)) 0 else row(r).get(c)
	val numRows = if(row.contains(null)) 0 else row.length
	val numColumns = if(row.contains(null)) 0 else row(0).length
}

class Vector(elem: List[Double]) {
	def iter(count: Int, list: List[Double]): List[Double] = {
			if(elem == null) null
			if(count >= elem.length) list
			else iter(count + 1, list :+ elem(count))
	}
	
	val component: List[Double] = iter(0, List())
	
	val length: Int = component.length
	
	def get(n: Int): Double = {
		component(n)
	}
	
	def append(x: Double): Vector = if(component == Nil) new Vector(List(x)) else new Vector(component :+ x)
	
	def remove(n: Int): Vector = new Vector(component.take(n) ++ component.drop(n + 1))
	
	def dot(v: Vector): Double = {
		if(elem.length != v.length) 0
		else {
			def iter(count: Int, value: Double): Double = {
				if(count > elem.length - 1) value
				else {
					iter(count + 1, value + elem(count) * v.get(count))
				}
			}
			iter(0, 0)
		}
	}
	
	def add(v: Vector): Vector = {
		if(component.length != v.length) new Vector(null)
		else {
			val vector: List[Double] = (for(i <- 0 to elem.length - 1) yield elem(i) + v.get(i)).toList
			new Vector(vector)
		}
	}
	
	def view = {
		if(component == null) "< E >"
		else {
			def iter(count: Int): String = {
				if(count == 0) "<" + component(count) + iter(count + 1)
				else if(count == component.length - 1) ", " + component(count) + ">"
				else ", " + component(count) + iter(count + 1)
			}
			iter(0)
		}
	}
	
}