package com.grobster.game {
	case class LotteryDrawing(name: String, drawDate: java.time.LocalDate) {
		def getWinningNumbers() = List(92, 19, 83, 65, 53, 93)
	}

	case class Player(lastName: String, firstName: String, amountPaid: Int)

	object NumberMatch {
		def countNumberNumsMatching(tries: List[Int], master: List[Int]): Int = {
			@scala.annotation.tailrec
			def _count(tries: List[Int], master: List[Int], count: Int): Int = tries match {
				case Nil => count
				case head :: tail if(master.contains(head)) => _count(tail, master, 1 + count)
				case head :: tail if(!master.contains(head)) => _count(tail, master, count)
			}
			_count(tries, master, 0)
		}
	}
}

package com.grobster.util {
	object ListHelper {
		def removeDuplicate[A](li: List[A]): List[A] = li match {
			case Nil => Nil
			case head :: tail if(tail.contains(head)) => removeDuplicate(tail)
			case head :: tail if(!tail.contains(head)) => head :: removeDuplicate(tail)
		}
	}
}

package com.grobster.test {
import com.grobster.game._
import com.grobster.util._
	object Tester {
		def main(args: Array[String]): Unit = {
			val p1 = Player("Doe", "John", 2)
			val p2 = Player("Dawson", "Jane", 2)
			val players = List(p1, p2)
			val totalReceived = players.map(x => x.amountPaid).sum
			val lottery = LotteryDrawing("Mega-Winnings", java.time.LocalDate.now)
			val lotteryNumbers = List(List(12, 44, 32, 53, 50), List(45,17,55,23,42,53), List(34,5, 46,53))
			val winnings = lotteryNumbers.map(x => NumberMatch.countNumberNumsMatching(x, lottery.getWinningNumbers))
			println("Amount Received: " + totalReceived)
			println("number of matched winning numbers: " + winnings)
		}
	}
}