package dataset

import dataset.util.Commit.{Commit, CommitData, File, Parent, Stats, User}
import java.text.SimpleDateFormat
import java.util.SimpleTimeZone


import java.text.SimpleDateFormat
import java.util.SimpleTimeZone
import scala.math.Ordering.Implicits._

/**
 * Use your knowledge of functional programming to complete the following functions.
 * You are recommended to use library functions when possible.
 *
 * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
 * When asked for dates, use the `commit.commit.committer.date` field.
 *
 * This part is worth 40 points.
 */
object Dataset {


  /** Q23 (4p)
   * For the commits that are accompanied with stats data, compute the average of their additions.
   * You can assume a positive amount of usable commits is present in the data.
   *
   * @param input the list of commits to process.
   * @return the average amount of additions in the commits that have stats data.
   */
  def avgAdditions(input: List[Commit]): Int = {
    val mapped = input.flatMap(x => x.stats).map(x => x.additions)
    mapped.sum / mapped.length
  }

  /** Q24 (4p)
   * Find the hour of day (in 24h notation, UTC time) during which the most javascript (.js) files are changed in commits.
   * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
   * NB!filename of a file is always defined.
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   *
   * @param input list of commits to process.
   * @return the hour and the amount of files changed during this hour.
   */
  def jsTime(input: List[Commit]): (Int, Int) = {
    def classifier(c : Commit): Int = {
      val formatter = new SimpleDateFormat("HH")
      formatter.setTimeZone(new SimpleTimeZone(0, "UTC"))

      formatter.format(c.commit.committer.date).toInt
    }
    if(input.nonEmpty){
      val map = input.groupBy(classifier)
      val filtered = map.mapValues(x => x.map(t => t.files.map(s => {
        if (s.filename.getOrElse("").contains(".js")) 1
        else 0
      }).sum
      ))
      filtered.mapValues(x => x.sum).maxBy(t => t._2) // finds the biggest count and returns as a tuple the hour and the amount of files
    }
    else (0, 0)
  }


  /** Q25 (5p)
   * For a given repository, output the name and amount of commits for the person
   * with the most commits to this repository.
   * For the name, use `commit.commit.author.name`.
   *
   * @param input the list of commits to process.
   * @param repo  the repository name to consider.
   * @return the name and amount of commits for the top committer.
   */
  def topCommitter(input: List[Commit], repo: String): (String, Int) = {
    def classifier(c : Commit): String = {
      c.commit.author.name
    }
    val map = input.groupBy(classifier).mapValues(x => x.length)
    map.maxBy(t => t._2)
  }

  /** Q26 (9p)
   * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
   * Leave out all repositories that had no activity this year.
   *
   * @param input the list of commits to process.
   * @return a map that maps the repo name to the amount of commits.
   *
   *         Example output:
   *         Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
   */
  def commitsPerRepo(input: List[Commit]): Map[String, Int] = {
    def classifier(c : Commit): String = {
      val list = c.url.split("/") // example url from our github: https://github.com/almost06/fp-big-data/commit/a4e82614639068fdebc3a7d7c972965dc4ef63da
      list(3) + "/" + list(4)
    }
    val map = input.groupBy(classifier).mapValues(t => t.count(x => x.commit.author.date.getYear == 119))
    map.filter{case(key, value) => value > 0}
  }


  /** Q27 (9p)
   * Derive the 5 file types that appear most frequent in the commit logs.
   * NB!filename of a file is always defined.
   * @param input the list of commits to process.
   * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
   */
  def topFileFormats(input: List[Commit]): List[(String, Int)] = {
    val list = input.flatMap(x => {
      x.files.map(t => t.filename.getOrElse("").replaceAll("^.*\\.", ""))
    }).groupBy(identity).mapValues(x => x.length).toList

    list.sortBy(t => t._2).take(5)
  }


  /** Q28 (9p)
   *
   * A day has different parts:
   * morning 5 am to 12 pm (noon)
   * afternoon 12 pm to 5 pm.
   * evening 5 pm to 9 pm.
   * night 9 pm to 4 am.
   *
   * Which part of the day was the most productive in terms of commits ?
   * Return a tuple with the part of the day and the number of commits
   *
   * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
   */
  def mostProductivePart(input: List[Commit]): (String, Int) = {
    def classifier(c: Commit): String = {
      val formatter = new SimpleDateFormat("HH")
      formatter.setTimeZone(new SimpleTimeZone(0, "UTC"))

      val hour = formatter.format(c.commit.committer.date).toInt
      hour match {
        case x if x >= 5 && x < 12 => "morning"
        case x if x >= 12 && x < 17 => "afternoon"
        case x if x >= 17 && x < 21 => "evening"
        case x if x >= 21 || x < 5 => "night"
      }
    }
    input.groupBy(classifier).mapValues(x => x.length).maxBy(t => t._2)

  }
}
