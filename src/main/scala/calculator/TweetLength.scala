package calculator

object TweetLength {
  final val MaxTweetLength = 140

  def tweetRemainingCharsCount(tweetText: Signal[String]): Signal[Int] = {
    Var(MaxTweetLength - tweetLength(tweetText.apply()))
  }

  def colorForRemainingCharsCount(remainingCharsCount: Signal[Int]): Signal[String] = {
    val nChars = remainingCharsCount.apply()
    nChars match {
      case nChars: Int if (nChars >= 15) => Var("green")
      case nChars: Int if ((0 <= nChars) && (nChars < 15)) => Var("orange")
      case nChars: Int if (nChars < 0) => Var("red")
    }
  }

  /** Computes the length of a tweet, given its text string.
   *  This is not equivalent to text.length, as tweet lengths count the number
   *  of Unicode *code points* in the string.
   *  Note that this is still a simplified view of the reality. Full details
   *  can be found at
   *  https://dev.twitter.com/overview/api/counting-characters
   */
  private def tweetLength(text: String): Int = {
    /* This should besimply text.codePointCount(0, text.length), but it
     * is not implemented in Scala.js 0.6.2.
     */
    if (text.isEmpty) 0
    else {
      text.length - text.init.zip(text.tail).count(
          (Character.isSurrogatePair _).tupled)
    }
  }
}
