import java.io.File
import scala.io.Source
import scala.collection.mutable.Map
import scala.collection.mutable.ArrayBuffer
import scala.math

// Based on http://www.tfidf.com/
// Possible variations following https://en.wikipedia.org/wiki/Tf%E2%80%93idf
object TFIDF {
	var separators = "[¿?¡!“”',.;:… —\n‖‗―\"()]"
	var verbose = false

	def main(args: Array[String]) {
		var dir = args(0)
		var trivial_filename = args(1)

		var trivial_words = load_trivial_words(trivial_filename)

		var file_word_frequency = calculate_file_word_frequency(dir, trivial_words)
		var file_word_tfidf = calculate_file_word_tfidf(file_word_frequency)

		printf("TF/IDF Analysis of directory: '%s'\n", dir)
		for((file, word_tfidf) <- file_word_tfidf) {
			printf("  File: %s\n" , file)
			var sorted_by_tfidf = word_tfidf.toSeq.sortWith(_._2 > _._2)
			for((word, tfidf) <- sorted_by_tfidf.take(30)) {
				printf("    %s: %f\n", word, tfidf)
			}
		}
	}

	def load_trivial_words(trivial_filename: String): ArrayBuffer[String] = {
		var trivial_words = ArrayBuffer[String]()

		for (line <- Source.fromFile(trivial_filename).getLines) {
		    for (word <- line.split(separators)) {
		    	trivial_words += word.toLowerCase
		    }
		}

		if (verbose) {
			printf("Trivial words:\n")
			for (word <- trivial_words)
				printf("%s ",word)
			printf("\n")
		}

		return trivial_words
	}

	def getListOfFiles(base_dir: String, extensions: List[String]): List[File] = {
		var dir = new File(base_dir)
		dir.listFiles.filter(_.isFile).toList.filter { file =>
			extensions.exists(file.getName.endsWith(_))
		}
	}

	def calculate_file_word_frequency(base_dir: String, trivial_words: ArrayBuffer[String]): Map[String, Map[String, Int]] = {
		var file_word_frequency = Map[String, Map[String, Int]]()

		val files = getListOfFiles(base_dir, List("txt"))
		for(file <- files) {
			var word_frequency = Map[String, Int]()
			for(line <- Source.fromFile(file).getLines) {
				var lowercase_line = line.toLowerCase
			    for (word <- lowercase_line.split(separators)) {
			    	if (!trivial_words.contains(word)) {
				    	if (word_frequency.contains(word)) {
				    		word_frequency += ((word, word_frequency(word) + 1))
				    	}
				    	else {
				    		word_frequency += ((word, 1))
				    	}
			    	}
			    }
			}
			file_word_frequency += ((s"$file", word_frequency))
		}

		if (verbose) {
			printf("Word frequency:\n")
			for((file, word_frequency) <- file_word_frequency) {
				printf("  File: %s\n" , file)
				for((word, frequency) <- word_frequency) {
					printf("    %s: %d\n", word, frequency)
				}
			}
		}

		return file_word_frequency
	}

	def calculate_file_word_tfidf(file_word_frequency: Map[String, Map[String, Int]]): Map[String, Map[String, Double]] = {
		var num_of_docs = file_word_frequency.size

		var file_word_tfidf = Map[String, Map[String, Double]]()
		for ((file, word_frequency) <- file_word_frequency) {
			var words_in_file = count_words_in_file(word_frequency)
			var word_tfidf = Map[String, Double]()
			for ((word, frequency) <- word_frequency) {
				var tf = frequency.toDouble / words_in_file // term frequency adjusted for document length
				var idf = math.log(num_of_docs.toDouble / num_docs_with_word(word, file_word_frequency))
				var tfidf = tf * idf
				word_tfidf += ((word, tfidf))
			}
			file_word_tfidf += ((file,word_tfidf))
		}

		if (verbose) {
			printf("Word TF/IDF:\n")
			for((file, word_tfidf) <- file_word_tfidf) {
				printf("  File: %s\n" , file)
				for((word, tfidf) <- word_tfidf) {
					printf("    %s: %f\n", word, tfidf)
				}
			}
		}

		return file_word_tfidf
	}

	def count_words_in_file(word_frequency: Map[String, Int]) : Int = {
		var number_of_words = 0

		for((word, frequency) <- word_frequency) {
			number_of_words += frequency
		}

		return number_of_words
	}

	def num_docs_with_word(word: String, file_word_frequency: Map[String, Map[String, Int]]) : Int = {
		var count = 0

		for((file, word_frequency) <- file_word_frequency) {
			if(word_frequency.contains(word))
				count += 1
		}
		
		return count
	}
}
