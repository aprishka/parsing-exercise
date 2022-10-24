package sb.be.guildexercises.parsing

import parsley.Parsley
import parsley.Parsley.LazyMapParsley
import parsley.combinator.{some, between, many}
import parsley.character.{alphaNum, char}

final case class Tree(value: String, children: List[Tree])

object Tree {

  def format(tree: Tree): String =
    tree.value + "[" + tree.children.map(format).mkString + "]"

  def parse(input: String): Option[Tree] =
    pTree.parse(input).toOption



  val pValue: Parsley[String]        = some(alphaNum).map(_.mkString)

  val pChildren: Parsley[List[Tree]] = between(char('['), char(']'), many(pTree))

  val pTree: Parsley[Tree]           = (Tree(_, _)).curried <#> pValue <*> pChildren

}
