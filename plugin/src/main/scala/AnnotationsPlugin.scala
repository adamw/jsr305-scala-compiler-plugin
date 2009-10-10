import scala.tools.nsc
import nsc.Global
import nsc.plugins.Plugin
import nsc.plugins.PluginComponent
import nsc.transform.Transform

class AnnotationsPlugin(val global: Global) extends Plugin {
  val name = "annotations"
  val description = "checks for correct usage of the nonnull annotation"
  val components = List[PluginComponent](AnnotationsComponent)

  private object AnnotationsComponent extends PluginComponent with Transform {
    val global: AnnotationsPlugin.this.global.type = AnnotationsPlugin.this.global
    val runsAfter = "namer"
    // Using the Scala Compiler 2.8.x the runsAfter should be written as below
    // val runsAfter = List[String]("refchecks");
    val phaseName = AnnotationsPlugin.this.name

    def newTransformer(unit: global.CompilationUnit) = AnnotationsTransformer

    object AnnotationsTransformer extends global.Transformer {
      override def transform(tree: global.Tree): global.Tree = {
        tree match {
          case global.DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
            {
              // First filtering the list of parameters to get only those parameters which are annotated
              // with a non-null annotation
              val nonnullParameters = findNonnullParameters(vparamss)
              println("nonullParamters: %s".format(nonnullParameters))
              if (!nonnullParameters.isEmpty) {
                var ident = 0
                new global.ForeachTreeTraverser(tree => {
                  println("%sT: %s %s".format(" " * ident, tree.getClass, tree))
                  tree match {
                    case global.Select(qualifier, symbol) => println("%sT: q = %s, s = %s".format(" " * ident, qualifier, symbol))
                    case _ => ()
                  }
                }) {
                  override def traverse(tree: global.Tree) = {
                    ident += 1
                    super.traverse(tree)
                    ident -= 1
                  }
                }.traverse(rhs)
                println()

                // Looking for the "Block" element defining the body of the method
                var block: global.Block = null
                tree.find(tree => tree.isInstanceOf[global.Block]) match {
                  case None => throw new IllegalArgumentException("Cannot find body for method %s".format(name))
                  case Some(blockTree) => block = blockTree.asInstanceOf[global.Block]
                }

                // For each non-null variable, generating and adding a check to the body of the method
                nonnullParameters foreach { param =>
                  val nonnullCheck = createNonnullCheck(param)
                  println("Check: %s".format(nonnullCheck))
                  
                  block = block + nonnullCheck
                }

                println("Whole block:\n%s".format(block))

                // Returning the tree with the block element substituted with the modified one
                tree
              } else {
                // Just returning the tree, in case no changes need to be done 
                tree
              }
            }
          case _ => super.transform(tree)
        }
      }

      private def addToBlock(block: global.Block, toAdd: global.Tree) = {
        null
      }

      /**
       * Here we extend a <code>Block</code> with a plus (+) method, which adds a new statement to the list of
       * statements in the block. The result is a new block.
       * @param block The block to extend.
       */
      private implicit def extendBlockWithPlus(block: global.Block) = new {
        def +(toAdd: global.Tree) = {
          new global.Block(toAdd :: block.stats, block.expr)
        }
      }

      private def createNonnullCheck(param: global.ValDef) = {
        // Getting the name of the parameter

        /*
          We need to generate:
          if (param == null) throw new IllegalArgumentException("Parameter '#{param.name}' should not be null.")
         */
        global.If(
          // if: param == ...
          global.Apply(
            global.Select(
              global.Ident(param.name),
              global.newTermName("$eq$eq")),
            List(global.Literal(global.Constant(null)))
            ),
          // then: throw ...
          global.Throw(
            global.Apply(
              global.Select(
                global.New(global.Ident(global.newTypeName("IllegalArgumentException"))),
                global.newTermName("<init>")),
              List(global.Literal(global.Constant("Parameter '%s' should not be null.".format(param.name)))))),
          // else
          global.EmptyTree)
      }

      private def findNonnullParameters(vparamss: List[List[global.ValDef]]) = {
        vparamss.flatten[global.ValDef].filter(param => {
          // Checking if the parameter contains a non-null annotation
          param.mods.annotations.exists(annotation => {
            // Checking if the annotation is a non-null annotation
            annotation.constr.find(tree => {
              tree match {
                case global.Ident(name) => "No[nt][Nn]ull".r.findFirstIn(name.toString).isDefined
                case _ => false
              }
            }).isDefined
          })
        })
      }
    }
  }
}