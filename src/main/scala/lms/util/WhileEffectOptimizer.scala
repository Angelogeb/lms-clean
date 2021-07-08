package lms.util

import lms.core.Backend.{Block, Const, EffectSummary, Node, Sym}
import lms.core.Graph

import scala.collection.mutable

/**
 * `optimize` breaks hard dependencies to `ein` symbols
 * caused by reads/writes for which the last write/definition is not in the same block
 * for While loops.
 *
 * Example:
 *   x12 = (var_get x3)  [x3 : _ | x10]
 *   ...
 *   x31 = (W Block(List(),x14,x10,[x2 x3 : _ | x10]) Block(List(),(),x15,[x2 x3 CTRL* x2* x9*: _ | x23, x27, x30]))  [x2 x3 CTRL* x2* x9*: _ | x0, x2, x3, x9]
 *
 *   In the snippet above the statement at node `x12` has a hard dependency
 *   to `x10` which is the `ein` of the conditional block of the while loop (statement x31).
 *   This hard dependency is inserted by `latest` which returns `curBlock` for reads
 *   for which the last write is not in the same block (the dependency system does not
 *   "reason" about the kind of nodes).
 *   However, from the reads and write keys of statement `x31` we know that x3 is only
 *   read (by both branches) and never written to. Therefore we can break this dependency.
 *
 * Problem:
 *   Are ein-deps caused induced only by these cases?
 *
 * Considerations:
 *   Currently the optimization is applied to only while loops because in these
 *   cases we would like to hoist things out of the block. For Ifs breaking dependencies
 *   is not beneficial since we usually push things inside and the dependency does no harm.
 */
object WhileEffectOptimizer {
  val ops = Set("W")

  def optimize(g: Graph): Graph = {
    val ein2node = mutable.Map[Sym, Node]()
    g.nodes.foreach {
      case n@Node(_, op, args, _) if ops contains op =>
        args.foreach {
          case Block(_, _, ein, _) => ein2node(ein) = n
          case _ => ()
        }
      case _ => ()
    }

    val optNodes: Seq[Node] = g.nodes.map {
      case node@Node(name, op, rhs, eff@EffectSummary(sdeps, hdeps, rkeys, wkeys))
        if eff.hdeps.nonEmpty && !(eff.wkeys contains Const("CTRL")) =>
        val einHDeps = eff.hdeps.filter(s => {
          ein2node.get(s) match {
            case Some(n) => ops contains n.op
            case _ => false
          }
        })

        if (einHDeps.isEmpty) {
          node
        } else {
          val safeToFilter = mutable.Set[Sym]()
          einHDeps.foreach(ein => {
            val block = ein2node(ein)
            block match {
              case Node(_,
              "W",
              Block(_, _, _, EffectSummary(_, _, crkeys, cwkeys)) ::
                Block(_, _, _, EffectSummary(_, _, brkeys, bwkeys)) :: Nil, _) =>
                val crbw = crkeys.intersect(bwkeys)
                val cwbr = cwkeys.intersect(brkeys)
                val cwbw = cwkeys.intersect(bwkeys)
                // read and write in same blocks
                val crcw = crkeys.intersect(cwkeys)
                val brbw = brkeys.intersect(bwkeys)

                if (rkeys.forall(key => !(crbw(key) || cwbr(key) || cwbw(key) || crcw(key) || brbw(key)))) {
                  safeToFilter.add(ein)
                }

              case _ => ()
            }
          })
          Node(name, op, rhs, EffectSummary(sdeps, hdeps.diff(safeToFilter), rkeys, wkeys))
        }
      case n => n
    }
    Graph(optNodes, g.block, g.globalDefsCache)
  }
}
