module BigGraph where

import Data.Maybe
import Graph
import BigTree

{-
 -
 - 63
 - |\
 - | \
 - |  \
 - |   \
 - |    \
 - |     \
 - |      \
 - |       \
 - |        \
 - |         \
 - |          \
 - |           \
 - |            \
 - |             \
 - |              \
 - |               \
 - |                \
 - |                 \
 - |                  \
 - |                   \
 - |                    \
 - |                     \
 - |                      \
 - |                       \
 - |                        \
 - |                         \
 - |                          \
 - |                           \
 - |                            \
 - |                             \
 - |                              \
 - |                               \
 - |                                \
 - |                                 \
 - |                                  \
 - |                                   \
 - |                                    \
 - |                                     \
 - |                                      \
 - |                                       \
 - |                                        \
 - |                                         \
 - |                                          \
 - |                                           \
 - |                                            \
 - |                                             \
 - |                                              \
 - v                                               v
 - 61                      100-------------------->62
 - |\                      /                       /\
 - | \                    /                       /. \
 - |  \                  /                       / .  \
 - |   \                /                       /  .   \
 - |    \              /                       /   .    \
 - |     \            /                       /    .     \
 - |      \          /                       /     .      \
 - |       \        /                       /      .       \
 - |        \      /                       /       .        \
 - |         \    /                       /        .         \
 - |          \  /                       /         .          \
 - |           \/                       /          .           \
 - |           /\                      /           .            \
 - |          /  \                    /            .             \
 - |         /    \                  /             .              \
 - |        /      \                /              .               \
 - |       /        \              /               .                \
 - |      /          \            /                .                 \
 - |     /            \          /                 .                  \
 - |    /              \        /                  .                   \
 - |   /                \      /                   .                    \
 - |  /                  \    /                    .                     \
 - | /                    \  /                     .                      \
 - vv                      vv                     .............            v
 - 57                      58                     .59 wuz here.            60
 - |\                      |\                     .............            |\
 - | \                     | \                                             | \
 - |  \                    |  \                                            |  \
 - |   \                   |   \                                           |   \
 - |    \                  |    \                                          |    \
 - |     \                 |     \                                         |     \
 - |      \                |      \                                        |      \
 - |       \               |       \                                       |       \
 - |        \              |        \                                      |        \
 - |         \             |         \                                     |         \
 - |          \            |          \                                    |          \
 - v           v           v           v                                   v           v
 - 49          50          51          52<----------------------\          55          56
 - |\          |\          |\          |\                        \         |\          |\
 - | \         | \         | \         | \                        \        | \         | \
 - |  \        |  \        |  \        |  \                        \       |  \        |  \
 - |   \       |   \       |   \       |   \                        \      |   \       |   \
 - |    \      |    \      |    \      |    \                        \     |    \      |    \
 - v     v     v     v     v     v     v     v                        \    v     v     v     v
 - 33    34    35    36    37    38    39    40                        \   45    46    47    48
 - |\    |\    |\    |\    |\    |\    |\    |\                         \  |\    |\    |\    |\
 - | \   | \   | \   | \   | \   | \   | \   | \                         \ | \   | \   | \   | \
 - v  v  v  v  v  v  v  v  v  v  v  v  v  v  v  v                         \v  v  v  v  v  v  v  v
 - 1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16                         25 26 27 28 29 30 31 32
 -
 -}



bigGraph =
    let
        n62p = head $ paths bigTree 62
        n59p = head $ paths bigTree 59
        n58p = head $ paths bigTree 58
        n57p = head $ paths bigTree 57
        n52p = head $ paths bigTree 52
        n25p = head $ paths bigTree 25

        n62 = fromJust $ followNode bigTree n62p
        n58 = fromJust $ followNode bigTree n58p
        n57 = fromJust $ followNode bigTree n57p
        n52 = fromJust $ followNode bigTree n52p
        n25 = fromJust $ followNode bigTree n25p

        g1 = replaceNode (const n58) bigTree n59p
        g2 = replaceNode (const n52) g1 (n25p ++ [GraphLeft])
        g3 = replaceNode (const n58) g2 n59p

        supplement = Node 100 n62 n57
    in FullGraph [supplement, g3]