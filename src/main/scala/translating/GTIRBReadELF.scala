package translating

import com.google.protobuf.ByteString
import com.grammatech.gtirb.proto.CFG.EdgeType.*
import com.grammatech.gtirb.proto.CFG.CFG
import com.grammatech.gtirb.proto.CFG.Edge
import com.grammatech.gtirb.proto.CFG.EdgeLabel
import com.grammatech.gtirb.proto.Module.Module
import com.grammatech.gtirb.proto.Symbol.Symbol

object GTIRBReadELF {

  def getExternalFunctions(mod: Module) = {

    val proxyBlockUuids = mod.proxies.map(_.uuid).toSet
    val externalFunctionSymbols = mod.symbols.filter(x => proxyBlockUuids.contains(x.getReferentUuid))

  }
}
