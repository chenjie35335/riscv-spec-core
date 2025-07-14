package rvspeccore.checker

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils
import rvspeccore.core.{Internal, PrivilegedState, RVConfig, State}
import rvspeccore.core.spec.instset.csr.CSR
import rvspeccore.core.spec.instset.csr.EventSig
import rvspeccore.core.tool.TLBSig

abstract class ConnectHelper {}

trait UniqueId {
  val uniqueIdReg: String   = "UniqueIdReg"
  val uniqueIdMem: String   = "UniqueIdMem"
  val uniqueIdCSR: String   = "UniqueIdCSR"
  val uniqueIdEvent: String = "UniqueIdEvent"
  val uniqueIdDTLB: String  = "UniqueIdDTLB"
  val uniqueIdITLB: String  = "UniqueIdITLB"
  val uniqueIdCSRNext: String = "UniqueIdCSRNext"
  val uniqueIdPrivilegeMode: String = "UniqueIdPrivilegeMode"
  val uniqueIdPrivilegeModeNext: String = "UniqueIdePrivilegeModeNext"
}

/** Connect RegFile to io.result.reg by BoringUtils
  */
object ConnectCheckerResult extends ConnectHelper with UniqueId {

  def setRegSource(regVec: Vec[UInt]) = {
    BoringUtils.addSource(regVec, uniqueIdReg)
  }
  class MemOneSig()(implicit XLEN: Int) extends Bundle {
    val valid    = Bool()
    val addr     = UInt(XLEN.W)
    val memWidth = UInt(log2Ceil(XLEN + 1).W)
    val data     = UInt(XLEN.W)
  }
  class MemSig()(implicit XLEN: Int) extends Bundle {
    val read  = new MemOneSig
    val write = new MemOneSig
  }
  def makeTLBSource(isDTLB: Boolean)(implicit XLEN: Int): TLBSig = {
    val tlbmem = WireInit(0.U.asTypeOf(new TLBSig))
    BoringUtils.addSource(tlbmem, if (isDTLB) uniqueIdDTLB else uniqueIdITLB)
    tlbmem
  }
  def makeMemSource()(implicit XLEN: Int) = {
    val mem = Wire(new MemSig)

    mem.read.valid     := false.B
    mem.read.addr      := 0.U
    mem.read.data      := 0.U
    mem.read.memWidth  := 0.U
    mem.write.valid    := false.B
    mem.write.addr     := 0.U
    mem.write.data     := 0.U
    mem.write.memWidth := 0.U

    BoringUtils.addSource(mem, uniqueIdMem)

    mem
  }
  def regNextDelay[T <: Bundle](signal: T, delay: Int): T = {
    delay match {
      case 0 => signal
      case _ => regNextDelay(RegNext(signal), delay - 1)
    }
  }

  def makeCSRSource()(implicit XLEN: Int, config: RVConfig): CSR = {
    val csr = Wire(CSR())
    csr := DontCare
    BoringUtils.addSource(csr, uniqueIdCSR)
    csr
  }

  def makeEventSource()(implicit XLEN: Int, config: RVConfig): EventSig = {
    val event = Wire(new EventSig())
    event := DontCare
    BoringUtils.addSource(event, uniqueIdEvent)
    event
  }

  def setChecker(
      checker: CheckerWithResult,
      memDelay: Int = 0
  )(implicit XLEN: Int, config: RVConfig) = {
    // reg
    if (config.formal.arbitraryRegFile) ArbitraryRegFile.init

    val regVec = Wire(Vec(32, UInt(XLEN.W)))
    regVec := DontCare
    BoringUtils.addSink(regVec, uniqueIdReg)

    checker.io.result.reg := regVec
    checker.io.result.pc  := DontCare

    checker.io.result.privilege.internal := DontCare

    if (checker.checkMem) {
      val mem = Wire(new MemSig)
      mem := DontCare
      BoringUtils.addSink(mem, uniqueIdMem)
      checker.io.mem.get := regNextDelay(mem, memDelay)
      if (config.functions.tlb) {
        val dtlbmem = Wire(new TLBSig)
        val itlbmem = Wire(new TLBSig)
        dtlbmem := DontCare
        itlbmem := DontCare
        BoringUtils.addSink(dtlbmem, uniqueIdDTLB)
        BoringUtils.addSink(itlbmem, uniqueIdITLB)
        checker.io.dtlbmem.get := dtlbmem
        checker.io.itlbmem.get := itlbmem
        // expose the signal below
        // assert(RegNext(checker.io.dtlbmem.get.read.valid, false.B) === false.B)
        // assert(RegNext(dtlbmem.read.valid, false.B) === false.B)
        // assert(RegNext(dtlbmem.read.addr, 0.U) === 0.U)
        // assert(RegNext(dtlbmem.read.data, 0.U) === 0.U)
      }
    }
    // csr
    val csr = Wire(CSR())
    csr := DontCare
    BoringUtils.addSink(csr, uniqueIdCSR)
    checker.io.result.privilege.csr := csr

    val event = Wire(new EventSig())
    event := DontCare
    BoringUtils.addSink(event, uniqueIdEvent)
    checker.io.event := event
  }
}

object ConnectCheckerWb extends ConnectHelper with UniqueId {
  def regNextDelay[T <: Bundle](signal: T, delay: Int): T = {
    delay match {
      case 0 => signal
      case _ => regNextDelay(RegNext(signal), delay - 1)
    }
  }

  class MemOneSig()(implicit XLEN: Int) extends Bundle {
    val valid    = Bool()
    val addr     = UInt(XLEN.W)
    val memWidth = UInt(log2Ceil(XLEN + 1).W)
    val data     = UInt(XLEN.W)
  }

  class MemSig()(implicit XLEN: Int) extends Bundle {
    val read  = new MemOneSig
    val write = new MemOneSig
  }

  def makeMemSource()(implicit XLEN: Int) = {
    val mem = Wire(new MemSig)

    mem.read.valid     := false.B
    mem.read.addr      := 0.U
    mem.read.data      := 0.U
    mem.read.memWidth  := 0.U
    mem.write.valid    := false.B
    mem.write.addr     := 0.U
    mem.write.data     := 0.U
    mem.write.memWidth := 0.U

    BoringUtils.addSource(mem, uniqueIdMem)

    mem
  }

  def makeCSRSource()(implicit XLEN: Int, config: RVConfig): CSR = {
    val csr = CSR.wireInit()
    BoringUtils.addSource(csr, uniqueIdCSR)
    csr
  }

  def makeModeSource()(implicit XLEN: Int, config: RVConfig): UInt = {
    val privilegeMode = Internal.wireInit().privilegeMode
    BoringUtils.addSource(privilegeMode, uniqueIdPrivilegeMode)
    privilegeMode
  }

  def makeModeNextSource()(implicit XLEN: Int, config: RVConfig): UInt = {
    val privilegeMode = Internal.wireInit().privilegeMode
    BoringUtils.addSource(privilegeMode, uniqueIdPrivilegeModeNext)
    privilegeMode
  }

  def makeCSRNextSource()(implicit XLEN: Int, config: RVConfig): CSR = {
    val csr = CSR.wireInit()
    BoringUtils.addSource(csr, uniqueIdCSRNext)
    csr
  }

  def makeEventSource()(implicit XLEN: Int, config: RVConfig): EventSig = {
    val event = Wire(new EventSig())
    event := DontCare
    BoringUtils.addSource(event, uniqueIdEvent)
    event
  }

  def setChecker(
      checker: CheckerWithWB,
      memDelay: Int = 0
  )(implicit XLEN: Int, config: RVConfig) = {
    // init
    val privilege = PrivilegedState.wireInit()
    val privilegeNext = PrivilegedState.wireInit()
    checker.io.privilegeNext := privilegeNext
    checker.io.privilege := privilege

    // mem
    val mem = Wire(new MemSig)
    mem := DontCare
    BoringUtils.addSink(mem, uniqueIdMem)
    if (checker.checkMem) {
      checker.io.mem.get := regNextDelay(mem, memDelay)
//      if (config.functions.tlb) {
//        val dtlbmem = Wire(new TLBSig)
//        val itlbmem = Wire(new TLBSig)
//        dtlbmem := DontCare
//        itlbmem := DontCare
//        BoringUtils.addSink(dtlbmem, uniqueIdDTLB)
//        BoringUtils.addSink(itlbmem, uniqueIdITLB)
//        checker.io.dtlbmem.get := dtlbmem
//        checker.io.itlbmem.get := itlbmem
//        // expose the signal below
//        // assert(RegNext(checker.io.dtlbmem.get.read.valid, false.B) === false.B)
//        // assert(RegNext(dtlbmem.read.valid, false.B) === false.B)
//        // assert(RegNext(dtlbmem.read.addr, 0.U) === 0.U)
//        // assert(RegNext(dtlbmem.read.data, 0.U) === 0.U)
//      }
    }
    // csr
    val csr = Wire(CSR())
    csr := DontCare
    BoringUtils.addSink(csr, uniqueIdCSR)
    checker.io.privilege.csr := csr
    // csrNext
    val csrNext = Wire(CSR())
    csrNext := DontCare
    BoringUtils.addSink(csrNext, uniqueIdCSRNext)
    checker.io.privilegeNext.csr := csrNext
    // privilegeMode
    val privilegeMode = Wire(UInt(2.W))
    privilegeMode := Internal.wireInit().privilegeMode
    BoringUtils.addSink(privilegeMode, uniqueIdPrivilegeMode)
    checker.io.privilege.internal.privilegeMode := privilegeMode
    // next privilegeMode
    val privilegeModeNext = Wire(UInt(2.W))
    privilegeModeNext := Internal.wireInit().privilegeMode
    BoringUtils.addSink(privilegeMode, uniqueIdPrivilegeModeNext)
    checker.io.privilegeNext.internal.privilegeMode := privilegeMode
    // exception events
    val event = Wire(new EventSig())
    event := DontCare
    BoringUtils.addSink(event, uniqueIdEvent)
    checker.io.event := event
  }
}
