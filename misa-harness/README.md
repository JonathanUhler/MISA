# MISA Peripheral Harness

`misa-harness` provides a common API for memory-mapped I/O peripherals shared by two deployments.
The first is the reference simulator in `misa-sim`. The second is future hobby-tapeout silicon,
where the full 64K memory is emulated on a laptop connected to the chip through a pin shim.

## Components

- `constants.py`: data widths, masks, and MMIO base addresses.
- `clock.py`: `Clock`, `VirtualClock` for the simulator, `RealClock` for hardware.
- `bus.py`: `MemoryBus`, address decode and dispatch over the full 64K space.
- `ram.py`: `Ram`, default read/write consumer for addresses that are not mapped by a peripheral.
- `peripheral.py`: the `Peripheral` base class and little-endian helpers.
- `interrupt.py`: `InterruptSink` and `InterruptController`.
- `uart.py`, `timer.py`: reference peripherals.
- `machine.py`: `Machine`, holds the bus, RAM, clock, and peripherals.

## Deployments

The only pieces that differ between the simulator and hardware are the bus master and the concrete
clock. In the simulator, the Python CPU drives the bus and uses a `VirtualClock`. On hardware, a
host uses a `Machine(RealClock())`, translates chip pin transactions into `bus.read` and `bus.write`
calls, calls `machine.poll()` each loop, and connects `machine.irq.pending()` to the physical
interrupt pin. Peripherals stay single-threaded. The host serializes asynchronous external events
onto its loop thread.
