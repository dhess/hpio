{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-binds #-}

module System.GPIO.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Terminology and types
    --
    -- Let's define some terms that will be used throughout this tutorial.
    -- $pin
      Pin(..)
    -- $pin_direction
    , PinDirection(..)
    -- $pin_value
    , PinValue (..)
    -- $pin_read_trigger
    , PinReadTrigger(..)

    -- * Interpreters
    -- $interpreters

    -- * A mock interpreter
    -- $mock_interpreter
    , runTutorial

    -- * Basic pin operations
    -- $basic_pin_operations

    -- * Copyright
    -- $copyright
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import System.GPIO.Monad (MonadGpio(..), withPin)
import System.GPIO.Linux.Sysfs.Mock
       (MockGpioChip(..), MockPinState(..), SysfsGpioMockIO,
        defaultMockPinState, initialMockWorld, evalSysfsGpioMockIO)
import System.GPIO.Linux.Sysfs.Types (SysfsException(..))
import System.GPIO.Types (Pin(..), PinDirection(..), PinValue(..), PinReadTrigger(..))

-- $setup
-- >>> import System.GPIO.Monad

{- $introduction

The @gpio@ package is a collection of monads for writing GPIO programs
directly in Haskell.

Though Haskell is a much more capable programming language than, say,
<http://wiring.org.co Wiring>, this power comes with a few trade-offs.
Whereas a program written in Wiring (or even C) can run directly on a
low-cost microcontroller, a program written in Haskell cannot.
Therefore, @gpio@ is intended for use with more powerful GPIO-capable
platforms, such as the <https://www.raspberrypi.org Raspberry Pi platform>,
or the <http://beagleboard.org Beagle platform>, which
marry a 32- or 64-bit CPU core with GPIO functionality.

For each supported GPIO platform, @gpio@ provides two contexts for
writing GPIO programs: a cross-platform domain-specific language
(DSL), and a platform-specific DSL. Programs written in the
cross-platform DSL will run on any @gpio@-supported platform, but as
the cross-platform DSL must take a "least-common denominator"
approach, cross-platform programs may not be capable of taking
advantage of all of the features of a particular GPIO platform. On the
other hand, programs written for a platform-specific DSL can use all
of those platform-specific features, but will not work on other GPIO
platforms.

(Currently, this is not really an issue, as @gpio@ only supports the
Linux @sysfs@ GPIO platform, and the cross-platform DSL has been
modeled on that platform's capabilities. However, future support for
other Unix-based GPIO platforms is planned.)

Primarily, this tutorial focuses on the cross-platform DSL.

-}

{- $pin

== GPIO

/General-purpose input\/output/. A GPIO /pin/ is a user-programmable,
serial (i.e., a single-bit wide) interface from the system to the
external world. GPIO pins can usually be configured either for input
(for reading external signals) or for output (for driving signals to
external devices), though sometimes a pin may be hard-wired to one
direction or the other.

Some platforms may reserve one or more GPIO pins for their own use,
e.g., to drive an external storage interface. Typically these pins are
not visible to the user and therefore cannot be programmed by @gpio@,
but you should always consult your hardware documentation to make sure
you don't accidentally use a system-reserved pin.

GPIO pins are often physically expressed on a circuit board as a male
or female <https://www.google.com/#q=gpio+pin+header breakout header>, which
is a bank of pins (male) or sockets (female) for hooking up to
individual wires or low-density molded connectors. However, on
platforms with a large number of GPIO pins, it is typically the case
that just a handful of pins are accessible via such a header, while
the rest are only accessible via a high-density connector, intended
for use by high-volume system integrators with custom hardware
designs.

== Pin number

GPIO pins are typically identified by their /pin number/.
Unfortunately, it is often the case that the pin number used in the
system's hardware documentation is different than the pin number used
by the software to identify the same pin.

In @gpio@, a pin's number refers to the number used by the system
software to identify the pin. Consult your hardware documentation (or
Google) for the hardware-to-software pin mapping.

@gpio@ uses the 'Pin' type to identify GPIO pins.

-}

{- $pin_direction

== Pin direction

We say a pin's /direction/ is either /in/ (for input) or /out/ (for
output).

On some platforms, pins may be configured for other states which
describe a specific type of input or output (e.g., open-drain); or for
a third, high-impedance state. However, as they are so
platform-specific, these configurations are not supported by the
cross-platform DSL.

In @gpio@, the 'PinDirection' type represents a pin's direction.

-}

{- $pin_value

== Pin (signal) value

In digital design, the flavor of logic design used with GPIO pins (as
opposed to analog design), a pin's /value/ (sometimes called its
/signal level/) is either /high/ or /low/. When we say that a pin's
value or signal level is /high/, we mean the general notion of the pin
being "on" or /active/; and when we say the pin's value or signal
level /low/, we mean the pin is "off" or /inactive/.

Complicating matters is the concept of /active-low/ logic. Digital
electronic components are built using either positive (/active-high/)
logic, or negative (/active-low/) logic. In active-high logic, a pin
is active when the voltage on the pin is high (relative to ground);
whereas in active-low logic, a pin is active when the voltage on the
pin is low (or grounded).

When designing logic, or programs to interface with logic, it's often
easier to think of a signal as being active or inactive, rather than
worrying about its physical voltage. Therefore, the @gpio@
cross-platform DSL supports, on a pin-by-pin basis, both types of
logic: active-high and active-low. When writing your programs, you can
simply use the values @High@ and @Low@, and then set a per-pin active
level before running your program, depending on whether you're
interfacing with active-high or active-low logic.

In the @gpio@ documentation, and in this tutorial, whenever you see a
reference to a "pin value" or "signal level," unless otherwise noted,
we mean the abstract notion of the pin being "on" or "off,"
independent of the voltage level seen on the physical pin. We refer to
this notion as the pin's (or signal's) /logical value/, as opposed to
its /physical value/.

In @gpio@, the 'PinValue' type represents a pin's value.

-}

{- $pin_read_trigger

== Pin triggers (interrupts)

In logic programming, it's often useful to block the program's
execution on an input pin until its value changes. Furthermore, you
may want to wait for a particular change: when the signal transitions
from low to high (its /rising edge/), or from high to low (its
/falling edge/).

The @gpio@ cross-platform DSL supports this functionality, allowing
you to block the current Haskell thread on a GPIO input pin until its
rising edge, falling edge, or either edge (/level-triggered/). We call
this the pin's /read trigger/, but it may be helpful to think of it as
an interrupt.

If you want to mask interrupts for some period of time without needing
to stop and re-start the blocking thread, you can also disable the
trigger entirely from another thread.

Some pins (and indeed, some platforms) may not support this
functionality, but the cross-platform DSL provides a mechanism to
query a pin to see whether it's supported.

In @gpio@, the 'PinReadTrigger' type represents the type of event to
wait for.

-}

{- $interpreters

The @gpio@ cross-platform DSL is defined by the 'MonadGpio' type
class. Each method of the 'MonadGpio' type class describes an action
that can be performed on a GPIO pin (or on the GPIO system as a
whole).

For each supported platform, @gpio@ provides an instance of the
'MonadGpio' type class. The platform-specific instance maps actions in
the cross-platform DSL to actions on that particular GPIO platform.
You can therefore think of each 'MonadGpio' instance as a
platform-specific interpreter for the cross-platform DSL. Each
interpreter provides a "run" action which, given a 'MonadGpio'
program, will execute the program on its GPIO platform.

== @mtl@ compatibility

Each @gpio@ interpreter is implemented as a monad transformer, and
each is also an instance of the monad type classes defined in the
<https://hackage.haskell.org/package/mtl mtl> package, so long as its
implementation satisfies the laws of that particular @mtl@ type class.
This makes it easy to integrate @gpio@ interpreters into @mtl@-style
monad transformer stacks.

Additionally, the 'MonadGpio' type class provides instances of itself
for all the @mtl@ monad type classes for which it can satisfy the
laws, meaning that you don't need to 'lift' 'MonadGpio' operations out
of these monads manually.

-}

{- $mock_interpreter

Testing GPIO programs is inconvenient. The target system is often
under-powered compared to our development environment, and is probably
a completely different architecture; cross-compiling Haskell programs
is, circa 2016, still somewhat problematic. It's also not uncommon for
our development environments not to have any GPIO capabilities at all.

For your convenience, @gpio@ provides a reasonably complete, entirely
software-based "mock" GPIO implementation that can run on any system
where Haskell programs can run, irrespective of that system's GPIO
capabilities or operating system. This particular implementation mocks
the Linux @sysfs@ GPIO filesystem and is capable of emulating much of
that platform's functionality. (The most significant unimplemented
functionality is the ability to wait for input pin read triggers.)

In this tutorial, we will make use of this mock GPIO implementation in
many of the code examples, meaning that those examples can be run on
any Haskell-capable system. In a few cases, we'll discuss
functionality that the mock implementation does not handle. These
cases will be called out.

Let's start by briefly describing how this mock interpreter is
implemented. You don't need to know this in order to use @gpio@, and
you quite possibly will never use the mock interpreter outside of this
tutorial anyway, but understanding how it's implemented helps explain
why using the Linux @sysfs@ GPIO interpreter is a bit complicated.

In Linux @sysfs@ GPIO, userspace GPIO operations are performed on
virtual files in the @sysfs@ filesystem. See the
<https://www.kernel.org/doc/Documentation/gpio/sysfs.txt Linux kernel documentation>
for details, but in a nutshell:

* Pins are /exported/ (akin to opening a file) by writing their pin
number to the @\/sys\/class\/gpio\/export@ file.

* Once a pin is exported, the Linux kernel creates a subdirectory for
that pin number (e.g., @\/sys\/class\/gpio\/gpio7@), along with several
pseudo-files, called /attributes/, for controlling the pin's
direction, reading and writing its pin value, etc.

* Pins are /unexported/ (akin to closing a file) by writing their pin
number to the @\/sys\/class\/gpio\/unexport@ file. When the pin is
unexported, the kernel removes the pin's @sysfs@ subdirectory.

The @gpio@ interpreter for the Linux @sysfs@ GPIO system translates
actions in the cross-platform DSL to @sysfs@ filesystem operations.
The most straightforward way to implement this interpreter is to use
filesystem actions such as 'readFile' and 'writeFile' directly.
However, by adding a level of abstraction at the filesystem layer, we
can substitute a @sysfs@ filesystem emulator for the real thing, and
the interpreter's none the wiser. Because we're only implementing the
subset of filesystem functionality required by the Linux @sysfs@ GPIO
interpreter (and certainly not an entire real filesystem!), there are
only a handful of actions we need to emulate.

So that is the approach used by @gpio@'s @sysfs@ interprefer. It
breaks the Linux @sysfs@ GPIO interpreter into two pieces: a
high-level piece which maps cross-platform GPIO operations to abstract
filesystem actions, and a low-level piece which implements those
filesystem actions. It then provides two low-level implementations:
one which maps the abstract filesystem actions onto real filesystem
operations, and one which implements a subset of the @sysfs@
filesystem as an in-memory "filesystem" for mocking the Linux kernel's
@sysfs@ GPIO behavior.

To use this implementation, you don't need to worry about these
details; you just need to know how to compose the two interpreters. If
you want to run real GPIO programs on a real Linux GPIO-capable
system, the composition is relatively straightforward. Assuming that
@action@ is your program:

> runSysfsIOT $ runSysfsGpioT action

Here the 'System.GPIO.Linux.Sysfs.runSysfsGpioT' interpreter
translates GPIO actions in @action@ to abstract @sysfs@ filesystem
operations, and the 'System.GPIO.Linux.Sysfs.runSysfsIOT' interpreter
translates abstract @sysfs@ filesystem operations to their native
filesystem equivalents.

(Note that if @action@ runs directly in 'IO' and not in a transformer
stack, then you can use the 'System.GPIO.Linux.Sysfs.runSysfsGpioIO'
action, which conveniently composes these two interpreters for you.)

On the other hand, if you want to run GPIO programs in a mock Linux
@sysfs@ GPIO environment, you need to do a bit more work, as you need
to provide the description of the mock environment itself: which pins
are available, how they're numbered, their initial state, etc. For
this purpose, @gpio@ provides the "System.GPIO.Linux.Sysfs.Mock"
module, which defines a number of mock types. See the module
documentation for details on how to configure a mock GPIO system, but
for the purposes of this tutorial, we provide a 'runTutorial' wrapper
which composes the two @sysfs@ GPIO interpreters and takes care of all
of the mock setup for you. It defines a mock GPIO system with 17 pins,
numbered 0 through 16. Pins 0-15 are initially configured in their
default mock state (see
'System.GPIO.Linux.Sysfs.Mock.defaultMockPinState'). Pin 16 is special
so that we can demonstrate some exceptional cases.

__Note__: in our examples, each time we use 'runTutorial' we are
creating a new mock environment from scratch, so any changes made to
the mock environment are not persistent from one example to the next.

-}

-- | Run a @gpio@ program on a mock system with 16 GPIO pins.
runTutorial :: SysfsGpioMockIO a -> IO a
runTutorial program =
  let chip0 :: MockGpioChip
      chip0 = MockGpioChip "chip0" 0 (replicate 16 defaultMockPinState)
      chip1 :: MockGpioChip
      chip1 = MockGpioChip "chip1" 16 [defaultMockPinState {_direction = In, _userVisibleDirection = False, _edge = Nothing}]
  in
    evalSysfsGpioMockIO program initialMockWorld [chip0, chip1]

{- $basic_pin_operations

== Which pins are available?

To get the list of all pins available on the system, use the 'pins' command:

>>> runTutorial pins
[Pin 0,Pin 1,Pin 2,Pin 3,Pin 4,Pin 5,Pin 6,Pin 7,Pin 8,Pin 9,Pin 10,Pin 11,Pin 12,Pin 13,Pin 14,Pin 15,Pin 16]

== Pin resource management

Before you can operate on a GPIO pin, you must signal your intention
to the system by /opening/ it. Opening the pin returns a /handle/,
which you then use to operate on that pin. Then, when you're finished
with a GPIO pin, you should allow the system to clean up any
pin-related resources by /closing/ it.

(Note that, because our interpreter is an instance of 'MonadIO', we
can interleave 'IO' actions into our GPIO computations.)

>>> :{
runTutorial $
  do h <- openPin (Pin 5)
     liftIO $ putStrLn "Opened pin 5"
     closePin h
     liftIO $ putStrLn "Closed pin 5"
:}
Opened pin 5
Closed pin 5

As with file handles, when an exception occurs in a computation, we
should clean up any open pin handles. We could wrap each 'openPin' /
'closePin' pair with 'Control.Monad.Catch.bracket', or we could just
use the provided 'withPin' wrapper, which does this for us:

>>> :{
runTutorial $
  withPin (Pin 5) $ \h ->
    do liftIO $ putStrLn "Opened pin 5"
       fail "Oops"
:}
Opened pin 5
*** Exception: user error (Oops)

Using 'withPin' is good hygiene, so we'll use it throughout this
tutorial.

== Pin configuration

Every pin has an active level:

>>> runTutorial $ withPin (Pin 8) getPinActiveLevel
High

You can change it:

>>> :{
runTutorial $
  withPin (Pin 5) $ \h ->
    do setPinActiveLevel h Low
       getPinActiveLevel h
:}
Low

or toggle it:

>>> runTutorial $ withPin (Pin 8) togglePinActiveLevel
Low

While all GPIO pins by definition have a direction (/in/ or /out/), on
some platforms you may not be able to modify, or even query, a
particular pin's direction in a portable way. Therefore, when querying
a pin's direction using the cross-platform DSL, the returned value is
wrapped in a 'Maybe':

>>> runTutorial $ withPin (Pin 10) getPinDirection
Just Out

>>> runTutorial $ withPin (Pin 16) getPinDirection -- Pin 16's direction is not settable
Nothing

If 'getPinDirection' returns 'Nothing', as it does for 'Pin' @16@ in
our example, then the pin's direction is not settable, and you'll need
another (platform-specific) method for determining its hard-wired
value. Conversely, if 'getPinDirection' returns a 'Just', the pin's
direction is configurable via the 'setPinDirection' action:

>>> :{
runTutorial $
  withPin (Pin 5) $ \h ->
    do setPinDirection h In
       getPinDirection h
:}
Just In

You can also toggle it:

>>> :{
runTutorial $
  withPin (Pin 5) togglePinDirection
:}
Just In

Obviously, it's an error to try to set the direction of a pin whose
direction is not settable:

>>> :{
-- Pin 16's direction is not settable
runTutorial $
  withPin (Pin 16) $ \h ->
    do setPinDirection h In
       getPinDirection h
:}
*** Exception: NoDirectionAttribute (Pin 16)

However, 'togglePinDirection' signals the error by returning 'Nothing':

>>> :{
runTutorial $
  withPin (Pin 16) togglePinDirection
:}
Nothing

(This is a good time to mention exceptions. Like any other 'IO'
computation, GPIO computations throw exceptions when an error occurs.
The @gpio@ exception system is hierarchical; the top level exception
type is 'System.GPIO.Types.SomeGpioException'. Each GPIO platform
interpreter then defines its own exception type whose set of values
are specific to the errors that can occur on that particular platform.
Because we're using the mock Linux @sysfs@ GPIO interpreter in this
tutorial, all of the exceptions shown here are of type
'SysfsException'.)

Finally, some pins, /when configured for input/, may support edge- or
level-triggered interrupts. As with the pin's direction, you can
discover whether a pin supports this functionality by asking for its
read trigger value, though this action is generally only valid when
the pin is configured for input:

 >>> :{
 runTutorial $
   withPin (Pin 5) $ \h ->
     do setPinDirection h In
        getPinReadTrigger h
 :}
 Just Disabled

  >>> runTutorial $ withPin (Pin 16) $ getPinReadTrigger -- Note: Pin 16's direction is hard-wired to 'In'
  Nothing

If 'getPinReadTrigger' returns 'Nothing', as it does for 'Pin' @16@ in
our example, then the pin does not support interrupts.

You might be wondering, what is the difference between 'Just'
'Disabled' and 'Nothing'? As explained above, 'Nothing' means the pin
does not support interrupts at all, whereas a read trigger value of
'Disabled' means that the pin supports interrupts, but that they're
currently disabled.

If the pin supports interrupts, you can change its read trigger. In
this example, we configure 'Pin' @5@ for level-triggered interrupts.
Note that we must configure the pin for input before we do so:

 >>> :{
 runTutorial $
   withPin (Pin 5) $ \h ->
     do setPinDirection h In
        setPinReadTrigger h Level
        getPinReadTrigger h
 :}
 Just Level

If the pin does not support interrupts, or if the pin is configured
for output, it is an error to attempt to set its read trigger:

  >>> :{
  -- Here we have tried to set an output pin's read trigger
  runTutorial $
    withPin (Pin 5) $ \h ->
      do setPinDirection h Out
         setPinReadTrigger h Level
         getPinReadTrigger h
  :}
  *** Exception: InvalidOperation (Pin 5)

   >>> :{
   -- Pin 16 does not support interrupts
   runTutorial $
     withPin (Pin 16) $ \h ->
       do setPinReadTrigger h Level
          getPinReadTrigger h
   :}
   *** Exception: NoEdgeAttribute (Pin 16)

Note that the exception value thrown in each case is different, to
better help you identify what you did wrong. (The 'NoEdgeAttribute'
exception value refers to the Linux @sysfs@ GPIO per-pin @edge@
attribute, which is used to configure the pin's read trigger.)

See below for examples of how to make use of pin interrupts and a
pin's read trigger setting.

-}

{- $copyright

This tutorial is copyright Drew Hess, 2016, and is licensed under the
<http://creativecommons.org/licenses/by/4.0/ Creative Commons Attribution 4.0 International License>.

-}
