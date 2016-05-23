/*
 * pollSysfs.c - a poll(2) wrapper for Linux sysfs GPIO.
 *
 * Using poll(2) to wait for GPIO interrupts in Linux sysfs is a bit
 * flaky:
 *
 * - On certain combinations of kernels+hardware, a "dummy read(2)" is
 *   needed before the poll(2) operation. As read(2) on a GPIO sysfs
 *   pin's "value" attribute doesn't block, it doesn't hurt to do this
 *   in all cases, anyway.
 *
 * - The Linux man page for poll(2) states that setting POLLERR in the
 *   'events' field is meaningless. However, the kernel GPIO
 *   documentation states: "If you use poll(2), set the events POLLPRI
 *   and POLLERR." Here we do what the kernel documentation says.
 *
 * - When poll(2) returns, an lseek(2) is needed before read(2), per
 *   the Linux kernel documentation.
 *
 * - It appears that poll(2) on the GPIO sysfs pin's 'value' attribute
 *   always returns POLLERR in 'revents', even if there is no error.
 *   (This is supposedly true for all sysfs files, not just for GPIO.)
 *   We simply ignore that bit and only consider the return value of
 *   poll(2) to determine whether an error has occurred. (Presumably,
 *   if POLLERR is set and poll(2) returns no error, then the
 *   subsequent lseek(2) or read(2) will fail.)
 *
 * This module wraps poll(2) for use with Linux sysfs files by
 * accounting for these quirks.
 *
 * Ref:
 * https://e2e.ti.com/support/dsp/davinci_digital_media_processors/f/716/t/182883
 * http://www.spinics.net/lists/linux-gpio/msg03848.html
 * https://www.kernel.org/doc/Documentation/gpio/sysfs.txt
 * http://stackoverflow.com/questions/16442935/why-doesnt-this-call-to-poll-block-correctly-on-a-sysfs-device-attribute-file
 * http://stackoverflow.com/questions/27411013/poll-returns-both-pollpri-pollerr 
 */

#include <errno.h>
#include <poll.h>
#include <stdint.h>
#include <unistd.h>

/*
 * Poll a sysfs file descriptor for an event.
 *
 * As this function was written for the Haskell C FFI, and standard
 * practice is for Haskell timeouts/delays to be specified in
 * microseconds, the 'timeout' parameter is specified in microseconds.
 * However, poll(2)'s timeout argument is specified in milliseconds.
 * This function converts the specified microsecond timeout to
 * milliseconds before calling poll(2), but keep in mind that its
 * precision is therefore only millisecond-accurate.
 *
 * As with poll(2), if 'timeout' is negative, then the timeout is
 * disabled.
 *
 * This function may block, so when calling it from Haskell, you
 * should use the interruptible variant of the C FFI. Therefore, the
 * function may return EINTR and you should be prepared to re-try it
 * in this case.
 */
int pollSysfs(int fd, int timeout)
{
    uint8_t dummy;
    if (read(fd, &dummy, 1) == -1) {
        return -1;
    }

    struct pollfd fds = { .fd = fd, .events = POLLPRI|POLLERR, .revents = 0 };

    int timeout_in_ms = (timeout > 0) ? (timeout / 1000) : timeout;

    int poll_result = poll(&fds, 1, timeout_in_ms);
    if (poll_result == -1)  {
        return -1;
    }
    if (lseek(fds.fd, 0, SEEK_SET) == -1) {
        return -1;
    }
    return poll_result;
}
