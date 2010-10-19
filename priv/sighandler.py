
import signal
import time

def register():
    signal.signal(signal.SIGWINCH, handler)

def handler(signum, frame):
    print 'Signal handler called with signal', signum
    register()
    time.sleep(50000)

register()

time.sleep(50000)

