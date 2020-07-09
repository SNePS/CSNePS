package csneps.util;

import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.AbstractQueuedSynchronizer;

public class CountingLatch
{

  /**
   * Synchronization control for CountingLatch.
   * Uses AQS state to represent count.
   */
  private static final class Sync extends AbstractQueuedSynchronizer
  {
    private Sync()
    {
    }

    private Sync(final int initialState)
    {
      setState(initialState);
    }

    int getCount()
    {
      return getState();
    }

    protected int tryAcquireShared(final int acquires)
    {
      return getState()==0 ? 1 : -1;
    }

    protected boolean tryReleaseShared(final int delta)
    {
      // Decrement count; signal when transition to zero
      for(; ; ){
        final int
                c=getState(),
                nextc=c+delta;
        if(compareAndSetState(c,nextc)){
          return nextc==0;
        }
      }
    }
  }

  private final Sync sync;

  public CountingLatch()
  {
    sync=new Sync();
  }

  public CountingLatch(final int initialCount)
  {
    sync=new Sync(initialCount);
  }

  public void increment()
  {
    sync.releaseShared(1);
  }

  public int getCount()
  {
    return sync.getCount();
  }

  public void decrement()
  {
    sync.releaseShared(-1);
  }

  public void await() throws InterruptedException
  {
    sync.acquireSharedInterruptibly(1);
  }

  public boolean await(final long timeout) throws InterruptedException
  {
    return sync.tryAcquireSharedNanos(1,TimeUnit.MILLISECONDS.toNanos(timeout));
  }
  
  public String toString(){
	  return Integer.toString(this.getCount());
  }

}