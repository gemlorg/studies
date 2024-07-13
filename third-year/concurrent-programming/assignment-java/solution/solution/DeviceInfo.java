package cp2023.solution;

import cp2023.base.ComponentId;
import cp2023.base.DeviceId;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class DeviceInfo {
  private Integer freeSpace;
  private final Map<ComponentId, ComponentId> cycledComponents;
  private final Set<ComponentId> noUseComponents;
  private final ReentrantReadWriteLock lock;
  private final  LinkedList<ComponentId> leavingComponents = new LinkedList<>();
  private final LinkedList<Map.Entry<ComponentId, Semaphore>> reservingComponents = new LinkedList<>();
  private final Map<ComponentId, Semaphore> enteringComponents = new ConcurrentHashMap<>();
  private final Map<ComponentId, ComponentId> pairs = new ConcurrentHashMap<>();
  private final Semaphore mutex = new Semaphore(1, true);

  public DeviceInfo(
      DeviceId id,
      Integer capacity,
      ReentrantReadWriteLock lock,
      Map<ComponentId, ComponentId> cycledComponents,
      Set<ComponentId> noUseComponents) {
    if (id == null
        || capacity == null
        || capacity <= 0
        || lock == null
        || cycledComponents == null
        || noUseComponents == null)
      throw new IllegalArgumentException("bad arguments for device info");

    this.freeSpace = capacity;
    this.cycledComponents = cycledComponents;
    this.noUseComponents = noUseComponents;
    this.lock = lock;
  }

  public void addInitialComponent(ComponentId id) {
    if (id == null) throw new IllegalArgumentException("null can't be a componentId");

    acquireMutex();

    if (freeSpace <= 0) throw new IllegalArgumentException("initial component can't be placed");

    freeSpace--;
    releaseMutex();
  }

  public void reserveComponent(ComponentId id) {
    Semaphore a = new Semaphore(1);
    Semaphore b = new Semaphore(1);
    boolean needWait = false;
    a.acquireUninterruptibly();
    b.acquireUninterruptibly();

    acquireMutex();
    lock.readLock().lock();

    enteringComponents.put(id, a);

    if (!cycledComponents.containsValue(id) && !pairs.containsValue(id)) {
      if (freeSpace > 0) {
        noUseComponents.add(id);
        a.release();
        freeSpace--;
      } else if (leavingComponents.isEmpty()) {
        //the component will be removed from reserving components by a thread that will wake him up
        reservingComponents.addLast(Map.entry(id, b));
        needWait = true;
      } else {
        ComponentId dependency = leavingComponents.removeFirst();
        pairs.put(dependency, id);
        noUseComponents.add(id);
      }
    }

    lock.readLock().unlock();
    releaseMutex();
    if(needWait)
      b.acquireUninterruptibly();
  }

  public void announceComponent(ComponentId id) {
    acquireMutex();
    lock.readLock().lock();

    if (cycledComponents.containsKey(id)) {
      ComponentId dependent = cycledComponents.get(id);
      pairs.put(id, dependent);
      cycledComponents.remove(id);
      for (Map.Entry<ComponentId, Semaphore> entry : reservingComponents) {
        if (entry.getKey().equals(dependent)) {
          noUseComponents.add(entry.getKey());
          entry.getValue().release();
          reservingComponents.remove(entry);
          break;
        }
      }
    } else
    if (reservingComponents.isEmpty()) {
      leavingComponents.add(id);
    } else {
      Map.Entry<ComponentId, Semaphore> dependant = reservingComponents.removeFirst();
      pairs.put(id, dependant.getKey());
      noUseComponents.add(dependant.getKey());
      dependant.getValue().release();
    }

    lock.readLock().unlock();
    releaseMutex();
  }

  public void addComponent(ComponentId id) {
    enteringComponents.get(id).acquireUninterruptibly();
  }

  public void removeComponent(ComponentId id) {
    acquireMutex();

    if (pairs.containsKey(id)) {
      ComponentId dependency = pairs.get(id);
      pairs.remove(id);
      enteringComponents.get(dependency).release();

    } else {
      freeSpace++;
      leavingComponents.remove(id);
    }

    releaseMutex();
  }

  private void acquireMutex() {
    try {
      mutex.acquire();
    } catch (InterruptedException e) {
      throw new RuntimeException("panic: unexpected thread interruption");
    }
  }

  private void releaseMutex() {
    mutex.release();
  }
}
