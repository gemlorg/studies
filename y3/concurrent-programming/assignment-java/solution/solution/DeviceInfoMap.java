package cp2023.solution;

import cp2023.base.ComponentId;
import cp2023.base.ComponentTransfer;
import cp2023.base.DeviceId;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class DeviceInfoMap extends ConcurrentHashMap<DeviceId, DeviceInfo> {
  private final Map<DeviceId, ArrayList<Map.Entry<DeviceId, ComponentId>>> moveRequests = new ConcurrentHashMap<>();
  private final Map<ComponentId, ComponentId> cycledComponents = new ConcurrentHashMap<>();
  private final ReentrantReadWriteLock lock = new ReentrantReadWriteLock(true);
  private final Set<ComponentId> noUseComponents = ConcurrentHashMap.newKeySet();

  @Override
  public DeviceInfo put(DeviceId id, DeviceInfo info) {
    moveRequests.put(id, new ArrayList<>());
    return super.put(id, info);
  }

  public void addDevice(DeviceId id, Integer capacity) {
    this.put(id, new DeviceInfo(id, capacity, lock, cycledComponents, noUseComponents));
  }

  public void initComponents(Map<ComponentId, DeviceId> componentsPlacement) {
    for (Map.Entry<ComponentId, DeviceId> entry : componentsPlacement.entrySet()) {
      if (entry.getKey() == null || entry.getValue() == null)
        throw new IllegalArgumentException("null can't be a componentId or a deviceId");

      super.get(entry.getValue()).addInitialComponent(entry.getKey());
    }
  }

  public void removeComponent(ComponentTransfer transfer) {
    DeviceInfo sourceDevice = super.get(transfer.getSourceDeviceId());
    ComponentId id = transfer.getComponentId();

    sourceDevice.announceComponent(id);

    transfer.prepare();

    sourceDevice.removeComponent(id);

    transfer.perform();
  }

  public void addComponent(ComponentTransfer transfer) {
    DeviceInfo destinationDevice = super.get(transfer.getDestinationDeviceId());
    ComponentId id = transfer.getComponentId();

    destinationDevice.reserveComponent(id);

    transfer.prepare();

    destinationDevice.addComponent(id);

    transfer.perform();
  }

  public void moveComponent(ComponentTransfer transfer) {

    DeviceInfo destinationDevice = super.get(transfer.getDestinationDeviceId());
    DeviceInfo sourceDevice = super.get(transfer.getSourceDeviceId());
    ComponentId id = transfer.getComponentId();

    findCycle(transfer);

    destinationDevice.reserveComponent(id);
    moveRequests
        .get(transfer.getSourceDeviceId())
        .remove(Map.entry(transfer.getDestinationDeviceId(), transfer.getComponentId()));
    noUseComponents.remove(id);
    sourceDevice.announceComponent(id);

    transfer.prepare();

    sourceDevice.removeComponent(id);
    destinationDevice.addComponent(id);

    transfer.perform();
  }

  // if finds cycle writes {dependency : dependant} to cycledComponents and makes sure nobody can use them
  // else adds transfer to moveRequests
  private void findCycle(ComponentTransfer transfer) {
    ArrayList<Entry<DeviceId, ComponentId>> path = new ArrayList<>();
    ArrayList<DeviceId> seen = new ArrayList<>();
    Entry<DeviceId, ComponentId> a =
        Map.entry(transfer.getDestinationDeviceId(), transfer.getComponentId());

    lock.writeLock().lock();

    if (findPath(transfer.getDestinationDeviceId(), transfer.getSourceDeviceId(), path, seen)) {
      path.add(a);
      for (int i = path.size() - 1; i > 0; i--) {
        Entry<DeviceId, ComponentId> dependent = path.get(i);
        Entry<DeviceId, ComponentId> dependency = path.get(i - 1);
        cycledComponents.put(dependency.getValue(), dependent.getValue());
        noUseComponents.add(dependent.getValue());
      }
      cycledComponents.put(path.get(path.size() - 1).getValue(), path.get(0).getValue());
    } else {
      moveRequests.get(transfer.getSourceDeviceId()).add(a);
    }

    lock.writeLock().unlock();
  }

  //returns true iff finds a path from start to end using moveRequests.
  private boolean findPath(
      DeviceId start,
      DeviceId end,
      ArrayList<Entry<DeviceId, ComponentId>> path,
      ArrayList<DeviceId> seen) {
    if (start.equals(end)) {
      return true;
    }
    seen.add(start);
    for (Entry<DeviceId, ComponentId> entry : moveRequests.get(start)) {
      if (!seen.contains(entry.getKey()) && !noUseComponents.contains(entry.getValue())) {
        if (findPath(entry.getKey(), end, path, seen)) {
          path.add(entry);
          return true;
        }
      }
    }
    return false;
  }
}
