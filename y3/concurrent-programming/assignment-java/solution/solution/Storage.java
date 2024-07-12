package cp2023.solution;

import cp2023.base.ComponentId;
import cp2023.base.ComponentTransfer;
import cp2023.base.DeviceId;
import cp2023.base.StorageSystem;
import cp2023.exceptions.*;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class Storage implements StorageSystem {

  Set<ComponentId> movedComponents = ConcurrentHashMap.newKeySet();
  ConcurrentHashMap<ComponentId, DeviceId> componentPlacement;
  DeviceInfoMap infoMap;

  public Storage(
      Map<DeviceId, Integer> deviceTotalSlots, Map<ComponentId, DeviceId> componentPlacement) {
    if (deviceTotalSlots == null || componentPlacement == null || deviceTotalSlots.isEmpty())
      throw new IllegalArgumentException("null arguments");

    this.componentPlacement = new ConcurrentHashMap<>(componentPlacement);
    this.infoMap = new DeviceInfoMap();


    for (Map.Entry<DeviceId, Integer> entry : deviceTotalSlots.entrySet()) {
      infoMap.addDevice(entry.getKey(), entry.getValue());
    }

    infoMap.initComponents(componentPlacement);
  }

  @Override
  public void execute(ComponentTransfer transfer) throws TransferException {

    checkForBasicExceptions(transfer);

    if (transfer.getSourceDeviceId() == null) {
      addComponent(transfer);
    } else if (transfer.getDestinationDeviceId() == null) {
      removeComponent(transfer);
    } else {
      moveComponent(transfer);
    }
  }

  // in every other function we assume that this has already been checked
  private void checkForBasicExceptions(ComponentTransfer transfer) throws TransferException {
    if (transfer.getComponentId() == null) throw new IllegalTransferType(null);
    if (transfer.getSourceDeviceId() == null && transfer.getDestinationDeviceId() == null)
      throw new IllegalTransferType(transfer.getComponentId());
    if (transfer.getSourceDeviceId() != null && !infoMap.containsKey(transfer.getSourceDeviceId()))
      throw new DeviceDoesNotExist(transfer.getSourceDeviceId());
    if (transfer.getDestinationDeviceId() != null
        && !infoMap.containsKey(transfer.getDestinationDeviceId()))
      throw new DeviceDoesNotExist(transfer.getDestinationDeviceId());
  }

  private void addComponent(ComponentTransfer transfer) throws TransferException {
    // we check if a transfer if correct and if it is mark the component in the system. after that
    // the sync part is
    // over and we only deal with the deviceInfoMap

    synchronized (this) {
      if (componentExists(transfer.getComponentId()))
        throw new ComponentAlreadyExists(
            transfer.getComponentId(), componentPlacement.get(transfer.getComponentId()));
      componentPlacement.put(transfer.getComponentId(), transfer.getDestinationDeviceId());
      movedComponents.add(transfer.getComponentId());
    }

    // only correct transfers can be allowed inside the deviceInfoMap
    infoMap.addComponent(transfer);

    synchronized (this) {
      movedComponents.remove(transfer.getComponentId());
    }
  }

  private void removeComponent(ComponentTransfer transfer) throws TransferException {
    synchronized (this) {
      if (isBeingTransferred(transfer.getComponentId()))
        throw new ComponentIsBeingOperatedOn(transfer.getComponentId());
      if (!componentExists(transfer.getComponentId()))
        throw new ComponentDoesNotNeedTransfer(
            transfer.getComponentId(), transfer.getDestinationDeviceId());
      if (!componentExistsOn(transfer.getComponentId(), transfer.getSourceDeviceId()))
        throw new ComponentDoesNotExist(transfer.getComponentId(), transfer.getSourceDeviceId());

      movedComponents.add(transfer.getComponentId());
    }

    infoMap.removeComponent(transfer);

    synchronized (this) {
      componentPlacement.remove(transfer.getComponentId());
      movedComponents.remove(transfer.getComponentId());
    }
  }

  private void moveComponent(ComponentTransfer transfer) throws TransferException {
    synchronized (this) {
      if (isBeingTransferred(transfer.getComponentId()))
        throw new ComponentIsBeingOperatedOn(transfer.getComponentId());
      if (componentExistsOn(transfer.getComponentId(), transfer.getDestinationDeviceId()))
        throw new ComponentDoesNotNeedTransfer(
            transfer.getComponentId(), transfer.getDestinationDeviceId());
      if (!componentExistsOn(transfer.getComponentId(), transfer.getSourceDeviceId()))
        throw new ComponentDoesNotExist(transfer.getComponentId(), transfer.getSourceDeviceId());
      movedComponents.add(transfer.getComponentId());
    }

    infoMap.moveComponent(transfer);

    synchronized (this) {
      componentPlacement.put(transfer.getComponentId(), transfer.getDestinationDeviceId());
      movedComponents.remove(transfer.getComponentId());
    }
  }

  private synchronized boolean componentExists(ComponentId component) {
    return componentPlacement.containsKey(component);
  }

  private synchronized boolean componentExistsOn(ComponentId component, DeviceId device) {
    return componentExists(component) && componentPlacement.get(component).equals(device);
  }

  private synchronized boolean isBeingTransferred(ComponentId component) {
    return movedComponents.contains(component);
  }
}
