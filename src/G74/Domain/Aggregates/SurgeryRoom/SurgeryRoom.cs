public class SurgeryRoom{
	public string type { get; }
	public int capacity { get; }
	public List<string> assignedEquipment { get; }
	public RoomStatus roomStatus { get; }
	public List<int> maintenanceSlot { get; }
	
	public SurgeryRoom(string type, int capacity, List<string> assignedEquipment, RoomStatus roomStatus, List<int> maintenanceSlot){
		this.type = type;
		this.capacity = capacity;
		this.assignedEquipment = assignedEquipment;
		this.roomStatus = roomStatus;
		this.maintenanceSlot = maintenanceSlot;
	}
}