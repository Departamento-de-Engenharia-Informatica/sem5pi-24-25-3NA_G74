
using G74.Domain.Shared;

namespace G74.DTO;

public class SurgeryRoomDataModel : Entity<Guid>
{
	public int roomNumber { get; private set; }
	public string type { get; private set; }
	public int capacity { get; private set; }
	public string assignedEquipment { get; private set; }
	public string roomStatus { get; private set; }
	public string maintenanceSlot { get; private set; }
    
    protected SurgeryRoomDataModel() : base(Guid.NewGuid())
    {
        
    }
    public SurgeryRoomDataModel(SurgeryRoom surgeryRoom) : base(Guid.NewGuid())
    {
	    roomNumber = surgeryRoom.roomNumber;
        type = surgeryRoom.type;
        capacity = surgeryRoom.capacity;
        assignedEquipment = surgeryRoom.assignedEquipment.ToString();
        roomStatus = surgeryRoom.roomStatus.ToString();
		maintenanceSlot = surgeryRoom.maintenanceSlot.ToString();
    }
}