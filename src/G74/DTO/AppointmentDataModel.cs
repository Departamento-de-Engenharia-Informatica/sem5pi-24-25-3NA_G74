
using G74.Domain.Aggregates.Appointment;
using G74.Domain.Shared;

namespace G74.DTO;

public class AppointmentDataModel : Entity<Guid>
{
	public int operationRequestId { get; private set; }
	public int surgeryRoomId { get; private set; }
	public string date { get; private set; }
	public int time { get; private set; }
	public string status { get; private set; }
    
    protected AppointmentDataModel() : base(Guid.NewGuid())
    {
        
    }
    public AppointmentDataModel(Appointment appointment) : base(Guid.NewGuid())
    {
	    operationRequestId = appointment.operationRequestId;
	    surgeryRoomId = appointment.surgeryRoomId;
        date = appointment.date;
        time = appointment.time;
        status = appointment.status.ToString();
    }
}