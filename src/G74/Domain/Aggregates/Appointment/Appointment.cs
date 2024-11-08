
namespace G74.Domain.Aggregates.Appointment;

public class Appointment
{
	public int operationRequestId { get; }
	public int surgeryRoomId { get; }
	public string date { get; }
	public int time { get; }
	public StatusAppointment status { get; }

	public Appointment(int operationRequestId, int surgeryRoomId, string date, int time, StatusAppointment status)
	{
		this.operationRequestId = operationRequestId;
		this.surgeryRoomId = surgeryRoomId;
		this.date = date;
		this.time = time;
		this.status = status;
	}
    
}