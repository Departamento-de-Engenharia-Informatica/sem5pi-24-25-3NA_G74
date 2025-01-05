
namespace G74.Domain.Aggregates.Appointment;

public class Appointment
{
	public int OperationRequestId { get; }
	public int SurgeryRoomId { get; }
	public DateTime Date { get; }
	public int Time { get; }
	public StatusAppointment Status { get; }

	public Appointment(int operationRequestId, int surgeryRoomId, DateTime date, int time, StatusAppointment status)
	{
		this.OperationRequestId = operationRequestId;
		this.SurgeryRoomId = surgeryRoomId;
		this.Date = date;
		this.Time = time;
		this.Status = status;
	}


    
}