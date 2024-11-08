
using G74.Domain.Shared;

namespace G74.DTO;

public class AppointmentDataModel : Entity<Guid>
{
	public string date { get; private set; }
	public int time { get; private set; }
	public string status { get; private set; }
    
    protected AppointmentDataModel() : base(Guid.NewGuid())
    {
        
    }
    public AppointmentDataModel(Appointment appointment) : base(Guid.NewGuid())
    {
        date = appointment.date;
        time = appointment.time;
        status = appointment.status.ToString();
    }
}