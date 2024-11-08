
public class Appointment
{
	public string date { get; }
	public int time { get; }
	public StatusAppointment status { get; }

    public Appointment(string date, int time, StatusAppointment status)
    {
		this.date = date;
		this.time = time;
		this.status = status;
    }
    
}