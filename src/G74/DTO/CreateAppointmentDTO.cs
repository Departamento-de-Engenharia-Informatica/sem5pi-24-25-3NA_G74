public class CreateAppointmentDTO
{
    public int operationRequestId { get; private set; }
	public int surgeryRoomId { get; private set; }
	public DateTime date { get; private set; }
	public int time { get; private set; }
	public string status { get; private set; }

    public CreateAppointmentDTO()
    {

    }

}