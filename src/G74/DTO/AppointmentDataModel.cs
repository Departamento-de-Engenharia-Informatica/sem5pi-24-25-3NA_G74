using System.Text.Json.Serialization;
using G74.Domain.Aggregates.Appointment;
using G74.Domain.Shared;

namespace G74.DTO;

public class AppointmentDataModel : Entity<Guid>
{
    
    public int OperationRequestId { get;  set; }
    public int SurgeryRoomId { get;  set; }
    public string Date { get;  set; }
    public int Time { get;  set; }
    public string Status { get;  set; }

    // Construtor sem parâmetros
    public AppointmentDataModel() : base(Guid.NewGuid())
    {
    }

    // Construtor parametrizado com JsonConstructor
    [JsonConstructor]
    public AppointmentDataModel(int operationRequestId, int surgeryRoomId, string date, int time, string status) : base(Guid.NewGuid())
    {
        OperationRequestId = operationRequestId;
        SurgeryRoomId = surgeryRoomId;
        Date = date;
        Time = time;
        Status = status;
    }

    public AppointmentDataModel(Appointment appointment) : base(Guid.NewGuid())
    {
        OperationRequestId = appointment.OperationRequestId;
        SurgeryRoomId = appointment.SurgeryRoomId;
        Date = appointment.Date.ToString();
        Time = appointment.Time;
        Status = appointment.Status.ToString();
    }
}