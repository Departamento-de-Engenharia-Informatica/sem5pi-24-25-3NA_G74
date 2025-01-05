using G74.Domain.Aggregates.Appointment;
using G74.DTO;

namespace G74.Mappers;

public class AppointmentToDataModelMapper
{
    /**
    public AppointmentDataModel(Appointment appointment) : base(Guid.NewGuid())
    {
	    operationRequestId = appointment.operationRequestId;
	    surgeryRoomId = appointment.surgeryRoomId;
        date = appointment.date;
        time = appointment.time;
        status = appointment.status.ToString();
    }
    */
    public static Appointment FromDataModelToDomain(AppointmentDataModel appointmentDataModel){
        StatusAppointment statusAppointment = StatusAppointment.canceled;

        if(appointmentDataModel.Status=="scheduled"){
            statusAppointment = StatusAppointment.scheduled;
        }
        if(appointmentDataModel.Status=="completed"){
            statusAppointment = StatusAppointment.completed;
        }
        if(appointmentDataModel.Status == "canceled"){
            statusAppointment = StatusAppointment.canceled;
        }
        
        return new 
             Appointment(
                appointmentDataModel.OperationRequestId,
                appointmentDataModel.SurgeryRoomId,
                DateTime.Parse(appointmentDataModel.Date),
                appointmentDataModel.Time,
                statusAppointment
            );
        
    }

    
}