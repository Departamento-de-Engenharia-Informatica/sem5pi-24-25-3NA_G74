using G74.DTO;

public interface IAppServiceAppointment
{
    Task<AppointmentDataModel> RegisterAppointment(AppointmentDataModel operationData);
    Task<AppointmentDataModel> Update(AppointmentDataModel appointmentDataModel, long id);
}