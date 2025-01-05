using G74.Domain.Aggregates.Appointment;
using G74.DTO;

namespace G74.Domain.IRepositories;

public interface IAppointmentRepository
{
    Task<bool> AppointmentExists(string id);
    Task ExportAppointmentDataToProlog();
    Task<Appointment> Create(AppointmentDataModel app);

    Task<Appointment> Update(AppointmentDataModel apppointmentDataModel, long id);
    
}