namespace G74.Domain.IRepositories;

public interface IAppointmentRepository
{
    Task<bool> AppointmentExists(string id);
    Task ExportAppointmentDataToProlog();
}