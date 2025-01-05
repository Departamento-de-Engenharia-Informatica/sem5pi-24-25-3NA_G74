
using G74.Domain.IRepositories;
using G74.DTO;

public class AppServiceAppointment : IAppServiceAppointment
{

    private readonly IAppointmentRepository _appointmentRepository;
    

    public AppServiceAppointment(IAppointmentRepository appointmentRepository){
        _appointmentRepository = appointmentRepository;
    }

    public async Task<AppointmentDataModel> RegisterAppointment(AppointmentDataModel operationData)
    {
        await _appointmentRepository.Create(operationData);
        return operationData;
    }

    public async Task<AppointmentDataModel> Update(AppointmentDataModel appointmentDataModel, long id)
    {
        await _appointmentRepository.Update(appointmentDataModel, id);
        return appointmentDataModel;
    }

    

}