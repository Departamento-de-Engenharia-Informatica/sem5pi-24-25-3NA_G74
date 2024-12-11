using G74.DTO;
using G74.Services;
using Microsoft.AspNetCore.Mvc;

[ApiController]
[Route("api/[controller]")]
public class AppointmentController : ControllerBase
{
    private readonly IAppServiceAppointment _appServiceAppointment;
    public AppointmentController(IAppServiceAppointment appServiceAppointment){
        _appServiceAppointment=appServiceAppointment;
    }

    [HttpPost]
    public async Task<AppointmentDataModel> Create([FromBody] AppointmentDataModel appointmentDataModel)
    {
        Console.WriteLine("Cheguei");
        await _appServiceAppointment.RegisterAppointment(appointmentDataModel);
        return appointmentDataModel;
    }
    
    
}