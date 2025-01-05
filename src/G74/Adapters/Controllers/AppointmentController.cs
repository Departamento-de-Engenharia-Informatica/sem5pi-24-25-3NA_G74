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
        
        await _appServiceAppointment.RegisterAppointment(appointmentDataModel);
        return appointmentDataModel;
    }

    [HttpPut("{id}")]
    public async Task<AppointmentDataModel> Update([FromBody] AppointmentDataModel appointmentDataModel, long id)
    {
        await _appServiceAppointment.Update(appointmentDataModel, id);
        return appointmentDataModel;
    }
    
    
}