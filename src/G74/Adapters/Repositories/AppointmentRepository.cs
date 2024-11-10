using G74.Domain.Aggregates.Appointment;
using G74.Domain.IRepositories;
using G74.DTO;
using G74.Infrastructure;
using G74.Mappers;
using Microsoft.EntityFrameworkCore;

namespace G74.Adapters.Repositories;

public class AppointmentRepository : GenericRepository<Appointment>, IAppointmentRepository
{
    private readonly AppointmentToDataModelMapper _appointmentToDataModelMapper;
    private readonly BackofficeAppDbContext _context;
    
    public AppointmentRepository(BackofficeAppDbContext context, AppointmentToDataModelMapper appointmentToDataModelMapper) : base(context!)
    {
        _appointmentToDataModelMapper = appointmentToDataModelMapper;
        _context = context;
    }
    
    public async Task<bool> AppointmentExists(string id)
    {
        return await _context.Appointments
            .AnyAsync(u => u.Id.Equals(id));
    }
}