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
    
    public async Task ExportAppointmentDataToProlog()
    {
        //TODO:Terminar Rui Beloto.
    }

    public async Task<Appointment> Create(AppointmentDataModel appointmentDataModel)
    {
        if(await CheckIfDataValid(appointmentDataModel.OperationRequestId,appointmentDataModel.SurgeryRoomId, appointmentDataModel.Time, appointmentDataModel.Status, DateTime.Parse(appointmentDataModel.Date))){
            _context.Appointments.Add(appointmentDataModel);
            await _context.SaveChangesAsync();
            return AppointmentToDataModelMapper.FromDataModelToDomain(appointmentDataModel);
        }else{
            return null;
        }
    }

    public async Task<bool> CheckIfDataValid(int operationRequestId, int surgeryRoomId, int time, string status, DateTime date)
    {
        var operations = await _context.OperationRequests.ToListAsync();
        var surgeryRooms = await _context.SurgeryRooms.ToListAsync();
        var appointments = await _context.Appointments.ToListAsync();

        if (time <= 0)
        {
            throw new Exception("Time must be positive.");
        }

        if (!status.Equals("Scheduled", StringComparison.OrdinalIgnoreCase) &&
            !status.Equals("Completed", StringComparison.OrdinalIgnoreCase) &&
            !status.Equals("Canceled", StringComparison.OrdinalIgnoreCase))
        {
            throw new Exception("Status must be Scheduled/Completed/Canceled");
        }

        if (date <= DateTime.Now)
        {
            throw new Exception("Date must be valid.");
        }

        if (!operations.Any(o => o.Id == operationRequestId))
        {
            throw new Exception("Operation Request ID does not exist.");
        }

        if (!surgeryRooms.Any(sr => sr.roomNumber == surgeryRoomId))
        {
            throw new Exception("Surgery Room ID does not exist.");
        }

        // Additional validation logic can be added here
        return true;
    }

    public async Task<Appointment> Update(AppointmentDataModel appointmentDataModel, long id)
    {
        var appointment = await _context.Appointments.FindAsync(id) ?? throw new Exception("ID doesn't exist");

        appointment.OperationRequestId = appointmentDataModel.OperationRequestId;
        appointment.Date = appointmentDataModel.Date;
        appointment.Status = appointmentDataModel.Status;
        appointment.SurgeryRoomId = appointmentDataModel.SurgeryRoomId;
        appointment.Time = appointmentDataModel.Time;

        if (await CheckIfDataValid(appointmentDataModel.OperationRequestId,appointmentDataModel.SurgeryRoomId, appointmentDataModel.Time, appointmentDataModel.Status, DateTime.Parse(appointmentDataModel.Date))){
            _context.Appointments.Update(appointmentDataModel);
            await _context.SaveChangesAsync();
            return AppointmentToDataModelMapper.FromDataModelToDomain(appointmentDataModel);
        }
        return null;
    }

}