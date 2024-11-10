using G74.Domain.IRepositories;
using G74.Infrastructure;
using G74.Mappers;
using Microsoft.EntityFrameworkCore;

namespace G74.Adapters.Repositories;

public class SurgeryRoomRepository : GenericRepository<SurgeryRoom>, ISurgeryRoomRepository
{
    private readonly SurgeryRoomToDataModelMapper _surgeryRoomToDataModelMapper;
    private readonly BackofficeAppDbContext _context;

    public SurgeryRoomRepository(BackofficeAppDbContext context, SurgeryRoomToDataModelMapper surgeryRoomToDataModelMapper) : base(context!)
    {
        _surgeryRoomToDataModelMapper = surgeryRoomToDataModelMapper;
        _context = context;
    }
    
    public async Task<bool> SurgeryRoomExists(string id)
    {
        return await _context.SurgeryRooms
            .AnyAsync(u => u.Id.Equals(id));
    }
}