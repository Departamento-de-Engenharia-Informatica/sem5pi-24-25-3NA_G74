using G74.Adapters.Repositories;
using G74.Mappers;
using G74.Domain;

namespace DefaultNamespace;

public class StaffRepository : GenericRepository<Staff>, IStaffRepository
{
    StaffMapper _staffMapper;
    
    // TODO: public StaffRepository(?, context, StaffMapper mapper) : base(context!)
    public StaffRepository(StaffMapper mapper) : base(context!)
    {
        _staffMapper = mapper;
    }
    
    public async Task<Staff> GetStaffByLicenseNumberAsync(string licenseNumber)
    {
        try {
            StaffDataModel staffDataModel = await _context.Set<StaffDataModel>()
                .FirstAsync(s => s.LicenseNumber == licenseNumber);

            Staff staff = _staffMapper.ToDomain(staffDataModel);

            return staff;
        }
        catch
        {
            return null;
        }
    }
    
    public async Task<Staff> Add(Staff staff)
    {
        try {
            StaffDataModel staffDataModel = _staffMapper.ToDataModel(staff);

            EntityEntry<StaffDataModel> staffDataModelEntityEntry = _context.Set<StaffDataModel>().Add(staffDataModel);
            
            await _context.SaveChangesAsync();

            StaffDataModel staffDataModelSaved = staffDataModelEntityEntry.Entity;

            Staff staffSaved = _staffMapper.ToDomain(staffDataModelSaved);

            return staffSaved;    
        }
        catch
        {
            throw;
        }
    }
    
    public async Task<bool> StaffExists(string licenseNumber)
    {
        return await _context.Set<StaffDataModel>().AnyAsync(e => e.LicenseNumber == licenseNumber);
    }
    
    // TODO
    public async Task<IEnumerable<Staff>> GetStaffAsync()
    {
        return null;
    }
    
    // TODO
    public async Task<Staff> GetStaffByIdAsync(long id)
    {
        return null;
    }
    
    // TODO
    public async Task<Staff> Update(Staff staff)
    {
        return null;
    }
    
    
}