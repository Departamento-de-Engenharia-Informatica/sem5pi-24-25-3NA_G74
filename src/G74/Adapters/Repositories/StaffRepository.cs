using System.Text;
using G74.Domain.Aggregates.Staff;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.Staff.Doctor;
using G74.DTO;
using G74.Infrastructure;
using G74.Infrastructure.Shared;
using G74.Mappers;
using Microsoft.EntityFrameworkCore;

namespace G74.Adapters.Repositories;

// TODO: maybe remove BaseRepository inheritance. switch to GenericRepository?
public class StaffRepository : BaseRepository<StaffDataModel, Guid>, IStaffRepository
{
    private readonly BackofficeAppDbContext _dbContext;


    public StaffRepository(BackofficeAppDbContext dbContext) : base(dbContext.Staff)
    {
        _dbContext = dbContext;
    }

    public async Task<Staff?> GetByLicenceNumber(LicenceNumber licenceNumber)
    {
        try
        {
            StaffDataModel staffDataModel = await _dbContext.Set<StaffDataModel>()
                .FirstAsync(s => s.LicenceNumber == licenceNumber.Value);

            return StaffDataModel.ToDomain(staffDataModel);
        }
        catch (InvalidOperationException ex)
        {
            return null;
        }
    }



    public async Task<Staff> Add(Staff staff)
    {
        StaffDataModel staffDataModel = StaffDataModel.FromDomain(staff);
        
        var ret = _dbContext.Set<StaffDataModel>().Add(staffDataModel);
        await _dbContext.SaveChangesAsync();

        return StaffDataModel.ToDomain(ret.Entity);
    }

    // public async Task<bool> StaffExists(LicenceNumber licenceNumber)
    // {
    //     return await _dbContext.Set<Staff>().AnyAsync(e => e.Id.Value == licenceNumber.Value);
    // }

    public async Task<IEnumerable<Staff>> GetStaffAsync()
    {
        try
        {
            IEnumerable<StaffDataModel> staffDataModel = await _dbContext.Set<StaffDataModel>()
                .ToListAsync();

            return StaffDataModel.ToDomain(staffDataModel);
        }
        catch (Exception ex)
        {
            throw ex.InnerException!;
        }
    }




    public async Task<Staff?> Update(LicenceNumber licenceNumber, Staff staff)
    {

        var staffDataModel = await _dbContext.Set<StaffDataModel>()
            .FirstOrDefaultAsync(s => s.LicenceNumber == licenceNumber.Value);

        if (staffDataModel == null)
        {
            return null;
        }

        Staff existingStaff = StaffDataModel.ToDomain(staffDataModel);

        staffDataModel.LicenceNumber = staff.LicenceNumber.Value;
        staffDataModel.Name = staff.Name.Value;
        staffDataModel.PhoneNumber = staff.PhoneNumber.Value;
        staffDataModel.ContactEmail = staff.ContactEmail.email;
        staffDataModel.StaffSpecialization = staff.StaffSpecialization.Value;
        staffDataModel.Status = staff.Status.Value;
        
        existingStaff.UpdateLicenceNumber(staff.LicenceNumber);
        existingStaff.UpdateName(staff.Name);
        existingStaff.UpdatePhoneNumber(staff.PhoneNumber);
        existingStaff.UpdateContactEmail(staff.ContactEmail);
        existingStaff.UpdateStaffSpecialization(staff.StaffSpecialization);
        existingStaff.UpdateStatus(staff.Status);

        await _dbContext.SaveChangesAsync();

        return existingStaff;
    }

    public async Task<Staff> UpdateStatus(LicenceNumber licenceNumber, Staff staff)
    {
        var existingStaffDataModel = await _dbContext.Set<StaffDataModel>()
            .FirstOrDefaultAsync(s => s.LicenceNumber == licenceNumber.Value);

        if (existingStaffDataModel == null)
        {
            throw new Exception($"Staff with licence number {licenceNumber} not found");
        }

        Staff existingStaff = StaffDataModel.ToDomain(existingStaffDataModel);

        // Update only the status
        existingStaff.Deactivate();

        await _dbContext.SaveChangesAsync();

        return existingStaff;
    }

    public async Task ExportStaffDataToProlog()
    {
        var staffList = await _dbContext.Staff.ToListAsync();
        var prologData = new StringBuilder();

        foreach (var staff in staffList)
        {
            prologData.AppendLine($"staff('{staff.Id}', '{staff.LicenceNumber}', '{staff.Name}'," +
                                  $"'{staff.PhoneNumber}', '{staff.ContactEmail}', '{staff.StaffSpecialization}'," +
                                  $"'{staff.Status}', '{staff.Availability}').");
        }

        File.WriteAllText("exported_staff.pl", prologData.ToString());
    }
}