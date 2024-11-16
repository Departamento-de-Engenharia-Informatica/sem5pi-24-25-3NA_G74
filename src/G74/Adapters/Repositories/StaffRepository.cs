using System.Text;
using G74.Domain.Aggregates.Staff;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Staff;
using G74.DTO;
using G74.Infrastructure;
using G74.Infrastructure.Shared;
using G74.Mappers;
using Microsoft.EntityFrameworkCore;

namespace G74.Adapters.Repositories;

// TODO: maybe remove BaseRepository inheritance. switch to GenericRepository?
public class StaffRepository : BaseRepository<Staff, Guid>, IStaffRepository
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
            Staff staff = await _dbContext.Set<Staff>()
                .FirstAsync(s => s.LicenceNumber == licenceNumber);

            return staff;
        }
        catch (InvalidOperationException ex)
        {
            return null;
        }
    }



    public async Task<Staff> Add(Staff staff)
    {
        var ret = _dbContext.Staff.Add(staff);
        await _dbContext.SaveChangesAsync();

        return ret.Entity;
    }

    // public async Task<bool> StaffExists(LicenceNumber licenceNumber)
    // {
    //     return await _dbContext.Set<Staff>().AnyAsync(e => e.Id.Value == licenceNumber.Value);
    // }

    public async Task<IEnumerable<Staff>> GetStaffAsync()
    {
        try
        {
            IEnumerable<Staff> staffs = await _dbContext.Set<Staff>()
                .ToListAsync();

            return staffs;
        }
        catch (Exception ex)
        {
            throw ex.InnerException!;
        }
    }




    public async Task<Staff?> Update(LicenceNumber licenceNumber, Staff staff)
    {

        var existingStaff = await _dbContext.Set<Staff>()
            .FirstOrDefaultAsync(s => s.LicenceNumber == licenceNumber);

        if (existingStaff == null)
        {
            return null;
        }

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
        try
        {
            var existingStaff = await _dbContext.Set<Staff>()
                .FirstOrDefaultAsync(s => s.LicenceNumber == licenceNumber);

            if (existingStaff == null)
            {
                throw new Exception($"Staff with licence number {licenceNumber} not found");
            }

            // Update only the status
            existingStaff.UpdateStatus(staff.Status);

            await _dbContext.SaveChangesAsync();

            return existingStaff;
        }
        catch (Exception ex)
        {
            throw ex.InnerException!;
        }
    }

    public async Task ExportStaffDataToProlog()
    {
        var staffList = await _dbContext.Staff.ToListAsync();
        var prologData = new StringBuilder();

        foreach (var staff in staffList)
        {
            prologData.AppendLine($"staff('{staff.Id}', '{staff.LicenceNumber}', '{staff.Name}', '{staff.PhoneNumber}', '{staff.ContactEmail}', '{staff.StaffSpecialization}', '{staff.Status}', '{staff.Availability}').");
        }

        File.WriteAllText("exported_staff.pl", prologData.ToString());
    }
}