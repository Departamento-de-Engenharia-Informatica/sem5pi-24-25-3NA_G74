using G74.Domain.Aggregates.Staff;

namespace G74.Domain.IRepositories;

public interface IStaffRepository
{
    Task<IEnumerable<Staff>> GetStaffAsync();

    Task<Staff?> GetStaffByLicenseNumberAsync(string licenseNumber);

    Task<Staff> GetStaffByIdAsync(long id);

    Task<Staff> Add(Staff staff);

    Task<Staff> Update(string licenseNumber, Staff staff);

    Task<bool> StaffExists(string licenseNumber);
    
    Task<Staff> UpdateStatus(string licenseNumber, Staff staff);
}