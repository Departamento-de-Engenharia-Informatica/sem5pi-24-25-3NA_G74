using G74.Domain;

namespace DefaultNamespace;

public interface IStaffRepository
{
    Task<IEnumerable<Staff>> GetStaffAsync();

    Task<Staff> GetStaffByLicenseNumberAsync(string licenseNumber);

    Task<Staff> GetStaffByIdAsync(long id);

    Task<Staff> Add(Staff staff);

    Task<Staff> Update(Staff staff);

    Task<bool> StaffExists(string licenseNumber);
}