using G74.Domain.Aggregates.Staff;
using G74.Domain.Value_Objects.Staff;

namespace G74.Domain.IRepositories;

public interface IStaffRepository
{
    Task<IEnumerable<Staff>> GetStaffAsync();

    Task<Staff?> GetByLicenseNumber(LicenseNumber licenseNumber);

    // Task<Staff> GetByIdAsync(LicenseNumber id);
    
    Task<Staff> Add(Staff staff);

    Task<Staff?> Update(LicenseNumber licenseNumber, Staff staff);

    // // TODO: boolean
    // Task<bool> StaffExists(LicenseNumber licenseNumber);
    
    // TODO: void?; receive just Staff staff?
    Task<Staff> UpdateStatus(LicenseNumber licenseNumber, Staff staff);

    Task ExportStaffDataToProlog();
}