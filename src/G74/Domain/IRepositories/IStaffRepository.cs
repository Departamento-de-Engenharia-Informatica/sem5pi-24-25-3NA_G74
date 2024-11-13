using G74.Domain.Aggregates.Staff;
using G74.Domain.Value_Objects.Staff;

namespace G74.Domain.IRepositories;

public interface IStaffRepository
{
    Task<IEnumerable<Staff>> GetStaffAsync();

    Task<Staff?> GetByLicenseNumber(LicenceNumber licenceNumber);

    // Task<Staff> GetByIdAsync(LicenseNumber id);
    
    Task<Staff> Add(Staff staff);

    Task<Staff?> Update(LicenceNumber licenceNumber, Staff staff);

    // // TODO: boolean
    // Task<bool> StaffExists(LicenseNumber licenseNumber);
    
    // TODO: void?; receive just Staff staff?
    Task<Staff> UpdateStatus(LicenceNumber licenceNumber, Staff staff);

    Task ExportStaffDataToProlog();
}