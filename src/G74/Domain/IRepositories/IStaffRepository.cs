using G74.Domain.Aggregates.Staff;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.Staff.Doctor;

namespace G74.Domain.IRepositories;

public interface IStaffRepository
{
    Task<IEnumerable<Staff>> GetStaffAsync();

    Task<Staff?> GetByLicenceNumber(LicenceNumber licenceNumber);

    // Task<Staff> GetByIdAsync(LicenceNumber id);

    Task<Staff> Add(Staff staff);

    Task<Staff?> Update(LicenceNumber licenceNumber, Staff staff);

    // // TODO: boolean
    // Task<bool> StaffExists(LicenceNumber licenceNumber);

    // TODO: void?; receive just Staff staff?
    Task<Staff> UpdateStatus(LicenceNumber licenceNumber, Staff staff);

    Task ExportStaffDataToProlog();
}