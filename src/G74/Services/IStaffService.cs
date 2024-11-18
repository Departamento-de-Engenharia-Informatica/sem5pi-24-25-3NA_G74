using G74.Domain.Aggregates.Staff;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Staff.Doctor;
using G74.DTO;

namespace G74.Services;

public interface IStaffService
{
    Task<IEnumerable<StaffDto>> GetAll();

    Task<StaffDto?> GetByLicenceNumber(long licenceNumber);

    Task<StaffDto> Add(StaffDto staffDto);

    Task<StaffDto> Update(long licenceNumber, StaffDto staffDto);

    Task<StaffDto?> Deactivate(long licenceNumber);
}