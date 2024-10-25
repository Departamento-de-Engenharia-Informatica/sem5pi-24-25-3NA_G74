using DefaultNamespace;
using G74.Domain.Aggregates.Staff;
using G74.DTO;

namespace G74.Services;

public class StaffService
{
    private readonly IStaffRepository _staffRepository;
    
    public StaffService(IStaffRepository staffRepository) {
        _staffRepository = staffRepository;
    }
    
    public async Task<StaffDto> GetByLicenseNumber(string licenseNumber)
    {    
        Staff staff =  await _staffRepository.GetStaffByLicenseNumberAsync(licenseNumber);

        if(staff != null)
        {
            StaffDto staffDTO = StaffDto.FromDomain(staff);
            return staffDTO;
        }
        return null;
    }
    
    public async Task<StaffDto> Add(StaffDto staffDto)
    {
        bool exists = await _staffRepository.StaffExists(staffDto.LicenseNumber);
        if(exists) {
            throw new Exception("Already exists");
        }

        try
        {
            Staff staff = StaffDto.ToDomain(staffDto);

            Staff staffSaved = await _staffRepository.Add(staff);

            StaffDto staffDtoResult = StaffDto.FromDomain(staffSaved);

            return staffDtoResult;
        }
        catch
        {
            throw;
        }
    }
    
}