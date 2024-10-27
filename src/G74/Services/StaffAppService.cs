using G74.Domain.Aggregates.Staff;
using G74.Domain.IRepositories;
using G74.DTO;

namespace G74.Services;

public class StaffAppService
{
    private readonly IStaffRepository _staffRepository;
    
    public StaffAppService(IStaffRepository staffRepository) {
        _staffRepository = staffRepository;
    }
    
    public async Task<IEnumerable<StaffDto>> GetAll()
    {    
        IEnumerable<Staff> staff = await _staffRepository.GetStaffAsync();

        IEnumerable<StaffDto> staffDto = StaffDto.FromDomain(staff);

        return staffDto;
    }
    
    public async Task<StaffDto?> GetByLicenseNumber(string licenseNumber)
    {    
        Staff? staff =  await _staffRepository.GetStaffByLicenseNumberAsync(licenseNumber);

        if(staff != null)
        {
            StaffDto staffDto = StaffDto.FromDomain(staff);
            return staffDto;
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
        catch (Exception ex)
        {
            throw ex.InnerException!;
        }
    }

    public async Task<StaffDto> Update(string licenseNumber, StaffDto staffDto)
    {
        // try
        // {
            // // Make sure we use the license number from the URL, not from the DTO
            // staffDto.LicenseNumber = licenseNumber;
        
            Staff staff = StaffDto.ToDomain(staffDto);
            Staff staffUpdated = await _staffRepository.Update(licenseNumber, staff);
            StaffDto staffDtoResult = StaffDto.FromDomain(staffUpdated);

            return staffDtoResult;
        // }
        // catch (Exception ex)
        // {
        //     throw ex.InnerException!;
        // }
    }
    
    public async Task<StaffDto?> Deactivate(string licenseNumber)
    {
        Staff? staff = await _staffRepository.GetStaffByLicenseNumberAsync(licenseNumber);
        if (staff == null)
        {
            return null;
        }

        staff.Deactivate();
        Staff updatedStaff = await _staffRepository.UpdateStatus(licenseNumber, staff);
        return StaffDto.FromDomain(updatedStaff);
    }
    
}