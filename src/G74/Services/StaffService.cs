using Microsoft.EntityFrameworkCore;
using G74.Domain.IRepositories;
using G74.Domain.IRepositories;

namespace DefaultNamespace;

public class StaffService
{
    private readonly IStaffRepository _staffRepository;
    
    public StaffService(IStaffRepository staffRepository) {
        _staffRepository = staffRepository;
    }
    
    public async Task<StaffDTO> GetByLicenseNumber(string licenseNumber)
    {    
        Staff staff =  await _staffRepository.GetStaffByLicenseNumberAsync(licenseNumber);

        if(staff != null)
        {
            StaffDTO staffDTO = StaffDTO.FromDomain(staff);
            return staffDTO;
        }
        return null;
    }
    
    public async Task<StaffDTO> Add(StaffDTO staffDTO)
    {
        bool exists = await _staffRepository.StaffExists(staffDTO.LicenseNumber);
        if(exists) {
            throw new Exception("Already exists");
        }

        try
        {
            Staff staff = StaffDTO.ToDomain(staffDTO);

            Staff staffSaved = await _staffRepository.Add(staff);

            StaffDTO staffDTO = StaffDTO.FromDomain(staffSaved);

            return staffDTO;
        }
        catch (Exception e)
        {
            throw e;
        }
    }
    
}