using G74.Domain.Aggregates.Staff;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;

namespace G74.DTO;

public class StaffDto
{
    public string LicenseNumber { get; set; }
    public string Name { get; set; }
    public string PhoneNumber { get; set; }
    public string ContactEmail { get; set; }
    public string StaffSpecialization { get; set; }

    // Constructor for creating from primitive types (e.g., from API request)
    public StaffDto(string licenseNumber, string name, string phoneNumber, 
        string contactEmail, string staffSpecialization)
    {
        LicenseNumber = licenseNumber;
        Name = name;
        PhoneNumber = phoneNumber;
        ContactEmail = contactEmail;
        StaffSpecialization = staffSpecialization;
    }
    
    // Constructor for creating from domain model (e.g., for API response)
    public static StaffDto FromDomain(Staff staff)
    {
        return new StaffDto(
            staff.LicenseNumber.Value,
            staff.Name.TheName,
            staff.PhoneNumber.Value,
            staff.ContactEmail.Value,
            staff.StaffSpecialization.Value
        );
    }
    
    // Method to convert to domain model with validation
    public Staff ToDomain(StaffDto staffDto)
    {
        return new Staff(
            new LicenseNumber(staffDto.LicenseNumber),  // Validation happens in value objects
            new Name(staffDto.Name),
            new PhoneNumber(staffDto.PhoneNumber),
            new Email(staffDto.ContactEmail),
            new StaffSpecialization(staffDto.StaffSpecialization)
        );
    }
    
    
    
}