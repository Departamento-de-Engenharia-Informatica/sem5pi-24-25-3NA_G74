using G74.Domain;

namespace DefaultNamespace;

public class StaffDTO
{
    // TODO: put [required] validations, maybe [email] too
    public string LicenseNumber { get; set; }
    public string Name { get; set; }
    public string PhoneNumber { get; set; }
    public string ContactEmail { get; set; }
    public string Specialization { get; set; }

    // Constructor for creating from primitive types (e.g., from API request)
    public StaffDTO(string licenseNumber, string name, string phoneNumber, 
        string contactEmail, string specialization)
    {
        LicenseNumber = licenseNumber;
        Name = name;
        PhoneNumber = phoneNumber;
        ContactEmail = contactEmail;
        Specialization = specialization;
    }
    
    // Constructor for creating from domain model (e.g., for API response)
    public static StaffDTO FromDomain(Staff staff)
    {
        return new StaffDTO(
            staff.LicenseNumber.Value,
            staff.Name.Value,
            staff.PhoneNumber.Value,
            staff.ContactEmail.Value,
            staff.Specialization.Value
        );
    }
    
    // Method to convert to domain model with validation
    public Staff ToDomain()
    {
        return new Staff(
            new LicenseNumber(LicenseNumber),  // Validation happens in value objects
            new Name(Name),
            new PhoneNumber(PhoneNumber),
            new Email(ContactEmail),
            new Specialization(Specialization)
        );
    }
    
    
    
}