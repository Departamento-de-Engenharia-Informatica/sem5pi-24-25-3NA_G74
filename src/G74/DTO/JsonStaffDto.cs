namespace G74.DTO;

public class JsonStaffDto
{
    public string LicenseNumber { get; set; }
    public string Name { get; set; }
    public string PhoneNumber { get; set; }
    public string ContactEmail { get; set; }
    public string StaffSpecialization { get; set; }

    public JsonStaffDto(string licenseNumber, string name, string phoneNumber, string contactEmail, string staffSpecialization)
    {
        LicenseNumber = licenseNumber;
        Name = name;
        PhoneNumber = phoneNumber;
        ContactEmail = contactEmail;
        StaffSpecialization = staffSpecialization;
    }
}