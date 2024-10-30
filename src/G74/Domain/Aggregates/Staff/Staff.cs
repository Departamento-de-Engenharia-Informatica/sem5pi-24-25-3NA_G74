using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.User;

namespace G74.Domain.Aggregates.Staff;

public class Staff
{
    public Guid Id {get; set;}
    public LicenseNumber LicenseNumber { get; private set; }
    public Name Name { get; private set; }
    public PhoneNumber PhoneNumber { get; private set; }
    public Email ContactEmail { get; private set; }
    public StaffSpecialization StaffSpecialization { get; private set; }
    public Status Status { get; private set; }
    

    public Staff(LicenseNumber licenseNumber, Name name, PhoneNumber phoneNumber,
        Email contactEmail, StaffSpecialization staffSpecialization, Status status)
    {
        LicenseNumber = licenseNumber;
        Name = name;
        PhoneNumber = phoneNumber;
        ContactEmail = contactEmail;
        StaffSpecialization = staffSpecialization;
        Status = status;
    }
    
    public void Deactivate()
    {
        Status = new Status("deactivated");
    }
}