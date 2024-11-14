using G74.Domain.Shared;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.User;

namespace G74.Domain.Aggregates.Staff;

public class Staff : Entity<Guid>, IAggregateRoot
{
    public LicenceNumber LicenceNumber { get; private set; }
    public Name Name { get; private set; }
    public PhoneNumber PhoneNumber { get; private set; }
    public Email ContactEmail { get; private set; }
    public StaffSpecialization StaffSpecialization { get; private set; }
    public Status Status { get; private set; }
    public string Availability { get; private set; }


    public Staff(LicenseNumber licenseNumber, Name name, PhoneNumber phoneNumber,
        Email contactEmail, StaffSpecialization staffSpecialization, Status status, string availability) : base(Guid.NewGuid())
    {
        LicenceNumber = licenseNumber;
        Name = name;
        PhoneNumber = phoneNumber;
        ContactEmail = contactEmail;
        StaffSpecialization = staffSpecialization;
        Status = status;
        Availability = availability;
    }

    private Staff()
    {
    }


    public void Deactivate()
    {
        Status = new Status("deactivated");
    }

    public static Staff Create(long licenseNumber, string name, string phoneNumber,
                                string contactEmail, string staffSpecialization, string status, string availability)
    {
        return new Staff(
            new LicenceNumber(licenseNumber),
            new Name(name),
            new PhoneNumber(phoneNumber),
            new Email(contactEmail),
            new StaffSpecialization(staffSpecialization),
            new Status(status),
            availability
        );
    }

    public void UpdateLicenseNumber(LicenceNumber licenseNumber)
    {
        this.LicenceNumber = licenseNumber;
    }

    public void UpdateName(Name name)
    {
        this.Name = name;
    }

    public void UpdatePhoneNumber(PhoneNumber phoneNumber)
    {
        this.PhoneNumber = phoneNumber;
    }

    public void UpdateContactEmail(Email email)
    {
        this.ContactEmail = email;
    }

    public void UpdateStaffSpecialization(StaffSpecialization specialization)
    {
        this.StaffSpecialization = specialization;
    }

    public void UpdateStatus(Status status)
    {
        this.Status = status;
    }


}