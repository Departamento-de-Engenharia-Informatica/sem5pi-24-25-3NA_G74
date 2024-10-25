using G74.Domain.Aggregates.Staff;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;

namespace G74.Domain.Factory;

public  class StaffFactory
{
    public  Staff NewStaff(String licenseNumber, String name, String phoneNumber,
        String contactEmail, String staffSpecialization)
    {
        return new Staff(new LicenseNumber(licenseNumber), new Name(name), new PhoneNumber(phoneNumber),
            new Email(contactEmail), new StaffSpecialization(staffSpecialization));
    }
}