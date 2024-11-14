using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.User;
using JetBrains.Annotations;
using Xunit;

namespace G74.Tests.Domain.Aggregates.Staff;

[TestSubject(typeof(G74.Domain.Aggregates.Staff.Staff))]
public class StaffTest
{
    private readonly LicenseNumber _validLicenseNumber;
    private readonly Name _validName;
    private readonly PhoneNumber _validPhone;
    private readonly Email _validEmail;
    private readonly StaffSpecialization _validSpecialization;
    private readonly Status _activeStatus;

    public StaffTest()
    {
        _validLicenseNumber = new LicenseNumber("12345");
        _validName = new Name("John Doe");
        _validPhone = new PhoneNumber("+1234567890");
        
        _validEmail = new Email("john.doe@example.com");
        _validSpecialization = new StaffSpecialization("General");
        _activeStatus = new Status("active");
    }

    
}