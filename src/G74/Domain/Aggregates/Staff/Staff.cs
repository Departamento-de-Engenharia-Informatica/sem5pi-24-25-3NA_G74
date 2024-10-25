using G74.Domain.Shared;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using Microsoft.OpenApi.Models;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.Patient;
public class Staff : IAggregateRoot
{
    public Username Username { get; private set; }
    public Email Email { get; private set; }
    public Name Name { get; private set; }
    public LicenceNumber LicenceNumber { get; private set; }
    public SpecializationStaff Specialization { get; private set; }

   public ContactInformation ContactInformation { get; private set; }
   public Role Role { get; private set; }

    public Staff(){}

    public Staff(Username username, Email email, Name name, LicenceNumber licenceNumber, SpecializationStaff specialization, ContactInformation contactInformation, Role role)
    {
        Username = username;
        Email = email;
        Name = name;
        LicenceNumber = licenceNumber;
        Specialization = specialization;
        ContactInformation = contactInformation;
        Role = role;

    }


}