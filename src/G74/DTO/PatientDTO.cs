using G74.Domain;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;

public class PatientDTO
{
    public Name Name { get; set; }

    public string Gender { get; set; }

    public DateOfBirth DateOfBirth { get; set; }

    public ContactInformation ContactInformation { get; set; }

    public EmergencyContact EmergencyContact { get; set; }


    public PatientDTO(Name name, string gender, DateOfBirth dateOfBirth,
        ContactInformation contactInformation, EmergencyContact emergencyContact)
    {
        Name = name;
        Gender = gender;
        DateOfBirth = dateOfBirth;
        ContactInformation = contactInformation;
        EmergencyContact = emergencyContact;
    }
}