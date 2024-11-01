using G74.Domain;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.DTO;

public class PatientDTO
{
    public string? Name { get; set; }

    public string? Gender { get; set; }

    public DateOfBirthDTO? DateOfBirth { get; set; }

    public ContactInformationDTO? ContactInformation { get; set; }

    public EmergencyContactDTO? EmergencyContact { get; set; }

    public PatientDTO() { }

    public PatientDTO(string? name = null, string? gender = null, DateOfBirthDTO? dateOfBirth = null,
        ContactInformationDTO? contactInformation = null,
        EmergencyContactDTO? emergencyContact = null)
    {
        Name = name;
        Gender = gender;
        DateOfBirth = dateOfBirth;
        ContactInformation = contactInformation;
        EmergencyContact = emergencyContact;
    }
}