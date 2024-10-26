namespace G74.DTO;

public class CreatePatientDTO
{
    public string Name { get; set; }

    public string Gender { get; set; }

    public DateOfBirthDTO DateOfBirth { get; set; }

    public ContactInformationDTO ContactInformation { get; set; }

    public EmergencyContactDTO EmergencyContact { get; set; }

    public CreatePatientDTO(string name, string gender, DateOfBirthDTO dateOfBirth,
        ContactInformationDTO contactInformation, EmergencyContactDTO emergencyContact)
    {
        Name = name;
        Gender = gender;
        DateOfBirth = dateOfBirth;
        ContactInformation = contactInformation;
        EmergencyContact = emergencyContact;
    }
}