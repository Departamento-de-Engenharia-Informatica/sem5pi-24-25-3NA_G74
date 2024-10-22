public class DataPatient 
{
    public Guid Id { get; set; }
    public string Name { get; set; }
    public string Gender { get; set; }
    public string DateOfBirth { get; set; }
    public string ContactInformation { get; set; }
    public string EmergencyContact { get; set; }

    public DataPatient(Guid id, String name, String gender, String dateOfBirth, String contactInformation, String emergencyContact)
    {
        Id = id;
        Name = name;
        Gender = gender;
        DateOfBirth = dateOfBirth;
        ContactInformation = contactInformation;
        EmergencyContact = emergencyContact;
    }
}