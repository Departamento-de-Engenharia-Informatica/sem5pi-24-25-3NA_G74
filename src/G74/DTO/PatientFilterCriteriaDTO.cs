namespace G74.DTO;

public class PatientFilterCriteriaDTO
{
    public string? Name { get; set; }
    public string? Email { get; set; }
    public string? PhoneNumber { get; set; }
    public string? MedicalRecordNumber { get; set; }
    public DateOfBirthDTO? DateOfBirth { get; set; }
    public string? Gender { get; set; }

    
    public PatientFilterCriteriaDTO() { }

    // Construtor
    public PatientFilterCriteriaDTO(
        string? name = null,
        string? email = null,
        string? phoneNumber = null,
        string? medicalRecordNumber = null,
        DateOfBirthDTO? dateOfBirth = null,
        string? gender = null
    )
    {
        Name = name;
        Email = email;
        PhoneNumber = phoneNumber;
        MedicalRecordNumber = medicalRecordNumber;
        DateOfBirth = dateOfBirth;
        Gender = gender;
    }
}