using G74.Domain;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;

namespace G74.DataModel;

public class PatientDataModel : Entity<Guid>
{
    public string PatientName { get; set; }

    public string MedicalRecordNumber { get; set; }

    public string PatientGender { get; set; }

    public DateOnly BirthDate { get; set; }

    public string PersonalPhoneNumber { get; set; }

    public string PersonalEmail { get; set; }

    public string EmergencyContactName { get; set; }

    public string EmergencyContactPhoneNumber { get; set; }

    public bool MarkedForDeletion { get; set; }

    public DateTime? DateToBeDeleted { get; set; }

    protected PatientDataModel() : base(Guid.NewGuid())
    {
    }

    public PatientDataModel(string patientName, string medicalRecordNumber, string patientGender,
        DateOnly birthDate, string personalPhoneNumber, string personalEmail, string emergencyContactName,
        string emergencyContactPhoneNumber) : base(Guid.NewGuid())
    {
        PatientName = patientName;
        MedicalRecordNumber = medicalRecordNumber;
        PatientGender = patientGender;
        BirthDate = birthDate;
        PersonalPhoneNumber = personalPhoneNumber;
        PersonalEmail = personalEmail;
        EmergencyContactName = emergencyContactName;
        EmergencyContactPhoneNumber = emergencyContactPhoneNumber;
        MarkedForDeletion = false;
        DateToBeDeleted = null;
    }
    
    public void UpdateName(string newName)
    {
        PatientName = Name.ValidateAndTrimName(newName);
    }

    public void UpdateGender(string newGender)
    {
        PatientGender = Gender.FromString(newGender).GenderDescription;
    }

    public void UpdateDateOfBirth(int year, int month, int day)
    {
        BirthDate = new DateOnly(year,month,day);
    }

    public void UpdatePersonalPhoneNumber(string newPersonalPhoneNumber)
    {
        PersonalPhoneNumber = ContactInformation.ValidatePhoneNumber(newPersonalPhoneNumber);
    }

    public void UpdatePersonalEmail(string newPersonalEmail)
    {
        PersonalEmail = Email.ValidateEmail(newPersonalEmail).email;
    }

    public void UpdateEmergencyContactName(string newEmergencyContactName)
    {
        EmergencyContactName = Name.ValidateAndTrimName(newEmergencyContactName);
    }

    public void UpdateEmergencyContactPhoneNumber(string newEmergencyContactPhoneNumber)
    {
        EmergencyContactPhoneNumber = EmergencyContact.ValidatePhoneNumber(newEmergencyContactPhoneNumber);
    }


    public void MarkForDeletion(TimeSpan timeToDeletion)
    {
        if (timeToDeletion != null)
        {
            MarkedForDeletion = true;
            DateToBeDeleted = DateTime.UtcNow.Add(timeToDeletion);
        }
    }
}