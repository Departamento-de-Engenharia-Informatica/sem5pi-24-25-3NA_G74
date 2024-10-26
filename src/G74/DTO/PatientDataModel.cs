using G74.Domain;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.DataModel;

public class PatientDataModel : Entity<Guid>
{
    public Name Name { get; private set; }

    public MedicalRecordNumber MedicalRecordNumber { get; private set; }

    public DateOfBirth DateOfBirth { get; private set; }

    public Gender Gender { get; private set; }

    public ContactInformation ContactInformation { get; private set; }

    public EmergencyContact EmergencyContact { get; private set; }

    public DeletionInformation DeletionInformation { get; private set; }
    
    public bool ToDelete => DeletionInformation?.ToDelete ?? false;
    public DateTime? DateToBeDeleted => DeletionInformation?.DateToBeDeleted;


    protected PatientDataModel() : base(Guid.NewGuid())
    {
    }

    public PatientDataModel(Patient patient) : base(Guid.NewGuid())
    {
        Name = patient.Name;
        MedicalRecordNumber = patient.MedicalRecordNumber;
        DateOfBirth = patient.DateOfBirth;
        Gender = patient.Gender;
        ContactInformation = patient.ContactInformation;
        EmergencyContact = patient.EmergencyContact;
        DeletionInformation = new DeletionInformation(false, TimeSpan.FromMinutes(0));
    }

    public void UpdateName(Name newName)
    {
        Name = newName
               ?? throw new ArgumentNullException(nameof(newName));
    }

    public void UpdateDateOfBirth(DateOfBirth newDateOfBirth)
    {
        DateOfBirth = newDateOfBirth
                      ?? throw new ArgumentNullException(nameof(newDateOfBirth));
    }

    public void UpdateGender(Gender newGender)
    {
        Gender = newGender
                 ?? throw new ArgumentNullException(nameof(newGender));
    }

    public void UpdateContactInformation(ContactInformation newContactInformation)
    {
        ContactInformation = newContactInformation
                             ?? throw new ArgumentNullException(nameof(newContactInformation));
    }

    public void UpdateEmergencyContact(EmergencyContact newEmergencyContact)
    {
        EmergencyContact = newEmergencyContact
                           ?? throw new ArgumentNullException(nameof(newEmergencyContact));
    }

    public void MarkForDeletion(int minutes)
    {
        DeletionInformation = new DeletionInformation(true, TimeSpan.FromMinutes(minutes));
    }
}