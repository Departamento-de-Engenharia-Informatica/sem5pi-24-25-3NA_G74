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
        
    }
    
    
}