using G74.Domain.Aggregates.Patient;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Domain;

public class Patient : Entity<PatientId>, IAggregateRoot
{
    public Name Name { get; private set; }

    public MedicalRecordNumber MedicalRecordNumber { get; private set; }

    public DateOfBirth DateOfBirth { get; private set; }

    public Gender Gender { get; private set; }

    public ContactInformation ContactInformation { get; private set; }

    public EmergencyContact EmergencyContact { get; private set; }

    public AppointmentHistory AppointmentHistory { get; private set; }

    public MedicalCondition MedicalCondition { get; private set; }


    public Patient(Name name, MedicalRecordNumber medicalRecordNumber, DateOfBirth dateOfBirth,
        Gender gender,
        ContactInformation contactInformation, EmergencyContact emergencyContact)
    {
        Name = name;
        MedicalRecordNumber = medicalRecordNumber;
        DateOfBirth = dateOfBirth;
        Gender = gender;
        ContactInformation = contactInformation;
        EmergencyContact = emergencyContact;
    }
}