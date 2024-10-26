using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Domain;

public class Patient : IAggregateRoot
{
    public Name Name { get; private set; }

    public MedicalRecordNumber MedicalRecordNumber { get; private set; }

    public DateOfBirth DateOfBirth { get; private set; }

    public Gender Gender { get; private set; }

    public ContactInformation ContactInformation { get; private set; }

    public EmergencyContact EmergencyContact { get; private set; }

    public AppointmentHistory AppointmentHistory { get; private set; }

    public MedicalCondition MedicalCondition { get; private set; }


    public Patient(Name name,
        MedicalRecordNumber medicalRecordNumber,
        DateOfBirth dateOfBirth,
        Gender gender,
        ContactInformation contactInformation,
        EmergencyContact emergencyContact
    )
    {
        PatientValidations(name, medicalRecordNumber, dateOfBirth, gender, contactInformation, emergencyContact);
        Name = name;
        MedicalRecordNumber = medicalRecordNumber;
        DateOfBirth = dateOfBirth;
        Gender = gender;
        ContactInformation = contactInformation;
        EmergencyContact = emergencyContact;
    }

    private void PatientValidations(Name name,
        MedicalRecordNumber medicalRecordNumber,
        DateOfBirth dateOfBirth,
        Gender gender,
        ContactInformation contactInformation,
        EmergencyContact emergencyContact)
    {
        if (name == null)
            throw new ArgumentNullException(nameof(name), "Name cannot be null creating a patient");
        if (medicalRecordNumber == null)
            throw new ArgumentNullException(nameof(medicalRecordNumber),
                "Medical Record Number cannot be null creating a patient");
        if (dateOfBirth == null)
            throw new ArgumentNullException(nameof(dateOfBirth), "Date of Birth cannot be null creating a patient");
        if (gender == null)
            throw new ArgumentNullException(nameof(gender), "Gender cannot be null creating a patient");
        if (contactInformation == null)
            throw new ArgumentNullException(nameof(contactInformation),
                "Contact information cannot be null creating a patient");
        if (emergencyContact == null)
            throw new ArgumentNullException(nameof(emergencyContact),
                "Emergency contact cannot be null creating a patient");
    }
}