using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Domain;

public class Patient : IAggregateRoot, IEquatable<Patient>
{
    public Name Name { get; set; }

    public MedicalRecordNumber MedicalRecordNumber { get; }

    public DateOfBirth DateOfBirth { get; set; }

    public Gender Gender { get; set; }

    public ContactInformation ContactInformation { get; set; }

    public EmergencyContact EmergencyContact { get; set; }

    public AppointmentHistory? AppointmentHistory { get; set; }

    public MedicalCondition? MedicalCondition { get; set; }


    public Patient(
        Name name,
        MedicalRecordNumber medicalRecordNumber,
        DateOfBirth dateOfBirth,
        Gender gender,
        ContactInformation contactInformation,
        EmergencyContact emergencyContact
    )
    {
        Name = name ?? throw new ArgumentNullException(nameof(name), "Name cannot be null creating a patient");

        MedicalRecordNumber = medicalRecordNumber ?? throw new ArgumentNullException(nameof(medicalRecordNumber),
            "Medical Record Number cannot be null creating a patient");

        DateOfBirth = dateOfBirth ?? throw new ArgumentNullException(nameof(dateOfBirth),
            "Date of Birth cannot be null creating a patient");

        Gender = gender ?? throw new ArgumentNullException(nameof(gender), "Gender cannot be null creating a patient");

        ContactInformation = contactInformation ?? throw new ArgumentNullException(nameof(contactInformation),
            "Contact information cannot be null creating a patient");

        EmergencyContact = emergencyContact ?? throw new ArgumentNullException(nameof(emergencyContact),
            "Emergency contact cannot be null creating a patient");
    }

    public void UpdateName(Name newName)
    {
        Name = newName ?? throw new ArgumentNullException(nameof(newName), "Name cannot be null when updating");
    }

    public void UpdateDateOfBirth(DateOfBirth newDateOfBirth)
    {
        DateOfBirth = newDateOfBirth ??
                      throw new ArgumentNullException(nameof(newDateOfBirth), "Name cannot be null when updating");
    }

    public void UpdateGender(Gender newGender)
    {
        Gender = newGender ?? throw new ArgumentNullException(nameof(newGender), "Name cannot be null when updating");
    }

    public void UpdateAppointmentHistory(AppointmentHistory newAppointmentHistory)
    {
        AppointmentHistory = newAppointmentHistory ?? throw new ArgumentNullException(nameof(newAppointmentHistory),
            "Name cannot be null when updating");
    }

    public void UpdateMedicalCondition(MedicalCondition newMedicalCondition)
    {
        MedicalCondition = newMedicalCondition ?? throw new ArgumentNullException(nameof(newMedicalCondition),
            "Name cannot be null when updating");
    }

    public void UpdateContactInformation(ContactInformation newContactInformation)
    {
        ContactInformation = newContactInformation ?? throw new ArgumentNullException(nameof(newContactInformation),
            "Contact information cannot be null when updating.");
    }

    public void UpdateEmergencyContact(EmergencyContact newEmergencyContact)
    {
        EmergencyContact = newEmergencyContact ?? throw new ArgumentNullException(nameof(newEmergencyContact),
            "Emergency contact cannot be null when updating.");
    }

    public bool Equals(Patient? other)
    {
        return other != null &&
               Name.Equals(other.Name) &&
               MedicalRecordNumber.Equals(other.MedicalRecordNumber) &&
               DateOfBirth.Equals(other.DateOfBirth) &&
               Gender.Equals(other.Gender);
    }

    public override bool Equals(object? obj) => obj is Patient other && Equals(other);

    public override int GetHashCode() => HashCode.Combine(Name, MedicalRecordNumber, DateOfBirth, Gender);
}