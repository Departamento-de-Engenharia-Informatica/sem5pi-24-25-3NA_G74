using G74.DataModel;
using G74.Domain;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;

namespace G74.Mappers;

public class PatientDataModelMapper
{
    public Patient ToDomain(PatientDataModel patientDataModel)
    {
        Name name = new Name(patientDataModel.PatientName);

        MedicalRecordNumber medicalRecordNumber = new MedicalRecordNumber(patientDataModel.MedicalRecordNumber);

        DateOfBirth dateOfBirth = new DateOfBirth(patientDataModel.BirthDate.Year, patientDataModel.BirthDate.Month,
            patientDataModel.BirthDate.Day);

        Gender gender = Gender.FromString(patientDataModel.PatientGender);

        ContactInformation contactInformation = new ContactInformation(patientDataModel.PersonalPhoneNumber,
            new Email(patientDataModel.PersonalEmail));

        EmergencyContact emergencyContact = new EmergencyContact(patientDataModel.EmergencyContactPhoneNumber,
            new Name(patientDataModel.EmergencyContactName));

        return new Patient(name, medicalRecordNumber, dateOfBirth, gender, contactInformation, emergencyContact);
    }

    public PatientDataModel ToDataModel(Patient patient)
    {
        string patientName = patient.Name.Value;

        string medicalRecordNumber = patient.MedicalRecordNumber.MedicalNumber;

        string patientGender = patient.Gender.GenderDescription;

        DateOnly birthDate = new DateOnly(patient.DateOfBirth.dateOfBirth.Year, patient.DateOfBirth.dateOfBirth.Month,
            patient.DateOfBirth.dateOfBirth.Day);

        string personalPhoneNumber = patient.ContactInformation.PhoneNumber;

        string personalEmail = patient.ContactInformation.EmailAddress.email;

        string emergencyContactName = patient.EmergencyContact.Name.Value;

        string emergencyContactPhoneNumber = patient.EmergencyContact.PhoneNumber;

        return new PatientDataModel(patientName, medicalRecordNumber, patientGender, birthDate, personalPhoneNumber,
            personalEmail, emergencyContactName, emergencyContactPhoneNumber);
    }

    public bool UpdateDataModel(PatientDataModel patientDataModel, Patient patient)
    {
        patientDataModel.UpdateName(patient.Name.Value);
        patientDataModel.UpdateGender(patient.Gender.GenderDescription);
        patientDataModel.UpdateDateOfBirth(patient.DateOfBirth.dateOfBirth.Year, patient.DateOfBirth.dateOfBirth.Month,
            patient.DateOfBirth.dateOfBirth.Day);
        
        patientDataModel.UpdatePersonalPhoneNumber(patient.ContactInformation.PhoneNumber);
        patientDataModel.UpdatePersonalEmail(patient.ContactInformation.EmailAddress.email);
        patientDataModel.UpdateEmergencyContactName(patient.EmergencyContact.Name.Value);
        patientDataModel.UpdateEmergencyContactPhoneNumber(patient.EmergencyContact.PhoneNumber);


        return true;
    }

    public IEnumerable<Patient> ToDomain(IEnumerable<PatientDataModel> patientDataModels)
    {
        var patientList = patientDataModels.Select(ToDomain).ToList();

        return patientList.AsEnumerable();
    }
}