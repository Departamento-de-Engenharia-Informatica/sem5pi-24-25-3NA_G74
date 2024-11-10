using G74.DataModel;
using G74.Domain;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.Staff;
using G74.Domain.Value_Objects.User;
using G74.DTO;

namespace G74.Mappers;

public class PatientMapper
{
    public PatientDTO ToDTO(Patient patient)
    {
        ArgumentNullException.ThrowIfNull(patient);

        DateOfBirthDTO dateOfBirthDto = new DateOfBirthDTO(patient.DateOfBirth.dateOfBirth.Year,
            patient.DateOfBirth.dateOfBirth.Month,
            patient.DateOfBirth.dateOfBirth.Day);

        ContactInformationDTO contactInformationDto = new ContactInformationDTO(patient.ContactInformation.PhoneNumber,
            patient.ContactInformation.EmailAddress.email);

        EmergencyContactDTO emergencyContactDto =
            new EmergencyContactDTO(patient.EmergencyContact.Name.Value, patient.EmergencyContact.PhoneNumber);


        return new PatientDTO(patient.Name.Value, patient.Gender.GenderDescription, dateOfBirthDto,
            contactInformationDto, emergencyContactDto);
    }


    public Patient ToDomain(PatientDTO patientDto, string medicalRecordNumber)
    {
        ArgumentNullException.ThrowIfNull(patientDto);
        ArgumentNullException.ThrowIfNull(medicalRecordNumber);

        
        if (string.IsNullOrEmpty(patientDto.Name))
        {
            throw new ArgumentNullException(nameof(patientDto.Name));
        }
        Name name = new Name(patientDto.Name);
        

        MedicalRecordNumber medicalnumber = new MedicalRecordNumber(medicalRecordNumber);

        DateOfBirth dateOfBirth = new DateOfBirth(patientDto.DateOfBirth.YearOfBirth,
            patientDto.DateOfBirth.MonthOfBirth, patientDto.DateOfBirth.DayOfBirth);

        Gender gender = Gender.FromString(patientDto.Gender);

        Email email = new Email(patientDto.ContactInformation.EmailAddress);

        ContactInformation contactInformation =
            new ContactInformation(patientDto.ContactInformation.PhoneNumber, email);

        EmergencyContact emergencyContact = new EmergencyContact(patientDto.EmergencyContact.PhoneNumber,
            new Name(patientDto.EmergencyContact.Name));
            
        return new Patient(name, medicalnumber, dateOfBirth, gender, contactInformation, emergencyContact);
    }
}