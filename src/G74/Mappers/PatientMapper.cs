using G74.DataModel;
using G74.Domain;
using G74.DTO;

namespace G74.Mappers;

public class PatientMapper
{
    public static PatientDTO ToDTO(Patient patient)
    {
        ArgumentNullException.ThrowIfNull(patient);

        return new PatientDTO(patient.Name, patient.Gender.GenderDescription,
            patient.DateOfBirth, patient.ContactInformation, patient.EmergencyContact);
    }

    public static Patient toDomain(PatientDTO patientDto)
    {
        throw new NotImplementedException();
    }

    public static PatientDataModel ToDataModel(Patient patient)
    {
        ArgumentNullException.ThrowIfNull(patient, nameof(Patient));

        return new PatientDataModel(patient);
    }

    public static CreatePatientDTO FromDataModelToCreatePatientDto(PatientDataModel patientDataModel)
    {
        ArgumentNullException.ThrowIfNull(patientDataModel);

        return new CreatePatientDTO(patientDataModel.Name.TheName, patientDataModel.Gender.GenderDescription,
            new DateOfBirthDTO(patientDataModel.DateOfBirth.YearOfBirth, patientDataModel.DateOfBirth.MonthOfBirth,
                patientDataModel.DateOfBirth.DayOfBirth),
            new ContactInformationDTO(patientDataModel.ContactInformation.PhoneNumber,
                patientDataModel.ContactInformation.EmailAddress.email),
            new EmergencyContactDTO(patientDataModel.EmergencyContact._phoneNumber));
    }
}