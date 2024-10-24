using G74.DataModel;
using G74.Domain;

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
}