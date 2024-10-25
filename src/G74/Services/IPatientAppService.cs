using G74.DTO;

namespace G74.Services;

public interface IPatientAppService
{
    Task<PatientDTO> RegisterPatient(CreatePatientDTO patientDto);
    Task<CreatePatientDTO> UpdatePatient(string medicalRecordNumber, CreatePatientDTO updatedInfoPatientDto);
    Task MarkPatientToBeDeleted(string medicalRecordNumber);
}