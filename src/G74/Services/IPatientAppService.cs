using G74.Domain.Value_Objects.Patient;
using G74.DTO;

namespace G74.Services;

public interface IPatientAppService
{
    Task<PatientDTO> RegisterPatient(CreatePatientDTO patientDto);
    Task<PatientDTO> GetPatientByEmail(string email);
    Task<PatientDTO> GetPatientById(long id);
    Task<PatientDTO> UpdatePatient(long id, PatientDTO updatedPatientDto);
    Task<PatientDTO> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber);
}