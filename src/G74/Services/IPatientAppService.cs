namespace G74.Services;

public interface IPatientAppService
{
    Task<PatientDTO> RegisterPatient(PatientDTO patientDto);
    Task<PatientDTO> GetPatientByEmail(string email);
    Task<PatientDTO> GetPatientById(long id);
    Task<PatientDTO> UpdatePatient(long id, PatientDTO updatedPatientDto);
}