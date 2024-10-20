using G74.Domain;
using G74.Domain.Aggregates.Patient;
using G74.DTO;

namespace G74.Services;

public interface IPatientAppService
{
    Task<PatientDTO> RegisterPatient(PatientDTO patientDto);
    Task<PatientDTO> GetPatientByEmail(string email);
    Task<PatientDTO> GetPatientById(long id);
}