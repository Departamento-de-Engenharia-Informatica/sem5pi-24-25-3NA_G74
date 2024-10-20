using G74.Domain;
using G74.Domain.Aggregates.Patient;
using G74.Domain.Aggregates.User;
using G74.Domain.Builders;
using G74.Domain.DomainServices;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.DTO;
using G74.Mappers;

namespace G74.Services;

public class PatientAppService : IPatientAppService
{
    private readonly IPatientRepository _patientRepository;
    private readonly IRepoUser _repoUser;
    private readonly IMedicalRecordNumberGenerator _medicalRecordNumberGenerator;


    public PatientAppService(IPatientRepository patientRepository, IRepoUser repoUser,
        IMedicalRecordNumberGenerator medicalRecordNumberGenerator)
    {
        _patientRepository = patientRepository;
        _medicalRecordNumberGenerator = medicalRecordNumberGenerator;
        _repoUser = repoUser;
    }

    public async Task<PatientDTO> RegisterPatient(PatientDTO patientDto)
    {
        User patientUser = _repoUser.GetUserByEmail(patientDto.ContactInformation.EmailAddress);

        if (patientUser == null)
            throw new ArgumentException("There are no records of user with this email");


        if (!Enum.TryParse<Gender.GenderEnum>(patientDto.Gender, true, out var gender))
        {
            throw new ArgumentException("Invalid gender");
        }

        var patient = await new PatientBuilder(_medicalRecordNumberGenerator, patientDto.Name,
                patientDto.DateOfBirth, new Gender(gender), patientDto.ContactInformation, patientDto.EmergencyContact)
            .Build();


        await _patientRepository.Add(patient);

        return PatientMapper.ToDTO(patient);
    }

    public async Task<PatientDTO> GetPatientByEmail(string email)
    {
        var existingPatient = await _patientRepository.GetPatientByEmail(email);

        return PatientMapper.ToDTO(existingPatient);
    }

    public async Task<PatientDTO> GetPatientById(long id)
    {
        var existingPatient = await _patientRepository.GetPatientByIdAysnc(id);

        return PatientMapper.ToDTO(existingPatient);
    }
}