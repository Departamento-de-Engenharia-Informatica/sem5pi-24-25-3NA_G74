using G74.Domain.Aggregates.User;
using G74.Domain.Builders;
using G74.Domain.DomainServices;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.Patient;
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

    public async Task<PatientDTO> RegisterPatient(CreatePatientDTO patientDto)
    {
        /*
        Task<User> patientUser = _repoUser.GetUserByEmail(patientDto.ContactInformation.EmailAddress.ToString());

        if (patientUser == null)
            throw new ArgumentException("There are no records of user with this email");

        */

        if (!Enum.TryParse<Gender.GenderEnum>(patientDto.Gender, true, out var gender))
        {
            throw new ArgumentException("Invalid gender");
        }

        var patient = await new PatientBuilder(
                _medicalRecordNumberGenerator, 
                new Name(patientDto.Name),
                new DateOfBirth(patientDto.DateOfBirth.YearOfBirth, patientDto.DateOfBirth.MonthOfBirth, patientDto.DateOfBirth.DayOfBirth), 
                new Gender(gender), 
                new ContactInformation(patientDto.ContactInformation.PhoneNumber, new Email(patientDto.ContactInformation.EmailAddress)), 
                new EmergencyContact(patientDto.EmergencyContact.PhoneNumber))
            .Build();

        var patientDataModel = PatientMapper.ToDataModel(patient);

        await _patientRepository.AddPatient(patientDataModel);

        return PatientMapper.ToDTO(patient);
    }

    public async Task<PatientDTO> GetPatientByEmail(string email)
    {
        var existingPatient = await _patientRepository.GetPatientByEmail(email);

        return PatientMapper.ToDTO(existingPatient);
    }

    public async Task<PatientDTO> GetPatientById(long id)
    {
        var existingPatient = await _patientRepository.GetPatientByIdAsync(id);

        return PatientMapper.ToDTO(existingPatient);
    }

    public async Task<PatientDTO> UpdatePatient(long id, PatientDTO updatedPatientDto)
    {
        var existingPatient = await _patientRepository.GetPatientByIdAsync(id);

        if (existingPatient == null)
        {
            throw new InvalidOperationException("Patient not found");
        }

        try
        {
            // Create value objects from DTO
            var name = new Name(updatedPatientDto.Name.TheName);
            var dateOfBirth = new DateOfBirth(updatedPatientDto.DateOfBirth);
            var gender = new Gender(Enum.Parse<Gender.GenderEnum>(updatedPatientDto.Gender));
            var contactInformation = new ContactInformation(updatedPatientDto.ContactInformation);
            var emergencyContact = new EmergencyContact(updatedPatientDto.EmergencyContact);

            // Update patient properties
            existingPatient.UpdateName(name);
            existingPatient.UpdateDateOfBirth(dateOfBirth);
            existingPatient.UpdateGender(gender);
            existingPatient.UpdateContactInformation(contactInformation);
            existingPatient.UpdateEmergencyContact(emergencyContact);

            // Save changes
            await _patientRepository.Update(existingPatient);

            // Log the changes
            await LogPatientChanges(id, PatientMapper.ToDTO(existingPatient));

            return PatientMapper.ToDTO(existingPatient);
        }
        catch (ArgumentException ex)
        {
            // Catch and rethrow any validation errors from the domain
            throw new InvalidOperationException($"Invalid patient data: {ex.Message}", ex);
        }
    }

    private static async Task LogPatientChanges(long patientId, PatientDTO updatedPatientDto)
    {
        // TODO: implement logging here. we're missing the dependency
    }
}