using G74.DataModel;
using G74.Domain;
using G74.Domain.Aggregates.User;
using G74.Domain.Builders;
using G74.Domain.DomainServices;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Mappers;

namespace G74.Services;

public class PatientAppService : IPatientAppService
{
    private readonly IPatientRepository _patientRepository;
    private readonly IRepoUser _repoUser;
    private readonly IMedicalRecordNumberGenerator _medicalRecordNumberGenerator;
    private readonly IConfiguration _configuration;


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
                new DateOfBirth(patientDto.DateOfBirth.YearOfBirth, patientDto.DateOfBirth.MonthOfBirth,
                    patientDto.DateOfBirth.DayOfBirth),
                new Gender(gender),
                new ContactInformation(patientDto.ContactInformation.PhoneNumber,
                    new Email(patientDto.ContactInformation.EmailAddress)),
                new EmergencyContact(patientDto.EmergencyContact.PhoneNumber))
            .Build();

        var patientDataModel = PatientMapper.ToDataModel(patient);

        await _patientRepository.AddPatient(patientDataModel);

        return PatientMapper.ToDTO(patient);
    }


    public async Task<CreatePatientDTO> UpdatePatient(string medicalRecordNumber, CreatePatientDTO updatedInfoPatientDto)
    {
        var existingPatient =
            await _patientRepository.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(medicalRecordNumber));

        if (existingPatient == null)
        {
            throw new InvalidOperationException("Patient not found");
        }

        try
        {
            UpdatePatientHelper(updatedInfoPatientDto, existingPatient);
            
            await _patientRepository.UpdatePatient(existingPatient);

            // Log the changes
            //await LogPatientChanges(id, PatientMapper.ToDTO(existingPatient));

            return PatientMapper.FromDataModelToCreatePatientDto(existingPatient);
        }
        catch (ArgumentException ex)
        {
            // Catch and rethrow any validation errors from the domain
            throw new InvalidOperationException($"Invalid patient data: {ex.Message}", ex);
        }
    }



    public void UpdatePatientHelper(CreatePatientDTO updatedInfoPatientInfoDto,
        PatientDataModel patientDataModelToUpdate)
    {
        
        if (!string.IsNullOrWhiteSpace(updatedInfoPatientInfoDto.Name))
        {
            patientDataModelToUpdate.UpdateName(new Name(updatedInfoPatientInfoDto.Name));
        }

        if (updatedInfoPatientInfoDto.DateOfBirth != null)
        {
            int year = updatedInfoPatientInfoDto.DateOfBirth.YearOfBirth;
            int month = updatedInfoPatientInfoDto.DateOfBirth.MonthOfBirth;
            int day = updatedInfoPatientInfoDto.DateOfBirth.DayOfBirth;

            patientDataModelToUpdate.UpdateDateOfBirth(new DateOfBirth(year, month, day));
        }

        if (!string.IsNullOrWhiteSpace(updatedInfoPatientInfoDto.Gender))
        {
            patientDataModelToUpdate.UpdateGender(Gender.FromString(updatedInfoPatientInfoDto.Gender));
        }

        if (updatedInfoPatientInfoDto.ContactInformation != null)
        {
            string phoneNumber = updatedInfoPatientInfoDto.ContactInformation.PhoneNumber;
            string emailAdress = updatedInfoPatientInfoDto.ContactInformation.EmailAddress;

            patientDataModelToUpdate.UpdateContactInformation(new ContactInformation(phoneNumber,
                new Email(emailAdress)));
        }

        if (updatedInfoPatientInfoDto.EmergencyContact != null)
        {
            string phoneNumber = updatedInfoPatientInfoDto.EmergencyContact.PhoneNumber;

            patientDataModelToUpdate.UpdateEmergencyContact(new EmergencyContact(phoneNumber));
        }
    }


    public async Task MarkPatientToBeDeleted(string medicalRecordNumber)
    {
        var existingPatient =
            await _patientRepository.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(medicalRecordNumber));

        if (existingPatient == null)
        {
            throw new InvalidOperationException("Patient not found");
        }

        int time = _configuration.GetValue<int>("GPRD:RetainInfoInMinutes");
        
        existingPatient.MarkForDeletion(time);

        await _patientRepository.UpdatePatient(existingPatient);

    }
    
    private static async Task LogPatientChanges(long patientId, PatientDTO updatedPatientDto)
    {
        // TODO: implement logging here. we're missing the dependency
    }

    public async Task<PatientDTO> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber)
    {
        PatientDataModel patient = await _patientRepository.GetPatientByMedicalRecordNumber(medicalRecordNumber);
        
        if (patient == null)
        {
            throw new InvalidOperationException($"Patient with medical record number '{medicalRecordNumber}' not found.");
        }
        
        var patientDto = new PatientDTO(            
            patient.Name,
            patient.Gender.GenderDescription,
            new DateOfBirth(patient.DateOfBirth.YearOfBirth, patient.DateOfBirth.MonthOfBirth, patient.DateOfBirth.DayOfBirth),
            new ContactInformation(patient.ContactInformation.PhoneNumber, patient.ContactInformation.EmailAddress),
            new EmergencyContact(patient.EmergencyContact._phoneNumber))
        {
        };

        return patientDto;
    }

}