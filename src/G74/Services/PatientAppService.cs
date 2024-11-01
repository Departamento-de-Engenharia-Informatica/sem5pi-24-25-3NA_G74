using G74.DataModel;
using G74.Domain;
using G74.Domain.DomainServices;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using G74.Mappers;

namespace G74.Services;

public class PatientAppService : IPatientAppService
{
    private readonly IPatientRepository _patientRepository;

    private readonly IMedicalRecordNumberGenerator _medicalRecordNumberGenerator;
    private readonly IConfiguration _configuration;

    private readonly PatientMapper _patientMapper;

    public PatientAppService(IPatientRepository patientRepository, PatientMapper mapper,
        IMedicalRecordNumberGenerator medicalRecordNumberGenerator, IConfiguration configuration)
    {
        _patientRepository = patientRepository;
        _medicalRecordNumberGenerator = medicalRecordNumberGenerator;
        _patientMapper = mapper;
        _configuration = configuration;
    }

    public async Task<PatientDTO> RegisterPatient(PatientDTO patientDto)
    {
        MedicalRecordNumber medicalRecordNumber = _medicalRecordNumberGenerator.GenerateMedicalNumber().Result;

        Patient addedPatient =
            await _patientRepository.AddPatient(_patientMapper.ToDomain(patientDto, medicalRecordNumber.MedicalNumber));

        return _patientMapper.ToDTO(addedPatient);
    }


    public async Task<PatientDTO> UpdatePatientLimited(string medicalRecordNumber,
        PatientDTO updatedInfoPatientDto)
    {
        var patient = _patientRepository.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(medicalRecordNumber))
            .Result;

        ArgumentNullException.ThrowIfNull(patient);

        if (updatedInfoPatientDto.Name != null)
        {
            patient.UpdateName(new Name(updatedInfoPatientDto.Name));
        }

        if (updatedInfoPatientDto.DateOfBirth != null)
        {
            DateOfBirth newDateOfBirth = new DateOfBirth(updatedInfoPatientDto.DateOfBirth.YearOfBirth,
                updatedInfoPatientDto.DateOfBirth.MonthOfBirth, updatedInfoPatientDto.DateOfBirth.DayOfBirth);

            patient.UpdateDateOfBirth(newDateOfBirth);
        }

        if (updatedInfoPatientDto.ContactInformation != null)
        {
            patient.UpdateContactInformation(new ContactInformation(
                updatedInfoPatientDto.ContactInformation.PhoneNumber,
                new Email(updatedInfoPatientDto.ContactInformation.EmailAddress)));
        }

        var updatedPatient = _patientRepository.UpdatePatient(patient).Result;

        if (updatedPatient == null)
        {
            throw new InvalidOperationException("Could not update patient info");
        }

        return _patientMapper.ToDTO(updatedPatient);
    }

    public async Task<PatientDTO> UpdatePatientComplete(string medicalRecordNumber,
        PatientDTO updatedInfoPatientDto)
    {
        var patient = _patientRepository.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(medicalRecordNumber))
            .Result;

        ArgumentNullException.ThrowIfNull(patient);

        if (updatedInfoPatientDto.Name != null)
        {
            patient.UpdateName(new Name(updatedInfoPatientDto.Name));
        }

        if (updatedInfoPatientDto.DateOfBirth != null)
        {
            DateOfBirth newDateOfBirth = new DateOfBirth(updatedInfoPatientDto.DateOfBirth.YearOfBirth,
                updatedInfoPatientDto.DateOfBirth.MonthOfBirth, updatedInfoPatientDto.DateOfBirth.DayOfBirth);

            patient.UpdateDateOfBirth(newDateOfBirth);
        }

        if (updatedInfoPatientDto.ContactInformation != null)
        {
            patient.UpdateContactInformation(new ContactInformation(
                updatedInfoPatientDto.ContactInformation.PhoneNumber,
                new Email(updatedInfoPatientDto.ContactInformation.EmailAddress)));
        }

        if (updatedInfoPatientDto.EmergencyContact != null)
        {
            patient.UpdateEmergencyContact(new EmergencyContact(updatedInfoPatientDto.EmergencyContact.PhoneNumber,
                new Name(updatedInfoPatientDto.EmergencyContact.Name)));
        }
        
        var updatedPatient = _patientRepository.UpdatePatient(patient).Result;

        if (updatedPatient == null)
        {
            throw new InvalidOperationException("Could not update patient info");
        }

        return _patientMapper.ToDTO(updatedPatient);
    }


    public async Task MarkPatientToBeDeleted(string medicalRecordNumber)
    {
        var existingPatient =
            await _patientRepository.GetPatientByMedicalRecordNumber(new MedicalRecordNumber(medicalRecordNumber));

        if (existingPatient == null)
        {
            throw new InvalidOperationException("Patient not found");
        }

        string time = _configuration["GPRD:RetainInfoPeriod"] ?? "2m";

        TimeSpan retainInfoPeriod;
        if (time.EndsWith("m"))
        {
            retainInfoPeriod = TimeSpan.FromMinutes(double.Parse(time.TrimEnd('m')));
        }
        else if (time.EndsWith("h"))
        {
            retainInfoPeriod = TimeSpan.FromHours(double.Parse(time.TrimEnd('h')));
        }
        else if (time.EndsWith("d"))
        {
            retainInfoPeriod = TimeSpan.FromDays(double.Parse(time.TrimEnd('d')));
        }
        else
        {
            throw new ArgumentException("Invalid time format. Use 'm' for minutes, 'h' for hours, or 'd' for days.");
        }

        await _patientRepository.MarkPatientToBeDeleted(existingPatient, retainInfoPeriod);
    }


    public async Task<PatientDTO?> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber)
    {
        var patient = _patientRepository.GetPatientByMedicalRecordNumber(medicalRecordNumber);

        if (patient.Result != null)
        {
            return _patientMapper.ToDTO(patient.Result);
        }

        return null;
    }


    public async Task<IEnumerable<PatientDTO>> SearchPatientsByFilters(PatientDTO criteria)
    {
        try
        {
            var patientsFound = await _patientRepository.SearchPatientsByFiltersAsync(
                criteria.Name,
                criteria.Gender,
                criteria.ContactInformation?.PhoneNumber,
                criteria.ContactInformation?.EmailAddress,
                criteria.DateOfBirth
            );


            return patientsFound.Select(_patientMapper.ToDTO).ToList() ?? new List<PatientDTO>();
        }
        catch (Exception ex)
        {
            throw new Exception($"An error occurred while searching for patients: {ex.Message}", ex);
        }
    }
}