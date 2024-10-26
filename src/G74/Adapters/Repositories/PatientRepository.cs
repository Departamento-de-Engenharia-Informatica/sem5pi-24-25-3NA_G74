using G74.DataModel;
using G74.Domain;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;
using G74.Domain.Value_Objects.User;
using G74.DTO;
using G74.Infrastructure.Shared;
using Microsoft.EntityFrameworkCore;

namespace G74.Adapters.Repositories;

public class PatientRepository : BaseRepository<PatientDataModel, Guid>, IPatientRepository
{
    private readonly BackofficeAppDbContext _context;

    public PatientRepository(BackofficeAppDbContext context) : base(context.Patients)
    {
        _context = context;
    }

    public async Task AddPatient(PatientDataModel patient)
    {
        await AddAsync(patient);
        await _context.SaveChangesAsync();
    }

    public async Task<PatientDataModel?> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber)
    {
        var patient =
            await _context.Patients.FirstOrDefaultAsync(x => x.MedicalRecordNumber.Equals(medicalRecordNumber));

        if (patient != null && patient.ToDelete == false)
        {
            return patient;
        }

        return null;
    }

    public Task<Patient> GetPatientByEmail(string email)
    {
        throw new NotImplementedException();
    }

    public async Task UpdatePatient(PatientDataModel patient)
    {
        _context.Patients.Update(patient);
        await _context.SaveChangesAsync();
    }

    public async Task<List<PatientDataModel>> GetPatientsReadyForDeletion()
    {
        DateTime currentTime = DateTime.Now;

        return await _context.Patients
            .Where(p => p.DeletionInformation.ToDelete && p.DeletionInformation.DateToBeDeleted <= currentTime)
            .ToListAsync();
    }

    public async Task DeletePatientDefinitive(PatientDataModel patient)
    {
        _context.Patients.Remove(patient);
        await _context.SaveChangesAsync();
    }


    public async Task<IEnumerable<PatientDataModel>> SearchPatientsByFiltersAsync(PatientFilterCriteriaDTO criteria)
    {
        IQueryable<PatientDataModel> myQueryable = _context.Patients;

        myQueryable = ApplyNameFilter(myQueryable, criteria.Name);
        myQueryable = ApplyContactInformationFilter(myQueryable, criteria.Email, criteria.PhoneNumber);
        
        myQueryable = ApplyMedicalRecordNumberFilter(myQueryable, criteria.MedicalRecordNumber);
        //myQueryable = ApplyDateOfBirthFilter(myQueryable, criteria.DateOfBirth);
        myQueryable = ApplyGenderFilter(myQueryable, criteria.Gender);

        var patientsFound = await myQueryable.ToListAsync();

        return patientsFound;
    }

    private IQueryable<PatientDataModel> ApplyNameFilter(IQueryable<PatientDataModel> query, string name)
    {
        if (!string.IsNullOrEmpty(name))
        {
            return query.Where(p => p.Name.Equals(new Name(name)));
        }

        return query;
    }


    private static IQueryable<PatientDataModel> ApplyContactInformationFilter(IQueryable<PatientDataModel> query, string email, string phoneNumber)
    {
        if (!string.IsNullOrEmpty(email) && !string.IsNullOrEmpty(phoneNumber))
        {
            return query.Where(p => p.ContactInformation.Equals(ContactInformation.FromString($"{phoneNumber};{email}")));
        }

        return query;
    }

    private static IQueryable<PatientDataModel> ApplyPhoneNumberFilter(IQueryable<PatientDataModel> query,
        string phoneNumber)
    {
        if (!string.IsNullOrEmpty(phoneNumber))
        {
            return query.Where(p => p.ContactInformation.PhoneNumber.Equals(phoneNumber));
        }

        return query;
    }

    private static IQueryable<PatientDataModel> ApplyMedicalRecordNumberFilter(IQueryable<PatientDataModel> query,
        string medicalRecordNumber)
    {
        if (!string.IsNullOrEmpty(medicalRecordNumber))
        {
            return query.Where(p => p.MedicalRecordNumber.Equals(new MedicalRecordNumber(medicalRecordNumber)));
        }

        return query;
    }
    // NOT USED
    private static IQueryable<PatientDataModel> ApplyDateOfBirthFilter(IQueryable<PatientDataModel> query,
        DateOfBirthDTO dateOfBirthDto)
    {
        if (dateOfBirthDto != null)
        {
            query = query.Where(p => p.YearOfBirth == dateOfBirthDto.YearOfBirth);
            query = query.Where(p => p.MonthOfBirth == dateOfBirthDto.MonthOfBirth);
            query = query.Where(p => p.DayOfBirth == dateOfBirthDto.DayOfBirth);
        }

        return query;
    }


    private static IQueryable<PatientDataModel> ApplyGenderFilter(IQueryable<PatientDataModel> query, string gender)
    {
        if (!string.IsNullOrEmpty(gender))
        {
            Gender theGender = Gender.FromString(gender);

            return query.Where(p => p.Gender.Equals(theGender));
        }

        return query;
    }
}