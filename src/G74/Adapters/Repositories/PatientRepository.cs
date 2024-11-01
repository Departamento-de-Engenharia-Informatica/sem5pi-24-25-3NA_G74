using G74.DataModel;
using G74.Domain;
using G74.Domain.IRepositories;
using G74.Domain.Value_Objects.Patient;
using G74.DTO;
using G74.Mappers;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.ChangeTracking;

namespace G74.Adapters.Repositories;

public class PatientRepository : GenericRepository<Patient>, IPatientRepository
{
    private readonly PatientDataModelMapper _patientMapper;

    public PatientRepository(BackofficeAppDbContext context, PatientDataModelMapper mapper) : base(context!)
    {
        _patientMapper = mapper;
    }

    public async Task<Patient?> GetPatientByMedicalRecordNumber(MedicalRecordNumber medicalRecordNumber)
    {
        var patientDataModel =  GetPatientDataModelByMedicalRecordNumber(medicalRecordNumber).Result;

        if (patientDataModel != null && patientDataModel.MarkedForDeletion == false)
        {
            return _patientMapper.ToDomain(patientDataModel);
        }

        return null;
    }

    private async Task<PatientDataModel?> GetPatientDataModelByMedicalRecordNumber(
        MedicalRecordNumber medicalRecordNumber)
    {
        var patientDataModel =
            await _context.Set<PatientDataModel>().FirstOrDefaultAsync(x =>
                x.MedicalRecordNumber.Equals(medicalRecordNumber.MedicalNumber));

        return patientDataModel;
    }

    public async Task<Patient> AddPatient(Patient patient)
    {
        PatientDataModel patientDataModel = _patientMapper.ToDataModel(patient);

        EntityEntry<PatientDataModel> patientDataModelEntityEntry =
            _context.Set<PatientDataModel>().Add(patientDataModel);

        await _context.SaveChangesAsync();

        PatientDataModel patientDataModelSaved = patientDataModelEntityEntry.Entity;

        return _patientMapper.ToDomain(patientDataModelSaved);
    }


    public async Task<Patient?> UpdatePatient(Patient patient)
    {
        PatientDataModel patientDataModel = await _context.Set<PatientDataModel>()
            .FirstAsync(p => p.MedicalRecordNumber == patient.MedicalRecordNumber.MedicalNumber);

        _patientMapper.UpdateDataModel(patientDataModel, patient);

        _context.Entry(patientDataModel).State = EntityState.Modified;

        await _context.SaveChangesAsync();

        return patient;
    }

    public async Task MarkPatientToBeDeleted(Patient patient, TimeSpan retainInfoPeriod)
    {
        PatientDataModel patientDataModel = await _context.Set<PatientDataModel>()
            .FirstAsync(p => p.MedicalRecordNumber == patient.MedicalRecordNumber.MedicalNumber);

        patientDataModel.MarkForDeletion(retainInfoPeriod);

        _context.Entry(patientDataModel).State = EntityState.Modified;

        await _context.SaveChangesAsync();
    }


    public async Task<IEnumerable<Patient>> GetPatientsReadyForDeletion()
    {
        DateTime currentTime = DateTime.UtcNow;

        var patientDataModels = _context.Set<PatientDataModel>()
            .Where(p => p.MarkedForDeletion && p.DateToBeDeleted <= currentTime)
            .ToListAsync().Result.AsEnumerable();

        return _patientMapper.ToDomain(patientDataModels);
    }

    public async Task DeletePatientDefinitive(Patient patient)
    {
        var patientDataModelToRemove = GetPatientDataModelByMedicalRecordNumber(patient.MedicalRecordNumber).Result;

        if (patientDataModelToRemove != null)
        {
            _context.Set<PatientDataModel>().Remove(patientDataModelToRemove);
            await _context.SaveChangesAsync();
        }
    }


    public async Task<IEnumerable<Patient>> SearchPatientsByFiltersAsync(string? name,
        string? gender, string? phoneNumber, string? email, DateOfBirthDTO? dateOfBirth)
    {
        IQueryable<PatientDataModel> myQueryable = _context.Set<PatientDataModel>();
        
        if (name != null && name.Length > 0)
        {
            myQueryable = ApplyNameFilter(myQueryable, name);
        }

        if (gender != null && gender.Length > 0)
        {
            myQueryable = ApplyGenderFilter(myQueryable, gender);
        }

        if (phoneNumber != null && phoneNumber.Length > 0)
        {
            myQueryable = ApplyPhoneNumberFilter(myQueryable, phoneNumber);
        }

        if (email != null && email.Length > 0)
        {
            myQueryable = ApplyEmailFilter(myQueryable, email);
        }

        if (dateOfBirth != null)
        {
            myQueryable = ApplyDateOfBirthFilter(myQueryable, dateOfBirth);
        }

        var patientsFound = await myQueryable.ToListAsync();

        return _patientMapper.ToDomain(patientsFound);
    }

    
    private IQueryable<PatientDataModel> ApplyNameFilter(IQueryable<PatientDataModel> query, string name)
    {
        if (!string.IsNullOrEmpty(name))
        {
            return query.Where(p => p.PatientName.Equals(name));
        }

        return query;
    }


    private static IQueryable<PatientDataModel> ApplyPhoneNumberFilter(IQueryable<PatientDataModel> query,
        string phoneNumber)
    {
        if (!string.IsNullOrEmpty(phoneNumber))
        {
            return query.Where(p => p.PersonalPhoneNumber.Equals(phoneNumber));
        }

        return query;
    }

    private static IQueryable<PatientDataModel> ApplyEmailFilter(IQueryable<PatientDataModel> query,
        string email)
    {
        if (!string.IsNullOrEmpty(email))
        {
            return query.Where(p => p.PersonalEmail.Equals(email));
        }

        return query;
    }

    private static IQueryable<PatientDataModel> ApplyGenderFilter(IQueryable<PatientDataModel> query, string gender)
    {
        if (!string.IsNullOrEmpty(gender))
        {
            return query.Where(p => p.PatientGender.Equals(gender));
        }

        return query;
    }

    private static IQueryable<PatientDataModel> ApplyDateOfBirthFilter(IQueryable<PatientDataModel> query,
        DateOfBirthDTO dateOfBirth)
    {
        if (dateOfBirth.YearOfBirth != null && dateOfBirth.MonthOfBirth != null && dateOfBirth.DayOfBirth != null)
        {
            if (dateOfBirth.YearOfBirth != 0 && dateOfBirth.MonthOfBirth != 0 && dateOfBirth.DayOfBirth != 0)
            {
                DateOnly searchDateTime =
                    new DateOnly(dateOfBirth.YearOfBirth, dateOfBirth.MonthOfBirth, dateOfBirth.DayOfBirth);

                return query.Where(p => p.BirthDate.Equals(searchDateTime));
            }
        }

        return query;
    }


    // Version letting the database do the work
    public async Task<int> GetMaxMedicalRecordNumberSequentialPartAsync()
    {
        var dates = await _context.Set<PatientDataModel>()
            .Select(p => p.MedicalRecordNumber.Substring(0, 6))
            .ToListAsync();

        var latestDate = dates
            .Select(mrn => new
            {
                Year = int.Parse(mrn.Substring(0, 4)),
                Month = int.Parse(mrn.Substring(4, 2))
            })
            .OrderByDescending(d => d.Year)
            .ThenByDescending(d => d.Month)
            .FirstOrDefault();

        if (latestDate == null) return 0;

        // Switching to client-side evaluation with AsEnumerable() here
        var maxSequentialNumber = _context.Set<PatientDataModel>()
            .AsEnumerable()
            .Where(p => p.MedicalRecordNumber.StartsWith($"{latestDate.Year}{latestDate.Month:D2}"))
            .Select(p => int.Parse(p.MedicalRecordNumber.Substring(6)))
            .DefaultIfEmpty(0)
            .Max();

        return maxSequentialNumber;
    }
}