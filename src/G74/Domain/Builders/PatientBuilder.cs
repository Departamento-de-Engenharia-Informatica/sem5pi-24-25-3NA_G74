using G74.Domain.Aggregates.User;
using G74.Domain.DomainServices;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.SharedValueObjects;

namespace G74.Domain.Builders;

public class PatientBuilder
{
    private readonly IMedicalRecordNumberGenerator _medicalRecordNumberGenerator;
    
    private Name _name;
    private DateOfBirth _dateOfBirth;
    private Gender _gender;
    private ContactInformation _contactInformation;
    private EmergencyContact _emergencyContact;
    private MedicalRecordNumber _medicalRecordNumber;
    private MedicalCondition? _medicalCondition;
    private AppointmentHistory? _appointmentHistory;


    public PatientBuilder(IMedicalRecordNumberGenerator medicalRecordNumberGenerator, Name name,
        DateOfBirth dateOfBirth,
        Gender gender, ContactInformation contactInformation, EmergencyContact emergencyContact)
    {
        _medicalRecordNumberGenerator = medicalRecordNumberGenerator;
        _name = name;
        _dateOfBirth = dateOfBirth;
        _gender = gender;
        _contactInformation = contactInformation;
        _emergencyContact = emergencyContact;
    }
    //TODO : Incomplete, WithMedicalCondition
    public PatientBuilder WithMedicalCondition(string medicalCondition)
    {
        _medicalCondition = new MedicalCondition(medicalCondition);

        return this;
    }

    //TODO : Incomplete, needs Appointment 
    public PatientBuilder WithAppointmentHistory()
    {
        _appointmentHistory = new AppointmentHistory();

        return this;
    }

    public async Task<Patient> Build()
    {
        _medicalRecordNumber = await _medicalRecordNumberGenerator.GenerateMedicalNumber();

        return new Patient(_name, _medicalRecordNumber, _dateOfBirth,
            _gender, _contactInformation, _emergencyContact);
    }
}