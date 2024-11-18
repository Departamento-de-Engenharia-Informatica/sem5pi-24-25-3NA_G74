using G74.Domain.Aggregates.OperationType;
using G74.Domain.Value_Objects.Patient;
using G74.Domain.Value_Objects.Staff.Doctor;

public class OperationRequestBuilder
{
    private MedicalRecordNumber _medicalRecordNumber ;
    private LicenceNumber _licenceNumber ;
    private long _operationTypeId ;
    private DeadlineDate _deadlineDate ;
    private Priority _priority ;

    public OperationRequestBuilder(
        MedicalRecordNumber medicalRecordNumber,
        LicenceNumber licenceNumber,
        long operationTypeId,
        DeadlineDate deadlineDate,
        Priority priority
    ){
        _medicalRecordNumber = medicalRecordNumber;
        _licenceNumber = licenceNumber;
        _operationTypeId = operationTypeId;
        _deadlineDate = deadlineDate;
        _priority = priority;
    }

    /**
    public async Task<Patient> Build()
    {
        _medicalRecordNumber = await _medicalRecordNumberGenerator.GenerateMedicalNumber();

        return new Patient(_name, _medicalRecordNumber, _dateOfBirth,
            _gender, _contactInformation, _emergencyContact);
    }
    */

    public async Task<OperationRequest> Build()
    {
        return new OperationRequest(
            _medicalRecordNumber,
            _licenceNumber,
            _operationTypeId,
            _deadlineDate,
            _priority
        );
    }

    
}