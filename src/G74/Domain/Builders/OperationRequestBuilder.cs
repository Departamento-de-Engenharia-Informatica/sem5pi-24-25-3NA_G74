using G74.Domain.Value_Objects.Patient;

public class OperationRequestBuilder
{
    private MedicalRecordNumber _medicalRecordNumber ;
    private LicenceNumber _licenceNumber ;
    private OperationType _operationType ;
    private DeadlineDate _deadlineDate ;
    private Priority _priority ;

    public OperationRequestBuilder(
        MedicalRecordNumber medicalRecordNumber,
        LicenceNumber licenceNumber,
        OperationType operationType,
        DeadlineDate deadlineDate,
        Priority priority
    ){
        _medicalRecordNumber = medicalRecordNumber;
        _licenceNumber = licenceNumber;
        _operationType = operationType;
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
            _operationType,
            _deadlineDate,
            _priority
        );
    }

    
}