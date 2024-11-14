using G74.Domain.Aggregates.OperationType;
using G74.Domain.Value_Objects.Patient;

public class OperationRequestDTO {
    public long OperationRequestId {get; set; } 
    public MedicalRecordNumber MedicalRecordNumber {get;private set; }
    public long LicenceNumber {get;private set; }
    public long OperationTypeId {get;private set; }
    public DeadlineDate DeadlineDate {get;private set; }
    public Priority Priority {get;private set; }

    public OperationRequestDTO(MedicalRecordNumber medicalRecordNumber, LicenceNumber licenceNumber, long operationTypeId, DeadlineDate deadlineDate, Priority priority, long operationRequestId)
    {
        OperationRequestId = operationRequestId;
        MedicalRecordNumber = medicalRecordNumber;
        LicenceNumber = licenceNumber.licenceNumber;
        OperationTypeId = operationTypeId;
        DeadlineDate = deadlineDate;
        Priority = priority;
    }
    public OperationRequestDTO(MedicalRecordNumber medicalRecordNumber, LicenceNumber licenceNumber, long operationTypeId, DeadlineDate deadlineDate, Priority priority)
    {
        MedicalRecordNumber = medicalRecordNumber;
        LicenceNumber = licenceNumber.licenceNumber;
        OperationTypeId = operationTypeId;
        DeadlineDate = deadlineDate;
        Priority = priority;
    }
    

}