using G74.Domain;
using G74.Domain.Aggregates.OperationType;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;

public class OperationRequest : IAggregateRoot
{
    public long OperationRequestId {get; set; }
    public MedicalRecordNumber MedicalRecordNumber {get; set; }
    public LicenceNumber LicenceNumber {get; set; }
    public long OperationTypeId {get; set; }
    public DeadlineDate DeadlineDate {get; set; }
    public Priority Priority {get; set; }

    public OperationRequest(MedicalRecordNumber medicalRecordNumber, LicenceNumber licenceNumber, long operationTypeId, DeadlineDate deadlineDate, Priority priority)
    {
        MedicalRecordNumber = medicalRecordNumber;
        LicenceNumber = licenceNumber;
        OperationTypeId = operationTypeId;
        DeadlineDate = deadlineDate;
        Priority = priority;
    }
    public OperationRequest(MedicalRecordNumber medicalRecordNumber, LicenceNumber licenceNumber, long operationTypeId, DeadlineDate deadlineDate, Priority priority, long operationRequestId)
    {
        MedicalRecordNumber = medicalRecordNumber;
        LicenceNumber = licenceNumber;
        OperationTypeId = operationTypeId;
        DeadlineDate = deadlineDate;
        Priority = priority;
        OperationRequestId = operationRequestId;
    }
    

}