using G74.Domain;
using G74.Domain.Shared;
using G74.Domain.Value_Objects.Patient;

public class OperationRequest : IAggregateRoot
{
    public MedicalRecordNumber MedicalRecordNumber {get; set; }
    public LicenceNumber LicenceNumber {get; set; }
    public OperationType OperationType {get; set; }
    public DeadlineDate DeadlineDate {get; set; }
    public Priority Priority {get; set; }

    public OperationRequest(MedicalRecordNumber medicalRecordNumber, LicenceNumber licenceNumber, OperationType operationType, DeadlineDate deadlineDate, Priority priority)
    {
        MedicalRecordNumber = medicalRecordNumber;
        LicenceNumber = licenceNumber;
        OperationType = operationType;
        DeadlineDate = deadlineDate;
        Priority = priority;
    }
    

}