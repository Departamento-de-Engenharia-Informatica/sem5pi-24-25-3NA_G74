using G74.Domain.Value_Objects.Patient;

public class OperationRequestDTO {
    public MedicalRecordNumber MedicalRecordNumber {get;private set; }
    public LicenceNumber LicenceNumber {get;private set; }
    public OperationType OperationType {get;private set; }
    public DeadlineDate DeadlineDate {get;private set; }
    public Priority Priority {get;private set; }

    public OperationRequestDTO(MedicalRecordNumber medicalRecordNumber, LicenceNumber licenceNumber, OperationType operationType, DeadlineDate deadlineDate, Priority priority)
    {
        MedicalRecordNumber = medicalRecordNumber;
        LicenceNumber = licenceNumber;
        OperationType = operationType;
        DeadlineDate = deadlineDate;
        Priority = priority;
    }

}