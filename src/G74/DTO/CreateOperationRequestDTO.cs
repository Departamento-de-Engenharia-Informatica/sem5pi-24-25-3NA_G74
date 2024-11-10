using G74.Domain.Value_Objects.Patient;

public class CreateOperationRequestDTO
{   
    public string MedicalRecordNumber {get; set; }
    public string LicenceNumber {get; set; }
    public long OperationTypeId { get; }
    public DateTime DeadlineDate {get;set; }
    public string Priority {get;set; }
    public CreateOperationRequestDTO(string medicalRecordNumber, string licenceNumber, long operationTypeId, DateTime deadlineDate, string priority)
    {
        MedicalRecordNumber = medicalRecordNumber;
        LicenceNumber = licenceNumber;
        OperationTypeId = operationTypeId;
        DeadlineDate = deadlineDate;
        Priority = priority;
    }
    

}