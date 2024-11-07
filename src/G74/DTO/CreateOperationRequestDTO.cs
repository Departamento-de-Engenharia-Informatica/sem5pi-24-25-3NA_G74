using G74.Domain.Value_Objects.Patient;

public class CreateOperationRequestDTO
{
    public string MedicalRecordNumber {get; set; }
    public string LicenceNumber {get; set; }
    public string Name {get; set; }
    public List<string> RequiredStaffBySpecialization {get; set; }
    public Duration EstimatedDuration {get; set; }
    public DateTime DeadlineDate {get;set; }
    public string Priority {get;set; }
    public CreateOperationRequestDTO(string medicalRecordNumber, string licenceNumber, string name, List<string> requiredStaffBySpecialization, Duration estimatedDuration, DateTime deadlineDate, string priority)
    {
        MedicalRecordNumber = medicalRecordNumber;
        LicenceNumber = licenceNumber;
        Name = name;
        RequiredStaffBySpecialization = requiredStaffBySpecialization;
        EstimatedDuration = estimatedDuration;
        DeadlineDate = deadlineDate;
        Priority = priority;
    }
    

}