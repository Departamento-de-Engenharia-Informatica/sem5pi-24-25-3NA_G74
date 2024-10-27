using G74.Domain.Value_Objects.Patient;

public class CreateOperationRequestDTO
{
    public required string MedicalRecordNumber {get; set; }
    public required string LicenceNumber {get; set; }
    public required string Name {get; set; }
    public required List<string> RequiredStaffBySpecialization {get; set; }
    public required Duration EstimatedDuration {get; set; }
    public DateTime DeadlineDate {get;set; }
    public required string Priority {get;set; }
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