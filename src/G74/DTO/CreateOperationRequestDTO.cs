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

    

}