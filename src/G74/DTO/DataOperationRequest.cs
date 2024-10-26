using G74.Domain.Shared;

public class DataOperationRequest : Entity<Guid>
{
    public string MedicalRecordNumber {get; set; }
    public string LicenceNumber {get; set; }
    public string NameOperationType {get; set; }
    public List<SpecializationStaff> RequiredStaffBySpecialization {get;  set;}
    public int Seconds {get;  set; }
    public int Minutes {get;  set; }
    public int Hours {get;  set; }
    public int Days {get;  set; }
    public DateTime DeadlineDate {get; set; }
    public string Priority {get; set; }

    public DataOperationRequest(): base(Guid.NewGuid())  { }
    public DataOperationRequest(
        OperationRequest request
        )
        : base(Guid.NewGuid()) 
    {
        MedicalRecordNumber = request.MedicalRecordNumber.MedicalNumber;
        LicenceNumber = request.LicenceNumber.licenceNumber;
        NameOperationType = request.OperationType.Name.TheName;
        RequiredStaffBySpecialization = request.OperationType.RequiredStaffBySpecialization.SpecializationStaffList;
        Seconds = request.OperationType.EstimatedDuration.Seconds;
        Minutes = request.OperationType.EstimatedDuration.Minutes;
        Hours = request.OperationType.EstimatedDuration.Hours;
        Days = request.OperationType.EstimatedDuration.Days;
        DeadlineDate = request.DeadlineDate.date;
        Priority = request.Priority.PriorityDescription.ToString();
    }

    


}