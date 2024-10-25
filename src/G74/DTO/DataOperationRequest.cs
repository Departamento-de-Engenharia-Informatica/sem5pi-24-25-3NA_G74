using G74.Domain.Shared;

public class DataOperationRequest : Entity<Guid>
{
    public string MedicalRecordNumber {get;private set; }
    public string LicenceNumber {get;private set; }
    public string NameOperationType {get;private set; }
    public List<SpecializationStaff> RequiredStaffBySpecialization {get; private set;}
    public int Seconds {get; private set; }
    public int Minutes {get; private set; }
    public int Hours {get; private set; }
    public int Days {get; private set; }
    public DateTime DeadlineDate {get;private set; }
    public string Priority {get;private set; }

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