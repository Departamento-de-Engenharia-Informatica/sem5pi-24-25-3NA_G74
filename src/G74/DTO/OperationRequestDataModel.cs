using G74.Domain.Shared;

namespace G74.DTO;

public class OperationRequestDataModel : Entity<Guid>
{
    public long Id {get; set; } 
    public long MedicalRecordNumber {get; set; }
    public long LicenceNumber {get; set; }
    public int OperationTypeId {get; set; }
    public DateTime DeadlineDate {get; set; }
    public string Priority {get; set; }

    public OperationRequestDataModel(): base(Guid.NewGuid())  { }
    public OperationRequestDataModel(
        OperationRequest request
    )
        : base(Guid.NewGuid()) 
    {
        MedicalRecordNumber = long.Parse(request.MedicalRecordNumber.MedicalNumber);
        LicenceNumber = request.LicenceNumber.Value;
        OperationTypeId = request.OperationTypeId;
        DeadlineDate = request.DeadlineDate.date;
        Priority = request.Priority.PriorityDescription.ToString();
    }
    
    

    


}