using G74.Domain.Shared;

namespace G74.DTO;

public class OperationRequestDataModel : Entity<Guid>
{
    public int MedicalRecordNumber {get; set; }
    public int LicenceNumber {get; set; }
    public int OperationTypeId {get; set; }
    public DateTime DeadlineDate {get; set; }
    public string Priority {get; set; }

    public OperationRequestDataModel(): base(Guid.NewGuid())  { }
    public OperationRequestDataModel(
        OperationRequest request
    )
        : base(Guid.NewGuid()) 
    {
        MedicalRecordNumber = int.Parse(request.MedicalRecordNumber.MedicalNumber);
        LicenceNumber = int.Parse(request.LicenceNumber.licenceNumber);
        OperationTypeId = request.OperationTypeId;
        DeadlineDate = request.DeadlineDate.date;
        Priority = request.Priority.PriorityDescription.ToString();
    }

    


}